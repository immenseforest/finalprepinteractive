library(shiny)

# ---------- Transform catalogue ----------
signal_catalogue <- list(
  exponential = list(
    label = "Exponential decay",
    parameters = c("Decay rate" = "a"),
    defaults = c(a = 1.5),
    ranges = list(a = c(0.1, 5, 0.1)),
    time = function(t, p) exp(-p["a"] * t),
    transform = function(s, p) 1 / (s + p["a"]),
    formula = function(p) sprintf("f(t)=e^{-%.2f t}", p["a"]),
    laplace = function(p) sprintf("F(s)=\\frac{1}{s+%.2f},\\qquad \\operatorname{Re}(s)>-%.2f", p["a"], p["a"])
  ),
  sine = list(
    label = "Sine wave",
    parameters = c("Angular frequency" = "omega"),
    defaults = c(omega = 2),
    ranges = list(omega = c(0.2, 8, 0.1)),
    time = function(t, p) sin(p["omega"] * t),
    transform = function(s, p) p["omega"] / (s^2 + p["omega"]^2),
    formula = function(p) sprintf("f(t)=\\sin(%.2f t)", p["omega"]),
    laplace = function(p) sprintf("F(s)=\\frac{%.2f}{s^2+%.2f},\\qquad \\operatorname{Re}(s)>0",
                                  p["omega"], p["omega"]^2)
  ),
  cosine = list(
    label = "Cosine wave",
    parameters = c("Angular frequency" = "omega"),
    defaults = c(omega = 2),
    ranges = list(omega = c(0.2, 8, 0.1)),
    time = function(t, p) cos(p["omega"] * t),
    transform = function(s, p) s / (s^2 + p["omega"]^2),
    formula = function(p) sprintf("f(t)=\\cos(%.2f t)", p["omega"]),
    laplace = function(p) sprintf("F(s)=\\frac{s}{s^2+%.2f},\\qquad \\operatorname{Re}(s)>0", p["omega"]^2)
  ),
  polynomial = list(
    label = "Power function",
    parameters = c("Power (integer)" = "n"),
    defaults = c(n = 2),
    ranges = list(n = c(0, 6, 1)),
    time = function(t, p) t^p["n"],
    transform = function(s, p) factorial(p["n"]) / s^(p["n"] + 1),
    formula = function(p) sprintf("f(t)=t^{%d}", p["n"]),
    laplace = function(p) sprintf("F(s)=\\frac{%d!}{s^{%d}},\\qquad \\operatorname{Re}(s)>0",
                                  p["n"], p["n"] + 1)
  ),
  damped_sine = list(
    label = "Damped sine",
    parameters = c("Decay rate" = "a", "Angular frequency" = "omega"),
    defaults = c(a = 0.7, omega = 3),
    ranges = list(a = c(0.1, 4, 0.1), omega = c(0.2, 8, 0.1)),
    time = function(t, p) exp(-p["a"] * t) * sin(p["omega"] * t),
    transform = function(s, p) p["omega"] / ((s + p["a"])^2 + p["omega"]^2),
    formula = function(p) sprintf("f(t)=e^{-%.2f t}\\sin(%.2f t)", p["a"], p["omega"]),
    laplace = function(p) sprintf("F(s)=\\frac{%.2f}{(s+%.2f)^2+%.2f}",
                                  p["omega"], p["a"], p["omega"]^2)
  )
)

vehicle_presets <- list(
  outback = c(mass = 1652, stiffness = 102000, damping = 10900, force = 3000),
  f150 = c(mass = 2312, stiffness = 166000, damping = 15700, force = 4000),
  crv = c(mass = 1629, stiffness = 109000, damping = 12000, force = 3000)
)

chemical_presets <- list(
  beverage = c(volume = 5000, flow = 500, initial = 0.2, inlet = 1.2),
  pharma = c(volume = 1000, flow = 50, initial = 0.1, inlet = 0.8),
  chlorine = c(volume = 45000, flow = 756, initial = 0.1, inlet = 1.0)
)

building_presets <- list(
  taipei = c(mass = 50, frequency = 0.15, damping = 2.0, force = 3000),
  burj = c(mass = 80, frequency = 0.12, damping = 2.0, force = 4500),
  cn = c(mass = 60, frequency = 0.18, damping = 2.5, force = 2500)
)

thermal_presets <- list(
  laptop = c(ambient = 22, initial = 22, rise = 48, tau = 8),
  battery = c(ambient = 25, initial = 25, rise = 18, tau = 35),
  motor = c(ambient = 30, initial = 30, rise = 55, tau = 60)
)

truss_presets <- list(
  roof = c(left = 45, right = 45, horizontal = 0, vertical = 20),
  bridge = c(left = 35, right = 55, horizontal = 5, vertical = 40),
  crane = c(left = 65, right = 25, horizontal = -12, vertical = 30)
)

nav_items <- c("Transform Explorer" = "explorer", "Property Lab" = "properties",
               "Quick Reference" = "reference")

help_tip <- function(text) {
  tags$span(
    class = "help-tip", tabindex = "0", `data-tip` = text,
    `aria-label` = paste("Simple explanation:", text), "?"
  )
}

math_block <- function(tex, label) {
  div(
    class = "formula math-formula", role = "img", `aria-label` = label,
    withMathJax(HTML(paste0("\\[", tex, "\\]")))
  )
}

math_inline <- function(tex, label) {
  span(
    class = "math-inline", role = "img", `aria-label` = label,
    withMathJax(HTML(paste0("\\(", tex, "\\)")))
  )
}

module_header <- function(number, title, description) {
  div(
    class = "module-heading",
    div(class = "module-seal", number),
    div(
      div(class = "module-kicker", "Course module"),
      h2(title),
      tags$p(description),
      div(class = "module-route", "Overview  →  Explore  →  Watch  →  Apply")
    )
  )
}

video_lesson_page <- function(topic, intro, videos, catalogue_url) {
  div(
    div(class = "card",
      div(class = "eyebrow", "Watch · pause · practise"),
      h2(paste(topic, "video lessons")),
      tags$p(intro),
      tags$p(class = "hint",
        "Suggested routine: watch one example, pause before each worked step, solve it yourself, then reproduce the method in this app."),
      tags$a(class = "source-link", href = catalogue_url, target = "_blank",
             rel = "noopener noreferrer", "View the source lesson catalogue on Video-Tutor.net")
    ),
    div(class = "video-grid",
      lapply(videos, function(video) {
        div(class = "card video-card",
          div(class = "video-number", video$lesson),
          div(class = "video-source", video$creator),
          h3(video$title),
          tags$p(video$purpose),
          tags$a(
            class = "btn video-link", href = video$url, target = "_blank",
            rel = "noopener noreferrer", "Watch video ↗"
          )
        )
      })
    )
  )
}

laplace_videos <- list(
  list(lesson = "01", creator = "3Blue1Brown",
       title = "Why Laplace transforms are so useful",
       purpose = "Build intuition for why a time-domain differential equation becomes easier algebra in the s-domain.",
       url = "https://www.youtube.com/watch?v=FE-hM1kRK4Y"),
  list(lesson = "02", creator = "Dr. Trefor Bazett",
       title = "Inverse Laplace transform using partial fractions",
       purpose = "Practise decomposing a rational transform and returning to the time-domain signal.",
       url = "https://www.youtube.com/watch?v=c6YnYr8KsSo"),
  list(lesson = "03", creator = "Dr. Trefor Bazett",
       title = "Solve a differential equation with Laplace transforms",
       purpose = "Connect derivative rules and initial conditions to the algebraic equation for Y(s).",
       url = "https://www.youtube.com/watch?v=fuxFrpaMLtw"),
  list(lesson = "04", creator = "Dr. Trefor Bazett",
       title = "Laplace transform of a piecewise unit-step function",
       purpose = "Apply the unit-step function and second-shifting theorem used in the Properties Lab.",
       url = "https://www.youtube.com/watch?v=yHzXAoFjU3k"),
  list(lesson = "05", creator = "Dr. Trefor Bazett",
       title = "Solving ODEs with Dirac-delta impulse inputs",
       purpose = "Model a sudden kick or impact and solve the resulting initial-value problem with Laplace transforms.",
       url = "https://www.youtube.com/watch?v=LOoM3qlpYuU"),
  list(lesson = "06", creator = "Dr. Trefor Bazett",
       title = "Convolution of two functions",
       purpose = "Learn the definition and mechanics behind the convolution theorem used in transform problems.",
       url = "https://www.youtube.com/watch?v=AgKQQtEc9dk")
)

differential_videos <- list(
  list(lesson = "9.1", creator = "The Organic Chemistry Tutor",
       title = "The Initial Value Problem",
       purpose = "Learn how an equation and its starting values combine to determine one specific solution.",
       url = "https://www.youtube.com/watch?v=kwGukY_2qWQ"),
  list(lesson = "9.2", creator = "The Organic Chemistry Tutor",
       title = "Separable First-Order Differential Equations",
       purpose = "Build the separate–integrate–solve habit for the most recognizable first-order family.",
       url = "https://www.youtube.com/watch?v=C7nuJcJriWM"),
  list(lesson = "9.10", creator = "The Organic Chemistry Tutor",
       title = "First-Order Linear Differential Equations",
       purpose = "Review standard form and the integrating-factor method.",
       url = "https://www.youtube.com/watch?v=gd1FYn86P0c"),
  list(lesson = "9.12", creator = "The Organic Chemistry Tutor",
       title = "Second-Order Linear Differential Equations",
       purpose = "Match characteristic roots to the solution forms explored in the Linear ODE Lab.",
       url = "https://www.youtube.com/watch?v=uI2xt8nTOlQ"),
  list(lesson = "9.13", creator = "Jeffrey Chasnov",
       title = "Phase portraits of linear systems",
       purpose = "Read the stability and motion of a two-state system from its eigenvalues and phase portrait.",
       url = "https://www.youtube.com/watch?v=UO_dgXa5szg")
)

linear_algebra_videos <- list(
  list(lesson = "15.7", creator = "The Organic Chemistry Tutor",
       title = "Elementary Row Operations",
       purpose = "Review the legal row moves that preserve the solution set of a linear system.",
       url = "https://www.youtube.com/watch?v=9PNCjHemIhI"),
  list(lesson = "15.8", creator = "The Organic Chemistry Tutor",
       title = "Gaussian Elimination and Row-Echelon Form",
       purpose = "Practise finding pivots, solving systems, and recognizing free variables.",
       url = "https://www.youtube.com/watch?v=eDb6iugi6Uk"),
  list(lesson = "15.11", creator = "The Organic Chemistry Tutor",
       title = "Inverse of a 2×2 Matrix",
       purpose = "Connect the inverse formula to the determinant and the solution of Ax=b.",
       url = "https://www.youtube.com/watch?v=aiBgjz5xbyg"),
  list(lesson = "15.13", creator = "The Organic Chemistry Tutor",
       title = "Determinants of 2×2 and 3×3 Matrices",
       purpose = "Build speed with determinant calculations before using determinant identities and eigenvalue tests.",
       url = "https://www.youtube.com/watch?v=3ROzG6n4yMc"),
  list(lesson = "15.14", creator = "3Blue1Brown",
       title = "Eigenvectors and eigenvalues",
       purpose = "See geometrically why eigenvector directions stay fixed while a matrix transformation scales them.",
       url = "https://www.youtube.com/watch?v=PFDu9oVAE-g"),
  list(lesson = "15.15", creator = "Dr. Trefor Bazett",
       title = "Diagonalizing a matrix: full example",
       purpose = "Build P and D from eigenvectors and eigenvalues, then verify A=PDP⁻¹.",
       url = "https://www.youtube.com/watch?v=ieWyx2mlZyk")
)

catalog_tutorial_match <- function(title) {
  if (title %in% c(
    "Constant-coefficient initial-value problem",
    "Cauchy-Euler equation with complex roots",
    "Forced oscillator at resonance",
    "Repeated operator with matching forcing"
  )) {
    return(list(
      video = differential_videos[[4]],
      reason = "Refresh characteristic roots, complementary solutions, and the trial-solution workflow for second-order linear equations."
    ))
  }
  if (title %in% c(
    "Reduce the order of a nonlinear equation",
    "Nonlinear equation missing the dependent variable"
  )) {
    return(list(
      video = differential_videos[[2]],
      reason = "After reducing the order, the remaining first-order equation is solved by separating variables—the central method in this lesson."
    ))
  }
  if (title %in% c(
    "Classify a planar equilibrium",
    "Eigenvalue classification of a flow"
  )) {
    return(list(
      video = differential_videos[[5]],
      reason = "Review how eigenvalues determine stability and the node, saddle, spiral, or center seen in a phase portrait."
    ))
  }
  if (title %in% c(
    "Inverse transform by completing the square",
    "Recognize a frequency shift"
  )) {
    return(list(
      video = laplace_videos[[2]],
      reason = "Refresh the pattern-matching and algebra used to decompose a transform before returning to the time domain."
    ))
  }
  if (title %in% c(
    "Delayed forcing in an initial-value problem",
    "Invert a delayed damped signal"
  )) {
    return(list(
      video = laplace_videos[[4]],
      reason = "Review unit-step notation and the second-shifting theorem used to move a signal or forcing input in time."
    ))
  }
  if (title %in% c(
    "Solve a convolution integral equation",
    "Integral equation with an exponential kernel"
  )) {
    return(list(
      video = laplace_videos[[6]],
      reason = "Refresh how a convolution integral becomes a product of transforms and how that simplifies an integral equation."
    ))
  }
  if (title == "Differentiate a transform") {
    return(list(
      video = laplace_videos[[1]],
      reason = "Revisit the time-domain versus s-domain viewpoint before applying the property that multiplication by time differentiates the transform."
    ))
  }
  if (title %in% c(
    "Impulse-driven oscillator",
    "Zero-state response to a delayed impulse"
  )) {
    return(list(
      video = laplace_videos[[5]],
      reason = "Review the transform of a delayed Dirac impulse and how it produces the shifted response of a differential equation."
    ))
  }
  if (title == "Inverse transform and convolution structure") {
    return(list(
      video = laplace_videos[[2]],
      reason = "Refresh partial-fraction decomposition, the most direct route for inverting this rational transform."
    ))
  }
  if (title %in% c(
    "Classify a parameterized linear system",
    "Consistency and a determinant identity"
  )) {
    return(list(
      video = linear_algebra_videos[[2]],
      reason = "Review pivots and row-echelon form to distinguish unique, inconsistent, and infinitely many solution cases."
    ))
  }
  if (title == "Simplify a determinant expression") {
    return(list(
      video = linear_algebra_videos[[4]],
      reason = "Refresh determinant calculations before applying the product, transpose, inverse, and power identities."
    ))
  }
  if (title %in% c(
    "Diagonalize a matrix and express its powers",
    "Use symmetry to compute a matrix power",
    "Principal square root of a symmetric matrix",
    "A matrix function from spectral data"
  )) {
    return(list(
      video = linear_algebra_videos[[6]],
      reason = "Review how eigenvectors form P, eigenvalues form D, and matrix powers or roots are computed through the diagonal matrix."
    ))
  }
  stop("No tutorial catalog match for mock-exam question: ", title)
}

mock_question <- function(topic, title, prompt, steps, answer) {
  list(
    topic = topic, title = title, prompt = prompt, steps = steps, answer = answer,
    tutorial = catalog_tutorial_match(title)
  )
}

# Five newly written practice finals modeled on the recurring skills in the
# supplied 2020-2025 papers. They are predictions for study coverage, not copies
# of past questions or claims about the contents of a future final.
mock_exam_bank <- list(
  "Version 1" = list(
    subtitle = "Balanced foundations",
    emphasis = "A broad rehearsal of characteristic roots, transform shifts, step inputs, parameterized systems, and diagonalization.",
    questions = list(
      mock_question(
        "Differential equations · 20 marks", "Constant-coefficient initial-value problem",
        "y''+4y'+3y=0,\\qquad y(0)=2,\\quad y'(0)=-4",
        c(
          "Form the characteristic equation \\(r^2+4r+3=0\\).",
          "Factor it to obtain the two modes associated with \\(r=-1\\) and \\(r=-3\\).",
          "Write \\(y=C_1e^{-x}+C_2e^{-3x}\\), then apply both initial conditions.",
          "Solve \\(C_1+C_2=2\\) and \\(-C_1-3C_2=-4\\)."
        ),
        "y(x)=e^{-x}+e^{-3x}"
      ),
      mock_question(
        "Laplace transforms · 20 marks", "Inverse transform by completing the square",
        "\\mathcal{L}^{-1}\\!\\left\\{\\frac{2s+5}{s^2+4s+13}\\right\\}",
        c(
          "Complete the square: \\(s^2+4s+13=(s+2)^2+9\\).",
          "Rewrite the numerator as \\(2(s+2)+1\\).",
          "Match the two pieces to the shifted cosine and sine transform pairs.",
          "Use the frequency-shift rule: replacing \\(s\\) by \\(s+2\\) multiplies the time function by \\(e^{-2t}\\)."
        ),
        "f(t)=2e^{-2t}\\cos(3t)+\\frac{1}{3}e^{-2t}\\sin(3t)"
      ),
      mock_question(
        "Laplace transforms · 20 marks", "Delayed forcing in an initial-value problem",
        "y'+2y=u(t-3),\\qquad y(0)=1",
        c(
          "Transform the equation to get \\((s+2)Y-1=e^{-3s}/s\\).",
          "Isolate \\(Y\\) as \\(1/(s+2)+e^{-3s}/[s(s+2)]\\).",
          "Use partial fractions: \\(1/[s(s+2)]=\\tfrac12(1/s-1/(s+2))\\).",
          "Apply the second-shifting theorem to the delayed term."
        ),
        "y(t)=e^{-2t}+\\frac12\\left[1-e^{-2(t-3)}\\right]u(t-3)"
      ),
      mock_question(
        "Linear algebra · 20 marks", "Classify a parameterized linear system",
        "x+ky=1,\\qquad 2x+4y=2",
        c(
          "Write the coefficient matrix and compute its determinant \\(4-2k\\).",
          "When \\(k\\ne2\\), the determinant is nonzero, so the solution is unique.",
          "Substitute \\(x=1-ky\\) into the second equation to find that unique solution.",
          "When \\(k=2\\), compare the two equations and determine whether they coincide or contradict."
        ),
        "\\begin{cases}k\\ne2:&(x,y)=(1,0),\\\\k=2:&\\text{infinitely many solutions on }x+2y=1.\\end{cases}"
      ),
      mock_question(
        "Linear algebra · 20 marks", "Diagonalize a matrix and express its powers",
        "A=\\begin{bmatrix}4&1\\\\2&3\\end{bmatrix},\\qquad \\text{find a diagonalization and }A^n",
        c(
          "Compute \\(\\det(A-\\lambda I)=(\\lambda-5)(\\lambda-2)\\).",
          "Find eigenvectors \\(v_1=(1,1)^T\\) for \\(\\lambda=5\\) and \\(v_2=(1,-2)^T\\) for \\(\\lambda=2\\).",
          "Build \\(P=[v_1\\ v_2]\\) and \\(D=\\operatorname{diag}(5,2)\\).",
          "Use \\(A=PDP^{-1}\\), then raise only the diagonal entries to obtain \\(A^n=PD^nP^{-1}\\)."
        ),
        "A^n=\\begin{bmatrix}1&1\\\\1&-2\\end{bmatrix}\\begin{bmatrix}5^n&0\\\\0&2^n\\end{bmatrix}\\begin{bmatrix}\\frac23&\\frac13\\\\\\frac13&-\\frac13\\end{bmatrix}"
      )
    )
  ),
  "Version 2" = list(
    subtitle = "Signals and structure",
    emphasis = "Extra weight on nonlinear reduction, convolution, delayed signals, determinant laws, and symmetric diagonalization.",
    questions = list(
      mock_question(
        "Differential equations · 20 marks", "Reduce the order of a nonlinear equation",
        "y''=yy',\\qquad y(0)=0,\\quad y'(0)=1",
        c(
          "Because \\(x\\) is absent, set \\(v(y)=y'\\), so \\(y''=v\\,dv/dy\\).",
          "Substitute to obtain \\(v\\,dv/dy=yv\\), then use the nonzero branch through the initial data.",
          "Integrate \\(dv/dy=y\\) and use \\(v(0)=1\\) to get \\(y'=1+y^2/2\\).",
          "Separate variables, integrate, and use \\(y(0)=0\\)."
        ),
        "y(x)=\\sqrt{2}\\tan\\!\\left(\\frac{x}{\\sqrt{2}}\\right)"
      ),
      mock_question(
        "Laplace transforms · 20 marks", "Solve a convolution integral equation",
        "y(t)+9\\int_0^t(t-\\tau)y(\\tau)\\,d\\tau=3t",
        c(
          "Recognize the integral as the convolution \\(t*y(t)\\).",
          "Transform it using \\(\\mathcal{L}\\{t\\}=1/s^2\\), giving \\(Y+9Y/s^2=3/s^2\\).",
          "Solve algebraically for \\(Y\\).",
          "Match the result to the transform of a sine."
        ),
        "y(t)=\\sin(3t)"
      ),
      mock_question(
        "Laplace transforms · 20 marks", "Invert a delayed damped signal",
        "\\mathcal{L}^{-1}\\!\\left\\{e^{-4s}\\frac{6}{s^2+4s+13}\\right\\}",
        c(
          "Complete the square in the denominator: \\((s+2)^2+3^2\\).",
          "First invert the undelayed transform as \\(2e^{-2t}\\sin(3t)\\).",
          "For the factor \\(e^{-4s}\\), replace \\(t\\) by \\(t-4\\).",
          "Multiply the shifted signal by \\(u(t-4)\\)."
        ),
        "f(t)=2e^{-2(t-4)}\\sin\\!\\bigl(3(t-4)\\bigr)u(t-4)"
      ),
      mock_question(
        "Linear algebra · 20 marks", "Simplify a determinant expression",
        "\\det\\!\\left(A^2B^{-1}A^TB^3\\right)",
        c(
          "Split the determinant of the product into a product of determinants.",
          "Use \\(\\det(A^2)=\\det(A)^2\\) and \\(\\det(A^T)=\\det(A)\\).",
          "Use \\(\\det(B^{-1})=\\det(B)^{-1}\\) and \\(\\det(B^3)=\\det(B)^3\\).",
          "Combine the powers belonging to each matrix."
        ),
        "\\det\\!\\left(A^2B^{-1}A^TB^3\\right)=\\det(A)^3\\det(B)^2"
      ),
      mock_question(
        "Linear algebra · 20 marks", "Use symmetry to compute a matrix power",
        "A=\\begin{bmatrix}2&1\\\\1&2\\end{bmatrix},\\qquad \\text{compute }A^5",
        c(
          "Use the orthogonal eigen-directions \\((1,1)^T\\) and \\((1,-1)^T\\).",
          "Their eigenvalues are \\(3\\) and \\(1\\), respectively.",
          "Write \\(A=P\\operatorname{diag}(3,1)P^T\\) with normalized eigenvectors.",
          "Replace the eigenvalues by \\(3^5\\) and \\(1^5\\), then multiply."
        ),
        "A^5=\\begin{bmatrix}122&121\\\\121&122\\end{bmatrix}"
      )
    )
  ),
  "Version 3" = list(
    subtitle = "Dynamics and impulses",
    emphasis = "A rehearsal of phase classification, Cauchy-Euler modes, transform differentiation, impulse response, and matrix roots.",
    questions = list(
      mock_question(
        "Differential equations · 20 marks", "Classify a planar equilibrium",
        "\\mathbf{x}'=\\begin{bmatrix}0&1\\\\-4&-4\\end{bmatrix}\\mathbf{x}",
        c(
          "Compute the characteristic polynomial \\(\\lambda^2+4\\lambda+4\\).",
          "The repeated eigenvalue is \\(\\lambda=-2\\).",
          "Check the nullspace of \\(A+2I\\) and observe that only one independent eigenvector is available.",
          "Combine the negative real eigenvalue with the defective eigenspace to classify the origin."
        ),
        "\\text{The origin is an asymptotically stable improper node.}"
      ),
      mock_question(
        "Differential equations · 20 marks", "Cauchy-Euler equation with complex roots",
        "x^2y''+xy'+4y=0,\\qquad x>0",
        c(
          "Try the Cauchy-Euler form \\(y=x^m\\).",
          "Substitution gives \\(m(m-1)+m+4=m^2+4=0\\).",
          "The roots are \\(m=\\pm2i\\).",
          "Convert \\(x^{\\pm2i}\\) into real sine and cosine functions of \\(\\ln x\\)."
        ),
        "y(x)=C_1\\cos(2\\ln x)+C_2\\sin(2\\ln x)"
      ),
      mock_question(
        "Laplace transforms · 20 marks", "Differentiate a transform",
        "\\mathcal{L}\\{te^{-2t}\\sin(3t)\\}",
        c(
          "Begin with \\(F(s)=\\mathcal{L}\\{e^{-2t}\\sin(3t)\\}=3/[(s+2)^2+9]\\).",
          "Use \\(\\mathcal{L}\\{tf(t)\\}=-F'(s)\\).",
          "Differentiate the reciprocal carefully with the chain rule.",
          "Simplify the two negative signs."
        ),
        "\\mathcal{L}\\{te^{-2t}\\sin(3t)\\}=\\frac{6(s+2)}{\\left((s+2)^2+9\\right)^2}"
      ),
      mock_question(
        "Laplace transforms · 20 marks", "Impulse-driven oscillator",
        "y''+4y=\\delta(t-\\pi),\\qquad y(0)=0,\\quad y'(0)=1",
        c(
          "Transform the left side, including both initial conditions.",
          "Use \\(\\mathcal{L}\\{\\delta(t-\\pi)\\}=e^{-\\pi s}\\).",
          "Solve \\((s^2+4)Y-1=e^{-\\pi s}\\) for \\(Y\\).",
          "Invert the ordinary and delayed sine terms separately."
        ),
        "y(t)=\\frac12\\sin(2t)+\\frac12\\sin\\!\\bigl(2(t-\\pi)\\bigr)u(t-\\pi)"
      ),
      mock_question(
        "Linear algebra · 20 marks", "Principal square root of a symmetric matrix",
        "A=\\begin{bmatrix}10&6\\\\6&10\\end{bmatrix},\\qquad \\text{find }A^{1/2}",
        c(
          "Find the eigenvalues along \\((1,1)^T\\) and \\((1,-1)^T\\): they are \\(16\\) and \\(4\\).",
          "Use normalized eigenvectors to form an orthogonal matrix \\(P\\).",
          "Take the positive square roots of the eigenvalues to obtain \\(\\operatorname{diag}(4,2)\\).",
          "Reconstruct the principal root as \\(P\\operatorname{diag}(4,2)P^T\\)."
        ),
        "A^{1/2}=\\begin{bmatrix}3&1\\\\1&3\\end{bmatrix}"
      )
    )
  ),
  "Version 4" = list(
    subtitle = "Method selection",
    emphasis = "Focuses on recognizing resonance, classifying a linear flow, shifting frequency, transforming an integral equation, and testing consistency.",
    questions = list(
      mock_question(
        "Differential equations · 20 marks", "Forced oscillator at resonance",
        "y''+4y=\\cos(2x)",
        c(
          "Solve the homogeneous characteristic equation \\(r^2+4=0\\).",
          "The forcing duplicates the homogeneous cosine frequency, so the ordinary cosine trial would fail.",
          "Use the resonant trial \\(y_p=Ax\\sin(2x)\\).",
          "Substitute to determine \\(A=1/4\\), then combine homogeneous and particular parts."
        ),
        "y=C_1\\cos(2x)+C_2\\sin(2x)+\\frac{x}{4}\\sin(2x)"
      ),
      mock_question(
        "Differential equations · 20 marks", "Eigenvalue classification of a flow",
        "\\mathbf{x}'=\\begin{bmatrix}3&1\\\\-2&0\\end{bmatrix}\\mathbf{x}",
        c(
          "Compute \\(\\det(A-\\lambda I)=\\lambda^2-3\\lambda+2\\).",
          "Factor the polynomial to obtain \\(\\lambda=1\\) and \\(\\lambda=2\\).",
          "Both eigenvalues are real, positive, and distinct.",
          "Translate those three facts into the phase-portrait classification and stability."
        ),
        "\\text{The origin is an unstable node.}"
      ),
      mock_question(
        "Laplace transforms · 20 marks", "Recognize a frequency shift",
        "\\mathcal{L}^{-1}\\!\\left\\{\\frac{s+1}{s^2+2s+10}\\right\\}",
        c(
          "Complete the square: \\(s^2+2s+10=(s+1)^2+9\\).",
          "The numerator is already the matching shifted variable \\(s+1\\).",
          "Match the fraction to the cosine transform with angular frequency \\(3\\).",
          "Use the shift \\(s\\mapsto s+1\\) to supply the exponential factor."
        ),
        "f(t)=e^{-t}\\cos(3t)"
      ),
      mock_question(
        "Laplace transforms · 20 marks", "Integral equation with an exponential kernel",
        "y(t)-\\int_0^t e^{-(t-\\tau)}y(\\tau)\\,d\\tau=1",
        c(
          "Recognize the integral as \\(e^{-t}*y(t)\\).",
          "Transform to get \\(Y-Y/(s+1)=1/s\\).",
          "Factor \\(Y\\) and simplify \\(1-1/(s+1)=s/(s+1)\\).",
          "Solve for \\(Y=(s+1)/s^2\\), split it, and invert."
        ),
        "y(t)=1+t"
      ),
      mock_question(
        "Linear algebra · 20 marks", "Consistency and a determinant identity",
        "\\text{(a) }kx+y=1,\\;2x+2y=k.\\qquad \\text{(b) If }A\\in\\mathbb{R}^{3\\times3},\\;\\det\\!\\left(3A^{-1}A^T\\right)",
        c(
          "For part (a), compute the coefficient determinant \\(2(k-1)\\).",
          "When \\(k\\ne1\\), the system has one solution. When \\(k=1\\), compare the two left sides and right sides.",
          "For part (b), use \\(\\det(3M)=3^3\\det(M)\\) for a three-by-three matrix.",
          "Then apply \\(\\det(A^{-1})\\det(A^T)=\\det(A)^{-1}\\det(A)=1\\)."
        ),
        "\\text{(a) One solution for }k\\ne1;\\;\\text{no solution for }k=1.\\qquad \\text{(b) }27"
      )
    )
  ),
  "Version 5" = list(
    subtitle = "Challenge rehearsal",
    emphasis = "Combines a repeated operator, nonlinear order reduction, partial fractions, an impulse response, and spectral matrix functions.",
    questions = list(
      mock_question(
        "Differential equations · 20 marks", "Repeated operator with matching forcing",
        "y''+2y'+y=xe^{-x}",
        c(
          "Recognize the left side as \\((D+1)^2y\\).",
          "Set \\(y=e^{-x}v(x)\\); then \\((D+1)^2y=e^{-x}v''\\).",
          "Cancel \\(e^{-x}\\) to obtain \\(v''=x\\).",
          "Integrate twice and substitute back."
        ),
        "y=e^{-x}\\left(C_1+C_2x+\\frac{x^3}{6}\\right)"
      ),
      mock_question(
        "Differential equations · 20 marks", "Nonlinear equation missing the dependent variable",
        "y''+(y')^2=0,\\qquad y(0)=0,\\quad y'(0)=1",
        c(
          "Let \\(p(x)=y'(x)\\), reducing the equation to \\(p'+p^2=0\\).",
          "Separate variables: \\(dp/p^2=-dx\\).",
          "Use \\(p(0)=1\\) to obtain \\(p=1/(x+1)\\).",
          "Integrate once more and use \\(y(0)=0\\)."
        ),
        "y(x)=\\ln(x+1)"
      ),
      mock_question(
        "Laplace transforms · 20 marks", "Inverse transform and convolution structure",
        "\\mathcal{L}^{-1}\\!\\left\\{\\frac{1}{s^2(s+2)}\\right\\}",
        c(
          "Recognize the product \\((1/s^2)(1/(s+2))\\) as the transform of \\(t*e^{-2t}\\).",
          "For a direct calculation, write \\(1/[s^2(s+2)]=A/s+B/s^2+C/(s+2)\\).",
          "Solve for \\(A=-1/4\\), \\(B=1/2\\), and \\(C=1/4\\).",
          "Invert the three standard transform pairs."
        ),
        "f(t)=\\frac{t}{2}-\\frac14+\\frac14e^{-2t}"
      ),
      mock_question(
        "Laplace transforms · 20 marks", "Zero-state response to a delayed impulse",
        "y''+y=\\delta\\!\\left(t-\\frac{\\pi}{2}\\right),\\qquad y(0)=0,\\quad y'(0)=0",
        c(
          "Transform the equation to obtain \\((s^2+1)Y=e^{-\\pi s/2}\\).",
          "The undelayed inverse of \\(1/(s^2+1)\\) is \\(\\sin t\\).",
          "Apply a delay of \\(\\pi/2\\).",
          "Multiply the delayed response by the matching unit step."
        ),
        "y(t)=\\sin\\!\\left(t-\\frac{\\pi}{2}\\right)u\\!\\left(t-\\frac{\\pi}{2}\\right)"
      ),
      mock_question(
        "Linear algebra · 20 marks", "A matrix function from spectral data",
        "A=\\begin{bmatrix}13&12\\\\12&13\\end{bmatrix},\\qquad \\text{find the principal }A^{1/2}",
        c(
          "Use the symmetric eigen-directions \\((1,1)^T\\) and \\((1,-1)^T\\).",
          "Their eigenvalues are \\(25\\) and \\(1\\).",
          "Take the principal square roots \\(5\\) and \\(1\\).",
          "Reconstruct the matrix with the same orthonormal eigenvectors."
        ),
        "A^{1/2}=\\begin{bmatrix}3&2\\\\2&3\\end{bmatrix}"
      )
    )
  )
)

mock_exam_question_ui <- function(question, number) {
  tutorial <- question$tutorial
  video <- tutorial$video
  div(
    class = "mock-question",
    div(
      class = "mock-question-heading",
      div(
        span(class = "mock-question-number", sprintf("Question %d", number)),
        span(class = "mock-question-topic", question$topic)
      ),
      span(class = "mock-question-marks", "20 marks")
    ),
    h3(question$title),
    math_block(question$prompt, paste("Mock exam question", number)),
    div(
      class = "mock-video-match",
      div(
        class = "mock-video-copy",
        div(class = "eyebrow", "Tutorial refresh · best catalog match"),
        strong(paste(video$creator, "·", video$title)),
        tags$p(tutorial$reason)
      ),
      tags$a(
        class = "btn video-link mock-video-link",
        href = video$url, target = "_blank", rel = "noopener noreferrer",
        "Watch matching tutorial ↗"
      )
    ),
    tags$details(
      class = "solution-disclosure",
      tags$summary("Reveal step-by-step solution hint"),
      div(
        class = "solution-body",
        div(class = "eyebrow", "Solve this problem step by step"),
        tags$ol(
          class = "learning-path",
          lapply(question$steps, function(step) tags$li(withMathJax(HTML(step))))
        ),
        tags$h4("Final answer"),
        math_block(question$answer, paste("Final answer for mock exam question", number))
      )
    )
  )
}

source_prompt_story <- list(
  list(
    phase = "01 · Foundation", title = "Build the first Laplace lab",
    prompts = c(
      "Build me a simple and straightforward app based on Shiny and R scripts that explores Laplace transforms with neat dynamic graphs and interactive elements. Make sure it is in dark mode.",
      "Please open it for me under localhost."
    ),
    outcome = "Created a self-contained Shiny app with reactive signal controls, time-domain and s-domain plots, transform pairs, and a shifting-property experiment."
  ),
  list(
    phase = "02 · Learning design", title = "Explain how the app teaches",
    prompts = c(
      "Tell me how this app helps me learn Laplace.",
      "Add this introduction into the app and make an About page that acts as the home page."
    ),
    outcome = "Added a learner-first home page, a recommended path through the labs, and explanations that connect each interaction to a mathematical idea."
  ),
  list(
    phase = "03 · Mechanical engineering", title = "Turn theory into a suspension model",
    prompts = c(
      "What is an interesting real-life application of the Laplace transform in engineering? Make an interactive page that explores such an example.",
      "Add approximate suspension design specifics for a Subaru Outback, Ford F-150, and Honda CR-V."
    ),
    outcome = "Built an interactive mass–spring–damper suspension model with vehicle presets, displacement and frequency-response plots, damping metrics, and educational design caveats."
  ),
  list(
    phase = "04 · Plain-language guidance", title = "Make technical terms approachable",
    prompts = c(
      "Add a hidden description sign beside “Frequency response of the suspension” with an explain-like-I’m-five description. Look for other technical definitions and do the same.",
      "Label vehicle mass, spring stiffness, damping c, and sudden road force. Add an instruction manual explaining what to look for when designing a suspension.",
      "Move the Suspension Design Field Guide section to the top."
    ),
    outcome = "Added keyboard-accessible hover definitions, plain-language labels, and a prominent suspension field guide explaining comfort, settling, resonance, and the effect of low damping."
  ),
  list(
    phase = "05 · More disciplines", title = "Expand the engineering examples",
    prompts = c(
      "Turn the current engineering example into a subpage and let the user select examples from different disciplines. Add one chemical-engineering example and one civil-engineering example.",
      "Have the other subpages mimic the first one: add a design guide and three real-life specifics for each."
    ),
    outcome = "Added chemical mixing-tank and civil building-vibration labs, each with interactive controls, a design guide, three realistic presets, and model limitations."
  ),
  list(
    phase = "06 · Exam analysis", title = "Grow from one topic into a course app",
    prompts = c(
      "I have previous final exams and Laplace transforms are only one of the concepts I need to learn. Analyze the problems and make the ultimate app for mastering the final exam.",
      "Make sure the new concept pages follow the same format as the Laplace page."
    ),
    outcome = "Analyzed the supplied 2020–2025 finals and expanded the app with differential-equation and linear-algebra modules that mirror the same overview, exploration, application, and review structure."
  ),
  list(
    phase = "07 · Information architecture", title = "Restructure the course navigation",
    prompts = c(
      "Wrap the Laplace Transform Lab into a subpage alongside About, Transform Explorer, Property Lab, and Engineering Example.",
      "Make a new About page that explains the intention and purpose of this app."
    ),
    outcome = "Introduced a course-level home page and nested topic modules so the app could scale without flattening every activity into one navigation bar."
  ),
  list(
    phase = "08 · Formula presentation", title = "Create a readable formula library",
    prompts = c(
      "Refresh the quick-reference page with key formulas from the newly added topics.",
      "For the formulas, add an easy-to-read traditional-style picture for each of them.",
      "Make sure all formulas use an easy-to-read traditional style."
    ),
    outcome = "Created a multi-topic Formula Library and rendered mathematical notation consistently with MathJax in traditional textbook form."
  ),
  list(
    phase = "09 · Applied mathematics", title = "Add engineering labs to every core topic",
    prompts = c(
      "The differential-equations and linear-algebra pages lack an Engineering Example subpage. Add similar interactive example pages for the newly added topics."
    ),
    outcome = "Added a thermal-response lab for differential equations and a truss-force lab for linear algebra, including presets, plots, design interpretation, and real-world context."
  ),
  list(
    phase = "10 · Guided practice", title = "Build an exam coach",
    prompts = c(
      "For the Show Solution Guide function, infuse it with the prompt “solve this problem step by step” and display the answer."
    ),
    outcome = "Built a topic-and-difficulty practice generator with staged solution reasoning, final answers, and an exam-frequency roadmap based on the supplied papers."
  ),
  list(
    phase = "11 · Publishing", title = "Make the project shareable",
    prompts = c(
      "Upload this app to my GitHub repository and give me the link.",
      "I connected my GitHub repository to Posit Cloud. Can you look into it?",
      "Show me a public link that I can share."
    ),
    outcome = "Published the source to GitHub, connected the main branch to Posit Connect Cloud, and established the public URL used for every later release."
  ),
  list(
    phase = "12 · Video curriculum", title = "Match lessons to concepts",
    prompts = c(
      "For each core concept, add a subpage containing relevant videos from the Organic Chemistry Tutor’s playlist.",
      "I only want an exact video-link match to the concept. If there is none, find a helpful YouTube tutorial from another channel."
    ),
    outcome = "Added direct concept-matched video libraries, prioritizing Organic Chemistry Tutor and filling genuine gaps with carefully selected lessons from other established educators."
  ),
  list(
    phase = "13 · Navigation and visual system", title = "Make the hierarchy intuitive",
    prompts = c(
      "The main pages, their subpages, and their navigation components seem messy. Make them more intuitive while using a Baroque colour scheme."
    ),
    outcome = "Standardized every module into Overview → Explore → Watch → Apply, separated primary and secondary navigation, and introduced the decorative visual system that later evolved into the current themes."
  ),
  list(
    phase = "14 · Interface modes", title = "Create Dark Mode and Bright Mode",
    prompts = c(
      "Using the same UI arrangement, add a legacy mode that looks like an early Internet Explorer website and add a top-right mode switch.",
      "Turn the modern UI into true Dark Mode and rename the legacy UI Bright Mode.",
      "Dark Mode should use a black-and-blue colour scheme, not gold."
    ),
    outcome = "Created a switchable black-and-blue Dark Mode and a bright classic-Windows interface with faux Internet Explorer chrome, while preserving the same pages, controls, and navigation state."
  ),
  list(
    phase = "15 · Exam simulation", title = "Generate five full practice finals",
    prompts = c(
      "Under the Exam Practice page, make a subpage called Mockup Exam with five predicted versions. Put a step-by-step solution hint under every question, hidden until the user clicks.",
      "I only see Version 1.",
      "For all mock-exam versions, match each question to a relevant tutorial video from the catalog so the user can easily refresh their knowledge."
    ),
    outcome = "Added five newly written 100-mark mock exams modeled on recurring 2020–2025 patterns, with 25 questions and a native click-to-reveal solution guide beneath every problem. Replaced the compact dropdown with five always-visible version buttons and matched every question to its closest lesson in the app's tutorial catalog."
  )
)

source_prompts_page <- function() {
  div(
    class = "source-prompts-page",
    div(
      class = "card prompt-intro",
      div(class = "eyebrow", "Build history · curated transcript"),
      h2("Source prompts"),
      tags$p(
        "A readable reconstruction of the prompts that shaped this app—from the first Laplace graph ",
        "to the exam coach, engineering labs, publishing workflow, video curriculum, and interface modes."
      ),
      div(
        class = "prompt-note",
        strong("Reading note"),
        span(
          "The supplied 3.5 MB export was cleaned for presentation. Environment dumps, tool calls, ",
          "token counters, repeated status messages, and encrypted-reasoning placeholders are intentionally omitted. ",
          "The user prompts and resulting product decisions are preserved."
        )
      ),
      div(
        class = "prompt-stats",
        div(strong("15"), span("build milestones")),
        div(strong("36"), span("source requests reviewed")),
        div(strong("2020–2025"), span("finals represented"))
      )
    ),
    div(
      class = "chat-window",
      div(
        class = "chat-window-bar",
        div(class = "chat-window-mark", "◎"),
        div(
          strong("How FinalPrep Interactive was made"),
          span("A prompt-to-product conversation")
        ),
        div(class = "chat-window-status", "Published")
      ),
      div(
        class = "chat-thread",
        lapply(source_prompt_story, function(item) {
          tagList(
            div(class = "prompt-phase", item$phase, " · ", item$title),
            div(
              class = "chat-message user-message",
              div(class = "chat-avatar user-avatar", "U"),
              div(
                class = "chat-message-content",
                div(class = "chat-role", "You"),
                lapply(item$prompts, tags$p)
              )
            ),
            div(
              class = "chat-message assistant-message",
              div(class = "chat-avatar assistant-avatar", "C"),
              div(
                class = "chat-message-content",
                div(class = "chat-role", "Codex · build outcome"),
                tags$p(item$outcome)
              )
            )
          )
        })
      ),
      div(
        class = "chat-composer",
        span("This transcript documents the finished build."),
        span(class = "composer-button", "✓")
      )
    )
  )
}

ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      :root { color-scheme: dark; --bg:#000000; --panel:#070b10; --panel2:#0b1119;
        --text:#eef6ff; --muted:#9aaec2; --cyan:#4da3ff; --violet:#6f7cff;
        --green:#23b8d1; --border:#1f3d5c; --wine:#0b2945; --wine-deep:#061625;
        --gold-soft:#b9dcff; --ink:#03080d; }
      * { box-sizing:border-box; }
      body {
        background:
          radial-gradient(circle at 18% -8%,rgba(28,94,160,.36) 0,transparent 36%),
          radial-gradient(circle at 88% 12%,rgba(50,130,210,.16) 0,transparent 30%),
          linear-gradient(145deg,#02070c 0,var(--bg) 58%,#030a11 100%);
        color:var(--text); font-family:Inter,system-ui,-apple-system,sans-serif;
      }
      .container-fluid { padding:0; }
      .app-shell { min-height:100vh; }
      .hero { position:relative; padding:38px max(24px,calc((100vw - 1240px)/2));
        border-bottom:3px double rgba(77,163,255,.55);
        background:linear-gradient(90deg,rgba(6,30,53,.88),rgba(3,8,13,.45) 62%,rgba(10,42,72,.22)); }
      .hero::after { content:'❦'; position:absolute; left:50%; bottom:-17px; transform:translateX(-50%);
        width:44px; height:30px; display:flex; align-items:center; justify-content:center;
        color:var(--cyan); background:var(--bg); font-family:Georgia,serif; font-size:22px; }
      .eyebrow { color:var(--cyan); text-transform:uppercase; letter-spacing:.16em;
        font-weight:700; font-size:12px; }
      h1,h2,h3,h4 { font-family:Georgia,'Times New Roman',serif; }
      h1 { margin:8px 0 7px; color:var(--gold-soft); font-size:clamp(29px,4vw,48px);
        font-weight:700; letter-spacing:-.025em; text-shadow:0 2px 18px rgba(0,0,0,.4); }
      .subtitle { color:var(--muted); max-width:700px; font-size:16px; }
      .content { max-width:1240px; margin:auto; padding:30px 24px 40px; }
      #main_navigation { position:sticky; top:10px; z-index:70; display:grid;
        grid-template-columns:repeat(7,minmax(0,1fr)); gap:8px; float:none; padding:9px;
        margin:0 0 26px; border:1px solid rgba(77,163,255,.42); border-radius:15px;
        background:rgba(16,10,16,.93); box-shadow:0 14px 34px rgba(0,0,0,.38);
        backdrop-filter:blur(18px); }
      #main_navigation::before,#main_navigation::after,
      #laplace_navigation::before,#laplace_navigation::after,
      #differential_navigation::before,#differential_navigation::after,
      #linear_algebra_navigation::before,#linear_algebra_navigation::after,
      #reference_navigation::before,#reference_navigation::after,
      #exam_navigation::before,#exam_navigation::after,
      #engineering_navigation::before,#engineering_navigation::after { display:none; content:none; }
      #main_navigation>li { float:none; margin:0; }
      #main_navigation>li>a { display:flex; min-height:52px; align-items:center; justify-content:center;
        padding:10px 8px; color:var(--muted); text-align:center; line-height:1.2;
        border:1px solid transparent!important; border-radius:10px; background:transparent; }
      #main_navigation>li.active>a,#main_navigation>li.active>a:hover,#main_navigation>li>a:hover,
      #main_navigation>li>a:focus { color:#eef6ff; background:linear-gradient(145deg,var(--wine),#0a2036);
        border-color:rgba(77,163,255,.78)!important; box-shadow:inset 0 0 0 1px rgba(185,220,255,.12); }
      #laplace_navigation,#differential_navigation,#linear_algebra_navigation,#reference_navigation,
      #exam_navigation {
        display:flex; flex-wrap:wrap; gap:8px; padding:9px; margin:0 0 22px;
        border:1px solid rgba(31,61,92,.9); border-radius:13px;
        background:linear-gradient(90deg,rgba(8,38,65,.72),rgba(3,11,18,.92)); }
      #laplace_navigation>li,#differential_navigation>li,#linear_algebra_navigation>li,
      #reference_navigation>li,#exam_navigation>li { float:none; flex:1 1 165px; margin:0; }
      #laplace_navigation>li>a,#differential_navigation>li>a,#linear_algebra_navigation>li>a,
      #reference_navigation>li>a,#exam_navigation>li>a { display:flex; min-height:48px; align-items:center; justify-content:center;
        padding:9px 12px; color:var(--muted); text-align:center; line-height:1.25;
        border:1px solid rgba(31,61,92,.9); border-radius:9px; background:rgba(3,10,16,.62); }
      #laplace_navigation>li.active>a,#differential_navigation>li.active>a,
      #linear_algebra_navigation>li.active>a,#reference_navigation>li.active>a,#exam_navigation>li.active>a,
      #laplace_navigation>li>a:hover,#differential_navigation>li>a:hover,
      #linear_algebra_navigation>li>a:hover,#reference_navigation>li>a:hover,#exam_navigation>li>a:hover {
        color:#eef6ff; border-color:var(--cyan); background:linear-gradient(145deg,#0f2a44,#071625);
        box-shadow:inset 0 -2px 0 var(--cyan); }
      #engineering_navigation { display:flex; flex-wrap:wrap; gap:8px; padding:7px;
        margin:0 0 20px; border-bottom:1px solid var(--border); }
      #engineering_navigation>li { float:none; margin:0; }
      #engineering_navigation>li>a { color:var(--muted); border:0; border-radius:8px; }
      #engineering_navigation>li.active>a,#engineering_navigation>li>a:hover {
        color:var(--gold-soft); background:rgba(77,163,255,.18); }
      .module-heading { display:grid; grid-template-columns:auto 1fr; gap:18px; align-items:center;
        margin:2px 0 18px; padding:19px 21px; border:1px solid rgba(77,163,255,.38);
        border-radius:15px; background:linear-gradient(115deg,rgba(11,52,88,.48),rgba(4,12,20,.92) 48%,rgba(13,54,91,.24)); }
      .module-seal { width:54px; height:54px; display:flex; align-items:center; justify-content:center;
        border:1px solid var(--cyan); border-radius:50%; color:var(--gold-soft);
        background:radial-gradient(circle,#123f68 0,#061625 70%); font-family:Georgia,serif;
        font-size:22px; box-shadow:0 0 0 4px rgba(77,163,255,.09); }
      .module-kicker { color:var(--cyan); text-transform:uppercase; letter-spacing:.14em;
        font-size:11px; font-weight:750; }
      .module-heading h2 { margin:3px 0 4px; color:var(--gold-soft); font-size:25px; }
      .module-heading p { margin:0; color:var(--muted); }
      .module-route { margin-top:8px; color:#b7cde4; font-size:12px; letter-spacing:.04em; }
      .card { position:relative;
        background:linear-gradient(145deg,rgba(8,16,25,.98),rgba(3,8,13,.98));
        border:1px solid rgba(31,61,92,.82); border-radius:14px; padding:20px; margin-bottom:20px;
        box-shadow:0 14px 35px rgba(0,0,0,.28); }
      .card::before { content:''; position:absolute; left:12px; right:12px; top:0; height:1px;
        background:linear-gradient(90deg,transparent,rgba(77,163,255,.35),transparent); }
      .card h3 { margin:0 0 14px; font-size:17px; }
      .control-label { color:#c7d8e8; font-size:13px; margin-bottom:7px; }
      .form-control, .selectize-input, .selectize-control.single .selectize-input.input-active {
        background:#04080d!important; color:var(--text)!important; border:1px solid var(--border)!important;
        border-radius:9px; box-shadow:none!important; }
      .selectize-dropdown { background:#04080d; color:var(--text); border-color:var(--border); }
      .selectize-dropdown .active { background:var(--wine); color:white; }
      .irs--shiny .irs-bar { background:linear-gradient(90deg,var(--cyan),var(--violet)); border:0; }
      .irs--shiny .irs-handle { background:var(--text); border:2px solid var(--cyan); }
      .irs--shiny .irs-line { background:#17314a; border:0; }
      .irs--shiny .irs-single { background:var(--cyan); color:#020a12; }
      .formula { padding:18px 20px; border-left:3px solid var(--violet); background:#04080d;
        border-radius:0 11px 11px 0; font-family:Cambria Math,serif; font-size:18px;
        overflow-wrap:anywhere; }
      .formula + .formula { margin-top:10px; border-color:var(--cyan); }
      .plot-wrap { min-height:330px; }
      .hint { color:var(--muted); font-size:13px; line-height:1.6; }
      .concept { display:grid; grid-template-columns:repeat(3,1fr); gap:16px; }
      .concept .card { margin:0; }
      .concept strong { color:var(--cyan); display:block; margin-bottom:7px; }
      .video-grid { display:grid; grid-template-columns:repeat(2,minmax(0,1fr)); gap:16px; }
      .video-grid .card { margin:0; }
      .video-card { display:flex; flex-direction:column; min-height:220px; }
      .video-card p { color:#c7d8e8; line-height:1.6; flex:1; }
      .video-number { color:var(--violet); font-weight:800; letter-spacing:.12em;
        font-size:12px; margin-bottom:10px; }
      .video-source { color:var(--cyan); font-size:12px; font-weight:750;
        letter-spacing:.08em; margin-bottom:7px; text-transform:uppercase; }
      .video-link { align-self:flex-start; color:#020a12!important; background:var(--cyan)!important;
        border:0!important; border-radius:9px!important; font-weight:750; padding:9px 13px!important; }
      .video-link:hover,.video-link:focus { background:var(--gold-soft)!important; color:#020a12!important; }
      .learning-path { margin:0; padding-left:22px; color:#d3e5f5; }
      .learning-path li { padding:6px 0 6px 5px; }
      .learning-path li::marker { color:var(--violet); font-weight:700; }
      .metric-row { display:grid; grid-template-columns:repeat(3,1fr); gap:12px; margin-bottom:20px; }
      .metric { background:#04080d; border:1px solid var(--border); border-radius:12px; padding:14px; }
      .metric span { display:block; color:var(--muted); font-size:12px; margin-bottom:5px; }
      .metric strong { color:var(--text); font-size:18px; font-variant-numeric:tabular-nums; }
      .preset-actions { display:flex; flex-wrap:wrap; gap:8px; margin-bottom:18px; }
      .preset-actions .btn { color:var(--text); background:#0a1a2a; border:1px solid var(--border);
        border-radius:9px; }
      .preset-actions .btn:hover,.preset-actions .btn:focus { color:white; background:var(--wine);
        border-color:var(--cyan); }
      .source-link { color:var(--cyan); }
      .help-tip { position:relative; display:inline-flex; align-items:center; justify-content:center;
        width:18px; height:18px; margin-left:6px; border:1px solid var(--cyan);
        border-radius:50%; color:var(--cyan); font-family:system-ui,sans-serif; font-size:12px;
        font-weight:700; cursor:help; vertical-align:2px; outline-offset:3px; }
      .help-tip::after { content:attr(data-tip); position:absolute; z-index:100; left:50%; bottom:calc(100% + 10px);
        width:min(280px,75vw); padding:11px 13px; border:1px solid var(--border); border-radius:10px;
        background:#02070c; color:var(--text); font-size:13px; font-weight:400; line-height:1.45;
        font-family:Inter,system-ui,sans-serif; text-align:left; box-shadow:0 12px 30px rgba(0,0,0,.45);
        opacity:0; visibility:hidden; pointer-events:none; transform:translate(-50%,5px);
        transition:opacity .16s ease,transform .16s ease,visibility .16s; }
      .help-tip:hover::after,.help-tip:focus::after { opacity:1; visibility:visible; transform:translate(-50%,0); }
      .math-formula { overflow-x:auto; overflow-y:hidden; }
      .math-formula .MathJax_Display { margin:0!important; text-align:left!important; }
      .math-inline { white-space:nowrap; }
      table { width:100%; color:#d3e5f5; }
      th { color:var(--muted); font-weight:600; text-align:left; }
      th,td { padding:12px 10px; border-bottom:1px solid var(--border); }
      code { background:#03080d; color:#8fc7ff; padding:3px 6px; }
      a { color:var(--cyan); }
      .theme-toggle { position:absolute; z-index:5; top:28px;
        right:max(24px,calc((100vw - 1240px)/2)); min-width:118px; padding:9px 13px;
        color:#eef6ff; background:linear-gradient(145deg,var(--wine),#0a2036);
        border:1px solid var(--cyan); border-radius:9px; font-weight:750;
        box-shadow:0 6px 18px rgba(0,0,0,.32); }
      .theme-toggle:hover,.theme-toggle:focus { color:#020a12; background:var(--gold-soft);
        outline:2px solid rgba(185,220,255,.28); outline-offset:2px; }
      .legacy-browser-chrome { display:none; }
      .mock-exam-intro { overflow:hidden; }
      .mock-exam-instructions { display:grid; gap:5px; margin:18px 0; padding:14px 16px;
        color:#c9ddf0; background:#071b2d; border:1px solid rgba(77,163,255,.38);
        border-left:4px solid var(--cyan); border-radius:9px; line-height:1.55; }
      .mock-exam-instructions strong { color:#eef6ff; }
      .mock-version-picker { margin-top:17px; }
      .mock-version-picker .control-label { display:block; margin-bottom:9px; }
      .mock-version-picker .shiny-options-group { display:grid;
        grid-template-columns:repeat(5,minmax(0,1fr)); gap:8px; }
      .mock-version-picker .radio-inline { display:flex; align-items:center; min-height:42px;
        margin:0; padding:8px 10px; color:#c8d9e9; background:#050b11;
        border:1px solid #29465f; border-radius:9px; cursor:pointer; }
      .mock-version-picker .radio-inline:hover { color:#fff; border-color:var(--cyan);
        background:#0b2945; }
      .mock-version-picker .radio-inline input { position:static; margin:0 8px 0 0;
        accent-color:#4da3ff; }
      .mock-version-picker .radio-inline:has(input:checked) { color:#fff;
        background:#0b2945; border-color:var(--cyan); box-shadow:inset 0 -2px 0 var(--cyan); }
      .mock-exam-banner { display:grid; grid-template-columns:minmax(0,1fr) auto; gap:20px;
        align-items:center; margin:0 0 18px; padding:18px 20px; color:#d8e8f7;
        background:linear-gradient(120deg,#0b2945,#06101a); border:1px solid #27557c;
        border-radius:13px; }
      .mock-exam-banner h3 { margin:4px 0 5px; color:#eef6ff; }
      .mock-exam-banner p { margin:0; color:#b8cee1; }
      .mock-exam-balance { display:flex; flex-wrap:wrap; justify-content:flex-end; gap:7px;
        max-width:360px; }
      .mock-exam-balance span,.mock-question-marks { padding:6px 9px; color:#b9dcff;
        background:#061625; border:1px solid #2c6090; border-radius:999px; font-size:11px;
        font-weight:750; white-space:nowrap; }
      .mock-question { position:relative; margin:0 0 18px; padding:21px;
        background:linear-gradient(145deg,rgba(9,18,28,.98),rgba(3,8,13,.98));
        border:1px solid #1d3c58; border-radius:14px; box-shadow:0 12px 32px rgba(0,0,0,.28); }
      .mock-question::before { content:''; position:absolute; top:0; bottom:0; left:0;
        width:4px; background:linear-gradient(var(--cyan),var(--violet)); border-radius:14px 0 0 14px; }
      .mock-question-heading { display:flex; align-items:flex-start; justify-content:space-between;
        gap:14px; margin-bottom:10px; }
      .mock-question-number { display:block; color:var(--cyan); font-size:12px; font-weight:800;
        letter-spacing:.1em; text-transform:uppercase; }
      .mock-question-topic { display:block; margin-top:4px; color:var(--muted); font-size:12px; }
      .mock-question h3 { margin:0 0 13px; color:#eef6ff; font-size:19px; }
      .mock-question>.formula { margin-bottom:16px; }
      .mock-video-match { display:grid; grid-template-columns:minmax(0,1fr) auto; gap:16px;
        align-items:center; margin:0 0 16px; padding:14px 15px; background:#071524;
        border:1px solid #244d70; border-radius:10px; }
      .mock-video-copy strong { display:block; margin:4px 0 5px; color:#e9f4ff; }
      .mock-video-copy p { margin:0; color:#adc3d7; font-size:13px; line-height:1.55; }
      .mock-video-link { flex:0 0 auto; margin:0; white-space:nowrap; }
      .solution-disclosure { overflow:hidden; border:1px solid #29465f; border-radius:10px;
        background:#050a0f; }
      .solution-disclosure summary { padding:12px 14px; color:#b9dcff; cursor:pointer;
        font-weight:750; user-select:none; }
      .solution-disclosure summary:hover,.solution-disclosure summary:focus {
        color:#fff; background:#0b2945; }
      .solution-disclosure[open] summary { color:#fff; background:#0b2945;
        border-bottom:1px solid #29465f; }
      .solution-body { padding:16px 18px 18px; }
      .solution-body .learning-path { margin:10px 0 17px; }
      .solution-body h4 { margin:5px 0 10px; color:#eef6ff; }
      .source-prompts-page { max-width:1100px; margin:0 auto; }
      .prompt-intro { overflow:hidden; }
      .prompt-note { display:grid; gap:5px; margin:18px 0; padding:14px 16px;
        color:#c9ddf0; background:#071b2d; border:1px solid rgba(77,163,255,.38);
        border-left:4px solid var(--cyan); border-radius:9px; line-height:1.55; }
      .prompt-note strong { color:#eef6ff; }
      .prompt-stats { display:grid; grid-template-columns:repeat(3,minmax(0,1fr));
        gap:12px; margin-top:18px; }
      .prompt-stats>div { padding:15px 16px; background:#04080d;
        border:1px solid var(--border); border-radius:10px; }
      .prompt-stats strong { display:block; color:var(--cyan); font-family:Georgia,serif;
        font-size:24px; line-height:1.1; }
      .prompt-stats span { display:block; margin-top:5px; color:var(--muted); font-size:12px; }
      .chat-window { overflow:hidden; background:#05080c; border:1px solid #234563;
        border-radius:15px; box-shadow:0 20px 50px rgba(0,0,0,.48); }
      .chat-window-bar { display:grid; grid-template-columns:auto 1fr auto; gap:12px;
        align-items:center; padding:14px 18px; color:#edf6ff; background:#0b1119;
        border-bottom:1px solid #1f3d5c; }
      .chat-window-mark { width:34px; height:34px; display:flex; align-items:center;
        justify-content:center; color:#fff; background:#15558e; border:1px solid #4da3ff;
        border-radius:50%; font-size:20px; }
      .chat-window-bar strong,.chat-window-bar span { display:block; }
      .chat-window-bar span { color:var(--muted); font-size:12px; }
      .chat-window-status { padding:5px 9px; color:#9ed1ff!important;
        background:#0b2945; border:1px solid #245f92; border-radius:999px; }
      .prompt-phase { padding:10px 24px; color:#7fc0ff; background:#06101a;
        border-bottom:1px solid #16324a; text-transform:uppercase; letter-spacing:.1em;
        font-size:10px; font-weight:800; }
      .chat-message { display:grid; grid-template-columns:42px minmax(0,1fr); gap:14px;
        padding:20px 24px; border-bottom:1px solid #162b3e; }
      .user-message { background:#0b1621; }
      .assistant-message { background:#05080c; }
      .chat-avatar { width:36px; height:36px; display:flex; align-items:center;
        justify-content:center; border-radius:50%; color:#fff; font-size:12px; font-weight:800; }
      .user-avatar { background:#15558e; border:1px solid #4da3ff; }
      .assistant-avatar { color:#c9ebff; background:#16344f; border:1px solid #2e709f; }
      .chat-role { margin:1px 0 8px; color:#edf6ff; font-weight:750; }
      .chat-message-content p { margin:0; color:#cbdced; line-height:1.65; }
      .chat-message-content p+p { margin-top:11px; }
      .chat-composer { display:flex; align-items:center; justify-content:space-between;
        gap:12px; margin:18px; padding:12px 14px; color:#8097aa; background:#070d13;
        border:1px solid #29465f; border-radius:12px; font-size:13px; }
      .composer-button { width:28px; height:28px; display:flex; align-items:center;
        justify-content:center; flex:0 0 auto; color:#02101c; background:#4da3ff;
        border-radius:50%; font-weight:900; }

      /* True dark mode: near-black structure with layered blue accents. */
      body:not(.legacy-mode) {
        background:
          radial-gradient(circle at 18% -8%,rgba(28,94,160,.23) 0,transparent 34%),
          radial-gradient(circle at 88% 12%,rgba(50,130,210,.12) 0,transparent 28%),
          #000;
      }
      body:not(.legacy-mode) .hero {
        background:linear-gradient(90deg,#070709,#000 64%,#080706);
        border-bottom-color:rgba(77,163,255,.48);
      }
      body:not(.legacy-mode) .hero::after { background:#000; }
      body:not(.legacy-mode) .theme-toggle {
        background:linear-gradient(145deg,#151518,#070708);
        border-color:rgba(77,163,255,.78); box-shadow:0 7px 22px rgba(0,0,0,.68);
      }
      body:not(.legacy-mode) #main_navigation {
        background:rgba(3,6,10,.97); border-color:rgba(77,163,255,.34);
        box-shadow:0 14px 38px rgba(0,0,0,.72);
      }
      body:not(.legacy-mode) #main_navigation>li.active>a,
      body:not(.legacy-mode) #main_navigation>li.active>a:hover,
      body:not(.legacy-mode) #main_navigation>li>a:hover,
      body:not(.legacy-mode) #main_navigation>li>a:focus {
        color:#fff7e4; background:linear-gradient(145deg,#18181b,#09090a);
        border-color:var(--cyan)!important; box-shadow:inset 0 -2px 0 var(--cyan);
      }
      body:not(.legacy-mode) #laplace_navigation,
      body:not(.legacy-mode) #differential_navigation,
      body:not(.legacy-mode) #linear_algebra_navigation,
      body:not(.legacy-mode) #reference_navigation,
      body:not(.legacy-mode) #exam_navigation {
        background:linear-gradient(90deg,#08080a,#030304);
        border-color:rgba(77,163,255,.28);
      }
      body:not(.legacy-mode) #laplace_navigation>li>a,
      body:not(.legacy-mode) #differential_navigation>li>a,
      body:not(.legacy-mode) #linear_algebra_navigation>li>a,
      body:not(.legacy-mode) #reference_navigation>li>a,
      body:not(.legacy-mode) #exam_navigation>li>a {
        background:#050506; border-color:#312b22;
      }
      body:not(.legacy-mode) #laplace_navigation>li.active>a,
      body:not(.legacy-mode) #differential_navigation>li.active>a,
      body:not(.legacy-mode) #linear_algebra_navigation>li.active>a,
      body:not(.legacy-mode) #reference_navigation>li.active>a,
      body:not(.legacy-mode) #exam_navigation>li.active>a,
      body:not(.legacy-mode) #laplace_navigation>li>a:hover,
      body:not(.legacy-mode) #differential_navigation>li>a:hover,
      body:not(.legacy-mode) #linear_algebra_navigation>li>a:hover,
      body:not(.legacy-mode) #reference_navigation>li>a:hover,
      body:not(.legacy-mode) #exam_navigation>li>a:hover {
        background:linear-gradient(145deg,#17171a,#080809);
      }
      body:not(.legacy-mode) #engineering_navigation>li.active>a,
      body:not(.legacy-mode) #engineering_navigation>li>a:hover { background:#0a1c2c; }
      body:not(.legacy-mode) .module-heading {
        background:linear-gradient(115deg,#0d0d10,#050506 55%,#0b0907);
        border-color:rgba(77,163,255,.3);
      }
      body:not(.legacy-mode) .module-seal {
        background:radial-gradient(circle,#0d3152 0,#050a10 72%);
      }
      body:not(.legacy-mode) .card {
        background:linear-gradient(145deg,rgba(13,14,17,.99),rgba(5,5,6,.99));
        border-color:#182e45; box-shadow:0 16px 40px rgba(0,0,0,.58);
      }
      body:not(.legacy-mode) .form-control,
      body:not(.legacy-mode) .selectize-input,
      body:not(.legacy-mode) .selectize-control.single .selectize-input.input-active,
      body:not(.legacy-mode) .selectize-dropdown,
      body:not(.legacy-mode) .formula,
      body:not(.legacy-mode) .metric { background:#040405!important; }
      body:not(.legacy-mode) .selectize-dropdown .active { background:#17171a; }
      body:not(.legacy-mode) .preset-actions .btn { background:#111114; }
      body:not(.legacy-mode) .preset-actions .btn:hover,
      body:not(.legacy-mode) .preset-actions .btn:focus { background:#1a1a1e; }
      body:not(.legacy-mode) .help-tip::after,
      body:not(.legacy-mode) code { background:#030304; }

      /* Bright mode keeps the information architecture but recreates a
         late-1990s/early-2000s Internet Explorer and Windows desktop aesthetic. */
      body.legacy-mode { background:#c0c0c0; color:#000; font-family:'Times New Roman',Times,serif; }
      .legacy-mode .legacy-browser-chrome { display:block; color:#000;
        font-family:'MS Sans Serif',Tahoma,Arial,sans-serif; font-size:12px;
        background:#c0c0c0; border:2px outset #fff; }
      .legacy-titlebar { display:flex; align-items:center; justify-content:space-between;
        min-height:25px; padding:3px 5px; color:#fff; font-weight:700;
        background:linear-gradient(90deg,#000080,#1084d0); }
      .legacy-window-buttons { display:flex; gap:3px; }
      .legacy-window-buttons span { width:18px; height:17px; display:flex; align-items:center;
        justify-content:center; color:#000; background:#c0c0c0; border:2px outset #fff;
        font-size:11px; line-height:1; }
      .legacy-menubar { display:flex; gap:18px; padding:4px 8px; border-bottom:1px solid #808080; }
      .legacy-menubar span::first-letter { text-decoration:underline; }
      .legacy-toolbar { display:flex; flex-wrap:wrap; gap:5px; align-items:center; padding:5px 7px;
        border-top:1px solid #fff; border-bottom:1px solid #808080; }
      .legacy-tool { min-width:58px; padding:4px 7px; text-align:center; background:#c0c0c0;
        border:2px outset #fff; }
      .legacy-address { display:grid; grid-template-columns:auto 1fr auto; gap:6px; align-items:center;
        padding:5px 7px; border-top:1px solid #fff; }
      .legacy-address-box { min-height:24px; padding:4px 7px; overflow:hidden; color:#000;
        white-space:nowrap; background:#fff; border:2px inset #fff; font-family:Arial,sans-serif; }
      .legacy-go { padding:3px 8px; background:#c0c0c0; border:2px outset #fff; }
      .legacy-mode .app-shell { min-height:calc(100vh - 100px); background:#fff; border:2px inset #fff; }
      .legacy-mode .hero { padding:25px max(18px,calc((100vw - 1240px)/2));
        background:#000080; border:0; border-bottom:4px ridge #c0c0c0; }
      .legacy-mode .hero::after { display:none; }
      .legacy-mode .hero .eyebrow { color:#ffff00; font-family:Arial,sans-serif; letter-spacing:.08em; }
      .legacy-mode h1 { max-width:calc(100% - 145px); color:#fff; font-family:'Times New Roman',Times,serif;
        font-size:clamp(28px,4vw,44px); text-shadow:none; letter-spacing:0; }
      .legacy-mode h2,.legacy-mode h3,.legacy-mode h4 {
        color:#000080; font-family:'Times New Roman',Times,serif; }
      .legacy-mode .subtitle { color:#fff; }
      .legacy-mode .theme-toggle { top:22px; color:#000; background:#c0c0c0;
        border:2px outset #fff; border-radius:0; box-shadow:none; font-family:Arial,sans-serif; }
      .legacy-mode .theme-toggle:hover,.legacy-mode .theme-toggle:focus {
        color:#000; background:#d4d0c8; border-style:inset; outline:1px dotted #000; }
      .legacy-mode .content { max-width:none; padding:12px; background:#fff; }
      .legacy-mode .content .row { margin-left:0; margin-right:0; }
      .legacy-mode #main_navigation { position:static; display:grid; top:auto; padding:4px;
        margin:0 0 10px; border:2px ridge #fff; border-radius:0; background:#c0c0c0;
        box-shadow:none; backdrop-filter:none; }
      .legacy-mode #main_navigation>li>a { min-height:42px; padding:7px 5px; color:#000;
        border:2px outset #fff!important; border-radius:0; background:#c0c0c0;
        font-family:Arial,sans-serif; font-size:13px; text-decoration:none; }
      .legacy-mode #main_navigation>li.active>a,.legacy-mode #main_navigation>li.active>a:hover,
      .legacy-mode #main_navigation>li>a:hover,.legacy-mode #main_navigation>li>a:focus {
        color:#fff; background:#000080; border:2px inset #fff!important; box-shadow:none; }
      .legacy-mode #laplace_navigation,.legacy-mode #differential_navigation,
      .legacy-mode #linear_algebra_navigation,.legacy-mode #reference_navigation,
      .legacy-mode #exam_navigation {
        gap:3px; padding:4px; margin:0 0 10px; border:2px groove #fff; border-radius:0;
        background:#c0c0c0; }
      .legacy-mode #laplace_navigation>li>a,.legacy-mode #differential_navigation>li>a,
      .legacy-mode #linear_algebra_navigation>li>a,.legacy-mode #reference_navigation>li>a,
      .legacy-mode #exam_navigation>li>a {
        min-height:40px; padding:6px 7px; color:#000; border:2px outset #fff;
        border-radius:0; background:#c0c0c0; font-family:Arial,sans-serif; text-decoration:none; }
      .legacy-mode #laplace_navigation>li.active>a,.legacy-mode #differential_navigation>li.active>a,
      .legacy-mode #linear_algebra_navigation>li.active>a,.legacy-mode #reference_navigation>li.active>a,
      .legacy-mode #exam_navigation>li.active>a,
      .legacy-mode #laplace_navigation>li>a:hover,.legacy-mode #differential_navigation>li>a:hover,
      .legacy-mode #linear_algebra_navigation>li>a:hover,.legacy-mode #reference_navigation>li>a:hover,
      .legacy-mode #exam_navigation>li>a:hover {
        color:#fff; background:#000080; border:2px inset #fff; box-shadow:none; }
      .legacy-mode #engineering_navigation { gap:3px; padding:3px; margin-bottom:9px;
        background:#c0c0c0; border:2px groove #fff; }
      .legacy-mode #engineering_navigation>li>a { color:#0000ee; border-radius:0;
        text-decoration:underline; font-family:Arial,sans-serif; }
      .legacy-mode #engineering_navigation>li.active>a,
      .legacy-mode #engineering_navigation>li>a:hover { color:#fff; background:#000080; }
      .legacy-mode .module-heading { grid-template-columns:auto 1fr; gap:12px; margin:0 0 9px;
        padding:10px; color:#000; border:2px ridge #fff; border-radius:0; background:#ffffe1; }
      .legacy-mode .module-seal { width:45px; height:45px; color:#fff; border:2px outset #fff;
        border-radius:0; background:#000080; box-shadow:none; font-family:'Times New Roman',serif; }
      .legacy-mode .module-kicker { color:#800000; font-family:Arial,sans-serif; letter-spacing:.06em; }
      .legacy-mode .module-heading h2 { margin:1px 0 3px; color:#000080; }
      .legacy-mode .module-heading p,.legacy-mode .module-route { color:#000; }
      .legacy-mode .card { color:#000; background:#fff; border:2px ridge #c0c0c0;
        border-radius:0; box-shadow:none; }
      .legacy-mode .card::before { display:none; }
      .legacy-mode .eyebrow { color:#800000; font-family:Arial,sans-serif; }
      .legacy-mode .concept strong { color:#000080; }
      .legacy-mode .hint,.legacy-mode .metric span { color:#444; }
      .legacy-mode .formula,.legacy-mode .metric { color:#000; background:#ffffe1;
        border:2px inset #fff; border-radius:0; }
      .legacy-mode .formula { border-left:5px solid #000080; }
      .legacy-mode .control-label,.legacy-mode .video-card p,.legacy-mode .learning-path {
        color:#000; }
      .legacy-mode .form-control,.legacy-mode .selectize-input,
      .legacy-mode .selectize-control.single .selectize-input.input-active,
      .legacy-mode .selectize-dropdown { color:#000!important; background:#fff!important;
        border:2px inset #fff!important; border-radius:0; font-family:Arial,sans-serif; }
      .legacy-mode .selectize-dropdown .active { color:#fff; background:#000080; }
      .legacy-mode .irs--shiny .irs-line { background:#c0c0c0; border:2px inset #fff; border-radius:0; }
      .legacy-mode .irs--shiny .irs-bar { background:#000080; border:0; }
      .legacy-mode .irs--shiny .irs-handle { background:#c0c0c0; border:2px outset #fff; border-radius:0; }
      .legacy-mode .irs--shiny .irs-single { color:#fff; background:#000080; border-radius:0; }
      .legacy-mode .preset-actions .btn,.legacy-mode .video-link,.legacy-mode .btn {
        color:#000!important; background:#c0c0c0!important; border:2px outset #fff!important;
        border-radius:0!important; box-shadow:none!important; font-family:Arial,sans-serif; }
      .legacy-mode .preset-actions .btn:hover,.legacy-mode .video-link:hover,.legacy-mode .btn:hover {
        color:#000!important; background:#d4d0c8!important; border-style:inset!important; }
      .legacy-mode .video-number,.legacy-mode .video-source { color:#800000; }
      .legacy-mode .help-tip { color:#000080; border-color:#000080; border-radius:0; }
      .legacy-mode .help-tip::after { color:#000; background:#ffffe1; border:2px outset #fff;
        border-radius:0; font-family:Arial,sans-serif; }
      .legacy-mode table { color:#000; border-collapse:collapse; background:#fff; }
      .legacy-mode th { color:#fff; background:#000080; font-family:Arial,sans-serif; }
      .legacy-mode th,.legacy-mode td { border:1px solid #808080; }
      .legacy-mode code { color:#000; background:#ffffe1; border:1px solid #808080; }
      .legacy-mode a,.legacy-mode .source-link { color:#0000ee; text-decoration:underline; }
      .legacy-mode .plot-wrap { background:#fff; }
      .legacy-mode .mock-exam-instructions { color:#000; background:#ffffe1;
        border:2px inset #fff; border-left:5px solid #000080; border-radius:0; }
      .legacy-mode .mock-exam-instructions strong { color:#000080; }
      .legacy-mode .mock-version-picker .radio-inline { color:#000; background:#c0c0c0;
        border:2px outset #fff; border-radius:0; font-family:Arial,sans-serif; }
      .legacy-mode .mock-version-picker .radio-inline:hover { color:#000;
        background:#d4d0c8; border-color:#fff; }
      .legacy-mode .mock-version-picker .radio-inline:has(input:checked) { color:#fff;
        background:#000080; border:2px inset #fff; box-shadow:none; }
      .legacy-mode .mock-version-picker .radio-inline input { accent-color:#000080; }
      .legacy-mode .mock-exam-banner { color:#000; background:#ffffe1;
        border:2px ridge #c0c0c0; border-radius:0; }
      .legacy-mode .mock-exam-banner h3 { color:#000080; }
      .legacy-mode .mock-exam-banner p { color:#000; }
      .legacy-mode .mock-exam-balance span,.legacy-mode .mock-question-marks {
        color:#000; background:#c0c0c0; border:2px outset #fff; border-radius:0; }
      .legacy-mode .mock-question { color:#000; background:#fff; border:2px ridge #c0c0c0;
        border-radius:0; box-shadow:none; }
      .legacy-mode .mock-question::before { display:none; }
      .legacy-mode .mock-question-number,.legacy-mode .mock-question h3 { color:#000080; }
      .legacy-mode .mock-question-topic { color:#444; }
      .legacy-mode .mock-video-match { color:#000; background:#ffffe1;
        border:2px inset #fff; border-radius:0; }
      .legacy-mode .mock-video-copy strong,.legacy-mode .mock-video-copy p { color:#000; }
      .legacy-mode .solution-disclosure { background:#fff; border:2px inset #fff; border-radius:0; }
      .legacy-mode .solution-disclosure summary { color:#000080; background:#c0c0c0;
        font-family:Arial,sans-serif; }
      .legacy-mode .solution-disclosure summary:hover,
      .legacy-mode .solution-disclosure summary:focus,
      .legacy-mode .solution-disclosure[open] summary { color:#fff; background:#000080;
        border-bottom:2px groove #fff; }
      .legacy-mode .solution-body h4 { color:#000080; }
      .legacy-mode .prompt-note { color:#000; background:#ffffe1; border:2px inset #fff;
        border-left:5px solid #000080; border-radius:0; }
      .legacy-mode .prompt-note strong { color:#000080; }
      .legacy-mode .prompt-stats>div { color:#000; background:#fff; border:2px inset #fff;
        border-radius:0; }
      .legacy-mode .prompt-stats strong { color:#000080; }
      .legacy-mode .prompt-stats span { color:#444; }
      .legacy-mode .chat-window { color:#000; background:#fff; border:2px inset #fff;
        border-radius:0; box-shadow:none; }
      .legacy-mode .chat-window-bar { color:#000; background:#c0c0c0;
        border-bottom:2px groove #fff; font-family:Arial,sans-serif; }
      .legacy-mode .chat-window-bar span { color:#333; }
      .legacy-mode .chat-window-mark,.legacy-mode .chat-avatar {
        border:2px outset #fff; border-radius:0; }
      .legacy-mode .chat-window-mark,.legacy-mode .user-avatar { background:#000080; }
      .legacy-mode .assistant-avatar { color:#fff; background:#008080; }
      .legacy-mode .chat-window-status { color:#000!important; background:#c0c0c0;
        border:2px outset #fff; border-radius:0; }
      .legacy-mode .prompt-phase { color:#000080; background:#ffffe1;
        border-bottom:1px solid #808080; font-family:Arial,sans-serif; }
      .legacy-mode .chat-message { border-bottom:1px solid #808080; }
      .legacy-mode .user-message { background:#eef4ff; }
      .legacy-mode .assistant-message { background:#fff; }
      .legacy-mode .chat-role { color:#000080; }
      .legacy-mode .chat-message-content p { color:#000; }
      .legacy-mode .chat-composer { color:#444; background:#fff; border:2px inset #fff;
        border-radius:0; }
      .legacy-mode .composer-button { color:#fff; background:#000080; border-radius:0; }
      @media(max-width:1050px){ #main_navigation{grid-template-columns:repeat(3,minmax(0,1fr));position:static;} }
      @media(max-width:800px){ .concept,.video-grid{grid-template-columns:1fr;} .hero{padding:30px 20px;}
        .content{padding:22px 16px;} .metric-row{grid-template-columns:1fr;}
        .module-heading{grid-template-columns:1fr;} .module-seal{width:46px;height:46px;}
        .theme-toggle{position:static;margin-top:16px;} .legacy-mode h1{max-width:none;}
        .legacy-mode .theme-toggle{position:static;} .prompt-stats{grid-template-columns:1fr;}
        .mock-version-picker .shiny-options-group{grid-template-columns:repeat(3,minmax(0,1fr));}
        .mock-exam-banner{grid-template-columns:1fr;}.mock-exam-balance{justify-content:flex-start;max-width:none;}
        .mock-video-match{grid-template-columns:1fr;}.mock-video-link{justify-self:start;}
        .mock-question-heading{align-items:flex-start;}.mock-question{padding:17px 15px;}
        .chat-message{grid-template-columns:34px minmax(0,1fr);padding:16px 14px;}
        .chat-window-bar{grid-template-columns:auto 1fr;}.chat-window-status{display:none;}
        .prompt-phase{padding:9px 14px;} }
      @media(max-width:560px){ #main_navigation{grid-template-columns:repeat(2,minmax(0,1fr));}
        .mock-version-picker .shiny-options-group{grid-template-columns:repeat(2,minmax(0,1fr));}
        #laplace_navigation>li,#differential_navigation>li,#linear_algebra_navigation>li,
        #reference_navigation>li,#exam_navigation>li{flex-basis:100%;}
        .legacy-address{grid-template-columns:auto 1fr;}.legacy-go{display:none;} }
    "))
    ,
    tags$script(HTML("
      (function () {
        function initializeThemeToggle() {
          var button = document.getElementById('theme_toggle');
          if (!button || button.dataset.ready === 'true') return;
          button.dataset.ready = 'true';

          function setLegacyMode(isLegacy) {
            document.body.classList.toggle('legacy-mode', isLegacy);
            button.setAttribute('aria-pressed', isLegacy ? 'true' : 'false');
            button.textContent = isLegacy ? 'Dark Mode' : 'Bright Mode';
            button.title = isLegacy ?
              'Switch to Dark Mode' :
              'Switch to Bright Mode';
            if (window.Shiny && Shiny.setInputValue) {
              Shiny.setInputValue('ui_theme', isLegacy ? 'legacy' : 'modern', {priority: 'event'});
            }
          }

          button.addEventListener('click', function () {
            setLegacyMode(!document.body.classList.contains('legacy-mode'));
          });
          setLegacyMode(false);
        }

        if (document.readyState === 'loading') {
          document.addEventListener('DOMContentLoaded', initializeThemeToggle);
        } else {
          initializeThemeToggle();
        }
      })();
    "))
  ),
  div(class = "app-shell",
      div(class = "legacy-browser-chrome", `aria-hidden` = "true",
        div(class = "legacy-titlebar",
          span("FinalPrep Interactive - Microsoft Internet Explorer"),
          div(class = "legacy-window-buttons", span("_"), span("□"), span("×"))
        ),
        div(class = "legacy-menubar",
          span("File"), span("Edit"), span("View"), span("Favorites"), span("Tools"), span("Help")
        ),
        div(class = "legacy-toolbar",
          span(class = "legacy-tool", "← Back"),
          span(class = "legacy-tool", "Forward →"),
          span(class = "legacy-tool", "Stop"),
          span(class = "legacy-tool", "Refresh"),
          span(class = "legacy-tool", "Home"),
          span(class = "legacy-tool", "Search"),
          span(class = "legacy-tool", "Favorites")
        ),
        div(class = "legacy-address",
          strong("Address"),
          div(class = "legacy-address-box", "http://www.finalprepinteractive.edu/index.htm"),
          span(class = "legacy-go", "Go")
        )
      ),
      div(class = "hero",
          div(class = "eyebrow", "Interactive mathematics"),
          h1("Engineering Analysis B Learning Lab"),
          div(class = "subtitle",
              "Build exam-ready intuition for differential equations, Laplace transforms, and linear algebra."),
          tags$button(
            id = "theme_toggle", type = "button", class = "theme-toggle",
            `aria-pressed` = "false", title = "Switch to Bright Mode",
            "Bright Mode"
          )
      ),
      div(class = "content",
          tabsetPanel(id = "main_navigation", type = "tabs",
            tabPanel("Course Home", value = "app_about",
              div(class = "card",
                div(class = "eyebrow", "Start here"),
                h2("Learn the course by exploring it"),
                tags$p(
                  "This app turns the recurring concepts from six ENGI-3022 final exams into interactive learning ",
                  "experiences. Its purpose is to help you understand why the methods work, recognize common exam ",
                  "patterns, and practise choosing the right method—not simply memorize isolated formulas."
                ),
                tags$p(class = "hint",
                  "The material is organized around the 2020–2025 finals you supplied. It supports exam preparation but does not predict the exact content of a future exam."
                )
              ),
              div(class = "concept",
                div(class = "card",
                  strong("Laplace transforms"),
                  span("Translate time signals and differential equations into easier algebraic forms.")
                ),
                div(class = "card",
                  strong("Differential equations"),
                  span("Classify equations, choose a solution method, and connect roots to behavior.")
                ),
                div(class = "card",
                  strong("Linear algebra"),
                  span("Understand systems, determinants, eigenvectors, and diagonalization geometrically.")
                )
              ),
              br(),
              fluidRow(
                column(7,
                  div(class = "card",
                    h3("How to use the app"),
                    tags$ol(class = "learning-path",
                      tags$li("Begin with a topic overview and read its solution guide."),
                      tags$li("Predict what a graph or answer will do before changing a control."),
                      tags$li("Use the interactive lab to test your prediction."),
                      tags$li("Practise the matching recurring exam patterns."),
                      tags$li("Use Exam Practice to rotate between topics and reveal guidance only after attempting a problem.")
                    )
                  )
                ),
                column(5,
                  div(class = "card",
                    h3("The learning goal"),
                    math_block("\\text{Recognize}\\;\\longrightarrow\\;\\text{Choose}\\;\\longrightarrow\\;\\text{Solve}\\;\\longrightarrow\\;\\text{Verify}",
                               "Recognize, choose, solve, then verify."),
                    tags$p(class = "hint",
                      "Mastery means you can identify the structure of a new problem, select a justified method, carry out the mathematics, and check the result.")
                  )
                )
              )
            ),
            tabPanel("1 · Laplace", value = "laplace",
              module_header("I", "Laplace transforms",
                "Translate time-domain behavior into algebra, then connect the rules to real engineering systems."),
              tabsetPanel(id = "laplace_navigation", type = "pills",
            tabPanel("1 · Overview", value = "laplace_about",
              div(class = "card",
                div(class = "eyebrow", "Start here"),
                h2("Learn Laplace transforms visually"),
                tags$p(
                  "This app connects formulas to visual changes. Instead of only memorizing transform pairs, ",
                  "you can adjust a signal and immediately see how its time-domain behavior maps into the s-domain."
                ),
                tags$p(class = "hint",
                  "Building this intuition makes transform rules easier to remember and prepares you to solve differential equations."
                )
              ),
              div(class = "concept",
                div(class = "card",
                  strong("Transform Lab"),
                  span("Choose an exponential, sine, cosine, polynomial, or damped sine. Change its parameters and compare f(t) with F(s).")
                ),
                div(class = "card",
                  strong("Properties Lab"),
                  span("Delay a signal and observe how time shifting becomes multiplication by e^(−as) in the s-domain.")
                ),
                div(class = "card",
                  strong("Formula Library"),
                  span("Review the definition, common transform pairs, and the weight–accumulate–solve interpretation.")
                )
              ),
              br(),
              fluidRow(
                column(7,
                  div(class = "card",
                    h3("A good learning path"),
                    tags$ol(class = "learning-path",
                      tags$li("Select exponential decay and change its decay rate."),
                      tags$li("Compare sine waves with low and high frequency."),
                      tags$li("Try the damped sine to combine decay and oscillation."),
                      tags$li("Use the Properties Lab to understand time shifting."),
                      tags$li("Predict each graph before moving a slider, then test your prediction.")
                    )
                  )
                ),
                column(5,
                  div(class = "card",
                    h3("Core idea", help_tip("It is a math translator: it changes a signal that moves through time into a form that is often easier to calculate with.")),
                    math_block("F(s)=\\mathcal{L}\\{f(t)\\}=\\int_0^\\infty e^{-st}f(t)\\,dt",
                               "The Laplace transform definition."),
                    tags$p(class = "hint",
                      "The transform weights a time signal, accumulates it, and represents the result as a function of s."
                    )
                  )
                )
              )
            ),
            tabPanel("2 · Explore: Transforms", value = "explorer",
              fluidRow(
                column(4,
                  div(class = "card",
                    h3("Choose a signal"),
                    selectInput("signal", NULL,
                      choices = setNames(names(signal_catalogue),
                                         vapply(signal_catalogue, `[[`, "", "label"))),
                    uiOutput("parameter_controls"),
                    sliderInput("tmax", "Time window", min = 2, max = 20, value = 10, step = 1)
                  ),
                  div(class = "card",
                    h3("Transform pair", help_tip("These are two different ways to describe the same signal—one in time and one after the Laplace transform.")),
                    uiOutput("formula_card"),
                    tags$p(class = "hint",
                      "The plotted s-domain curve uses real positive s. Poles are avoided so the graph stays readable.")
                  )
                ),
                column(8,
                  div(class = "card plot-wrap",
                      h3("Time domain  f(t)", help_tip("This is the signal as you would watch it happen on a clock, from one moment to the next.")),
                      plotOutput("time_plot", height = 300)),
                  div(class = "card plot-wrap",
                      h3("Laplace domain  F(s)", help_tip("This is the same signal translated into a math map that makes patterns, decay, and oscillation easier to work with.")),
                      plotOutput("laplace_plot", height = 300))
                )
              )
            ),
            tabPanel("3 · Explore: Properties", value = "properties",
              fluidRow(
                column(4,
                  div(class = "card",
                    h3("Time-shifting experiment", help_tip("Time shifting means making the same event happen later—like pressing play after waiting two seconds.")),
                    sliderInput("shift", "Delay  a", min = 0, max = 6, value = 2, step = .25),
                    sliderInput("decay", "Base decay  b", min = .2, max = 4, value = 1, step = .1),
                    tags$p(class = "hint",
                      "Compare the original exponential with its delayed version u(t−a)f(t−a).")
                  ),
                  div(class = "card",
                    h3("What to notice"),
                    math_block("\\mathcal{L}\\{u(t-a)f(t-a)\\}=e^{-as}F(s)",
                               "The second shifting theorem."),
                    tags$p(class = "hint",
                      "A delay in time becomes multiplication by an exponential in the s-domain.")
                  )
                ),
                column(8,
                  div(class = "card plot-wrap", h3("Original and shifted signals"),
                      plotOutput("shift_plot", height = 360)),
                  div(class = "card plot-wrap",
                      h3("Transform magnitude", help_tip("Magnitude means how strong or large the transformed signal is at each value of s.")),
                      plotOutput("shift_laplace_plot", height = 300))
                )
              )
            ),
            tabPanel("4 · Watch: Videos", value = "laplace_videos",
              video_lesson_page(
                "Laplace transform",
                paste(
                  "Every card opens one exact video that matches the named concept.",
                  "Video-Tutor.net does not currently expose a dedicated Laplace chapter,",
                  "so carefully selected lessons from established mathematics educators fill those gaps."
                ),
                laplace_videos,
                "https://www.video-tutor.net/video-playlists.html"
              )
            ),
            tabPanel("5 · Apply: Engineering", value = "engineering",
              tabsetPanel(id = "engineering_navigation", type = "pills",
                tabPanel("Mechanical: Suspension", value = "mechanical",
              div(class = "card",
                div(class = "eyebrow", "Real-world application"),
                h2("Designing a vehicle suspension"),
                tags$p(
                  "A car suspension can be approximated by a mass, spring, and damper. After a sudden road force, ",
                  "engineers want the body to return smoothly to equilibrium without bouncing for too long."
                ),
                math_block("m\\ddot{x}(t)+c\\dot{x}(t)+kx(t)=F_0u(t)",
                           "The suspension motion equation."),
                math_block("X(s)=\\frac{F_0}{s(ms^2+cs+k)}",
                           "The transformed suspension displacement.")
              ),
              div(class = "card",
                h3("Suspension design field guide"),
                fluidRow(
                  column(6,
                    tags$h4("What should you look for?"),
                    tags$ol(class = "learning-path",
                      tags$li(tags$strong("Keep overshoot controlled. "),
                              "A large first peak means passengers feel a strong rebound after the bump."),
                      tags$li(tags$strong("Aim for a short settling time. "),
                              "The body should stop bouncing before the next disturbance arrives."),
                      tags$li(tags$strong("Avoid a large frequency-response peak. "),
                              "A tall peak means repeated bumps at that frequency can build into uncomfortable motion."),
                      tags$li(tags$strong("Balance comfort and control. "),
                              "Softer springs isolate small bumps, while firmer springs limit body movement and support heavier loads."),
                      tags$li(tags$strong("Test different loads. "),
                              "Passengers and cargo increase mass, changing the natural frequency and damping ratio.")
                    )
                  ),
                  column(6,
                    tags$h4("Why does low damping cause dramatic motion?"),
                    tags$p(
                      "A spring stores energy when it is compressed and returns that energy as it expands. ",
                      "The damper is the part that turns some of this motion into heat. When c is small, ",
                      "very little energy is removed during each bounce, so the mass and spring keep trading energy back and forth."
                    ),
                    math_block("\\zeta=\\frac{c}{2\\sqrt{km}}",
                               "The suspension damping ratio."),
                    tags$p(class = "hint",
                      "Lower c means a lower damping ratio ζ. The poles move closer to the imaginary axis, ",
                      "oscillations fade more slowly, overshoot grows, and the frequency-response peak near the ",
                      "natural frequency becomes much taller. At resonance, repeated pushes arrive in step with ",
                      "the bouncing—like pushing a playground swing at just the right time."
                    )
                  )
                ),
                tags$p(class = "hint",
                  tags$strong("Design goal: "),
                  "Choose values that settle promptly without a large rebound or resonance peak. Real engineers ",
                  "also model tire stiffness, wheel mass, suspension travel, nonlinear dampers, road holding, and load variation."
                )
              ),
              fluidRow(
                column(4,
                  div(class = "card",
                    h3("Suspension design"),
                    div(class = "preset-actions",
                      actionButton("preset_outback", "2025 Subaru Outback"),
                      actionButton("preset_f150", "2025 Ford F-150"),
                      actionButton("preset_crv", "2026 Honda CR-V")
                    ),
                    sliderInput("mass",
                                tagList("Vehicle mass  m (kg)",
                                  help_tip("How heavy the part of the vehicle supported by the suspension is. A heavier body is harder to start and stop moving.")),
                                min = 200, max = 3000,
                                value = 900, step = 50),
                    sliderInput("stiffness",
                                tagList("Spring stiffness  k (N/m)",
                                  help_tip("How strongly the spring pushes back when squeezed. A larger number feels firmer; a smaller number allows more movement.")),
                                min = 5000,
                                max = 200000, value = 22000, step = 1000),
                    sliderInput("damping",
                                tagList("Damping  c (N·s/m)",
                                  help_tip("How strongly the shock absorber removes bouncing energy. More damping calms motion faster; too much can feel harsh and slow.")),
                                min = 200,
                                max = 25000, value = 4500, step = 100),
                    sliderInput("force",
                                tagList("Sudden road force  F₀ (N)",
                                  help_tip("A simplified push from a bump or pothole. A bigger force represents a stronger hit to the suspension.")),
                                min = 500,
                                max = 8000, value = 3000, step = 250),
                    tags$p(class = "hint",
                      "Try low damping to create a bouncy ride, then increase it until the response settles quickly.")
                  )
                ),
                column(8,
                  uiOutput("suspension_metrics"),
                  div(class = "card plot-wrap",
                      h3("Vehicle-body displacement", help_tip("Displacement is how far the car body moves away from its resting position after the bump pushes it.")),
                      plotOutput("suspension_plot", height = 350)),
                  div(class = "card plot-wrap",
                      h3("Frequency response of the suspension",
                         help_tip("Imagine shaking the car slowly, then faster and faster. This graph shows which shaking speeds make the car move a little and which make it bounce a lot.")),
                      plotOutput("frequency_plot", height = 300))
                )
              ),
              div(class = "card",
                h3("How the Laplace transform helps",
                   help_tip("It turns a motion equation with derivatives into ordinary algebra, like changing a hard puzzle into an easier one.")),
                tags$p(
                  "The transform replaces differentiation with multiplication by s. The differential equation ",
                  "becomes an algebraic transfer function, making its poles, damping, resonant behavior, and response ",
                  "to an input much easier to calculate."
                ),
                tags$p(class = "hint",
                  "This same workflow is used in control systems, robotics, aircraft stability, structural vibration, and electrical circuits.")
              ),
              div(class = "card",
                h3("Approximate real-vehicle presets"),
                tags$p(
                  "Choose a preset above to load an educational whole-vehicle model. Curb mass is based on a ",
                  "representative published configuration; equivalent stiffness and damping are estimates chosen ",
                  "to produce realistic passenger-vehicle ride frequencies and damping ratios."
                ),
                tags$table(
                  tags$thead(tags$tr(
                    tags$th("Vehicle"), tags$th("Model mass"),
                    tags$th("Estimated k"), tags$th("Estimated c"), tags$th("Character")
                  )),
                  tags$tbody(
                    tags$tr(tags$td("2025 Subaru Outback"), tags$td("1,652 kg"),
                            tags$td("102 kN/m"), tags$td("10.9 kN·s/m"),
                            tags$td("Comfort-oriented crossover")),
                    tags$tr(tags$td("2025 Ford F-150 SuperCrew 4×4"), tags$td("2,312 kg"),
                            tags$td("166 kN/m"), tags$td("15.7 kN·s/m"),
                            tags$td("Heavier truck, firmer support")),
                    tags$tr(tags$td("2026 Honda CR-V AWD"), tags$td("1,629 kg"),
                            tags$td("109 kN/m"), tags$td("12.0 kN·s/m"),
                            tags$td("Compact crossover balance"))
                  )
                ),
                tags$p(class = "hint",
                  "Published curb-weight references: ",
                  tags$a(class = "source-link", href = "https://www.subaru.com/content/dam/subaru/downloads/pdf/brochures/2025/2025_Outback_Brochure_071524.pdf",
                         target = "_blank", "Subaru Outback brochure"), " · ",
                  tags$a(class = "source-link", href = "https://www.ford.com/trucks/f150/2025/",
                         target = "_blank", "Ford F-150 specifications"), " · ",
                  tags$a(class = "source-link", href = "https://automobiles.honda.com/cr-v/specs-features-trim-comparison",
                         target = "_blank", "Honda CR-V specifications"),
                  ". Values vary by trim, drivetrain, passengers, cargo, and suspension option."
                )
              )
                ),
                tabPanel("Chemical: Mixing tank", value = "chemical",
                  div(class = "card",
                    div(class = "eyebrow", "Chemical engineering"),
                    h2("Controlling concentration in a mixing tank"),
                    tags$p(
                      "A well-stirred tank receives liquid containing a dissolved substance while the same amount ",
                      "flows out. Engineers use the Laplace transform to predict how quickly the outlet concentration ",
                      "responds when the incoming concentration changes."
                    ),
                    math_block("V\\frac{dC}{dt}=q(C_{\\mathrm{in}}-C)",
                               "The well-mixed tank material balance."),
                    math_block("\\frac{\\Delta C(s)}{\\Delta C_{\\mathrm{in}}(s)}=\\frac{1}{\\tau s+1},\\qquad \\tau=\\frac{V}{q}",
                               "The tank transfer function and residence time.")
                  ),
                  div(class = "card",
                    h3("Mixing-process design guide"),
                    fluidRow(
                      column(6,
                        tags$h4("What should you look for?"),
                        tags$ol(class = "learning-path",
                          tags$li(tags$strong("Choose an appropriate residence time. "),
                                  "The liquid needs enough time in the tank to mix or react."),
                          tags$li(tags$strong("Avoid a response that is too slow. "),
                                  "A very large tank or low flow delays production changes and corrective action."),
                          tags$li(tags$strong("Keep concentration changes smooth. "),
                                  "Gradual transitions reduce off-spec product and sudden downstream disturbances."),
                          tags$li(tags$strong("Check peak and low flow. "),
                                  "The same tank responds differently when the production flow rate changes."),
                          tags$li(tags$strong("Verify real mixing. "),
                                  "Baffles and agitators should prevent stagnant zones and short-circuiting.")
                        )
                      ),
                      column(6,
                        tags$h4("The key design relationship"),
                        math_block("\\tau=\\frac{V}{q}", "Residence time equals volume divided by flow rate."),
                        tags$p(
                          "The time constant is tank volume divided by flow. Increasing volume gives the process ",
                          "more memory; increasing flow replaces the contents faster."
                        ),
                        tags$p(class = "hint",
                          "After one τ the change is about 63% complete, after three τ it is about 95% complete, ",
                          "and after five τ it is essentially settled.")
                      )
                    )
                  ),
                  fluidRow(
                    column(4,
                      div(class = "card",
                        h3("Tank operating conditions"),
                        div(class = "preset-actions",
                          actionButton("preset_beverage", "Beverage blend"),
                          actionButton("preset_pharma", "Pharma buffer"),
                          actionButton("preset_chlorine", "Chlorine contact")
                        ),
                        sliderInput("tank_volume",
                          tagList("Tank volume  V (L)",
                            help_tip("How much liquid the tank holds. A larger tank takes longer to replace and therefore responds more slowly.")),
                          min = 100, max = 50000, value = 1000, step = 100),
                        sliderInput("flow_rate",
                          tagList("Flow rate  q (L/min)",
                            help_tip("How much liquid enters and leaves each minute. Faster flow changes the tank concentration sooner.")),
                          min = 10, max = 2000, value = 100, step = 10),
                        sliderInput("initial_conc",
                          tagList("Initial concentration  C₀ (relative units)",
                            help_tip("The amount of dissolved material in each litre before the inlet change happens.")),
                          min = 0, max = 2, value = 0.2, step = 0.05),
                        sliderInput("inlet_conc",
                          tagList("New inlet concentration  Cᵢₙ (relative units)",
                            help_tip("The concentration of the liquid now entering the tank—the new value the tank moves toward.")),
                          min = 0, max = 2, value = 1, step = 0.05)
                      ),
                      div(class = "card",
                        h3("What should you look for?"),
                        tags$p("The response should approach the new inlet concentration smoothly."),
                        tags$p(class = "hint",
                          "A larger V or smaller q increases the time constant. That gives slower, gentler changes but delays production adjustments.")
                      )
                    ),
                    column(8,
                      uiOutput("tank_metrics"),
                      div(class = "card plot-wrap",
                        h3("Outlet concentration over time",
                          help_tip("This shows how the mixed liquid slowly changes from its old concentration to the new inlet concentration.")),
                        plotOutput("tank_plot", height = 380))
                    )
                  ),
                  div(class = "card",
                    h3("Why the Laplace transform is useful"),
                    tags$p(
                      "The material-balance differential equation becomes a first-order transfer function. Its pole ",
                      "at −1/τ immediately tells an engineer how fast the process responds, which is essential for ",
                      "controller tuning, product consistency, and safe chemical dosing."
                    )
                  ),
                  div(class = "card",
                    h3("Approximate real-process presets"),
                    tags$p(
                      "These presets represent realistic operating scales and demonstrate how the same first-order ",
                      "Laplace model appears in food production, pharmaceutical preparation, and water treatment."
                    ),
                    tags$table(
                      tags$thead(tags$tr(tags$th("Process"), tags$th("Volume"),
                                        tags$th("Flow"), tags$th("τ"), tags$th("Design focus"))),
                      tags$tbody(
                        tags$tr(tags$td("Beverage blending tank"), tags$td("5,000 L"),
                                tags$td("500 L/min"), tags$td("10 min"),
                                tags$td("Fast recipe changeover")),
                        tags$tr(tags$td("Pharmaceutical buffer tank"), tags$td("1,000 L"),
                                tags$td("50 L/min"), tags$td("20 min"),
                                tags$td("Smooth, controlled composition")),
                        tags$tr(tags$td("Wastewater chlorine-contact pilot"), tags$td("45,000 L"),
                                tags$td("756 L/min"), tags$td("≈60 min"),
                                tags$td("Adequate disinfection contact"))
                      )
                    ),
                    tags$p(class = "hint",
                      "The chlorine pilot scale follows an EPA-reported 45 m³ tank operated at 12.6 L/s; the ",
                      "beverage and pharmaceutical values are representative educational estimates. ",
                      tags$a(class = "source-link",
                        href = "https://nepis.epa.gov/Exe/ZyPURL.cgi?Dockey=9101NTIC.TXT",
                        target = "_blank", "EPA chlorine-contact study"),
                      ". Real tanks require mixing, reaction, safety, and quality validation beyond this ideal model."
                    )
                  )
                ),
                tabPanel("Civil: Building vibration", value = "civil",
                  div(class = "card",
                    div(class = "eyebrow", "Civil engineering"),
                    h2("Limiting building motion under wind"),
                    tags$p(
                      "A simplified tall building behaves like a heavy mass supported by flexible columns with ",
                      "structural damping. Engineers study its response to wind so occupants remain comfortable ",
                      "and motion stays within safe limits."
                    ),
                    math_block("M\\ddot{x}(t)+C\\dot{x}(t)+Kx(t)=F_wu(t)",
                               "The building motion equation."),
                    math_block("X(s)=\\frac{F_w}{s(Ms^2+Cs+K)}",
                               "The transformed building displacement.")
                  ),
                  div(class = "card",
                    h3("Wind-response design guide"),
                    fluidRow(
                      column(6,
                        tags$h4("What should you look for?"),
                        tags$ol(class = "learning-path",
                          tags$li(tags$strong("Limit peak movement. "),
                                  "Large sideways displacement can damage non-structural components."),
                          tags$li(tags$strong("Protect occupant comfort. "),
                                  "People can feel acceleration even when structural stresses remain safe."),
                          tags$li(tags$strong("Avoid resonance. "),
                                  "Wind patterns close to the natural frequency can amplify repeated swaying."),
                          tags$li(tags$strong("Provide enough damping. "),
                                  "Structural damping or added devices should remove vibration energy promptly."),
                          tags$li(tags$strong("Study multiple wind directions and loads. "),
                                  "Real towers respond differently as wind speed and direction change.")
                        )
                      ),
                      column(6,
                        tags$h4("Common engineering responses"),
                        tags$p(
                          "Engineers can increase lateral stiffness, reshape the tower to disrupt vortices, add ",
                          "viscous dampers, or install a tuned mass damper that moves against the building sway."
                        ),
                        math_block("f_n=\\frac{1}{2\\pi}\\sqrt{\\frac{K}{M}}",
                                   "The natural frequency in hertz."),
                        tags$p(class = "hint",
                          "More stiffness raises the natural frequency. More mass lowers it. More damping reduces ",
                          "the resonance peak and helps motion fade sooner.")
                      )
                    )
                  ),
                  fluidRow(
                    column(4,
                      div(class = "card",
                        h3("Building model"),
                        div(class = "preset-actions",
                          actionButton("preset_taipei", "Taipei 101"),
                          actionButton("preset_burj", "Burj Khalifa"),
                          actionButton("preset_cn", "CN Tower")
                        ),
                        sliderInput("building_mass",
                          tagList("Effective mass  M (million kg)",
                            help_tip("The portion of the building mass that moves together in this vibration shape.")),
                          min = 5, max = 100, value = 30, step = 5),
                        sliderInput("building_frequency",
                          tagList("Natural frequency  fₙ (Hz)",
                            help_tip("The rhythm the building naturally prefers to sway at—like the preferred tempo of a playground swing.")),
                          min = 0.1, max = 1, value = 0.3, step = 0.05),
                        sliderInput("building_damping",
                          tagList("Structural damping  ζ (%)",
                            help_tip("How much swaying energy is removed each cycle by the structure and damping devices.")),
                          min = 0.5, max = 10, value = 3, step = 0.5),
                        sliderInput("wind_force",
                          tagList("Equivalent wind force  Fw (kN)",
                            help_tip("A simplified sideways push representing the combined effect of wind on the building.")),
                          min = 100, max = 5000, value = 1500, step = 100)
                      )
                    ),
                    column(8,
                      uiOutput("building_metrics"),
                      div(class = "card plot-wrap",
                        h3("Building sway after the wind begins",
                          help_tip("This graph shows how far the building top moves sideways and how long the swaying lasts.")),
                        plotOutput("building_plot", height = 380))
                    )
                  ),
                  div(class = "card",
                    h3("Civil design lesson"),
                    tags$p(
                      "The poles of the Laplace-domain model reveal the sway frequency and how quickly vibration ",
                      "dies away. Engineers can change stiffness, add viscous dampers, or install a tuned mass damper ",
                      "to reduce motion near resonance."
                    ),
                    tags$p(class = "hint",
                      "This is a single-mode educational model. Real structural analysis includes many vibration modes, changing wind loads, soil interaction, and code requirements.")
                  ),
                  div(class = "card",
                    h3("Approximate landmark presets"),
                    tags$p(
                      "Published landmark facts provide context; effective modal mass, frequency, damping, and ",
                      "equivalent wind force are educational estimates for this one-mode model, not construction data."
                    ),
                    tags$table(
                      tags$thead(tags$tr(tags$th("Structure"), tags$th("Published context"),
                                        tags$th("Model M"), tags$th("Model fₙ / ζ"), tags$th("Wind strategy"))),
                      tags$tbody(
                        tags$tr(tags$td("Taipei 101"), tags$td("508 m; 660 t tuned mass damper"),
                                tags$td("50 million kg"), tags$td("0.15 Hz / 2%"),
                                tags$td("Large pendulum damper")),
                        tags$tr(tags$td("Burj Khalifa"), tags$td("828 m; wind-shaped setbacks"),
                                tags$td("80 million kg"), tags$td("0.12 Hz / 2%"),
                                tags$td("Shape disrupts wind loading")),
                        tags$tr(tags$td("CN Tower"), tags$td("Tapered concrete tower"),
                                tags$td("60 million kg"), tags$td("0.18 Hz / 2.5%"),
                                tags$td("Taper and prestressed cables"))
                      )
                    ),
                    tags$p(class = "hint",
                      "Context sources: ",
                      tags$a(class = "source-link",
                        href = "https://www.skyscrapercenter.com/building/taipei-101/117",
                        target = "_blank", "Taipei 101 height"), " · ",
                      tags$a(class = "source-link",
                        href = "https://www.burjkhalifa.ae/the-tower/architecture-design/",
                        target = "_blank", "Burj Khalifa wind design"), " · ",
                      tags$a(class = "source-link",
                        href = "https://www.cntower.ca/history-and-science/design-science-and-innovation",
                        target = "_blank", "CN Tower engineering"), "."
                    )
                  )
                )
              )
            ),
              )
            ),
            tabPanel("2 · Differential Equations", value = "differential",
              module_header("II", "Differential equations",
                "Classify equations, connect characteristic roots to motion, and interpret coupled systems."),
              tabsetPanel(id = "differential_navigation", type = "pills",
                tabPanel("1 · Overview",
                  div(class = "card",
                    div(class = "eyebrow", "Exam domain"),
                    h2("Differential equations"),
                    tags$p(
                      "Differential equations describe how a quantity changes. Across the exams, you are expected ",
                      "to recognize the equation family, choose the matching method, apply initial conditions, and ",
                      "check whether the answer satisfies the original equation."
                    )
                  ),
                  div(class = "card",
                    h3("Differential-equation solution guide"),
                    div(class = "concept",
                      div(class = "card", strong("1 · Classify"),
                          span("Identify order, linearity, coefficient type, forcing, and initial conditions.")),
                      div(class = "card", strong("2 · Choose"),
                          span("Use characteristic roots, Cauchy-Euler powers, order reduction, or a system method.")),
                      div(class = "card", strong("3 · Verify"),
                          span("Substitute the result back and check every initial condition."))
                    )
                  ),
                  div(class = "card",
                    h3("Patterns found in the 2020–2025 finals"),
                    tags$table(
                      tags$thead(tags$tr(tags$th("Pattern"), tags$th("What mastery looks like"),
                                        tags$th("Seen in"))),
                      tags$tbody(
                        tags$tr(tags$td("Constant-coefficient linear ODEs"),
                                tags$td("Characteristic roots, homogeneous and particular solutions"),
                                tags$td("2020, 2024, 2025")),
                        tags$tr(tags$td("Cauchy-Euler equations"),
                                tags$td("Try y=xᵐ and use variation of parameters when forced"),
                                tags$td("2025")),
                        tags$tr(tags$td("Reducible nonlinear ODEs"),
                                tags$td("Use u=y′ and recognize when x or y is missing"),
                                tags$td("2022, 2024, 2025")),
                        tags$tr(tags$td("Systems and impulse IVPs"),
                                tags$td("Translate coupled equations or use Laplace transforms"),
                                tags$td("2020–2024"))
                      )
                    )
                  )
                ),
                tabPanel("2 · Explore: Linear ODEs",
                  div(class = "card",
                    div(class = "eyebrow", "Interactive method"),
                    h2("Characteristic-root explorer"),
                    tags$p("Explore the homogeneous equation y″ + ay′ + by = 0 and see how its roots control the motion."),
                    math_block("r^2+ar+b=0", "The characteristic equation.")
                  ),
                  div(class = "card",
                    h3("Linear ODE design guide"),
                    tags$ol(class = "learning-path",
                      tags$li("Move every term to one side and identify a and b."),
                      tags$li("Solve the characteristic polynomial."),
                      tags$li("Write the solution form for distinct, repeated, or complex roots."),
                      tags$li("Use y(0) and y′(0) to determine the constants."),
                      tags$li("Differentiate and substitute to verify the result.")
                    )
                  ),
                  fluidRow(
                    column(4,
                      div(class = "card",
                        h3("Equation and initial values"),
                        sliderInput("ode_a", "Coefficient a", min = -6, max = 8, value = 2, step = .25),
                        sliderInput("ode_b", "Coefficient b", min = -8, max = 16, value = 5, step = .25),
                        sliderInput("ode_y0", "Initial value y(0)", min = -5, max = 5, value = 1, step = .25),
                        sliderInput("ode_v0", "Initial slope y′(0)", min = -8, max = 8, value = 0, step = .25)
                      )
                    ),
                    column(8,
                      uiOutput("ode_metrics"),
                      div(class = "card plot-wrap", h3("Solution over time"),
                          plotOutput("ode_plot", height = 380))
                    )
                  )
                ),
                tabPanel("3 · Explore: Systems",
                  div(class = "card",
                    div(class = "eyebrow", "Coupled equations"),
                    h2("Two-state system explorer"),
                    tags$p("Study x′ = ax + by and y′ = cx + dy. Eigenvalues reveal whether trajectories decay, grow, spiral, or form a saddle."),
                    math_block("\\mathbf{u}'=A\\mathbf{u}", "A coupled linear differential equation system.")
                  ),
                  div(class = "card",
                    h3("System solution guide"),
                    tags$ol(class = "learning-path",
                      tags$li("Write the coefficient matrix A."),
                      tags$li("Find det(A−λI)=0."),
                      tags$li("Find an eigenvector for each eigenvalue."),
                      tags$li("Combine eigenmodes and apply the initial vector."),
                      tags$li("Use the phase portrait to check the predicted stability.")
                    )
                  ),
                  fluidRow(
                    column(4,
                      div(class = "card",
                        h3("Matrix A"),
                        sliderInput("sys_a", "a", min = -4, max = 4, value = -1, step = .25),
                        sliderInput("sys_b", "b", min = -4, max = 4, value = 1, step = .25),
                        sliderInput("sys_c", "c", min = -4, max = 4, value = 2, step = .25),
                        sliderInput("sys_d", "d", min = -4, max = 4, value = -2, step = .25)
                      )
                    ),
                    column(8,
                      uiOutput("system_metrics"),
                      div(class = "card plot-wrap", h3("Phase portrait",
                        help_tip("Each curve shows how the two state variables evolve together from a different starting point.")),
                        plotOutput("system_plot", height = 420))
                    )
                  )
                ),
                tabPanel("4 · Watch: Videos",
                  video_lesson_page(
                    "Differential equations",
                    paste(
                      "Every card opens one exact concept-matched video.",
                      "Organic Chemistry Tutor lessons are used where a direct match exists;",
                      "the phase-portrait lesson comes from an engineering-focused educator."
                    ),
                    differential_videos,
                    "https://www.video-tutor.net/differential-equations.html"
                  )
                ),
                tabPanel("5 · Apply: Engineering",
                  div(class = "card",
                    div(class = "eyebrow", "Thermal engineering"),
                    h2("Keeping equipment at a safe temperature"),
                    tags$p(
                      "Electronic devices, battery packs, and motors store heat and release it to their surroundings. ",
                      "A first-order differential equation predicts how quickly temperature approaches a safe steady value."
                    ),
                    math_block("C_{th}\\frac{dT}{dt}=P-\\frac{T-T_a}{R_{th}}",
                               "Thermal capacitance times temperature rate equals heat power minus heat loss to ambient."),
                    math_block("T(t)=T_{ss}+(T_0-T_{ss})e^{-t/\\tau},\\qquad \\tau=R_{th}C_{th}",
                               "Temperature equals steady temperature plus the initial difference times e to the negative t over tau.")
                  ),
                  div(class = "card",
                    h3("Thermal design guide"),
                    fluidRow(
                      column(6,
                        tags$h4("What should you look for?"),
                        tags$ol(class = "learning-path",
                          tags$li(tags$strong("Keep steady temperature below the component limit. "),
                                  "Long-term heat generation must be balanced by cooling."),
                          tags$li(tags$strong("Watch the time constant. "),
                                  "A large thermal mass heats slowly but also cools slowly."),
                          tags$li(tags$strong("Check startup and overload conditions. "),
                                  "Short power surges can create dangerous transient peaks."),
                          tags$li(tags$strong("Test ambient extremes. "),
                                  "The same cooling system has less margin on a hot day."),
                          tags$li(tags$strong("Leave safety margin. "),
                                  "Real airflow, contact resistance, and material properties vary.")
                        )
                      ),
                      column(6,
                        tags$h4("Differential-equation lesson"),
                        tags$p(
                          "The rate of temperature change is proportional to the gap between the current temperature ",
                          "and its final equilibrium. The exponential term describes how the system gradually forgets its initial condition."
                        ),
                        math_block("1\\tau\\approx63\\%,\\qquad 3\\tau\\approx95\\%,\\qquad 5\\tau\\approx99\\%",
                                   "One, three, and five time constants reach about 63, 95, and 99 percent."),
                        tags$p(class = "hint",
                          "Reducing thermal resistance lowers the final temperature. Reducing thermal capacitance makes the response faster.")
                      )
                    )
                  ),
                  fluidRow(
                    column(4,
                      div(class = "card",
                        h3("Thermal model"),
                        div(class = "preset-actions",
                          actionButton("thermal_laptop", "Laptop processor"),
                          actionButton("thermal_battery", "EV battery pack"),
                          actionButton("thermal_motor", "Industrial motor")
                        ),
                        sliderInput("thermal_ambient",
                          tagList("Ambient temperature  Tₐ (°C)",
                            help_tip("The temperature of the surrounding air or coolant.")),
                          min = -10, max = 50, value = 22, step = 1),
                        sliderInput("thermal_initial",
                          tagList("Initial temperature  T₀ (°C)",
                            help_tip("The equipment temperature at the moment the operating condition begins.")),
                          min = -10, max = 120, value = 22, step = 1),
                        sliderInput("thermal_rise",
                          tagList("Steady temperature rise  ΔT (°C)",
                            help_tip("How far above ambient the equipment will eventually operate under this heat load.")),
                          min = 2, max = 100, value = 48, step = 1),
                        sliderInput("thermal_tau",
                          tagList("Thermal time constant  τ (min)",
                            help_tip("How quickly the equipment moves toward its final temperature.")),
                          min = 1, max = 120, value = 8, step = 1)
                      )
                    ),
                    column(8,
                      uiOutput("thermal_metrics"),
                      div(class = "card plot-wrap",
                        h3("Temperature response"),
                        plotOutput("thermal_plot", height = 400))
                    )
                  ),
                  div(class = "card",
                    h3("Approximate real-system presets"),
                    tags$table(
                      tags$thead(tags$tr(tags$th("System"), tags$th("Ambient"),
                                        tags$th("Steady rise"), tags$th("τ"), tags$th("Design concern"))),
                      tags$tbody(
                        tags$tr(tags$td("Laptop processor"), tags$td("22 °C"), tags$td("48 °C"),
                                tags$td("8 min"), tags$td("Fast heating and fan response")),
                        tags$tr(tags$td("EV battery pack"), tags$td("25 °C"), tags$td("18 °C"),
                                tags$td("35 min"), tags$td("Uniform temperature and cell life")),
                        tags$tr(tags$td("Industrial motor"), tags$td("30 °C"), tags$td("55 °C"),
                                tags$td("60 min"), tags$td("Insulation temperature limit"))
                      )
                    ),
                    tags$p(class = "hint",
                      "These are educational lumped-model estimates. Real thermal design uses measured losses, ",
                      "temperature-dependent properties, spatial models, duty cycles, and manufacturer limits.")
                  )
                )
              )
            ),
            tabPanel("3 · Linear Algebra", value = "linear_algebra",
              module_header("III", "Linear algebra",
                "Move from systems and determinants to eigenvectors, diagonalization, and structural applications."),
              tabsetPanel(id = "linear_algebra_navigation", type = "pills",
                tabPanel("1 · Overview",
                  div(class = "card",
                    div(class = "eyebrow", "Highest-weight exam domain"),
                    h2("Linear algebra"),
                    tags$p(
                      "Linear algebra carried roughly half of many finals. The recurring core is solving systems, ",
                      "testing invertibility, using determinant identities, finding eigenpairs, and diagonalizing ",
                      "matrices to compute powers, inverses, and roots."
                    )
                  ),
                  div(class = "card",
                    h3("Linear-algebra solution guide"),
                    div(class = "concept",
                      div(class = "card", strong("1 · Structure"),
                          span("Check dimensions, symmetry, triangular form, and obvious dependence.")),
                      div(class = "card", strong("2 · Compute"),
                          span("Use row reduction, determinants, inverse rules, or the characteristic polynomial.")),
                      div(class = "card", strong("3 · Interpret"),
                          span("Connect pivots, determinant, eigenvalues, and geometry instead of treating them separately."))
                    )
                  ),
                  div(class = "card",
                    h3("Repeated exam patterns"),
                    tags$table(
                      tags$thead(tags$tr(tags$th("Pattern"), tags$th("Key connection"))),
                      tags$tbody(
                        tags$tr(tags$td("Systems: none, one, or infinitely many solutions"),
                                tags$td("Compare pivots/ranks of A and [A|b]")),
                        tags$tr(tags$td("Invertibility and determinant identities"),
                                tags$td("det(A)=0 ⇔ singular ⇔ zero is an eigenvalue")),
                        tags$tr(tags$td("Eigenvalues and normalized eigenvectors"),
                                tags$td("Solve det(A−λI)=0, then solve each null space")),
                        tags$tr(tags$td("Diagonalization, powers, and matrix roots"),
                                tags$td("A=PDP⁻¹ makes functions of A become functions of D"))
                      )
                    )
                  )
                ),
                tabPanel("2 · Explore: Systems",
                  div(class = "card",
                    div(class = "eyebrow", "Interactive calculation"),
                    h2("Solve Ax=b and test invertibility"),
                    math_block("\\begin{bmatrix}a&b\\\\c&d\\end{bmatrix}\\begin{bmatrix}x\\\\y\\end{bmatrix}=\\begin{bmatrix}p\\\\q\\end{bmatrix}",
                               "A two by two linear system in matrix form.")
                  ),
                  div(class = "card",
                    h3("System solution guide"),
                    tags$ol(class = "learning-path",
                      tags$li("Compute det(A)=ad−bc."),
                      tags$li("If det(A)≠0, the system has exactly one solution."),
                      tags$li("If det(A)=0, compare the equation rows and constants."),
                      tags$li("Use elimination or A⁻¹b only when the inverse exists."),
                      tags$li("Substitute the answer into both original equations.")
                    )
                  ),
                  fluidRow(
                    column(4,
                      div(class = "card",
                        h3("Coefficients"),
                        sliderInput("mat_a", "a", min = -6, max = 6, value = 3, step = 1),
                        sliderInput("mat_b", "b", min = -6, max = 6, value = 2, step = 1),
                        sliderInput("mat_c", "c", min = -6, max = 6, value = 5, step = 1),
                        sliderInput("mat_d", "d", min = -6, max = 6, value = 3, step = 1),
                        sliderInput("mat_p", "p", min = -12, max = 24, value = 13, step = 1),
                        sliderInput("mat_q", "q", min = -12, max = 24, value = 21, step = 1)
                      )
                    ),
                    column(8,
                      uiOutput("matrix_metrics"),
                      div(class = "card plot-wrap", h3("Two equations as lines"),
                          plotOutput("matrix_plot", height = 420))
                    )
                  )
                ),
                tabPanel("3 · Explore: Eigenvalues",
                  div(class = "card",
                    div(class = "eyebrow", "Geometric intuition"),
                    h2("Eigenvectors and diagonalization"),
                    tags$p("For a symmetric 2×2 matrix, change the entries and watch the eigenvectors remain perpendicular."),
                    math_block("A\\mathbf{v}=\\lambda\\mathbf{v}", "The eigenvalue equation.")
                  ),
                  div(class = "card",
                    h3("Eigenvalue solution guide"),
                    tags$ol(class = "learning-path",
                      tags$li("Form A−λI and set its determinant to zero."),
                      tags$li("Solve the characteristic polynomial for λ."),
                      tags$li("For each λ, solve (A−λI)v=0."),
                      tags$li("Normalize v when requested."),
                      tags$li("Build P from eigenvectors and D from matching eigenvalues.")
                    )
                  ),
                  fluidRow(
                    column(4,
                      div(class = "card",
                        h3("Symmetric matrix"),
                        sliderInput("eig_a", "Top-left a", min = -5, max = 8, value = 4, step = .25),
                        sliderInput("eig_b", "Off-diagonal b", min = -5, max = 5, value = 2, step = .25),
                        sliderInput("eig_d", "Bottom-right d", min = -5, max = 8, value = 1, step = .25)
                      )
                    ),
                    column(8,
                      uiOutput("eigen_metrics"),
                      div(class = "card plot-wrap", h3("Eigenvector directions"),
                          plotOutput("eigen_plot", height = 420))
                    )
                  )
                ),
                tabPanel("4 · Watch: Videos",
                  video_lesson_page(
                    "Linear algebra",
                    paste(
                      "Every card opens one exact concept-matched video.",
                      "Organic Chemistry Tutor lessons are used where a direct match exists;",
                      "specialist visual lessons fill the eigenvalue and diagonalization gaps."
                    ),
                    linear_algebra_videos,
                    "https://www.video-tutor.net/matrices.html"
                  )
                ),
                tabPanel("5 · Apply: Engineering",
                  div(class = "card",
                    div(class = "eyebrow", "Structural engineering"),
                    h2("Solving forces at a truss joint"),
                    tags$p(
                      "Roof trusses, bridges, and cranes carry loads through connected members. At each joint, ",
                      "horizontal and vertical forces must balance. Linear algebra solves the member forces simultaneously."
                    ),
                    math_block(
                      paste0(
                        "\\begin{bmatrix}-\\cos\\theta_L&\\cos\\theta_R\\\\\\sin\\theta_L&\\sin\\theta_R\\end{bmatrix}",
                        "\\begin{bmatrix}F_L\\\\F_R\\end{bmatrix}=",
                        "\\begin{bmatrix}-P_x\\\\P_y\\end{bmatrix}"
                      ),
                      "A two by two equilibrium matrix times the left and right member force vector equals the external load vector."
                    )
                  ),
                  div(class = "card",
                    h3("Truss-joint design guide"),
                    fluidRow(
                      column(6,
                        tags$h4("What should you look for?"),
                        tags$ol(class = "learning-path",
                          tags$li(tags$strong("Draw a free-body diagram. "),
                                  "Show every force acting at the isolated joint."),
                          tags$li(tags$strong("Choose a sign convention. "),
                                  "Assume unknown member forces pull away from the joint."),
                          tags$li(tags$strong("Resolve forces into x and y components. "),
                                  "Use sine and cosine with the correct member angles."),
                          tags$li(tags$strong("Check the determinant. "),
                                  "A near-zero determinant means the geometry cannot resist the load reliably."),
                          tags$li(tags$strong("Interpret the signs. "),
                                  "Positive indicates tension; negative indicates compression for this convention.")
                        )
                      ),
                      column(6,
                        tags$h4("Linear-algebra lesson"),
                        tags$p(
                          "The two equilibrium equations share the same two unknown member forces. Writing them as ",
                          "A F = p exposes invertibility, sensitivity, and the geometric reason some layouts are weak."
                        ),
                        math_block("\\mathbf{F}=A^{-1}\\mathbf{p}\\qquad\\text{when}\\qquad\\det(A)\\ne0",
                                   "Member force vector equals A inverse times the load vector when determinant A is nonzero."),
                        tags$p(class = "hint",
                          "As the members become nearly parallel, the determinant shrinks and small load changes can create very large member forces.")
                      )
                    )
                  ),
                  fluidRow(
                    column(4,
                      div(class = "card",
                        h3("Joint geometry and load"),
                        div(class = "preset-actions",
                          actionButton("truss_roof", "Roof truss"),
                          actionButton("truss_bridge", "Bridge joint"),
                          actionButton("truss_crane", "Crane brace")
                        ),
                        sliderInput("truss_left",
                          tagList("Left-member angle  θL (degrees)",
                            help_tip("The angle between the left member and the horizontal.")),
                          min = 10, max = 80, value = 45, step = 1),
                        sliderInput("truss_right",
                          tagList("Right-member angle  θR (degrees)",
                            help_tip("The angle between the right member and the horizontal.")),
                          min = 10, max = 80, value = 45, step = 1),
                        sliderInput("truss_horizontal",
                          tagList("Horizontal load  Px (kN)",
                            help_tip("A sideways load at the joint; positive points to the right.")),
                          min = -30, max = 30, value = 0, step = 1),
                        sliderInput("truss_vertical",
                          tagList("Downward load  Py (kN)",
                            help_tip("The weight or applied load pulling downward on the joint.")),
                          min = 5, max = 100, value = 20, step = 1)
                      )
                    ),
                    column(8,
                      uiOutput("truss_metrics"),
                      div(class = "card plot-wrap",
                        h3("Joint free-body diagram"),
                        plotOutput("truss_plot", height = 420))
                    )
                  ),
                  div(class = "card",
                    h3("Approximate real-structure presets"),
                    tags$table(
                      tags$thead(tags$tr(tags$th("Scenario"), tags$th("Member angles"),
                                        tags$th("Applied load"), tags$th("Lesson"))),
                      tags$tbody(
                        tags$tr(tags$td("Symmetric roof truss"), tags$td("45° / 45°"),
                                tags$td("20 kN downward"), tags$td("Equal member forces")),
                        tags$tr(tags$td("Bridge-panel joint"), tags$td("35° / 55°"),
                                tags$td("5 kN right, 40 kN down"), tags$td("Asymmetric force sharing")),
                        tags$tr(tags$td("Crane-brace joint"), tags$td("65° / 25°"),
                                tags$td("12 kN left, 30 kN down"), tags$td("Geometry controls force amplification"))
                      )
                    ),
                    tags$p(class = "hint",
                      "These presets demonstrate a pin-jointed, two-force-member idealization. Real design also ",
                      "checks member strength, buckling, connection details, load combinations, and safety factors.")
                  )
                )
              )
            ),
            tabPanel("Exam Practice", value = "exam_coach",
              tabsetPanel(id = "exam_navigation", type = "pills",
                tabPanel("Roadmap & Practice", value = "exam_roadmap",
                  div(class = "card",
                    div(class = "eyebrow", "Based on six finals"),
                    h2("2020–2025 exam roadmap"),
                    tags$p(
                      "Use this page to prioritize recurring skills. The ranking reflects frequency and mark weight ",
                      "across the supplied papers, not a guarantee of the next exam."
                    )
                  ),
                  div(class = "metric-row",
                    div(class = "metric", span("Priority 1"), strong("Linear algebra")),
                    div(class = "metric", span("Priority 2"), strong("Laplace transforms")),
                    div(class = "metric", span("Priority 3"), strong("Differential equations"))
                  ),
                  fluidRow(
                    column(7,
                      div(class = "card",
                        h3("Recurring mastery checklist"),
                        tags$table(
                          tags$thead(tags$tr(tags$th("Skill"), tags$th("Exam signal"), tags$th("Practice target"))),
                          tags$tbody(
                            tags$tr(tags$td("Eigenpairs and diagonalization"), tags$td("Every year"), tags$td("3 complete matrices")),
                            tags$tr(tags$td("Determinants, inverses, singularity"), tags$td("Every year"), tags$td("10 short identities")),
                            tags$tr(tags$td("Direct and inverse Laplace transforms"), tags$td("Every year"), tags$td("8 mixed transforms")),
                            tags$tr(tags$td("Unit steps, impulses, convolution"), tags$td("Frequent"), tags$td("2 of each type")),
                            tags$tr(tags$td("ODE classification and solution"), tags$td("Increasing since 2022"), tags$td("1 linear, 1 Euler, 1 nonlinear"))
                          )
                        )
                      )
                    ),
                    column(5,
                      div(class = "card",
                        h3("Practice generator"),
                        selectInput("practice_topic", "Topic",
                          choices = c("Laplace transforms", "Differential equations", "Linear algebra")),
                        selectInput("practice_level", "Difficulty",
                          choices = c("Foundation", "Exam-style", "Challenge")),
                        actionButton("new_practice", "Generate a new prompt", class = "btn-primary"),
                        hr(),
                        uiOutput("practice_prompt"),
                        actionButton("show_solution", "Show solution guide"),
                        conditionalPanel("input.show_solution % 2 == 1", uiOutput("practice_solution"))
                      )
                    )
                  ),
                  div(class = "card",
                    h3("Coverage by year"),
                    tags$table(
                      tags$thead(tags$tr(tags$th("Final"), tags$th("Differential equations"),
                                        tags$th("Laplace"), tags$th("Linear algebra"))),
                      tags$tbody(
                        tags$tr(tags$td("2020"), tags$td("IVPs"), tags$td("Transforms, convolution, integral equations"), tags$td("Systems, determinants, eigenpairs")),
                        tags$tr(tags$td("2021"), tags$td("Coupled system, impulse IVP"), tags$td("Properties, shifts, integral equations"), tags$td("Diagonalization, roots, determinant rules")),
                        tags$tr(tags$td("2022"), tags$td("Nonlinear ODEs, system"), tags$td("Transforms, shifts, impulse"), tags$td("Orthogonal eigenvectors, diagonalization")),
                        tags$tr(tags$td("2023"), tags$td("System and impulse IVP"), tags$td("Transforms, convolution, shifts"), tags$td("Eigenpairs, powers, singularity")),
                        tags$tr(tags$td("2024"), tags$td("Linear and nonlinear ODEs"), tags$td("Inverse, properties, convolution"), tags$td("Determinants, diagonalization, matrix roots")),
                        tags$tr(tags$td("2025"), tags$td("Cauchy-Euler and nonlinear ODE"), tags$td("Transforms, inverse, integral equation"), tags$td("Systems, inverse, eigenvalues, diagonalization"))
                      )
                    )
                  )
                ),
                tabPanel("Mockup Exam", value = "mock_exam",
                  div(class = "card mock-exam-intro",
                    div(class = "eyebrow", "Five predicted practice versions"),
                    h2("Mockup exam"),
                    tags$p(
                      "Each version is a newly written 100-mark practice final based on the recurring methods in ",
                      "the supplied 2020–2025 papers. It is a study prediction, not a forecast or reproduction of ",
                      "the next official exam."
                    ),
                    div(class = "mock-exam-instructions",
                      strong("Recommended attempt"),
                      span("Allow about 150 minutes, work without revealing hints, then use each solution disclosure to mark your method and final result.")
                    ),
                    div(
                      class = "mock-version-picker",
                      radioButtons(
                        "mock_exam_version", "Choose a mock exam version",
                        choices = names(mock_exam_bank), selected = "Version 1",
                        inline = TRUE
                      )
                    )
                  ),
                  uiOutput("mock_exam_questions")
                )
              )
            ),
            tabPanel("Formula Library", value = "reference",
              div(class = "card",
                div(class = "eyebrow", "Course formula sheet"),
                h2("Formula library"),
                tags$p("Use these formulas to identify a method, then verify the conditions before applying it.")
              ),
              tabsetPanel(id = "reference_navigation", type = "pills",
                tabPanel("Laplace",
                  div(class = "card",
                    h3("Definition and core properties"),
                    math_block(
                      "F(s)=\\mathcal{L}\\{f(t)\\}=\\int_{0}^{\\infty}e^{-st}f(t)\\,dt",
                      "F of s equals the Laplace transform of f of t, equal to the integral from zero to infinity of e to the negative s t times f of t d t."
                    ),
                    tags$table(
                      tags$thead(tags$tr(tags$th("Operation"), tags$th("Transform rule"))),
                      tags$tbody(
                        tags$tr(tags$td("Derivative"), tags$td(math_inline("\\mathcal{L}\\{f'(t)\\}=sF(s)-f(0)", "Laplace of f prime equals s F of s minus f of zero."))),
                        tags$tr(tags$td("Second derivative"), tags$td(math_inline("\\mathcal{L}\\{f''(t)\\}=s^2F(s)-sf(0)-f'(0)", "Laplace of f double prime equals s squared F minus s f of zero minus f prime of zero."))),
                        tags$tr(tags$td("Multiply by t"), tags$td(math_inline("\\mathcal{L}\\{tf(t)\\}=-\\frac{dF}{ds}", "Laplace of t times f equals negative derivative of F with respect to s."))),
                        tags$tr(tags$td("Frequency shift"), tags$td(math_inline("\\mathcal{L}\\{e^{at}f(t)\\}=F(s-a)", "Laplace of e to the a t times f equals F of s minus a."))),
                        tags$tr(tags$td("Time delay"), tags$td(math_inline("\\mathcal{L}\\{f(t-a)u(t-a)\\}=e^{-as}F(s)", "Time delay property."))),
                        tags$tr(tags$td("Convolution"), tags$td(math_inline("\\mathcal{L}\\{f*g\\}=F(s)G(s)", "Laplace of a convolution equals the product F G."))),
                        tags$tr(tags$td("Impulse"), tags$td(math_inline("\\mathcal{L}\\{\\delta(t-a)\\}=e^{-as}", "Laplace of a delayed impulse equals e to the negative a s.")))
                      )
                    )
                  ),
                  div(class = "card",
                    h3("Common transform pairs",
                      help_tip("A transform pair is like a word and its translation: the left side is the time signal and the right side is its Laplace version.")),
                    tags$table(
                      tags$thead(tags$tr(tags$th("Time function f(t)"), tags$th("Laplace transform F(s)"))),
                      tags$tbody(
                        tags$tr(tags$td(math_inline("1", "one")), tags$td(math_inline("\\frac{1}{s}", "one over s"))),
                        tags$tr(tags$td(math_inline("t^n", "t to the n")), tags$td(math_inline("\\frac{n!}{s^{n+1}}", "n factorial over s to the n plus one"))),
                        tags$tr(tags$td(math_inline("e^{-at}", "e to the negative a t")), tags$td(math_inline("\\frac{1}{s+a}", "one over s plus a"))),
                        tags$tr(tags$td(math_inline("\\sin(\\omega t)", "sine omega t")), tags$td(math_inline("\\frac{\\omega}{s^2+\\omega^2}", "omega over s squared plus omega squared"))),
                        tags$tr(tags$td(math_inline("\\cos(\\omega t)", "cosine omega t")), tags$td(math_inline("\\frac{s}{s^2+\\omega^2}", "s over s squared plus omega squared"))),
                        tags$tr(tags$td(math_inline("\\sinh(at)", "hyperbolic sine a t")), tags$td(math_inline("\\frac{a}{s^2-a^2}", "a over s squared minus a squared"))),
                        tags$tr(tags$td(math_inline("\\cosh(at)", "hyperbolic cosine a t")), tags$td(math_inline("\\frac{s}{s^2-a^2}", "s over s squared minus a squared")))
                      )
                    )
                  )
                ),
                tabPanel("Differential Equations",
                  div(class = "card",
                    h3("Constant-coefficient linear equations"),
                    math_block("ay''+by'+cy=g(x)\\quad\\Longrightarrow\\quad ar^2+br+c=0",
                               "A y double prime plus b y prime plus c y equals g of x, leading to the characteristic equation a r squared plus b r plus c equals zero."),
                    tags$table(
                      tags$thead(tags$tr(tags$th("Characteristic roots"), tags$th("Homogeneous solution"))),
                      tags$tbody(
                        tags$tr(tags$td(math_inline("r_1,r_2", "distinct real roots r one and r two")),
                                tags$td(math_inline("y_h=C_1e^{r_1x}+C_2e^{r_2x}", "homogeneous solution for distinct roots"))),
                        tags$tr(tags$td(math_inline("r,r", "repeated real root r")),
                                tags$td(math_inline("y_h=(C_1+C_2x)e^{rx}", "homogeneous solution for a repeated root"))),
                        tags$tr(tags$td(math_inline("\\alpha\\pm i\\beta", "complex roots alpha plus or minus i beta")),
                                tags$td(math_inline("y_h=e^{\\alpha x}(C_1\\cos\\beta x+C_2\\sin\\beta x)", "homogeneous solution for complex roots")))
                      )
                    ),
                    tags$p(class = "hint",
                      "For a forced equation, ", math_inline("y=y_h+y_p", "y equals homogeneous plus particular solution"),
                      ". Multiply the usual trial for ", math_inline("y_p", "the particular solution"),
                      " by ", math_inline("x", "x"), " when it duplicates a homogeneous mode.")
                  ),
                  div(class = "card",
                    h3("Cauchy-Euler and order reduction"),
                    math_block("ax^2y''+bxy'+cy=0\\quad\\Longrightarrow\\quad y=x^m",
                               "For a Cauchy Euler equation, try y equals x to the m."),
                    math_block("x\\text{ missing:}\\quad u(y)=y',\\qquad y''=u\\frac{du}{dy}",
                               "If x is missing, let u of y equal y prime, so y double prime equals u d u d y."),
                    math_block("y\\text{ missing:}\\quad u(x)=y',\\qquad y''=\\frac{du}{dx}",
                               "If y is missing, let u of x equal y prime, so y double prime equals d u d x."),
                    tags$p(class = "hint",
                      "After reducing the order, solve for u first and integrate once more to recover y.")
                  ),
                  div(class = "card",
                    h3("Two-state linear systems"),
                    math_block("\\mathbf{u}'=A\\mathbf{u},\\qquad \\det(A-\\lambda I)=0,\\qquad \\mathbf{u}=\\sum_i c_i\\mathbf{v}_i e^{\\lambda_i t}",
                               "Vector u prime equals A u. Eigenvalues solve determinant A minus lambda I equals zero. The solution is a sum of eigenmodes."),
                    tags$p(class = "hint",
                      "Negative real parts imply decay; positive real parts imply growth; opposite signs produce a saddle; imaginary parts produce rotation or oscillation.")
                  )
                ),
                tabPanel("Linear Algebra",
                  div(class = "card",
                    h3("Two-by-two essentials"),
                    math_block("A=\\begin{bmatrix}a&b\\\\c&d\\end{bmatrix},\\qquad \\det(A)=ad-bc",
                               "Two by two matrix A has determinant a d minus b c."),
                    math_block("A^{-1}=\\frac{1}{\\det(A)}\\begin{bmatrix}d&-b\\\\-c&a\\end{bmatrix},\\qquad \\det(A)\\ne0",
                               "The inverse of a two by two matrix is one over its determinant times d negative b, negative c a."),
                    tags$p(class = "hint",
                      "For ", math_inline("A\\mathbf{x}=\\mathbf{b}", "A x equals b"), ": ",
                      math_inline("\\det(A)\\ne0", "determinant A is nonzero"), " means one solution. If ",
                      math_inline("\\det(A)=0", "determinant A equals zero"), ", compare ",
                      math_inline("\\operatorname{rank}(A)", "rank of A"), " with ",
                      math_inline("\\operatorname{rank}([A\\mid\\mathbf{b}])", "rank of the augmented matrix"), ".")
                  ),
                  div(class = "card",
                    h3("Determinant identities"),
                    tags$table(
                      tags$thead(tags$tr(tags$th("Expression"), tags$th("Identity"))),
                      tags$tbody(
                        tags$tr(tags$td(math_inline("\\det(AB)", "determinant of A B")), tags$td(math_inline("\\det(A)\\det(B)", "determinant A times determinant B"))),
                        tags$tr(tags$td(math_inline("\\det(A^T)", "determinant of A transpose")), tags$td(math_inline("\\det(A)", "determinant of A"))),
                        tags$tr(tags$td(math_inline("\\det(A^{-1})", "determinant of A inverse")), tags$td(math_inline("\\frac{1}{\\det(A)}", "one over determinant A"))),
                        tags$tr(tags$td(math_inline("\\det(A^k)", "determinant of A to the k")), tags$td(math_inline("[\\det(A)]^k", "determinant A to the k"))),
                        tags$tr(tags$td(math_inline("\\det(cA),\\;A\\in\\mathbb{R}^{n\\times n}", "determinant c A for n by n A")), tags$td(math_inline("c^n\\det(A)", "c to the n times determinant A"))),
                        tags$tr(tags$td(math_inline("\\det(I)", "determinant of identity")), tags$td(math_inline("1", "one")))
                      )
                    )
                  ),
                  div(class = "card",
                    h3("Eigenvalues and diagonalization"),
                    math_block("A\\mathbf{v}=\\lambda\\mathbf{v},\\qquad \\det(A-\\lambda I)=0",
                               "A times eigenvector v equals lambda v, and eigenvalues solve determinant A minus lambda I equals zero."),
                    math_block("A=PDP^{-1}\\quad\\Longrightarrow\\quad A^k=PD^kP^{-1}",
                               "If A equals P D P inverse, then A to the k equals P D to the k P inverse."),
                    math_block("A=A^T\\quad\\Longrightarrow\\quad P^{-1}=P^T",
                               "For a symmetric matrix, the inverse of the orthogonal eigenvector matrix P equals its transpose."),
                    tags$table(
                      tags$thead(tags$tr(tags$th("Connection"), tags$th("Result"))),
                      tags$tbody(
                        tags$tr(tags$td(math_inline("\\det(A)", "determinant of A")), tags$td("Product of eigenvalues")),
                        tags$tr(tags$td(math_inline("\\operatorname{tr}(A)", "trace of A")), tags$td("Sum of eigenvalues")),
                        tags$tr(tags$td(math_inline("A^{-1}", "A inverse")), tags$td("Eigenvalues become ", math_inline("\\lambda^{-1}", "one over lambda"))),
                        tags$tr(tags$td(math_inline("A^k", "A to the k")), tags$td("Eigenvalues become ", math_inline("\\lambda^k", "lambda to the k"))),
                        tags$tr(tags$td("Singular A"), tags$td("At least one eigenvalue is 0"))
                      )
                    )
                  )
                ),
                tabPanel("Method Selector",
                  div(class = "card",
                    h3("What should I try first?"),
                    tags$table(
                      tags$thead(tags$tr(tags$th("Clue in the question"), tags$th("First method to consider"))),
                      tags$tbody(
                        tags$tr(tags$td("Delay, unit step, impulse, or integral equation"), tags$td("Laplace transform")),
                        tags$tr(tags$td("Constant-coefficient homogeneous ODE"), tags$td("Characteristic polynomial")),
                        tags$tr(tags$td("Coefficients follow ", math_inline("x^2,x,1", "x squared, x, constant")), tags$td("Cauchy-Euler trial ", math_inline("y=x^m", "y equals x to the m"))),
                        tags$tr(tags$td("Nonlinear second-order ODE missing x or y"), tags$td("Reduce order with ", math_inline("u=y'", "u equals y prime"))),
                        tags$tr(tags$td("Parameterized system: none, one, or infinite"), tags$td("Row reduction and rank")),
                        tags$tr(tags$td("Matrix powers or roots"), tags$td("Diagonalization")),
                        tags$tr(tags$td("Invertibility or singularity"), tags$td("Determinant, pivots, or zero eigenvalue"))
                      )
                    ),
                    math_block("\\text{Recognize}\\;\\longrightarrow\\;\\text{Choose}\\;\\longrightarrow\\;\\text{Solve}\\;\\longrightarrow\\;\\text{Verify}",
                               "Recognize, then choose, then solve, then verify.")
                  )
                )
              )
            ),
            tabPanel(
              "Source Prompts",
              value = "source_prompts",
              source_prompts_page()
            )
          )
      )
  )
)

server <- function(input, output, session) {
  plot_bg <- "#08090b"
  plot_axis <- "#a6a6aa"
  plot_text <- "#eeeeea"
  plot_grid <- "#292a2e"
  plot_gold <- "#4da3ff"
  plot_ruby <- "#6f7cff"
  plot_emerald <- "#23b8d1"
  plot_muted <- "#85786d"
  plot_orange <- "#2f7ed8"

  current <- reactive(signal_catalogue[[input$signal]])

  output$parameter_controls <- renderUI({
    sig <- current()
    lapply(unname(sig$parameters), function(id) {
      r <- sig$ranges[[id]]
      sliderInput(paste0("p_", id), names(sig$parameters)[unname(sig$parameters) == id],
                  min = r[1], max = r[2], value = sig$defaults[id], step = r[3])
    })
  })

  params <- reactive({
    sig <- current()
    values <- vapply(unname(sig$parameters), function(id) {
      value <- input[[paste0("p_", id)]]
      if (is.null(value)) sig$defaults[id] else value
    }, numeric(1))
    setNames(values, unname(sig$parameters))
  })

  output$formula_card <- renderUI({
    p <- params()
    tagList(
      math_block(current()$formula(p), "Selected time-domain function."),
      math_block(current()$laplace(p), "Its Laplace transform and convergence condition.")
    )
  })

  dark_plot <- function(xlab, ylab) {
    par(bg = plot_bg, fg = plot_axis, col.axis = plot_axis, col.lab = plot_text,
        mar = c(4.2, 4.4, 1, 1), family = "sans")
  }

  output$time_plot <- renderPlot({
    t <- seq(0, input$tmax, length.out = 700)
    y <- current()$time(t, params())
    dark_plot()
    plot(t, y, type = "n", xlab = "time  t", ylab = "amplitude")
    grid(col = plot_grid, lty = 1)
    lines(t, y, col = plot_gold, lwd = 3)
    abline(h = 0, col = plot_muted, lwd = 1)
  }, res = 110)

  output$laplace_plot <- renderPlot({
    s <- seq(.08, 8, length.out = 700)
    y <- Re(current()$transform(s, params()))
    cap <- quantile(abs(y[is.finite(y)]), .94)
    y <- pmax(pmin(y, cap), -cap)
    dark_plot()
    plot(s, y, type = "n", xlab = "real frequency  s", ylab = "F(s)")
    grid(col = plot_grid, lty = 1)
    lines(s, y, col = plot_ruby, lwd = 3)
    abline(h = 0, col = plot_muted, lwd = 1)
  }, res = 110)

  output$shift_plot <- renderPlot({
    t <- seq(0, 12, length.out = 800)
    original <- exp(-input$decay * t)
    shifted <- ifelse(t >= input$shift, exp(-input$decay * (t - input$shift)), 0)
    dark_plot()
    plot(t, original, type = "n", ylim = c(0, 1.08), xlab = "time  t", ylab = "amplitude")
    grid(col = plot_grid, lty = 1)
    lines(t, original, col = plot_gold, lwd = 3)
    lines(t, shifted, col = plot_emerald, lwd = 3)
    abline(v = input$shift, col = plot_muted, lty = 3)
    legend("topright", c("f(t)", "u(t−a)f(t−a)"), col = c(plot_gold, plot_emerald),
           lwd = 3, bty = "n", text.col = plot_text)
  }, res = 110)

  output$shift_laplace_plot <- renderPlot({
    s <- seq(.05, 6, length.out = 700)
    original <- 1 / (s + input$decay)
    shifted <- exp(-input$shift * s) / (s + input$decay)
    dark_plot()
    plot(s, original, type = "n", xlab = "real frequency  s", ylab = "magnitude")
    grid(col = plot_grid, lty = 1)
    lines(s, original, col = plot_gold, lwd = 3)
    lines(s, shifted, col = plot_emerald, lwd = 3)
    legend("topright", c("F(s)", "e^(−as)F(s)"), col = c(plot_gold, plot_emerald),
           lwd = 3, bty = "n", text.col = plot_text)
  }, res = 110)

  load_vehicle_preset <- function(preset) {
    updateSliderInput(session, "mass", value = unname(preset["mass"]))
    updateSliderInput(session, "stiffness", value = unname(preset["stiffness"]))
    updateSliderInput(session, "damping", value = unname(preset["damping"]))
    updateSliderInput(session, "force", value = unname(preset["force"]))
  }

  observeEvent(input$preset_outback, load_vehicle_preset(vehicle_presets$outback))
  observeEvent(input$preset_f150, load_vehicle_preset(vehicle_presets$f150))
  observeEvent(input$preset_crv, load_vehicle_preset(vehicle_presets$crv))

  load_chemical_preset <- function(preset) {
    updateSliderInput(session, "tank_volume", value = unname(preset["volume"]))
    updateSliderInput(session, "flow_rate", value = unname(preset["flow"]))
    updateSliderInput(session, "initial_conc", value = unname(preset["initial"]))
    updateSliderInput(session, "inlet_conc", value = unname(preset["inlet"]))
  }

  observeEvent(input$preset_beverage, load_chemical_preset(chemical_presets$beverage))
  observeEvent(input$preset_pharma, load_chemical_preset(chemical_presets$pharma))
  observeEvent(input$preset_chlorine, load_chemical_preset(chemical_presets$chlorine))

  load_building_preset <- function(preset) {
    updateSliderInput(session, "building_mass", value = unname(preset["mass"]))
    updateSliderInput(session, "building_frequency", value = unname(preset["frequency"]))
    updateSliderInput(session, "building_damping", value = unname(preset["damping"]))
    updateSliderInput(session, "wind_force", value = unname(preset["force"]))
  }

  observeEvent(input$preset_taipei, load_building_preset(building_presets$taipei))
  observeEvent(input$preset_burj, load_building_preset(building_presets$burj))
  observeEvent(input$preset_cn, load_building_preset(building_presets$cn))

  suspension_data <- reactive({
    m <- input$mass
    c <- input$damping
    k <- input$stiffness
    f0 <- input$force
    wn <- sqrt(k / m)
    zeta <- c / (2 * sqrt(k * m))
    t <- seq(0, 15, length.out = 1200)
    xeq <- f0 / k

    # Closed-form unit-step response obtained from
    # X(s) = F0 / [s(ms^2 + cs + k)].
    if (zeta < 0.999) {
      wd <- wn * sqrt(1 - zeta^2)
      x <- xeq * (1 - exp(-zeta * wn * t) *
                    (cos(wd * t) + zeta / sqrt(1 - zeta^2) * sin(wd * t)))
      regime <- "Underdamped"
    } else if (zeta <= 1.001) {
      x <- xeq * (1 - exp(-wn * t) * (1 + wn * t))
      regime <- "Critically damped"
    } else {
      r1 <- -wn * (zeta - sqrt(zeta^2 - 1))
      r2 <- -wn * (zeta + sqrt(zeta^2 - 1))
      x <- xeq * (1 + (r2 * exp(r1 * t) - r1 * exp(r2 * t)) / (r1 - r2))
      regime <- "Overdamped"
    }

    outside <- abs(x - xeq) > .02 * xeq
    last_outside <- if (any(outside)) max(which(outside)) else 1
    settling <- if (last_outside < length(t)) t[last_outside + 1] else NA_real_
    overshoot <- max(0, (max(x) - xeq) / xeq * 100)

    list(t = t, x = x, xeq = xeq, wn = wn, zeta = zeta, regime = regime,
         settling = settling, overshoot = overshoot)
  })

  output$suspension_metrics <- renderUI({
    d <- suspension_data()
    settle_text <- if (is.na(d$settling)) "> 15 s" else sprintf("%.2f s", d$settling)
    div(class = "metric-row",
      div(class = "metric",
          span("Damping ratio  ζ",
               help_tip("This number says how strongly the shock absorbers calm the bouncing. Small means bouncy; near one means it settles quickly.")),
          strong(sprintf("%.2f · %s", d$zeta, d$regime))),
      div(class = "metric",
          span("Overshoot",
               help_tip("Overshoot is how far the car moves past its final resting position before coming back.")),
          strong(sprintf("%.1f%%", d$overshoot))),
      div(class = "metric",
          span("2% settling time",
               help_tip("This is how long it takes until the car stays very close to its final resting position.")),
          strong(settle_text))
    )
  })

  output$suspension_plot <- renderPlot({
    d <- suspension_data()
    dark_plot()
    ymax <- max(d$x, d$xeq) * 1.14
    plot(d$t, d$x * 1000, type = "n", ylim = c(0, ymax * 1000),
         xlab = "time after force is applied (s)", ylab = "displacement (mm)")
    grid(col = plot_grid, lty = 1)
    polygon(c(d$t, rev(d$t)), c(d$x * 1000, rep(0, length(d$t))),
            col = adjustcolor(plot_gold, alpha.f = .13), border = NA)
    lines(d$t, d$x * 1000, col = plot_gold, lwd = 3)
    abline(h = d$xeq * 1000, col = plot_ruby, lty = 2, lwd = 2)
    legend("topright", c("Body response", "Final position"), col = c(plot_gold, plot_ruby),
           lwd = c(3, 2), lty = c(1, 2), bty = "n", text.col = plot_text)
  }, res = 110)

  output$frequency_plot <- renderPlot({
    w <- 10^seq(-1, 2, length.out = 800)
    magnitude <- 1 / sqrt((input$stiffness - input$mass * w^2)^2 +
                            (input$damping * w)^2)
    dark_plot()
    plot(w, magnitude * 1e6, type = "n", log = "x",
         xlab = "angular frequency  ω (rad/s)", ylab = "|H(jω)|  (mm/kN)")
    grid(col = plot_grid, lty = 1)
    lines(w, magnitude * 1e6, col = plot_emerald, lwd = 3)
    abline(v = suspension_data()$wn, col = plot_muted, lty = 3)
  }, res = 110)

  tank_data <- reactive({
    tau <- input$tank_volume / input$flow_rate
    t <- seq(0, 5 * tau, length.out = 700)
    concentration <- input$inlet_conc +
      (input$initial_conc - input$inlet_conc) * exp(-t / tau)
    list(t = t, concentration = concentration, tau = tau)
  })

  output$tank_metrics <- renderUI({
    d <- tank_data()
    div(class = "metric-row",
      div(class = "metric",
          span("Time constant  τ",
               help_tip("After one time constant, the tank has completed about 63% of its concentration change.")),
          strong(sprintf("%.1f min", d$tau))),
      div(class = "metric",
          span("About 95% complete",
               help_tip("After roughly three time constants, the tank is very close to its new concentration.")),
          strong(sprintf("%.1f min", 3 * d$tau))),
      div(class = "metric",
          span("Laplace pole",
               help_tip("The pole is the number that controls how quickly this process forgets its starting condition.")),
          strong(sprintf("%.3f min⁻¹", -1 / d$tau)))
    )
  })

  output$tank_plot <- renderPlot({
    d <- tank_data()
    y_range <- range(c(input$initial_conc, input$inlet_conc, d$concentration))
    padding <- max(diff(y_range) * .15, .08)
    dark_plot()
    plot(d$t, d$concentration, type = "n",
         ylim = y_range + c(-padding, padding),
         xlab = "time after inlet change (min)", ylab = "relative concentration")
    grid(col = plot_grid, lty = 1)
    polygon(c(d$t, rev(d$t)),
            c(d$concentration, rep(min(y_range) - padding, length(d$t))),
            col = adjustcolor(plot_emerald, alpha.f = .13), border = NA)
    lines(d$t, d$concentration, col = plot_emerald, lwd = 3)
    abline(h = input$inlet_conc, col = plot_ruby, lty = 2, lwd = 2)
    abline(v = d$tau, col = plot_muted, lty = 3)
    legend("bottomright", c("Tank concentration", "New inlet value", "One time constant"),
           col = c(plot_emerald, plot_ruby, plot_muted), lwd = c(3, 2, 1),
           lty = c(1, 2, 3), bty = "n", text.col = plot_text)
  }, res = 110)

  building_data <- reactive({
    mass <- input$building_mass * 1e6
    wn <- 2 * pi * input$building_frequency
    zeta <- input$building_damping / 100
    stiffness <- mass * wn^2
    damping <- 2 * zeta * mass * wn
    force <- input$wind_force * 1000
    xeq <- force / stiffness
    wd <- wn * sqrt(1 - zeta^2)
    t_end <- max(60, min(300, 6 / (zeta * wn)))
    t <- seq(0, t_end, length.out = 1000)
    x <- xeq * (1 - exp(-zeta * wn * t) *
                  (cos(wd * t) + zeta / sqrt(1 - zeta^2) * sin(wd * t)))
    list(t = t, x = x, xeq = xeq, wn = wn, zeta = zeta,
         stiffness = stiffness, damping = damping,
         settling = 4 / (zeta * wn), peak = max(x))
  })

  output$building_metrics <- renderUI({
    d <- building_data()
    div(class = "metric-row",
      div(class = "metric",
          span("Natural period",
               help_tip("How many seconds one free back-and-forth sway would take.")),
          strong(sprintf("%.2f s", 2 * pi / d$wn))),
      div(class = "metric",
          span("Peak top movement",
               help_tip("The largest sideways movement predicted by this simplified model.")),
          strong(sprintf("%.1f mm", d$peak * 1000))),
      div(class = "metric",
          span("Approx. settling time",
               help_tip("About how long the wind-triggered vibration takes to become very small.")),
          strong(sprintf("%.1f s", d$settling)))
    )
  })

  output$building_plot <- renderPlot({
    d <- building_data()
    ymax <- max(d$x, d$xeq) * 1.15
    dark_plot()
    plot(d$t, d$x * 1000, type = "n", ylim = c(0, ymax * 1000),
         xlab = "time after wind begins (s)", ylab = "top displacement (mm)")
    grid(col = plot_grid, lty = 1)
    polygon(c(d$t, rev(d$t)), c(d$x * 1000, rep(0, length(d$t))),
            col = adjustcolor(plot_ruby, alpha.f = .13), border = NA)
    lines(d$t, d$x * 1000, col = plot_ruby, lwd = 3)
    abline(h = d$xeq * 1000, col = plot_gold, lty = 2, lwd = 2)
    legend("topright", c("Building response", "Static wind position"),
           col = c(plot_ruby, plot_gold), lwd = c(3, 2), lty = c(1, 2),
           bty = "n", text.col = plot_text)
  }, res = 110)

  load_thermal_preset <- function(preset) {
    updateSliderInput(session, "thermal_ambient", value = unname(preset["ambient"]))
    updateSliderInput(session, "thermal_initial", value = unname(preset["initial"]))
    updateSliderInput(session, "thermal_rise", value = unname(preset["rise"]))
    updateSliderInput(session, "thermal_tau", value = unname(preset["tau"]))
  }

  observeEvent(input$thermal_laptop, load_thermal_preset(thermal_presets$laptop))
  observeEvent(input$thermal_battery, load_thermal_preset(thermal_presets$battery))
  observeEvent(input$thermal_motor, load_thermal_preset(thermal_presets$motor))

  thermal_data <- reactive({
    steady <- input$thermal_ambient + input$thermal_rise
    t <- seq(0, 5 * input$thermal_tau, length.out = 800)
    temperature <- steady + (input$thermal_initial - steady) *
      exp(-t / input$thermal_tau)
    list(t = t, temperature = temperature, steady = steady,
         tau = input$thermal_tau, t95 = 3 * input$thermal_tau)
  })

  output$thermal_metrics <- renderUI({
    d <- thermal_data()
    div(class = "metric-row",
      div(class = "metric", span("Steady temperature"), strong(sprintf("%.1f °C", d$steady))),
      div(class = "metric", span("Time constant  τ"), strong(sprintf("%.1f min", d$tau))),
      div(class = "metric", span("About 95% settled"), strong(sprintf("%.1f min", d$t95)))
    )
  })

  output$thermal_plot <- renderPlot({
    d <- thermal_data()
    y_range <- range(c(d$temperature, d$steady, input$thermal_ambient))
    padding <- max(diff(y_range) * .12, 3)
    dark_plot()
    plot(d$t, d$temperature, type = "n", ylim = y_range + c(-padding, padding),
         xlab = "time (min)", ylab = "temperature (°C)")
    grid(col = plot_grid)
    polygon(c(d$t, rev(d$t)),
            c(d$temperature, rep(y_range[1] - padding, length(d$t))),
            col = adjustcolor(plot_gold, alpha.f = .13), border = NA)
    lines(d$t, d$temperature, col = plot_gold, lwd = 3)
    abline(h = d$steady, col = plot_ruby, lty = 2, lwd = 2)
    abline(v = d$tau, col = plot_muted, lty = 3)
    legend("bottomright", c("Equipment temperature", "Steady temperature", "One time constant"),
           col = c(plot_gold, plot_ruby, plot_muted), lwd = c(3, 2, 1),
           lty = c(1, 2, 3), bty = "n", text.col = plot_text)
  }, res = 110)

  load_truss_preset <- function(preset) {
    updateSliderInput(session, "truss_left", value = unname(preset["left"]))
    updateSliderInput(session, "truss_right", value = unname(preset["right"]))
    updateSliderInput(session, "truss_horizontal", value = unname(preset["horizontal"]))
    updateSliderInput(session, "truss_vertical", value = unname(preset["vertical"]))
  }

  observeEvent(input$truss_roof, load_truss_preset(truss_presets$roof))
  observeEvent(input$truss_bridge, load_truss_preset(truss_presets$bridge))
  observeEvent(input$truss_crane, load_truss_preset(truss_presets$crane))

  truss_data <- reactive({
    left <- input$truss_left * pi / 180
    right <- input$truss_right * pi / 180
    A <- matrix(c(-cos(left), sin(left), cos(right), sin(right)), 2, 2)
    load <- c(-input$truss_horizontal, input$truss_vertical)
    determinant <- det(A)
    forces <- if (abs(determinant) > 1e-8) solve(A, load) else c(NA_real_, NA_real_)
    list(left = left, right = right, A = A, determinant = determinant,
         forces = forces, px = input$truss_horizontal, py = input$truss_vertical)
  })

  output$truss_metrics <- renderUI({
    d <- truss_data()
    force_label <- function(value) {
      if (!is.finite(value)) return("Indeterminate")
      sprintf("%.1f kN · %s", abs(value), if (value >= 0) "tension" else "compression")
    }
    geometry <- if (abs(d$determinant) < .2) "Poor: high force sensitivity" else
      if (abs(d$determinant) < .5) "Moderate" else "Well-conditioned"
    div(class = "metric-row",
      div(class = "metric", span("Left member"), strong(force_label(d$forces[1]))),
      div(class = "metric", span("Right member"), strong(force_label(d$forces[2]))),
      div(class = "metric", span("Geometry"), strong(geometry))
    )
  })

  output$truss_plot <- renderPlot({
    d <- truss_data()
    dark_plot()
    plot(0, 0, type = "n", xlim = c(-5.5, 5.5), ylim = c(-2.5, 5),
         xlab = "horizontal position", ylab = "vertical position", asp = 1,
         axes = FALSE)
    grid(col = plot_grid)
    left_end <- c(-4 * cos(d$left), 4 * sin(d$left))
    right_end <- c(4 * cos(d$right), 4 * sin(d$right))
    lines(c(0, left_end[1]), c(0, left_end[2]), col = plot_gold, lwd = 8)
    lines(c(0, right_end[1]), c(0, right_end[2]), col = plot_ruby, lwd = 8)
    points(c(left_end[1], right_end[1]), c(left_end[2], right_end[2]),
           pch = 24, bg = plot_muted, col = plot_text, cex = 1.4)
    points(0, 0, pch = 21, bg = plot_emerald, col = plot_text, cex = 1.8)

    load_scale <- 2 / max(10, sqrt(d$px^2 + d$py^2))
    arrows(0, 0, d$px * load_scale, -d$py * load_scale,
           length = .12, lwd = 3, col = plot_orange)
    text(d$px * load_scale, -d$py * load_scale - .25, "Applied load", col = plot_text)
    if (all(is.finite(d$forces))) {
      text(left_end[1] / 2, left_end[2] / 2 + .25,
           sprintf("%.1f kN", d$forces[1]), col = plot_text)
      text(right_end[1] / 2, right_end[2] / 2 + .25,
           sprintf("%.1f kN", d$forces[2]), col = plot_text)
    }
    legend("topright", c("Left member", "Right member", "External load"),
           col = c(plot_gold, plot_ruby, plot_orange), lwd = c(8, 8, 3),
           bty = "n", text.col = plot_text)
  }, res = 110)

  ode_data <- reactive({
    a <- input$ode_a
    b <- input$ode_b
    y0 <- input$ode_y0
    v0 <- input$ode_v0
    disc <- a^2 - 4 * b
    t <- seq(0, 10, length.out = 900)

    if (disc > 1e-8) {
      r1 <- (-a + sqrt(disc)) / 2
      r2 <- (-a - sqrt(disc)) / 2
      c1 <- (v0 - r2 * y0) / (r1 - r2)
      c2 <- y0 - c1
      y <- c1 * exp(r1 * t) + c2 * exp(r2 * t)
      roots <- sprintf("r₁ = %.2f, r₂ = %.2f", r1, r2)
      regime <- "Two distinct real roots"
      form <- sprintf("y = %.2fe^(%.2ft) + %.2fe^(%.2ft)", c1, r1, c2, r2)
    } else if (disc < -1e-8) {
      alpha <- -a / 2
      beta <- sqrt(-disc) / 2
      c1 <- y0
      c2 <- (v0 - alpha * y0) / beta
      y <- exp(alpha * t) * (c1 * cos(beta * t) + c2 * sin(beta * t))
      roots <- sprintf("r = %.2f ± %.2fi", alpha, beta)
      regime <- "Complex-conjugate roots"
      form <- sprintf("y = e^(%.2ft)[%.2f cos(%.2ft) + %.2f sin(%.2ft)]",
                      alpha, c1, beta, c2, beta)
    } else {
      r <- -a / 2
      c1 <- y0
      c2 <- v0 - r * y0
      y <- (c1 + c2 * t) * exp(r * t)
      roots <- sprintf("r = %.2f (repeated)", r)
      regime <- "Repeated real root"
      form <- sprintf("y = (%.2f + %.2ft)e^(%.2ft)", c1, c2, r)
    }
    y[abs(y) > 1e5] <- NA_real_
    list(t = t, y = y, roots = roots, regime = regime, form = form)
  })

  output$ode_metrics <- renderUI({
    d <- ode_data()
    div(class = "metric-row",
      div(class = "metric", span("Root type"), strong(d$regime)),
      div(class = "metric", span("Characteristic roots"), strong(d$roots)),
      div(class = "metric", span("Solution"), strong(d$form))
    )
  })

  output$ode_plot <- renderPlot({
    d <- ode_data()
    finite_y <- d$y[is.finite(d$y)]
    validate(need(length(finite_y) > 1, "The solution grows beyond the display range."))
    cap <- max(5, quantile(abs(finite_y), .98))
    y <- pmax(pmin(d$y, cap), -cap)
    dark_plot()
    plot(d$t, y, type = "n", xlab = "time", ylab = "y(t)")
    grid(col = plot_grid)
    lines(d$t, y, col = plot_gold, lwd = 3)
    abline(h = 0, col = plot_muted)
  }, res = 110)

  system_data <- reactive({
    A <- matrix(c(input$sys_a, input$sys_c, input$sys_b, input$sys_d), 2, 2)
    ev <- eigen(A)$values
    real_parts <- Re(ev)
    stability <- if (all(real_parts < -1e-7)) "Stable: trajectories decay" else
      if (any(real_parts > 1e-7) && any(real_parts < -1e-7)) "Saddle: one mode grows" else
      if (any(real_parts > 1e-7)) "Unstable: trajectories grow" else "Neutral or marginal"
    list(A = A, ev = ev, stability = stability)
  })

  output$system_metrics <- renderUI({
    d <- system_data()
    fmt <- function(z) {
      if (abs(Im(z)) < 1e-8) sprintf("%.2f", Re(z))
      else sprintf("%.2f %+.2fi", Re(z), Im(z))
    }
    div(class = "metric-row",
      div(class = "metric", span("Eigenvalue λ₁"), strong(fmt(d$ev[1]))),
      div(class = "metric", span("Eigenvalue λ₂"), strong(fmt(d$ev[2]))),
      div(class = "metric", span("Phase behavior"), strong(d$stability))
    )
  })

  output$system_plot <- renderPlot({
    A <- system_data()$A
    dark_plot()
    plot(0, 0, type = "n", xlim = c(-6, 6), ylim = c(-6, 6),
         xlab = "state x", ylab = "state y", asp = 1)
    grid(col = plot_grid)
    gx <- seq(-5, 5, by = 2)
    field <- expand.grid(x = gx, y = gx)
    deriv <- t(A %*% t(as.matrix(field)))
    lens <- sqrt(rowSums(deriv^2))
    lens[lens == 0] <- 1
    arrows(field$x, field$y,
           field$x + deriv[, 1] / lens * .55,
           field$y + deriv[, 2] / lens * .55,
           length = .06, col = plot_muted)

    starts <- rbind(c(4, 0), c(-4, 0), c(0, 4), c(0, -4),
                    c(3, 3), c(-3, 3), c(3, -3), c(-3, -3))
    dt <- .015
    for (j in seq_len(nrow(starts))) {
      u <- starts[j, ]
      path <- matrix(u, nrow = 1)
      for (i in 1:900) {
        k1 <- as.vector(A %*% u)
        k2 <- as.vector(A %*% (u + dt * k1 / 2))
        k3 <- as.vector(A %*% (u + dt * k2 / 2))
        k4 <- as.vector(A %*% (u + dt * k3))
        u <- u + dt * (k1 + 2 * k2 + 2 * k3 + k4) / 6
        path <- rbind(path, u)
        if (any(abs(u) > 8)) break
      }
      lines(path[, 1], path[, 2], col = if (j %% 2) plot_gold else plot_ruby, lwd = 2)
    }
    points(0, 0, pch = 16, col = plot_emerald, cex = 1.2)
  }, res = 110)

  matrix_data <- reactive({
    A <- matrix(c(input$mat_a, input$mat_c, input$mat_b, input$mat_d), 2, 2)
    rhs <- c(input$mat_p, input$mat_q)
    determinant <- det(A)
    rank_a <- qr(A)$rank
    rank_aug <- qr(cbind(A, rhs))$rank
    classification <- if (rank_a == 2) "One unique solution" else
      if (rank_a == rank_aug) "Infinitely many solutions" else "No solution"
    solution <- if (rank_a == 2) solve(A, rhs) else c(NA_real_, NA_real_)
    list(A = A, rhs = rhs, determinant = determinant,
         classification = classification, solution = solution)
  })

  output$matrix_metrics <- renderUI({
    d <- matrix_data()
    solution_text <- if (all(is.finite(d$solution)))
      sprintf("x = %.2f, y = %.2f", d$solution[1], d$solution[2]) else "Not unique"
    div(class = "metric-row",
      div(class = "metric", span("det(A)"), strong(sprintf("%.2f", d$determinant))),
      div(class = "metric", span("System type"), strong(d$classification)),
      div(class = "metric", span("Solution"), strong(solution_text))
    )
  })

  output$matrix_plot <- renderPlot({
    d <- matrix_data()
    dark_plot()
    plot(0, 0, type = "n", xlim = c(-10, 10), ylim = c(-10, 10),
         xlab = "x", ylab = "y", asp = 1)
    grid(col = plot_grid)
    draw_equation <- function(coef, value, color) {
      if (abs(coef[2]) > 1e-9) {
        x <- c(-10, 10)
        lines(x, (value - coef[1] * x) / coef[2], col = color, lwd = 3)
      } else if (abs(coef[1]) > 1e-9) {
        abline(v = value / coef[1], col = color, lwd = 3)
      }
    }
    draw_equation(d$A[1, ], d$rhs[1], plot_gold)
    draw_equation(d$A[2, ], d$rhs[2], plot_ruby)
    if (all(is.finite(d$solution)) && all(abs(d$solution) <= 10))
      points(d$solution[1], d$solution[2], pch = 16, cex = 1.5, col = plot_emerald)
    legend("topright", c("Equation 1", "Equation 2"), col = c(plot_gold, plot_ruby),
           lwd = 3, bty = "n", text.col = plot_text)
  }, res = 110)

  eigen_data <- reactive({
    A <- matrix(c(input$eig_a, input$eig_b, input$eig_b, input$eig_d), 2, 2)
    eig <- eigen(A, symmetric = TRUE)
    list(A = A, values = eig$values, vectors = eig$vectors)
  })

  output$eigen_metrics <- renderUI({
    d <- eigen_data()
    dot_product <- sum(d$vectors[, 1] * d$vectors[, 2])
    div(class = "metric-row",
      div(class = "metric", span("Eigenvalue λ₁"), strong(sprintf("%.3f", d$values[1]))),
      div(class = "metric", span("Eigenvalue λ₂"), strong(sprintf("%.3f", d$values[2]))),
      div(class = "metric", span("v₁ · v₂"), strong(sprintf("%.3f (orthogonal)", dot_product)))
    )
  })

  output$eigen_plot <- renderPlot({
    d <- eigen_data()
    dark_plot()
    plot(0, 0, type = "n", xlim = c(-2, 2), ylim = c(-2, 2),
         xlab = "x₁", ylab = "x₂", asp = 1)
    grid(col = plot_grid)
    theta <- seq(0, 2 * pi, length.out = 300)
    lines(cos(theta), sin(theta), col = plot_muted)
    colors <- c(plot_gold, plot_ruby)
    for (i in 1:2) {
      v <- d$vectors[, i]
      arrows(-v[1], -v[2], v[1], v[2], length = .1, lwd = 4, col = colors[i])
      text(1.3 * v[1], 1.3 * v[2], labels = paste0("v", i), col = plot_text)
    }
    legend("topright", sprintf("λ%d = %.2f", 1:2, d$values),
           col = colors, lwd = 4, bty = "n", text.col = plot_text)
  }, res = 110)

  practice_bank <- list(
    "Laplace transforms" = list(
      Foundation = list(
        c("Find L{t²e⁻³ᵗ}.",
          "Start with the table entry L{t²}=2/s³.|Multiplication by e⁻³ᵗ shifts s to s+3.|Replace every s in 2/s³ with s+3.",
          "L{t²e⁻³ᵗ}=2/(s+3)³."),
        c("Find L⁻¹{(s+2)/((s+2)²+9)}.",
          "Recognize s/(s²+a²) as the transform of cos(at).|Here a²=9, so a=3.|The repeated s+2 shift produces multiplication by e⁻²ᵗ.",
          "f(t)=e⁻²ᵗcos(3t).")
      ),
      `Exam-style` = list(
        c("Find L⁻¹{e⁻²ˢ·s/(s²+4)}.",
          "Ignore the delay factor first: L⁻¹{s/(s²+4)}=cos(2t).|Use the second-shifting theorem for e⁻²ˢF(s).|Replace t by t−2 and multiply by u(t−2).",
          "f(t)=cos(2(t−2))u(t−2)."),
        c("Use Laplace transforms to solve y′+3y=δ(t−2), y(0)=0.",
          "Transform the derivative: L{y′}=sY−y(0)=sY.|Transform the delayed impulse: L{δ(t−2)}=e⁻²ˢ.|Solve (s+3)Y=e⁻²ˢ, so Y=e⁻²ˢ/(s+3).|Invert 1/(s+3) and apply a delay of 2.",
          "y(t)=e⁻³⁽ᵗ⁻²⁾u(t−2).")
      ),
      Challenge = list(
        c("Solve y(t)+4∫₀ᵗ(t−τ)y(τ)dτ=2t.",
          "Recognize the integral as the convolution t*y(t).|Since L{t}=1/s², its transform is Y/s².|Transform the equation: Y+4Y/s²=2/s².|Factor Y and solve: Y=2/(s²+4).|Use L{sin(at)}=a/(s²+a²).",
          "y(t)=sin(2t)."),
        c("Find L{t∫₀ᵗe⁻τsin(2τ)dτ}.",
          "Let g(t)=∫₀ᵗe⁻τsin(2τ)dτ.|L{e⁻ᵗsin(2t)}=2/((s+1)²+4), so G(s)=2/[s((s+1)²+4)].|Use L{tg(t)}=−dG/ds.|Differentiate the quotient and simplify.",
          "L{tg(t)}=2(3s²+4s+5)/[s²(s²+2s+5)²].")
      )
    ),
    "Differential equations" = list(
      Foundation = list(
        c("Solve y″+5y′+6y=0.",
          "Write the characteristic equation r²+5r+6=0.|Factor it as (r+2)(r+3)=0.|The distinct roots are −2 and −3.|Write one exponential mode for each root.",
          "y=C₁e⁻²ˣ+C₂e⁻³ˣ."),
        c("Build an ODE for y=C₁+C₂e⁻²ˣ.",
          "The constant term corresponds to the root r=0.|The e⁻²ˣ term corresponds to r=−2.|Build the characteristic polynomial r(r+2)=r²+2r.|Replace r² and r with y″ and y′.",
          "y″+2y′=0.")
      ),
      `Exam-style` = list(
        c("Solve x²y″+3xy′−3y=0.",
          "Recognize a Cauchy-Euler equation and try y=xᵐ.|Then y′=mxᵐ⁻¹ and y″=m(m−1)xᵐ⁻².|Substitute to obtain m(m−1)+3m−3=0.|Factor m²+2m−3=(m−1)(m+3).|Combine the modes for m=1 and m=−3.",
          "y=C₁x+C₂x⁻³."),
        c("Solve y″=2yy′ with y(0)=0 and y′(0)=1.",
          "Because x is missing, let u(y)=y′, so y″=u du/dy.|Substitute: u du/dy=2yu.|For this IVP u is not identically zero, so du/dy=2y.|Integrate: u=y²+C. Use y′(0)=1 and y(0)=0 to get C=1.|Now dy/dx=y²+1, so arctan(y)=x+C₂. Use y(0)=0 to get C₂=0.",
          "y(x)=tan(x), on intervals where tan(x) is defined.")
      ),
      Challenge = list(
        c("Classify the origin for x′=−x+y, y′=2x.",
          "Write A=[[-1,1],[2,0]].|Compute det(A−λI)=(-1−λ)(−λ)−2=λ²+λ−2.|Factor (λ−1)(λ+2)=0.|The eigenvalues are 1 and −2.|Opposite signs mean one mode grows and the other decays.",
          "The origin is an unstable saddle point."),
        c("Solve y″−2y′+y=eˣ.",
          "The characteristic polynomial is (r−1)², so yₕ=eˣ(C₁+C₂x).|The forcing eˣ duplicates the repeated root r=1.|Multiply the usual Aeˣ trial by x²: use yₚ=Ax²eˣ.|Equivalently set y=eˣv; then (D−1)²y=eˣv″=eˣ, so v″=1.|Integrate v″=1 to get the particular part vₚ=x²/2.",
          "y=eˣ(C₁+C₂x+x²/2).")
      )
    ),
    "Linear algebra" = list(
      Foundation = list(
        c("Find det([[3,2],[5,3]]) and decide whether it is invertible.",
          "For [[a,b],[c,d]], det(A)=ad−bc.|Compute 3·3−2·5=9−10.|A matrix is invertible exactly when its determinant is nonzero.",
          "det(A)=−1, so A is invertible."),
        c("If eigenvalues are 1, 2, and 4, find det(A) and the eigenvalues of A⁻¹.",
          "The determinant equals the product of the eigenvalues.|Multiply 1·2·4.|Eigenvalues of A⁻¹ are the reciprocals of the nonzero eigenvalues.",
          "det(A)=8 and eig(A⁻¹)={1, 1/2, 1/4}.")
      ),
      `Exam-style` = list(
        c("For A=[[1,k],[k,1]], find eigenvalues and the values of k that make A singular.",
          "Form det(A−λI)=(1−λ)²−k².|Factor as (1−λ−k)(1−λ+k).|Set each factor to zero to get λ₁=1+k and λ₂=1−k.|A is singular when at least one eigenvalue is zero.",
          "Eigenvalues are 1+k and 1−k; A is singular for k=±1."),
        c("Classify kx+y=1 and 2x+2y=2 for all k.",
          "The coefficient determinant is 2k−2=2(k−1).|If k≠1, the determinant is nonzero, so the system has one solution.|If k=1, the second equation is exactly twice the first.|The two equations then describe the same line.",
          "One unique solution for k≠1; infinitely many solutions for k=1; no value of k gives no solution.")
      ),
      Challenge = list(
        c("For A=[[5,4],[4,5]], compute the principal square root A¹ᐟ².",
          "Because A is symmetric, use orthonormal eigenvectors.|The eigenvectors [1,1] and [1,−1] give eigenvalues 9 and 1.|Write A=P diag(9,1) Pᵀ.|Take square roots on the diagonal: A¹ᐟ²=P diag(3,1) Pᵀ.|Multiply the matrices.",
          "A¹ᐟ²=[[2,1],[1,2]]."),
        c("Simplify det(A³B⁻¹AᵀB²).",
          "Use det of a product as the product of determinants.|det(A³)=det(A)³.|det(B⁻¹)=1/det(B).|det(Aᵀ)=det(A) and det(B²)=det(B)².|Combine powers of each determinant.",
          "det(A³B⁻¹AᵀB²)=det(A)⁴det(B).")
      )
    )
  )

  # Textbook-style versions of every generated practice prompt and final result.
  # The explanatory steps stay conversational, while the mathematics is rendered
  # consistently by MathJax rather than by improvised Unicode superscripts.
  practice_math <- list(
    "Laplace transforms" = list(
      Foundation = list(
        c("\\text{Find }\\mathcal{L}\\{t^2e^{-3t}\\}.",
          "\\mathcal{L}\\{t^2e^{-3t}\\}=\\frac{2}{(s+3)^3}"),
        c("\\text{Find }\\mathcal{L}^{-1}\\!\\left\\{\\frac{s+2}{(s+2)^2+9}\\right\\}.",
          "f(t)=e^{-2t}\\cos(3t)")
      ),
      `Exam-style` = list(
        c("\\text{Find }\\mathcal{L}^{-1}\\!\\left\\{e^{-2s}\\frac{s}{s^2+4}\\right\\}.",
          "f(t)=\\cos\\!\\bigl(2(t-2)\\bigr)u(t-2)"),
        c("\\text{Solve }y'+3y=\\delta(t-2),\\qquad y(0)=0.",
          "y(t)=e^{-3(t-2)}u(t-2)")
      ),
      Challenge = list(
        c("y(t)+4\\int_0^t(t-\\tau)y(\\tau)\\,d\\tau=2t",
          "y(t)=\\sin(2t)"),
        c("\\text{Find }\\mathcal{L}\\!\\left\\{t\\int_0^t e^{-\\tau}\\sin(2\\tau)\\,d\\tau\\right\\}.",
          "\\mathcal{L}\\{tg(t)\\}=\\frac{2(3s^2+4s+5)}{s^2(s^2+2s+5)^2}")
      )
    ),
    "Differential equations" = list(
      Foundation = list(
        c("y''+5y'+6y=0", "y=C_1e^{-2x}+C_2e^{-3x}"),
        c("\\text{Build an ODE for }y=C_1+C_2e^{-2x}.", "y''+2y'=0")
      ),
      `Exam-style` = list(
        c("x^2y''+3xy'-3y=0", "y=C_1x+C_2x^{-3}"),
        c("y''=2yy',\\qquad y(0)=0,\\quad y'(0)=1", "y(x)=\\tan x")
      ),
      Challenge = list(
        c("x'=-x+y,\\qquad y'=2x", "\\text{The origin is an unstable saddle point.}"),
        c("y''-2y'+y=e^x", "y=e^x\\left(C_1+C_2x+\\frac{x^2}{2}\\right)")
      )
    ),
    "Linear algebra" = list(
      Foundation = list(
        c("A=\\begin{bmatrix}3&2\\\\5&3\\end{bmatrix},\\qquad \\text{find }\\det(A).",
          "\\det(A)=-1\\ne0\\quad\\Longrightarrow\\quad A\\text{ is invertible}"),
        c("\\lambda(A)=\\{1,2,4\\},\\qquad \\text{find }\\det(A)\\text{ and }\\lambda(A^{-1}).",
          "\\det(A)=8,\\qquad \\lambda(A^{-1})=\\left\\{1,\\frac12,\\frac14\\right\\}")
      ),
      `Exam-style` = list(
        c("A=\\begin{bmatrix}1&k\\\\k&1\\end{bmatrix}",
          "\\lambda_1=1+k,\\quad\\lambda_2=1-k;\\qquad A\\text{ is singular when }k=\\pm1"),
        c("kx+y=1,\\qquad 2x+2y=2",
          "\\begin{cases}k\\ne1:&\\text{one solution},\\\\k=1:&\\text{infinitely many solutions.}\\end{cases}")
      ),
      Challenge = list(
        c("A=\\begin{bmatrix}5&4\\\\4&5\\end{bmatrix},\\qquad \\text{find }A^{1/2}.",
          "A^{1/2}=\\begin{bmatrix}2&1\\\\1&2\\end{bmatrix}"),
        c("\\text{Simplify }\\det\\!\\left(A^3B^{-1}A^TB^2\\right).",
          "\\det\\!\\left(A^3B^{-1}A^TB^2\\right)=\\det(A)^4\\det(B)")
      )
    )
  )

  selected_practice <- reactive({
    topic <- input$practice_topic
    level <- input$practice_level
    bank <- practice_bank[[topic]][[level]]
    index <- (input$new_practice %% length(bank)) + 1
    bank[[index]]
  })

  output$practice_prompt <- renderUI({
    topic <- input$practice_topic
    level <- input$practice_level
    bank <- practice_math[[topic]][[level]]
    index <- (input$new_practice %% length(bank)) + 1
    math_block(bank[[index]][1], "Practice problem.")
  })

  output$practice_solution <- renderUI({
    item <- selected_practice()
    steps <- strsplit(item[2], "|", fixed = TRUE)[[1]]
    topic <- input$practice_topic
    level <- input$practice_level
    math_bank <- practice_math[[topic]][[level]]
    index <- (input$new_practice %% length(math_bank)) + 1
    tagList(
      div(class = "eyebrow", "Solve this problem step by step"),
      tags$ol(class = "learning-path", lapply(steps, tags$li)),
      tags$h4("Final answer"),
      math_block(math_bank[[index]][2], "Final answer.")
    )
  })

  output$mock_exam_questions <- renderUI({
    exam <- mock_exam_bank[[input$mock_exam_version]]
    tagList(
      div(
        class = "mock-exam-banner",
        div(
          div(class = "eyebrow", paste(input$mock_exam_version, "·", exam$subtitle)),
          h3("Five questions · 100 marks"),
          tags$p(exam$emphasis)
        ),
        div(
          class = "mock-exam-balance",
          span("Differential equations"),
          span("Laplace transforms"),
          span("Linear algebra")
        )
      ),
      lapply(
        seq_along(exam$questions),
        function(index) mock_exam_question_ui(exam$questions[[index]], index)
      )
    )
  })
}

shinyApp(ui, server)
