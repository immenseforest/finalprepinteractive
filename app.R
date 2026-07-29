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

ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      :root { color-scheme: dark; --bg:#080b12; --panel:#111723; --panel2:#151d2b;
        --text:#eef2ff; --muted:#94a3b8; --cyan:#38bdf8; --violet:#a78bfa;
        --green:#34d399; --border:#263246; }
      * { box-sizing:border-box; }
      body { background:radial-gradient(circle at 20% 0%,#111d35 0,#080b12 38%);
        color:var(--text); font-family:Inter,system-ui,-apple-system,sans-serif; }
      .container-fluid { padding:0; }
      .app-shell { min-height:100vh; }
      .hero { padding:34px max(24px,calc((100vw - 1240px)/2)); border-bottom:1px solid var(--border); }
      .eyebrow { color:var(--cyan); text-transform:uppercase; letter-spacing:.16em;
        font-weight:700; font-size:12px; }
      h1 { margin:8px 0 7px; font-size:clamp(29px,4vw,48px); font-weight:750; letter-spacing:-.04em; }
      .subtitle { color:var(--muted); max-width:700px; font-size:16px; }
      .content { max-width:1240px; margin:auto; padding:24px; }
      .nav-tabs { border:0; margin-bottom:22px; display:flex; gap:8px; }
      .nav-tabs>li>a { color:var(--muted); border:1px solid transparent!important;
        border-radius:10px; background:transparent; }
      .nav-tabs>li.active>a,.nav-tabs>li.active>a:hover,.nav-tabs>li>a:hover {
        color:var(--text); background:var(--panel2); border-color:var(--border)!important; }
      .nav-pills { display:flex; flex-wrap:wrap; gap:8px; margin-bottom:20px; }
      .nav-pills>li>a { color:var(--muted); border:1px solid var(--border); border-radius:10px; }
      .nav-pills>li.active>a,.nav-pills>li.active>a:hover,.nav-pills>li>a:hover {
        color:var(--text); background:#1d2b41; }
      .card { background:linear-gradient(145deg,rgba(21,29,43,.96),rgba(13,18,29,.96));
        border:1px solid var(--border); border-radius:16px; padding:20px; margin-bottom:20px;
        box-shadow:0 14px 35px rgba(0,0,0,.2); }
      .card h3 { margin:0 0 14px; font-size:17px; }
      .control-label { color:#cbd5e1; font-size:13px; margin-bottom:7px; }
      .form-control, .selectize-input, .selectize-control.single .selectize-input.input-active {
        background:#0b101a!important; color:var(--text)!important; border:1px solid var(--border)!important;
        border-radius:9px; box-shadow:none!important; }
      .selectize-dropdown { background:#0b101a; color:var(--text); border-color:var(--border); }
      .selectize-dropdown .active { background:#1d2b41; color:white; }
      .irs--shiny .irs-bar { background:linear-gradient(90deg,var(--cyan),var(--violet)); border:0; }
      .irs--shiny .irs-handle { background:var(--text); border:2px solid var(--cyan); }
      .irs--shiny .irs-line { background:#263246; border:0; }
      .irs--shiny .irs-single { background:var(--cyan); color:#041018; }
      .formula { padding:18px 20px; border-left:3px solid var(--violet); background:#0b101a;
        border-radius:0 11px 11px 0; font-family:Cambria Math,serif; font-size:18px;
        overflow-wrap:anywhere; }
      .formula + .formula { margin-top:10px; border-color:var(--cyan); }
      .plot-wrap { min-height:330px; }
      .hint { color:var(--muted); font-size:13px; line-height:1.6; }
      .concept { display:grid; grid-template-columns:repeat(3,1fr); gap:16px; }
      .concept .card { margin:0; }
      .concept strong { color:var(--cyan); display:block; margin-bottom:7px; }
      .learning-path { margin:0; padding-left:22px; color:#dbeafe; }
      .learning-path li { padding:6px 0 6px 5px; }
      .learning-path li::marker { color:var(--violet); font-weight:700; }
      .metric-row { display:grid; grid-template-columns:repeat(3,1fr); gap:12px; margin-bottom:20px; }
      .metric { background:#0b101a; border:1px solid var(--border); border-radius:12px; padding:14px; }
      .metric span { display:block; color:var(--muted); font-size:12px; margin-bottom:5px; }
      .metric strong { color:var(--text); font-size:18px; font-variant-numeric:tabular-nums; }
      .preset-actions { display:flex; flex-wrap:wrap; gap:8px; margin-bottom:18px; }
      .preset-actions .btn { color:var(--text); background:#172033; border:1px solid var(--border);
        border-radius:9px; }
      .preset-actions .btn:hover,.preset-actions .btn:focus { color:white; background:#22304a;
        border-color:var(--cyan); }
      .source-link { color:var(--cyan); }
      .help-tip { position:relative; display:inline-flex; align-items:center; justify-content:center;
        width:18px; height:18px; margin-left:6px; border:1px solid var(--cyan);
        border-radius:50%; color:var(--cyan); font-family:system-ui,sans-serif; font-size:12px;
        font-weight:700; cursor:help; vertical-align:2px; outline-offset:3px; }
      .help-tip::after { content:attr(data-tip); position:absolute; z-index:100; left:50%; bottom:calc(100% + 10px);
        width:min(280px,75vw); padding:11px 13px; border:1px solid var(--border); border-radius:10px;
        background:#070b12; color:var(--text); font-size:13px; font-weight:400; line-height:1.45;
        font-family:Inter,system-ui,sans-serif; text-align:left; box-shadow:0 12px 30px rgba(0,0,0,.45);
        opacity:0; visibility:hidden; pointer-events:none; transform:translate(-50%,5px);
        transition:opacity .16s ease,transform .16s ease,visibility .16s; }
      .help-tip:hover::after,.help-tip:focus::after { opacity:1; visibility:visible; transform:translate(-50%,0); }
      .math-formula { overflow-x:auto; overflow-y:hidden; }
      .math-formula .MathJax_Display { margin:0!important; text-align:left!important; }
      .math-inline { white-space:nowrap; }
      table { width:100%; color:#dbeafe; }
      th { color:var(--muted); font-weight:600; text-align:left; }
      th,td { padding:12px 10px; border-bottom:1px solid var(--border); }
      code { background:#0a0f18; color:#c4b5fd; padding:3px 6px; }
      @media(max-width:800px){ .concept{grid-template-columns:1fr;} .hero{padding:26px 20px;}
        .content{padding:16px;} .metric-row{grid-template-columns:1fr;} }
    "))
  ),
  div(class = "app-shell",
      div(class = "hero",
          div(class = "eyebrow", "Interactive mathematics"),
          h1("Engineering Analysis B Learning Lab"),
          div(class = "subtitle",
              "Build exam-ready intuition for differential equations, Laplace transforms, and linear algebra.")
      ),
      div(class = "content",
          tabsetPanel(type = "tabs",
            tabPanel("About", value = "app_about",
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
                      tags$li("Use Exam Coach to rotate between topics and reveal guidance only after attempting a problem.")
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
            tabPanel("Laplace Transform", value = "laplace",
              tabsetPanel(type = "pills",
            tabPanel("About", value = "laplace_about",
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
                  strong("Transform Explorer"),
                  span("Choose an exponential, sine, cosine, polynomial, or damped sine. Change its parameters and compare f(t) with F(s).")
                ),
                div(class = "card",
                  strong("Property Lab"),
                  span("Delay a signal and observe how time shifting becomes multiplication by e^(−as) in the s-domain.")
                ),
                div(class = "card",
                  strong("Quick Reference"),
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
                      tags$li("Use Property Lab to understand time shifting."),
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
            tabPanel("Transform Explorer", value = "explorer",
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
            tabPanel("Property Lab", value = "properties",
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
            tabPanel("Engineering Example", value = "engineering",
              tabsetPanel(type = "pills",
                tabPanel("Mechanical · Suspension", value = "mechanical",
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
                tabPanel("Chemical · Mixing tank", value = "chemical",
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
                tabPanel("Civil · Building vibration", value = "civil",
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
            tabPanel("Differential Equations", value = "differential",
              tabsetPanel(type = "pills",
                tabPanel("Overview",
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
                tabPanel("Linear ODE lab",
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
                tabPanel("System phase lab",
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
                tabPanel("Engineering Example",
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
            tabPanel("Linear Algebra", value = "linear_algebra",
              tabsetPanel(type = "pills",
                tabPanel("Overview",
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
                tabPanel("Matrix and system lab",
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
                tabPanel("Eigenvalue lab",
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
                tabPanel("Engineering Example",
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
            tabPanel("Exam Coach", value = "exam_coach",
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
            tabPanel("Quick Reference", value = "reference",
              div(class = "card",
                div(class = "eyebrow", "Course formula sheet"),
                h2("Quick Reference"),
                tags$p("Use these formulas to identify a method, then verify the conditions before applying it.")
              ),
              tabsetPanel(type = "pills",
                tabPanel("Laplace transforms",
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
                tabPanel("Differential equations",
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
                tabPanel("Linear algebra",
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
                tabPanel("Method selector",
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
            )
          )
      )
  )
)

server <- function(input, output, session) {
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
    par(bg = "#111723", fg = "#94a3b8", col.axis = "#94a3b8", col.lab = "#cbd5e1",
        mar = c(4.2, 4.4, 1, 1), family = "sans")
  }

  output$time_plot <- renderPlot({
    t <- seq(0, input$tmax, length.out = 700)
    y <- current()$time(t, params())
    dark_plot()
    plot(t, y, type = "n", xlab = "time  t", ylab = "amplitude")
    grid(col = "#263246", lty = 1)
    lines(t, y, col = "#38bdf8", lwd = 3)
    abline(h = 0, col = "#64748b", lwd = 1)
  }, res = 110)

  output$laplace_plot <- renderPlot({
    s <- seq(.08, 8, length.out = 700)
    y <- Re(current()$transform(s, params()))
    cap <- quantile(abs(y[is.finite(y)]), .94)
    y <- pmax(pmin(y, cap), -cap)
    dark_plot()
    plot(s, y, type = "n", xlab = "real frequency  s", ylab = "F(s)")
    grid(col = "#263246", lty = 1)
    lines(s, y, col = "#a78bfa", lwd = 3)
    abline(h = 0, col = "#64748b", lwd = 1)
  }, res = 110)

  output$shift_plot <- renderPlot({
    t <- seq(0, 12, length.out = 800)
    original <- exp(-input$decay * t)
    shifted <- ifelse(t >= input$shift, exp(-input$decay * (t - input$shift)), 0)
    dark_plot()
    plot(t, original, type = "n", ylim = c(0, 1.08), xlab = "time  t", ylab = "amplitude")
    grid(col = "#263246", lty = 1)
    lines(t, original, col = "#38bdf8", lwd = 3)
    lines(t, shifted, col = "#34d399", lwd = 3)
    abline(v = input$shift, col = "#64748b", lty = 3)
    legend("topright", c("f(t)", "u(t−a)f(t−a)"), col = c("#38bdf8", "#34d399"),
           lwd = 3, bty = "n", text.col = "#cbd5e1")
  }, res = 110)

  output$shift_laplace_plot <- renderPlot({
    s <- seq(.05, 6, length.out = 700)
    original <- 1 / (s + input$decay)
    shifted <- exp(-input$shift * s) / (s + input$decay)
    dark_plot()
    plot(s, original, type = "n", xlab = "real frequency  s", ylab = "magnitude")
    grid(col = "#263246", lty = 1)
    lines(s, original, col = "#38bdf8", lwd = 3)
    lines(s, shifted, col = "#34d399", lwd = 3)
    legend("topright", c("F(s)", "e^(−as)F(s)"), col = c("#38bdf8", "#34d399"),
           lwd = 3, bty = "n", text.col = "#cbd5e1")
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
    grid(col = "#263246", lty = 1)
    polygon(c(d$t, rev(d$t)), c(d$x * 1000, rep(0, length(d$t))),
            col = "#38bdf81f", border = NA)
    lines(d$t, d$x * 1000, col = "#38bdf8", lwd = 3)
    abline(h = d$xeq * 1000, col = "#a78bfa", lty = 2, lwd = 2)
    legend("topright", c("Body response", "Final position"), col = c("#38bdf8", "#a78bfa"),
           lwd = c(3, 2), lty = c(1, 2), bty = "n", text.col = "#cbd5e1")
  }, res = 110)

  output$frequency_plot <- renderPlot({
    w <- 10^seq(-1, 2, length.out = 800)
    magnitude <- 1 / sqrt((input$stiffness - input$mass * w^2)^2 +
                            (input$damping * w)^2)
    dark_plot()
    plot(w, magnitude * 1e6, type = "n", log = "x",
         xlab = "angular frequency  ω (rad/s)", ylab = "|H(jω)|  (mm/kN)")
    grid(col = "#263246", lty = 1)
    lines(w, magnitude * 1e6, col = "#34d399", lwd = 3)
    abline(v = suspension_data()$wn, col = "#64748b", lty = 3)
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
    grid(col = "#263246", lty = 1)
    polygon(c(d$t, rev(d$t)),
            c(d$concentration, rep(min(y_range) - padding, length(d$t))),
            col = "#34d3991f", border = NA)
    lines(d$t, d$concentration, col = "#34d399", lwd = 3)
    abline(h = input$inlet_conc, col = "#a78bfa", lty = 2, lwd = 2)
    abline(v = d$tau, col = "#64748b", lty = 3)
    legend("bottomright", c("Tank concentration", "New inlet value", "One time constant"),
           col = c("#34d399", "#a78bfa", "#64748b"), lwd = c(3, 2, 1),
           lty = c(1, 2, 3), bty = "n", text.col = "#cbd5e1")
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
    grid(col = "#263246", lty = 1)
    polygon(c(d$t, rev(d$t)), c(d$x * 1000, rep(0, length(d$t))),
            col = "#a78bfa1f", border = NA)
    lines(d$t, d$x * 1000, col = "#a78bfa", lwd = 3)
    abline(h = d$xeq * 1000, col = "#38bdf8", lty = 2, lwd = 2)
    legend("topright", c("Building response", "Static wind position"),
           col = c("#a78bfa", "#38bdf8"), lwd = c(3, 2), lty = c(1, 2),
           bty = "n", text.col = "#cbd5e1")
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
    grid(col = "#263246")
    polygon(c(d$t, rev(d$t)),
            c(d$temperature, rep(y_range[1] - padding, length(d$t))),
            col = "#38bdf81f", border = NA)
    lines(d$t, d$temperature, col = "#38bdf8", lwd = 3)
    abline(h = d$steady, col = "#a78bfa", lty = 2, lwd = 2)
    abline(v = d$tau, col = "#64748b", lty = 3)
    legend("bottomright", c("Equipment temperature", "Steady temperature", "One time constant"),
           col = c("#38bdf8", "#a78bfa", "#64748b"), lwd = c(3, 2, 1),
           lty = c(1, 2, 3), bty = "n", text.col = "#cbd5e1")
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
    grid(col = "#263246")
    left_end <- c(-4 * cos(d$left), 4 * sin(d$left))
    right_end <- c(4 * cos(d$right), 4 * sin(d$right))
    lines(c(0, left_end[1]), c(0, left_end[2]), col = "#38bdf8", lwd = 8)
    lines(c(0, right_end[1]), c(0, right_end[2]), col = "#a78bfa", lwd = 8)
    points(c(left_end[1], right_end[1]), c(left_end[2], right_end[2]),
           pch = 24, bg = "#64748b", col = "#cbd5e1", cex = 1.4)
    points(0, 0, pch = 21, bg = "#34d399", col = "#eef2ff", cex = 1.8)

    load_scale <- 2 / max(10, sqrt(d$px^2 + d$py^2))
    arrows(0, 0, d$px * load_scale, -d$py * load_scale,
           length = .12, lwd = 3, col = "#f97316")
    text(d$px * load_scale, -d$py * load_scale - .25, "Applied load", col = "#eef2ff")
    if (all(is.finite(d$forces))) {
      text(left_end[1] / 2, left_end[2] / 2 + .25,
           sprintf("%.1f kN", d$forces[1]), col = "#eef2ff")
      text(right_end[1] / 2, right_end[2] / 2 + .25,
           sprintf("%.1f kN", d$forces[2]), col = "#eef2ff")
    }
    legend("topright", c("Left member", "Right member", "External load"),
           col = c("#38bdf8", "#a78bfa", "#f97316"), lwd = c(8, 8, 3),
           bty = "n", text.col = "#cbd5e1")
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
    grid(col = "#263246")
    lines(d$t, y, col = "#38bdf8", lwd = 3)
    abline(h = 0, col = "#64748b")
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
    grid(col = "#263246")
    gx <- seq(-5, 5, by = 2)
    field <- expand.grid(x = gx, y = gx)
    deriv <- t(A %*% t(as.matrix(field)))
    lens <- sqrt(rowSums(deriv^2))
    lens[lens == 0] <- 1
    arrows(field$x, field$y,
           field$x + deriv[, 1] / lens * .55,
           field$y + deriv[, 2] / lens * .55,
           length = .06, col = "#64748b")

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
      lines(path[, 1], path[, 2], col = if (j %% 2) "#38bdf8" else "#a78bfa", lwd = 2)
    }
    points(0, 0, pch = 16, col = "#34d399", cex = 1.2)
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
    grid(col = "#263246")
    draw_equation <- function(coef, value, color) {
      if (abs(coef[2]) > 1e-9) {
        x <- c(-10, 10)
        lines(x, (value - coef[1] * x) / coef[2], col = color, lwd = 3)
      } else if (abs(coef[1]) > 1e-9) {
        abline(v = value / coef[1], col = color, lwd = 3)
      }
    }
    draw_equation(d$A[1, ], d$rhs[1], "#38bdf8")
    draw_equation(d$A[2, ], d$rhs[2], "#a78bfa")
    if (all(is.finite(d$solution)) && all(abs(d$solution) <= 10))
      points(d$solution[1], d$solution[2], pch = 16, cex = 1.5, col = "#34d399")
    legend("topright", c("Equation 1", "Equation 2"), col = c("#38bdf8", "#a78bfa"),
           lwd = 3, bty = "n", text.col = "#cbd5e1")
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
    grid(col = "#263246")
    theta <- seq(0, 2 * pi, length.out = 300)
    lines(cos(theta), sin(theta), col = "#64748b")
    colors <- c("#38bdf8", "#a78bfa")
    for (i in 1:2) {
      v <- d$vectors[, i]
      arrows(-v[1], -v[2], v[1], v[2], length = .1, lwd = 4, col = colors[i])
      text(1.3 * v[1], 1.3 * v[2], labels = paste0("v", i), col = "#eef2ff")
    }
    legend("topright", sprintf("λ%d = %.2f", 1:2, d$values),
           col = colors, lwd = 4, bty = "n", text.col = "#cbd5e1")
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
}

shinyApp(ui, server)
