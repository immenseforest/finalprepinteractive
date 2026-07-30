# ENGI-3022 final exam analysis (2020-2025)

Six supplied final exams were reviewed. The recurring curriculum has three
main domains.

## Paper structure audit

| Paper | Major structure | Total marks | Weighting signal |
| --- | --- | ---: | --- |
| 2020 | 4 major problems | 52 | Laplace 24; linear algebra 28 |
| 2021 | 3 major problems | 51 | Laplace/differential systems 21; linear algebra 30 |
| 2022 | 4 major problems | 49 | differential equations 6; Laplace/systems 22; linear algebra 21 |
| 2023 | 4 major problems | 50 | Laplace/systems 27; linear algebra 23 |
| 2024 | 3 named topic sections | 50 | differential equations 8; Laplace 16; linear algebra 26 |
| 2025 | 3 named topic sections | 50 | differential equations 11; Laplace 14; linear algebra 25 |

The papers consistently use uneven marks, dense multi-part problem groups,
and a large linear-algebra block. The 2024 and 2025 papers provide the clearest
current section labels and are therefore used as the two mock-exam templates.

## Question size and demand audit

A linear-algebra "question group" means one numbered prompt, including all of
its subparts. Across 38 groups:

| Final | 2 x 2 focus | 3 x 3 focus | Mixed/abstract | 4 x 4 or larger focus | Total |
| --- | ---: | ---: | ---: | ---: | ---: |
| 2020 | 3 | 9 | 0 | 0 | 12 |
| 2021 | 2 | 5 | 0 | 0 | 7 |
| 2022 | 1 | 4 | 0 | 0 | 5 |
| 2023 | 1 | 4 | 0 | 0 | 5 |
| 2024 | 0 | 4 | 2 | 0 | 6 |
| 2025 | 1 | 2 | 0 | 0 | 3 |
| **Total** | **8** | **28** | **2** | **0** | **38** |

The 2024 mixed groups include a rectangular matrix-product problem and an
abstract determinant-identity problem. No supplied final contains a question
focused on dense 4 x 4, 5 x 5, or 6 x 6 hand computation. A future 6 x 6
question is still possible unless official course guidance rules it out, so the
recommended boundary is to learn scalable procedures and recognize structured
large matrices without spending substantial time on dense large-matrix drills.

For the 21 linear-algebra groups whose individual marks are printed (2021,
2022, 2024, and 2025), the marks-based demand profile is:

- 10 short groups worth 2-3 marks
- 5 medium groups worth 4-6 marks
- 6 long, integrated groups worth 7-10 marks

Marks are a proxy for task breadth and duration, not a pure measure of
conceptual difficulty. The 2020 and 2023 papers are excluded from this demand
count because they print section totals rather than marks for each group.

## Efficiency guidance

The combined 2024-2025 topic weights are linear algebra 51%, Laplace
transforms 30%, and differential equations 19%. The roadmap uses a rounded
50/30/20 study-time split and lets the learner scale it to the hours available.

High-value recurring methods include:

- Eigenvalues/eigenvectors, determinant/inverse/singularity, and
  parameter-dependent matrices: 6 of 6 papers
- Full diagonalization, matrix powers, or matrix roots: 5 of 6 papers
- Direct/inverse Laplace transforms, delays, convolution/integral equations,
  and Laplace-based differential equations: 6 of 6 papers
- Dirac impulses: 4 of 6 papers
- Coupled first-order systems: 3 of 6 papers
- Standalone nonlinear ODEs: 3 of 6 papers

Low-evidence extensions include dense 4 x 4-or-larger matrix computation,
fourth-order-or-higher standalone ODEs, PDEs, and numerical methods: none
appears in the supplied sample. These are not impossible; they should receive
extra study time only when the course outline or instructor guidance supports
them.

## 1. Linear algebra

This is usually the largest section by mark weight.

- Linear systems: unique, inconsistent, and infinitely many solutions
- Matrix arithmetic and inverse-based solution methods
- Determinants, singularity, and invertibility
- Determinant identities involving products, powers, inverses, and transposes
- Eigenvalues and eigenvectors, including normalization
- Orthogonality for symmetric matrices
- Diagonalization
- Matrix powers, inverses, and square/cube roots through diagonalization

## 2. Laplace transforms

This appears in every supplied exam.

- Direct and inverse transforms
- Frequency and time shifting
- Unit-step functions and delayed signals
- Dirac impulses
- Differentiation, multiplication by time, and scaling properties
- Convolution
- Integral equations
- Initial-value problems and coupled systems

## 3. Differential equations

Standalone coverage becomes more explicit in the later papers.

- Constant-coefficient linear equations
- Characteristic roots and solution forms
- Constructing an ODE from a supplied general solution
- Cauchy-Euler equations
- Reducible second-order nonlinear equations
- Coupled first-order systems
- Initial-value problems

## App structure

The app maps these findings into:

- Existing Laplace Transform Lab
- Differential Equations overview, characteristic-root lab, and phase lab
- Linear Algebra overview, system lab, and eigenvalue lab
- Exam Coach with a six-year coverage table, mastery checklist, and tiered
  practice generator
- Question-level matrix-size counts, marks-based demand statistics, method
  frequencies, recent-exam time allocation, and evidence-based stopping rules
- Five 50-mark mock finals: Versions 1-2 use the 2024 topic split (8/16/26)
  and Versions 3-5 use the 2025 topic split (11/14/25)

The practice prompts are newly written examples modeled on the recurring
skills and grouped like recent multi-part exam problems. They do not reproduce
the supplied final exams verbatim or predict the next official exam.

## Answer verification

All 25 mock-exam answers were independently recalculated after the paper
structure audit. The automated checks cover initial conditions, transform
identities, delayed and impulse terms, parameter cases, determinant laws,
eigenpairs, matrix powers, inverses, and principal square roots. Every hidden
solution now includes both a method checklist and displayed algebra showing how
the verified final answer is obtained.
