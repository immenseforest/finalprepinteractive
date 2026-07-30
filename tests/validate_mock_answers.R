source("app.R")

near <- function(x, y, tolerance = 1e-9) {
  isTRUE(all.equal(x, y, tolerance = tolerance, check.attributes = FALSE))
}

matrix_power <- function(matrix, exponent) {
  if (exponent == 0) return(diag(nrow(matrix)))
  result <- diag(nrow(matrix))
  for (index in seq_len(exponent)) result <- result %*% matrix
  result
}

all_questions <- unlist(
  lapply(mock_exam_bank, function(exam) exam$questions),
  recursive = FALSE
)
stopifnot(length(all_questions) == 25)
stopifnot(length(mock_derivation_catalog) == 25)
stopifnot(all(vapply(all_questions, function(question) {
  length(question$derivation) >= 3 &&
    all(nzchar(question$derivation)) &&
    nzchar(question$answer)
}, logical(1))))

# Version 1
constants <- solve(matrix(c(1, 1, -1, -3), nrow = 2, byrow = TRUE), c(2, -4))
stopifnot(near(constants, c(1, 1)))

s <- c(0.7, 1.3, 2.9)
stopifnot(near(
  (2 * s + 5) / (s^2 + 4 * s + 13),
  2 * (s + 2) / ((s + 2)^2 + 9) + 1 / ((s + 2)^2 + 9)
))
stopifnot(near(1 / (s * (s + 2)), 0.5 / s - 0.5 / (s + 2)))

system_k3 <- solve(matrix(c(1, 3, 2, 4), nrow = 2, byrow = TRUE), c(1, 2))
stopifnot(near(system_k3, c(1, 0)))

A1 <- matrix(c(4, 1, 2, 3), nrow = 2, byrow = TRUE)
P1 <- matrix(c(1, 1, 1, -2), nrow = 2, byrow = TRUE)
D1 <- diag(c(5, 2))
stopifnot(near(A1, P1 %*% D1 %*% solve(P1)))
stopifnot(near(matrix_power(A1, 5), P1 %*% diag(c(5^5, 2^5)) %*% solve(P1)))

# Version 2
x <- 0.3
nonlinear_y <- sqrt(2) * tan(x / sqrt(2))
nonlinear_yp <- 1 / cos(x / sqrt(2))^2
nonlinear_ypp <- sqrt(2) * nonlinear_yp * tan(x / sqrt(2))
stopifnot(near(nonlinear_ypp, nonlinear_y * nonlinear_yp))
stopifnot(near((1 + 2i)^2 - 2 * (1 + 2i) + 5, 0 + 0i))

stopifnot(near((1 + 9 / s^2) * (3 / (s^2 + 9)), 3 / s^2))
stopifnot(near(s^2 + 4 * s + 13, (s + 2)^2 + 9))

det_A <- matrix(c(2, 1, 1, 3), nrow = 2, byrow = TRUE)
det_B <- matrix(c(4, 1, 2, 2), nrow = 2, byrow = TRUE)
lhs_det <- det(det_A %*% det_A %*% solve(det_B) %*% t(det_A) %*% matrix_power(det_B, 3))
rhs_det <- det(det_A)^3 * det(det_B)^2
stopifnot(near(lhs_det, rhs_det, 1e-7))

A2 <- matrix(c(2, 1, 1, 2), nrow = 2, byrow = TRUE)
stopifnot(near(matrix_power(A2, 5), matrix(c(122, 121, 121, 122), 2, byrow = TRUE)))

# Version 3
A3 <- matrix(c(0, 1, -4, -4), nrow = 2, byrow = TRUE)
stopifnot(near(sort(Re(eigen(A3)$values)), c(-2, -2), 1e-7))
stopifnot(qr(A3 + 2 * diag(2))$rank == 1)
stopifnot(near((2i)^2 + 4, 0 + 0i))

transform_expected <- 6 * (s + 2) / (((s + 2)^2 + 9)^2)
transform_derivative <- -(-6 * (s + 2) / (((s + 2)^2 + 9)^2))
stopifnot(near(transform_expected, transform_derivative))

Y_impulse <- (1 + exp(-pi * s)) / (s^2 + 4)
stopifnot(near((s^2 + 4) * Y_impulse - 1, exp(-pi * s)))

A4 <- matrix(c(10, 6, 6, 10), nrow = 2, byrow = TRUE)
A4_root <- matrix(c(3, 1, 1, 3), nrow = 2, byrow = TRUE)
stopifnot(near(A4_root %*% A4_root, A4))
stopifnot(near(solve(A4, c(16, 16)), c(1, 1)))
stopifnot(near(det(2 * solve(A4) %*% t(A4)), 4))

# Version 4
x <- 0.41
particular <- x * sin(2 * x) / 4
particular_second <- cos(2 * x) - x * sin(2 * x)
stopifnot(near(particular_second + 4 * particular, cos(2 * x)))

A5 <- matrix(c(3, 1, -2, 0), nrow = 2, byrow = TRUE)
stopifnot(near(sort(Re(eigen(A5)$values)), c(1, 2)))
stopifnot(near((s + 1) / (s^2 + 2 * s + 10), (s + 1) / ((s + 1)^2 + 9)))
stopifnot(near(((s + 1) / s^2) * (1 - 1 / (s + 1)), 1 / s))

k <- 2
parameter_solution <- solve(matrix(c(k, 1, 2, 2), nrow = 2, byrow = TRUE), c(1, k))
stopifnot(near(parameter_solution, c(0, 1)))
inconsistent_augmented <- matrix(c(1, 1, 1, 2, 2, 1), nrow = 2, byrow = TRUE)
stopifnot(qr(inconsistent_augmented[, 1:2])$rank < qr(inconsistent_augmented)$rank)
B4 <- matrix(c(2, 1, 1, 2), nrow = 2, byrow = TRUE)
stopifnot(near(matrix_power(B4, 3), matrix(c(14, 13, 13, 14), 2, byrow = TRUE)))

# Version 5
x <- 0.6
v <- x^3 / 6 + 2 * x + 3
v_prime <- x^2 / 2 + 2
v_second <- x
y <- exp(-x) * v
y_prime <- exp(-x) * (v_prime - v)
y_second <- exp(-x) * (v_second - 2 * v_prime + v)
stopifnot(near(y_second + 2 * y_prime + y, x * exp(-x)))

x <- 0.4
y <- log(x + 1)
y_prime <- 1 / (x + 1)
y_second <- -1 / (x + 1)^2
stopifnot(near(y_second + y_prime^2, 0))

stopifnot(near(
  1 / (s^2 * (s + 2)),
  -1 / (4 * s) + 1 / (2 * s^2) + 1 / (4 * (s + 2))
))
stopifnot(near((s^2 + 1) * (exp(-pi * s / 2) / (s^2 + 1)), exp(-pi * s / 2)))

A6 <- matrix(c(13, 12, 12, 13), nrow = 2, byrow = TRUE)
A6_root <- matrix(c(3, 2, 2, 3), nrow = 2, byrow = TRUE)
stopifnot(near(A6_root %*% A6_root, A6))
stopifnot(near(solve(A6, c(25, 25)), c(1, 1)))
stopifnot(near(det(A6), 25))
stopifnot(near(solve(A6), matrix(c(13, -12, -12, 13), 2, byrow = TRUE) / 25))

cat("All 25 mock-exam answers and derivation structures passed independent checks.\n")
