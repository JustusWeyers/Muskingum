
demo_Q = data.frame(
  t = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
  Q_in = c(5, 23, 37, 45, 40, 30, 20, 11, 6, 5, 5, 5, 5),
  Q_out = c(5, 5, 19, 28, 34, 38, 32, 25, 18, 13, 9, 6, 5)
)

demo_flood = data.frame(
  t = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
  Q_flood = c(2.4, 4.4, 18.5, 28.6, 31.8, 49.5, 50.5, 29.9, 17.7, 10.6, 5.0, 2.4)
)

source("functions.R")
