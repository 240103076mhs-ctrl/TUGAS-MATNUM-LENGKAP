f <- function(x) {
  x^3 - x - 2
}

bisection <- function(f, a, b, tol = 1e-6, max_iter = 100) {
  for (i in 1:max_iter) {
    c <- (a + b) / 2
    
    if (abs(f(c)) < tol) {
      return(c)
    }
    
    if (f(a) * f(c) < 0) {
      b <- c
    } else {
      a <- c
    }
  }
  return(c)
}

hasil <- bisection(f, 1, 2)
print(hasil)