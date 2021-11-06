# This is the function itself
pwr.t2n.ratio <- function(n_ratio = 1, d, sig.level, power, alternative) {
  if (power >= 1) {
    return(Inf)
  }
  fn <- Vectorize(function(n1) {
    effN <- n1 * n_ratio / (1 + n_ratio)
    df <- n1 * (1 + n_ratio) - 2
    ncp <- sqrt(effN) * d
    if (alternative == "two.sided") {
      critt <- qt(sig.level / 2, df)
      pow <- pt(critt, df, ncp) + 1 - pt(-critt, df, ncp)
    } else if (alternative == "less") {
      critt <- qt(1-sig.level, df)
      pow <- 1-pt(critt, df, ncp)
    } else if (alternative == "greater") {
      critt <- qt(1 - sig.level, df)
      pow <- 1 - pt(critt, df, ncp)
    } else {
      stop("Invalid alternative.")
    }
    return(log(pow) - log(power))
  }, "n1")
  rt <- uniroot(fn, c(ceiling(3 / (1 + n_ratio)), 1e+09))$root
  return(ceiling(rt))
}

# Here are exemplary tests
library(testthat)
test_that("The currently broken scenario is fixed", {
  expect_equal(
    pwr.t2n.ratio(n_ratio = 1, d = 0.5, sig.level = .05, power = 0.9, alternative = "less"),
    70
  )
})

test_that("currently working functions continue working", {
  expect_equal(
    pwr.t2n.ratio(n_ratio = 1, d = 0.5, sig.level = .05, power = 0.9, alternative = "two.sided"),
    86
  )
  
  expect_equal(
    pwr.t2n.ratio(n_ratio = 1, d = 0.5, sig.level = .05, power = 0.9, alternative = "greater"),
    70
  )
})