library(testthat)
library(slide)

if (requireNamespace("xml2")) {
  test_check("slide", reporter = MultiReporter$new(reporters = list(JunitReporter$new(file = "test-results.xml"), CheckReporter$new())))
} else {
  test_check("slide")
}
