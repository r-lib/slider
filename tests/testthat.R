library(testthat)
library(slider)

if (requireNamespace("xml2")) {
  test_check("slider", reporter = MultiReporter$new(reporters = list(JunitReporter$new(file = "test-results.xml"), CheckReporter$new())))
} else {
  test_check("slider")
}
