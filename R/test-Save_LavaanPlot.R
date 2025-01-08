## Saved file
test_that("saveLavaanPlot saves the file successfully", {
  test_file <- tempfile(fileext = ".png")

  # Test with a simple lavaan model
  library(lavaan)
  model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
  data <- HolzingerSwineford1939
  fit <- cfa(model, data = data)

  # Call the function
  saveLavaanPlot(fit = fit, filePath = test_file)

  # Check if the file exists
  expect_true(file.exists(test_file))

  # Cleanup
  unlink(test_file)
})


## valid fit object
test_that("saveLavaanPlot throws an error for invalid fit object", {
  expect_error(saveLavaanPlot(fit = "not_a_model", filePath = "test.png"))
})


## invalid file path
test_that("saveLavaanPlot throws an error for invalid file path", {
  library(lavaan)
  model <- ' visual  =~ x1 + x2 + x3 '
  fit <- cfa(model, data = HolzingerSwineford1939)
  expect_error(saveLavaanPlot(fit = fit, filePath = "test"))
})



## Directory creation
test_that("saveLavaanPlot creates a directory if it does not exist", {
  temp_dir <- tempfile()
  temp_file <- file.path(temp_dir, "test.png")

  library(lavaan)
  model <- ' visual  =~ x1 + x2 + x3 '
  fit <- cfa(model, data = HolzingerSwineford1939)

  saveLavaanPlot(fit = fit, filePath = temp_file)

  expect_true(file.exists(temp_dir))
  expect_true(file.exists(temp_file))

  # Clean-up
  unlink(temp_dir, recursive = TRUE)
})


