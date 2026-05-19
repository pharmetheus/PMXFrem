test_that("filterDataFromModel applies a simple IGNORE statement", {
  # Setup
  td <- withr::local_tempdir()
  model_path <- file.path(td, "model.mod")
  
  wide_data <- data.frame(
    ID = c(1, 1, 2, 2),
    SEX = c(1, 1, 0, 0),
    DV = c(10, 12, 20, 22)
  )
  
  writeLines(c(
    "$PROBLEM Test",
    "$INPUT ID SEX DV",
    "$DATA data.csv IGNORE=(SEX.EQ.1)"
  ), model_path)
  
  # Action
  filtered <- filterDataFromModel(model_path, wide_data)
  
  # Assertions
  expect_equal(nrow(filtered), 2)
  expect_equal(unique(filtered$SEX), 0)
})

test_that("filterDataFromModel applies a simple ACCEPT statement", {
  # Setup
  td <- withr::local_tempdir()
  model_path <- file.path(td, "model.mod")
  
  wide_data <- data.frame(
    ID = c(1, 1, 2, 2),
    AGE = c(25, 25, 35, 35),
    DV = c(10, 12, 20, 22)
  )
  
  writeLines(c(
    "$PROBLEM Test",
    "$INPUT ID AGE DV",
    "$DATA data.csv ACCEPT=(AGE.GT.30)"
  ), model_path)
  
  # Action
  filtered <- filterDataFromModel(model_path, wide_data)
  
  # Assertions
  expect_equal(nrow(filtered), 2)
  expect_equal(unique(filtered$AGE), 35)
})

test_that("filterDataFromModel handles comma as OR operator", {
  # Setup
  td <- withr::local_tempdir()
  model_path <- file.path(td, "model.mod")
  
  wide_data <- data.frame(
    ID = c(1, 2, 3, 4),
    AGE = c(20, 55, 30, 60)
  )
  
  writeLines(c(
    "$PROBLEM Test",
    "$INPUT ID AGE",
    "$DATA data.csv ACCEPT=(ID==1, AGE.GT.50)" # Accept ID 1 OR Age > 50
  ), model_path)
  
  # Action
  filtered <- filterDataFromModel(model_path, wide_data)
  
  # Assertions
  expect_equal(nrow(filtered), 3)
  expect_equal(sort(filtered$ID), c(1, 2, 4))
})

test_that("filterDataFromModel handles default equality operator", {
  # Setup
  td <- withr::local_tempdir()
  model_path <- file.path(td, "model.mod")
  
  wide_data <- data.frame(
    ID = c(1, 2, 3),
    SEX = c(1, 0, 1)
  )
  
  writeLines(c(
    "$PROBLEM Test",
    "$INPUT ID SEX",
    "$DATA data.csv IGNORE=(SEX=1)" # Should default to SEX == 1
  ), model_path)
  
  # Action
  filtered <- filterDataFromModel(model_path, wide_data)
  
  # Assertions
  expect_equal(nrow(filtered), 1)
  expect_equal(filtered$ID, 2)
})


test_that("filterDataFromModel handles optional equals sign", {
  # Setup
  td <- withr::local_tempdir()
  model_path <- file.path(td, "model.mod")
  
  wide_data <- data.frame(ID = c(1, 2), DV = c(10, 20))
  
  writeLines(c(
    "$PROBLEM Test",
    "$INPUT ID DV",
    "$DATA data.csv ACCEPT(ID==2)" # No equals sign
  ), model_path)
  
  # Action
  filtered <- filterDataFromModel(model_path, wide_data)
  
  # Assertions
  expect_equal(nrow(filtered), 1)
  expect_equal(filtered$ID, 2)
})

test_that("filterDataFromModel handles multiple IGNORE statements", {
  # Setup
  td <- withr::local_tempdir()
  model_path <- file.path(td, "model.mod")
  
  wide_data <- data.frame(
    ID = 1:5,
    SEX = c(1, 0, 1, 0, 1),
    BLQ = c(1, 1, 0, 0, 0)
  )
  
  writeLines(c(
    "$PROBLEM Test",
    "$INPUT ID SEX BLQ",
    "$DATA data.csv IGNORE=(SEX.EQ.1) IGNORE=(BLQ.EQ.1)"
  ), model_path)
  
  # Action
  # The function should ignore rows where SEX is 1 OR BLQ is 1
  filtered <- filterDataFromModel(model_path, wide_data)
  
  # Assertions
  expect_equal(nrow(filtered), 1)
  expect_equal(filtered$ID, 4)
})

test_that("filterDataFromModel errors on combined ACCEPT and IGNORE lists", {
  # Setup
  td <- withr::local_tempdir()
  model_path <- file.path(td, "model.mod")
  wide_data <- data.frame(ID = 1, DV = 10)
  
  writeLines(c(
    "$PROBLEM Test",
    "$INPUT ID DV",
    "$DATA data.csv ACCEPT=(ID==1) IGNORE=(ID==2)"
  ), model_path)
  
  # Action & Assertion
  expect_error(
    filterDataFromModel(model_path, wide_data),
    "An ACCEPT=\\(list\\) and IGNORE=\\(list\\) cannot be used in the same"
  )
})

test_that("filterDataFromModel warns for single-character IGNOREs and can be silenced", {
  # Setup
  td <- withr::local_tempdir()
  model_path <- file.path(td, "model.mod")
  wide_data <- data.frame(ID = 1, DV = 10)
  
  writeLines(c(
    "$PROBLEM Test",
    "$INPUT ID DV",
    "$DATA data.csv IGNORE=@"
  ), model_path)
  
  # Assertions
  # 1. Expect a warning by default
  expect_warning(
    filterDataFromModel(model_path, wide_data),
    "A single-character IGNORE statement was found"
  )
  
  # 2. Expect NO warning when quiet = TRUE
  expect_no_warning(
    filterDataFromModel(model_path, wide_data, quiet = TRUE)
  )
  
  # 3. Data should be unchanged since the filter is ignored
  filtered_quiet <- filterDataFromModel(model_path, wide_data, quiet = TRUE)
  expect_equal(nrow(filtered_quiet), 1)
})
