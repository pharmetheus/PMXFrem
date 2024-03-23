# the correct columns are added

    Code
      addFREMcovariates(data, covariates = "test")
    Condition
      Warning in `addFREMcovariates()`:
      test does not exist in the data set
      Error in `addFREMcovariates()`:
      ! No binarised covariates to add to the FFEM data.

---

    Code
      addFREMcovariates(data, covariates = "SEX")
    Condition
      Warning in `addFREMcovariates()`:
      SEX has only two non-missing levels. Nothing added to data set.
      Error in `addFREMcovariates()`:
      ! No binarised covariates to add to the FFEM data.

---

    Code
      addFREMcovariates(data, covariates = c("ETHNIC", "SEX"))
    Condition
      Warning in `addFREMcovariates()`:
      ETHNIC has only two non-missing levels. Nothing added to data set.
      Warning in `addFREMcovariates()`:
      SEX has only two non-missing levels. Nothing added to data set.
      Error in `addFREMcovariates()`:
      ! No binarised covariates to add to the FFEM data.

