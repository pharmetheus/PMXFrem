# the correct columns are added

    Code
      addFREMcovariates(data %>% filter(NCIL != 2), covariates = c("RACEL", "NCIL"))
    Warning <simpleWarning>
      NCIL has only two non-missing levels, not added to data set.
    Output
      # A tibble: 33,454 x 37
            NO    ID STUDYID   TAD  TIME   DAY   AMT  RATE    DV  LNDV  EVID   BLQ
         <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
       1     2     1       1   0     0      15     5    -2  0     0        4     0
       2     2     1       1   0.5   0.5    15     0     0 34.3   3.53     0     0
       3     2     1       1   1     1      15     0     0 53.2   3.97     0     0
       4     2     1       1   2     2      15     0     0 35.7   3.58     0     0
       5     2     1       1   4     4      15     0     0 43.3   3.77     0     0
       6     2     1       1   6     6      15     0     0 61.1   4.11     0     0
       7     2     1       1   8     8      15     0     0 33.8   3.52     0     0
       8     2     1       1  12    12      15     0     0 23.9   3.18     0     0
       9     2     1       1  16    16      16     0     0 10.9   2.39     0     0
      10     2     1       1  24    24      16     0     0  6.88  1.93     0     0
      # ... with 33,444 more rows, and 25 more variables: DOSE <dbl>, FOOD <dbl>,
      #   FORM <dbl>, TYPE <dbl>, WT <dbl>, HT <dbl>, LBWT <dbl>, BSA <dbl>,
      #   SEX <dbl>, RACE <dbl>, AGE <dbl>, AST <dbl>, ALT <dbl>, BILI <dbl>,
      #   CRCL <dbl>, BMI <dbl>, NCI <dbl>, GENO2 <dbl>, ETHNIC <dbl>, SMOK <dbl>,
      #   RACEL <dbl>, NCIL <dbl>, RACEL_3 <dbl>, RACEL_2 <dbl>, RACEL_1 <dbl>

---

    Code
      addFREMcovariates(data %>% filter(NCIL != 2), covariates = c("RACEL", "NCIL",
        "RACE"))
    Warning <simpleWarning>
      NCIL has only two non-missing levels, not added to data set.
    Output
      # A tibble: 33,454 x 40
            NO    ID STUDYID   TAD  TIME   DAY   AMT  RATE    DV  LNDV  EVID   BLQ
         <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
       1     2     1       1   0     0      15     5    -2  0     0        4     0
       2     2     1       1   0.5   0.5    15     0     0 34.3   3.53     0     0
       3     2     1       1   1     1      15     0     0 53.2   3.97     0     0
       4     2     1       1   2     2      15     0     0 35.7   3.58     0     0
       5     2     1       1   4     4      15     0     0 43.3   3.77     0     0
       6     2     1       1   6     6      15     0     0 61.1   4.11     0     0
       7     2     1       1   8     8      15     0     0 33.8   3.52     0     0
       8     2     1       1  12    12      15     0     0 23.9   3.18     0     0
       9     2     1       1  16    16      16     0     0 10.9   2.39     0     0
      10     2     1       1  24    24      16     0     0  6.88  1.93     0     0
      # ... with 33,444 more rows, and 28 more variables: DOSE <dbl>, FOOD <dbl>,
      #   FORM <dbl>, TYPE <dbl>, WT <dbl>, HT <dbl>, LBWT <dbl>, BSA <dbl>,
      #   SEX <dbl>, RACE <dbl>, AGE <dbl>, AST <dbl>, ALT <dbl>, BILI <dbl>,
      #   CRCL <dbl>, BMI <dbl>, NCI <dbl>, GENO2 <dbl>, ETHNIC <dbl>, SMOK <dbl>,
      #   RACEL <dbl>, NCIL <dbl>, RACEL_3 <dbl>, RACEL_2 <dbl>, RACE_5 <dbl>,
      #   RACE_4 <dbl>, RACE_3 <dbl>, RACE_2 <dbl>

---

    Code
      addFREMcovariates(data, covariates = "test")
    Warning <simpleWarning>
      test does not exist in the data set
    Error <simpleError>
      No binarised covariates to add to the FFEM data.

---

    Code
      addFREMcovariates(data, covariates = "SEX")
    Warning <simpleWarning>
      SEX has only two non-missing levels, not added to data set.
    Error <simpleError>
      No binarised covariates to add to the FFEM data.

---

    Code
      addFREMcovariates(data, covariates = c("ETHNIC", "SEX"))
    Warning <simpleWarning>
      ETHNIC has only two non-missing levels, not added to data set.
      SEX has only two non-missing levels, not added to data set.
    Error <simpleError>
      No binarised covariates to add to the FFEM data.

