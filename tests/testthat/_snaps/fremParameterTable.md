# fremParameterTable works

    Code
      stabilize(fremParameterTable(runno = runno, modDevDir = modDevDir, thetaNum = 1:
        7, omegaNum = 1:5, sigmaNum = 1:2, numNonFREMThetas = numNonFREMThetas,
      numSkipOm = numSkipOm, availCov = "all", quiet = TRUE))
    Output
      $parameterTable
          Type   Parameter     Estimate
      1  THETA      THETA1   1.00000000
      2  THETA      THETA2   6.14514000
      3  THETA      THETA3 122.52500000
      4  THETA      THETA4   1.88694000
      5  THETA      THETA5   0.67037400
      6  THETA      THETA6  -0.05222250
      7  THETA      THETA7   0.12113200
      8  OMEGA OMEGA1 (SD)   0.23280872
      9  OMEGA OMEGA2 (SD)   0.01000000
      10 OMEGA OMEGA3 (SD)   0.25530927
      11 OMEGA OMEGA4 (SD)   0.22819528
      12 OMEGA OMEGA5 (SD)   0.20621543
      13 SIGMA SIGMA1 (SD)   0.17604289
      14 SIGMA SIGMA2 (SD)   0.00031623
      
      $Samples
      data frame with 0 columns and 0 rows
      

---

    Code
      stabilize(fremParameterTable(runno = runno, modDevDir = modDevDir, thetaNum = 1:
        7, omegaNum = 1:5, sigmaNum = 1:2, numNonFREMThetas = numNonFREMThetas,
      numSkipOm = numSkipOm, availCov = c("SEX", "WT"), quiet = TRUE))
    Output
      $parameterTable
          Type   Parameter     Estimate
      1  THETA      THETA1   1.00000000
      2  THETA      THETA2   6.14514000
      3  THETA      THETA3 122.52500000
      4  THETA      THETA4   1.88694000
      5  THETA      THETA5   0.67037400
      6  THETA      THETA6  -0.05222250
      7  THETA      THETA7   0.12113200
      8  OMEGA OMEGA1 (SD)   0.23280872
      9  OMEGA OMEGA2 (SD)   0.01000000
      10 OMEGA OMEGA3 (SD)   0.40968797
      11 OMEGA OMEGA4 (SD)   0.37586803
      12 OMEGA OMEGA5 (SD)   0.21696123
      13 SIGMA SIGMA1 (SD)   0.17604289
      14 SIGMA SIGMA2 (SD)   0.00031623
      
      $Samples
      data frame with 0 columns and 0 rows
      

---

    Code
      stabilize(fremParameterTable(runno = runno, modDevDir = modDevDir, thetaNum = 2:
        6, omegaNum = c(1, 3, 4, 5), sigmaNum = 1, numNonFREMThetas = numNonFREMThetas,
      numSkipOm = numSkipOm, availCov = "all", quiet = TRUE))
    Output
      $parameterTable
          Type   Parameter    Estimate
      1  THETA      THETA2   6.1451400
      2  THETA      THETA3 122.5250000
      3  THETA      THETA4   1.8869400
      4  THETA      THETA5   0.6703740
      5  THETA      THETA6  -0.0522225
      6  OMEGA OMEGA1 (SD)   0.2328087
      7  OMEGA OMEGA3 (SD)   0.2553093
      8  OMEGA OMEGA4 (SD)   0.2281953
      9  OMEGA OMEGA5 (SD)   0.2062154
      10 SIGMA SIGMA1 (SD)   0.1760429
      
      $Samples
      data frame with 0 columns and 0 rows
      

---

    Code
      stabilize(fremParameterTable(runno = runno, modDevDir = modDevDir, thetaNum = 2:
        7, omegaNum = c(1, 3, 4, 5), sigmaNum = 1, thetaLabels = c("CL (L/h)",
        "V (L)", "MAT (h)", "D1 (h)", "Food on Frel", "Food on MAT"), omegaLabels = c(
        "IIV on RUV", "IIV on CL", "IIV on V", "IIV on MAT"), sigmaLabels = c("RUV"),
      numNonFREMThetas = numNonFREMThetas, numSkipOm = numSkipOm, availCov = "all",
      quiet = TRUE))
    Output
      $parameterTable
          Type       Parameter    Estimate
      1  THETA        CL (L/h)   6.1451400
      2  THETA           V (L) 122.5250000
      3  THETA         MAT (h)   1.8869400
      4  THETA          D1 (h)   0.6703740
      5  THETA    Food on Frel  -0.0522225
      6  THETA     Food on MAT   0.1211320
      7  OMEGA IIV on RUV (SD)   0.2328087
      8  OMEGA  IIV on CL (SD)   0.2553093
      9  OMEGA   IIV on V (SD)   0.2281953
      10 OMEGA IIV on MAT (SD)   0.2062154
      11 SIGMA        RUV (SD)   0.1760429
      
      $Samples
      data frame with 0 columns and 0 rows
      

