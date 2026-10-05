# updateFREMmodel can remove covariates from FREM models

    {
      "type": "character",
      "attributes": {},
      "value": [";; 1. Based on: 30", ";; 2. Description:", ";;    Final frem30.dir mdoel", ";; 3. Label:", ";;    SimVal base model", ";------------------------------------------------------------------------------", "$PROBLEM    run 1", "$INPUT ID TIME AMT EVID RATE DV FOOD FREMTYPE", "$DATA [placeholder_path] IGNORE=@", "$SUBROUTINE ADVAN2 TRANS2", "$PK", ";;; MATFOOD-DEFINITION START", "IF(FOOD.EQ.1) MATFOOD = 1  ; Most common", "IF(FOOD.EQ.0) MATFOOD = ( 1 + THETA(6))", ";;; MATFOOD-DEFINITION END", "", ";;; MAT-RELATION START", "MATCOVTIME = MATFOOD", ";;; MAT-RELATION END", "", "", ";;; FRELFOOD-DEFINITION START", "IF(FOOD.EQ.1) FRELFOOD = 1  ; Most common", "IF(FOOD.EQ.0) FRELFOOD = ( 1 + THETA(7))", ";;; FRELFOOD-DEFINITION END", "", ";;; FREL-RELATION START", "FRELCOVTIME = FRELFOOD", ";;; FREL-RELATION END", "", "TVFREL  = THETA(1)", "TVCL    = THETA(2)", "TVV     = THETA(3)", "TVMAT   = THETA(4)", "TVD1    = THETA(5)", "", ";MU_1  = LOG(TVRUV)", "MU_2  = TVD1", "MU_3  = LOG(TVCL)", "MU_4  = LOG(TVV)", "MU_5  = LOG(TVMAT)", "", "D1FR  = MU_2                   + ETA(2)", "FREL  = TVFREL*FRELCOVTIME", "CL    = EXP(MU_3               + ETA(3))", "V     = EXP(MU_4               + ETA(4))", "MAT   = MATCOVTIME * EXP(MU_5  + ETA(5))", "D1    = MAT*(1-D1FR)", "", "F1    = FREL", "KA    = 1 / (MAT-D1)", "S2    = V", "", "      MU_6 = THETA(8)", "      COV6 = MU_6 + ETA(6)", "      MU_7 = THETA(9)", "      COV7 = MU_7 + ETA(7)", "      MU_8 = THETA(10)", "      COV8 = MU_8 + ETA(8)", "      MU_9 = THETA(11)", "      COV9 = MU_9 + ETA(9)", "      MU_10 = THETA(12)", "      COV10 = MU_10 + ETA(10)", "      MU_11 = THETA(13)", "      COV11 = MU_11 + ETA(11)", "      MU_12 = THETA(14)", "      COV12 = MU_12 + ETA(12)", "      MU_13 = THETA(15)", "      COV13 = MU_13 + ETA(13)", "      MU_14 = THETA(16)", "      COV14 = MU_14 + ETA(14)", "      MU_15 = THETA(17)", "      COV15 = MU_15 + ETA(15)", "      MU_16 = THETA(18)", "      COV16 = MU_16 + ETA(16)", "      MU_17 = THETA(19)", "      COV17 = MU_17 + ETA(17)", "      MU_18 = THETA(20)", "      COV18 = MU_18 + ETA(18)", "      MU_19 = THETA(21)", "      COV19 = MU_19 + ETA(19)", "      MU_20 = THETA(22)", "      COV20 = MU_20 + ETA(20)", "      MU_21 = THETA(23)", "      COV21 = MU_21 + ETA(21)", "$ERROR", "CP    = A(2)*1000 / V", "IPRED = LOG(CP + 0.00001)", "Y     = IPRED + EPS(1) * EXP(ETA(1))", "", ";;;FREM CODE BEGIN COMPACT", ";;;DO NOT MODIFY", "      IF(FREMTYPE.EQ.100) THEN", ";        HT 1", "         Y = COV6 + EPS(2)", "         IPRED = COV6", "      END IF", "      IF(FREMTYPE.EQ.200) THEN", ";        LBWT 1", "         Y = COV7 + EPS(2)", "         IPRED = COV7", "      END IF", "      IF(FREMTYPE.EQ.300) THEN", ";        BSA 1", "         Y = COV8 + EPS(2)", "         IPRED = COV8", "      END IF", "      IF(FREMTYPE.EQ.400) THEN", ";        AGE 1", "         Y = COV9 + EPS(2)", "         IPRED = COV9", "      END IF", "      IF(FREMTYPE.EQ.500) THEN", ";        AST 1", "         Y = COV10 + EPS(2)", "         IPRED = COV10", "      END IF", "      IF(FREMTYPE.EQ.600) THEN", ";        ALT 1", "         Y = COV11 + EPS(2)", "         IPRED = COV11", "      END IF", "      IF(FREMTYPE.EQ.700) THEN", ";        BILI 1", "         Y = COV12 + EPS(2)", "         IPRED = COV12", "      END IF", "      IF(FREMTYPE.EQ.800) THEN", ";        CRCL 1", "         Y = COV13 + EPS(2)", "         IPRED = COV13", "      END IF", "      IF(FREMTYPE.EQ.900) THEN", ";        BMI 1", "         Y = COV14 + EPS(2)", "         IPRED = COV14", "      END IF", "      IF(FREMTYPE.EQ.1000) THEN", ";        RACEL_3 1", "         Y = COV15 + EPS(2)", "         IPRED = COV15", "      END IF", "      IF(FREMTYPE.EQ.1100) THEN", ";        RACEL_2 1", "         Y = COV16 + EPS(2)", "         IPRED = COV16", "      END IF", "      IF(FREMTYPE.EQ.1200) THEN", ";        NCIL_2 1", "         Y = COV17 + EPS(2)", "         IPRED = COV17", "      END IF", "      IF(FREMTYPE.EQ.1300) THEN", ";        NCIL_1 1", "         Y = COV18 + EPS(2)", "         IPRED = COV18", "      END IF", "      IF(FREMTYPE.EQ.1400) THEN", ";        GENO2 1", "         Y = COV19 + EPS(2)", "         IPRED = COV19", "      END IF", "      IF(FREMTYPE.EQ.1500) THEN", ";        ETHNIC 1", "         Y = COV20 + EPS(2)", "         IPRED = COV20", "      END IF", "      IF(FREMTYPE.EQ.1600) THEN", ";        SMOK 1", "         Y = COV21 + EPS(2)", "         IPRED = COV21", "      END IF", ";;;FREM CODE END COMPACT", "$THETA 1 FIX ; 1. TVFREL", "$THETA 6.14514 ; 2. TVCL", "$THETA 122.525 ; 3. TVV", "$THETA 1.88694 ; 4. TVMAT", "$THETA 0.670374 ; 5. D1", "$THETA -0.0522225 ; 6. FRELFOOD1", "$THETA 0.121132 ; 7. MATFOOD1", "$THETA 169.651 ; TV_WT", "$THETA 57.5254 ; TV_HT", "$THETA 2.01183 ; TV_LBWT", "$THETA 43.8226 ; TV_BSA", "$THETA 25.5516 ; TV_AGE", "$THETA 28.5493 ; TV_AST", "$THETA 9.81901 ; TV_ALT", "$THETA 119.212 ; TV_BILI", "$THETA 30.0976 ; TV_CRCL", "$THETA 0.0246596 ; TV_BMI", "$THETA 0.195798 ; TV_SEX", "$THETA 0.0224549 ; TV_RACEL_3", "$THETA 0.157896 ; TV_RACEL_2", "$THETA 1.8036 ; TV_NCIL_2", "$THETA 0.458201 ; TV_NCIL_1", "$THETA 0.0369864 ; TV_GENO2", "$OMEGA BLOCK(1) 0.0541999 ; 1. IIV on RUV", "$OMEGA BLOCK(1) 1e-04 FIX ; 2. IIV on D1", "$OMEGA BLOCK(19) ", "0.183435  ; 3. IIV on CL", "0.137661 0.196733  ; 4. IIV on V", "0.0136641 0.0282188 0.0473756  ; 5. IIV on MAT", "0.127998 0.021 0.100029 104.095  ; BSV_WT", "0.666724 0.792017 0.195637 94.8366 123.336  ; BSV_HT", "0.0287738 0.0459165 0.00464313 1.81187 2.6657 0.0752155  ; BSV_LBWT", "0.701637 0.909366 0.0836751 -8.6079 16.5304 0.70081 177.113  ; BSV_BSA", "-0.251462 0.17594 -0.281731 14.3898 19.9453 0.393441 21.189 125.748  ; BSV_AGE", "-0.253527 0.160016 0.104703 26.3127 40.2234 0.777756 19.3603 157.022 300.217  ; BSV_AST", "-0.476132 -0.276369 -0.123245 12.8684 10.09 0.0757738 -12.7251 7.61158 10.7853 27.7596  ; BSV_ALT", "1.6075 2.8213 0.41529 54.6493 90.4787 3.43366 -144.73 3.33242 26.0449 -10.3118 626.339  ; BSV_BILI", "0.779549 1.33402 0.0758799 -1.39477 27.318 1.25871 24.7927 3.68852 8.78061 -4.29812 73.508 38.2588  ; BSV_CRCL", "-0.00520646 -0.00649566 0.000836734 -0.14328 -0.118583 -0.00401741 0.0124454 -0.0306336 0.0290185 0.0573855 -0.151578 -0.0471858 0.0240089  ; BSV_BMI", "-0.00205181 0.00539926 -0.00233377 0.652586 0.344738 0.00818335 -0.947977 -0.250417 -1.2021 0.0407131 -0.90198 -0.0996553 -0.00498082 0.157669  ; BSV_SEX", "-0.0106033 -0.00342586 -0.002829 0.0627517 0.0477866 -0.000747647 -0.0325867 0.21578 0.315912 0.472319 -0.359349 -0.0532011 -0.000442111 0.000264146 0.0225553  ; BSV_RACEL_3", "-0.0288916 -0.0105478 -0.00454967 0.718929 0.719799 0.00996643 -0.105498 1.62038 3.47002 0.700085 -0.0195396 -0.086853 0.00510647 -0.00848174 -0.00363168 0.132082  ; BSV_RACEL_2", "0.118593 0.117165 0.00231381 -0.245131 -0.170493 -0.00169642 0.278638 -0.0668201 -0.209589 -0.132641 -0.126858 0.0789434 -0.00739947 -0.000340582 -0.000354871 -0.0139211 0.158199  ; BSV_NCIL_2", "-0.00827334 -0.031517 0.000344643 -1.73893 -1.74645 -0.0488599 -0.134246 -0.492962 0.0117242 -0.29347 -1.01561 -0.527158 -0.0112897 -0.0642058 -0.00402577 -0.0132472 0.0203806 0.248758  ; BSV_NCIL_1", "0.0143385 0.0122878 0.00182188 0.0551072 0.000468563 0.00185725 -0.076698 0.0353325 0.0892892 -0.0177387 -0.102328 0.0331443 0.00224461 -0.00310526 -0.000857598 -6.18609e-05 2.5645e-05 -0.0100051 0.0356667  ; BSV_GENO2", "$SIGMA  0.0310236  ;     1. RUV", "$SIGMA  1E-07  FIX  ;     EPSCOV", "$ESTIMATION METHOD=IMPMAP AUTO=1 RANMETHOD=3P INTER NOABORT PRINT=1", "            NOCOV=1 ISAMPEND=100 NITER=50 ISAMPLE=300", "$ESTIMATION METHOD=IMPMAP INTER EONLY=1 NITER=50 ISAMPLE=10000 NOCOV=0", "            PHITYPE=1", ";$COVARIANCE  UNCONDITIONAL", "$TABLE      NO ID STUDYID TAD TIME DAY AMT RATE ODV DV EVID BLQ DOSE", "            FOOD FORM TYPE WT HT LBWT BSA SEX RACE AGE AST ALT BILI", "            CRCL BMI NCI GENO2 ETHNIC SMOK RACE NCIL CPRED CIPREDI", "            CWRES CIWRES ETAS(1:LAST) NOPRINT ONEHEADER FILE=xptab31", ""]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["column", "type", "n", "nMissing", "nDistinct", "mean", "sd", "min", "max", "hash"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["ID", "TIME", "AMT", "EVID", "RATE", "DV", "FOOD", "FREMTYPE"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["integer", "numeric", "numeric", "integer", "integer", "numeric", "integer", "numeric"]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [44083, 44083, 44083, 44083, 44083, 44083, 44083, 44083]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [760, 8034, 5, 3, 2, 5241, 2, 17]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [512.254, 555.532, 2.10444, 0.65143, -1.2589, 7.38636, 0.820225, 195.284]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [265.354, 423.586, 2.555, 0.56125, 0.965916, 27.0188, 0.384004, 425.079]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 0, 0, 0, -2, -2.263, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [964, 1678.4, 10, 4, 0, 198.5, 1, 1600]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["3234674995-1535433802", "860326880-3610783765", "4067671857-2886453885", "2472987486-1847772953", "3968599918-3627343596", "1980526463-1946662023", "2918003864-2112127059", "3690130705-785820642"]
        }
      ]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["ID", "TIME", "AMT", "EVID", "RATE", "DV", "FOOD", "FREMTYPE"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 3, 4, 5, 6, 7, 8, 9]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        }
      },
      "value": [
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [5, 0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [4, 0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-2, 0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0, 180.6, 58.4254342, 1.88993827, 39, 25, 23.946, 95.3]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0, 0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0, 100, 200, 300, 400, 600, 700, 800]
        }
      ]
    }

# updateFREMmodel can add covariates to FREM models

    {
      "type": "character",
      "attributes": {},
      "value": [";; 1. Based on: 30", ";; 2. Description:", ";;    Final frem30.dir mdoel", ";; 3. Label:", ";;    SimVal base model", ";------------------------------------------------------------------------------", "$PROBLEM    run 1", "$INPUT ID TIME AMT EVID RATE DV FOOD FREMTYPE", "$DATA [placeholder_path] IGNORE=@", "$SUBROUTINE ADVAN2 TRANS2", "$PK", ";;; MATFOOD-DEFINITION START", "IF(FOOD.EQ.1) MATFOOD = 1  ; Most common", "IF(FOOD.EQ.0) MATFOOD = ( 1 + THETA(6))", ";;; MATFOOD-DEFINITION END", "", ";;; MAT-RELATION START", "MATCOVTIME = MATFOOD", ";;; MAT-RELATION END", "", "", ";;; FRELFOOD-DEFINITION START", "IF(FOOD.EQ.1) FRELFOOD = 1  ; Most common", "IF(FOOD.EQ.0) FRELFOOD = ( 1 + THETA(7))", ";;; FRELFOOD-DEFINITION END", "", ";;; FREL-RELATION START", "FRELCOVTIME = FRELFOOD", ";;; FREL-RELATION END", "", "TVFREL  = THETA(1)", "TVCL    = THETA(2)", "TVV     = THETA(3)", "TVMAT   = THETA(4)", "TVD1    = THETA(5)", "", ";MU_1  = LOG(TVRUV)", "MU_2  = TVD1", "MU_3  = LOG(TVCL)", "MU_4  = LOG(TVV)", "MU_5  = LOG(TVMAT)", "", "D1FR  = MU_2                   + ETA(2)", "FREL  = TVFREL*FRELCOVTIME", "CL    = EXP(MU_3               + ETA(3))", "V     = EXP(MU_4               + ETA(4))", "MAT   = MATCOVTIME * EXP(MU_5  + ETA(5))", "D1    = MAT*(1-D1FR)", "", "F1    = FREL", "KA    = 1 / (MAT-D1)", "S2    = V", "", "      MU_6 = THETA(8)", "      COV6 = MU_6 + ETA(6)", "      MU_7 = THETA(9)", "      COV7 = MU_7 + ETA(7)", "      MU_8 = THETA(10)", "      COV8 = MU_8 + ETA(8)", "      MU_9 = THETA(11)", "      COV9 = MU_9 + ETA(9)", "      MU_10 = THETA(12)", "      COV10 = MU_10 + ETA(10)", "      MU_11 = THETA(13)", "      COV11 = MU_11 + ETA(11)", "      MU_12 = THETA(14)", "      COV12 = MU_12 + ETA(12)", "      MU_13 = THETA(15)", "      COV13 = MU_13 + ETA(13)", "      MU_14 = THETA(16)", "      COV14 = MU_14 + ETA(14)", "      MU_15 = THETA(17)", "      COV15 = MU_15 + ETA(15)", "      MU_16 = THETA(18)", "      COV16 = MU_16 + ETA(16)", "      MU_17 = THETA(19)", "      COV17 = MU_17 + ETA(17)", "      MU_18 = THETA(20)", "      COV18 = MU_18 + ETA(18)", "      MU_19 = THETA(21)", "      COV19 = MU_19 + ETA(19)", "      MU_20 = THETA(22)", "      COV20 = MU_20 + ETA(20)", "      MU_21 = THETA(23)", "      COV21 = MU_21 + ETA(21)", "      MU_22 = THETA(24)", "      COV22 = MU_22 + ETA(22)", "      MU_23 = THETA(25)", "      COV23 = MU_23 + ETA(23)", "$ERROR", "CP    = A(2)*1000 / V", "IPRED = LOG(CP + 0.00001)", "Y     = IPRED + EPS(1) * EXP(ETA(1))", "", ";;;FREM CODE BEGIN COMPACT", ";;;DO NOT MODIFY", "      IF(FREMTYPE.EQ.100) THEN", ";        WT 1", "         Y = COV6 + EPS(2)", "         IPRED = COV6", "      END IF", "      IF(FREMTYPE.EQ.200) THEN", ";        HT 1", "         Y = COV7 + EPS(2)", "         IPRED = COV7", "      END IF", "      IF(FREMTYPE.EQ.300) THEN", ";        LBWT 1", "         Y = COV8 + EPS(2)", "         IPRED = COV8", "      END IF", "      IF(FREMTYPE.EQ.400) THEN", ";        BSA 1", "         Y = COV9 + EPS(2)", "         IPRED = COV9", "      END IF", "      IF(FREMTYPE.EQ.500) THEN", ";        AGE 1", "         Y = COV10 + EPS(2)", "         IPRED = COV10", "      END IF", "      IF(FREMTYPE.EQ.600) THEN", ";        AST 1", "         Y = COV11 + EPS(2)", "         IPRED = COV11", "      END IF", "      IF(FREMTYPE.EQ.700) THEN", ";        ALT 1", "         Y = COV12 + EPS(2)", "         IPRED = COV12", "      END IF", "      IF(FREMTYPE.EQ.800) THEN", ";        BILI 1", "         Y = COV13 + EPS(2)", "         IPRED = COV13", "      END IF", "      IF(FREMTYPE.EQ.900) THEN", ";        CRCL 1", "         Y = COV14 + EPS(2)", "         IPRED = COV14", "      END IF", "      IF(FREMTYPE.EQ.1000) THEN", ";        BMI 1", "         Y = COV15 + EPS(2)", "         IPRED = COV15", "      END IF", "      IF(FREMTYPE.EQ.1100) THEN", ";        RACEL_3 1", "         Y = COV16 + EPS(2)", "         IPRED = COV16", "      END IF", "      IF(FREMTYPE.EQ.1200) THEN", ";        RACEL_2 1", "         Y = COV17 + EPS(2)", "         IPRED = COV17", "      END IF", "      IF(FREMTYPE.EQ.1300) THEN", ";        NCIL_2 1", "         Y = COV18 + EPS(2)", "         IPRED = COV18", "      END IF", "      IF(FREMTYPE.EQ.1400) THEN", ";        NCIL_1 1", "         Y = COV19 + EPS(2)", "         IPRED = COV19", "      END IF", "      IF(FREMTYPE.EQ.1500) THEN", ";        GENO2 1", "         Y = COV20 + EPS(2)", "         IPRED = COV20", "      END IF", "      IF(FREMTYPE.EQ.1600) THEN", ";        ETHNIC 1", "         Y = COV21 + EPS(2)", "         IPRED = COV21", "      END IF", "      IF(FREMTYPE.EQ.1700) THEN", ";        SMOK 1", "         Y = COV22 + EPS(2)", "         IPRED = COV22", "      END IF", "      IF(FREMTYPE.EQ.1800) THEN", ";        SEX 1", "         Y = COV23 + EPS(2)", "         IPRED = COV23", "      END IF", ";;;FREM CODE END COMPACT", "$THETA 1 FIX ; 1. TVFREL", "$THETA 6.17489 ; 2. TVCL", "$THETA 123.72 ; 3. TVV", "$THETA 1.86813 ; 4. TVMAT", "$THETA 0.666833 ; 5. D1", "$THETA -0.0551668 ; 6. FRELFOOD1", "$THETA 0.1326 ; 7. MATFOOD1", "$THETA 86.8533 ; TV_WT", "$THETA 169.651 ; TV_HT", "$THETA 57.5143 ; TV_LBWT", "$THETA 2.01171 ; TV_BSA", "$THETA 43.8048 ; TV_AGE", "$THETA 25.542 ; TV_AST", "$THETA 28.5022 ; TV_ALT", "$THETA 9.81823 ; TV_BILI", "$THETA 119.285 ; TV_CRCL", "$THETA 30.0935 ; TV_BMI", "$THETA 0.0245902 ; TV_SEX", "$THETA 0.196098 ; TV_RACEL_3", "$THETA 0.0224704 ; TV_RACEL_2", "$THETA 0.157421 ; TV_NCIL_2", "$THETA 1.8032 ; TV_NCIL_1", "$THETA 0.458589 ; TV_GENO2", "$THETA 0.0369861 ; TV_ETHNIC", "$THETA 1.44 FIX ; 25 TV_SEX", "$OMEGA BLOCK(1) 0.0551649 ; 1. IIV on RUV", "$OMEGA BLOCK(1) 0.000116094 FIX ; 2. IIV on D1", "$OMEGA BLOCK(21) ", "0.183694  ; 3. IIV on CL", "0.138827 0.200023  ; 4. IIV on V", "0.0123448 0.0254075 0.0476874  ; 5. IIV on MAT", "2.3941 3.95563 0.185568 434.104  ; BSV_WT", "0.118969 0.0060112 0.113939 104 104.095  ; BSV_HT", "0.638755 0.753182 0.203469 181.665 94.7357 123.461  ; BSV_LBWT", "0.0283623 0.046066 0.00291337 5.60708 1.81051 2.67184 0.0754029  ; BSV_BSA", "0.693113 0.901184 0.0169782 63.8584 -8.30336 16.4586 0.697718 177.814  ; BSV_AGE", "-0.265491 0.159895 -0.308241 25.6286 14.3207 20.3521 0.39578 21.627 125.595  ; BSV_AST", "-0.249637 0.146304 0.121545 52.588 26.5557 40.8902 0.78794 20.2094 156.793 299.97  ; BSV_ALT", "-0.476454 -0.289272 -0.11408 0.0433564 12.8859 10.0499 0.075132 -12.8881 7.73622 10.9686 27.8108  ; BSV_BILI", "1.65457 2.90078 0.305008 269.646 54.5547 91.693 3.45973 -146.287 2.88634 25.5172 -11.2933 631.706  ; BSV_CRCL", "0.772157 1.34796 0.0117214 110.435 -1.43241 27.5662 1.26533 24.5285 3.83095 8.94584 -4.30816 74.3695 38.4847  ; BSV_BMI", "-0.00490766 -0.00639512 0.00117313 -0.283725 -0.147047 -0.124473 -0.00409009 0.0169571 -0.0278788 0.0296585 0.0566026 -0.136263 -0.047744 0.0240297  ; BSV_SEX", "-0.0024188 0.00527979 -0.00218323 0.414273 0.650629 0.346294 0.00822938 -0.959747 -0.246373 -1.21109 0.040256 -0.873395 -0.0967988 -0.00502836 0.157741  ; BSV_RACEL_3", "-0.0106453 -0.00380084 -0.0026421 -0.103752 0.0634981 0.0443651 -0.000798562 -0.0372864 0.216275 0.313907 0.472608 -0.370428 -0.0551989 -0.000469918 0.000314039 0.0225625  ; BSV_RACEL_2", "-0.0287403 -0.0111923 -0.00388014 0.484006 0.719672 0.732211 0.0100409 -0.10856 1.61068 3.47516 0.701141 -0.0586527 -0.0847343 0.00505769 -0.00859566 -0.00370609 0.132058  ; BSV_NCIL_2", "0.118842 0.117957 0.00196097 -0.0311726 -0.242087 -0.173611 -0.00172917 0.287934 -0.0634738 -0.200353 -0.126555 -0.0870974 0.0759638 -0.00743327 -0.000737226 -0.000553235 -0.0129684 0.158649  ; BSV_NCIL_1", "-0.00842435 -0.032388 0.00211288 -3.34658 -1.75236 -1.76219 -0.0491455 -0.132199 -0.494224 0.00704007 -0.29364 -1.03298 -0.528109 -0.0111578 -0.0640721 -0.00386051 -0.0136643 0.0206967 0.249198  ; BSV_GENO2", "0.014238 0.0122447 0.00147499 0.133606 0.0551143 0.000891277 0.00186186 -0.0740465 0.0317411 0.0938387 -0.0168279 -0.10566 0.0332996 0.00217189 -0.00310031 -0.000846107 -3.39826e-05 5.35366e-05 -0.0101204 0.0356664  ; BSV_ETHNIC", "1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 0.246886516711145  ; 23 BSV_SEX", "$SIGMA  0.0310236  ;     1. RUV", "$SIGMA  1E-07  FIX  ;     EPSCOV", "$ESTIMATION METHOD=IMPMAP AUTO=1 RANMETHOD=3P INTER NOABORT PRINT=1", "            NOCOV=1 ISAMPEND=100 NITER=50 ISAMPLE=300", "$ESTIMATION METHOD=IMPMAP INTER EONLY=1 NITER=50 ISAMPLE=10000 NOCOV=0", "            PHITYPE=1", ";$COVARIANCE  UNCONDITIONAL", "$TABLE      NO ID STUDYID TAD TIME DAY AMT RATE ODV DV EVID BLQ DOSE", "            FOOD FORM TYPE WT HT LBWT BSA SEX RACE AGE AST ALT BILI", "            CRCL BMI NCI GENO2 ETHNIC SMOK RACE NCIL CPRED CIPREDI", "            CWRES CIWRES ETAS(1:LAST) NOPRINT ONEHEADER FILE=xptab31", ""]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["column", "type", "n", "nMissing", "nDistinct", "mean", "sd", "min", "max", "hash"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["ID", "TIME", "AMT", "EVID", "RATE", "DV", "FOOD", "FREMTYPE"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["integer", "numeric", "numeric", "numeric", "numeric", "numeric", "integer", "numeric"]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [45498, 45498, 45498, 45498, 45498, 45498, 45498, 45498]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [760, 8034, 5, 3, 2, 5479, 2, 19]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [509.957, 544.608, 2.03899, 0.631171, -1.21975, 8.48284, 0.81564, 241.65]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [266.236, 424.895, 2.54135, 0.563908, 0.975568, 28.4213, 0.387782, 495.93]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 0, 0, 0, -2, -2.263, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [964, 1678.4, 10, 4, 0, 198.5, 1, 1800]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["862517496-3473949993", "3448637231-959130312", "228690197-2891492906", "1286647932-3820693669", "1625917643-3908090853", "705135647-3622982003", "1799933164-4114468388", "4282857573-1709245534"]
        }
      ]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["ID", "TIME", "AMT", "EVID", "RATE", "DV", "FOOD", "FREMTYPE"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 3, 4, 5, 6, 7, 8, 9]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        }
      },
      "value": [
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [5, 0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [4, 0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-2, 0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0, 71.2, 180.6, 58.4254342, 1.88993827, 39, 25, 23.946]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0, 0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0, 100, 200, 300, 400, 500, 700, 800]
        }
      ]
    }

# updateFREMmodel can update initial estimates in FREM models

    {
      "type": "character",
      "attributes": {},
      "value": [";; 1. Based on: 30", ";; 2. Description:", ";;    Final frem30.dir mdoel", ";; 3. Label:", ";;    SimVal base model", ";------------------------------------------------------------------------------", "$PROBLEM    run 1", "$INPUT      NO ID STUDYID TAD TIME DAY AMT RATE ODV DV EVID BLQ DOSE", "            FOOD FORM TYPE WT HT LBWT BSA SEX RACE AGE AST ALT BILI", "            CRCL BMI NCI GENO2 ETHNIC SMOK RACEL NCIL FREMTYPE", "$DATA      [placeholder_path] IGNORE=@", "$SUBROUTINE ADVAN2 TRANS2", "$PK", ";;; MATFOOD-DEFINITION START", "IF(FOOD.EQ.1) MATFOOD = 1  ; Most common", "IF(FOOD.EQ.0) MATFOOD = ( 1 + THETA(6))", ";;; MATFOOD-DEFINITION END", "", ";;; MAT-RELATION START", "MATCOVTIME = MATFOOD", ";;; MAT-RELATION END", "", "", ";;; FRELFOOD-DEFINITION START", "IF(FOOD.EQ.1) FRELFOOD = 1  ; Most common", "IF(FOOD.EQ.0) FRELFOOD = ( 1 + THETA(7))", ";;; FRELFOOD-DEFINITION END", "", ";;; FREL-RELATION START", "FRELCOVTIME = FRELFOOD", ";;; FREL-RELATION END", "", "TVFREL  = THETA(1)", "TVCL    = THETA(2)", "TVV     = THETA(3)", "TVMAT   = THETA(4)", "TVD1    = THETA(5)", "", ";MU_1  = LOG(TVRUV)", "MU_2  = TVD1", "MU_3  = LOG(TVCL)", "MU_4  = LOG(TVV)", "MU_5  = LOG(TVMAT)", "", "D1FR  = MU_2                   + ETA(2)", "FREL  = TVFREL*FRELCOVTIME", "CL    = EXP(MU_3               + ETA(3))", "V     = EXP(MU_4               + ETA(4))", "MAT   = MATCOVTIME * EXP(MU_5  + ETA(5))", "D1    = MAT*(1-D1FR)", "", "F1    = FREL", "KA    = 1 / (MAT-D1)", "S2    = V", "", "      MU_6 = THETA(8)", "      COV6 = MU_6 + ETA(6)", "      MU_7 = THETA(9)", "      COV7 = MU_7 + ETA(7)", "      MU_8 = THETA(10)", "      COV8 = MU_8 + ETA(8)", "      MU_9 = THETA(11)", "      COV9 = MU_9 + ETA(9)", "      MU_10 = THETA(12)", "      COV10 = MU_10 + ETA(10)", "      MU_11 = THETA(13)", "      COV11 = MU_11 + ETA(11)", "      MU_12 = THETA(14)", "      COV12 = MU_12 + ETA(12)", "      MU_13 = THETA(15)", "      COV13 = MU_13 + ETA(13)", "      MU_14 = THETA(16)", "      COV14 = MU_14 + ETA(14)", "      MU_15 = THETA(17)", "      COV15 = MU_15 + ETA(15)", "      MU_16 = THETA(18)", "      COV16 = MU_16 + ETA(16)", "      MU_17 = THETA(19)", "      COV17 = MU_17 + ETA(17)", "      MU_18 = THETA(20)", "      COV18 = MU_18 + ETA(18)", "      MU_19 = THETA(21)", "      COV19 = MU_19 + ETA(19)", "      MU_20 = THETA(22)", "      COV20 = MU_20 + ETA(20)", "      MU_21 = THETA(23)", "      COV21 = MU_21 + ETA(21)", "      MU_22 = THETA(24)", "      COV22 = MU_22 + ETA(22)", "      MU_23 = THETA(25)", "      COV23 = MU_23 + ETA(23)", "$ERROR", "CP    = A(2)*1000 / V", "IPRED = LOG(CP + 0.00001)", "Y     = IPRED + EPS(1) * EXP(ETA(1))", "", ";;;FREM CODE BEGIN COMPACT", ";;;DO NOT MODIFY", "      IF(FREMTYPE.EQ.100) THEN", ";        WT 1", "         Y = COV6 + EPS(2)", "         IPRED = COV6", "      END IF", "      IF(FREMTYPE.EQ.200) THEN", ";        HT 1", "         Y = COV7 + EPS(2)", "         IPRED = COV7", "      END IF", "      IF(FREMTYPE.EQ.300) THEN", ";        LBWT 1", "         Y = COV8 + EPS(2)", "         IPRED = COV8", "      END IF", "      IF(FREMTYPE.EQ.400) THEN", ";        BSA 1", "         Y = COV9 + EPS(2)", "         IPRED = COV9", "      END IF", "      IF(FREMTYPE.EQ.500) THEN", ";        AGE 1", "         Y = COV10 + EPS(2)", "         IPRED = COV10", "      END IF", "      IF(FREMTYPE.EQ.600) THEN", ";        AST 1", "         Y = COV11 + EPS(2)", "         IPRED = COV11", "      END IF", "      IF(FREMTYPE.EQ.700) THEN", ";        ALT 1", "         Y = COV12 + EPS(2)", "         IPRED = COV12", "      END IF", "      IF(FREMTYPE.EQ.800) THEN", ";        BILI 1", "         Y = COV13 + EPS(2)", "         IPRED = COV13", "      END IF", "      IF(FREMTYPE.EQ.900) THEN", ";        CRCL 1", "         Y = COV14 + EPS(2)", "         IPRED = COV14", "      END IF", "      IF(FREMTYPE.EQ.1000) THEN", ";        BMI 1", "         Y = COV15 + EPS(2)", "         IPRED = COV15", "      END IF", "      IF(FREMTYPE.EQ.1100) THEN", ";        SEX 1", "         Y = COV16 + EPS(2)", "         IPRED = COV16", "      END IF", "      IF(FREMTYPE.EQ.1200) THEN", ";        RACEL_3 1", "         Y = COV17 + EPS(2)", "         IPRED = COV17", "      END IF", "      IF(FREMTYPE.EQ.1300) THEN", ";        RACEL_2 1", "         Y = COV18 + EPS(2)", "         IPRED = COV18", "      END IF", "      IF(FREMTYPE.EQ.1400) THEN", ";        NCIL_2 1", "         Y = COV19 + EPS(2)", "         IPRED = COV19", "      END IF", "      IF(FREMTYPE.EQ.1500) THEN", ";        NCIL_1 1", "         Y = COV20 + EPS(2)", "         IPRED = COV20", "      END IF", "      IF(FREMTYPE.EQ.1600) THEN", ";        GENO2 1", "         Y = COV21 + EPS(2)", "         IPRED = COV21", "      END IF", "      IF(FREMTYPE.EQ.1700) THEN", ";        ETHNIC 1", "         Y = COV22 + EPS(2)", "         IPRED = COV22", "      END IF", "      IF(FREMTYPE.EQ.1800) THEN", ";        SMOK 1", "         Y = COV23 + EPS(2)", "         IPRED = COV23", "      END IF", ";;;FREM CODE END COMPACT", "$THETA 1 FIX ; 1. TVFREL", "$THETA 6.14514 ; 2. TVCL", "$THETA 122.525 ; 3. TVV", "$THETA 1.88694 ; 4. TVMAT", "$THETA 0.670374 ; 5. D1", "$THETA -0.0522225 ; 6. FRELFOOD1", "$THETA 0.121132 ; 7. MATFOOD1", "$THETA 86.8633 ; TV_WT", "$THETA 169.651 ; TV_HT", "$THETA 57.5254 ; TV_LBWT", "$THETA 2.01183 ; TV_BSA", "$THETA 43.8226 ; TV_AGE", "$THETA 25.5516 ; TV_AST", "$THETA 28.5493 ; TV_ALT", "$THETA 9.81901 ; TV_BILI", "$THETA 119.212 ; TV_CRCL", "$THETA 30.0976 ; TV_BMI", "$THETA 1.44109 ; TV_SEX", "$THETA 0.0246596 ; TV_RACEL_3", "$THETA 0.195798 ; TV_RACEL_2", "$THETA 0.0224549 ; TV_NCIL_2", "$THETA 0.157896 ; TV_NCIL_1", "$THETA 1.8036 ; TV_GENO2", "$THETA 0.458201 ; TV_ETHNIC", "$THETA 0.0369864 ; TV_SMOK", "$OMEGA BLOCK(1) 0.0541999 ; 1. IIV on RUV", "$OMEGA BLOCK(1) 1e-04 FIX ; 2. IIV on D1", "$OMEGA BLOCK(21) ", "0.183435  ; 3. IIV on CL", "0.137661 0.196733  ; 4. IIV on V", "0.0136641 0.0282188 0.0473756  ; 5. IIV on MAT", "2.4222 3.92909 0.350015 432.543  ; BSV_WT", "0.127998 0.021 0.100029 104.131 104.095  ; BSV_HT", "0.666724 0.792017 0.195637 181.089 94.8366 123.336  ; BSV_LBWT", "0.0287738 0.0459165 0.00464313 5.59003 1.81187 2.6657 0.0752155  ; BSV_BSA", "0.701637 0.909366 0.0836751 64.2565 -8.6079 16.5304 0.70081 177.113  ; BSV_AGE", "-0.251462 0.17594 -0.281731 25.4064 14.3898 19.9453 0.393441 21.189 125.748  ; BSV_AST", "-0.253527 0.160016 0.104703 51.9152 26.3127 40.2234 0.777756 19.3603 157.022 300.217  ; BSV_ALT", "-0.476132 -0.276369 -0.123245 0.0888749 12.8684 10.09 0.0757738 -12.7251 7.61158 10.7853 27.7596  ; BSV_BILI", "1.6075 2.8213 0.41529 267.257 54.6493 90.4787 3.43366 -144.73 3.33242 26.0449 -10.3118 626.339  ; BSV_CRCL", "0.779549 1.33402 0.0758799 109.832 -1.39477 27.318 1.25871 24.7927 3.68852 8.78061 -4.29812 73.508 38.2588  ; BSV_BMI", "-0.00113323 0.0292476 -0.00522425 -3.90112 -3.25387 -4.23233 -0.0643208 -0.330324 -0.80053 -1.85893 -0.746362 -0.104641 -0.206379 0.246868  ; BSV_SEX", "-0.00520646 -0.00649566 0.000836734 -0.279906 -0.14328 -0.118583 -0.00401741 0.0124454 -0.0306336 0.0290185 0.0573855 -0.151578 -0.0471858 -0.00193126 0.0240089  ; BSV_RACEL_3", "-0.00205181 0.00539926 -0.00233377 0.410641 0.652586 0.344738 0.00818335 -0.947977 -0.250417 -1.2021 0.0407131 -0.90198 -0.0996553 0.00182635 -0.00498082 0.157669  ; BSV_RACEL_2", "-0.0106033 -0.00342586 -0.002829 -0.0987262 0.0627517 0.0477866 -0.000747647 -0.0325867 0.21578 0.315912 0.472319 -0.359349 -0.0532011 -0.00729448 -0.000442111 0.000264146 0.0225553  ; BSV_NCIL_2", "-0.0288916 -0.0105478 -0.00454967 0.478708 0.718929 0.719799 0.00996643 -0.105498 1.62038 3.47002 0.700085 -0.0195396 -0.086853 -0.0394045 0.00510647 -0.00848174 -0.00363168 0.132082  ; BSV_NCIL_1", "0.118593 0.117165 0.00231381 -0.0264943 -0.245131 -0.170493 -0.00169642 0.278638 -0.0668201 -0.209589 -0.132641 -0.126858 0.0789434 0.0095879 -0.00739947 -0.000340582 -0.000354871 -0.0139211 0.158199  ; BSV_GENO2", "-0.00827334 -0.031517 0.000344643 -3.32889 -1.73893 -1.74645 -0.0488599 -0.134246 -0.492962 0.0117242 -0.29347 -1.01561 -0.527158 0.0267216 -0.0112897 -0.0642058 -0.00402577 -0.0132472 0.0203806 0.248758  ; BSV_ETHNIC", "0.0143385 0.0122878 0.00182188 0.133204 0.0551072 0.000468563 0.00185725 -0.076698 0.0353325 0.0892892 -0.0177387 -0.102328 0.0331443 0.00423904 0.00224461 -0.00310526 -0.000857598 -6.18609e-05 2.5645e-05 -0.0100051 0.0356667  ; BSV_SMOK", "$SIGMA  0.0310236  ;     1. RUV", "$SIGMA  1E-07  FIX  ;     EPSCOV", "$ESTIMATION METHOD=IMPMAP AUTO=1 RANMETHOD=3P INTER NOABORT PRINT=1", "            NOCOV=1 ISAMPEND=100 NITER=50 ISAMPLE=300", "$ESTIMATION METHOD=IMPMAP INTER EONLY=1 NITER=50 ISAMPLE=10000 NOCOV=0", "            PHITYPE=1", ";$COVARIANCE  UNCONDITIONAL", "$TABLE      NO ID STUDYID TAD TIME DAY AMT RATE ODV DV EVID BLQ DOSE", "            FOOD FORM TYPE WT HT LBWT BSA SEX RACE AGE AST ALT BILI", "            CRCL BMI NCI GENO2 ETHNIC SMOK RACE NCIL CPRED CIPREDI", "            CWRES CIWRES ETAS(1:LAST) NOPRINT ONEHEADER FILE=xptab31", ""]
    }

# updateFREMmodel correctly removes a polycotomous covariate

    {
      "type": "character",
      "attributes": {},
      "value": [";; 1. Based on: 30", ";; 2. Description:", ";;    Final frem30.dir mdoel", ";; 3. Label:", ";;    SimVal base model", ";------------------------------------------------------------------------------", "$PROBLEM    run 1", "$INPUT ID TIME AMT DV EVID RATE FREMTYPE", "$DATA [placeholder_path] IGNORE=@", "$SUBROUTINE ADVAN2 TRANS2", "$PK", ";;; MATFOOD-DEFINITION START", "IF(FOOD.EQ.1) MATFOOD = 1  ; Most common", "IF(FOOD.EQ.0) MATFOOD = ( 1 + THETA(6))", ";;; MATFOOD-DEFINITION END", "", ";;; MAT-RELATION START", "MATCOVTIME = MATFOOD", ";;; MAT-RELATION END", "", "", ";;; FRELFOOD-DEFINITION START", "IF(FOOD.EQ.1) FRELFOOD = 1  ; Most common", "IF(FOOD.EQ.0) FRELFOOD = ( 1 + THETA(7))", ";;; FRELFOOD-DEFINITION END", "", ";;; FREL-RELATION START", "FRELCOVTIME = FRELFOOD", ";;; FREL-RELATION END", "", "TVFREL  = THETA(1)", "TVCL    = THETA(2)", "TVV     = THETA(3)", "TVMAT   = THETA(4)", "TVD1    = THETA(5)", "", ";MU_1  = LOG(TVRUV)", "MU_2  = TVD1", "MU_3  = LOG(TVCL)", "MU_4  = LOG(TVV)", "MU_5  = LOG(TVMAT)", "", "D1FR  = MU_2                   + ETA(2)", "FREL  = TVFREL*FRELCOVTIME", "CL    = EXP(MU_3               + ETA(3))", "V     = EXP(MU_4               + ETA(4))", "MAT   = MATCOVTIME * EXP(MU_5  + ETA(5))", "D1    = MAT*(1-D1FR)", "", "F1    = FREL", "KA    = 1 / (MAT-D1)", "S2    = V", "", "      MU_6 = THETA(8)", "      COV6 = MU_6 + ETA(6)", "      MU_7 = THETA(9)", "      COV7 = MU_7 + ETA(7)", "      MU_8 = THETA(10)", "      COV8 = MU_8 + ETA(8)", "      MU_9 = THETA(11)", "      COV9 = MU_9 + ETA(9)", "      MU_10 = THETA(12)", "      COV10 = MU_10 + ETA(10)", "      MU_11 = THETA(13)", "      COV11 = MU_11 + ETA(11)", "      MU_12 = THETA(14)", "      COV12 = MU_12 + ETA(12)", "      MU_13 = THETA(15)", "      COV13 = MU_13 + ETA(13)", "      MU_14 = THETA(16)", "      COV14 = MU_14 + ETA(14)", "      MU_15 = THETA(17)", "      COV15 = MU_15 + ETA(15)", "      MU_16 = THETA(18)", "      COV16 = MU_16 + ETA(16)", "      MU_17 = THETA(19)", "      COV17 = MU_17 + ETA(17)", "      MU_18 = THETA(20)", "      COV18 = MU_18 + ETA(18)", "      MU_19 = THETA(21)", "      COV19 = MU_19 + ETA(19)", "      MU_20 = THETA(22)", "      COV20 = MU_20 + ETA(20)", "      MU_21 = THETA(23)", "      COV21 = MU_21 + ETA(21)", "$ERROR", "CP    = A(2)*1000 / V", "IPRED = LOG(CP + 0.00001)", "Y     = IPRED + EPS(1) * EXP(ETA(1))", "", ";;;FREM CODE BEGIN COMPACT", ";;;DO NOT MODIFY", "      IF(FREMTYPE.EQ.100) THEN", ";        WT 1", "         Y = COV6 + EPS(2)", "         IPRED = COV6", "      END IF", "      IF(FREMTYPE.EQ.200) THEN", ";        HT 1", "         Y = COV7 + EPS(2)", "         IPRED = COV7", "      END IF", "      IF(FREMTYPE.EQ.300) THEN", ";        LBWT 1", "         Y = COV8 + EPS(2)", "         IPRED = COV8", "      END IF", "      IF(FREMTYPE.EQ.400) THEN", ";        BSA 1", "         Y = COV9 + EPS(2)", "         IPRED = COV9", "      END IF", "      IF(FREMTYPE.EQ.500) THEN", ";        AGE 1", "         Y = COV10 + EPS(2)", "         IPRED = COV10", "      END IF", "      IF(FREMTYPE.EQ.600) THEN", ";        AST 1", "         Y = COV11 + EPS(2)", "         IPRED = COV11", "      END IF", "      IF(FREMTYPE.EQ.700) THEN", ";        ALT 1", "         Y = COV12 + EPS(2)", "         IPRED = COV12", "      END IF", "      IF(FREMTYPE.EQ.800) THEN", ";        BILI 1", "         Y = COV13 + EPS(2)", "         IPRED = COV13", "      END IF", "      IF(FREMTYPE.EQ.900) THEN", ";        CRCL 1", "         Y = COV14 + EPS(2)", "         IPRED = COV14", "      END IF", "      IF(FREMTYPE.EQ.1000) THEN", ";        BMI 1", "         Y = COV15 + EPS(2)", "         IPRED = COV15", "      END IF", "      IF(FREMTYPE.EQ.1100) THEN", ";        SEX 1", "         Y = COV16 + EPS(2)", "         IPRED = COV16", "      END IF", "      IF(FREMTYPE.EQ.1200) THEN", ";        RACEL_3 1", "         Y = COV17 + EPS(2)", "         IPRED = COV17", "      END IF", "      IF(FREMTYPE.EQ.1300) THEN", ";        RACEL_2 1", "         Y = COV18 + EPS(2)", "         IPRED = COV18", "      END IF", "      IF(FREMTYPE.EQ.1400) THEN", ";        GENO2 1", "         Y = COV19 + EPS(2)", "         IPRED = COV19", "      END IF", "      IF(FREMTYPE.EQ.1500) THEN", ";        ETHNIC 1", "         Y = COV20 + EPS(2)", "         IPRED = COV20", "      END IF", "      IF(FREMTYPE.EQ.1600) THEN", ";        SMOK 1", "         Y = COV21 + EPS(2)", "         IPRED = COV21", "      END IF", ";;;FREM CODE END COMPACT", "$THETA 1 FIX ; 1. TVFREL", "$THETA 6.14514 ; 2. TVCL", "$THETA 122.525 ; 3. TVV", "$THETA 1.88694 ; 4. TVMAT", "$THETA 0.670374 ; 5. D1", "$THETA -0.0522225 ; 6. FRELFOOD1", "$THETA 0.121132 ; 7. MATFOOD1", "$THETA 86.8633 ; TV_WT", "$THETA 169.651 ; TV_HT", "$THETA 57.5254 ; TV_LBWT", "$THETA 2.01183 ; TV_BSA", "$THETA 43.8226 ; TV_AGE", "$THETA 25.5516 ; TV_AST", "$THETA 28.5493 ; TV_ALT", "$THETA 9.81901 ; TV_BILI", "$THETA 119.212 ; TV_CRCL", "$THETA 30.0976 ; TV_BMI", "$THETA 1.44109 ; TV_SEX", "$THETA 0.0246596 ; TV_RACEL_3", "$THETA 0.195798 ; TV_RACEL_2", "$THETA 1.8036 ; TV_NCIL_2", "$THETA 0.458201 ; TV_NCIL_1", "$THETA 0.0369864 ; TV_GENO2", "$OMEGA BLOCK(1) 0.0541999 ; 1. IIV on RUV", "$OMEGA BLOCK(1) 1e-04 FIX ; 2. IIV on D1", "$OMEGA BLOCK(19) ", "0.183435  ; 3. IIV on CL", "0.137661 0.196733  ; 4. IIV on V", "0.0136641 0.0282188 0.0473756  ; 5. IIV on MAT", "2.4222 3.92909 0.350015 432.543  ; BSV_WT", "0.127998 0.021 0.100029 104.131 104.095  ; BSV_HT", "0.666724 0.792017 0.195637 181.089 94.8366 123.336  ; BSV_LBWT", "0.0287738 0.0459165 0.00464313 5.59003 1.81187 2.6657 0.0752155  ; BSV_BSA", "0.701637 0.909366 0.0836751 64.2565 -8.6079 16.5304 0.70081 177.113  ; BSV_AGE", "-0.251462 0.17594 -0.281731 25.4064 14.3898 19.9453 0.393441 21.189 125.748  ; BSV_AST", "-0.253527 0.160016 0.104703 51.9152 26.3127 40.2234 0.777756 19.3603 157.022 300.217  ; BSV_ALT", "-0.476132 -0.276369 -0.123245 0.0888749 12.8684 10.09 0.0757738 -12.7251 7.61158 10.7853 27.7596  ; BSV_BILI", "1.6075 2.8213 0.41529 267.257 54.6493 90.4787 3.43366 -144.73 3.33242 26.0449 -10.3118 626.339  ; BSV_CRCL", "0.779549 1.33402 0.0758799 109.832 -1.39477 27.318 1.25871 24.7927 3.68852 8.78061 -4.29812 73.508 38.2588  ; BSV_BMI", "-0.00113323 0.0292476 -0.00522425 -3.90112 -3.25387 -4.23233 -0.0643208 -0.330324 -0.80053 -1.85893 -0.746362 -0.104641 -0.206379 0.246868  ; BSV_SEX", "-0.00520646 -0.00649566 0.000836734 -0.279906 -0.14328 -0.118583 -0.00401741 0.0124454 -0.0306336 0.0290185 0.0573855 -0.151578 -0.0471858 -0.00193126 0.0240089  ; BSV_RACEL_3", "-0.00205181 0.00539926 -0.00233377 0.410641 0.652586 0.344738 0.00818335 -0.947977 -0.250417 -1.2021 0.0407131 -0.90198 -0.0996553 0.00182635 -0.00498082 0.157669  ; BSV_RACEL_2", "0.118593 0.117165 0.00231381 -0.0264943 -0.245131 -0.170493 -0.00169642 0.278638 -0.0668201 -0.209589 -0.132641 -0.126858 0.0789434 0.0095879 -0.00739947 -0.000340582 0.158199  ; BSV_NCIL_2", "-0.00827334 -0.031517 0.000344643 -3.32889 -1.73893 -1.74645 -0.0488599 -0.134246 -0.492962 0.0117242 -0.29347 -1.01561 -0.527158 0.0267216 -0.0112897 -0.0642058 0.0203806 0.248758  ; BSV_NCIL_1", "0.0143385 0.0122878 0.00182188 0.133204 0.0551072 0.000468563 0.00185725 -0.076698 0.0353325 0.0892892 -0.0177387 -0.102328 0.0331443 0.00423904 0.00224461 -0.00310526 2.5645e-05 -0.0100051 0.0356667  ; BSV_GENO2", "$SIGMA  0.0310236  ;     1. RUV", "$SIGMA  1E-07  FIX  ;     EPSCOV", "$ESTIMATION METHOD=IMPMAP AUTO=1 RANMETHOD=3P INTER NOABORT PRINT=1", "            NOCOV=1 ISAMPEND=100 NITER=50 ISAMPLE=300", "$ESTIMATION METHOD=IMPMAP INTER EONLY=1 NITER=50 ISAMPLE=10000 NOCOV=0", "            PHITYPE=1", ";$COVARIANCE  UNCONDITIONAL", "$TABLE      NO ID STUDYID TAD TIME DAY AMT RATE ODV DV EVID BLQ DOSE", "            FOOD FORM TYPE WT HT LBWT BSA SEX RACE AGE AST ALT BILI", "            CRCL BMI NCI GENO2 ETHNIC SMOK RACE NCIL CPRED CIPREDI", "            CWRES CIWRES ETAS(1:LAST) NOPRINT ONEHEADER FILE=xptab31", ""]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["column", "type", "n", "nMissing", "nDistinct", "mean", "sd", "min", "max", "hash"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["ID", "TIME", "AMT", "DV", "EVID", "RATE", "FREMTYPE"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["integer", "numeric", "numeric", "numeric", "integer", "integer", "numeric"]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [44102, 44102, 44102, 44102, 44102, 44102, 44102]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [760, 8034, 5, 5479, 3, 2, 17]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [512.212, 555.381, 2.10353, 8.74845, 0.65115, -1.25836, 193.853]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [265.326, 423.582, 2.55482, 28.8278, 0.561291, 0.966061, 423.345]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 0, 0, -2.263, 0, -2, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [964, 1678.4, 10, 198.5, 4, 0, 1600]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["3768025294-1669779547", "1897979191-3879372652", "4246941371-2868970448", "1479213852-3595513226", "2570883008-1872422609", "4109099252-3686275897", "4041142478-957716439"]
        }
      ]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["ID", "TIME", "AMT", "DV", "EVID", "RATE", "FREMTYPE"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        }
      },
      "value": [
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [5, 0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0, 71.2, 180.6, 58.4254342, 1.88993827, 39, 25, 23.946]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [4, 0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-2, 0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0, 100, 200, 300, 400, 500, 700, 800]
        }
      ]
    }

# updateFREMmodel correctly adds a polycotomous covariate

    {
      "type": "character",
      "attributes": {},
      "value": [";; 1. Based on: 30", ";; 2. Description:", ";;    Final frem30.dir mdoel", ";; 3. Label:", ";;    SimVal base model", ";------------------------------------------------------------------------------", "$PROBLEM    run 1", "$INPUT ID TIME AMT DV EVID RATE FREMTYPE", "$DATA [placeholder_path] IGNORE=@", "$SUBROUTINE ADVAN2 TRANS2", "$PK", ";;; MATFOOD-DEFINITION START", "IF(FOOD.EQ.1) MATFOOD = 1  ; Most common", "IF(FOOD.EQ.0) MATFOOD = ( 1 + THETA(6))", ";;; MATFOOD-DEFINITION END", "", ";;; MAT-RELATION START", "MATCOVTIME = MATFOOD", ";;; MAT-RELATION END", "", "", ";;; FRELFOOD-DEFINITION START", "IF(FOOD.EQ.1) FRELFOOD = 1  ; Most common", "IF(FOOD.EQ.0) FRELFOOD = ( 1 + THETA(7))", ";;; FRELFOOD-DEFINITION END", "", ";;; FREL-RELATION START", "FRELCOVTIME = FRELFOOD", ";;; FREL-RELATION END", "", "TVFREL  = THETA(1)", "TVCL    = THETA(2)", "TVV     = THETA(3)", "TVMAT   = THETA(4)", "TVD1    = THETA(5)", "", ";MU_1  = LOG(TVRUV)", "MU_2  = TVD1", "MU_3  = LOG(TVCL)", "MU_4  = LOG(TVV)", "MU_5  = LOG(TVMAT)", "", "D1FR  = MU_2                   + ETA(2)", "FREL  = TVFREL*FRELCOVTIME", "CL    = EXP(MU_3               + ETA(3))", "V     = EXP(MU_4               + ETA(4))", "MAT   = MATCOVTIME * EXP(MU_5  + ETA(5))", "D1    = MAT*(1-D1FR)", "", "F1    = FREL", "KA    = 1 / (MAT-D1)", "S2    = V", "", "      MU_6 = THETA(8)", "      COV6 = MU_6 + ETA(6)", "      MU_7 = THETA(9)", "      COV7 = MU_7 + ETA(7)", "      MU_8 = THETA(10)", "      COV8 = MU_8 + ETA(8)", "      MU_9 = THETA(11)", "      COV9 = MU_9 + ETA(9)", "      MU_10 = THETA(12)", "      COV10 = MU_10 + ETA(10)", "      MU_11 = THETA(13)", "      COV11 = MU_11 + ETA(11)", "      MU_12 = THETA(14)", "      COV12 = MU_12 + ETA(12)", "      MU_13 = THETA(15)", "      COV13 = MU_13 + ETA(13)", "      MU_14 = THETA(16)", "      COV14 = MU_14 + ETA(14)", "      MU_15 = THETA(17)", "      COV15 = MU_15 + ETA(15)", "      MU_16 = THETA(18)", "      COV16 = MU_16 + ETA(16)", "      MU_17 = THETA(19)", "      COV17 = MU_17 + ETA(17)", "      MU_18 = THETA(20)", "      COV18 = MU_18 + ETA(18)", "      MU_19 = THETA(21)", "      COV19 = MU_19 + ETA(19)", "      MU_20 = THETA(22)", "      COV20 = MU_20 + ETA(20)", "      MU_21 = THETA(23)", "      COV21 = MU_21 + ETA(21)", "      MU_22 = THETA(24)", "      COV22 = MU_22 + ETA(22)", "      MU_23 = THETA(25)", "      COV23 = MU_23 + ETA(23)", "      MU_24 = THETA(26)", "      COV24 = MU_24 + ETA(24)", "      MU_25 = THETA(27)", "      COV25 = MU_25 + ETA(25)", "$ERROR", "CP    = A(2)*1000 / V", "IPRED = LOG(CP + 0.00001)", "Y     = IPRED + EPS(1) * EXP(ETA(1))", "", ";;;FREM CODE BEGIN COMPACT", ";;;DO NOT MODIFY", "      IF(FREMTYPE.EQ.100) THEN", ";        WT 1", "         Y = COV6 + EPS(2)", "         IPRED = COV6", "      END IF", "      IF(FREMTYPE.EQ.200) THEN", ";        HT 1", "         Y = COV7 + EPS(2)", "         IPRED = COV7", "      END IF", "      IF(FREMTYPE.EQ.300) THEN", ";        LBWT 1", "         Y = COV8 + EPS(2)", "         IPRED = COV8", "      END IF", "      IF(FREMTYPE.EQ.400) THEN", ";        BSA 1", "         Y = COV9 + EPS(2)", "         IPRED = COV9", "      END IF", "      IF(FREMTYPE.EQ.500) THEN", ";        AGE 1", "         Y = COV10 + EPS(2)", "         IPRED = COV10", "      END IF", "      IF(FREMTYPE.EQ.600) THEN", ";        AST 1", "         Y = COV11 + EPS(2)", "         IPRED = COV11", "      END IF", "      IF(FREMTYPE.EQ.700) THEN", ";        ALT 1", "         Y = COV12 + EPS(2)", "         IPRED = COV12", "      END IF", "      IF(FREMTYPE.EQ.800) THEN", ";        BILI 1", "         Y = COV13 + EPS(2)", "         IPRED = COV13", "      END IF", "      IF(FREMTYPE.EQ.900) THEN", ";        CRCL 1", "         Y = COV14 + EPS(2)", "         IPRED = COV14", "      END IF", "      IF(FREMTYPE.EQ.1000) THEN", ";        BMI 1", "         Y = COV15 + EPS(2)", "         IPRED = COV15", "      END IF", "      IF(FREMTYPE.EQ.1100) THEN", ";        SEX 1", "         Y = COV16 + EPS(2)", "         IPRED = COV16", "      END IF", "      IF(FREMTYPE.EQ.1200) THEN", ";        RACEL_3 1", "         Y = COV17 + EPS(2)", "         IPRED = COV17", "      END IF", "      IF(FREMTYPE.EQ.1300) THEN", ";        RACEL_2 1", "         Y = COV18 + EPS(2)", "         IPRED = COV18", "      END IF", "      IF(FREMTYPE.EQ.1400) THEN", ";        NCIL_2 1", "         Y = COV19 + EPS(2)", "         IPRED = COV19", "      END IF", "      IF(FREMTYPE.EQ.1500) THEN", ";        NCIL_1 1", "         Y = COV20 + EPS(2)", "         IPRED = COV20", "      END IF", "      IF(FREMTYPE.EQ.1600) THEN", ";        GENO2 1", "         Y = COV21 + EPS(2)", "         IPRED = COV21", "      END IF", "      IF(FREMTYPE.EQ.1700) THEN", ";        ETHNIC 1", "         Y = COV22 + EPS(2)", "         IPRED = COV22", "      END IF", "      IF(FREMTYPE.EQ.1800) THEN", ";        SMOK 1", "         Y = COV23 + EPS(2)", "         IPRED = COV23", "      END IF", "      IF(FREMTYPE.EQ.1900) THEN", ";        SITE_102 1", "         Y = COV24 + EPS(2)", "         IPRED = COV24", "      END IF", "      IF(FREMTYPE.EQ.2000) THEN", ";        SITE_103 1", "         Y = COV25 + EPS(2)", "         IPRED = COV25", "      END IF", ";;;FREM CODE END COMPACT", "$THETA 1 FIX ; 1. TVFREL", "$THETA 6.14514 ; 2. TVCL", "$THETA 122.525 ; 3. TVV", "$THETA 1.88694 ; 4. TVMAT", "$THETA 0.670374 ; 5. D1", "$THETA -0.0522225 ; 6. FRELFOOD1", "$THETA 0.121132 ; 7. MATFOOD1", "$THETA 86.8633 ; TV_WT", "$THETA 169.651 ; TV_HT", "$THETA 57.5254 ; TV_LBWT", "$THETA 2.01183 ; TV_BSA", "$THETA 43.8226 ; TV_AGE", "$THETA 25.5516 ; TV_AST", "$THETA 28.5493 ; TV_ALT", "$THETA 9.81901 ; TV_BILI", "$THETA 119.212 ; TV_CRCL", "$THETA 30.0976 ; TV_BMI", "$THETA 1.44109 ; TV_SEX", "$THETA 0.0246596 ; TV_RACEL_3", "$THETA 0.195798 ; TV_RACEL_2", "$THETA 0.0224549 ; TV_NCIL_2", "$THETA 0.157896 ; TV_NCIL_1", "$THETA 1.8036 ; TV_GENO2", "$THETA 0.458201 ; TV_ETHNIC", "$THETA 0.0369864 ; TV_SMOK", "$THETA 0.25 FIX ; 26 TV_SITE_102", "$THETA 0.24 FIX ; 27 TV_SITE_103", "$OMEGA BLOCK(1) 0.0541999 ; 1. IIV on RUV", "$OMEGA BLOCK(1) 1e-04 FIX ; 2. IIV on D1", "$OMEGA BLOCK(23) ", "0.183435  ; 3. IIV on CL", "0.137661 0.196733  ; 4. IIV on V", "0.0136641 0.0282188 0.0473756  ; 5. IIV on MAT", "2.4222 3.92909 0.350015 432.543  ; BSV_WT", "0.127998 0.021 0.100029 104.131 104.095  ; BSV_HT", "0.666724 0.792017 0.195637 181.089 94.8366 123.336  ; BSV_LBWT", "0.0287738 0.0459165 0.00464313 5.59003 1.81187 2.6657 0.0752155  ; BSV_BSA", "0.701637 0.909366 0.0836751 64.2565 -8.6079 16.5304 0.70081 177.113  ; BSV_AGE", "-0.251462 0.17594 -0.281731 25.4064 14.3898 19.9453 0.393441 21.189 125.748  ; BSV_AST", "-0.253527 0.160016 0.104703 51.9152 26.3127 40.2234 0.777756 19.3603 157.022 300.217  ; BSV_ALT", "-0.476132 -0.276369 -0.123245 0.0888749 12.8684 10.09 0.0757738 -12.7251 7.61158 10.7853 27.7596  ; BSV_BILI", "1.6075 2.8213 0.41529 267.257 54.6493 90.4787 3.43366 -144.73 3.33242 26.0449 -10.3118 626.339  ; BSV_CRCL", "0.779549 1.33402 0.0758799 109.832 -1.39477 27.318 1.25871 24.7927 3.68852 8.78061 -4.29812 73.508 38.2588  ; BSV_BMI", "-0.00113323 0.0292476 -0.00522425 -3.90112 -3.25387 -4.23233 -0.0643208 -0.330324 -0.80053 -1.85893 -0.746362 -0.104641 -0.206379 0.246868  ; BSV_SEX", "-0.00520646 -0.00649566 0.000836734 -0.279906 -0.14328 -0.118583 -0.00401741 0.0124454 -0.0306336 0.0290185 0.0573855 -0.151578 -0.0471858 -0.00193126 0.0240089  ; BSV_RACEL_3", "-0.00205181 0.00539926 -0.00233377 0.410641 0.652586 0.344738 0.00818335 -0.947977 -0.250417 -1.2021 0.0407131 -0.90198 -0.0996553 0.00182635 -0.00498082 0.157669  ; BSV_RACEL_2", "-0.0106033 -0.00342586 -0.002829 -0.0987262 0.0627517 0.0477866 -0.000747647 -0.0325867 0.21578 0.315912 0.472319 -0.359349 -0.0532011 -0.00729448 -0.000442111 0.000264146 0.0225553  ; BSV_NCIL_2", "-0.0288916 -0.0105478 -0.00454967 0.478708 0.718929 0.719799 0.00996643 -0.105498 1.62038 3.47002 0.700085 -0.0195396 -0.086853 -0.0394045 0.00510647 -0.00848174 -0.00363168 0.132082  ; BSV_NCIL_1", "0.118593 0.117165 0.00231381 -0.0264943 -0.245131 -0.170493 -0.00169642 0.278638 -0.0668201 -0.209589 -0.132641 -0.126858 0.0789434 0.0095879 -0.00739947 -0.000340582 -0.000354871 -0.0139211 0.158199  ; BSV_GENO2", "-0.00827334 -0.031517 0.000344643 -3.32889 -1.73893 -1.74645 -0.0488599 -0.134246 -0.492962 0.0117242 -0.29347 -1.01561 -0.527158 0.0267216 -0.0112897 -0.0642058 -0.00402577 -0.0132472 0.0203806 0.248758  ; BSV_ETHNIC", "0.0143385 0.0122878 0.00182188 0.133204 0.0551072 0.000468563 0.00185725 -0.076698 0.0353325 0.0892892 -0.0177387 -0.102328 0.0331443 0.00423904 0.00224461 -0.00310526 -0.000857598 -6.18609e-05 2.5645e-05 -0.0100051 0.0356667  ; BSV_SMOK", "1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 0.187070634582464  ; 24 BSV_SITE_102", "1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 0.184990618435708  ; 25 BSV_SITE_103", "$SIGMA  0.0310236  ;     1. RUV", "$SIGMA  1E-07  FIX  ;     EPSCOV", "$ESTIMATION METHOD=IMPMAP AUTO=1 RANMETHOD=3P INTER NOABORT PRINT=1", "            NOCOV=1 ISAMPEND=100 NITER=50 ISAMPLE=300", "$ESTIMATION METHOD=IMPMAP INTER EONLY=1 NITER=50 ISAMPLE=10000 NOCOV=0", "            PHITYPE=1", ";$COVARIANCE  UNCONDITIONAL", "$TABLE      NO ID STUDYID TAD TIME DAY AMT RATE ODV DV EVID BLQ DOSE", "            FOOD FORM TYPE WT HT LBWT BSA SEX RACE AGE AST ALT BILI", "            CRCL BMI NCI GENO2 ETHNIC SMOK RACE NCIL CPRED CIPREDI", "            CWRES CIWRES ETAS(1:LAST) NOPRINT ONEHEADER FILE=xptab31", ""]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["column", "type", "n", "nMissing", "nDistinct", "mean", "sd", "min", "max", "hash"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["ID", "TIME", "AMT", "DV", "EVID", "RATE", "FREMTYPE"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["integer", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [46960, 46960, 46960, 46960, 46960, 46960, 46960]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [760, 8034, 5, 5479, 3, 2, 21]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [507.801, 534.356, 1.97551, 8.22637, 0.61152, -1.18177, 293.854]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [267.067, 425.76, 2.52642, 28.0121, 0.565781, 0.983351, 569.272]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 0, 0, -2.263, 0, -2, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [964, 1678.4, 10, 198.5, 4, 0, 2000]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["720921462-2562444559", "2194202063-1989636944", "1536758906-3476326085", "765320783-3961880889", "591280384-1811352815", "4206155733-324425476", "3403520590-421700769"]
        }
      ]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["ID", "TIME", "AMT", "DV", "EVID", "RATE", "FREMTYPE"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        }
      },
      "value": [
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [5, 0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0, 71.2, 180.6, 58.4254342, 1.88993827, 39, 25, 23.946]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [4, 0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-2, 0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0, 100, 200, 300, 400, 500, 700, 800]
        }
      ]
    }

# updateFREMmodel correctly adds new individuals

    {
      "type": "character",
      "attributes": {},
      "value": [";; 1. Based on: 30", ";; 2. Description:", ";;    Final frem30.dir mdoel", ";; 3. Label:", ";;    SimVal base model", ";------------------------------------------------------------------------------", "$PROBLEM    run 1", "$INPUT ID TIME AMT DV EVID RATE FREMTYPE", "$DATA [placeholder_path] IGNORE=@", "$SUBROUTINE ADVAN2 TRANS2", "$PK", ";;; MATFOOD-DEFINITION START", "IF(FOOD.EQ.1) MATFOOD = 1  ; Most common", "IF(FOOD.EQ.0) MATFOOD = ( 1 + THETA(6))", ";;; MATFOOD-DEFINITION END", "", ";;; MAT-RELATION START", "MATCOVTIME = MATFOOD", ";;; MAT-RELATION END", "", "", ";;; FRELFOOD-DEFINITION START", "IF(FOOD.EQ.1) FRELFOOD = 1  ; Most common", "IF(FOOD.EQ.0) FRELFOOD = ( 1 + THETA(7))", ";;; FRELFOOD-DEFINITION END", "", ";;; FREL-RELATION START", "FRELCOVTIME = FRELFOOD", ";;; FREL-RELATION END", "", "TVFREL  = THETA(1)", "TVCL    = THETA(2)", "TVV     = THETA(3)", "TVMAT   = THETA(4)", "TVD1    = THETA(5)", "", ";MU_1  = LOG(TVRUV)", "MU_2  = TVD1", "MU_3  = LOG(TVCL)", "MU_4  = LOG(TVV)", "MU_5  = LOG(TVMAT)", "", "D1FR  = MU_2                   + ETA(2)", "FREL  = TVFREL*FRELCOVTIME", "CL    = EXP(MU_3               + ETA(3))", "V     = EXP(MU_4               + ETA(4))", "MAT   = MATCOVTIME * EXP(MU_5  + ETA(5))", "D1    = MAT*(1-D1FR)", "", "F1    = FREL", "KA    = 1 / (MAT-D1)", "S2    = V", "", "      MU_6 = THETA(8)", "      COV6 = MU_6 + ETA(6)", "      MU_7 = THETA(9)", "      COV7 = MU_7 + ETA(7)", "      MU_8 = THETA(10)", "      COV8 = MU_8 + ETA(8)", "      MU_9 = THETA(11)", "      COV9 = MU_9 + ETA(9)", "      MU_10 = THETA(12)", "      COV10 = MU_10 + ETA(10)", "      MU_11 = THETA(13)", "      COV11 = MU_11 + ETA(11)", "      MU_12 = THETA(14)", "      COV12 = MU_12 + ETA(12)", "      MU_13 = THETA(15)", "      COV13 = MU_13 + ETA(13)", "      MU_14 = THETA(16)", "      COV14 = MU_14 + ETA(14)", "      MU_15 = THETA(17)", "      COV15 = MU_15 + ETA(15)", "      MU_16 = THETA(18)", "      COV16 = MU_16 + ETA(16)", "      MU_17 = THETA(19)", "      COV17 = MU_17 + ETA(17)", "      MU_18 = THETA(20)", "      COV18 = MU_18 + ETA(18)", "      MU_19 = THETA(21)", "      COV19 = MU_19 + ETA(19)", "      MU_20 = THETA(22)", "      COV20 = MU_20 + ETA(20)", "      MU_21 = THETA(23)", "      COV21 = MU_21 + ETA(21)", "      MU_22 = THETA(24)", "      COV22 = MU_22 + ETA(22)", "      MU_23 = THETA(25)", "      COV23 = MU_23 + ETA(23)", "$ERROR", "CP    = A(2)*1000 / V", "IPRED = LOG(CP + 0.00001)", "Y     = IPRED + EPS(1) * EXP(ETA(1))", "", ";;;FREM CODE BEGIN COMPACT", ";;;DO NOT MODIFY", "      IF(FREMTYPE.EQ.100) THEN", ";        WT 1", "         Y = COV6 + EPS(2)", "         IPRED = COV6", "      END IF", "      IF(FREMTYPE.EQ.200) THEN", ";        HT 1", "         Y = COV7 + EPS(2)", "         IPRED = COV7", "      END IF", "      IF(FREMTYPE.EQ.300) THEN", ";        LBWT 1", "         Y = COV8 + EPS(2)", "         IPRED = COV8", "      END IF", "      IF(FREMTYPE.EQ.400) THEN", ";        BSA 1", "         Y = COV9 + EPS(2)", "         IPRED = COV9", "      END IF", "      IF(FREMTYPE.EQ.500) THEN", ";        AGE 1", "         Y = COV10 + EPS(2)", "         IPRED = COV10", "      END IF", "      IF(FREMTYPE.EQ.600) THEN", ";        AST 1", "         Y = COV11 + EPS(2)", "         IPRED = COV11", "      END IF", "      IF(FREMTYPE.EQ.700) THEN", ";        ALT 1", "         Y = COV12 + EPS(2)", "         IPRED = COV12", "      END IF", "      IF(FREMTYPE.EQ.800) THEN", ";        BILI 1", "         Y = COV13 + EPS(2)", "         IPRED = COV13", "      END IF", "      IF(FREMTYPE.EQ.900) THEN", ";        CRCL 1", "         Y = COV14 + EPS(2)", "         IPRED = COV14", "      END IF", "      IF(FREMTYPE.EQ.1000) THEN", ";        BMI 1", "         Y = COV15 + EPS(2)", "         IPRED = COV15", "      END IF", "      IF(FREMTYPE.EQ.1100) THEN", ";        SEX 1", "         Y = COV16 + EPS(2)", "         IPRED = COV16", "      END IF", "      IF(FREMTYPE.EQ.1200) THEN", ";        RACEL_3 1", "         Y = COV17 + EPS(2)", "         IPRED = COV17", "      END IF", "      IF(FREMTYPE.EQ.1300) THEN", ";        RACEL_2 1", "         Y = COV18 + EPS(2)", "         IPRED = COV18", "      END IF", "      IF(FREMTYPE.EQ.1400) THEN", ";        NCIL_2 1", "         Y = COV19 + EPS(2)", "         IPRED = COV19", "      END IF", "      IF(FREMTYPE.EQ.1500) THEN", ";        NCIL_1 1", "         Y = COV20 + EPS(2)", "         IPRED = COV20", "      END IF", "      IF(FREMTYPE.EQ.1600) THEN", ";        GENO2 1", "         Y = COV21 + EPS(2)", "         IPRED = COV21", "      END IF", "      IF(FREMTYPE.EQ.1700) THEN", ";        ETHNIC 1", "         Y = COV22 + EPS(2)", "         IPRED = COV22", "      END IF", "      IF(FREMTYPE.EQ.1800) THEN", ";        SMOK 1", "         Y = COV23 + EPS(2)", "         IPRED = COV23", "      END IF", ";;;FREM CODE END COMPACT", "$THETA 1 FIX ; 1. TVFREL", "$THETA 6.14514 ; 2. TVCL", "$THETA 122.525 ; 3. TVV", "$THETA 1.88694 ; 4. TVMAT", "$THETA 0.670374 ; 5. D1", "$THETA -0.0522225 ; 6. FRELFOOD1", "$THETA 0.121132 ; 7. MATFOOD1", "$THETA 86.8633 ; TV_WT", "$THETA 169.651 ; TV_HT", "$THETA 57.5254 ; TV_LBWT", "$THETA 2.01183 ; TV_BSA", "$THETA 43.8226 ; TV_AGE", "$THETA 25.5516 ; TV_AST", "$THETA 28.5493 ; TV_ALT", "$THETA 9.81901 ; TV_BILI", "$THETA 119.212 ; TV_CRCL", "$THETA 30.0976 ; TV_BMI", "$THETA 1.44109 ; TV_SEX", "$THETA 0.0246596 ; TV_RACEL_3", "$THETA 0.195798 ; TV_RACEL_2", "$THETA 0.0224549 ; TV_NCIL_2", "$THETA 0.157896 ; TV_NCIL_1", "$THETA 1.8036 ; TV_GENO2", "$THETA 0.458201 ; TV_ETHNIC", "$THETA 0.0369864 ; TV_SMOK", "$OMEGA BLOCK(1) 0.0541999 ; 1. IIV on RUV", "$OMEGA BLOCK(1) 1e-04 FIX ; 2. IIV on D1", "$OMEGA BLOCK(21) ", "0.183435  ; 3. IIV on CL", "0.137661 0.196733  ; 4. IIV on V", "0.0136641 0.0282188 0.0473756  ; 5. IIV on MAT", "2.4222 3.92909 0.350015 432.543  ; BSV_WT", "0.127998 0.021 0.100029 104.131 104.095  ; BSV_HT", "0.666724 0.792017 0.195637 181.089 94.8366 123.336  ; BSV_LBWT", "0.0287738 0.0459165 0.00464313 5.59003 1.81187 2.6657 0.0752155  ; BSV_BSA", "0.701637 0.909366 0.0836751 64.2565 -8.6079 16.5304 0.70081 177.113  ; BSV_AGE", "-0.251462 0.17594 -0.281731 25.4064 14.3898 19.9453 0.393441 21.189 125.748  ; BSV_AST", "-0.253527 0.160016 0.104703 51.9152 26.3127 40.2234 0.777756 19.3603 157.022 300.217  ; BSV_ALT", "-0.476132 -0.276369 -0.123245 0.0888749 12.8684 10.09 0.0757738 -12.7251 7.61158 10.7853 27.7596  ; BSV_BILI", "1.6075 2.8213 0.41529 267.257 54.6493 90.4787 3.43366 -144.73 3.33242 26.0449 -10.3118 626.339  ; BSV_CRCL", "0.779549 1.33402 0.0758799 109.832 -1.39477 27.318 1.25871 24.7927 3.68852 8.78061 -4.29812 73.508 38.2588  ; BSV_BMI", "-0.00113323 0.0292476 -0.00522425 -3.90112 -3.25387 -4.23233 -0.0643208 -0.330324 -0.80053 -1.85893 -0.746362 -0.104641 -0.206379 0.246868  ; BSV_SEX", "-0.00520646 -0.00649566 0.000836734 -0.279906 -0.14328 -0.118583 -0.00401741 0.0124454 -0.0306336 0.0290185 0.0573855 -0.151578 -0.0471858 -0.00193126 0.0240089  ; BSV_RACEL_3", "-0.00205181 0.00539926 -0.00233377 0.410641 0.652586 0.344738 0.00818335 -0.947977 -0.250417 -1.2021 0.0407131 -0.90198 -0.0996553 0.00182635 -0.00498082 0.157669  ; BSV_RACEL_2", "-0.0106033 -0.00342586 -0.002829 -0.0987262 0.0627517 0.0477866 -0.000747647 -0.0325867 0.21578 0.315912 0.472319 -0.359349 -0.0532011 -0.00729448 -0.000442111 0.000264146 0.0225553  ; BSV_NCIL_2", "-0.0288916 -0.0105478 -0.00454967 0.478708 0.718929 0.719799 0.00996643 -0.105498 1.62038 3.47002 0.700085 -0.0195396 -0.086853 -0.0394045 0.00510647 -0.00848174 -0.00363168 0.132082  ; BSV_NCIL_1", "0.118593 0.117165 0.00231381 -0.0264943 -0.245131 -0.170493 -0.00169642 0.278638 -0.0668201 -0.209589 -0.132641 -0.126858 0.0789434 0.0095879 -0.00739947 -0.000340582 -0.000354871 -0.0139211 0.158199  ; BSV_GENO2", "-0.00827334 -0.031517 0.000344643 -3.32889 -1.73893 -1.74645 -0.0488599 -0.134246 -0.492962 0.0117242 -0.29347 -1.01561 -0.527158 0.0267216 -0.0112897 -0.0642058 -0.00402577 -0.0132472 0.0203806 0.248758  ; BSV_ETHNIC", "0.0143385 0.0122878 0.00182188 0.133204 0.0551072 0.000468563 0.00185725 -0.076698 0.0353325 0.0892892 -0.0177387 -0.102328 0.0331443 0.00423904 0.00224461 -0.00310526 -0.000857598 -6.18609e-05 2.5645e-05 -0.0100051 0.0356667  ; BSV_SMOK", "$SIGMA  0.0310236  ;     1. RUV", "$SIGMA  1E-07  FIX  ;     EPSCOV", "$ESTIMATION METHOD=IMPMAP AUTO=1 RANMETHOD=3P INTER NOABORT PRINT=1", "            NOCOV=1 ISAMPEND=100 NITER=50 ISAMPLE=300", "$ESTIMATION METHOD=IMPMAP INTER EONLY=1 NITER=50 ISAMPLE=10000 NOCOV=0", "            PHITYPE=1", ";$COVARIANCE  UNCONDITIONAL", "$TABLE      NO ID STUDYID TAD TIME DAY AMT RATE ODV DV EVID BLQ DOSE", "            FOOD FORM TYPE WT HT LBWT BSA SEX RACE AGE AST ALT BILI", "            CRCL BMI NCI GENO2 ETHNIC SMOK RACE NCIL CPRED CIPREDI", "            CWRES CIWRES ETAS(1:LAST) NOPRINT ONEHEADER FILE=xptab31", ""]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["column", "type", "n", "nMissing", "nDistinct", "mean", "sd", "min", "max", "hash"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["ID", "TIME", "AMT", "DV", "EVID", "RATE", "FREMTYPE"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["integer", "numeric", "numeric", "numeric", "integer", "integer", "numeric"]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [45722, 45722, 45722, 45722, 45722, 45722, 45722]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [768, 8034, 5, 5574, 3, 2, 19]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [512.336, 548.86, 2.04366, 8.58301, 0.639801, -1.21963, 241.851]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [267.73, 422.138, 2.54178, 28.5279, 0.591951, 0.975594, 494.097]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 0, 0, -2.263, 0, -2, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1019, 1678.4, 10, 198.5, 4, 0, 1800]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["3864137917-954630602", "1779415065-2420314336", "2232558881-3107965161", "2895266432-3880987293", "2521265630-4169089968", "3939530692-662952261", "3251316944-3132747178"]
        }
      ]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["ID", "TIME", "AMT", "DV", "EVID", "RATE", "FREMTYPE"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        }
      },
      "value": [
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [5, 0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0, 71.2, 180.6, 58.4254342, 1.88993827, 39, 25, 23.946]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [4, 0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-2, 0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0, 100, 200, 300, 400, 500, 700, 800]
        }
      ]
    }

# updateFREMmodel correctly adds a new DV type

    {
      "type": "character",
      "attributes": {},
      "value": [";; 1. Based on: 30", ";; 2. Description:", ";;    Final frem30.dir mdoel", ";; 3. Label:", ";;    SimVal base model", ";------------------------------------------------------------------------------", "$PROBLEM    run 1", "$INPUT ID TIME AMT DV EVID RATE FREMTYPE", "$DATA [placeholder_path] IGNORE=@", "$SUBROUTINE ADVAN2 TRANS2", "$PK", ";;; MATFOOD-DEFINITION START", "IF(FOOD.EQ.1) MATFOOD = 1  ; Most common", "IF(FOOD.EQ.0) MATFOOD = ( 1 + THETA(6))", ";;; MATFOOD-DEFINITION END", "", ";;; MAT-RELATION START", "MATCOVTIME = MATFOOD", ";;; MAT-RELATION END", "", "", ";;; FRELFOOD-DEFINITION START", "IF(FOOD.EQ.1) FRELFOOD = 1  ; Most common", "IF(FOOD.EQ.0) FRELFOOD = ( 1 + THETA(7))", ";;; FRELFOOD-DEFINITION END", "", ";;; FREL-RELATION START", "FRELCOVTIME = FRELFOOD", ";;; FREL-RELATION END", "", "TVFREL  = THETA(1)", "TVCL    = THETA(2)", "TVV     = THETA(3)", "TVMAT   = THETA(4)", "TVD1    = THETA(5)", "", ";MU_1  = LOG(TVRUV)", "MU_2  = TVD1", "MU_3  = LOG(TVCL)", "MU_4  = LOG(TVV)", "MU_5  = LOG(TVMAT)", "", "D1FR  = MU_2                   + ETA(2)", "FREL  = TVFREL*FRELCOVTIME", "CL    = EXP(MU_3               + ETA(3))", "V     = EXP(MU_4               + ETA(4))", "MAT   = MATCOVTIME * EXP(MU_5  + ETA(5))", "D1    = MAT*(1-D1FR)", "", "F1    = FREL", "KA    = 1 / (MAT-D1)", "S2    = V", "", "      MU_6 = THETA(8)", "      COV6 = MU_6 + ETA(6)", "      MU_7 = THETA(9)", "      COV7 = MU_7 + ETA(7)", "      MU_8 = THETA(10)", "      COV8 = MU_8 + ETA(8)", "      MU_9 = THETA(11)", "      COV9 = MU_9 + ETA(9)", "      MU_10 = THETA(12)", "      COV10 = MU_10 + ETA(10)", "      MU_11 = THETA(13)", "      COV11 = MU_11 + ETA(11)", "      MU_12 = THETA(14)", "      COV12 = MU_12 + ETA(12)", "      MU_13 = THETA(15)", "      COV13 = MU_13 + ETA(13)", "      MU_14 = THETA(16)", "      COV14 = MU_14 + ETA(14)", "      MU_15 = THETA(17)", "      COV15 = MU_15 + ETA(15)", "      MU_16 = THETA(18)", "      COV16 = MU_16 + ETA(16)", "      MU_17 = THETA(19)", "      COV17 = MU_17 + ETA(17)", "      MU_18 = THETA(20)", "      COV18 = MU_18 + ETA(18)", "      MU_19 = THETA(21)", "      COV19 = MU_19 + ETA(19)", "      MU_20 = THETA(22)", "      COV20 = MU_20 + ETA(20)", "      MU_21 = THETA(23)", "      COV21 = MU_21 + ETA(21)", "      MU_22 = THETA(24)", "      COV22 = MU_22 + ETA(22)", "      MU_23 = THETA(25)", "      COV23 = MU_23 + ETA(23)", "$ERROR", "CP    = A(2)*1000 / V", "IPRED = LOG(CP + 0.00001)", "Y     = IPRED + EPS(1) * EXP(ETA(1))", "", ";;;FREM CODE BEGIN COMPACT", ";;;DO NOT MODIFY", "      IF(FREMTYPE.EQ.100) THEN", ";        WT 1", "         Y = COV6 + EPS(2)", "         IPRED = COV6", "      END IF", "      IF(FREMTYPE.EQ.200) THEN", ";        HT 1", "         Y = COV7 + EPS(2)", "         IPRED = COV7", "      END IF", "      IF(FREMTYPE.EQ.300) THEN", ";        LBWT 1", "         Y = COV8 + EPS(2)", "         IPRED = COV8", "      END IF", "      IF(FREMTYPE.EQ.400) THEN", ";        BSA 1", "         Y = COV9 + EPS(2)", "         IPRED = COV9", "      END IF", "      IF(FREMTYPE.EQ.500) THEN", ";        AGE 1", "         Y = COV10 + EPS(2)", "         IPRED = COV10", "      END IF", "      IF(FREMTYPE.EQ.600) THEN", ";        AST 1", "         Y = COV11 + EPS(2)", "         IPRED = COV11", "      END IF", "      IF(FREMTYPE.EQ.700) THEN", ";        ALT 1", "         Y = COV12 + EPS(2)", "         IPRED = COV12", "      END IF", "      IF(FREMTYPE.EQ.800) THEN", ";        BILI 1", "         Y = COV13 + EPS(2)", "         IPRED = COV13", "      END IF", "      IF(FREMTYPE.EQ.900) THEN", ";        CRCL 1", "         Y = COV14 + EPS(2)", "         IPRED = COV14", "      END IF", "      IF(FREMTYPE.EQ.1000) THEN", ";        BMI 1", "         Y = COV15 + EPS(2)", "         IPRED = COV15", "      END IF", "      IF(FREMTYPE.EQ.1100) THEN", ";        SEX 1", "         Y = COV16 + EPS(2)", "         IPRED = COV16", "      END IF", "      IF(FREMTYPE.EQ.1200) THEN", ";        RACEL_3 1", "         Y = COV17 + EPS(2)", "         IPRED = COV17", "      END IF", "      IF(FREMTYPE.EQ.1300) THEN", ";        RACEL_2 1", "         Y = COV18 + EPS(2)", "         IPRED = COV18", "      END IF", "      IF(FREMTYPE.EQ.1400) THEN", ";        NCIL_2 1", "         Y = COV19 + EPS(2)", "         IPRED = COV19", "      END IF", "      IF(FREMTYPE.EQ.1500) THEN", ";        NCIL_1 1", "         Y = COV20 + EPS(2)", "         IPRED = COV20", "      END IF", "      IF(FREMTYPE.EQ.1600) THEN", ";        GENO2 1", "         Y = COV21 + EPS(2)", "         IPRED = COV21", "      END IF", "      IF(FREMTYPE.EQ.1700) THEN", ";        ETHNIC 1", "         Y = COV22 + EPS(2)", "         IPRED = COV22", "      END IF", "      IF(FREMTYPE.EQ.1800) THEN", ";        SMOK 1", "         Y = COV23 + EPS(2)", "         IPRED = COV23", "      END IF", ";;;FREM CODE END COMPACT", "$THETA 1 FIX ; 1. TVFREL", "$THETA 6.14514 ; 2. TVCL", "$THETA 122.525 ; 3. TVV", "$THETA 1.88694 ; 4. TVMAT", "$THETA 0.670374 ; 5. D1", "$THETA -0.0522225 ; 6. FRELFOOD1", "$THETA 0.121132 ; 7. MATFOOD1", "$THETA 86.8633 ; TV_WT", "$THETA 169.651 ; TV_HT", "$THETA 57.5254 ; TV_LBWT", "$THETA 2.01183 ; TV_BSA", "$THETA 43.8226 ; TV_AGE", "$THETA 25.5516 ; TV_AST", "$THETA 28.5493 ; TV_ALT", "$THETA 9.81901 ; TV_BILI", "$THETA 119.212 ; TV_CRCL", "$THETA 30.0976 ; TV_BMI", "$THETA 1.44109 ; TV_SEX", "$THETA 0.0246596 ; TV_RACEL_3", "$THETA 0.195798 ; TV_RACEL_2", "$THETA 0.0224549 ; TV_NCIL_2", "$THETA 0.157896 ; TV_NCIL_1", "$THETA 1.8036 ; TV_GENO2", "$THETA 0.458201 ; TV_ETHNIC", "$THETA 0.0369864 ; TV_SMOK", "$OMEGA BLOCK(1) 0.0541999 ; 1. IIV on RUV", "$OMEGA BLOCK(1) 1e-04 FIX ; 2. IIV on D1", "$OMEGA BLOCK(21) ", "0.183435  ; 3. IIV on CL", "0.137661 0.196733  ; 4. IIV on V", "0.0136641 0.0282188 0.0473756  ; 5. IIV on MAT", "2.4222 3.92909 0.350015 432.543  ; BSV_WT", "0.127998 0.021 0.100029 104.131 104.095  ; BSV_HT", "0.666724 0.792017 0.195637 181.089 94.8366 123.336  ; BSV_LBWT", "0.0287738 0.0459165 0.00464313 5.59003 1.81187 2.6657 0.0752155  ; BSV_BSA", "0.701637 0.909366 0.0836751 64.2565 -8.6079 16.5304 0.70081 177.113  ; BSV_AGE", "-0.251462 0.17594 -0.281731 25.4064 14.3898 19.9453 0.393441 21.189 125.748  ; BSV_AST", "-0.253527 0.160016 0.104703 51.9152 26.3127 40.2234 0.777756 19.3603 157.022 300.217  ; BSV_ALT", "-0.476132 -0.276369 -0.123245 0.0888749 12.8684 10.09 0.0757738 -12.7251 7.61158 10.7853 27.7596  ; BSV_BILI", "1.6075 2.8213 0.41529 267.257 54.6493 90.4787 3.43366 -144.73 3.33242 26.0449 -10.3118 626.339  ; BSV_CRCL", "0.779549 1.33402 0.0758799 109.832 -1.39477 27.318 1.25871 24.7927 3.68852 8.78061 -4.29812 73.508 38.2588  ; BSV_BMI", "-0.00113323 0.0292476 -0.00522425 -3.90112 -3.25387 -4.23233 -0.0643208 -0.330324 -0.80053 -1.85893 -0.746362 -0.104641 -0.206379 0.246868  ; BSV_SEX", "-0.00520646 -0.00649566 0.000836734 -0.279906 -0.14328 -0.118583 -0.00401741 0.0124454 -0.0306336 0.0290185 0.0573855 -0.151578 -0.0471858 -0.00193126 0.0240089  ; BSV_RACEL_3", "-0.00205181 0.00539926 -0.00233377 0.410641 0.652586 0.344738 0.00818335 -0.947977 -0.250417 -1.2021 0.0407131 -0.90198 -0.0996553 0.00182635 -0.00498082 0.157669  ; BSV_RACEL_2", "-0.0106033 -0.00342586 -0.002829 -0.0987262 0.0627517 0.0477866 -0.000747647 -0.0325867 0.21578 0.315912 0.472319 -0.359349 -0.0532011 -0.00729448 -0.000442111 0.000264146 0.0225553  ; BSV_NCIL_2", "-0.0288916 -0.0105478 -0.00454967 0.478708 0.718929 0.719799 0.00996643 -0.105498 1.62038 3.47002 0.700085 -0.0195396 -0.086853 -0.0394045 0.00510647 -0.00848174 -0.00363168 0.132082  ; BSV_NCIL_1", "0.118593 0.117165 0.00231381 -0.0264943 -0.245131 -0.170493 -0.00169642 0.278638 -0.0668201 -0.209589 -0.132641 -0.126858 0.0789434 0.0095879 -0.00739947 -0.000340582 -0.000354871 -0.0139211 0.158199  ; BSV_GENO2", "-0.00827334 -0.031517 0.000344643 -3.32889 -1.73893 -1.74645 -0.0488599 -0.134246 -0.492962 0.0117242 -0.29347 -1.01561 -0.527158 0.0267216 -0.0112897 -0.0642058 -0.00402577 -0.0132472 0.0203806 0.248758  ; BSV_ETHNIC", "0.0143385 0.0122878 0.00182188 0.133204 0.0551072 0.000468563 0.00185725 -0.076698 0.0353325 0.0892892 -0.0177387 -0.102328 0.0331443 0.00423904 0.00224461 -0.00310526 -0.000857598 -6.18609e-05 2.5645e-05 -0.0100051 0.0356667  ; BSV_SMOK", "$SIGMA  0.0310236  ;     1. RUV", "$SIGMA  1E-07  FIX  ;     EPSCOV", "$ESTIMATION METHOD=IMPMAP AUTO=1 RANMETHOD=3P INTER NOABORT PRINT=1", "            NOCOV=1 ISAMPEND=100 NITER=50 ISAMPLE=300", "$ESTIMATION METHOD=IMPMAP INTER EONLY=1 NITER=50 ISAMPLE=10000 NOCOV=0", "            PHITYPE=1", ";$COVARIANCE  UNCONDITIONAL", "$TABLE      NO ID STUDYID TAD TIME DAY AMT RATE ODV DV EVID BLQ DOSE", "            FOOD FORM TYPE WT HT LBWT BSA SEX RACE AGE AST ALT BILI", "            CRCL BMI NCI GENO2 ETHNIC SMOK RACE NCIL CPRED CIPREDI", "            CWRES CIWRES ETAS(1:LAST) NOPRINT ONEHEADER FILE=xptab31", ""]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["column", "type", "n", "nMissing", "nDistinct", "mean", "sd", "min", "max", "hash"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["ID", "TIME", "AMT", "DV", "EVID", "RATE", "FREMTYPE"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["integer", "numeric", "numeric", "numeric", "integer", "integer", "integer"]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [79150, 79150, 79150, 79150, 79150, 79150, 79150]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [760, 8035, 5, 11380, 3, 2, 20]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [520.037, 571.059, 2.33001, 0.323654, 0.719634, -1.3903, 138.702]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [262.247, 430.925, 2.59415, 23.9191, 0.547454, 0.920695, 392.282]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 0, 0, -13.8155, 0, -2, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [964, 1678.4, 10, 198.5, 4, 0, 1800]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["3647245290-1043890027", "3666449135-3637034598", "3264124857-911313318", "4031537763-928203186", "3296567457-3951632243", "3692879802-4289424131", "1120422368-4075075761"]
        }
      ]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["ID", "TIME", "AMT", "DV", "EVID", "RATE", "FREMTYPE"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        }
      },
      "value": [
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [5, 0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0, 71.2, 180.6, 58.4254342, 1.88993827, 39, 25, 23.946]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [4, 0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-2, 0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0, 100, 200, 300, 400, 500, 700, 800]
        }
      ]
    }

