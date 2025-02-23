;; 1. Based on: 30
;; 2. Description:
;;    Final frem30.dir mdoel
;; 3. Label:
;;    SimVal base model
;------------------------------------------------------------------------------
$PROBLEM    run 1
$INPUT ID TIME AMT EVID RATE DV FOOD FREMTYPE
$DATA frem_dataset_noSEX.csv IGNORE=@
$SUBROUTINE ADVAN2 TRANS2
$PK
;;; MATFOOD-DEFINITION START
IF(FOOD.EQ.1) MATFOOD = 1  ; Most common
IF(FOOD.EQ.0) MATFOOD = ( 1 + THETA(6))
;;; MATFOOD-DEFINITION END

;;; MAT-RELATION START
MATCOVTIME = MATFOOD
;;; MAT-RELATION END


;;; FRELFOOD-DEFINITION START
IF(FOOD.EQ.1) FRELFOOD = 1  ; Most common
IF(FOOD.EQ.0) FRELFOOD = ( 1 + THETA(7))
;;; FRELFOOD-DEFINITION END

;;; FREL-RELATION START
FRELCOVTIME = FRELFOOD
;;; FREL-RELATION END

TVFREL  = THETA(1)
TVCL    = THETA(2)
TVV     = THETA(3)
TVMAT   = THETA(4)
TVD1    = THETA(5)

;MU_1  = LOG(TVRUV)
MU_2  = TVD1
MU_3  = LOG(TVCL)
MU_4  = LOG(TVV)
MU_5  = LOG(TVMAT)

D1FR  = MU_2                   + ETA(2)
FREL  = TVFREL*FRELCOVTIME
CL    = EXP(MU_3               + ETA(3))
V     = EXP(MU_4               + ETA(4))
MAT   = MATCOVTIME * EXP(MU_5  + ETA(5))
D1    = MAT*(1-D1FR)

F1    = FREL
KA    = 1 / (MAT-D1)
S2    = V


      MU_6 = THETA(8)
      COV6 = MU_6 + ETA(6)
      MU_7 = THETA(9)
      COV7 = MU_7 + ETA(7)
      MU_8 = THETA(10)
      COV8 = MU_8 + ETA(8)
      MU_9 = THETA(11)
      COV9 = MU_9 + ETA(9)
      MU_10 = THETA(12)
      COV10 = MU_10 + ETA(10)
      MU_11 = THETA(13)
      COV11 = MU_11 + ETA(11)
      MU_12 = THETA(14)
      COV12 = MU_12 + ETA(12)
      MU_13 = THETA(15)
      COV13 = MU_13 + ETA(13)
      MU_14 = THETA(16)
      COV14 = MU_14 + ETA(14)
      MU_15 = THETA(17)
      COV15 = MU_15 + ETA(15)
      MU_16 = THETA(18)
      COV16 = MU_16 + ETA(16)
      MU_17 = THETA(19)
      COV17 = MU_17 + ETA(17)
      MU_18 = THETA(20)
      COV18 = MU_18 + ETA(18)
      MU_19 = THETA(21)
      COV19 = MU_19 + ETA(19)
      MU_20 = THETA(22)
      COV20 = MU_20 + ETA(20)
      MU_21 = THETA(23)
      COV21 = MU_21 + ETA(21)
$ERROR
CP    = A(2)*1000 / V
IPRED = LOG(CP + 0.00001)
Y     = IPRED + EPS(1) * EXP(ETA(1))


;;;FREM CODE BEGIN COMPACT
      IF(FREMTYPE.EQ.100) THEN
;       HT
        Y = COV6 + EPS(2)
        IPRED = COV6
      ENDIF
      IF(FREMTYPE.EQ.200) THEN
;       LBWT
        Y = COV7 + EPS(2)
        IPRED = COV7
      ENDIF
      IF(FREMTYPE.EQ.300) THEN
;       BSA
        Y = COV8 + EPS(2)
        IPRED = COV8
      ENDIF
      IF(FREMTYPE.EQ.400) THEN
;       AGE
        Y = COV9 + EPS(2)
        IPRED = COV9
      ENDIF
      IF(FREMTYPE.EQ.500) THEN
;       AST
        Y = COV10 + EPS(2)
        IPRED = COV10
      ENDIF
      IF(FREMTYPE.EQ.600) THEN
;       ALT
        Y = COV11 + EPS(2)
        IPRED = COV11
      ENDIF
      IF(FREMTYPE.EQ.700) THEN
;       BILI
        Y = COV12 + EPS(2)
        IPRED = COV12
      ENDIF
      IF(FREMTYPE.EQ.800) THEN
;       CRCL
        Y = COV13 + EPS(2)
        IPRED = COV13
      ENDIF
      IF(FREMTYPE.EQ.900) THEN
;       BMI
        Y = COV14 + EPS(2)
        IPRED = COV14
      ENDIF
      IF(FREMTYPE.EQ.1000) THEN
;       RACEL_3
        Y = COV15 + EPS(2)
        IPRED = COV15
      ENDIF
      IF(FREMTYPE.EQ.1100) THEN
;       RACEL_2
        Y = COV16 + EPS(2)
        IPRED = COV16
      ENDIF
      IF(FREMTYPE.EQ.1200) THEN
;       NCIL_2
        Y = COV17 + EPS(2)
        IPRED = COV17
      ENDIF
      IF(FREMTYPE.EQ.1300) THEN
;       NCIL_1
        Y = COV18 + EPS(2)
        IPRED = COV18
      ENDIF
      IF(FREMTYPE.EQ.1400) THEN
;       GENO2
        Y = COV19 + EPS(2)
        IPRED = COV19
      ENDIF
      IF(FREMTYPE.EQ.1500) THEN
;       ETHNIC
        Y = COV20 + EPS(2)
        IPRED = COV20
      ENDIF
      IF(FREMTYPE.EQ.1600) THEN
;       SMOK
        Y = COV21 + EPS(2)
        IPRED = COV21
      ENDIF
;;;FREM CODE END COMPACT

$THETA 1 FIX  ; 1 TV_BASE1
$THETA 6.14514  ; 2 TV_BASE2
$THETA 122.525  ; 3 TV_BASE3
$THETA 1.88694  ; 4 TV_BASE4
$THETA 0.670374  ; 5 TV_BASE5
$THETA -0.0522225  ; 6 TV_BASE6
$THETA 0.121132  ; 7 TV_BASE7
$THETA 169.651  ; 8 TV_HT
$THETA 57.5254  ; 9 TV_LBWT
$THETA 2.01183  ; 10 TV_BSA
$THETA 43.8226  ; 11 TV_AGE
$THETA 25.5516  ; 12 TV_AST
$THETA 28.5493  ; 13 TV_ALT
$THETA 9.81901  ; 14 TV_BILI
$THETA 119.212  ; 15 TV_CRCL
$THETA 30.0976  ; 16 TV_BMI
$THETA 0.0246596  ; 17 TV_RACEL_3
$THETA 0.195798  ; 18 TV_RACEL_2
$THETA 0.0224549  ; 19 TV_NCIL_2
$THETA 0.157896  ; 20 TV_NCIL_1
$THETA 1.8036  ; 21 TV_GENO2
$THETA 0.458201  ; 22 TV_ETHNIC
$THETA 0.0369864  ; 23 TV_SMOK
$OMEGA BLOCK(1) 
0.0541999 
$OMEGA BLOCK(1) 
1e-04 
$OMEGA BLOCK(19) 
0.183435   ; 3 BSV_BASE3
0.137661 0.196733   ; 4 BSV_BASE4
0.0136641 0.0282188 0.0473756   ; 5 BSV_BASE5
0.127998 0.021 0.100029 104.095   ; 6 BSV_HT
0.666724 0.792017 0.195637 94.8366 123.336   ; 7 BSV_LBWT
0.0287738 0.0459165 0.00464313 1.81187 2.6657 0.0752155   ; 8 BSV_BSA
0.701637 0.909366 0.0836751 -8.6079 16.5304 0.70081 177.113   ; 9 BSV_AGE
-0.251462 0.17594 -0.281731 14.3898 19.9453 0.393441 21.189 125.748   ; 10 BSV_AST
-0.253527 0.160016 0.104703 26.3127 40.2234 0.777756 19.3603 157.022 300.217   ; 11 BSV_ALT
-0.476132 -0.276369 -0.123245 12.8684 10.09 0.0757738 -12.7251 7.61158 10.7853 27.7596   ; 12 BSV_BILI
1.6075 2.8213 0.41529 54.6493 90.4787 3.43366 -144.73 3.33242 26.0449 -10.3118 626.339   ; 13 BSV_CRCL
0.779549 1.33402 0.0758799 -1.39477 27.318 1.25871 24.7927 3.68852 8.78061 -4.29812 73.508 38.2588   ; 14 BSV_BMI
-0.00520646 -0.00649566 0.000836734 -0.14328 -0.118583 -0.00401741 0.0124454 -0.0306336 0.0290185 0.0573855 -0.151578 -0.0471858 0.0240089   ; 15 BSV_RACEL_3
-0.00205181 0.00539926 -0.00233377 0.652586 0.344738 0.00818335 -0.947977 -0.250417 -1.2021 0.0407131 -0.90198 -0.0996553 -0.00498082 0.157669   ; 16 BSV_RACEL_2
-0.0106033 -0.00342586 -0.002829 0.0627517 0.0477866 -0.000747647 -0.0325867 0.21578 0.315912 0.472319 -0.359349 -0.0532011 -0.000442111 0.000264146 0.0225553   ; 17 BSV_NCIL_2
-0.0288916 -0.0105478 -0.00454967 0.718929 0.719799 0.00996643 -0.105498 1.62038 3.47002 0.700085 -0.0195396 -0.086853 0.00510647 -0.00848174 -0.00363168 0.132082   ; 18 BSV_NCIL_1
0.118593 0.117165 0.00231381 -0.245131 -0.170493 -0.00169642 0.278638 -0.0668201 -0.209589 -0.132641 -0.126858 0.0789434 -0.00739947 -0.000340582 -0.000354871 -0.0139211 0.158199   ; 19 BSV_GENO2
-0.00827334 -0.031517 0.000344643 -1.73893 -1.74645 -0.0488599 -0.134246 -0.492962 0.0117242 -0.29347 -1.01561 -0.527158 -0.0112897 -0.0642058 -0.00402577 -0.0132472 0.0203806 0.248758   ; 20 BSV_ETHNIC
0.0143385 0.0122878 0.00182188 0.0551072 0.000468563 0.00185725 -0.076698 0.0353325 0.0892892 -0.0177387 -0.102328 0.0331443 0.00224461 -0.00310526 -0.000857598 -6.18609e-05 2.5645e-05 -0.0100051 0.0356667   ; 21 BSV_SMOK
$SIGMA  0.0310236  ;     1. RUV
$SIGMA  1E-07  FIX  ;     EPSCOV
$ESTIMATION METHOD=IMPMAP AUTO=1 RANMETHOD=3P INTER NOABORT PRINT=1
            NOCOV=1 ISAMPEND=100 NITER=50 ISAMPLE=300
$ESTIMATION METHOD=IMPMAP INTER EONLY=1 NITER=50 ISAMPLE=10000 NOCOV=0
            PHITYPE=1
;$COVARIANCE  UNCONDITIONAL
$TABLE      NO ID STUDYID TAD TIME DAY AMT RATE ODV DV EVID BLQ DOSE
            FOOD FORM TYPE WT HT LBWT BSA SEX RACE AGE AST ALT BILI
            CRCL BMI NCI GENO2 ETHNIC SMOK RACE NCIL CPRED CIPREDI
            CWRES CIWRES ETAS(1:LAST) NOPRINT ONEHEADER FILE=xptab31

