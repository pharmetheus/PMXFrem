;; 1. Based on: run202
;; 2. Description:
;;    FREM base model: no covariates except food on MAT, which varies within subjects and is kept
;;    outside the MU expression; IMPMAP; OMEGA BLOCK(3) for CL, V and MAT
;; 3. Label:
;;    FREM base model, studies 7 and 3
;------------------------------------------------------------------------------
$PROBLEM    run210
$INPUT      ID STUDYID TAD TIME OCC AMT RATE SS II EVID ODV DV DOSE FOOD WT HT AGE RACE BMI CRCL NCI
            SEX ETHNIC SMOK GENO FORM AST ALT BILI NCIORG RACEORG LNWT
$DATA       dataset3-r2.csv IGNORE=@
$SUBROUTINE ADVAN2 TRANS2
$PK

IF(FOOD.EQ.1) MATFOOD = 1  ; Most common
IF(FOOD.EQ.0) MATFOOD = ( 1 + THETA(5))

TVCL    = THETA(1)
TVV     = THETA(2)
TVMAT   = THETA(3)
TVD1    = THETA(4)

MU_2  = TVD1
MU_3  = LOG(TVCL)
MU_4  = LOG(TVV)
MU_5  = LOG(TVMAT)

FRD1  = MU_2     + ETA(2)
CL    = EXP(MU_3 + ETA(3))
V     = EXP(MU_4 + ETA(4))

;; MATFOOD is kept outside the MU expression since FOOD varies with time
MAT   = MATFOOD * EXP(MU_5 + ETA(5))

D1    = MAT*(1-TVD1)

KA    = 1 / (MAT-D1)
S2    = V

$ERROR
CP    = A(2)*1000 / V
IPRED = LOG(CP + 0.00001)
Y     = IPRED + EPS(1) * EXP(ETA(1))

$THETA  (0,6.45)        ; 1. TVCL
$THETA  (0,132)         ; 2. TVV
$THETA  (0,1.97)        ; 3. TVMAT
$THETA  (0,0.395,1)     ; 4. fr D1
$THETA  (-1,-0.128,3)   ; 5. MATFOOD
$OMEGA  0.238           ; 1. IIV on RUV
$OMEGA  0.0001  FIX     ; 2. FRD1
$OMEGA  BLOCK(3)
 0.213                  ; 3. IIV on CL
 0.103 0.194            ; 4. IIV on V
 0.00245 0.0388 0.0641  ; 5. IIV on MAT
$SIGMA  0.0328          ; 1. RUV
$ESTIMATION METHOD=IMPMAP AUTO=1 RANMETHOD=3P INTER NOABORT PRINT=1
            NOCOV=1 ISAMPEND=100 NITER=200 ISAMPLE=300 CTYPE=1 CITER=15
            CALPHA=0.001
$ESTIMATION METHOD=IMPMAP INTER EONLY=1 NITER=10 ISAMPLE=1000 NOCOV=0
            PHITYPE=1
$COVARIANCE PRINT=E UNCONDITIONAL
