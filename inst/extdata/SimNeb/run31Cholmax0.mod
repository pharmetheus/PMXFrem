;; 1. Based on: 25
;; 2. Description:
;;    New simulated data set
;; 3. Label:
;;    SimVal base model
;------------------------------------------------------------------------------
$PROBLEM FFEM model
$INPUT      NO ID STUDYID TAD TIME DAY AMT RATE ODV DV EVID BLQ DOSE
            FOOD FORM TYPE WT HT LBWT BSA SEX RACE AGE AST ALT BILI
            CRCL BMI NCI GENO2 ETHNIC SMOK RACEL NCIL
         CLFREMCOV VFREMCOV MATFREMCOV V33 V43 V53 V44 V54 V55
$DATA testDataFile_cholesky.csv IGNORE=@
            IGNORE(BLQ.EQN.1)
$SUBROUTINE ADVAN2 TRANS2
$PK
; ----------------------------------------------------------------
; 1. Cholesky factors (L) for an 3x3 matrix
;    (Offset applied: starting at index 3)
; ----------------------------------------------------------------

; Column 3
L33 = SQRT(MAX(0.000001, V33))
L43 = V43 / L33
L53 = V53 / L33

; Column 4
L44 = SQRT(MAX(0.000001, V44 - L43**2))
L54 = (V54 - L53 * L43) / L44

; Column 5
L55 = SQRT(MAX(0.000001, V55 - L53**2 - L54**2))

; ----------------------------------------------------------------
; 2. Multiply standard ETA with L for correlated MYETA
; ----------------------------------------------------------------
MYETA3 = L33 * ETA(3)
MYETA4 = L43 * ETA(3) + L44 * ETA(4)
MYETA5 = L53 * ETA(3) + L54 * ETA(4) + L55 * ETA(5)
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
CL    = EXP(MU_3               + (MYETA3 + CLFREMCOV))
V     = EXP(MU_4               + (MYETA4 + VFREMCOV))
MAT   = MATCOVTIME * EXP(MU_5  + (MYETA5 + MATFREMCOV))
D1    = MAT*(1-D1FR)

F1    = FREL
KA    = 1 / (MAT-D1)
S2    = V

$ERROR
CP    = A(2)*1000 / V
IPRED = LOG(CP + 0.00001)
Y     = IPRED + EPS(1) * EXP(ETA(1))

$THETA 1 ; TH1
$THETA 6.14514 ; TH2
$THETA 122.525 ; TH3
$THETA 1.88694 ; TH4
$THETA 0.670374 ; TH5
$THETA -0.0522225 ; TH6
$THETA 0.121132 ; TH7
$OMEGA BLOCK(1) 
0.0541999 
$OMEGA BLOCK(1) 
1e-04
$OMEGA BLOCK(3) FIX
1.0
0.0 1.0
0.0 0.0 1.0
$SIGMA BLOCK(1) 
0.0309911
$ESTIMATION METHOD=1 INTER MAX=0
$TABLE      NO ID STUDYID TAD TIME DAY AMT RATE ODV DV EVID BLQ DOSE
            FOOD FORM TYPE WT HT LBWT BSA SEX RACE AGE AST ALT BILI
            CRCL BMI NCI GENO2 ETHNIC SMOK RACE NCIL CPRED
            CIPREDI CWRES CIWRES ETAS(1:LAST) NOPRINT ONEHEADER
            FILE=ffemtab31Chol

