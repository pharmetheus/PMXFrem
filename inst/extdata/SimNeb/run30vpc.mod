;; 1. Based on: 30
;; 2. Description:
;;    MAX=0 for diagnostict
;; 3. Label:
;;    SimVal base model
;------------------------------------------------------------------------------
$PROBLEM    run 1
$INPUT      NO ID STUDYID TAD TIME DAY AMT RATE ODV DV EVID BLQ DOSE
            FOOD FORM TYPE WT HT LBWT BSA SEX RACE AGE AST ALT BILI
            CRCL BMI NCI GENO2 ETHNIC SMOK RACEL NCIL
$DATA      ../ProducedData/Dataset/DAT-2-MI-PMX-2-onlyTYPE2-new.csv
            IGNORE=@
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

$ERROR
CP    = A(2)*1000 / V
IPRED = LOG(CP + 0.00001)
Y     = IPRED + EPS(1) * EXP(ETA(1))

REPI = IREP
$THETA  1 FIX ; 1. TVFREL
$THETA  (0,6.17206) ; 2. TVCL
$THETA  (0,121.358) ; 3. TVV
$THETA  (0,1.91756) ; 4. TVMAT
$THETA  (0,0.676682) ; 5. D1
$THETA  (-1.00,-0.0521041,3.00) ; 6. FRELFOOD1
$THETA  (-1.00,0.152505,3.00) ; 7. MATFOOD1
$OMEGA  0.054943  ; 1. IIV on RUV
$OMEGA  0.0001  FIX  ; 2. IIV on D1
$OMEGA  BLOCK(3)
 0.183099  ; 3. IIV on CL
 0.140718 0.200553  ; 4. IIV on V
 0.012395 0.0258711 0.047017  ; 5. IIV on MAT
$SIGMA  0.0309612  ;     1. RUV
$SIMULATION (1413) ONLYSIM NSUB=50
$TABLE      REPI ID TAD 
            NOPRINT NOAPPEND ONEHEADERALL FILE=vpctab30


