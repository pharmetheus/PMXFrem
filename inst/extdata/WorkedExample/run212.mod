;; 1. Based on: run202
;; 2. Description:
;;    FREM base model: no covariates except food on MAT, which varies within subjects and is kept
;;    outside the MU expression; IMPMAP; OMEGA BLOCK(3) for CL, V and MAT
;; 3. Label:
;;    FREM base model, studies 7 and 3
;------------------------------------------------------------------------------
$PROBLEM FFEM model
$INPUT      ID STUDYID TAD TIME OCC AMT RATE SS II EVID ODV DV DOSE FOOD WT HT AGE RACE BMI CRCL NCI
            SEX ETHNIC SMOK GENO FORM AST ALT BILI NCIORG RACEORG LNWT
         CLFREMCOV VFREMCOV MATFREMCOV
$DATA       run212_data.csv IGNORE=@
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
CL    = EXP(MU_3 + (ETA(3)+CLFREMCOV))
V     = EXP(MU_4 + (ETA(4)+VFREMCOV))

;; MATFOOD is kept outside the MU expression since FOOD varies with time
MAT   = MATFOOD * EXP(MU_5 + (ETA(5)+MATFREMCOV))

D1    = MAT*(1-TVD1)

KA    = 1 / (MAT-D1)
S2    = V

$ERROR
CP    = A(2)*1000 / V
IPRED = LOG(CP + 0.00001)
Y     = IPRED + EPS(1) * EXP(ETA(1))

$THETA (0,7.30633) ; 1. TVCL
$THETA (0,153.046) ; 2. TVV
$THETA (0,2.08915) ; 3. TVMAT
$THETA (0,0.395406,1) ; 4. fr D1
$THETA (-1,-0.125036,3) ; 5. MATFOOD
$OMEGA BLOCK(1) 
0.245237 
$OMEGA BLOCK(1) 
1e-04 
$OMEGA BLOCK(3) 
0.181029081130015 
0.0858081620927373 0.162565889130628 
0.0115262206953855 0.0434975526542371 0.0470268385920344 
$SIGMA BLOCK(1) 
0.0323074
$ESTIMATION METHOD=1 INTER MAX=0
$TABLE      ID TIME TAD EVID STUDYID DV PRED IPRED CWRES CIWRES CL V MAT
            NOPRINT ONEHEADER FILE=goftab212
