#' calcFFEM
#'
#' @description Computes the corresponding FFEM from a FREM model. Can handle missing covariates.
#' @param noBaseThetas Number of thetas that are not FREM covariates. These need to come before the FREM covariate thetas.
#' @param noCovThetas  The number thetas associated with covariate effects. 
#' @param noSigmas  Number of sigma (epsilon) parameters in the model, including the sigma(s) used for the covariate effects.
#' @param noParCov Number of parameters for which covariate relations are sought (often the same as noBaseThetas).
#' @param noSkipOm Number of diag omegas (variances) that should not be part of the FREM calculations. 
#' Such omegas has to come before the large FREM omega block.
#' @param dfExt A data frame of the ext file for the model. In case of multiple $EST, the dfext 
#' should contain the estimates of the *one* relevant $EST.
#' @param parNames Names of the parameters
#' @param covNames Names of the covariates
#' @param availCov Names of the covariates to use in teh calculation of the FFEM model.
#' @param quiet If FALSE, will print the FFEM model + associated $OMEGA BLOCK
#' @param fremEta Computes individual eta_prims.
#' @param eqFile File name to save the FFEM equations in
#' @param omFile File name to save the omega prim matrix in.
#' 
#' @return A list with components Coefficients, Vars and Expr:
#' 
#' Coeffients: A noBaseThetas x availCov matrix with FFEM model coefficients
#' 
#' Vars:       A noBaseThetas x noBaseThetas matrix with the FFEM variances (OMEGA)
#' 
#' Expr:       A vector of FFEM expressions, one for each base model parameter.
#' @export
#'
#' @examples
#' \dontrun{
#' Below is a frem model file. The base (non-frem) model contains 13 thetas and 6 etas.
#' 
#' The first two thetas are allometric constants fixed to o,75 and 1,
#' respectively, an they are not part of the frem covariate model. The third
#' theta is a a bioavailability term that is fixed to 1 and is associated with
#' an eta and should be part of the frem covariate model. Theta number 7 has in
#' practice no eta associated with it. However, to get the mu-efficiency with
#' the IMP estimation algorithm it is associated with a small eta with a fixed
#' value. This theta is not part of the frem covariate model. Thetas 8 to 13 are
#' mechanistic and structural covariate effect that are included as fixed
#' effects before the frem covariate analysis. They should not be part of the
#' frem covariate model. This means that it is theta 3 to 6 (that is 4 thetas)
#' that will be part of the frem covariate model development.
#' 
#' This translates to noBaseThetas=13, noParCov=4 and noCovThetas=13.
#' 
#' The base model has 6 omegas, of which omega 1 and two should not be part of
#' the frem covariate model. (the first of these is IIV in RUV and the second
#' one is the fixed omega associated with theta 7.)
#' 
#' This translates to noSkipOm = 2.
#' 
#' There are two sigmas in the model, which translates to noSigmas=2.
#' 
#' A call to calcFFEM for this model would be:
#' 
#' parEsts  <- read.table(extFile,data.table=FALSE,skip=1,h=T) %>% 
#'             filter(ITERATION==-1000000000)
#'             
#' calcFFEM(noBaseThetas = 13,
#'          noCovThetas = 13, 
#'          noSigmas = 2 , 
#'          noParCov = 4, 
#'          noSkipOm = 2, 
#'          dfext = parEsts)
#'
#' $PROBLEM    run 1
#' $INPUT      NO ID STUDYID TAD TIME DAY AMT RATE ODV DV EVID BLQ DOSE
#' FOOD FORM TYPE WT HT AGE SEX RACE ETHNIC GENO SMOK AST ALT
#' BILI BMI CRCL NCI NCIL RACEL RACEL1 RACEL2 RACEL3 GENO1
#' GENO2 GENO3 GENO4 FREMTYPE
#' $DATA      frem21.dir/frem_dataset.dta IGNORE=@
#'   $SUBROUTINE ADVAN2 TRANS2
#' $PK
#' ;;; FRELGENO4-DEFINITION START
#' IF(GENO4.EQ.0.0000E+00) FRELGENO4 = 1  ; Most common
#' IF(GENO4.EQ.1.0000E+00) FRELGENO4 = ( 1 + THETA(13))
#' ;;; FRELGENO4-DEFINITION END
#' 
#' 
#' ;;; FRELFORM-DEFINITION START
#' IF(FORM.EQ.1.0000E+00) FRELFORM = 1  ; Most common
#' IF(FORM.EQ.0.0000E+00) FRELFORM = ( 1 + THETA(12))
#' ;;; FRELFORM-DEFINITION END
#' 
#' ;;; FREL-RELATION START
#' FRELCOV     = FRELGENO4
#' FRELCOVTIME = FRELFORM
#' ;;; FREL-RELATION END
#' 
#' 
#' ;;; CLFOOD-DEFINITION START
#' IF(FOOD.EQ.1.0000E+00) CLFOOD = 1  ; Most common
#' IF(FOOD.EQ.0.0000E+00) CLFOOD = ( 1 + THETA(11))
#' ;;; CLFOOD-DEFINITION END
#' 
#' CLWT    =  (WT/75)**THETA(1)
#' CLGENO1 = 1
#' CLGENO2 = 1
#' CLGENO3 = 1
#' CLGENO4 = 1
#' IF(GENO1.EQ.1) CLGENO1 = (1+THETA(8))
#' IF(GENO3.EQ.1) CLGENO3 = (1+THETA(9))
#' IF(GENO4.EQ.1) CLGENO4 = (1+THETA(10))
#' 
#' ;;; CL-RELATION START
#' CLCOVTIME = CLFOOD
#' CLCOV     = CLWT*CLGENO1*CLGENO2*CLGENO3*CLGENO4
#' ;;; CL-RELATION END
#' 
#' 
#' VWT   = (WT/75)**THETA(2)
#' VCOV  = VWT
#' 
#' TVFREL  = THETA(3)*FRELCOV
#' TVCL    = THETA(4) * CLCOV
#' TVV     = THETA(5) * VCOV
#' TVMAT   = THETA(6)
#' TVD1    = THETA(7)
#' 
#' MU_2  = TVD1
#' MU_3  = LOG(TVFREL)
#' MU_4  = LOG(TVCL)
#' MU_5  = LOG(TVV)
#' MU_6  = LOG(TVMAT)
#' 
#' D1FR  = MU_2                   + ETA(2)
#' FREL  = FRELCOVTIME * EXP(MU_3 + ETA(3))
#' CL    = CLCOVTIME   * EXP(MU_4 + ETA(4))
#' V     = EXP(MU_5               + ETA(5))
#' MAT   = EXP(MU_6               + ETA(6))
#' 
#' 
#' D1    = MAT*(1-TVD1)
#' F1    = FREL
#' KA    = 1 / (MAT-D1)
#' S2    = V
#' 
#' MU_7 = THETA(14)
#' COV7 = MU_7 + ETA(7)
#' MU_8 = THETA(15)
#' COV8 = MU_8 + ETA(8)
#' MU_9 = THETA(16)
#' COV9 = MU_9 + ETA(9)
#' MU_10 = THETA(17)
#' COV10 = MU_10 + ETA(10)
#' MU_11 = THETA(18)
#' COV11 = MU_11 + ETA(11)
#' MU_12 = THETA(19)
#' COV12 = MU_12 + ETA(12)
#' MU_13 = THETA(20)
#' COV13 = MU_13 + ETA(13)
#' MU_14 = THETA(21)
#' COV14 = MU_14 + ETA(14)
#' MU_15 = THETA(22)
#' COV15 = MU_15 + ETA(15)
#' MU_16 = THETA(23)
#' COV16 = MU_16 + ETA(16)
#' MU_17 = THETA(24)
#' COV17 = MU_17 + ETA(17)
#' MU_18 = THETA(25)
#' COV18 = MU_18 + ETA(18)
#' MU_19 = THETA(26)
#' COV19 = MU_19 + ETA(19)
#' $ERROR
#' CP    = A(2)*1000 / V
#' IPRED = LOG(CP + 0.00001)
#' Y     = IPRED + EPS(1) * EXP(ETA(1))
#' 
#' ;;;FREM CODE BEGIN COMPACT
#' ;;;DO NOT MODIFY
#' IF (FREMTYPE.EQ.100) THEN
#' ;       AGE  1
#' Y = COV7 + EPS(2)
#' IPRED = COV7
#' END IF
#' IF (FREMTYPE.EQ.200) THEN
#' ;       AST  1
#' Y = COV8 + EPS(2)
#' IPRED = COV8
#' END IF
#' IF (FREMTYPE.EQ.300) THEN
#' ;       BILI  1
#' Y = COV9 + EPS(2)
#' IPRED = COV9
#' END IF
#' IF (FREMTYPE.EQ.400) THEN
#' ;       CRCL  1
#' Y = COV10 + EPS(2)
#' IPRED = COV10
#' END IF
#' IF (FREMTYPE.EQ.500) THEN
#' ;       BMI  1
#' Y = COV11 + EPS(2)
#' IPRED = COV11
#' END IF
#' IF (FREMTYPE.EQ.600) THEN
#' ;       HT  1
#' Y = COV12 + EPS(2)
#' IPRED = COV12
#' END IF
#' IF (FREMTYPE.EQ.700) THEN
#' ;       ALT  1
#' Y = COV13 + EPS(2)
#' IPRED = COV13
#' END IF
#' IF (FREMTYPE.EQ.800) THEN
#' ;       SEX  1
#' Y = COV14 + EPS(2)
#' IPRED = COV14
#' END IF
#' IF (FREMTYPE.EQ.900) THEN
#' ;       RACEL_3  1
#' Y = COV15 + EPS(2)
#' IPRED = COV15
#' END IF
#' IF (FREMTYPE.EQ.1000) THEN
#' ;       RACEL_2  1
#' Y = COV16 + EPS(2)
#' IPRED = COV16
#' END IF
#' IF (FREMTYPE.EQ.1100) THEN
#' ;       ETHNIC  1
#' Y = COV17 + EPS(2)
#' IPRED = COV17
#' END IF
#' IF (FREMTYPE.EQ.1200) THEN
#' ;       NCI_2  1
#' Y = COV18 + EPS(2)
#' IPRED = COV18
#' END IF
#' IF (FREMTYPE.EQ.1300) THEN
#' ;       NCI_1  1
#' Y = COV19 + EPS(2)
#' IPRED = COV19
#' END IF
#' ;;;FREM CODE END COMPACT
#' $THETA  0.75 FIX ; 1. WT coef on TVCL
#' $THETA  1 FIX ; 2. WT coef on TVV
#' $THETA  1 FIX ; 3. TVFREL
#' $THETA  (0,12.7773) ; 4. TVCL
#' $THETA  (0,205.829) ; 5. TVV
#' $THETA  (0,6.51647) ; 6. TVMAT
#' $THETA  (0,0.61699,1) ; 7 TVD1
#' $THETA  (-1,1.34324) ; GENO2=1 on CL
#' $THETA  (-1,-0.331668) ; GENO3=1 on CL
#' $THETA  (-1,-0.53044) ; GENO4=1 on CL
#' $THETA  (-1.00,0.242451,3.00) ; CLFOOD1
#' $THETA  (-1.00,-0.143797,3.00) ; FRELFORM1
#' $THETA  (-1.00,2.67707,3.00) ; FRELGENO41
#' $THETA  44.1836 ; TV_AGE
#' 25.0479 ; TV_AST
#' 9.88051 ; TV_BILI
#' 118.738 ; TV_CRCL
#' 30.2749 ; TV_BMI
#' 169.22 ; TV_HT
#' 27.1594 ; TV_ALT
#' 1.4473 ; TV_SEX
#' 0.0193839 ; TV_RACEL_3
#' 0.20073 ; TV_RACEL_2
#' 0.436689 ; TV_ETHNIC
#' 0.0221302 ; TV_NCI_2
#' 0.125865 ; TV_NCI_1
#' $OMEGA  0.0812086  ; 1. IIV on RUV
#' $OMEGA  0.0001  FIX  ;      2. D1
#' $OMEGA  BLOCK(17)
#' 0.544351  ; 2. IIV on FREL
#' 0.278425 0.31439  ; 3. IIV on CL
#' 0.148203 0.122238 0.137306  ; 4. IIV on V
#' -0.0189923 0.00918249 0.0285743 0.150912  ; 5. IIV on MAT
#' 0.626969 0.28696 0.481008 -0.116125 171.085  ;    BSV_AGE
#' 0.397298 0.122165 0.244924 0.285112 16.6782 113.205  ;    BSV_AST
#' 0.0530488 -0.205055 -0.0432656 -0.197691 -8.40381 -2.96932 26.4757  ;   BSV_BILI
#' 3.72852 3.19961 4.02062 1.08586 -119.834 -10.5479 -9.19654 592.341  ;   BSV_CRCL
#' 0.944631 0.691555 1.01369 0.175379 21.4572 5.11797 -6.07847 79.8047 39.8441  ;    BSV_BMI
#' 3.65846 3.0081 3.29489 -0.00666954 -1.19773 2.03668 4.47091 53.5262 -0.519596 104.085  ;     BSV_HT
#' 0.988694 0.432986 0.741061 0.547903 15.0461 129.559 0.557535 23.0129 13.1751 9.41745 230.087  ;    BSV_ALT
#' -0.167399 -0.128631 -0.116313 -0.00738728 -0.26027 -0.655333 -0.370499 -0.125096 -0.13336 -3.30564 -1.27733 0.247425  ;    BSV_SEX
#' 0.00104792 0.00254993 -0.00379118 0.00432268 0.0324295 0.0572677 0.0216384 -0.295361 -0.0574949 -0.0769385 -0.0143219 -0.00172448
#' 0.0190408  ; BSV_RACEL_3
#' 0.0114891 0.0127611 0.0174851 -0.00293476 -1.12389 -0.108775 0.105182 0.28354 -0.0167073 0.563866 -0.76742 0.00184791 -0.00389946
#' 0.160722  ; BSV_RACEL_2
#' -0.0504926 -0.0408726 -0.0527966 0.0125345 -0.960252 -0.152771 -0.00892902 -0.147249 -0.445574 -1.3043 0.736774 0.00348973 -0.00708516 -0.0780337
#' 0.246282  ; BSV_ETHNIC
#' 0.00418758 -0.00221833 -0.000838218 -0.00172047 -0.0374597 -0.10931 0.466311 -0.183617 -0.0975384 0.0627523 -0.0605703 -0.00296382 -0.000430372
#' -0.000295859 2.74251E-05 0.0216996  ;  BSV_NCI_2
#' 0.0218576 -0.000970116 0.00839849 0.00242089 -0.0417989 1.19467 0.653081 0.315424 0.110536 0.137869 2.40745 -0.0174543 0.00171392 -0.00176909
#' 0.00595996 -0.00279664 0.110307  ;  BSV_NCI_1
#' $SIGMA  0.0427759  ;     1. RUV
#' $SIGMA  1E-07  FIX  ;     EPSCOV
#' $ESTIMATION METHOD=IMPMAP AUTO=1 RANMETHOD=3P INTER NOABORT PRINT=1
#' CITER=15 NOCOV=1 ISAMPEND=100 NITER=50 ISAMPLE=300 CTYPE=1
#' CALPHA=0.001
#' }

calcFFEM <- function(noBaseThetas,noCovThetas,noSigmas,noParCov=noBaseThetas,noSkipOm=0,dfext,
                     parNames=paste("Par",1:noParCov,sep=""),covNames=paste("Cov",1:noCovThetas,sep=""),
                     availCov=covNames,quiet=FALSE,fremETA=NULL,eqFile = "",omFile="") {
  
  ## Computes the corresponding FFEM from a FREM model. Can handle missing covariates.
  
  # noBaseThetas: Number of thetas that are not covariates
  # noCovThetas:  Number of FREM covariates that are tested, i.e. number of thetas associated with covariate effects
  # noParCov:     Number of parameters for which covariate relations are sought (often the same as noBaseTheta)
  # dfExt:        A data frame of the ext file for the model. In case of mutiple $EST, the dfext should contain the estimates of the *one* relevant $EST.
  # parNames:     Names of the parameters
  # covNames:     Names of the covariates
  # availCov:     Names of the non-missing covariates
  # quiet:        If FALSE, will print the FFEM model + associated $OMEGA BLOCK
  # fremETA:      Vector with indivdual etas from FREM, used to calculate FFEM etas, if NULL no FFEM etas will be calculated
  
  ## Return value: A list with components Coefficients, Vars and Expr
  ##    Coeffients: A noBaseThetas x availCov matrix with FFEM model coefficients
  ##    Vars:       A noBaseThetas x noBaseThetas matrix with the FFEM variances (OMEGA)
  ##    Expr:       A vector of FFEM expressions, one for each base model parameter.
  ##    FullVars:   A full omega matrix including skipOM if available, otherwise same as Vars
  ##    UpperVars:  A omega matrix corresponding to the upper block (only relevant when noSkipOm!=0), otherwise=NULL
  ##    Eta_prim:   A vector with individual FFEM etas back-calculated from FREM etas, conditioned on availCov, NULL if fremETA=NULL
  iNumFREMOM<-(noCovThetas+noParCov)*(noCovThetas+noParCov+1)/2
  iNumSigma <- noSigmas*(noSigmas+1)/2
  if (nrow(dfext)>1) dfext  <- dfext[dfext$ITERATION==-1000000000,]
  
  df_th  <- as.numeric(dfext[,2:(noBaseThetas+1)])
  df_thm <- as.numeric(dfext[,(noBaseThetas+2):(noBaseThetas+1+noCovThetas)])
  #df_om  <- as.numeric(dfext[,(noBaseThetas+5+noCovThetas):(noBaseThetas+4+noCovThetas+iNumFREMOM)])
  df_om  <- as.numeric(dfext[,(noBaseThetas+iNumSigma+2+noCovThetas):(ncol(dfext)-1)])
  
  num_om <- -1/2+sqrt(1/4+2*iNumFREMOM)+noSkipOm #The col/row size of the full OM matrix (including all blocks)
  
  om_matrix          <- as.numeric(df_om)
  
  #Get the om-matrix
  OM                             <- matrix(0, nrow=num_om, ncol=num_om) #Define an empty matrix
  OM[upper.tri(OM,diag = TRUE)]  <- om_matrix #Assign upper triangular + diag
  tOM                            <- t(OM) #Get a transposed matrix
  OM[lower.tri(OM,diag = FALSE)] <- tOM[lower.tri(tOM,diag = FALSE)] #Assign the lower triangular except diag
  OMFULL<-OM
  
  if (noSkipOm!=0) OM<-OM[-(1:noSkipOm),-(1:noSkipOm)] #Remove upper block
  
  OM_PAR     <- OM[1:noParCov,1:noParCov] #The parameter covariance matrix
  OM_COV     <- OM[(noParCov+1):(noParCov+noCovThetas),(noParCov+1):(noParCov+noCovThetas)] #The covariates covariance matrix
  OM_PAR_COV <- OM[1:noParCov,(noParCov+1):(noParCov+noCovThetas)] #The covariance between covariates and parameters matrix
  
  if(length(availCov)!=0 & length(c(1:length(covNames))[!(covNames %in% availCov)])!=0) {
    missCov    <- c(1:length(covNames))[!(covNames %in% availCov)]
    #inv        <- inv[-missCov,-missCov]
    
    OM_COV <- OM_COV[-missCov,-missCov]
    inv    <- solve(OM_COV)
    
    if (ncol(as.matrix(OM_PAR_COV))==1) {
      OM_PAR_COV <- t(as.matrix(OM_PAR_COV))[,-missCov]
      if(!is.matrix(OM_PAR_COV)) OM_PAR_COV <- t(as.matrix(OM_PAR_COV))
    } else {
      OM_PAR_COV <-as.matrix(OM_PAR_COV[,-missCov])
    }
    
    ## Fix the covariate names and means
    covNames   <- covNames[-missCov]
    df_thm     <- df_thm[-missCov]
    
    COEFF     <- OM_PAR_COV%*%inv #The parameter-covariate coefficients
    COEFF_VAR <- OM_PAR-OM_PAR_COV%*%inv%*%t(OM_PAR_COV) #The parameter variances
    
  } else if(length(c(1:length(covNames))[!(covNames %in% availCov)])==0) {
    
    if (ncol(as.matrix(OM_PAR_COV))==1) {
      OM_PAR_COV <- t(as.matrix(OM_PAR_COV))
    } else {
      OM_PAR_COV <- as.matrix(OM_PAR_COV)
    }
    
    inv       <- solve(OM_COV)
    COEFF     <- OM_PAR_COV%*%inv #The parameter-covariate coefficients
    COEFF_VAR <- OM_PAR-OM_PAR_COV%*%inv%*%t(OM_PAR_COV) #The parameter variances
  } else {
    
    COEFF     <- t(as.matrix(OM_PAR_COV))
    #COEFF_VAR <- OM_PAR-OM_PAR_COV # Check with Jocke if this is the way it should be
    COEFF_VAR <- OM_PAR
  }
  
  ## Print the FFEM for inspection
  if(!quiet) {
    for(p in 1:nrow(COEFF)) {
      for(c in 1:ncol(COEFF)) {
        # if(c==1 & p==1) cat(parNames[p],":",sep="",file=eqFile)
        # if(c==1 & p!=1) cat(parNames[p],":",sep="",file=eqFile,append = TRUE)
        if(c==1 & p==1) cat(parNames[p],"",sep="",file=eqFile)
        if(c==1 & p!=1) cat(parNames[p],"",sep="",file=eqFile,append = TRUE)
        if(length(availCov)==0) {
          cat(paste("0","*","(",covNames[c],"-",paste(round(df_thm[c],3),")",sep=""),sep=""),file=eqFile,append=TRUE)
        } else {
          cat(paste(round(COEFF[p,c],3),"*","(",covNames[c],"-",paste(round(df_thm[c],3),")",sep=""),sep=""),file=eqFile,append=TRUE)
        }
        if(c!=ncol(COEFF)) cat("+",file=eqFile,append=TRUE)
        if(c==ncol(COEFF)) cat("\n",file=eqFile,append=TRUE)
      }
      # cat("\n",file=eqFile,append=TRUE)
    }
  }
  
  
  ## Create evaluable expression
  myExpr <- c()
  for(p in 1:nrow(COEFF)) {
    myExpr[p] <- ""
    for(c in 1:ncol(COEFF)) {
      if(length(availCov)==0) {
        myExpr[p] <- paste(myExpr[p],"0","*","(data$",covNames[c],"-",df_thm[c],")",sep="")
      } else {
        myExpr[p] <- paste(myExpr[p],COEFF[p,c],"*","(data$",covNames[c],"-",df_thm[c],")",sep="")
      }
      if(c!=ncol(COEFF)) myExpr[p] <- paste(myExpr[p],"+")
    }
  }
  
  if(!quiet) {
    cat(paste("\n$OMEGA BLOCK(",nrow(COEFF_VAR),")",sep=""),"\n",file=omFile)
    
    for(v in 1:nrow(COEFF_VAR)) {
      for(v2 in 1:v) {
        cat(round(COEFF_VAR[v,v2],5),file=omFile,append=TRUE)
        if(v2==v) cat("\n",append=TRUE,file=omFile)
      }
    }
  }
  
  if (noSkipOm==0) { #If no upper block
    FULLVARS<-COEFF_VAR
    UPPERVARS<-numeric(0)
  } else {
    FULLVARS<-matrix(0,ncol=noSkipOm+ncol(COEFF_VAR),nrow=noSkipOm+ncol(COEFF_VAR))  
    FULLVARS[(noSkipOm+1):ncol(FULLVARS),(noSkipOm+1):ncol(FULLVARS)]<-COEFF_VAR
    FULLVARS[1:noSkipOm,1:noSkipOm]<-OMFULL[1:noSkipOm,1:noSkipOm]
    UPPERVARS<-OMFULL[1:noSkipOm,1:noSkipOm]
  }
  ## Calculate eta_prim
  if (!is.null(fremETA)) {
    if (noSkipOm == 0) {
      if (!exists("missCov")) {
        eta_prim <- fremETA[1:noParCov] - COEFF %*% fremETA[(noParCov + 1):(noParCov + noCovThetas)]
      } else {
        eta_prim <- fremETA[1:noParCov] - COEFF %*% fremETA[(noParCov + 1):(noParCov + noCovThetas)][-missCov]
      }
    } else {
      if (!exists("missCov")) {
        eta_prim <- c(fremETA[1:noSkipOm], fremETA[(noSkipOm + 1):(noParCov + noSkipOm)] - COEFF %*% fremETA[(noSkipOm + noParCov + 1):(noSkipOm + noParCov + noCovThetas)])
      } else {
        eta_prim <- c(fremETA[1:noSkipOm], fremETA[(noSkipOm + 1):(noParCov + noSkipOm)] - COEFF %*% fremETA[(noSkipOm + noParCov + 1):(noSkipOm + noParCov + noCovThetas)][-missCov])
      }
    }
  } else {
    eta_prim <- NULL
  }
  
  return(
    invisible(
      list(Coefficients = COEFF,
           Vars         = COEFF_VAR,
           Expr         = myExpr,
           FullVars     = FULLVARS,
           UpperVars    = UPPERVARS,
           Eta_prim     = eta_prim
      )
    )
  )
}
