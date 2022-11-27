## Test replace model file
numbasethetas=7
numskipom=2
strCOEFF="COEFF"
strNewFilename<-"mynewdata.csv"
basemodel<-system.file("extdata/SimNeb/run30.mod", package = "PMXFrem")

fremmodelext<-system.file("extdata/SimNeb/run31.ext", package = "PMXFrem")
dfExt   <- getExt(extFile = fremmodelext)
calcFFEMtestout <-calcFFEM(dfExt,numNonFREMThetas = numbasethetas,numSkipOm = numskipom,quiet = T)

#Replace $PROBLEM
tmp<-findrecord(basemodel,record="\\$PROBLEM",replace="$PROBLEM FFEM model",quite=T)

#Replace $INPUT
strInput<-findrecord(basemodel,record="\\$INPUT",quite=T)
strInput<-c(strInput,paste0("         ",paste0(" ",strCOEFF,1:nrow(calcFFEMtestout$Coefficients),collapse = "")))
tmp<-findrecord(tmp,record="\\$INPUT",replace=strInput,quite=T)

#Replace $DATA #### TODO
strData<-findrecord(basemodel,record="\\$DATA",quite=T)
if (grepl("^(\\$DATA )(.*)(\\s+.+)$",strData[1])==FALSE) { #Only filename
  strData[1]<-gsub("^(\\$DATA )(.*)$",paste0("\\1",strNewFilename,"\\3"),strData[1])
} else {
  strData[1]<-gsub("^(\\$DATA )(.*)(\\s+.+)$",paste0("\\1",strNewFilename,"\\3"),strData[1])
}
tmp<-findrecord(tmp,record="\\$DATA",replace=strData,quite=T)

#Replace $OMEGA
tmp<-findrecord(tmp,record="\\$OMEGA",replace=buildmatrix(calcFFEMtestout$FullVars),quite=T)

#Replace $THETA
thvalues<-dfExt[dfExt$ITERATION==-1000000000,names(dfExt)[grepl("THETA.*",names(dfExt))]][1:numbasethetas]
tmp<-findrecord(tmp,record="\\$THETA",replace=paste0("$THETA"," ",thvalues, " ; TH",1:numbasethetas))

#Replace $SIGMA
nosigma<-length(dfExt[dfExt$ITERATION==-1000000000,names(dfExt)[grepl("SIGMA.*",names(dfExt))]])
df_sig  <- as.numeric(dfExt[dfExt$ITERATION==-1000000000,names(dfExt)[grepl("SIGMA.*",names(dfExt))]])
num_sig <- -1/2+sqrt(1/4+2*nosigma) #The col/row size of the full SIG matrix (including all blocks), remove last sigma
sig_matrix          <- as.numeric(df_sig)
#Get the sig-matrix
SIG                             <- matrix(0, nrow=num_sig, ncol=num_sig) #Define an empty matrix
SIG[upper.tri(SIG,diag = TRUE)]  <- sig_matrix #Assign upper triangular + diag
tSIG                            <- t(SIG) #Get a transposed matrix
SIG[lower.tri(SIG,diag = FALSE)] <- tSIG[lower.tri(tSIG,diag = FALSE)] #Assign the lower triangular except diag
SIGFULL<-SIG
# remove last sigma (FREM sigma)
SIGFULL<-SIGFULL[-nrow(SIGFULL),-ncol(SIGFULL)]
#Replace $SIGMA
tmp<-findrecord(tmp,record="\\$SIGMA",replace=buildmatrix(as.matrix(SIGFULL),strName = "$SIGMA"),quite=T)

#Replace FREM eta with ETA+Coefficients
for (i in 1:nrow(calcFFEMtestout$Coefficients)) {
  tmp  <- gsub(pattern = paste0("^(.*)([^TH]ETA\\(",i+numskipom,"\\))(.*)$"),
               replace = paste0("\\1(ETA(",i+numskipom,")+",strCOEFF,i,")\\3"),
               x = tmp)
}


##### Now TMP could be written to a new FFEM model file using writeLines()
