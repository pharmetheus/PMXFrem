#' Title
#'
#' @param listCovs a list with covariate names and values of the form; list("AGE"=c(1,2)) etc. For multiple covariates (i.e. not univariate) a list of lists should be used, e.g. list(list("AGE"=c(1,2),"BW"=3,4)) 
#' @param iMiss The value that should be filled for missing, default = NA
#'
#' @return a data frame with a covariate value (univariate) per row or several covariate values per row (multivariate). 
#' @export
#'
dfCreateInputForestData<-function(listCovs=NULL,iMiss=NA) {
  cstrUniqueCols<-c()
  if (!is.null(listCovs)) {
    for (i in 1:length(listCovs)) {
      if (is.list(listCovs[[i]])) {
        for (j in 1:length(listCovs[[i]])) {
          cstrUniqueCols<-c(cstrUniqueCols,names(listCovs[[i]][j]))
        }      
      } else {
        cstrUniqueCols<-c(cstrUniqueCols,names(listCovs[i]))
      }
    }
  } else {
    return(data.frame())
  }
  cstrUniqueCols<-unique(cstrUniqueCols)
  ncols<-length(cstrUniqueCols)
  df<-data.frame(rbind(1:ncols))
  names(df)<-cstrUniqueCols
  r<-1
  for (i in 1:length(listCovs)) {
    if (is.list(listCovs[[i]])) {
      strVal1<-listCovs[[i]][[1]]
      for (k in 1:length(strVal1)) {
        dfr<-df[1,]
        dfr[,]<- iMiss
        for (j in 1:length(listCovs[[i]])) {
          strName<-names(listCovs[[i]][j])
          dfr[[strName]]<-listCovs[[i]][[j]][k]
        }
        df<-rbind(df,dfr)
      }
    } else {
      vecr<-listCovs[[i]]
      strName<-names(listCovs[i])
      for (k in 1:length(vecr)) {
        dfr<-df[1,]
        dfr[,]<- iMiss
        dfr[[strName]]<-vecr[k]
        df<-rbind(df,dfr)
      }
    }
  }
  return(df[-1,])
}
