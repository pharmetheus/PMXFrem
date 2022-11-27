#' Title
#'
#' @param filename the filename of the NONMEM model file
#' @param record The NONMEM record to replace/find, note all of the records starting with "record" will be replaced/found 
#' @param replace string vector to replace with (default=NULL, no replacement)
#' @param quite verbose (quite=F) or not (quite=T, default)
#'
#' @return the new model file include the replacement (if replace!=NULL), otherwise the found record(s)
#' @export
#'
#' @examples
findrecord<-function(filename,record="\\$OMEGA",replace=NULL,quite=TRUE) {
  con=file(filename,open="r")
  line=readLines(con) 
  start<-NULL
  stop<-NULL
  for (i in 1:length(line)){
    tmp<-grep(paste0("^",record,".*"),line[i])
    if (!is.null(start)) {
      if (length(tmp)==0) {
        tmp1<-grep(paste0("^\\$.*"),line[i])
        if (length(tmp1)!=0) {
          stop<-i-1
          break
        }
      }
    } else  {
      if (length(tmp)>0) {
        start<-i
      }
    }
  }
  if (is.null(stop)) stop<-length(line)
  if (!quite && is.null(start)==FALSE && is.null(stop)==FALSE) print(line[start:stop])
  
  #Replace some text
  if (!is.null(replace) && is.null(start)==FALSE && is.null(stop)==FALSE) {
    newtext<-c()
    if (start>1) newtext<-line[1:(start-1)]
    newtext<-c(newtext,replace)
    if (stop<length(line)) newtext<-c(newtext,line[(stop+1):length(line)])    
    if (!quite) print(newtext)
    close(con)
    return(newtext)
  }
  close(con)
  if (is.null(start)==FALSE && is.null(stop)==FALSE) return(line[start:stop])
}