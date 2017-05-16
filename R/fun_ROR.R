#' wrapperfunction to calculate return of data.frame/ xts
#' @export

ROR <-function(mat, keepNAs = FALSE ){
  data = na.locf(mat, na.rm= F)

  if(any(class(data)%in%c("matrix","data.frame"))){
    temp = data/(rbind(NA,data)[-(nrow(data)+1),])
    rownames(temp) =rownames(data)
    temp[1,]=1
    temp[is.na(temp)]= 1

  }else if(any(class(data)%in% c("zoo","xts"))){
    temp = data/lag(data)
    temp [1,]=1

  }else{
    warning(paste("method not defined for this class :",class(data),", object coerce to matrix"))
    temp = data/(rbind(NA,data)[-(nrow(data)+1),])
    rownames(temp) =rownames(data)
    temp[1,]=1
    temp[is.na(temp)]= 1

  }
  if(keepNAs){
    temp[is.na(mat)] = NA
  }
  return(temp)
}
