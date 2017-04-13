#' wrapperfunction to calculate return of data.frame/ xts
#' @export

ROR <-function(data){
  data = na.locf(data, na.rm= F)

  if(any(class(data)%in%c("matrix","data.frame"))){
    temp = data/(rbind(NA,data)[-(nrow(data)+1),])
    rownames(temp) =rownames(data)
    temp[1,]=1
    temp[is.na(temp)]= 1
    return(temp)
  }else if(any(class(data)%in% c("zoo","xts"))){
    temp = data/lag(data)
    temp [1,]=1
    return(temp)
  }else{
    warning(paste("method not defined for this class :",class(data),", object coerce to matrix"))
    temp = data/(rbind(NA,data)[-(nrow(data)+1),])
    rownames(temp) =rownames(data)
    temp[1,]=1
    temp[is.na(temp)]= 1
    return(temp)
  }
}
