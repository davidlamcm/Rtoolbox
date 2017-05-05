#' given a matrix or xts object, ggplot the series
#' @export
#'

plotMatrix <-function(mat, title=paste("plot time:",Sys.time())){
  if(sum(class(b)=="xts")==1){
    mat = as.matrix(mat)
    }
  mat.long = melt(mat)
  colnames(mat.long) = c("date","series","value")
  if(!is.factor(mat.long$series)){
      mat.long$series = as.factor(mat.long$series)
  }
  if(is.factor(mat.long$date)){
    mat.long$date = as.Date(as.character(mat.long$date))
  }
  return(ggplot(mat.long,aes(x=date,y=value,color=series))+geom_line()+ggtitle(title))
}