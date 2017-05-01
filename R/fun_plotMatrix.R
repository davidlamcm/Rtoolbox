#' given a matrix or xts object, ggplot the series
#' @export
#'

plotMatrix <-function(mat, title=""){
  if(sum(class(b)=="xts")==1){
    mat = as.matrix(mat)
    }
  mat.long = melt(mat)
  colnames(mat.long) = c("date","series","value")
  mat.long$date = as.Date(as.character(mat.long$date))
  return(ggplot(e,aes(x=date,y=value,color=series))+geom_line()+ggtitle(title))
  }