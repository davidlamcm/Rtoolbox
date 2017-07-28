#' this function will be deprecated. 
#' wrapper for ggplot 
#' @export
plotTS <-function(ts, title, additional = F, legend = "fx" ,...){
  if(class(ts) == "numeric"){
    df = data.frame(date = as.Date(names(ts)), fx = ts, legend =legend)
    colnames(df) = c("date", "fx","legend")
  }else if(class(ts) == "matrix"){
    df = data.frame(date = as.Date(rownames(ts)), fx = ts, legend =legend)
    colnames(df) = c("date", "fx","legend")
  }else if(class(ts) == "data.frame"){
    df = data.frame(date = as.Date(rownames(ts)), fx = ts, legend = legend)
    colnames(df) = c("date", "fx","legend")
  }else if(class(ts) == "zoo"){
    df = data.frame(date = as.Date(index(ts)), fx = ts, legend =legend)
    colnames(df) = c("date", "fx","legend")
  }else{stop(paste("method for type", class(ts), "not defined"))}
  
  if(additional){
      return(geom_line(data = df, aes(x=date, y= fx, group =legend ), ...=...))
  }else{
    if(!missing(title)){
      return(ggplot(data = df, aes(x=date, y= fx, group =legend, color = legend)) + geom_line()+ ggtitle(title))
    }else{
      return(ggplot(data = df, aes(x=date, y= fx, group =legend, color = legend)) + geom_line() )
    }
  }
}