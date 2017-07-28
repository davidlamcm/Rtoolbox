#' this function will be deprecated
#' wrapper for ggplot
#' @export

plotSingleTS <-function(ts, title){
  if(class(ts) == "numeric"){
    df = data.frame(date = as.Date(names(ts)), fx = ts)
  }else if(class(ts) == "matrix"){
    df = data.frame(date = as.Date(rownames(ts)), fx = ts)
  }else if(class(ts) == "data.frame"){
    df = data.frame(date = as.Date(rownames(ts)), fx = ts)
    colnames(df) = c("date", "fx")
  }else if(class(ts) == "zoo"){
    df = data.frame(date = as.Date(index(ts)), fx = ts)
  }else{stop(paste("method for type", class(ts), "not defined"))}

  if(!missing(title)){
    return(ggplot(data = df, aes(x=date, y= fx, group =1)) + geom_line()+ ggtitle(title))
  }else{
    return(ggplot(data = df, aes(x=date, y= fx, group =1)) + geom_line() )
  }
  }
