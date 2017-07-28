#' given a list of futures chain series , concatenate them into one single time serie
#' the list of futures chain series must have the fields c("last_price","volume") or else wont work
#' @export

combineFutSeries <-function(futSeries ){
  #futSeries  is a list containing with colnames = date, last_price, volume for each future in the same chain
  #remove those series that have no data
  haveEntries = sapply(futSeries,dim)[1,]>= 1
  futSeries = futSeries[haveEntries]
  #trim the last row of each series
  lastPxCol = na.exclude(match(c("PX_LAST","LAST_PRICE"),toupper(colnames(futSeries[[1]]))))[1]
  volumeCol = match("VOLUME",toupper(colnames(futSeries[[1]])))
  
  futSeries =   lapply(futSeries,function(x){x[1:(nrow(x)-1),]})
  date = sort(unique(do.call(c, lapply(futSeries, function(x){x$date}))))
  
  
  df = futSeries[[1]][rep(1,length(date)),]
  rownames(df) = date; colnames(df) = colnames(futSeries[[1]])
  df[as.character(futSeries[[length(futSeries)]]$date),] =  futSeries[[length(futSeries)]][]
  for (i in (length(futSeries)-1):1){
    fut =  futSeries[[i]]
    ratio  = df[as.character(fut$date[nrow(fut)]),lastPxCol] / fut[,lastPxCol][nrow(fut)]
    fut[,lastPxCol] = fut[,lastPxCol] * ratio ; fut[,volumeCol] = fut[,volumeCol] / ratio ;
    df[as.character(fut$date),] = fut[]
  }

  warning("only Volume data is adjusted for price, other columns are not")
  return(df)
}
