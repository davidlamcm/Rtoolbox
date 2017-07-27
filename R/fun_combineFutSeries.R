#' given a list of futures chain series , concatenate them into one single time serie
#' the list of futures chain series must have the fields c("last_price","volume") or else wont work
#' @export

combineFutSeries <-function(futSeries ){
  #futSeries  is a list containing with colnames = date, last_price, volume for each future in the same chain
  #remove those series that have no data
  haveEntries = sapply(futSeries,dim)[1,]>= 1
  futSeries = futSeries[haveEntries]
  #trim the last row of each series
  lastPxCol = match("LAST_PRICE",toupper(colnames(futSeries[[1]])))
  volumeCol = match("VOLUME",toupper(colnames(futSeries[[1]])))
  
  futSeries =   lapply(futSeries,function(x){x[1:(nrow(x)-1),]})
  date = sort(unique(do.call(c, lapply(futSeries, function(x){x$date}))))
  df = data.frame(last_price = rep(NA,length(date)), volume = rep(NA, length(date)), row.names = date)
  df[as.character(futSeries[[length(futSeries)]]$date),lastPxCol] =  futSeries[[length(futSeries)]][,lastPxCol]

    df[as.character(futSeries[[length(futSeries)]]$date),volumeCol] =  futSeries[[length(futSeries)]][,volumeCol] 
  for (i in (length(futSeries)-1):1){
      fut =  futSeries[[i]]
      ratio  = df[as.character(fut$date[nrow(fut)]),lastPxCol] / fut[,lastPxCol][nrow(fut)]
      fut[,lastPxCol] = fut[,lastPxCol] * ratio ; fut[,volumeCol] = fut[,volumeCol] / ratio ;
      df[as.character(fut$date),] = fut[, c(lastPxCol,volumeCol)]
  }

  return(df)
}
