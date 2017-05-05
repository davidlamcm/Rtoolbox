#' given a list of futures chain series , concatenate them into one single time serie
#' the list of futures chain series must have the fields c("last_price","volume") or else wont work
#' @export

combineFutSeries <-function(futSeries ){
  #futSeries  is a list containing with colnames = date, last_price, volume for each future in the same chain
  #remove those series that have no data
  haveEntries = sapply(futSeries,dim)[1,]>= 1
  futSeries = futSeries[haveEntries]
  #trim the last row of each series
  futSeries =   lapply(futSeries,function(x){x[1:(nrow(x)-1),]})
  date = sort(unique(do.call(c, lapply(futSeries, function(x){x$date}))))
  df = data.frame(last_price = rep(NA,length(date)), volume = rep(NA, length(date)), row.names = date)
  df[as.character(futSeries[[length(futSeries)]]$date),"last_price"] =  futSeries[[length(futSeries)]]$last_price

    df[as.character(futSeries[[length(futSeries)]]$date),"volume"] =  futSeries[[length(futSeries)]]$volume
  for (i in (length(futSeries)-1):1){
      fut =  futSeries[[i]]
      ratio  = df[as.character(fut$date[nrow(fut)]),"last_price"] / fut$last_price[nrow(fut)]
      fut$last_price = fut$last_price * ratio ; fut$volume = fut$volume / ratio ;
      df[as.character(fut$date),] = fut[, c("last_price", "volume")]
  }

  return(df)
}
