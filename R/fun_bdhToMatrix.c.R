#'c implementation of the bdhToMatrix 
#'return a list of matrix which comprise of a consolidated value matrix and one date matrix given list of bdh data
#'@param bdhList list of bdh data to be converted to matrix
#'@param timeSeries date or character vector that determine the resulting matrix's rownames
#'@param validDays number of days a bdh data is valid since it is published, for price data that only valid for that day, use 1 with lag = 0
#'@param cl cluster that will be used for computation
#'@param lag number of trading day lag, default is 0
#'@export


bdhToMatrix.c <-function(bdhList, timeSeries, validDays =730, lag =0 ){

  if(!class(timeSeries) =="Date"){
    tryCatch({timeSeries = as.Date(timeSeries)}, error=function(){print("timeSeries is not class Date and failed to convert to Date")})
  }
  timeSeries = sort(timeSeries)
  timeSeries.int = as.numeric(timeSeries)
  timeSeries.char = as.character(timeSeries)
  
  #create matrix 
  mappedList = foreach(bdh = bdhList)%do%{
    bdh.dt =bdh$date
    bdh.dt.int = as.numeric(bdh$date)
    vect = bdh[,2]
    dtPoses = matchDate(bdh.dt.int,timeSeries.int,validDays, lag)
    dt = as.character(bdh.dt[dtPoses])
    value = vect[dtPoses]
    list(dt =dt,value=value)
  }
  
  dateMatrix =do.call(cbind,  foreach(ele = mappedList)%do%{
    ele$dt
  })
  bdhMatrix =do.call(cbind,  foreach(ele = mappedList)%do%{
    ele$value
  })
  
  colnames(bdhMatrix) = names(bdhList)
  rownames(bdhMatrix) = timeSeries.char
  return( list(value  = bdhMatrix, date = dateMatrix))
}