#"to be deprecated later please use bdhToMatrix.c
#'return a list of matrix which comprise of a consolidated value matrix and one date matrix given list of bdh data
#'@param bdhList list of bdh data to be converted to matrix
#'@param timeSeries date or character vector that determine the resulting matrix's rownames
#'@param validDays number of days a bdh data is valid since it is published, for price data that only valid for that day, use 1 with lag = 0
#'@param cl cluster that will be used for computation
#'@param lag number of day lag, default is 0
#'@export
bdhToMatrix <-function(bdhList, timeSeries, validDays =Inf, lag =0 , cl){
  warning("to be deprecated later, please use c++ speed up version bdhToMatrix.c")
  if( missing(cl)){
    cl <- makeCluster(detectCores()-1)
    registerDoParallel(cl)
  }
  
  if(!class(timeSeries) =="Date"){
    tryCatch({timeSeries = as.Date(timeSeries)}, error=function(){print("timeSeries is not class Date and failed to convert to Date")})
  }
  timeSeries = sort(timeSeries)
  timeSeries.char = as.character(timeSeries)
  
  #create matrix 
  dateList = foreach(ticker = names(bdhList))%dopar%{
    bdh.dt = bdhList[[ticker]]$date
    dtPoses = do.call(c, foreach(dt = timeSeries)%do%{
      dtPos = max(which(dt>=(bdh.dt+lag)))
    })
    as.character(bdh.dt[dtPoses])
  }
  dateMatrix = do.call(cbind,dateList)
  filter = apply(dateMatrix,2,function(j){(timeSeries - as.Date(j))< validDays})
  dateMatrix[!filter] = NA
  colnames(dateMatrix) = names(bdhList)
  rownames(dateMatrix) = timeSeries.char
  temp = lapply(names(bdhList),  function(ticker){ 
    vect = bdhList[[ticker]][,2]
    names(vect)  = bdhList[[ticker]]$date
    vect[dateMatrix[,ticker]]
  })
  bdhMatrix = do.call(cbind,temp)
  
 
  colnames(bdhMatrix) = colnames(dateMatrix)
  rownames(bdhMatrix) = rownames(dateMatrix)
  return( list(value  = bdhMatrix, date = dateMatrix))
}