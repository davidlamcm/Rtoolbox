#' given a list of stocks with data from bdh function, convert the list into a table 
#' cl is the cluster created by doParallel , if this parameter is not given , the function will spawn the cluster itself
#' @export

bdhLongToWide <-function(bdhList, cl){
  if( missing(cl)){
    cl <- makeCluster(detectCores()-1)
    registerDoParallel(cl)
  }


combineDate = foreach(bdh = bdhList) %dopar% {
  bdh$date
}
combineDate = sort(as.Date(unique(as.character(unlist(combineDate)))))
convertedBDH  = foreach(bdh = bdhList) %dopar%{
  output = rep(NA, length(combineDate))
  names(output) = combineDate
  output[as.character(bdh$date)] = bdh[,2]
  output
}
result = do.call(cbind,convertedBDH)
colnames(result)  =names(bdhList) 
result
}