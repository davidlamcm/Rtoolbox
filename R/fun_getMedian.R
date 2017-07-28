#' given a data matrix, return the median time series 
#' @param data data matrix containing stock data time series 
#' @param minCount minimum effective count to calculate median 
#' @export

getMedian <- function(data,minCount = 1){
  out = apply(data,1, median, na.rm= T)
  out[rowSums(!is.na(data))< minCount] = NA
  names(out) = rownames(data)
  out 
  }