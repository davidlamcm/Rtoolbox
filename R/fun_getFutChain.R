#' given future code , list of months code , starting year
#' return the list of future code
#' @export

getFutChain <- function(futureCode, months, years){
  df =as.matrix( expand.grid(toupper(months), years))
  return(paste0(toupper(futureCode ), df[,1],df[,2], " Index"))
}
