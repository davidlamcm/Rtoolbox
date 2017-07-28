#' given future code , list of months code , starting year
#' return the list of future code
#' @export
#' @param futureCode future code such as HC/HI/XU/ES
#' @param months a vector of months of the future chain e.g.c("F","G","H","J","K","M","N","Q","U","V","X","Z")
#' @param years a vector of years eq. c(99:16,7)

getFutChain <- function(futureCode, months=c("F","G","H","J","K","M","N","Q","U","V","X","Z"), years){
  df =as.matrix( expand.grid(toupper(months), years))
  return(paste0(toupper(futureCode ), df[,1],df[,2], " Index"))
}
