#' give the index ticker, and date (optional) return the list of stock wgt from bloomberg
#' @export
getIndexMember = function(index, date){
  if(missing(date)){
    date = format(Sys.Date(), "%Y%m%d")
    }
  library("Rblpapi")
  require("reshape2")

  con <- blpConnect()
  return(bds(index, "INDX_MWEIGHT_HIST",  overrides =c("END_DATE_OVERRIDE"=date)))
}

