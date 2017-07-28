#' given a data in long form , covert it to wide form
#' will improve the function later before being used again
#' @export

dfToMatrix = function(df, fieldname, locf = F){
  require("reshape2")
  para = list()
  para$fieldname = tolower(fieldname)
  dataObject=df
  if(sum(duplicated(paste(dataObject$date,dataObject$ticker)))==0){
    result=acast(dataObject,formula=date ~ ticker,value.var =para$fieldname)
    mode(result)= "numeric"
    if(locf){
      return(na.locf(result,na.rm=FALSE))
    }else{
      return(result)
    }
  }else{
    print("there are duplicates, nothing done")
  }
}
