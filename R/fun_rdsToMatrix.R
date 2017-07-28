#' will be deprecated ,
#' dont know the use
#' @export

rdsToMatrix = function(fieldname, filename = NULL, locf = F){
  require("reshape2")

  para = list()
  if(is.null(filename)){
    para$filename = tolower(fieldname)
  }else{
    para$filename = tolower(filename)
  }
  para$fieldname = tolower(fieldname)
  para$dataFilePath=paste0("data/",para$filename,".rds")
  print(filename)
  print(is.null(filename))
  print(para$dataFilePath)
  dataObject=readRDS(para$dataFilePath)
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
