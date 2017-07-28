#' this function will be deprecated
#' given fieldname , retrieve the rds file form the corresponding directory
#' @export
rdsToList <-function(fieldname, filename = NULL){
  library(plyr)

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
    temp = subset(dataObject, select= c("ticker",para$fieldname))
    result = dlply(temp, .(ticker))
      return(result)
  }else{
    print("there are duplicates, nothing done")
  }
}
