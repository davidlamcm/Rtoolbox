#' will be deprecate
#' function to download BDH data and store into rds
#' @export
updateByBDH=function(field,CshAdjNormal=F,CshAdjAbnormal=F,CapChg=F,period = "DAILY", override_fields =NULL,override_values =NULL,altname =NULL){
  library("parallel")
  library("Rbbg")
#   conn = blpConnect()
  field = tolower(field)
  para=list()
  para$no_cores=10
  para$field = tolower(field)
  para$override_fields =override_fields
  para$override_values =override_values
  para$period = period
  if(is.null(altname)){
    para$dataFilePath=paste0("data/",para$field,".rds")
    para$replacedFilePath=paste0("data/","replaced_",para$field,".rds")
  }else{
    para$dataFilePath=paste0("data/",tolower(altname),".rds")
    para$replacedFilePath=paste0("data/","replaced_",tolower(altname),".rds")
  }
  para$incremental=file.exists(para$dataFilePath)
  para$universe=readRDS("data/ticker.rds")    ###################implement automatic new additional for new ticker later


  if(para$incremental){
    dataObject=readRDS(para$dataFilePath)
    para$stDate=format(max(as.Date(dataObject$date))+1,format="%Y%m%d")
  }else{
    para$stDate="19991201"
  }
  para$endDate=format(as.Date(Sys.time())-1,format= "%Y%m%d") #up till yesterday
  if(para$endDate>para$stDate){
    cl <- makeCluster(para$no_cores,outfile="data/log.txt")
    clusterEvalQ(cl,{library("rJava"); library("Rbbg");})
    clusterExport(cl,varlist=c("para","CshAdjNormal","CshAdjAbnormal","CapChg","override_fields","override_values"),envir=environment())
    temp=parLapply(cl,split(para$universe$ticker,cut(1:length(para$universe$ticker),para$no_cores,label=FALSE)),function(x){
        conn=blpConnect()
        bdh(conn,x,para$field,para$stDate,para$endDate,option_names =c("periodicitySelection","CshAdjNormal","CshAdjAbnormal","CapChg","nonTradingDayFillOption"),option_values = c(para$period,CshAdjNormal,CshAdjAbnormal,CapChg,"ACTIVE_DAYS_ONLY"),override_fields =para$override_fields,override_values =para$override_values)
    }
    )

    if(exists("temp")){
      result=do.call(rbind.data.frame, temp)
      result=cbind(result,CshAdjNormal=CshAdjNormal,CshAdjAbnormal=CshAdjAbnormal,CapChg=CapChg,updateTime=Sys.time())
      rownames(result)=NULL
      if(exists("dataObject")){
        if(sum(names(result)!=names(dataObject))==0){
          row.orig = nrow(dataObject)
          row.incr = nrow(result)
          dataObject=rbind(dataObject,result)
          saveRDS(dataObject,file=para$dataFilePath)
          print(paste("orig.row=",row.orig,":incr.row",row.incr,":row.new=",dim(dataObject)[1], ":path=", para$dataFilePath))
        }else{
          print("didnt add the new data into orginal data as columns didn't match")
        }
      }else{
        row.orig = 0
        row.incr = nrow(result)
        dataObject=result
        saveRDS(dataObject,file=para$dataFilePath)
        print(paste("orig.row=",row.orig,":incr.row",row.incr,":row.new=",dim(dataObject)[1], ":path=", para$dataFilePath))
      }
    }else{
      print("nothing returned for existing tickers")
    }
    stopCluster(cl)
    gc()
  }else{
    print("nothing done: para$endDate<=para$stDate")
  }
}
