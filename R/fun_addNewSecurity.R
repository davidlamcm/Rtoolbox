#'  to be deprecated later on
#'  add security to existing rds file
#'  @export


addNewSecurity=function(ticker){
  library("Rbbg")
  conn=blpConnect()
  ticker = as.character(ticker)
  filePath = "data/ticker.rds"
  if(file.exists(filePath)){
    temp=readRDS(filePath)
    ticker.add=ticker[!ticker %in% temp$ticker]
  }else{
    temp =NULL
    ticker.add = ticker
  }
  if(length(ticker.add>0)){
    temp.add = data.frame(ticker= ticker.add,updateTime=Sys.time(),stringsAsFactors = F)
    bb=bdp(conn,temp.add$ticker,"EQY_INIT_PO_DT")
    cc = bdp(conn,temp.add$ticker,"OFFERING_FIRST_LISTING_DATE",override_fields = "OFFERING_TYPE_OVERRIDE",override_values = "IPO")
    dd= bdp(conn,temp.add$ticker,"INTERVAL_START_VALUE_DATE",override_fields = c("START_DATE_OVERRIDE","END_DATE_OVERRIDE"),override_values =c("19000101",format(Sys.Date(),"%Y%m%d")))

    bb[is.na(bb)]= cc[is.na(bb)]
    bb[is.na(bb)]= dd[is.na(bb)]

    temp.add = cbind(temp.add,listingDate = bb)

    if(sum(names(temp.add)!=names(temp))==0){
      temp=rbind(temp,temp.add)
      rownames(temp) = temp$ticker
      saveRDS(temp,file=filePath)
    }else{
      print("didnt add the new data into orginal data as columns didn't match")
    }
  }else{
    print("all tickers exist already, nothing done")
  }
}
