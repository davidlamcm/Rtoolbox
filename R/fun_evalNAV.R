#' given a time seeries of NAV  and benchmark (optional),
#' compute the statistics of the time series
#' @export

evalNAV <-function(NAV,benchmark=NULL){
  if(is.null(benchmark)){
    print("benchmark is not provided, beta will not be calculated")
  }else{
    if(!"xts" %in% class(benchmark)){
      stop("benchmark has to be 'xts' object")}
    if(sum(is.na(benchmark))>0){
      stop("benchmark contain NAs")}
  }
  
  if(!"xts" %in% class(NAV) ){
    stop("NAV has to be 'xts' object")}
  if(sum(is.na(NAV)) >0){
    stop("NAV contain NAs")
  }
  df = NAV
  #df=df[max(min(which(df$NAV != 1))-1,1):nrow(df),]
  df= df/as.numeric(df[1,])
  ret = df/lag(df,k=1)
  ret[1, ]=1
  
  if(!is.null(benchmark)){
    bm= benchmark/as.numeric(benchmark[1,])
    bm.ret  = bm/lag(bm,k=1)
    bm.ret[1, ]=1
  }
  
  df.w = df[head(endpoints(df,on = "weeks")+1,-1), ] ;
  ret.w=lag(df.w,k=1);
  ret.w[1, ]=1
  
  df.m = df[head(endpoints(df,on = "months")+1,-1), ] ;
  ret.m=lag(df.m,k=1);
  ret.m[1, ]=1
  
  stats <-list()
  #NAV
  stats[["NAV"]] =df[nrow(df), ,drop=F]
  #CAGR
  stats[["CAGR"]] = matrix(stats[["NAV"]]^(1/stats[["Year"]] )-1, nrow = 1)
  #SharpeRatio
  stats[["SharpeRatio"]] =  SharpeRatio.annualized(ret-1, scale=nrow(df)/ stats[["Year"]][1])
  #SD
  stats[["SD"]] = matrix(apply(ret,MARGIN = 2,sd), nrow=1)
  #SD.annunlized
  stats[["SD.annualized"]] =matrix(stats[["SD"]] *sqrt(nrow(df)/ stats[["Year"]]), nrow =1)
  #MDD & MDD Date
  stats[["MDD"]] = maxDrawdown(ret-1)
  
  #Downside Deviation
  stats[["DownsideDeviation"]] =DownsideDeviation(ret-1)
  #average DD
  stats[["averageDD"]] = AverageDrawdown(ret-1)
  #average DD length
  stats[["averageLength"]] =AverageLength(ret-1)
  #averageRecovery
  stats[["averageRecovery"]] =AverageRecovery(ret-1)
  #duration
  stats[["Year"]]= matrix(rep( as.numeric(index(df)[nrow(df)]-index(df)[1])/365.25,ncol(df)),nrow=1)
  #WorstDailyReturn
  stats[["WorstDailyReturn"]] =matrix( apply(ret,2,min),nrow=1)
  #WorstWeeklyReturn
  stats[["WorstWeeklyReturn"]] =    matrix( apply(ret.w,2,min),nrow=1)
  #WorstMonthlyReturn
  stats[["WorstMonthlyReturn"]] = matrix(  apply(ret.m,2,min),nrow=1)
  #DailyReturn
  stats[["DailyReturn"]] = matrix(stats[["NAV"]]^(1/nrow(df) )-1, nrow=1)
  #WeeklyReturn
  stats[["WeeklyReturn"]] = matrix(stats[["NAV"]]^(1/nrow(df.w) )-1, nrow=1)
  #DailyWinRate
  stats[["DailyWinRate"]] = matrix(colSums(ret>1)/colSums(ret!=1), nrow=1)
  #WeeklyWinRate
  stats[["WeeklyWinRate"]] = matrix(colSums(ret.w>1)/colSums(ret.w!=1), nrow=1)
  #MonthlyWinRate
  stats[["MonthlyWinRate"]] =  matrix(colSums(ret.m>1)/colSums(ret.m!=1), nrow=1)

  if(!is.null(benchmark)){
    #BETA
    stats[["Beta"]] = matrix(c(CAPM.beta(ret-1,bm.ret-1),1),nrow=1)
    #BETA.bull
    stats[["Beta.bull"]] = matrix(c(CAPM.beta.bull(ret-1,bm.ret-1),1),nrow=1)
    #BETA.bear
    stats[["Beta.bear"]] = matrix(c(CAPM.beta.bear(ret-1,bm.ret-1),1),nrow=1)
    #Tracking Error
    stats[["TrackingError"]] =matrix( c(TrackingError(ret-1,bm.ret-1, nrow(df)/stats[["Year"]][1]),0),nrow=1)
    #ExcessReturn
    stats[["ExcessAnnualizedReturn"]] =matrix(c(Return.annualized.excess(ret-1,bm.ret -1,scale = nrow(df)/ stats[["Year"]][1]),0),nrow=1)
    #InformationRatio
    stats[["InformationRatio"]] =  matrix(c(InformationRatio(ret-1,bm.ret-1, scale =nrow(df)/stats[["Year"]][1]),NA),nrow=1)
    
  }else{
    #BETA
    stats[["Beta"]] = matrix(rep("no BM",ncol(df)),nrow=1)
    #BETA.bull
    stats[["Beta.bull"]] = matrix(rep("no BM",ncol(df)),nrow=1)
    #BETA.bear
    stats[["Beta.bear"]] =  matrix(rep("no BM",ncol(df)),nrow=1)
    #Tracking Error
    stats[["TrackingError"]] = matrix(rep("no BM",ncol(df)),nrow=1)
    #ExcessReturn
    stats[["ExcessAnnualizedReturn"]] =  matrix(rep("no BM",ncol(df)),nrow=1)
    #InformationRatio
    stats[["InformationRatio"]] = matrix(rep("no BM",ncol(df)),nrow=1)
    
  }
  
  out = do.call(cbind.data.frame, lapply(stats,t))
  return(out)
}
