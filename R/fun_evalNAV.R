#' given a time seeries of NAV  and benchmark (optional),
#' compute the statistics of the time series
#' @export


evalNAV <-function(NAV,benchmark){
  library("PerformanceAnalytics");  library("zoo"); library("xts")
  if(missing(benchmark)){
    benchmark=NAV
    print("benchmark is not provided, will use the NAV as its own benchmark")
    }
  if(!"zoo" %in% class(NAV) | !"zoo" %in% class(benchmark)){
    stop("NAV and benchmark has to be 'zoo' object")}
  if(sum(is.na(NAV)) >0){
    stop("NAV contain NAs")
  }else if(sum(is.na(benchmark))>0){
    stop("benchmark contain NAs")
  }

  NAV.1 = NAV
  benchmark.1 = benchmark

  df = merge(NAV.1,  benchmark.1, all = F)
  colnames(df) = c("NAV", "bm")
  df=df[max(min(which(df$NAV != 1))-1,1):nrow(df),]
  df[,"bm"]= df[,"bm"]/as.numeric(df[1,"bm"])
  df[,"NAV"]= df[,"NAV"]/as.numeric(df[1,"NAV"])
  return.1 = df/lag(df,k=1)

  colnames(return.1) = c("NAV.ret","bm.ret")
  df =merge(df, return.1)
  df[1, c("NAV.ret","bm.ret")]=1

  df.w = df[head(endpoints(df,on = "weeks")+1,-1), c("NAV", "bm")] ; df.w=merge(df.w,df.w[,c("NAV","bm")]/lag(df.w[,c("NAV","bm")],k=-1));colnames(df.w)=c("NAV","bm","NAV.ret","bm.ret"); df.w[1, c("NAV.ret","bm.ret")]=1
  df.m = df[head(endpoints(df,on = "months")+1,-1), c("NAV", "bm")] ; df.m=merge(df.m,df.m[,c("NAV","bm")]/lag(df.m[,c("NAV","bm")],k=-1));colnames(df.m)=c("NAV","bm","NAV.ret","bm.ret"); df.m[1, c("NAV.ret","bm.ret")]=1



  stats <-list()
  #MDD & MDD Date
  stats[["MDD"]] = maxDrawdown(df[,c("NAV.ret","bm.ret"),drop = FALSE]-1)
  #average DD
  stats[["averageDD"]] = AverageDrawdown(df[,c("NAV.ret","bm.ret"),drop = FALSE]-1)
  #average DD length
  stats[["averageLength"]] =AverageLength(df[,c("NAV.ret","bm.ret"),drop = FALSE]-1)
  #averageRecovery
  stats[["averageRecovery"]] =AverageRecovery(df[,c("NAV.ret","bm.ret"),drop = FALSE]-1)
  #duration
  stats[["Year"]]= matrix(rep( as.numeric(index(df)[nrow(df)]-index(df)[1])/365.25,2), ncol = 2, dimnames = list("Year", c("NAV.ret","bm.ret")))
  #NAV
  stats[["NAV"]] =matrix(df[nrow(df), c("NAV","bm")], ncol = 2, dimnames = list("NAV", c("NAV.ret","bm.ret")))

  #SD
  stats[["SD"]] = matrix(sapply(df[,c("NAV.ret","bm.ret")],sd), ncol = 2, dimnames = list("SD", c("NAV.ret","bm.ret")))
  #SD.annunlized
  stats[["SD.annualized"]] =matrix(stats[["SD"]] *sqrt(nrow(df)/ stats[["Year"]]), ncol = 2, dimnames = list("SD.annualized", c("NAV.ret","bm.ret")))
  #Downside Deviation
  stats[["DownsideDeviation"]] =DownsideDeviation(df[,c("NAV.ret","bm.ret"),drop = FALSE]-1)
  #BETA
  stats[["Beta"]] = c(CAPM.beta(df[,c("NAV.ret"),drop = FALSE]-1,df[,c("bm.ret"),drop = FALSE]-1),1)
  #BETA.bull
  stats[["Beta.bull"]] = c(CAPM.beta.bull(df[,c("NAV.ret"),drop = FALSE]-1,df[,c("bm.ret"),drop = FALSE]-1),1)
  #BETA
  stats[["Beta.bear"]] = c(CAPM.beta.bear(df[,c("NAV.ret"),drop = FALSE]-1,df[,c("bm.ret"),drop = FALSE]-1),1)

  #Tracking Error
  stats[["TrackingError"]] = c(TrackingError(df[,c("NAV.ret"),drop = FALSE]-1,df[,c("bm.ret"),drop = FALSE]-1, nrow(df)/stats[["Year"]][1]),0)
  #WorstDailyReturn
  stats[["WorstDailyReturn"]] = c(min(df[,c("NAV.ret"),drop = FALSE]-1),min(df[,c("bm.ret"),drop = FALSE]-1))
  #WorstWeeklyReturn
  stats[["WorstWeeklyReturn"]] = c(min(df.w[,c("NAV.ret"),drop = FALSE]-1),min(df.w[,c("bm.ret"),drop = FALSE]-1))
  #WorstMonthlyReturn
  stats[["WorstMonthlyReturn"]] = c(min(df.m[,c("NAV.ret"),drop = FALSE]-1),min(df.m[,c("bm.ret"),drop = FALSE]-1))
  #DailyReturn
  stats[["DailyReturn"]] = matrix(stats[["NAV"]]^(1/nrow(df) )-1, ncol = 2, dimnames = list("DailyReturn", c("NAV.ret","bm.ret")))
  #WeeklyReturn
  stats[["WeeklyReturn"]] = matrix(stats[["NAV"]]^(1/nrow(df.w) )-1, ncol = 2, dimnames = list("WeeklyReturn", c("NAV.ret","bm.ret")))
  #DailyWinRate
  stats[["DailyWinRate"]] = matrix(colSums(df[, c("NAV.ret","bm.ret")]>1) /(colSums(df[, c("NAV.ret","bm.ret")]!=1)), ncol = 2, dimnames = list("DailyWinRate", c("NAV.ret","bm.ret")))
  #WeeklyWinRate
  stats[["WeeklyWinRate"]] = matrix(colSums(df.w[, c("NAV.ret","bm.ret")]>1) /(colSums(df.w[, c("NAV.ret","bm.ret")]!=1)), ncol = 2, dimnames = list("WeeklyWinRate", c("NAV.ret","bm.ret")))
  #MonthlyWinRate
  stats[["MonthlyWinRate"]] = matrix(colSums(df.m[, c("NAV.ret","bm.ret")]>1) /(colSums(df.m[, c("NAV.ret","bm.ret")]!=1)), ncol = 2, dimnames = list("MonthlyWinRate", c("NAV.ret","bm.ret")))


  #CAGR
  stats[["CAGR"]] = matrix(stats[["NAV"]]^(1/stats[["Year"]] )-1, ncol = 2, dimnames = list("CAGR", c("NAV.ret","bm.ret")))
  #ExcessReturn
  stats[["ExcessAnnualizedReturn"]] =c(Return.annualized.excess(df[,c("NAV.ret"),drop = FALSE]-1,df[,c("bm.ret"),drop = FALSE]-1,scale = nrow(df)/ stats[["Year"]][1]),0)
  #SharpeRatio
  stats[["SharpeRatio"]] =  SharpeRatio.annualized(df[, c("NAV.ret","bm.ret")]-1, scale=nrow(df)/ stats[["Year"]][1])
  #InformationRatio
  stats[["InformationRatio"]] =  c(InformationRatio(df[,c("NAV.ret"),drop = FALSE]-1,df[,c("bm.ret"),drop = FALSE]-1, scale =nrow(df)/stats[["Year"]][1]),NA)

  result = list()
  result[["stats"]] = do.call(rbind,stats)
  return(result)
}
