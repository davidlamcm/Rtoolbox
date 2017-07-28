#' to be depercated later
#' given params : capital ... etc
#' and list of stock / vwap / turnover
#' output the resulting portofolio in a matrix
#' @export
createPortfolio <- function(params, stockList, vwap, turnover){
  fundTargetWgt = function(wgtVector, testValue){
    if(missing(testValue)){
      testValue=sort(unique(c(wgtVector,0)), decreasing = T)
    } #testValue has to be decreasing
    if(sum(pmin(wgtVector,testValue[1]))>=1){
      ret1=fundTargetWgt(wgtVector,testValue[-1])
      if(ret1 ==0){
        return(testValue[2] + (1-sum(pmin(wgtVector,testValue[2])))/sum(wgtVector>testValue[2]))
      }else{
        return(ret1)
      }
    }else{
      return(0)
    }
  }

  library("RcppRoll")
  tickerNames = (intersect(colnames(stockList), intersect( colnames(vwap), colnames(turnover))))
  tickerNames = sort(intersect(colnames(stockList)[colSums(stockList, na.rm = T)!=0],tickerNames))
  date = intersect(rownames(stockList), intersect( rownames(vwap), rownames(turnover)))
  stockList.1 = stockList[date, tickerNames]
  vwap.1 = vwap[date, tickerNames]
  portfolio.target = stockList.1 ; portfolio.target[] = 0
  portfolio.targetWfConstraint = stockList.1 ; portfolio.targetWfConstraint[] = 0
  portfolio.actual = stockList.1 ; portfolio.actual[] = 0



  if(!is.null(params$capital)){
    turnover.1 = turnover[date, tickerNames]
    turnover.1[is.na(turnover.1)] = 0
    turnover.1 = turnover.1 / params$capital
    temp.n = 21 #used 21 days for roll mean to calculate monthly average turnover
    temp = turnover.1; temp[is.na(temp)] = 0 ; temp = roll_mean(temp, n = temp.n)
    turnover.estAvg = turnover.1; turnover.estAvg[]= 0 ; turnover.estAvg[-(1:temp.n), ] = temp[-nrow(temp),]
  }else{stop("capital missing")}

  if(!is.null(params$maxNumStock)){
    portfolio.target[stockList.1 <= params$maxNumStock] = 1
  }else{stop("maxNumStock missing")}

  if(!is.null(param$weigthing)){
    if(tolower(param$weigthing) == "ew"){
      portfolio.targetWgt  = portfolio.target * matrix(rep(ifelse(rowSums(portfolio.target)==0, 0 , 1/rowSums(portfolio.target)), ncol(portfolio.target)), ncol = ncol(portfolio.target))
    }else{stop("weighting method not defined")}
  }else{stop("maxNumStock missing")}

  if(!is.null(params$volInlinePct) & !is.null(params$volInlineDays) & !is.null(param$singleStockCap)){ #calculate the target underconstraint
    portfolio.cap = turnover.estAvg * params$volInlineDays * params$volInlinePct #estimate max size of a position given average turnover
    for(i in 1:nrow(stockList.1)){
      portfolio.targetWfConstraint[i,] = pmin(portfolio.cap[i,], portfolio.targetWgt[i,])
      if(sum(portfolio.targetWfConstraint[i,])<1){
        targetWgt = fundTargetWgt(portfolio.cap[i,]*portfolio.target[i,])
        portfolio.targetWfConstraint[i,] = pmin(portfolio.cap[i,]*portfolio.target[i,], min(targetWgt,param$singleStockCap))
      }
    }
  }else{stop("volInlinePct or volInlineDays or singleStockCap missing")}

  for(i in 2:nrow(stockList.1)){
    diff = portfolio.targetWfConstraint[i,] - portfolio.actual[i-1,]
    change = sign(diff)*pmin(turnover.1[i,]*params$volInlinePct,abs(diff))
    portfolio.actual[i, ]= portfolio.actual[i -1 , ] + change
  }
  result  = list()
  result$target = portfolio.target
  result$targetWfConstraint =portfolio.targetWfConstraint
  result$actual = portfolio.actual
  return(result)
}


