#' given a matrix of portfolio ? and price matrix, compute the NAV of a portoflio
#' to be rebuilt / deprecated later
#' @export
computeNAV <- function(portfolio, price, vwap){
  ROR <-function(DF){
    DF = na.locf(DF, na.rm= F)
    temp = DF/(rbind(NA,DF)[-(nrow(DF)+1),])
    rownames(temp) =rownames(DF)
    temp[1,]=0
    temp[is.na(temp)]= 0
    return(temp)
  }
  if(missing(vwap)){vwap = price}
  temp.ticker = sort(intersect(colnames(portfolio),intersect(colnames(vwap),colnames(price))))
  temp.date = sort(intersect(rownames(portfolio),intersect(rownames(vwap),rownames(price))))
  if(length(temp.ticker)< dim(portfolio)[2]){stop(paste(temp.ticker[!temp.ticker %in% rownames(portfolio)], "price/vwap info is not found"))}
  portfolio.1 = portfolio[temp.date, temp.ticker]
  price.1 = price[temp.date, temp.ticker] ; price.1 = na.locf(price.1,na.rm=F)
  vwap.1 = vwap[temp.date, temp.ticker]; vwap.1 = na.locf(vwap.1,na.rm=F)
  ret.1 = ROR(price.1)
  daytradeReturn  = (price.1/vwap.1) ; daytradeReturn[is.na(daytradeReturn)]=1; daytradeReturn[abs(daytradeReturn-1)>=0.2] = 1 #screen out data that vwap vs price deviate more than 20%
  portfolio.diff = diff(portfolio.1)
  portfolio.diff = rbind(0,portfolio.diff)
  portfolioCF = portfolio.1 ; portfolioCF[]= rbind(0,portfolio.1[-nrow(portfolio.1),])
  price.ret = rowSums(portfolioCF *(ret.1-1)) #used carried 1 day forward's portfolio to calculate return
  vwap.ret = rowSums(portfolio.diff * (daytradeReturn-1))
  NAV = cumprod(price.ret  + vwap.ret +1)
  return(data.frame(date = as.Date(temp.date),NAV= NAV, total.ret = price.ret+vwap.ret, price.ret = price.ret, vwap.ret = vwap.ret, position =rowSums(portfolio)))
}
