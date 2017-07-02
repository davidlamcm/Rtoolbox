#' given a matrix of portfolio composition, minimize the turnover by only changing the position if target vs current is larger than certain percentage
#' does not work for negative weight
#' it does not guarantee wgt add up to 100%
#' @param mat matrix of turnover
#' @param thershold percentage of deviation between target and current
#' @param minPositionValue min position of an asset class, else will be zero
#' @param clearanceThershold if theoretical position fall below this thershold, then will clear the existing position

#' @export
#' 
minPctChgTurnover <-function(mat, thershold = 0.2, minPositionValue = 0.01, clearanceThershold =0.005){
  out = mat
  pos = mat[1,]  #before change position
  for(i in 2:(nrow(out))){
    actionFlag= ((abs(pos/mat[i,]-1))>thershold) & (abs(mat[i,]) >minPositionValue)
    out[i, ] = ifelse(actionFlag, mat[i, ], pos)
    out[i, ] = ifelse(abs(mat[i,])<=clearanceThershold, 0,out[i, ]) #set position to zero if position is too small(clearanceThershold)
    pos = out[i, ]
  }
  out
}