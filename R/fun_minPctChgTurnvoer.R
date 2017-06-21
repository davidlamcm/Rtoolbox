#' given a matrix of portfolio composition, minimize the turnover by only changing the position if target vs current is larger than certain percentage
#' does not work for negative weight
#' it does not guarantee wgt add up to 100%
#' @param mat matrix of turnover
#' @param thershold percentage of deviation between target and current
#' @param minvalue min size of change in any case
#' @export
#' 
minPctChgTurnover <-function(mat, thershold = 0.2, minvalue = 0.005){
  out = mat
  pos = mat[1,]
  for(i in 2:(nrow(out))){
    actionFlag= ((abs(pos/mat[i,]-1))>thershold) & (abs(pos - mat[i,]) >minvalue)

    out[i, ] = ifelse(actionFlag, mat[i, ], pos)
    pos = mat[i, ]
  }
  out
}