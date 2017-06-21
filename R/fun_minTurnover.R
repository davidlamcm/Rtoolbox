#' given a matrix of portfolio composition, minimizee the portfolio turnover
#' @export
#' 
minTurnover <-function(mat, thershold = 0.05){
  out = mat
  pos = mat[1,]
  for(i in 2:(nrow(out))){
    if(sum(abs(pos-mat[i,]))>thershold){
        out[i, ] = mat[i, ]
        pos = mat[i, ]
    }else{
        out[i, ] = pos
      }
  }
  out
}