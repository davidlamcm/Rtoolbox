#' given a matrix of portfolio composition, minimizee the portfolio turnover
#' @export
#' 
minTurnover <-function(mat, thershold = 0.05){
  out = mat
  pos = mat[1,]
  for(i in 1:(nrow(out)-1)){
    if(sqrt(sum((pos-mat[i,])**2))>thershold){
        out[i, ] = mat[i, ]
        pos = mat[i, ]      
    }else{
        out[i, ] = pos
      }
  }
  out
}
