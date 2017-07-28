#' lag a matrix by n days, fill the missing data with the closest one
#' @export
lagMatrix <- function(mat, n = 1){
  output = mat
  if(n==0){
    return(output)
  }else if(n>0){
    output[-1:-n,] = mat[1:(nrow(output)-n),]
    output[1:n, ] = rep(mat[1, ], each = n)
  }else if(n<0){
    output[1:(nrow(output)+n),] = mat[-1:n,]
    output[(nrow(output)+n):nrow(output), ] = rep(mat[nrow(mat), ], each = -n)
  }
  return(output)
}