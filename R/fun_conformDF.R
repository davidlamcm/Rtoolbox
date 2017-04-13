#' convert one data.frame to the format of target data.frame
#' S stands for Same, U stands for Union, I stands for Intersect
#' @export



conformDF = function(from , to, locf = F, type = "S"){
  #type: S stands for Same, U stands for Union, I stands for Intersect
  if(class(from) != class(to)){stop("cannot conform object of different class")}
  uRow = sort( union(rownames(from), rownames(to)))
  iRow = sort( intersect(rownames(from), rownames(to)))
  result = matrix(NA, ncol = ncol(from), nrow = length(uRow))
  rownames(result) = uRow
  colnames(result) = colnames(from)
  result[rownames(from) , colnames(from)] = from
  if(locf){result = na.locf(result, na.rm = F )}
  if(type =="S"){
    result = result[rownames(to),]
  }else if(type =="I"){
    result = result[iRow,]
  }else if(type =="U"){
    result = result[uRow,]
  }
  result
}
