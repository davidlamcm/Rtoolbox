#'custom funciton for na.locf, alter the behaviour of gap filling with paramter maxlocf
#'some parameter of na.locf is disabled due to imcompetibility with roll_sum
#'@param object object that will be passed to na.locf/roll_sum.
#'@param maxlocf max number of days na.locf will be carried out. Default = 0 , do nothing 
#'@export
na.locfCustom <-function(object, maxlocf = 0){
  out = na.locf(object, na.rm = F)
  indicator = roll_sum(!is.na(object), n = (maxlocf+1), align = "right", na.rm = T, fill = 1)
  out[indicator==0] = NA
  out
  }