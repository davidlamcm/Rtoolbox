#' to be deprecated later
#' given a data.frame of Fiscal date map it to actual date
#' was used to map eps , but doesnt seem very useful now
#' @export
dateRemap <-function(Fiscal_df, Actual_df){
  Actual_df$announcement_dt = as.character(as.Date(as.character(Actual_df$announcement_dt), format="%Y%m%d"))
  a.temp = paste(Actual_df[,"ticker"], Actual_df[,"date"])
  f.temp = paste(Fiscal_df[,"ticker"], Fiscal_df[,"date"])
  join.temp = intersect(paste(Actual_df[,"ticker"], Actual_df[,"date"]),paste(Fiscal_df[,"ticker"], Fiscal_df[,"date"]))
  Actual_df = Actual_df[join.temp %in% a.temp,]
  Fiscal_df = Fiscal_df[join.temp %in% f.temp,]
  Actual_df = Actual_df[match(join.temp, paste(Actual_df[,"ticker"], Actual_df[,"date"])),]
  Fiscal_df = Fiscal_df[match(join.temp, paste(Fiscal_df[,"ticker"], Fiscal_df[,"date"])),]
  Fiscal_df[,"date"] =  Actual_df[,"announcement_dt"]
  row.names(Fiscal_df) = NULL
  return(Fiscal_df)
  }
