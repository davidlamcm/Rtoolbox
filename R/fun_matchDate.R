# #map one date series to the position of another date series 
# #helper function for bdhToMatrix convertion 
# cppFunction('IntegerVector matchDate(IntegerVector refSeries, IntegerVector tsSeries, int validDays, int lag){
# int n = refSeries.size();
# int m = tsSeries.size();
# int counter =0 ;
# int lagAdj = 0 ;
# IntegerVector out(m);
# 
# for(int j = 0; j < m; j++) {out[j] = NumericVector::get_na();}
# for(int j = 0; j < m; j++){
#   if(j<=lag){
#     lagAdj = j;
#   }
#   for(int i = counter ; i <n; i++){
#     if(tsSeries[j-lagAdj] >= refSeries[i]) {
#         for(int k = i+1; k<n; k++){
#           if(tsSeries[j-lagAdj] < refSeries[k]){
#             i = k - 1;
#             break;
#           }
#         }
#         counter = i;   
#         if((tsSeries[j] - refSeries[i]) < validDays){
#           out[j] = counter +1;
#       }
#     }else{  
#       break;
#     }
#   }
# }
# return out;
# }')
# 
