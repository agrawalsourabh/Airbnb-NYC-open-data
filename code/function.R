# Function to find the missing values in the data set
get_missing_data_df = function(x){
  column_names = c()
  missing_count = c()
  missing_percent = c()
  
  for (i in 1:ncol(x)) {
    if(sum(is.na(x[i])) > 0){
      column_name = names(x[i])
      count = sum(is.na(x[i]))
      per = round((count*100 / nrow(x)), 2)
      
      column_names = c(column_names, column_name)
      missing_count = c(missing_count, count)
      missing_percent = c(missing_percent, per)
    }
  }
  
  missing_df = data.frame(column_name = column_names, missing_count = missing_count, 
                          missing_percent = missing_percent)
  
  return(missing_df)
}