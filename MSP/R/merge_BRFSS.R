merge_BRFSS <- function(df_list) {
  
  library(dplyr); library(purrr)
  
  merged_df <- reduce(df_list, bind_rows)
 
  return(merged_df)
}