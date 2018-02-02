#' @title Merge the list of the BRFSS data frames into a single data frame
#' @description Use this function to integrate the BRFSS data frames into a single data frame
#' @author Jiacheng He
#'
#' @param df_list A list of BRFSS data frames
#'
#' @return A data frame
#' @examples
#' lapply(1995:2010, read_BRFSS) %>% merge_BRFSS()
#'
#' @importFrom dplyr bind_rows
#' @importFrom purrr reduce
#' @export



merge_BRFSS <- function(df_list) {

  merged_df <- reduce(df_list, bind_rows)

  return(merged_df)
}
