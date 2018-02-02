#' @title Select the subgroup of the data and perform regression analysis
#' @description Heterogeneity analysis
#' @author Jiacheng He
#'
#' @param condition The condition to subset the data. A character.
#' @param df The data frame that contains the regression variables
#' @param model The regression model
#' @param cluster Whether to cluster the standard error
#' @param event_plot Whether to generate the event study plot
#' @param xlab The xlabel of the event plot
#' @param ylab The ylabel of the event plot
#'
#' @return A list of regression result (and an event plot if specified)
#' @examples
#'
#' @importFrom dplyr filter_
#' @export

subgroup_regression <- function(condition, df, model, cluster=NULL, event_plot=FALSE, xlab=NULL, ylab=NULL) {

  df <- filter_(df, condition)

  return(regression_analysis(df, model, cluster, event_plot, xlab, ylab))
}
