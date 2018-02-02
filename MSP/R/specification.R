#' @title Specify the regression model
#' @description Specify the regression model
#' @author Jiacheng He
#'
#' @param LHS The LHS outcome variable name
#' @param model Model type: "DID" or "event_study"
#' @param controls Whether to include controls
#'
#' @return An expression of the regression specification
#' @examples
#'
#' @export


specification <- function(LHS, model, controls=FALSE) {

  if (model=="DID") {
    RHS <- "state + year + post_treatment"
  } else if (model == "event_time") {
    RHS <- "state + year + event_time"
  } else if (model == "event_month") {
    RHS <- "state + year + IMONTH + event_month"
  } else {
    stop('Model specification is either "DID" or "event_time" or "event_month')
  }

  if (controls==TRUE) {
    RHS <- paste(RHS, "+ AGE + SEX + EDUCA + MARITAL + EMPLOY")
  }

  equation <- paste(LHS, "~", RHS)

  return(list(equation = equation, model = model, y = LHS))
}
