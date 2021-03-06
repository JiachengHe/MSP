#' @title Run regression
#' @description Write all regression procedure in this file
#' @author Jiacheng He
#'
#' @param df The data frame that contains the regression variables
#' @param model The regression model
#' @param cluster Whether to cluster the standard error. A character of the cluster variable name
#' @param event_plot Whether to generate the event study plot
#' @param xlab The xlabel of the event plot
#' @param ylab The ylabel of the event plot
#'
#' @return A list of regression result (and an event plot if specified)
#' @examples
#'
#' @import ggplot2
#' @importFrom dplyr '%>%'
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr add_row
#' @importFrom lmtest coeftest
#' @importFrom plm vcovHC
#' @importFrom tidyr replace_na
#' @importFrom broom tidy
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @export


regression_analysis <- function(df, model, cluster=NULL, event_plot=FALSE, xlab=NULL, ylab=NULL) {

  fit <- lm(model$equation, weights = wt, data = df)

  N <- length(fit$residuals)

  if (is.null(cluster)) {
    reg_matrix <- tidy(fit, conf.int = TRUE)
    fit <- summary(fit)
  } else {
    fit <- fit %>%
      coeftest(vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = df[[cluster]]))

    reg_matrix <- fit %>%
      tidy() %>%
      mutate(conf.low = estimate - 1.96 * std.error,
             conf.high = estimate + 1.96 * std.error)
  }

  if (event_plot) {
    event_plot <- reg_matrix %>%
      filter(str_detect(term, model$model)) %>%
      mutate(term = str_replace(term, model$model, "")) %>%
      mutate(term = as.numeric(term)) %>%
      add_row(term = 0, estimate = 0) %>%
      replace_na(list(conf.high = 0, conf.low = 0)) %>%
      ggplot(aes(term, estimate)) +
      geom_point() +
      geom_line() +
      geom_vline(xintercept =  0, color = "red") +
      geom_hline(yintercept = 0, linetype = 2) +
      geom_line(aes(y = conf.low), linetype = 2) +
      geom_line(aes(y = conf.high), linetype = 2) +
      #  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
      xlab(model$model) +
      ylab("coefficient")
    return(list(reg_matrix = reg_matrix, event_plot = event_plot, fit = fit, N = N))
  } else {
    return(list(reg_matrix = reg_matrix, fit = fit, N = N))
  }

}
