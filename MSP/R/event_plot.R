#' @title Plot the event study coefficients
#' @author Jiacheng He
#'
#' @import ggplot2
#' @importFrom plm vcovHC
#' @importFrom lmtest coeftest
#' @export
#'


event_plot <- function(fit, eventVar, cluster=NULL) {

  if (!is.null(cluster)) {
    fit <- fit %>%
      coeftest(vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[[cluster]]))
  }

  reg_matrix <- tidy(fit) %>%
    mutate(conf.low  = estimate - 1.96 * std.error,
           conf.high = estimate + 1.96 * std.error)

  reg_matrix %>%
    filter(str_detect(term, eventVar)) %>%
    mutate(term = str_replace(term, eventVar, "")) %>%
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
    xlab("years relative to the treatment") +
    ylab("coefficient")
}
