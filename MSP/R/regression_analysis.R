regression_analysis <- function(df, model, cluster=NULL, event_plot=FALSE, 
                                xlab=NULL, ylab=NULL) {
  
  library(dplyr); library(broom); library(ggplot2); library(stringr); library(lmtest); library(plm); library(tidyr)
  
  fit <- lm(model, weights = wt, data = df)
  
  N <- length(fit$residuals)
  
  if (is.null(cluster)) {
    reg_matrix <- tidy(fit, conf.int = TRUE)
    fit <- summary(fit)
  } else {
    fit <- fit %>% 
      coeftest(vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = cluster)) 
    
    reg_matrix <- fit %>% 
      tidy() %>% 
      mutate(conf.low = estimate - 1.96 * std.error, 
             conf.high = estimate + 1.96 * std.error) 
  }
  
  if (event_plot) {
    event_plot <- reg_matrix %>% 
      filter(str_detect(term, "event_time")) %>% 
      mutate(term = str_replace(term, "event_time", "")) %>% 
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
      xlab("event time") + 
      ylab("coefficient")
    return(list(reg_matrix = reg_matrix, event_plot = event_plot, fit = fit, N = N))
  } else {
    return(list(reg_matrix = reg_matrix, fit = fit, N = N))
  }
  
}