# library(dplyr); library(broom); library(ggplot2); library(stringr)
# setwd("D:/BRFSS")
# df <- lapply(1995:2010, read_BRFSS) %>% merge_BRFSS() %>% clean_BRFSS()
# save(df, file = "BRFSS19952010.RData")
# setwd("C:/Users/j774h/OneDrive/MSP")
data("BRFSS19952010")
devtools::load_all()

model_1 <- specification("MEDCOST", model = "DID")
model_2 <- specification("MEDCOST", model = "DID", controls = TRUE)
model_3 <- specification("MEDCOST", model = "event_study")
model_4 <- specification("MEDCOST", model = "event_study", controls = TRUE)


fit1 <- regression_analysis(BRFSS19952010, model_1)
fit2 <- regression_analysis(BRFSS19952010, model_1, cluster = BRFSS19952010$state)
fit3 <- regression_analysis(BRFSS19952010, model_2, cluster = BRFSS19952010$state)

fit4 <- regression_analysis(BRFSS19952010, model_3, event_plot = TRUE)
fit5 <- regression_analysis(BRFSS19952010, model_3, cluster = BRFSS19952010$state, event_plot = TRUE)
fit6 <- regression_analysis(BRFSS19952010, model_4, cluster = BRFSS19952010$state, event_plot = TRUE)

fit1$fit
fit2$fit
fit3$fit
fit4$fit
fit5$fit
fit6$fit

fit4$event_plot + ggsave(filename = "fit4.pdf")
fit5$event_plot + ggsave(filename = "fit5.pdf")
fit6$event_plot + ggsave(filename = "fit6.pdf")

#df$CHECKUP <- ifelse(df$CHECKUP>1, 0, df$CHECKUP)
#df$PERSDOC <- ifelse(df$PERSDOC>2, 0, 1)

#fit5 <- regression_analysis(df, "CHECKUP ~ state + year + event_time", cluster = df$state, event_plot = TRUE)

#fit4 <- regression_analysis(df, "PERSDOC ~ state + year + event_time", cluster = df$state, event_plot = TRUE)



### CPS analysis

model_1 <- specification("HIMCAID", model = "DID")
fit1 <- regression_analysis(df, HIMCAID ~ treated + year + post_treatment, cluster = df$state)

model_2 <- specification("HIMCAID", model = "event_study")
fit2 <- regression_analysis(filter(df, POVERTY==10), model_2, cluster = df$state, event_plot = TRUE)
