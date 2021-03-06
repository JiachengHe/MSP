# library(dplyr); library(broom); library(ggplot2); library(stringr)
devtools::load_all()
library(dplyr)
library(purrr)
BRFSS <- lapply(1991:2016, read_BRFSS, path = "D:/BRFSS/") %>% reduce(bind_rows) %>% clean_BRFSS()
save(BRFSS, file = "BRFSS.RData")
library(dplyr)
data("BRFSS19952010")


model_1 <- specification("MEDCOST", model = "DID")
model_2 <- specification("MEDCOST", model = "DID", controls = TRUE)
model_3 <- specification("MEDCOST", model = "event_time")
model_4 <- specification("MEDCOST", model = "event_time", controls = TRUE)

model_5 <- specification("MEDCOST", model = "event_month")

fit7 <- regression_analysis(df, model_5, cluster = df$state, event_plot = TRUE)

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

CPS <- read_csv("../CPS19952010.csv") %>% clean_CPS()

model_1 <- specification("HIMCAID", model = "DID")
fit1 <- regression_analysis(CPS, HIMCAID ~ treated + year + post_treatment, cluster = CPS$state)

model_2 <- specification("HIMCAID", model = "event_study")
fit2 <- regression_analysis(filter(CPS, POVERTY==10), model_2, cluster = CPS$state, event_plot = TRUE)
