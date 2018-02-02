devtools::load_all()
data("BRFSS19952010")
library(dplyr)

model_1 <- specification("MEDCOST", model = "DID")
model_3 <- specification("MEDCOST", model = "event_time")

BRFSS_lowIncome <- filter(BRFSS19952010, INCOME %in% c("1", "2", "3", "4"))
BRFSS_highIncome <- filter(BRFSS19952010, INCOME %in% c("4", "5", "6", "7", "8"))


fit2 <- regression_analysis(BRFSS_lowIncome, model_1, cluster = BRFSS_lowIncome$state)
fit5 <- regression_analysis(BRFSS_lowIncome, model_3, cluster = BRFSS_lowIncome$state, event_plot = TRUE)

fit2 <- regression_analysis(BRFSS_highIncome, model_1, cluster = BRFSS_highIncome$state)
fit5 <- regression_analysis(BRFSS_highIncome, model_3, cluster = BRFSS_highIncome$state, event_plot = TRUE)


fit2 <- subgroup_regression("SEX=='1'", BRFSS19952010, model_1, cluster = "state")
fit5 <- subgroup_regression("SEX=='1'", BRFSS19952010, model_3, cluster = "state", event_plot = TRUE)

fit2 <- subgroup_regression("SEX=='2'", BRFSS19952010, model_1, cluster = "state")
fit5 <- subgroup_regression("SEX=='2'", BRFSS19952010, model_3, cluster = "state", event_plot = TRUE)


fit2 <- subgroup_regression("EDUCA %in% c('1', '2', '3', '4')", BRFSS19952010, model_1, cluster = "state")
fit5 <- subgroup_regression("EDUCA %in% c('1', '2', '3', '4')", BRFSS19952010, model_3, cluster = "state", event_plot = TRUE)

fit2 <- subgroup_regression("EDUCA %in% c('5', '6')", BRFSS19952010, model_1, cluster = "state")
fit5 <- subgroup_regression("EDUCA %in% c('5', '6')", BRFSS19952010, model_3, cluster = "state", event_plot = TRUE)






