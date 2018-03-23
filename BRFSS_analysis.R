devtools::load_all()
library(dplyr)
library(ggplot2)
library(gsynth)
data(BRFSS)
seed <- 2018

BRFSS_panel <- BRFSS %>%
  group_by(state, year) %>%
  summarize(MEDCOST = weighted.mean(MEDCOST, wt, na.rm = TRUE), 
            female = weighted.mean(SEX==2, wt, na.rm = TRUE), 
            college = weighted.mean(EDUCA %in% c(5,6), wt, na.rm = TRUE), 
            married = weighted.mean(MARITAL==1, wt, na.rm = TRUE), wt = n()) %>%
  create_panel() %>%
  filter(year != 2017 & !is.na(MEDCOST)) #%>%
#  filter(year != 2001) %>%
#  filter(MEDCOST < 0.2)

panelView(MEDCOST ~ treated, data = as.data.frame(BRFSS_panel), index = c("state", "year"), na.rm = TRUE, main = "BRFSS Data: Treatment Status") #+ 
  ggsave("plot/BRFSS_status.pdf", width = 12, height = 8)
panelView(MEDCOST ~ treated, data = as.data.frame(BRFSS_panel), index = c("state", "year"), na.rm = TRUE, type="raw")


# DID regression
fit <- lm(MEDCOST ~ state + factor(year) + treated, data = BRFSS_panel, weights = wt)
summary(fit)
# Clustered SE
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))
# With controls
fit <- lm(MEDCOST ~ state + factor(year) + female + college + married + treated, data = BRFSS_panel, weights = wt)
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))

BRFSS_panel %>% 
  group_by(treated_states, treated) %>% 
  summarize(MEDCOST = weighted.mean(MEDCOST, wt, na.rm = TRUE))


BRFSS_panel$event_year <- factor(BRFSS_panel$event_year) %>% relevel(ref = "0")

# Event-study regressiom
fit <- lm(MEDCOST ~ state + factor(year) + event_year, data = BRFSS_panel, weights = wt)
summary(fit)
# Clustered SE
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))
# Event plot
event_plot(fit, "event_year", cluster = "state") + xlim(c(-9,10)) + 
  ylim(c(-0.015, 0.05)) + 
  labs(y = "MEDCOST", title = "Event Study Coefficients on MEDCOST") + 
  ggsave("plot/BRFSS_event_plot.pdf", width = 9, height = 6)

# With controls
fit <- lm(MEDCOST ~ state + factor(year) + female + college + married + event_year, data = BRFSS_panel, weights = wt)
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))


out <- gsynth(MEDCOST ~ treated, data = as.data.frame(BRFSS_panel), na.rm = TRUE, index = c("state", "year"),
              force = "two-way", type = "fe", CV = TRUE, r = 1, se = TRUE,
              inference = "nonparametric", nboots = 1000, parallel = TRUE, min.T0 = 5, seed = seed)
plot(out)
plot(out, type = "raw")
plot(out, type = "counterfactual", raw = "none")
plot(out, type = "counterfactual", raw = "band")
plot(out, type = "counterfactual", raw = "all")

for (state in out$id.tr) {
  plot(out, type = "counterfactual", id = state)
  readline(prompt = "")
}

out$est.att
out$est.avg

