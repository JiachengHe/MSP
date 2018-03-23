devtools::load_all()
library(dplyr)
library(ggplot2)
library(gsynth)
data(CPS)
seed <- 2018

CPS_panel <- CPS %>%
  group_by(state, year) %>%
  summarise(HIMCAID = weighted.mean(HIMCAID, wt, na.rm = TRUE), 
            female = weighted.mean(SEX==2, wt, na.rm = TRUE), 
            white = weighted.mean(RACE==100, wt, na.rm = TRUE), 
            college = weighted.mean(EDUC>=80, wt, na.rm = TRUE), 
            married = weighted.mean(MARST<=2, wt, na.rm = TRUE), wt = n()) %>%
  create_panel() %>% 
  filter(year != 2017)

panelView(HIMCAID ~ treated, data = as.data.frame(CPS_panel), index = c("state", "year"), na.rm = TRUE, main = "CPS Data: Treatment Status") #+ 
  ggsave("plot/CPS_status.pdf", width = 12, height = 8)

panelView(HIMCAID ~ treated, data = as.data.frame(CPS_panel), index = c("state", "year"), na.rm = TRUE, type="raw")


# DID regression
fit <- lm(HIMCAID ~ state + factor(year) + treated, data = CPS_panel, weights = wt)
summary(fit)
# Cluster SE
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))
# With controls
fit <- lm(HIMCAID ~ state + factor(year) + female + white + college + married + treated, data = CPS_panel, weights = wt)
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))


CPS_panel %>% 
  group_by(treated_states, treated) %>% 
  summarize(HIMCAID = weighted.mean(HIMCAID, wt, na.rm = TRUE))


# CPS_panel <- filter(CPS_panel, event_year %in% -10:10)
CPS_panel$event_year <- factor(CPS_panel$event_year) %>% relevel(ref = "0")

# Event-study Regression
fit <- lm(HIMCAID ~ state + factor(year) + event_year, data = CPS_panel)
summary(fit)
# Clustered SE
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))
# Event plot
event_plot(fit, "event_year", cluster = "state") + 
  xlim(-10, 10) + 
  labs(y = "Medicaid Coverage", title = "Event Study Coefficients on Medicaid Coverage Rate") +
  ggsave("plot/CPS_event_plot.pdf", width = 9, height = 6)

# With controls
fit <- lm(HIMCAID ~ state + factor(year) + female + white + college + married + event_year, data = CPS_panel)
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))


out <- gsynth(HIMCAID ~ treated, data = as.data.frame(CPS_panel), na.rm = TRUE, index = c("state", "year"),
              force = "two-way", type = "fe", CV = TRUE, r = 4, se = TRUE,
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




