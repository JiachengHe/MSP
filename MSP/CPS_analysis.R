devtools::load_all()
library(readr)
library(dplyr)
library(gsynth)

CPS <- read_csv("D:/BRFSS/cps_19912017.csv") %>% clean_CPS()
CPS_panel <- CPS %>%
  group_by(state, year) %>%
  summarise(HIMCAID = weighted.mean(HIMCAID, wt, na.rm = TRUE), wt = n()) %>%
  create_panel()

panelView(HIMCAID ~ treated, data = as.data.frame(CPS_panel), index = c("state", "year"), na.rm = TRUE)
panelView(HIMCAID ~ treated, data = as.data.frame(CPS_panel), index = c("state", "year"), na.rm = TRUE, type="raw")



fit <- lm(HIMCAID ~ state + factor(year) + treated, data = CPS_panel, weights = wt)
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))

# CPS_panel <- filter(CPS_panel, event_year %in% -10:10)
CPS_panel$event_year <- factor(CPS_panel$event_year)
CPS_panel$event_year <- relevel(CPS_panel$event_year, ref = "0")

fit <- lm(HIMCAID ~ state + factor(year) + event_year, data = CPS_panel)
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))
event_plot(fit, "event_year", cluster = "state")



out <- gsynth(HIMCAID ~ treated, data = as.data.frame(CPS_panel), na.rm = TRUE, index = c("state", "year"),
              force = "two-way", type = "fe", CV = TRUE, r = 4, se = TRUE,
              inference = "nonparametric", nboots = 1000, parallel = TRUE, min.T0 = 5)
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




