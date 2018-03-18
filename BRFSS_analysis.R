devtools::load_all()
library(dplyr)
library(ggplot2)
library(gsynth)
BRFSS <- lapply(1991:2016, read_BRFSS, path = "D:/BRFSS/") %>% purrr::reduce(bind_rows) %>% clean_BRFSS()

BRFSS_panel <- BRFSS %>%
  group_by(state, year) %>%
  summarize(MEDCOST = weighted.mean(MEDCOST, wt, na.rm = TRUE), wt = n()) %>%
  create_panel() %>%
  filter(year != 2017) %>%
  filter(year != 2001) %>%
  filter(MEDCOST < 0.2)

panelView(MEDCOST ~ treated, data = as.data.frame(BRFSS_panel), index = c("state", "year"), na.rm = TRUE)
panelView(MEDCOST ~ treated, data = as.data.frame(BRFSS_panel), index = c("state", "year"), na.rm = TRUE, type="raw")



fit <- lm(MEDCOST ~ state + factor(year) + treated, data = BRFSS_panel, weights = wt)
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))

BRFSS_panel$event_year <- factor(BRFSS_panel$event_year)
BRFSS_panel$event_year <- relevel(BRFSS_panel$event_year, ref = "0")

fit <- lm(MEDCOST ~ state + factor(year) + event_year, data = BRFSS_panel, weights = wt)
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))
event_plot(fit, "event_year", cluster = "state") + xlim(c(-10,10)) + ylim(c(-0.026, 0.05))





out <- gsynth(MEDCOST ~ treated, data = as.data.frame(BRFSS_panel), na.rm = TRUE, index = c("state", "year"),
              force = "two-way", type = "fe", CV = TRUE, r = 1, se = TRUE,
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

