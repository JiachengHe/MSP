devtools::load_all()
library(dplyr)
library(ggplot2)
library(gsynth)
data(CPS)
seed <- 2018

CPS_panel <- CPS %>%
  group_by(state, year) %>%
  summarise(HIMCAID = weighted.mean(HIMCAID, wt, na.rm = TRUE), 
            PHINSUR = weighted.mean(PHINSUR, wt, na.rm = TRUE),
            HIMCARE = weighted.mean(HIMCARE, wt, na.rm = TRUE), 
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
  labs(y = "Medicaid Coverage", title = "Event Study Coefficients on Medicaid Coverage Rate") #+
  ggsave("plot/CPS_event_plot.pdf", width = 9, height = 6)

# With controls
fit <- lm(HIMCAID ~ state + factor(year) + female + white + college + married + event_year, data = CPS_panel)
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))


out <- gsynth(HIMCAID ~ treated, data = as.data.frame(CPS_panel), na.rm = TRUE, index = c("state", "year"),
              weight = NULL, force = "two-way", type = "fe", CV = TRUE, r = 4, se = TRUE,
              inference = "parametric", nboots = 1000, parallel = TRUE, min.T0 = 5, seed = seed)
plot(out)
plot(out, type = "raw")
plot(out, type = "counterfactual", raw = "none", main = "Average of All Treated States", xlab = "years relative to the treatment") #+ ggsave("plot/CPS_GSC_average.pdf", height = 9, width = 12)
plot(out, type = "counterfactual", raw = "band")
plot(out, type = "counterfactual", raw = "all")

for (state in out$id.tr) {
  if (state == "DC") { 
    state_title <- "DC"
  } else {
    state_title <- state.name[state.abb == state]
  }
  assign(state, plot(out, type = "counterfactual", raw = "band", id = state)$plot + labs(title = state_title))
}

#pdf("plot/CPS_GSC.pdf", height = 12, width = 18)
multiplot(AL, AZ, CT, DC, DE, ME, MS, NY, VT, cols = 3)
#dev.off()


out$est.att
out$est.avg


data_frame(year = 1991:2016, treated = out$Y.tr[,state_col], counterfactual = out$Y.ct[,state_col]) %>% 
  mutate(ci_lower = counterfactual - 1.96 * out$est.ind[,2,state_col],
         ci_upper = counterfactual + 1.96 * out$est.ind[,2,state_col]) %>% 
  ggplot(aes(x = year)) + 
  geom_line(aes(y = treated), color = "red") + 
  geom_line(aes(y = counterfactual)) + 
  geom_line(aes(y = ci_lower), linetype = 2) + 
  geom_line(aes(y = ci_upper), linetype = 2) 



out$Y.ct %>% colnames()
plot(out$Y)


CPS_panel <- CPS_panel %>%
  mutate(late_treated = treated * (state %in% c("NY", "DC", "CT")), 
         early_treated = treated * (!state %in% c("NY", "DC", "CT")))

CPS_panel <- CPS_panel %>%
  mutate(late_treated = treated * (year>2010), 
         early_treated = treated * (year<=2010))

CPS_panel <- CPS_panel %>%
  filter((state %in% c("NY", "DC", "CT")) | treated_states==0) %>% 
  mutate(late_treated = treated * (year>2010), 
         early_treated = treated * (year<=2010))





### Robustness check
# Drop AZ
fit <- lm(HIMCAID ~ state + factor(year) + treated, data = filter(CPS_panel, state!="AZ"), weights = wt)
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))

# Only AZ as treated group
fit <- lm(HIMCAID ~ state + factor(year) + treated, data = filter(CPS_panel, (state=="AZ" | treated_states==0)), weights = wt)
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))

# Drop MN
fit <- lm(HIMCAID ~ state + factor(year) + treated, data = filter(CPS_panel, state!="MN"), weights = wt)
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))

# MN as treated group
CPS_MN <- CPS_panel
CPS_MN$treated[CPS_MN$state=="MN"] <- CPS_MN$treated[CPS_MN$state=="AZ"]
fit <- lm(HIMCAID ~ state + factor(year) + treated, data = CPS_MN, weights = wt)
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))

# Drop AK (AK has too few observations in the state-year cell)
arrange(CPS_panel, wt) %>% select(state, year, wt)
fit <- lm(HIMCAID ~ state + factor(year) + treated, data = filter(CPS_panel, state!="AK"), weights = wt)
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))

# Only 1991-2010
fit <- lm(HIMCAID ~ state + factor(year) + treated, data = filter(CPS_panel, year<=2010), weights = wt)
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))

# Private insurance crowd out
fit <- lm(PHINSUR ~ state + factor(year) + treated, data = CPS_panel, weights = wt)
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))




CPS_panel_older <- CPS %>% 
  filter(AGE >= 70 & AGE < 80) %>% 
  group_by(state, year) %>%
  summarize(HIMCAID = weighted.mean(HIMCAID, wt, na.rm = TRUE), wt = n()) %>%
  create_panel() %>%
  filter(year != 2017)

fit <- lm(HIMCAID ~ state + factor(year) + treated, data = CPS_panel_older, weights = wt)
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))


CPS_panel_younger <- CPS %>% 
  filter(AGE > 80) %>% 
  group_by(state, year) %>%
  summarize(HIMCAID = weighted.mean(HIMCAID, wt, na.rm = TRUE), wt = n()) %>%
  create_panel() %>%
  filter(year != 2017)

fit <- lm(HIMCAID ~ state + factor(year) + treated, data = CPS_panel_younger, weights = wt)
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))








out <- gsynth(HIMCAID ~ treated, data = as.data.frame(filter(CPS_panel, (state=="AZ" | treated_states==0))), na.rm = TRUE, 
              index = c("state", "year"), force = "two-way", type = "fe", CV = TRUE, r = 5, se = TRUE,
              inference = "parametric", nboots = 1000, parallel = TRUE, min.T0 = 5, seed = seed)
plot(out)
plot(out, type = "raw")
plot(out, type = "counterfactual", raw = "none", main = "Average of All Treated States", xlab = "years relative to the treatment", 
     ylab = "MEDCOST") #+ ggsave("plot/BRFSS_GSC_average.pdf", height = 9, width = 12)
plot(out, type = "counterfactual", raw = "band")
plot(out, type = "counterfactual", raw = "all")




