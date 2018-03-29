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
  filter(year != 2017, !is.na(MEDCOST)) #%>%
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
  ylim(c(-0.015, 0.026)) + 
  labs(y = "MEDCOST", title = "Event Study Coefficients on MEDCOST") #+ 
  ggsave("plot/BRFSS_event_plot.pdf", width = 9, height = 6)

# With controls
fit <- lm(MEDCOST ~ state + factor(year) + female + college + married + event_year, data = BRFSS_panel, weights = wt)
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))



out <- gsynth(MEDCOST ~ treated, data = as.data.frame(filter(BRFSS_panel, year!=2001)), na.rm = TRUE, 
              index = c("state", "year"), force = "two-way", type = "fe", CV = TRUE, r = 2, se = TRUE,
              inference = "parametric", nboots = 1000, parallel = TRUE, min.T0 = 5, seed = seed)
plot(out)
plot(out, type = "raw")
plot(out, type = "counterfactual", raw = "none", main = "Average of All Treated States", xlab = "years relative to the treatment", 
     ylab = "MEDCOST") #+ ggsave("plot/BRFSS_GSC_average.pdf", height = 9, width = 12)
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

#pdf("plot/BRFSS_GSC.pdf", height = 12, width = 18)
multiplot(AL, AZ, CT, DC, DE, ME, MS, NY, VT, cols = 3)
#dev.off()

out$est.att
out$est.avg


data_frame(year = c(1991:2000,2002:2016), treated = out$Y.tr[,state_col], counterfactual = out$Y.ct[,state_col]) %>% 
  mutate(ci_lower = counterfactual - 1.96 * out$est.ind[,2,state_col],
         ci_upper = counterfactual + 1.96 * out$est.ind[,2,state_col]) %>% 
  ggplot(aes(x = year)) + 
  geom_line(aes(y = treated), color = "red") + 
  geom_line(aes(y = counterfactual)) + 
  geom_line(aes(y = ci_lower), linetype = 2) + 
  geom_line(aes(y = ci_upper), linetype = 2) 









# Robustness checks 

# Drop Arizona
fit <- lm(MEDCOST ~ state + factor(year) + treated, data = filter(BRFSS_panel, state!="AZ"), weights = wt)
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))

# Arizona Only
fit <- lm(MEDCOST ~ state + factor(year) + treated, data = filter(BRFSS_panel, (state=="AZ" | treated_states==0)), weights = wt)
summary(fit)
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))

# Drop Minnesota
fit <- lm(MEDCOST ~ state + factor(year) + treated, data = filter(BRFSS_panel, state!="MN"), weights = wt)
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))

# MN as treated group
BRFSS_MN <- BRFSS_panel
BRFSS_MN$treated[BRFSS_MN$state=="MN"] <- BRFSS_MN$treated[BRFSS_MN$state=="AZ"]
fit <- lm(MEDCOST ~ state + factor(year) + treated, data = BRFSS_MN, weights = wt)
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))

# Drop Alaska
arrange(BRFSS_panel, wt) %>% select(state, year, wt)
fit <- lm(MEDCOST ~ state + factor(year) + treated, data = filter(BRFSS_panel, state!="AK"), weights = wt)
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))

# drop 2001
fit <- lm(MEDCOST ~ state + factor(year) + treated, data = filter(BRFSS_panel, year!=2001), weights = wt)
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))

# Only 1991-2010
fit <- lm(MEDCOST ~ state + factor(year) + treated, data = filter(BRFSS_panel, year<=2010), weights = wt)
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))







# College
BRFSS_panel_college <- BRFSS %>% 
  filter(EDUCA %in% c(5,6)) %>% 
  group_by(state, year) %>%
  summarize(MEDCOST = weighted.mean(MEDCOST, wt, na.rm = TRUE), wt = n()) %>%
  create_panel() %>%
  filter(year != 2017 & !is.na(MEDCOST) & year != 2001)

fit <- lm(MEDCOST ~ state + factor(year) + treated, data = BRFSS_panel_college, weights = wt)
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))

# non-college
BRFSS_panel_noncollege <- BRFSS %>% 
  filter(EDUCA %in% c(1,2,3,4)) %>% 
  group_by(state, year) %>%
  summarize(MEDCOST = weighted.mean(MEDCOST, wt, na.rm = TRUE), wt = n()) %>%
  create_panel() %>%
  filter(year != 2017 & !is.na(MEDCOST) & year != 2001)

fit <- lm(MEDCOST ~ state + factor(year) + treated, data = BRFSS_panel_noncollege, weights = wt)
lmtest::coeftest(fit, vcov = plm::vcovHC(fit, method = "arellano", type = "HC0", cluster = fit$model[["state"]]))





dataprep(BRFSS_panel, predictors = "MEDCOST" )

dataprep.out<-
  dataprep(
    foo = synth.data,
    predictors = c("X1", "X2", "X3"),
    predictors.op = "mean",
    dependent = "Y",
    unit.variable = "unit.num",
    time.variable = "year",
    special.predictors = list(
      list("Y", 1991, "mean"),
      list("Y", 1985, "mean"),
      list("Y", 1980, "mean")
    ),
    treatment.identifier = 7,
    controls.identifier = c(29, 2, 13, 17, 32, 38),
    time.predictors.prior = c(1984:1989),
    time.optimize.ssr = c(1984:1990),
    unit.names.variable = "name",
    time.plot = 1984:1996
  )
synth.out <- synth(dataprep.out)

gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out)
path.plot(dataprep.res = dataprep.out,synth.res = synth.out)



out <- gsynth(MEDCOST ~ treated, data = as.data.frame(filter(BRFSS_panel, state!="AZ", year!=2001)), na.rm = TRUE, 
              index = c("state", "year"), force = "two-way", type = "fe", CV = TRUE, r = 0, se = TRUE,
              inference = "parametric", nboots = 1000, parallel = TRUE, min.T0 = 5, seed = seed)
plot(out)
plot(out, type = "raw")
plot(out, type = "counterfactual", raw = "none", main = "Average of All Treated States", xlab = "years relative to the treatment", 
     ylab = "MEDCOST") #+ ggsave("plot/BRFSS_GSC_average.pdf", height = 9, width = 12)
plot(out, type = "counterfactual", raw = "band")
plot(out, type = "counterfactual", raw = "all")
