library(gsynth)
library(dplyr)
library(tidyr)
library(ggplot2)
data(gsynth)


BRFSS_agg <- BRFSS %>%
  group_by(state, year) %>%
  summarize(MEDCOST = weighted.mean(MEDCOST, w=wt, na.rm=TRUE), wt = sum(wt)) %>%
  filter(!is.na(MEDCOST))

BRFSS_agg$state <- as.numeric(as.character(BRFSS_agg$state))
BRFSS_agg$year <- as.numeric(as.character(BRFSS_agg$year))

treated_states <-  data_frame(state = c(1, 4, 9, 10, 11, 23, 28, 36, 50),
                              treated = 1,
                              rm_year = c(1998, 2001, 2009, 2000, 2008, 2006, 1999, 2008, 2006),
                              rm_month = c(7, 10, 10, 5, 11, 2, 7, 4, 1))


state_fips <- c(1, 2, 4, 5, 6, 8:13, 15:42, 44:51, 53:56)
state_abb <- c(state.abb[1:8], "DC", state.abb[9:50])

state <- data_frame(state = rep(state_fips, each=26), abb = rep(state_abb, each=26), year = rep(1991:2016, 51)) %>%
  left_join(treated_states, by = "state") %>%
  replace_na(list(treated = 0, rm_year = 2049, rm_month = 100)) %>%
  mutate(treated = as.numeric(year > rm_year)) %>%
  select(-rm_year, -rm_month) %>%
  left_join(BRFSS_agg, by = c("state", "year"))

# state <- state %>% filter(year!=2001, MEDCOST < max(MEDCOST, na.rm = TRUE))

panelView(MEDCOST ~ treated, data = as.data.frame(state), index = c("abb", "year"), na.rm = TRUE)

panelView(MEDCOST ~ treated, data = as.data.frame(state), index = c("abb", "year"), na.rm = TRUE, type="raw")

out <- gsynth(MEDCOST ~ treated, data = as.data.frame(state), na.rm = TRUE,
              index = c("abb", "year"), force = "unit", type = "mc",
              CV = TRUE, r = 1, se = TRUE,
              inference = "nonparametric", nboots = 1000,
              parallel = TRUE, min.T0 = 5)

plot(out)
plot(out, type = "raw")
plot(out, type = "counterfactual", raw = "none")
plot(out, type = "counterfactual", raw = "band")
plot(out, type = "counterfactual", raw = "all")

plot_list <- list()
for (abb in out$id.tr) {
  plot_list[[abb]] <- plot(out, type = "counterfactual", id = abb)
}

plot(out, type = "counterfactual", id = "CT")
out$est.att


panelView(Y ~ D, data = simdata,  index = c("id","time"))
panelView(turnout ~ policy_edr, data = turnout,  index = c("abb","year"))
