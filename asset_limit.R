library(dplyr)
library(ggplot2)
library(tidyr)
asset_limit <- data_frame(year = 1991:2016)
asset_limit$QMB_individual <- c(rep(4000, 19), 6600, 6680, 6940, 7080, 7160, 7280, 7280)
asset_limit$QMB_couple <- c(rep(6000, 19), 9910, 10020, 10410, 10620, 10750, 10930, 10930)
asset_limit$SSI_individual <- 2000
asset_limit$SSI_couple <- 3000

asset_limit %>% 
  ggplot() + 
  geom_line(aes(year, QMB_individual)) + 
  geom_line(aes(year, QMB_couple)) + 
  ylim(0, 10930)

inflation <- readr::read_csv("FPCPITOTLZGUSA.csv")
inflation$year <- lubridate::year(inflation$DATE)

asset_limit <- asset_limit %>% 
  left_join(inflation, by = "year") %>% 
  mutate(inflation = 1 + FPCPITOTLZGUSA / 100) %>% 
  select(-FPCPITOTLZGUSA)

asset_limit$cum_inf <- 1
for (j in 1992:2016) {
  asset_limit$cum_inf[asset_limit$year == j] <- asset_limit$cum_inf[asset_limit$year == (j-1)] * asset_limit$inflation[asset_limit$year == j]
}

asset_limit$QMB_individual_real <- asset_limit$QMB_individual / asset_limit$cum_inf
asset_limit$QMB_couple_real <- asset_limit$QMB_couple / asset_limit$cum_inf
asset_limit$SSI_individual_real <- asset_limit$SSI_individual / asset_limit$cum_inf
asset_limit$SSI_couple_real <- asset_limit$SSI_couple / asset_limit$cum_inf

asset_limit %>% 
  select(year, `QMB individual` = QMB_individual_real, `QMB couple` = QMB_couple_real,
         `Medicaid individual` = SSI_individual_real, `Medicaid couple` = SSI_couple_real) %>% 
  gather(type, limit, -year) %>% 
  ggplot() + 
  geom_line(aes(year, limit, color = type)) + 
  ylim(0, 6400) + 
  labs(y = "Asset Limit (1991 Dollars)", title = "1991-2016 Medicaid Federal Asset Limit for Seniors in 1991 Dollars") + 
  guides(color = guide_legend(title = NULL)) #+ 
  ggsave("plot/asset_limit.pdf")



