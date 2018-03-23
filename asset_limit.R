asset_limit <- data_frame(year = 1991:2016)
asset_limit$individual <- c(rep(4000, 19), 6600, 6680, 6940, 7080, 7160, 7280, 7280)
asset_limit$couple <- c(rep(6000, 19), 9910, 10020, 10410, 10620, 10750, 10930, 10930)

asset_limit %>% 
  ggplot() + 
  geom_line(aes(year, individual)) + 
  geom_line(aes(year, couple)) + 
  ylim(0, 10930)

inflation <- readr::read_csv("FPCPITOTLZGUSA.csv")
inflation$year <- lubridate::year(inflation$DATE)

asset_limit <- asset_limit %>% 
  left_join(inflation, by = "year") %>% 
  mutate(inflation = 1 + FPCPITOTLZGUSA / 100) %>% 
  select(-FPCPITOTLZGUSA)

asset_limit$cum_inf <- 1
for (j in 2015:1991) {
  asset_limit$cum_inf[asset_limit$year == j] <- asset_limit$cum_inf[asset_limit$year == (j+1)] / asset_limit$inflation[asset_limit$year == j]
}

asset_limit$individual_real <- asset_limit$individual / asset_limit$cum_inf
asset_limit$couple_real <- asset_limit$couple / asset_limit$cum_inf

asset_limit %>% 
  select(year, individual_real, couple_real) %>% 
  gather(type, limit, -year) %>% 
  ggplot() + 
  geom_line(aes(year, limit, color = type)) + 
  ylim(0, 11000) + 
  labs(y = "Asset Limit (2016 Dollars)", title = "1991-2016 MSP Federal Asset Limit in 2016 Dollars")
  
CPS %>% 
  group_by(year) %>% 
  summarize(HIMCAID = weighted.mean(HIMCAID, wt)) %>% 
  ggplot() + 
  geom_line(aes(year, HIMCAID))
