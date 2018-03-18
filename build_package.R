library(devtools)
document()
load_all()

BRFSS <- lapply(1991:2016, read_BRFSS, path = "D:/BRFSS/") %>% purrr::reduce(bind_rows) %>% clean_BRFSS()
use_data(BRFSS)

CPS <- read_csv("D:/BRFSS/cps_19912017.csv") %>% clean_CPS()
use_data(CPS)
