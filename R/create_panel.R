#' @title Collapse the individual-level data into state-year-level panel data
#' @author Jiacheng He
#'
#' @import dplyr
#' @importFrom tidyr replace_na
#' @export

create_panel <- function(df) {

  year <- unique(df$year) %>% sort()
  n_years <- length(year)
  n_states <- length(unique(df$state))

  treated_states <-  data_frame(state = c(1, 4, 9, 10, 11, 23, 28, 36, 50),
                                treated_states = 1,
                                rm_year = c(1998, 2001, 2009, 2000, 2008, 2006, 1999, 2008, 2006),
                                rm_month = c(7, 10, 10, 5, 11, 2, 7, 4, 1))

  state_fips <- c(1, 2, 4, 5, 6, 8:13, 15:42, 44:51, 53:56)
  state_abb <- c(state.abb[1:8], "DC", state.abb[9:50])

  panel <- data_frame(state = rep(state_fips, each=n_years), abb = rep(state_abb, each=n_years), year = rep(year, n_states)) %>%
    left_join(treated_states, by = "state") %>%
    replace_na(list(treated_states = 0, rm_year = 2049, rm_month = 100)) %>%
    mutate(treated = as.numeric(year > rm_year)) %>%
    mutate(event_year = (year - rm_year) * treated_states) %>%
    left_join(df, by = c("state", "year")) %>%
    select(-state) %>%
    rename(state = abb)

  return(panel)
}
