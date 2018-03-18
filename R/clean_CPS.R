#' @title Cleaning the CPS data
#' @description This data
#' @author Jiacheng He
#'
#' @param df The original CPS data set
#'
#'
#'
#'


clean_CPS <- function(df) {

  df <- df %>%
    rename(state = STATEFIP, year = YEAR, wt = ASECWT)

  treated_states <-  data_frame(state = c(1, 4, 9, 10, 11, 23, 28, 36, 50),
                                treated = 1,
                                rm_year = c(1998, 2001, 2009, 2000, 2008, 2006, 1999, 2008, 2006),
                                rm_month = c(7, 10, 10, 5, 11, 2, 7, 4, 1))


  state <- data_frame(state = unique(df$state)) %>%
    left_join(treated_states) %>%
    replace_na(list(treated = 0, rm_year = 2049))

  df <- df %>%
    left_join(state, by = "state")

  df$post_treatment <- (df$year > df$rm_year)
  df$event_time <- (df$year - df$rm_year) * df$treated

  df$HIMCAID <- df$HIMCAID - 1
  df$HIMCARE <- df$HIMCARE - 1
  df$PHINSUR <- df$PHINSUR - 1

  df$event_time <- factor(df$event_time)
  df$event_time <- relevel(df$event_time, ref = "0")


  return(df)
}
