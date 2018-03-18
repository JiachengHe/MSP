#' @title Clean the BRFSS data
#' @description Write all cleaning process in this file
#' @author Jiacheng He
#'
#' @param df
#'
#' @return A data frame
#' @examples
#' read_BRFSS(2000) %>% clean_BRFSS()
#' lapply(1995:2010, read_BRFSS) %>% merge_BRFSS() %>% clean_BRFSS()
#'
#' @importFrom dplyr '%>%'
#' @importFrom dplyr data_frame
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr recode
#' @importFrom tidyr replace_na
#' @export


clean_BRFSS <- function(df) {

  treated_states <-  data_frame(state = c(1, 4, 9, 10, 11, 23, 28, 36, 50),
                     treated = 1,
                     rm_year = c(1998, 2001, 2009, 2000, 2008, 2006, 1999, 2008, 2006),
                     rm_month = c(7, 10, 10, 5, 11, 2, 7, 4, 1))


  state <- data_frame(state = unique(df$state)) %>%
    left_join(treated_states) %>%
    replace_na(list(treated = 0, rm_year = 2049, rm_month = 100))

  df <- df %>%
    left_join(state, by = "state")


  df$post_treatment <- (df$year > df$rm_year)
  df$post_treatment_month <- ((df$year > df$rm_year) & (df$IMONTH > df$rm_month))
  df$event_time <- (df$year - df$rm_year) * df$treated
  df$event_month <- ((df$year-df$rm_year)*12+(df$IMONTH-df$rm_month)) * df$treated

  # replace all nonsense values with NA (for LHS outcome variables)
#  if (max(df$year) >= 1993) {    ### Before 1993 these variable did not exist
#    df$GENHLTH <- recode(df$GENHLTH, `7`=NA_integer_, `9`=NA_integer_)
#    df$PHYSHLTH <- recode(df$PHYSHLTH, `88`=0L, `77`=NA_integer_, `99`=NA_integer_)
#    df$MENTHLTH <- recode(df$MENTHLTH, `88`=0L, `77`=NA_integer_, `99`=NA_integer_)
#    df$POORHLTH <- recode(df$POORHLTH, `88`=0L, `77`=NA_integer_, `99`=NA_integer_)
#    df$HLTHPLAN <- recode(df$HLTHPLAN, `2`=0L, `7`=NA_integer_, `9`=NA_integer_)
#  }
  if (max(df$year) >= 2000) {    ### Before 2000 these variable did not exist
    df$PERSDOC <- recode(df$PERSDOC, `7`=NA_integer_, `9`=NA_integer_)
  }
  df$MEDCOST <- recode(df$MEDCOST, `2`=0L, `7`=NA_integer_, `9`=NA_integer_)
  df$CHECKUP <- recode(df$CHECKUP, `7`=NA_integer_, `9`=NA_integer_)


  # replace NAs with a new category (for RHS regressors)
  df$MARITAL <- recode(df$MARITAL, .missing = 10L) %>% as.factor()
  df$EDUCA <- recode(df$EDUCA, .missing = 10L) %>% as.factor()
  df$EMPLOY <- recode(df$EMPLOY, .missing = 10L) %>% as.factor()
  df$INCOME <- recode(df$INCOME, .missing = 100L) %>% as.factor()

  df <- df %>%
    mutate(event_time = as.factor(event_time),
           event_month = as.factor(event_month),
           AGE = as.factor(AGE),
           SEX = as.factor(SEX))

  df$event_time <- relevel(df$event_time, ref = "0")
  df$event_month <- relevel(df$event_month, ref = "0")

  return(df)
}

