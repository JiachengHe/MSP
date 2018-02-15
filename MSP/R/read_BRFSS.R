#' @title Loading BRFSS data
#' @description Specify the year of the BRFSS and load it as a data frmae
#' @author Jiacheng He
#'
#' @param year
#'
#' @return A data frame
#' @examples
#' BRFSS1995 <- read_BRFSS(1995)
#'
#' @importFrom SASxport read.xport
#' @importFrom dplyr '%>%'
#' @importFrom dplyr as_data_frame
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @export



read_BRFSS <- function(year, path=NULL) {

  filename <- ifelse(year <= 2010,
                     paste0("CDBRFS", substr(year, 3, 4), ".XPT"),
                     paste0("LLCP", year, ".XPT"))

  if (!is.null(path)) {filename <- paste0(path, filename)}

#  if (year == 2002) {MEDCOST <- quo(MEDCARE)
#  } else {MEDCOST <- quo(MEDCOST)}

#  if (year == 1995) {INCOME <- quo(INCOME95)
#  } else {INCOME <- quo(INCOME2)}

  df <- read.xport(filename)
  df[] <- lapply(df, unclass)

  if (year >= 1993) {
    df <- df %>%
      as_data_frame() %>%
      select(state = X.STATE, IMONTH, IDAY, IYEAR,  ## ID
             GENHLTH, PHYSHLTH, MENTHLTH, POORHLTH,   ## health status
             HLTHPLAN, MEDCOST = starts_with("MEDC"), CHECKUP = starts_with("CHECKUP"), PERSDOC = starts_with("PERSDOC"), ## health care access
             AGE, MARITAL, EDUCA, EMPLOY, INCOME = starts_with("INCOME"), SEX, ## demographics
             wt = X.FINALWT) %>%
      filter(AGE >= 65, state <= 56)
  } else {
    df <- df %>%
      as_data_frame() %>%
      select(state = X.STATE, IMONTH, IDAY, IYEAR,  ## ID
             MEDCOST = starts_with("MEDC"), CHECKUP = starts_with("CHECKUP"), PERSDOC = starts_with("PERSDOC"), ## health care access
             AGE, MARITAL, EDUCA, EMPLOY, INCOME = starts_with("INCOME"), SEX, ## demographics
             wt = X.FINALWT) %>%
      filter(AGE >= 65, state <= 56)
  }



  ## 2007 survey contains IYEAR 2006,2007,2008
  if (3 %in% df$IYEAR) {
    print(paste0("In ", year, ", there are more than two interview years"))
    df$year <- df$IYEAR - 2 + year
  } else { df$year <- df$IYEAR - 1 + year }

  ## In 2004, IMONTH contains value "13"
  ## I found that is because they mistakenly record "IMONTH = IMONTH + 1"
  if (13 %in% df$IMONTH) {
    print(paste0("In ", year, ", there are 13 months ??"))
    df$IMONTH <- df$IMONTH - 1
  }


  return(df)
}

# INCOME2 = INCOME95 in 1995
# MEDCOST = MEDCARE in 2002
# No "CHECKUP" in 2003, 2004
# CHECKUP = CHECKUP1 in 2007, 2008, 2009, 2010
# 2000 PERSDOC, After 2001 PERSDOC2, no PERSDOC before 2000



