specification <- function(LHS, model, controls=FALSE) {
  
  if (model=="DID") {
    RHS <- "state + year + post_treatment"
  } else if (model == "event_study") {
    RHS <- "state + year + event_time"
  } else {
    stop('Model specification is either "DID" or "event_study"')
  }
    
  if (controls==TRUE) {
    RHS <- paste(RHS, "+ AGE + SEX + EDUCA + MARITAL + EMPLOY")
  }
  
  return(paste(LHS, "~", RHS))
}