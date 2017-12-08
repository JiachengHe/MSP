library(devtools)

pkg_name <- "MSP"


create(pkg_name)
setwd(pkg_name)
document()
load_all()