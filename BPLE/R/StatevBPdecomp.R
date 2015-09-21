# Author: tim
###############################################################################

# let's decompose with respect to the e0 that would come from the smoothed mx, mxs

if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/BPLE/BPLE")
} else {
	# in that case I'm on Berkeley system, and other people in the dept can run this too
	setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/BPLE/BPLE"))
}
library(data.table)
library(reshape2)
getwd()
source("R/LTuniform.R")
LTC <- local(get(load("Data/LTC1.Rdata")))

.SD <- LTC[LTC$Sex == "f" & LTC$State == "AK" & LTC$Cause == "Cardio.", ]
table(.SD$Age, .SD$Year)
gete0 <- function(.SD){
	mx <- acast(.SD, Age~Year, value.var = "mxs")
	LTuniform(mx,sex=unique(.SD$Sex))$e0
}
### not working so far
LT <- LTC[LTC$Cause == "Cardio. ",]
e0sm <- LT[LT,gete0(.SD),by=list(State,Sex)]
