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
Age <- .SD$Age
Year <- .SD$Year
mx <- .SD$mxs
Sex <- .SD$Sex

#gete0 <- function(CHUNK){
#	mx <- acast(CHUNK, Age~Year, value.var = "mxs")
#	data.frame(State = unique(CHUNK$State),
#			   Year = sort(unique(CHUNK$Year)),
#			   Sex = unique(CHUNK$Sex),
#			   e0 = LTuniform(mx,sex=unique(CHUNK$Sex))$e0,
#			   e0orig = CHUNK$ex[CHUNK$Age ==0],
#			   stringsAsFactors = FALSE)
#}
#gete02 <- function(.SD){
#	mx <- acast(.SD, Age~Year, value.var = "mxs")
#	c(LTuniform(mx,sex=unique(.SD$Sex))$ex)
#}
#
## for some reason couldn't do this with
## normal data.table. tried too much, so I gave up
#LT       <- LTC[LTC$Cause == "Cardio.",]
#LT       <-as.data.frame(LT)
#LTL      <- split(LT, list(LT$State,LT$Sex))
#e0states <- do.call(rbind, lapply(LTL, gete0))
#head(e0states)
#e0ms <- acast(e0states[e0states$Sex == "m", ],State~Year,value.var = "e0")
#e0m  <- acast(e0states[e0states$Sex == "m", ],State~Year,value.var = "e0orig")
#e0fs <- acast(e0states[e0states$Sex == "f", ],State~Year,value.var = "e0")
#e0f  <- acast(e0states[e0states$Sex == "f", ],State~Year,value.var = "e0orig")
#
#matplot(1959:2004, t(e0m), type = 'l', lty = 2, lwd=2, col = "#00000090")
#matplot(1959:2004, t(e0ms), type = 'l', lty =1, col = "#FF000050", add=TRUE)
#
#matplot(1959:2004, t(e0f), type = 'l', lty = 2, lwd=2, col = "#00000090")
#matplot(1959:2004, t(e0fs), type = 'l', lty =1, col = "#FF000050", add=TRUE)

# decide to decompose smooth vs BP smoothed. Remove stochasticity from results.
# in next iteration find out how to be smooth with respect to age, but a bit 
# less smooth over time.


# now decompose
# 1) mx is the sum of mxc

# we need a function that eats mxc, converts to mx, then spits back 
# e0. No problem. 1 year at a time...

# mxc is a matrix, Cause in columns, Age in rows, variable 'mxcs2',
# as it already has the property of summing to mxs, which we want to use.

# need the male and female BPmxc:

Mins   <- LTC[,min(mxcs2),by=list(Year, Sex, Age, Cause)]

setnames(Mins,"V1","mxcmin")

# ok, now we have Age~Cause matrices for the best practices. Each list element is a year.
FBPL <- lapply(1959:2004, function(yr,Mins){
			acast(Mins[Mins$Year == yr & Mins$Sex == "f", ], Age~Cause, value.var = "mxcmin")
		}, Mins = Mins)
MBPL <- lapply(1959:2004, function(yr,Mins){
			acast(Mins[Mins$Year == yr & Mins$Sex == "m", ], Age~Cause, value.var = "mxcmin")
		}, Mins = Mins)
MST <-  lapply(1959:2004, function(yr,LTC){
			YR <- LTC[LTC$Year == yr & LTC$Sex == "m", ]
			STlist <- lapply(unique(YR$State), function(state, YR){
						acast(YR[YR$State == state, ], Age~Cause, value.var = "mxcs2")
					}, YR = YR)
			names(STlist) <- unique(YR$State)
			STlist
		}, LTC = LTC)
FST <-  lapply(1959:2004, function(yr,LTC){
			YR <- LTC[LTC$Year == yr & LTC$Sex == "f", ]
			STlist <- lapply(unique(YR$State), function(state, YR){
						acast(YR[YR$State == state, ], Age~Cause, value.var = "mxcs2")
					}, YR = YR)
			names(STlist) <- unique(YR$State)
			STlist
		}, LTC = LTC)
names(MST) <- 1959:2004
names(FST) <- 1959:2004
names(FBPL) <- 1959:2004
names(MBPL) <- 1959:2004
# next
# now for a decomposable function, we need a function that gives
# e0 based on a vector of mxc
mxc      <- MST[[1]][[1]]
mxcvec   <- c(FBPL[[1]])
e0frommxc <- function(mxcvec,sex){
	dim(mxcvec) <- c(111,length(mxcvec)/111)
	mx          <- rowSums(mxcvec)
	LTuniformvecminimal(mx,sex)
}
e0frommxc(c(FBPL[[1]]),"f")
e0frommxc(c(FST[[1]][[1]]),"f")


library(DecompHoriuchi)
DecompContinuousOrig

# Males, takes a long time


system.time(contrib2 <- mydecomp(
		func = e0frommxc, 
		rates1 = c(FST[[1]][[1]]),
		rates2 = c(FBPL[[1]]),
				N = 20,
				sex = "f"))
15 * 51 * length(1959:2004)
(35190 / 60) / 60

#################################################################
# this will take a long time, so best prepare for a server run. #
#################################################################
library(parallel)

# takes a long time!
Females <- list()
Males   <- list()

# ca 2 hours on 4 cores
for (yr in 1959:2004){
	cat("\nYear\n")
	Females[[as.character(yr)]] <- mclapply(FST[[as.character(yr)]], function(YRST, YRBP, e0frommxc){
				 contrib <- mydecomp(
			         func = e0frommxc, 
			         rates1 = c(YRST),
			         rates2 = c(YRBP),
			         N = 20,
			         sex = "f"
			    )
				dim(contrib)        <- dim(YRST)
				dimnames(contrib)   <- dimnames(YRST)
				contrib
			}, YRBP = FBPL[[as.character(yr)]], e0frommxc = e0frommxc, mc.cores = 4)
	gc()
	# repeat for males
	Males[[as.character(yr)]] <- mclapply(MST[[as.character(yr)]], function(YRST, YRBP, e0frommxc){
				contrib <- mydecomp(
						func = e0frommxc, 
						rates1 = c(YRST),
						rates2 = c(YRBP),
						N = 20,
						sex = "m"
				)
				dim(contrib)        <- dim(YRST)
				dimnames(contrib)   <- dimnames(YRST)
				contrib
			}, YRBP = MBPL[[as.character(yr)]], e0frommxc = e0frommxc, mc.cores = 4)
	gc()
}

save(Females, file = "Data/fcontrib.Rdata")
save(Males, file = "Data/mcontrib.Rdata")







		


