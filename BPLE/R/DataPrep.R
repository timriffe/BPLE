
# Author: tim
###############################################################################

if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/BPLE/BPLE")
} else {
	# in that case I'm on Berkeley system, and other people in the dept can run this too
	setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/BPLE/BPLE"))
}
library(reshape2)
library(magrittr)
library(data.table)
library(MortalitySmooth)
library(HMDHFDplus)

# functions used here:
# convert mxc to proportions ( now imported as such, as fractions)
makeprop <- function(Mxc){
	prop <- Mxc 
	if (sum(prop) > 0){
		prop <- prop / sum(prop)
	}
	prop
}
# we smooth all-cause mortality 2d.
# given a matrix od DX and EX, lets smooth rates!
sm.mat <- function(DX, EX){
	ages  		<- as.integer(rownames(DX))
	years 		<- as.integer(colnames(DX))
	W     		<- EX
	W[W > 0] 	<- 1
	W[DX < 1 & W == 1] 	<- .1
	fit   <- Mort2Dsmooth(
			x = ages, 
			y = years, 
			Z = DX,
			offset = log(EX), 
			W = W,
			control=list(MAX.IT=200))
	mxs <- exp(fit$logmortality)
	mxs <- melt(mxs, varnames=c("Age","Year"), value.name = "mxs")
	mxs
}
# the iterator function for data.table
sm.chunk <- function(.SD){
	# using the Dx that came from the HMD smoothed
	# mx, keeps upper ages under control a bit more.
	DX      <- acast(.SD, Age~Year, value.var = "Dxhat")
	EX      <- acast(.SD, Age~Year, value.var = "Exposure")
	mxs     <- sm.mat(DX,EX)
	.SD$mxs <- mxs$mxs
	.SD
}
sm.chunk.cause <- function(.SD){
	# using the Dx that came from the HMD smoothed
	# mx, keeps upper ages under control a bit more.
	DX      <- acast(.SD, Age~Year, value.var = "Dxc5")
	EX      <- acast(.SD, Age~Year, value.var = "Exposure")
	if (sum(DX > 0) > 100){
		mxs     <- sm.mat(DX,EX)
		.SD$mxcs <- mxs$mxs
	} else {
		.SD$mxcs <- .SD$mxc5
	}
	.SD
}
######################################################
# now processing.

LT 			<- local(get(load("Data/MilaStatesSingleAge.Rdata")))

# fractions sent from Magali Mar 2, 2016.
# 1959-2013 (now go beyond Mila's lifetables by 3 years
frac 		<- local(get(load("Data/cod.fractions.Rdata")))
frac 		<- melt(frac, varnames = c("State","Year","Sex","Age","Cause"), value.name = "Fxc")
frac$State 	<- as.character(frac$State)
frac$Sex   	<- as.character(frac$Sex)
frac$Age   	<- as.character(frac$Age)


frac$Age <- age2int(frac$Age)
frac$Sex <- ifelse(frac$Sex == "M","m","f")

###################################################
# apply fraction to single age mx 

#range(LT$Year)
#range(frac$Year)
# now with causes through 2013,
# we need to cut off at 2010
frac <- frac[frac$Year <= 2010, ]

# this step added for memory management.
# each state to be processed separately.
# I don't like saving intermediate files,
# but this requires > 8Gb memory otherwise
states <- unique(frac$State)
for (st in states){
	ST <- frac[frac$State == st, ]
	save(ST, file = paste0("Data/StateChunks/frac/",st,".Rdata"))
}
rm(frac);gc()

# lots of zeros
sum(LT$Mx==0 & !is.na(LT$Mx))

# this is 'hat' due to old age smoothing
LT$Dxhat <- LT$Exposure * LT$mx

# takes a while, warnings for interpolation (exppsure = 0)
LT <- suppressWarnings(LT[, sm.chunk(.SD), by = list(State, Sex)])

# now we have a smoothed mx, very nice.
###############################################
# next step: attach proportions:
# 1) make age groups for LT

LT$Age5                            <- LT$Age - LT$Age %% 5
LT$Age5[LT$Age5 == 0 & LT$Age > 0] <- 1
LT$Age5[LT$Age5 > 100]             <- 100 # because they'll all get same proportions...

#frac <- as.data.frame(frac)
#LT   <- as.data.frame(LT)

states <- unique(LT$State)
st <- states[1]
library(parallel)
Nothing <- mclapply(states, function(st, LT){
			ST      <- LT[LT$State == st, ]
			fracc   <- local(get(load(paste0("Data/StateChunks/frac/",st,".Rdata"))))
			STc 	<- do.call(rbind,lapply(unique(fracc$Cause), function(cause, ST){
								ST$Cause <- cause
								ST
							}, ST = ST))
			values        	<- fracc$Fxc
			names(values) 	<- with(fracc,paste(State, Year, Sex, Age, Cause))
			STc$cprop     	<- values[with(STc, paste(State, Year, Sex, Age5, Cause))]
			# ok mxc is the product:
			STc$mxc5 		<- STc$mxs * STc$cprop
			STc$Dxc5 		<- STc$mxc5 * STc$Exposure
			save(STc, file = paste0("Data/StateChunks/LTC/",st,".Rdata"))
			NULL
		}, LT=LT, mc.cores = 4)
Nothing <- mclapply(states, function(st){
		
			STc   <- local(get(load(paste0("Data/StateChunks/LTC/",st,".Rdata"))))
			STc   <- data.table(STc)
			STc[, sm.chunk.cause(.SD), by = list(Sex, Cause)]
			head(STc)
			STc[, prop2 := makeprop(mxcs), by = list(Year, Sex, Age)]
			STc$mxcs2 <- STc$prop2 * STc$mxs
			save(STc, file = paste0("Data/StateChunks/LTC/",st,".Rdata"))
			NULL
		}, LT=LT, mc.cores = 4)
LexisUtils::LexisMap(acast(STc[STc$Cause == 16 & STc$Sex == "m", ], Age~Year, value.var = "mxs"), log = FALSE)
sm.chunk.cause(STc)
# make an LT for each Cause...
#LTC <- do.call(rbind,lapply(unique(frac$Cause),function(cause, LT){
#		LT$Cause <- cause
#		LT
#		}, LT=LT))
## use the recvec method, easiest to remember...
## add prop to LT
#
#values        <- frac$Fxc
#names(values) <- with(frac,paste(State, Year, Sex, Age, Cause))
#LTC$cprop     <- values[with(LTC, paste(State, Year, Sex, Age5, Cause))]




# ok mxc is the product:
#LTC$mxc5 <- LTC$mxs * LTC$cprop

# now it's best to smooth the cause props in nearly the same way: 2d
# then reconstrain.
#LTC$Dxc5 <- LTC$mxc5 * LTC$Exposure
#tapply(LTC$Dxc5, list(LTC$Sex, LTC$Cause), sum)


LTC <- LTC[!(LTC$Sex == "m" & LTC$Cause == 4), ]
LTC <- LTC[!(LTC$Sex == "f" & LTC$Cause == 8), ]
LTC <- LTC[!(LTC$Sex == "m" & LTC$Cause == 22), ]


save(LTC, file = "Data/LTC1.Rdata")
# Change name, so as to call up an error rather than choke memory
##############################################
LTC1 <- local(get(load("Data/LTC1.Rdata")))
#print(object.size(LTC1),units="Mb")
# takes a long time
LTC1 <- data.table(LTC1)
LTC1 <- LTC1[, sm.chunk.cause(.SD), by = list(State,Sex,Cause)]


# saver intermediary because this takes so long, just in case.
save(LTC1, file = "Data/LTC1.Rdata")
head(LTC1)
# next step is to convert to props and multiply back into mxs.
LTC[,prop2 := makeprop(mxcs), by = list(Year, State, Sex, Age)]
LTC$mxcs2 <- LTC$prop2 * LTC$mxs

# take look at example for small state...
#plot(with(LTC, mxcs2[Sex == "f" & Year == 1959 & Cause == "Breast"& State == "AK"]), log = 'y', type = 'l')
#points(with(LTC, mxcs[Sex == "f" & Year == 1959 & Cause == "Breast"& State == "AK"]))
#points(with(LTC, mxc5[Sex == "f" & Year == 1959 & Cause == "Breast"& State == "AK"]),pch=3)

# so depending on whether the result is smooth or not, may be due for a second round of smoothing
# within cause. Or maybe we accept this? Probably enough juice for now.
save(LTC, file = "Data/LTC1.Rdata")
