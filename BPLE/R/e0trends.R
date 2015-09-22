if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/BPLE/BPLE")
} else {
	# in that case I'm on Berkeley system, and other people in the dept can run this too
	setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/BPLE/BPLE"))
}
getwd()

LTC <- local(get(load("Data/LTC1.Rdata")))

gete0 <- function(CHUNK){
	mx <- acast(CHUNK, Age~Year, value.var = "mxs")
	data.frame(State = unique(CHUNK$State),
			Year = sort(unique(CHUNK$Year)),
			Sex = unique(CHUNK$Sex),
			e0 = LTuniform(mx,sex=unique(CHUNK$Sex))$e0,
			e0orig = CHUNK$ex[CHUNK$Age ==0],
			stringsAsFactors = FALSE)
}
gete03 <- function(mx,Sex){
	LTuniformvecminimal(mx,sex=unique(Sex))
}

# for some reason couldn't do this with
# normal data.table. tried too much, so I gave up
States <- LTC[,sum(mxcs2),by=list(State,Year,Sex,Age)]
setnames(States,"V1","mxs")

e0states <- States[,gete03(mxs,Sex), by = list(State,Year,Sex)]
setnames(e0states,"V1","e0")

# now get BPLE
Mins <- LTC[,min(mxcs2),by=list(Year, Sex, Age, Cause)]
BPmx  <- Mins[,sum(V1),by=list(Year, Sex, Age)]
setnames(BPmx,"V1","mxmin")
e0bp  <- BPmx[, gete03(mxmin,Sex), by = list(Year,Sex)]
setnames(e0bp,"V1","e0")


e0ms <- acast(e0states[e0states$Sex == "m", ],State~Year,value.var = "e0")
bpm  <- e0bp$e0[e0bp$Sex == "m"]
e0fs <- acast(e0states[e0states$Sex == "f", ],State~Year,value.var = "e0")
bpf  <- e0bp$e0[e0bp$Sex == "f"]

# now US aggregate.
USA <- LTC[,list(Dx = sum(Deaths), Ex = sum(Exposure)),by=list(Year, Sex, Age)]
USA$Mx <- USA$Dx / USA$Ex
USe0 <- USA[,gete03(Mx,Sex),by=list(Year,Sex)]
use0m  <- USe0$V1[USe0$Sex == "m"]
use0f  <- USe0$V1[USe0$Sex == "f"]

pdf("Figures/e0trendsM.pdf", height=5,width=5)
par(mai=c(.8,.8,.2,.2))
matplot(1959:2004, t(e0ms), type = 'l', lty = 1, col = "#00000050", ylim = c(60,85),
		ylab = "e(0)", xlab = "Year")
lines(1959:2004, apply(e0ms,2,max), col = "blue", lwd = 2, lty = 2)
lines(1959:2004, bpm, col = "red",lwd=2)
lines(1959:2004, use0m, col = "red",lwd=2,lty=2)
legend("bottomright",lty=c(1,2,2,1),col=c("#00000050","red","blue","red"),
		lwd=c(1,2,2,2),legend=c("state e(0)","US e(0)","vanguard e(0)","BP e(0)"),
		bty="n")
dev.off()

pdf("Figures/e0trendsF.pdf", height=5,width=5)
par(mai=c(.8,.8,.2,.2))
matplot(1959:2004, t(e0fs), type = 'l', lty =1, col = "#00000050", ylim = c(60,85),
		ylab = "e(0)", xlab = "Year")
lines(1959:2004, apply(e0fs,2,max), col = "blue", lwd = 2, lty = 2)
lines(1959:2004, bpf, col = "red",lwd=2)
lines(1959:2004, use0f, col = "red",lwd=2,lty=2)
legend("bottomright",lty=c(1,2,2,1),col=c("#00000050","red","blue","red"),
		lwd=c(1,2,2,2),legend=c("state e(0)","US e(0)","vanguard e(0)","BP e(0)"),
		bty="n")
dev.off()


# what's the most linear pattern?
yrs <- 1959:2004
summary(lm(bpm~yrs))$r.squared
summary(lm(bpf~yrs))$r.squared

max(apply(e0fs,1,function(x){
		summary(lm(x~yrs))$r.squared	
		}))

max(apply(e0ms,1,function(x){
			summary(lm(x~yrs))$r.squared	
		}))
mean(bpm - apply(e0ms,2,max))
mean(bpf - apply(e0fs,2,max))

