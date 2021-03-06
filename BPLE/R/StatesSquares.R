
if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/BPLE/BPLE")
} else {
	# in that case I'm on Berkeley system, and other people in the dept can run this too
	setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/BPLE/BPLE"))
}
source("R/StatesHeat.R")

FSCY <- local(get(load("Data/FSCY.Rdata")))
MSCY <- local(get(load("Data/MSCY.Rdata")))

FSCY <- data.table(FSCY)
MSCY <- data.table(MSCY)

setnames(FSCY,colnames(FSCY),c("Year","Decade","State","All_other","Breast","Cardio","Cerebrov",
				"Lung","Other_CVDs","Other_MN","Other_smoking","Prostate","Stomach","Uterus"))
setnames(MSCY,colnames(MSCY),c("Year","Decade","State","All_other","Breast","Cardio","Cerebrov",
				"Lung","Other_CVDs","Other_MN","Other_smoking","Prostate","Stomach"))

# aggregate: take decade means
FSCY <- FSCY[, list(All_other=mean(All_other), 
				   Breast=mean(Breast),
				   Cardio=mean(Cardio),
				   Cerebrov=mean(Cerebrov),
				   Lung=mean(Lung),
				   Other_CVDs=mean(Other_CVDs),
				   Other_MN=mean(Other_MN),
				   Other_smoking=mean(Other_smoking),
				   Stomach=mean(Stomach),
				   Uterus=mean(Uterus)), 
		by=list(State, Decade)]
MSCY <- MSCY[, list(All_other=mean(All_other), 
				Breast=mean(Breast),
				Cardio=mean(Cardio),
				Cerebrov=mean(Cerebrov),
				Lung=mean(Lung),
				Other_CVDs=mean(Other_CVDs),
				Other_MN=mean(Other_MN),
				Other_smoking=mean(Other_smoking),
				Prostate=mean(Prostate),
				Stomach=mean(Stomach)),
				by=list(State, Decade)]


MSCY   <- as.data.frame(MSCY)
FSCY   <- as.data.frame(FSCY)

MSCY$Lung          <- MSCY$Lung + MSCY$Other_smoking
FSCY$Lung          <- FSCY$Lung + FSCY$Other_smoking
MSCY$Other_smoking <- NULL
FSCY$Other_smoking <- NULL

FSCY$Cancer 	<- FSCY$Breast + FSCY$Other_MN + FSCY$Stomach + FSCY$Uterus
FSCY$Breast 	<- FSCY$Other_MN <- FSCY$Stomach <- FSCY$Uterus <- NULL
FSCY$Cerebrov 	<- FSCY$Cerebrov + FSCY$Other_CVDs
FSCY$Other_CVDs <- NULL

MSCY$Cancer   	<- MSCY$Other_MN + MSCY$Prostate + MSCY$Stomach + MSCY$Breast
MSCY$Other_MN 	<- MSCY$Prostate <- MSCY$Stomach <- MSCY$Breast <- NULL
MSCY$Cerebrov 	<- MSCY$Cerebrov + MSCY$Other_CVDs
MSCY$Other_CVDs <- NULL

# for purposes of plotting, top off upper contrib
MSCY[,-c(1,2)][MSCY[,-c(1,2)] > 2] <- 2
FSCY[,-c(1,2)][FSCY[,-c(1,2)] > 2] <- 2

F1960s <- FSCY[FSCY$Decade == 1960, ]
F1970s <- FSCY[FSCY$Decade == 1970, ]
F1980s <- FSCY[FSCY$Decade == 1980, ]
F1990s <- FSCY[FSCY$Decade == 1990, ]
F2000s <- FSCY[FSCY$Decade == 2000, ]

rownames(F1960s) <- F1960s$State
rownames(F1970s) <- F1970s$State
rownames(F1980s) <- F1980s$State
rownames(F1990s) <- F1990s$State
rownames(F2000s) <- F2000s$State

F1960s$State <- F1960s$Decade <- NULL
F1970s$State <- F1970s$Decade <- NULL
F1980s$State <- F1980s$Decade <- NULL
F1990s$State <- F1990s$Decade <- NULL
F2000s$State <- F2000s$Decade <- NULL

#colSums(MSCY[,-c(1,2)])
#colSums(FSCY[,-c(1,2)])

M1960s <- MSCY[MSCY$Decade == 1960, ]
M1970s <- MSCY[MSCY$Decade == 1970, ]
M1980s <- MSCY[MSCY$Decade == 1980, ]
M1990s <- MSCY[MSCY$Decade == 1990, ]
M2000s <- MSCY[MSCY$Decade == 2000, ]

rownames(M1960s) <- M1960s$State
rownames(M1970s) <- M1970s$State
rownames(M1980s) <- M1980s$State
rownames(M1990s) <- M1990s$State
rownames(M2000s) <- M2000s$State

M1960s$State <- M1960s$Decade <- NULL
M1970s$State <- M1970s$Decade <- NULL
M1980s$State <- M1980s$Decade <- NULL
M1990s$State <- M1990s$Decade <- NULL
M2000s$State <- M2000s$Decade <- NULL


Causesf <- colnames(FSCY)[-c(1:2)]
Causesm <- colnames(MSCY)[-c(1:2)]

breaks  <- seq(0, 2, by = .1)

xpos    <- seq(1, 6, length = 5)
ypos    <- 1:5

getcontr <- function(X,i){
	contr <- X[,i]
	names(contr) <- rownames(X)
	contr
}
ramp <- colorRampPalette(RColorBrewer::brewer.pal(9,"OrRd"),space="Lab")
##############################
# Females
#graphics.off()
#dev.new(height=6,width=6)
pdf("Figures/StatesDecadesF.pdf",height=6,width=8)
par(mai = c(.1,.6,.1,1.2), xaxs="i",yaxs="i", xpd=TRUE)
plot(NULL, type = 'n', xlim = c(0,6.5),ylim = range(ypos)+c(-1,1), axes = FALSE, xlab = "",ylab = "",asp=1)

for (i in 1:5){
	#rect(xpos[1]-.52,i-.52,xpos[1]+.52,i+.25,col=gray(.95),border=NA)
	StateHeat(getcontr(F1960s,i),x=xpos[1],y=i,breaks=breaks, ramp=ramp, labels = FALSE, border = NA)
	StateHeat(getcontr(F1970s,i),x=xpos[2],y=i,breaks=breaks, ramp=ramp, labels = FALSE, border = NA)
	StateHeat(getcontr(F1980s,i),x=xpos[3],y=i,breaks=breaks, ramp=ramp, labels = FALSE, border = NA)
	StateHeat(getcontr(F1990s,i),x=xpos[4],y=i,breaks=breaks, ramp=ramp, labels = FALSE, border = NA)
	StateHeat(getcontr(F2000s,i),x=xpos[5],y=i,breaks=breaks, ramp=ramp, labels = FALSE, border = NA)
}
causelabf <- gsub(pattern="_",replacement = " ",Causesf)
text(.2,ypos-.4,causelabf,pos=2,xpd=TRUE)
text(xpos,ypos[length(ypos)]+.5,paste0(seq(1960,2000,by=10),"s"))

lbrks <- pretty(breaks)
yat <- (1:length(lbrks)) / 2 + 2
xat <- 7
rect(xat,yat[-length(yat)],xat + .4,yat[-1],col=ramp(length(lbrks)-1))
text(xat+.4,yat,paste0(lbrks,c("","","","","+")),pos=4)
text(xat+.2,max(yat)+.3,"Years e(0)")
dev.off()
##############################
# Males
pdf("Figures/StatesDecadesM.pdf",height=6,width=8)
par(mai = c(.1,.6,.1,1.2), xaxs="i",yaxs="i", xpd=TRUE)
plot(NULL, type = 'n', xlim = c(0,6.5),ylim = range(ypos)+c(-1,1), axes = FALSE, xlab = "",ylab = "",asp=1)

for (i in 1:5){
	#rect(xpos[1]-.52,i-.52,xpos[1]+.52,i+.25,col=gray(.95),border=NA)
	StateHeat(getcontr(M1960s,i),x=xpos[1],y=i,breaks=breaks, ramp=ramp, labels = FALSE, border = NA)
	StateHeat(getcontr(M1970s,i),x=xpos[2],y=i,breaks=breaks, ramp=ramp, labels = FALSE, border = NA)
	StateHeat(getcontr(M1980s,i),x=xpos[3],y=i,breaks=breaks, ramp=ramp, labels = FALSE, border = NA)
	StateHeat(getcontr(M1990s,i),x=xpos[4],y=i,breaks=breaks, ramp=ramp, labels = FALSE, border = NA)
	StateHeat(getcontr(M2000s,i),x=xpos[5],y=i,breaks=breaks, ramp=ramp, labels = FALSE, border = NA)
}
causelabm <- gsub(pattern="_",replacement = " ",Causesm)
text(.2,ypos-.4,causelabf,pos=2,xpd=TRUE)
text(xpos,ypos[length(ypos)]+.5,paste0(seq(1960,2000,by=10),"s"))

lbrks <- pretty(breaks)
yat <- (1:length(lbrks)) / 2 + 2
xat <- 7
rect(xat,yat[-length(yat)],xat + .4,yat[-1],col=ramp(length(lbrks)-1))
text(xat+.4,yat,paste0(lbrks,c("","","","","+")),pos=4)
text(xat+.2,max(yat)+.3,"Years e(0)")
dev.off()


colMeans(F1960s)
colMeans(F1970s)
colMeans(F1980s)
colMeans(F1990s)
colMeans(F2000s)

hist(F1960s[,"Cancer"])
hist(F2000s[,"Cancer"])





