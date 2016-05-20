###############################################################################
if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/BPLE/BPLE")
} else {
	# in that case I'm on Berkeley system, and other people in the dept can run this too
	setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/BPLE/BPLE"))
}
source("R/colourtools.R")
library(colourtools)
#NYT <- read.csv("/home/tim/git/BPLE/BPLE/Data/NYTgridUS.csv")
#
#recvec <- 0:7
#names(recvec) <- 7:0
#NYT$y <- recvec[as.character(NYT$y)]
X <- NYT
#class(NYT) <-"gridUS"
plot.gridUS <- function(X,label=TRUE,offsetlabelcolor = TRUE,...){
	plot(NULL, type = "n", 
			axes=FALSE,
			xlab="",
			ylab="",
			asp=1,
			xlim=range(X$x)+c(0,1),
			ylim=range(X$y)+c(0,1))
	
	names(X) <- tolower(names(X))
	cols <- any(grepl("col",names(X)))
	if (!cols){
		X$col <- rep(gray(.9),length(X$key))
	} else {
		names(X)[grepl("col",names(X))] <- "col"
	}
	rect(X$x,X$y,X$x+1,X$y+1,col=X$col,...)
	
	if (label){
		if (offsetlabelcolor){
			level            <- colorspace::hex2RGB(to.grey(X$col))@coords[, 1]
			labcols <- X$col
			labcols[level > .5 ] <- gray(.3)
			labcols[level <= .5] <- gray(.9)
		}
		text(X$x+.5,X$y+.5,X$key, col=labcols)
	}
	
}
# regions 
plot.gridUS <- function(X,label=TRUE,offsetlabelcolor = TRUE,...){
	plot(NULL, type = "n", 
			axes=FALSE,
			xlab="",
			ylab="",
			asp=1,
			xlim=range(X$x)+c(0,1),
			ylim=range(X$y)+c(0,1),...)
	
	names(X) <- tolower(names(X))
	cols <- any(grepl("col",names(X)))
	if (!cols){
		X$col <- rep(gray(.9),length(X$key))
	} else {
		names(X)[grepl("col",names(X))] <- "col"
	}
	rect(X$x,X$y,X$x+1,X$y+1,col=X$col,...)
	
	if (label){
		if (offsetlabelcolor){
			level            <- colorspace::hex2RGB(to.grey(X$col))@coords[, 1]
			labcols <- X$col
			labcols[level > .5 ] <- gray(.3)
			labcols[level <= .5] <- gray(.9)
		}
		text(X$x+.5,X$y+.5,X$key, col=labcols)
	}
	
}


save(NYT,file="/home/tim/git/BPLE/BPLE/Data/NYTgridUS.Rdata")

library(reshape2)
library(RColorBrewer)
asdr <- local(get(load("Data/asdr4085.Rdata")))
asdr <- melt(asdr, varnames = c("State","Year","Sex","Cause"),value.name = "ASDR")
asdr$State <- as.character(asdr$State )
asdr$Sex <- as.character(asdr$Sex )
asdr$Cause <- as.character(asdr$Cause )
NYT <- local(get(load("/home/tim/git/BPLE/BPLE/Data/NYTgridUS.Rdata")))

brksf59 <- quantile(with(asdr, ASDR[Sex == "female" & Year == 1959 & Cause == "All"]),p=seq(0,1,by=.2))+c(-1,0,0,0,0,1)
brksm59 <- quantile(with(asdr, ASDR[Sex == "male" & Year == 1959& Cause == "All"]),p=seq(0,1,by=.2))+c(-1,0,0,0,0,1)
brksf10 <- quantile(with(asdr, ASDR[Sex == "female" & Year == 2010& Cause == "All"]),p=seq(0,1,by=.2))+c(-1,0,0,0,0,1)
brksm10 <- quantile(with(asdr, ASDR[Sex == "male" & Year == 2010& Cause == "All"]),p=seq(0,1,by=.2))+c(-1,0,0,0,0,1)



display.brewer.all()
ramp <- brewer.pal(5,"YlOrBr")
NYT$col <- 
		#Sex <- "male"; Cause  = "All"; Year = 1959
getcolstate <- function(.Sex, .Year, .Cause, ramp = brewer.pal(5,"YlOrBr"), p = seq(0,1,by=.2),asdr){
    ind <- with(asdr, Sex == .Sex & Year == .Year & Cause == .Cause)
    breaks <- quantile(asdr$ASDR[ind],p = p) 
    breaks[1] <- breaks[1] - 1
    breaks[length(breaks)] <- breaks[length(breaks)] + 1
	cols <- as.character(cut(asdr$ASDR[ind], breaks = breaks, labels = ramp))
	names(cols) <- asdr$State[ind]
	cols
}


NYT$col <- getcolstate("male",1959, "All",asdr=asdr)[NYT$key]
plot.gridUS(NYT)

NYT$col <- getcolstate("male",2010, "All",asdr=asdr)[NYT$key]
plot.gridUS(NYT)

NYT$col <- getcolstate("female",1959, "All",asdr=asdr)[NYT$key]
plot.gridUS(NYT)

NYT$col <- getcolstate("female",2010, "All",asdr=asdr)[NYT$key]
plot.gridUS(NYT)


# hand-drawing borders, use snapping. Spit back coords.
#locator.snap <- function(n=1){
#	xy <- locator(n)
#	xy$x <- round(xy$x)
#	xy$y <- round(xy$y)
#	xy
#}
#
#line.snap <- function(...){
#	cat("\nstart clicking around in the plot\n")
#	cat("\nto close the line, just click in a place you've already clicked\n")
#	xy <- locator.snap(1)
#	x <- c(xy$x)
#	y <- c(xy$y)
#	xy <- locator.snap(1)
#	while(! any(xy$x == x & xy$y == y)){
#		x <- c(x,xy$x)
#		y <- c(y,xy$y)
#		lines(x,y,...)
#		xy <- locator.snap(1)
#	}
#	if (xy$x == x[1] & xy$y == y[1]){
#		x <- c(x,xy$x)
#		y <- c(y,xy$y)
#	}
#	list(x=x,y=y)
#}

#NewEngland  <- line.snap(col = "yellow",lwd=2)
#MidAtlantic <- line.snap(col = "yellow",lwd=2)
#MidAtlantic <- line.snap(col = "yellow",lwd=2)
#EastNorthCentral <- line.snap(col = "yellow",lwd=2)
#WestSouthCentral <- line.snap(col = "yellow",lwd=2)
#WestNorthCentral <- line.snap(col = "yellow",lwd=2)
#Mountain <- line.snap(col = "yellow",lwd=2)
#SouthAtlantic <- line.snap(col = "yellow",lwd=2)
#EastSouthCentral <- line.snap(col = "yellow",lwd=2)
#Pacific <- list(line.snap(col = "yellow",lwd=2),
#		line.snap(col = "yellow",lwd=2),
#		line.snap(col = "yellow",lwd=2),
#		line.snap(col = "yellow",lwd=2)
#		)
#Pacific$x <- c(Pacific[[1]]$x,NA,
#		Pacific[[2]]$x,NA,
#		Pacific[[3]]$x,NA,
#		Pacific[[4]]$x)
#Pacific$y <- c(Pacific[[1]]$y,NA,
#		Pacific[[2]]$y,NA,
#		Pacific[[3]]$y,NA,
#		Pacific[[4]]$y)
		
#plot.gridUS(NYT)
#lines(NewEngland,col=gray(.7),lwd=3)
#lines(MidAtlantic,col=gray(.7),lwd=3)
#lines(EastNorthCentral,col=gray(.7),lwd=3)
#lines(WestSouthCentral,col=gray(.7),lwd=3)
#lines(WestNorthCentral,col=gray(.7),lwd=3)
#lines(Mountain,col=gray(.7),lwd=3)
#lines(SouthAtlantic,col=gray(.7),lwd=3)
#lines(EastSouthCentral,col=gray(.7),lwd=3)
#lines(Pacific,col=gray(.7),lwd=3)

#NYTRegionsOutline <- list(NewEngland=NewEngland,
#		MidAtlantic=MidAtlantic,
#		EastNorthCentral=EastNorthCentral,
#		WestSouthCentral=WestSouthCentral,
#		WestNorthCentral=WestNorthCentral,
#		Mountain=Mountain,
#		SouthAtlantic=SouthAtlantic,
#		EastSouthCentral=EastSouthCentral,
#		Pacific=Pacific)
#dput(NYTRegionsOutline)
#NYTRegionsOutline <- structure(list(NewEngland = structure(list(x = c(11, 12, 12, 
#										10, 10, 9, 9, 11, 11), y = c(8, 8, 5, 5, 6, 6, 7, 7, 8)), .Names = c("x", 
#								"y")), MidAtlantic = structure(list(x = c(9, 9, 11, 11, 10, 10, 
#										9), y = c(6, 4, 4, 5, 5, 6, 6)), .Names = c("x", "y")), EastNorthCentral = structure(list(
#								x = c(9, 6, 6, 8, 8, 9, 9), y = c(4, 4, 6, 6, 5, 5, 4)), .Names = c("x", 
#								"y")), WestSouthCentral = structure(list(x = c(3, 3, 4, 4, 6, 
#										6, 5, 5, 3), y = c(1, 2, 2, 3, 3, 2, 2, 1, 1)), .Names = c("x", 
#								"y")), WestNorthCentral = structure(list(x = c(6, 6, 4, 4, 3, 
#										3, 6), y = c(6, 3, 3, 5, 5, 6, 6)), .Names = c("x", "y")), Mountain = structure(list(
#								x = c(2, 3, 3, 4, 4, 2, 2, 1, 1, 2, 2), y = c(6, 6, 5, 5, 
#										2, 2, 3, 3, 4, 4, 6)), .Names = c("x", "y")), SouthAtlantic = structure(list(
#								x = c(7, 7, 8, 8, 9, 9, 11, 11, 7), y = c(4, 0, 0, 1, 1, 
#										3, 3, 4, 4)), .Names = c("x", "y")), EastSouthCentral = structure(list(
#								x = c(5, 7, 7, 6, 6, 5, 5), y = c(1, 1, 4, 4, 2, 2, 1)), .Names = c("x", 
#								"y")), Pacific = structure(list(structure(list(x = c(0, 0, 1, 
#														1, 0), y = c(1, 0, 0, 1, 1)), .Names = c("x", "y")), structure(list(
#												x = c(0, 1, 1, 0, 0), y = c(3, 3, 4, 4, 3)), .Names = c("x", 
#												"y")), structure(list(x = c(1, 1, 2, 2, 1), y = c(4, 6, 6, 4, 
#														4)), .Names = c("x", "y")), structure(list(x = c(0, 0, 1, 1, 
#														0), y = c(7, 8, 8, 7, 7)), .Names = c("x", "y")), x = c(0, 0, 
#										1, 1, 0, NA, 0, 1, 1, 0, 0, NA, 1, 1, 2, 2, 1, NA, 0, 0, 1, 1, 
#										0), y = c(1, 0, 0, 1, 1, NA, 3, 3, 4, 4, 3, NA, 4, 6, 6, 4, 4, 
#										NA, 7, 8, 8, 7, 7)), .Names = c("", "", "", "", "x", "y"))), .Names = c("NewEngland", 
#				"MidAtlantic", "EastNorthCentral", "WestSouthCentral", "WestNorthCentral", 
#				"Mountain", "SouthAtlantic", "EastSouthCentral", "Pacific"))


#plot.gridUS(NYT)
#lapply(NYTRegionsOutline,polygon,border="magenta",lwd=3,lty=1,xpd=TRUE)
#
#lapply(NYTRegionsOutline,polygon,border="blue",lwd=1,lty=1)
#NYT$col <- NA
#par(xaxs='i',yaxs='i',mai=c(.1,.1,.1,.1))
#plot.gridUS(NYT,bg=gray(.5))
#lapply(NYTRegionsOutline,polygon,border="black")

# rather than outline (doesn't look good), how about adding spaces
# between regions?

offsets <- list(
Pacific             = data.frame(Region = "Pacific",State = c("AK","CA","HI","OR","WA"), 
		xoff = rep(-.1,5),yoff=rep(.1,5)),
NewEngland  		= data.frame(Region = "NewEngland",State = c("CT","ME","MA","NH","RI","VT"), 
		xoff = rep(.4,6),yoff=rep(0,6)),
MidAtlantic 		= data.frame(Region = "MidAtlantic",State = c("NJ","NY","PA"), 
		xoff = rep(.3,3),yoff=rep(-.1,3)),
EastNorthCentral 	= data.frame(Region = "EastNorthCentral",State = c("IL","IN","MI","OH","WI"), 
		xoff = rep(.2,5),yoff=rep(-.1,5)),
WestSouthCentral 	= data.frame(Region = "WestSouthCentral",State = c("AR","LA","OK","TX"), 
		xoff = rep(.1,4),yoff=rep(-.1,4)),
WestNorthCentral 	= data.frame(Region = "WestNorthCentral",State = c("IA","KS","MN","MO","NE","ND","SD"), 
		xoff = rep(.1,7),yoff=rep(.1,7)),
Mountain 			= data.frame(Region = "Mountain",State = c("AZ","CO","ID","MT","NV","NM","UT","WY"), 
		xoff = rep(0,8),yoff=rep(0,8)),
SouthAtlantic 		= data.frame(Region = "SouthAtlantic",State = c("DE","DC","FL","GA","MD","NC","SC","VA","WV"), 
		xoff = rep(.3,9),yoff=rep(-.2,9)),
EastSouthCentral 	= data.frame(Region = "EastSouthCentral",State = c("AL","KY","MS","TN"), 
		xoff = rep(.2,4),yoff=rep(-.2,4)))
offsets <-	do.call(rbind, offsets)
		

plot.gridUSoff <- function(X,label=TRUE,offsetlabelcolor = TRUE,offsets=NULL,...){
	if (!is.null(offsets)){
		xoff <- offsets$xoff
		names(xoff) <- offsets$State
		X$x <- X$x + xoff[X$key]
		
		yoff <- offsets$yoff
		names(yoff) <- offsets$State
		X$y <- X$y + yoff[X$key]
	}
	
	plot(NULL, type = "n", 
			axes=FALSE,
			xlab="",
			ylab="",
			asp=1,
			xlim=range(X$x)+c(0,1),
			ylim=range(X$y)+c(0,1))
	
	names(X) <- tolower(names(X))
	cols <- any(grepl("col",names(X)))
	if (!cols){
		X$col <- rep(gray(.9),length(X$key))
	} else {
		names(X)[grepl("col",names(X))] <- "col"
	}
	rect(X$x,X$y,X$x+1,X$y+1,col=X$col,...)
	
	if (label){
		if (offsetlabelcolor){
			level            <- colorspace::hex2RGB(to.grey(X$col))@coords[, 1]
			labcols <- X$col
			labcols[level > .5 ] <- gray(.3)
			labcols[level <= .5] <- gray(.9)
		}
		text(X$x+.5,X$y+.5,X$key, col=labcols)
	}
	
}
		
getwd()

NYT$col <- getcolstate("male",1959, "All",asdr=asdr)[NYT$key]
pdf("/home/tim/workspace/Other/Figures/gridregions/asdr1959male.pdf", width = 12.5/2.54,height=8.5/2.54)
par(xaxs='i',yaxs='i',mai=c(.1,.1,.1,.1))
plot.gridUSoff(NYT,offsets = offsets, border=gray(.3), lwd = .5)
dev.off()

NYT$col <- getcolstate("male",2010, "All",asdr=asdr)[NYT$key]
pdf("/home/tim/workspace/Other/Figures/gridregions/asdr2010male.pdf", width = 12.5/2.54,height=8.5/2.54)
par(xaxs='i',yaxs='i',mai=c(.1,.1,.1,.1))
plot.gridUSoff(NYT,offsets = offsets, border=gray(.3), lwd = .5)
dev.off()


NYT$col <- getcolstate("female",1959, "All",asdr=asdr)[NYT$key]
pdf("/home/tim/workspace/Other/Figures/gridregions/asdr1959female.pdf", width = 12.5/2.54,height=8.5/2.54)
par(xaxs='i',yaxs='i',mai=c(.1,.1,.1,.1))
plot.gridUSoff(NYT,offsets = offsets, border=gray(.3), lwd = .5)
dev.off()


NYT$col <- getcolstate("female",2010, "All",asdr=asdr)[NYT$key]
pdf("/home/tim/workspace/Other/Figures/gridregions/asdr2010female.pdf", width = 12.5/2.54,height=8.5/2.54)
par(xaxs='i',yaxs='i',mai=c(.1,.1,.1,.1))
plot.gridUSoff(NYT,offsets = offsets, border=gray(.3), lwd = .5)
dev.off()

