# Author: tim states square heatmaps.
###############################################################################

# custom, for artisanal panel plots
centercoords <- function(x1,x2,y1,y2){
	xrange <- range(c(x1,x2))
	yrange <- range(c(y1,y2))
	div    <- max(c(xrange,yrange))
	x1     <- x1/div - .5
	x2     <- x2/div - .5
	y1     <- y1/div - .5
	y2     <- y2/div - .5
	
	list(x1=x1,x2=x2,y1=y1,y2=y2)
}
'%==%' <- function(x,y){
	!is.na(x) & !is.na(y) & x == y
}
#ST <- matrix(c("AK",rep("",10),"ME",
#		rep("",10),"VT","NH",
#		"","WA","ID","MT","ND","MN","IL","WI","MI","NY","RI","MS",
#		"","OR","NV","WY","SD","IA","IN","OH","PA","NJ","CT","",
#		"","CA","UT","CO","NB","MO","KY","WV","VA","MD","DL","",
#		"","","AZ","NM","KS","AR","TN","NC","SC","DC","","",
#		"","","","","OK","LA","MS","AL","GA","","","",
#		"HA","","","","TX","","","","","FL","",""),ncol = 12, nrow = 8,byrow=TRUE)
#
#ST[ST == ""] <- NA
#Color <- matrix(NA,nrow=nrow(ST),ncol=ncol(ST))
#Color[!is.na(ST[8:1,])] <- gray(.5)
#
#x1 <- col(ST) - 1
#x2 <- col(ST)
#y1 <- row(ST) - 1
#y2 <- row(ST)
#
#coords <- centercoords(x1,x2,y1,y2)
#
#
#plot(NULL, xlim = c(-.5,.5),ylim=c(-.5,.5),xlab = "", ylab = "", type= "n", axes = FALSE,asp=1)
#with(coords,rect(x1,y1,x2,y2,border="white",col = Color))
#with(coords,text((x1+x2)/2,(y1+y2)/2,ST[8:1,],col="white"))

#install.packages("spatstats")
#require(devtools)
#install_github('spatstat/spatstat')
# use to determine color for printing state name
# x must be a named vector
StateHeat <- function(contr, x=0,y=0,breaks = pretty(contr), 
		ramp = colorRampPalette(RColorBrewer::brewer.pal(9,"OrRd"),space="Lab"),labels = TRUE,...){
	states 			<- names(contr)
	cols   			<- ramp(length(breaks)-1)
	colors 			<- as.character(cut(contr,breaks=breaks,labels=cols))
	names(colors) 	<- states
	
	# now make states object:
	ST <- matrix(c("AK",rep("",10),"ME",
					rep("",10),"VT","NH",
					"","WA","ID","MT","ND","MN","IL","WI","MI","NY","RI","MA",
					"","OR","NV","WY","SD","IA","IN","OH","PA","NJ","CT","",
					"","CA","UT","CO","NE","MO","KY","WV","VA","MD","DE","",
					"","","AZ","NM","KS","AR","TN","NC","SC","DC","","",
					"","","","","OK","LA","MS","AL","GA","","","",
					"HI","","","","TX","","","","","FL","",""),ncol = 12, nrow = 8,byrow=TRUE)
	
	ST[ST == ""]     <- NA
	
	x1   			 <- col(ST) - 1
	x2   			 <- col(ST)
	y1   			 <- row(ST) - 1
	y2   			 <- row(ST)
	
	xrange <- range(c(x1,x2))
	yrange <- range(c(y1,y2))
	div    <- max(c(xrange,yrange))
	x1     <- x1/div - .5 + x
	x2     <- x2/div - .5 + x
	y1     <- y1/div - .5 + y
	y2     <- y2/div - .5 + y
	
	Colors           <- colors[ST]
	dim(Colors)      <- dim(ST)
	
	level            <- colorspace::hex2RGB(to.grey(Colors))@coords[, 1]
	dim(level)       <- dim(ST)
	ltdk             <- level
	ltdk[ltdk > .5 ] <- gray(.3)
	ltdk[ltdk <= .5] <- gray(.9)
	ltdk[is.na(ST)]  <- NA
	
	#border <- ltdk
	#plot(NULL, xlim = c(0,12),ylim=c(0,8),xlab = "", ylab = "", type= "n", axes = FALSE,asp=1)
	rect(x1,y1,x2,y2,col = Colors[8:1,], border = NA)
	if (labels){
		text((x1+x2)/2,(y1+y2)/2,ST[8:1,],col=ltdk[8:1, ],...)
	}
	
	# outline?
    # AK, HI, FL
    ST <- ST[8:1, ]
    rect(x1[ST %==% "AK"],y1[ST %==% "AK"],x2[ST %==% "AK"],y2[ST %==% "AK"],border=gray(.5) )
	rect(x1[ST %==% "HI"],y1[ST %==% "HI"],x2[ST %==% "HI"],y2[ST %==% "HI"],border=gray(.5) )
	rect(x1[ST %==% "FL"],y1[ST %==% "FL"],x2[ST %==% "FL"],y2[ST %==% "FL"],border=gray(.5) )
	
	lines(c(1,2,2,4,4,5,5,9,9,10,10,11,11,12,12,11,11,10,10,1,1) / div - .5 + x,
			c(3,3,2,2,0,0,1,1,2,2,3,3,5,5,8,8,7,7,6,6,3)/ div - .5 + y,col=gray(.5))
}

#plot(NULL, xlim = c(-.5,.5),ylim=c(-.5,.5),xlab = "", ylab = "", type= "n", axes = FALSE,asp=1)
#StateHeat(contr)







