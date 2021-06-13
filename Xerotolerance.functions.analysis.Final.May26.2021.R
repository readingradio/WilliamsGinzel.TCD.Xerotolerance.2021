library(car)
library(doBy)
library(tidyverse)
library(RColorBrewer)
library(plotrix)
library(ROCR)

xero.model <- function (..., mc, names=as.character(1:n), treat) {
	y <- list(...)
	n <- length(y)
	first <- list()
	second <- list()
	models <- list()
	for (i in 1:n) {
		first[[i]] <- glm(y[[i]] ~ mc, family=binomial("logit"))
		second[[i]] <- glm(y[[i]] ~ poly(mc, 2), family=binomial("logit"))
	}
	names(first)<-paste(names, "1st", sep=".")
	names(second)<-paste(names, "2nd", sep=".")
	d <- data.frame(..., mc=mc)
	list(models=c(first, second), data=d, treat=treat)
}


xero.figure.base <- function (mc, treat, xl, treat.text=levels(treat)) {
	
	#mm <- rbind(1,1,2,2,2)
	#layout(mm)
	#par(mar=c(0,6,2,2))#, lwd=1, cex=1)
	par(mar=c(0,5,1,1))

	bp <- boxplot(mc ~ treat, horizontal=TRUE, boxwex=0.5, xlab="", ylab="", axes=F, box=F, ylim=xl)
	
	text(bp$stats[3,], 1:4 + 0.5, treat.text, adj=0.5)
		
	#par(mar=c(8,6,0,2), xpd=T)
	par(mar=c(7,5,0,1), xpd=T)
	plot.new()
	plot.window(xl, ylim = c(0,1))
	axis(1)
	axis(2, at=c(0,.2,.4,.6,.8,1), las=2)
	
	box(bty="L")
	title(xlab='Moisture Content (mg/mg)', ylab='Survival rate', mgp=c(3,0,0))#, main = "Recovery of fungi from wood disks")

}

xero.figure.points <- function (t, v, n, p, legend.col.offset, legend.y.inset=-0.15, legend.text=v) {
		for (i in 1:n) {
			points(t$mc.mean, t[,paste(v[i], "mean", sep=".")], pch=15, col=p[i])
			arrows(t$mc.mean, t[,paste(v[i], "mean", sep=".")], t$mc.low, length=.05, angle=90, code=3, col=p[i])
			arrows(t$mc.mean, t[,paste(v[i], "mean", sep=".")], t$mc.high, length=.05, angle=90, code=3, col=p[i])
		}
		legend("bottom", inset=c(0,legend.y.inset), legend=legend.text, ncol=n-legend.col.offset, pch=15, col=p, lty=1, cex=0.75)
}

xero.figure.generic <- function (xero.model.object, orders = rep(1,n), treat=xero.model.object$treat, pal=rainbow(n), xl = c(min(xvals),max(xvals)), legend.col.offset=0, legend.y.inset=-.425, legend.text=vars, treat.text=NULL) {
	
	n <- length(xero.model.object$models)/2

	xvals<-xero.model.object$models[[1]]$model$mc
	newx <- seq(xl[1], xl[2], by = (xl[2]-xl[1]) / 100)
	pred <- lapply(xero.model.object$models[(1:n)+(n*(orders-1))], FUN = function (x) predict(x, newdata=data.frame(mc = newx), type='response'))

	if (is.null(treat.text))
	  xero.figure.base (xero.model.object$data$mc, xero.model.object$treat, xl)
	else
	  xero.figure.base (xero.model.object$data$mc, xero.model.object$treat, xl, treat.text=treat.text)

	trtsm <- summaryBy(. ~ treat, data = cbind(xero.model.object$data, treat), FUN=c(mean, sd, function (x) mean(x) - std.error(x), function (x) mean(x) + std.error(x), function(x) length(x)), fun.names=c('mean', 'sd', 'low', 'high', 'n'))
	vars <- names(xero.model.object$data)[1:n]

	xero.figure.points(trtsm, vars, n, pal,legend.col.offset, legend.y.inset, legend.text)

 	for (i in 1:n) points(newx, pred[[i]], type = 'l', col=pal[i])

}

setwd('/Users/will1809/OneDrive - purdue.edu/Dissertation/GMW.Dissertation.Analyses/Data')
#

####
##### EXPERIMENT 4

xerodat <- read.csv("WA.Nat.infected.xerotolerance.data.csv")

xerodat$Treat <- factor(xerodat$Treat, levels=c("CRT","C30C","NaCl","LiCl"))

xerodat$Aspergillus2 <- as.integer(xerodat$M12)-1
xerodat$Aspergillus3 <- as.integer(xerodat$M1)-1
xerodat$Gm <- as.integer(xerodat$Gm)-1

xerodat$Trichoderma1 <- as.integer(xerodat$M3)-1
xerodat$Trichoderma2 <- as.integer(xerodat$M15)-1
xerodat$Clonostachys1 <- as.integer(xerodat$M2)+as.integer(xerodat$M4)+as.integer(xerodat$M8)-3
xerodat$Pseudotremella1 <- as.integer(xerodat$M20)+as.integer(xerodat$M21)-2
xerodat$Schizophyllum1 <- as.integer(xerodat$M19)-1

xerodat$MC
xerodat[xerodat$Clonostachys1==1,"MC"] %>% summary()
xerodat[xerodat$Trichoderma1==1,"MC"] %>% summary()
xerodat[xerodat$Trichoderma2==1,"MC"] %>% summary()
xerodat[xerodat$Schizophyllum1==1,"MC"] %>% summary()
xerodat[xerodat$Pseudotremella1==1,"MC"] %>% summary()
xerodat[xerodat$Geosmithia==1,"MC"] %>% summary()

with(xerodat, Treat)

m.i <- with(xerodat, xero.model(Gm, Trichoderma1, Trichoderma2, mc=MC, names=c("Gm", "Trichoderma1", "Trichoderma2"), treat=Treat))
lapply(m.i$models, summary)

m.ii <- with(xerodat, xero.model(Aspergillus2, Aspergillus3, Clonostachys1, Schizophyllum1, Pseudotremella1, mc=MC, names=c("Aspergillus2", "Aspergillus3", "Clonostachys1", "Schizophyllum1", "Pseudotremella1"), treat=Treat))
lapply(m.ii$models, summary)

m <- with(xerodat, xero.model(Gm, Trichoderma1, Trichoderma2, Clonostachys1, Aspergillus3, mc=MC, names=c("Geosmithia", "Trichoderma1", "Trichoderma2", "Clonostachys1", "Aspergillus3"), treat=Treat))


lapply(m$model, summary)
lapply(m$model, Anova)
lapply(m$model, function(x) performance(prediction(x$fitted.values, x$y),"auc"))
prediction(m$model[[1]]$fitted.values, m$model[[1]]$y)


Experiment4 <- function (x=NULL, y=NULL) {
  if (is.null(x)) {
    xero.figure.generic(m, c(1,2,2,1,2), pal=c('black','darkgreen','green','salmon','orange'), legend.col.offset=2,legend.y.inset=-0.45, xl=c(0,1))
  } else {
    if (is.null(y))
      xero.figure.generic(m, c(1,2,2,1,2), pal=c('black','darkgreen','green','salmon','orange'), legend.col.offset=2,legend.y.inset=-0.45, xl=c(0,1), legend.text=x)
    else
      xero.figure.generic(m, c(1,2,2,1,2), pal=c('black','darkgreen','green','salmon','orange'), legend.col.offset=2,legend.y.inset=-0.45, xl=c(0,1), legend.text=x, treat.text=y)
  }
}

###
### EXPERIMENT 2
###


xerodat2 <- read.csv("Xerotolerance pilot Gm 2 Aug 11.csv")

xerodat2$Treat <- factor(xerodat2$Treat, levels=c("NH3Cl","MgNO3","KOAc","LiCl"))

xerodat2$Aspergillus1 <- xerodat2$Aspergillus


m2 <- with(xerodat2, xero.model(Geosmithia, Aspergillus1, Xylaria, mc=(Wet-Dry)/Dry, names=c("Geosmithia", "Aspergillus1", "Xylaria"), treat=Treat))
m2$data

m2$data[m2$data$Xylaria==1,"mc"] %>% summary()

lapply(m2$model, summary)
lapply(m2$model, Anova)
lapply(m2$model, function(x) performance(prediction(x$fitted.values, x$y),"auc"))

Experiment2 <- function (x=NULL, y=NULL) {
  if (is.null(x)) {
    xero.figure.generic(m2, c(1,1,1), pal=c('black','orange','grey'), xl=c(0,0.5))
  } else {
    if (is.null(y))
      xero.figure.generic(m2, c(1,1,1), pal=c('black','orange','grey'), xl=c(0,0.5), legend.text=x)
    else
      xero.figure.generic(m2, c(1,1,1), pal=c('black','orange','grey'), xl=c(0,0.5), legend.text=x, treat.text=y)
  }
}

###
#### EXPERIMENT 1
###

xerodat3 <- read.csv("Xerotolerance pilot Gm.csv")

xerodat3$Treat <- factor(xerodat3$Treat, levels=c("NH3Cl","MgNO3","KOAc","LiCl"))

xerodat3$Gm_morphology[is.na(xerodat3$Gm_morphology)] <-0
xerodat3$Live.Gm.isolate[is.na(xerodat3$Live.Gm.isolate)] <-0
xerodat3$Geosmithia <- as.numeric(xerodat3$Gm_morphology | xerodat3$Live.Gm.isolate)
xerodat3$Aspergillus1 <- xerodat3$Yellow_fungus

m3 <- with(xerodat3, xero.model(Geosmithia, Aspergillus1, mc=(Wet-Dry)/Dry, names=c("Geosmithia", "Aspergillus"), treat=Treat))
m3$data
lapply(m3$model, summary)
lapply(m3$model, Anova)
lapply(m3$model, function(x) performance(prediction(x$fitted.values, x$y),"auc"))
m3$model[[1]] %>% (function(x) performance(prediction(x$fitted.values, x$y),"auc"))

levels(xerodat3$Treat)

Experiment1 <- function (x=NULL, y=NULL) {
  if (is.null(x)) {
    xero.figure.generic(m3, c(1,2), pal=c('black','orange'), xl=c(0,0.3))
  } else {
    if (is.null(y))
      xero.figure.generic(m3, c(1,2), pal=c('black','orange'), xl=c(0,0.3), legend.text=x)
    else
      xero.figure.generic(m3, c(1,2), pal=c('black','orange'), xl=c(0,0.3), legend.text=x, treat.text=y)
  }
}

m2$data[m2$data$Aspergillus1==1,"mc"] %>% c(m3$data[m3$data$Aspergillus1==1,"mc"]) %>% summary()
m2$data[m2$data$Geosmithia==1,"mc"] %>% c(m3$data[m3$data$Geosmithia==1,"mc"]) %>% c(xerodat[xerodat$Geosmithia==1,"MC"]) %>% summary()

###
#### EXPERIMENT 3
###

xerodat.sterile<-read.csv("Xerotolerance pilot Gm T3.csv")

xerodat.sterile$Treat <- factor(xerodat.sterile$Treat, levels=c("NaCl","Amb","NoSalt","LiCl"))

xerodat.sterile$MC<- with(xerodat.sterile, (Wet-Dry)/Dry)
names(xerodat.sterile)[which(names(xerodat.sterile)=="Surv")]<-"Gm"
names(xerodat.sterile)[which(names(xerodat.sterile)=="MC")]<-"mc"
xerodat.sterile.Gm<-xerodat.sterile[xerodat.sterile$Inoc=="Gm",]

trtsm <- summaryBy(. ~ Treat, data = xerodat.sterile.Gm, FUN=c(mean, sd, function (x) mean(x) - std.error(x), function (x) mean(x) + std.error(x), function(x) length(x)), fun.names=c('mean', 'sd', 'low', 'high', 'n'))
vars<-vector(length=1); vars[1]<-"Gm"

Experiment3 <- function(x=NULL, y=NULL) {
  if (is.null(y))
    with(xerodat.sterile, xero.figure.base(mc, Treat, c(0,0.2)))
  else
    with(xerodat.sterile, xero.figure.base(mc, Treat, c(0,0.2), treat.text=y))
	if (is.null(x))
    xero.figure.points(trtsm,vars,1,"black",legend.col.offset=0, legend.y.inset=-.425)
  else
    xero.figure.points(trtsm,vars,1,"black",legend.col.offset=0, legend.y.inset=-.425, legend.text=x)
}

# multipanel
mm<- rbind(1,1,2,2,2,2,2)
mm2<- rbind(3,3,4,4,4,4,4)
mm3<- rbind(5,5,6,6,6,6,6)
mm4<- rbind(7,7,8,8,8,8,8)

	mm.multi <-cbind(rbind(mm,mm3),rbind(mm2,mm4))
	mm.multi
	
	quartz()
	layout(mm.multi)
	
	Experiment1();mtext("(A)",side=3,adj=-.2,line=5,cex=1.5)
	Experiment2();mtext("(B)",side=3,adj=-.2,line=5,cex=1.5)
	Experiment3();mtext("(C)",side=3,adj=-.2,line=5,cex=1.5)
	Experiment4();mtext("(D)",side=3,adj=-.2,line=5,cex=1.5)
	
	salts.treat1 <- c(
	  expression(paste("NH"[3],"Cl")),
	  expression("MgNO"[2]),
	  "KOAc",
	  "LiCl")
	
	salts.treat2 <- c(
	  "NaCl 30\u00B0C",
	  "NoSalt 23\u00B0C",
	  "NoSalt 30\u00B0C",
	  "LiCl 30\u00B0C")
	
	salts.treat3 <- c(
	  "NoSalt 23\u00B0C",
	  "NoSalt 30\u00B0C",
	  "NaCl 30\u00B0C",
	  "LiCl 30\u00B0C")
	
	#quartz()
	
	pdf(width=8, height=8, file="../Figures/Figure3.2.finalMay26.pdf")

	layout(mm.multi)
	par(xpd=TRUE)
	
	Experiment1(c(
	    expression(paste(italic("G. morbida   "))),
	    expression(paste(italic("Asprigillus"), " sp."))
	  ), salts.treat1)
	mtext("(A)",side=3,adj=-.2,line=5,cex=1.5)

	Experiment2(c(
	  expression(paste(italic("G. morbida   "))),
	  expression(paste(italic("Asprigillus"), " sp.")),
	  expression(paste(italic("Kretzshmaria"), " sp."))
	  ), salts.treat1)
	mtext("(B)",side=3,adj=-.2,line=5,cex=1.5)

	Experiment3(expression(paste(italic("G. morbida"))),salts.treat2)
	mtext("(C)",side=3,adj=-.2,line=5,cex=1.5)
	segments(0,1,.2,1)
	
	Experiment4(c(
	  expression(paste(italic("G. morbida"))),
	  expression(paste(italic("Trichoderma"), " sp. 1")),
	  expression(paste(italic("Trichoderma"), " sp. 2")),
	  expression(paste(italic("Clonostachys")," sp.")),
	  expression(paste(italic("Aspergillus"), " sp."))),salts.treat3)
	mtext("(D)",side=3,adj=-.2,line=5,cex=1.5)

dev.off()
