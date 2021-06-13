#library(gridExtra)
#library(grid)
#library(cowplot)
library(lattice)
library(tidyverse)
library(ncdf4)
library(ncdf4.helpers)
library(doParallel)
library(foreach)

#library(PCICt)
#library(readr)
library(dplyr)
library(viridis)
library(weathermetrics)
library(maps)
#library(maptools)

library(raster)
library(foreach)
library(doParallel)


convert.degreesE <- function(x) (-1)*(360*(x %/% 180) - x)

odds.to.rate <- function(x) exp(x) %>% (function (y) 1/((1/y) +1))(.)

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

emc.map.MACA.month <- function (file_min_temp, file_max_temp, file_min_rh, file_max_rh, year, month) {
	
	#read in first file to get map extent
	#c_out<- nc_open(file_min_temp)
	#longitude <- ncvar_get(c_out, varid = "lon")
	#latitude <- ncvar_get(c_out, varid = "lat")
	
	tmp_raster <- brick(file_min_temp, varname="air_temperature")
	
	
	#set up range of date objects
	days.in.a.month <- apply(expand.grid(1:31,month,year)[,c(3,2,1)], 1, paste, collapse="-") %>% as.Date %>% na.omit %>% format("%Y-%m-%d")
	
	# calculate temperature averages for time period from min max
	# convert to C
	ta_ <- map.variable.avg.MACA(file_min_temp, file_max_temp, "air_temperature", days.in.a.month) - 273.15
	
	# calculate r.h. averages for time period from min max
	# convert to decimal
	rha_ <- map.variable.avg.MACA(file_min_rh, file_max_rh, "relative_humidity", days.in.a.month) / 100
	
	# Use Simpson 1973 with parameters from Glass and Zelinka 2010
   	W <- 349 + 1.29*ta_ + 0.0135*ta_^2
	K <- 0.805 + 0.000736*ta_ - 0.00000273*ta_^2
	K1 <- 6.27 - 0.00938*ta_ - 0.000303*ta_^2
	K2 <- 1.91 + 0.0407*ta_ - 0.000293*ta_^2
	EMC <- (18/W)*( (K*rha_/(1-K*rha_)) + (K1*K*rha_ + 2*K1*K2*K^2*rha_^2) / (1 + K1*K*rha_ + K1*K2*K^2 * rha_^2) )

	data.frame(lon=as.vector(longitude), lat=as.vector(latitude), EMC=as.vector(EMC))

}

emc.map.MACA.month.raster <- function (file_min_temp, file_max_temp, file_min_rh, file_max_rh, year, month) {
	
	#set up range of date objects
	days.in.a.month <- apply(expand.grid(1:31,month,year)[,c(3,2,1)], 1, paste, collapse="-") %>% as.Date %>% na.omit %>% format("%Y-%m-%d")
	
	# calculate temperature averages for time period from min max
	# convert to C
	ta_ <- map.variable.avg.MACA.raster(file_min_temp, file_max_temp, "air_temperature", days.in.a.month) - 273.15
	
	# calculate r.h. averages for time period from min max
	# convert to decimal
	rha_ <- map.variable.avg.MACA.raster(file_min_rh, file_max_rh, "relative_humidity", days.in.a.month) / 100
	
	# Use Simpson 1973 with parameters from Glass and Zelinka 2010
   	W <- calc(ta_, fun=function(x) 349 + 1.29*x + 0.0135*x^2)
	K <- calc(ta_, fun=function(x) 0.805 + 0.000736*x - 0.00000273*x^2)
	K1 <- calc(ta_, fun=function(x) 6.27 - 0.00938*x - 0.000303*x^2)
	K2 <- calc(ta_, fun=function(x) 1.91 + 0.0407*x - 0.000293*x^2)
	EMC <- overlay(W, rha_, K, K1, K2, fun=function(x,y,z,z1,z2) (18/x)*( (z*y/(1-z*y)) + (z1*z*y + 2*z1*z2*z^2*y^2) / (1 + z1*z*y + z1*z2*z^2 * y^2) ))
	
	return(EMC)
}

Gm.survival.month.raster <- function  (model, file_min_temp, file_max_temp, file_min_rh, file_max_rh, year, month) {

	EMC.month <- emc.map.MACA.month.raster(file_min_temp, file_max_temp, file_min_rh, file_max_rh, year, month)
	Gm.surv <- calc(EMC.month, fun = function(x) predict(model, newdata=data.frame(mc=x)) %>% odds.to.rate)
	
}

Gm.survival.year.raster <- function  (model, file_min_temp, file_max_temp, file_min_rh, file_max_rh, year, months=c(5:9)) {

	stacked <- foreach(i = 1:length(months), .combine=function(x,y) x*y, .packages="raster") %dopar%
		Gm.survival.month.raster (model, file_min_temp, file_max_temp, file_min_rh, file_max_rh, year, months[i])
	
	stacked
	
}

Gm.survival.avg <- function  (model, file_min_temp, file_max_temp, file_min_rh, file_max_rh, years, months=c(5:9)) {

	stacked <- foreach(i = 1:length(years), .combine=stack, .packages="raster") %dopar%
		Gm.survival.year.raster (model, file_min_temp, file_max_temp, file_min_rh, file_max_rh, years[i], months) 

	mean(stacked)

}

# get the daily maximums and minimums for a timespan, compute average of the two, and take the average of that

map.variable.avg.MACA <- function (file_min, file_max, varname, fechas) {
	
	# open
	c_out_min<-nc_open(file_min)
	c_out_max<-nc_open(file_max)
	
	# get time values
	r_time <- nc.get.time.series(c_out_min, v = varname, time.dim.name = "time")
	
	# get time index values
	t_indices <- which(	format(r_time, "%Y-%m-%d") %in% fechas )

	# store daily minimums and maximums and put them in a 4D array
	var_min <- nc.get.var.subset.by.axes(c_out_min, varname, axis.indices = list(T = t_indices))
	var_max <- nc.get.var.subset.by.axes(c_out_max, varname, axis.indices = list(T = t_indices))

	#return average of averages between min and max for each datapoint
	return ( apply((var_min + var_max) / 2, c(1,2), mean) )
}

map.variable.avg.MACA.raster <- function (file_min, file_max, varname, fechas) {
	
	# open as raster brick
	min_raster <- brick(file_min, varname=varname)
	max_raster <- brick(file_max, varname=varname)
	
	# get time index values
	dates <- names(min_raster) %>% substring(2) %>% gsub("\\.","\\-",.) %>% as.Date() %>% format("%Y-%m-%d")
	t_indices <- which(dates %in% fechas)
	
	#return average of averages between min and max for each datapoint
	
	mean_min_subset <- mean(subset(min_raster, t_indices))
	mean_max_subset <- mean(subset(max_raster, t_indices))
	avg_subset <- mean(mean_min_subset, mean_max_subset)
	
	return(avg_subset)
}

# find the max expected EMC during a set of months for a given year

emc.max.map.MACA <- function (file_min_temp, file_max_temp, file_min_rh, file_max_rh, year, months=c(5:9)) {
	
	set.seed(579383)

	registerDoParallel(8)
	
	# %dopar% implemented with foreach implements each loop call as an independent function call
	# and then parallelizes all the function calls across the number of processers set above with
	# registerDoParallel(). First, we need to set up a dataframe 
	
	map.emc.months <- data.frame(year=NULL, month=NULL, lon=NULL, lat=NULL, EMC=NULL)
	i <- months[1]
	map.emc.months <- foreach (Try=months, .combine=cbind) %dopar% {
		this.month <- rbind(year=year, month=months[i], emc.map.MACA.month(file_min_temp, file_max_temp, file_min_rh, file_max_rh, year, months[i]))
		i<-1+1
		cbind(this.month, Try=Try)
	}
	
	df<-map.emc.months$EMC %>% apply(1, max) %>% cbind(map.emc.months[which(map.emc.months==months[1]), c("lon", "lat")], .)
	
	names(df)[3]<-"EMC"
	df
	
}

plot.survival.map <- function (dat_survival, nm = "Survival (%)") {
	dat_survival$lon = convert.degreesE(dat_survival$lon)
	ggplot(dat_survival)+geom_point(aes(x=lon,y=lat,color=Survival),size=.1)+
	    scale_color_viridis(name = "Survival rate",
        	option="viridis",
        	discrete=F,
        	na.value="white")+
		theme(plot.margin=unit(c(0,0,0,0),"cm"))+
		xlim(min(dat_survival$lon),max(dat_survival$lon)) +
		ylim(min(dat_survival$lat),max(dat_survival$lat)) +
		borders("state", colour="white",size=1, )+
		theme_void()+
		coord_map()
}

