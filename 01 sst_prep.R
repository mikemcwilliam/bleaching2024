

rm(list = ls())

library("ggplot2")
library("sf")

###################################################
# Prepare NOAA Coral Reef Watch Time-Series (ERDDAP) Data for analysis
# https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.html
# repeat for each year

sst <- read.csv("data/noaa_sst/dhw_5km_300e_098f_87bd.csv")
sst <- sst[-1,]
sst$time2 <- as.Date(sst$time, 1,10, format="%Y-%m-%d")
sst$month <- substr(sst$time, 6,7)
sst$year <- substr(sst$time, 1,4)
sst$latitude <- as.numeric(sst$latitude)
sst$longitude <- as.numeric(sst$longitude)
sst$dhw <- as.numeric(sst$CRW_DHW)
sst$sst <- as.numeric(sst$CRW_SST)
sst$gridID <- paste(sst$latitude, sst$longitude)
sst <- sst[!is.na(sst$sst),]
sst <- sst[!is.na(sst$dhw),]
head(sst)
nrow(sst)

# cut down to weekly observations, except Mar-Apr
times <- unique(sst$time2)
weeks <- times[seq(1, length(times), 7)] # every 7 days
mar_apr <- sst$time2[sst$month %in% c("03", "04")]
sst <- sst[sst$time2 %in% c(weeks,mar_apr),]
sst$weeks <- ifelse(sst$time2 %in% weeks, "y", "n")
head(sst)
nrow(sst)
unique(sst$time2)

###################################################
# maximum DHW

sst.max <- aggregate(dhw~latitude+longitude, sst, max)
head(sst.max)
ggplot()+geom_raster(data=sst.max, aes(longitude, latitude, fill=dhw))

###################################################
# crop to gbr area

library("concaveman")

aims <- st_read("data/GBR_map/TS_AIMS_NESP_Torres_Strait_Features_V1b_with_GBR_Features/TS_AIMS_NESP_Torres_Strait_Features_V1b_with_GBR_Features.shp")
aims2 <- aims[!aims$FEAT_NAME=="Mainland",]
aims2 <- aims2[!aims2$FEAT_NAME=="Island",]

sst.max.sf <- st_as_sf(sst.max, coords = c("longitude", "latitude")) # , as_points = FALSE
st_crs(aims2)<-st_crs(sst.max.sf)

conc <- data.frame(concaveman(st_coordinates(aims2)[,1:2], concavity=2))
pol2 = st_polygon( list( cbind( conc2$V1, conc2$V2) ) )
polc2 = st_sfc(pol2)
polc2 <- st_as_sf(polc2)
st_crs(polc2) <- st_crs(aims2)
cropped <- st_intersection(polc2, sst.max.sf)
head(cropped)

ggplot()+
geom_sf(data=cropped,  aes(col=dhw), size=0.05)+
#geom_sf(data=aims2)+
scale_colour_distiller(palette="Spectral")

# st_write(cropped, "data/noaa_sst/GBR/gbr.max.2024.shp")

###################################################
# crop to gbr reefs

# gbr 
load("data/GBR_map/reefs.RData")
summary(reefs)
gbr.sf<-st_as_sf(reefs)
head(gbr.sf)

sst.max.sf <- st_as_sf(sst.max, coords = c("longitude", "latitude")) # , as_points = FALSE
st_crs(sst.max.sf) <- st_crs(gbr.sf)
sf_use_s2(FALSE)
cropped2 <- st_intersection(gbr.sf,sst.max.sf)

ggplot()+
geom_sf(data=cropped2,  aes(col=dhw), size=0.05)+scale_colour_distiller(palette="Spectral")

# st_write(cropped, "data/noaa_sst/GBR_REEFS/gbr.max.2024.shp")

###################################################
# [[ Repeat for all years ]]

###################################################
# combine to one data frame

gbrmax24r <- st_read("data/noaa_sst/GBR_REEFS/gbr.max.2024.shp")
gbrmax22r <- st_read("data/noaa_sst/GBR_REEFS/gbr.max.2022.shp")
gbrmax20r <- st_read("data/noaa_sst/GBR_REEFS/gbr.max.2020.shp")
gbrmax17r <- st_read("data/noaa_sst/GBR_REEFS/gbr.max.2017.shp")
gbrmax16r <- st_read("data/noaa_sst/GBR_REEFS/gbr.max.2016.shp")
gbrmax02r <- st_read("data/noaa_sst/GBR_REEFS/gbr.max.2002.shp")
gbrmax98r <- st_read("data/noaa_sst/GBR_REEFS/gbr.max.1998.shp")

gbrmax24 <- st_read("data/noaa_sst/GBR/gbr.max.2024.shp")
gbrmax22 <- st_read("data/noaa_sst/GBR/gbr.max.2022.shp")
gbrmax20 <- st_read("data/noaa_sst/GBR/gbr.max.2020.shp")
gbrmax17 <- st_read("data/noaa_sst/GBR/gbr.max.2017.shp")
gbrmax16 <- st_read("data/noaa_sst/GBR/gbr.max.2016.shp")
gbrmax02 <- st_read("data/noaa_sst/GBR/gbr.max.2002.shp")
gbrmax98 <- st_read("data/noaa_sst/GBR/gbr.max.1998.shp")


df <- as.data.frame(rbind(
cbind(data.frame(gbrmax24), st_coordinates(gbrmax24, coord=c("lat", "long")), year=2024),
cbind(data.frame(gbrmax22), st_coordinates(gbrmax22, coord=c("lat", "long")), year=2022),
cbind(data.frame(gbrmax20), st_coordinates(gbrmax20, coord=c("lat", "long")), year=2020),
cbind(data.frame(gbrmax17), st_coordinates(gbrmax17, coord=c("lat", "long")), year=2017),
cbind(data.frame(gbrmax16), st_coordinates(gbrmax16, coord=c("lat", "long")), year=2016)[,-1],
cbind(data.frame(gbrmax02), st_coordinates(gbrmax02, coord=c("lat", "long")), year=2002),
cbind(data.frame(gbrmax98), st_coordinates(gbrmax98, coord=c("lat", "long")), year=1998)))

dfr <- as.data.frame(rbind(
cbind(data.frame(gbrmax24r), st_coordinates(gbrmax24r, coord=c("lat", "long")), year=2024),
cbind(data.frame(gbrmax22r), st_coordinates(gbrmax22r, coord=c("lat", "long")), year=2022)[,-1],
cbind(data.frame(gbrmax20r), st_coordinates(gbrmax20r, coord=c("lat", "long")), year=2020)[,-1],
cbind(data.frame(gbrmax17r), st_coordinates(gbrmax17r, coord=c("lat", "long")), year=2017)[,-1],
cbind(data.frame(gbrmax16r), st_coordinates(gbrmax16r, coord=c("lat", "long")), year=2016)[,-1],
cbind(data.frame(gbrmax02r), st_coordinates(gbrmax02r, coord=c("lat", "long")), year=2002)[,-1],
cbind(data.frame(gbrmax98r), st_coordinates(gbrmax98r, coord=c("lat", "long")), year=1998)[,-1]))

df$gridID <- paste(df$Y, df$X)
dfr$gridID <- paste(dfr$Y, dfr$X)

df <- subset(df,select=-c(geometry))
dfr <- subset(dfr,select=-c(geometry))

head(dfr)
head(df)

# write.csv(df, "data/noaa_sst/sst_gbr.csv")
# write.csv(dfr, "data/noaa_sst/sst_gbr_reefs.csv")

###################################################
# crop to surveyed gbr reefs (2016 and 2024)

# max DHW 2016 / 2024
sst.max16 <- st_read("data/noaa_sst/GBR/gbr.max.2016.shp") 
sst.max24 <- st_read("data/noaa_sst/GBR/gbr.max.2024.shp")
sst16 <- cbind(longitude=st_coordinates(sst.max16)[,1], latitude=st_coordinates(sst.max16)[,2], st_drop_geometry(sst.max16))
sst24 <- cbind(longitude=st_coordinates(sst.max24)[,1], latitude=st_coordinates(sst.max24)[,2], st_drop_geometry(sst.max24))

# transect coords
coord24 <- read.csv("data/info/sitecoordsX.csv") 
coord16 <- read.csv("data/info/sitecoords2016.csv") 
head(coord24)

#coord24[coord24$Transect_code=="MOO_B2_C3",]
#coord24[coord24$Transect_code=="HER_B2_C3",]
#coord24[coord24$Transect_code=="MOO_B3_S1",]


# coord corrections
coord24[coord24$Reef=="Thetford","GPS.S"] <- 16.798000
coord24[coord24$Reef=="Thetford","GPS.E"] <- 146.170892  # based on reef centre
coord24$GPS.E[coord24$GPS.S==23.83849] <- 151.8989 # wistari wrong? 
coord24$GPS.S[coord24$GPS.S==23.83849] <- 23.46908 
coord24$GPS.E[coord24$GPS.S==13.97648] <- 144.4389 # davie wrong
coord24$GPS.S[coord24$GPS.S==13.97648] <- 13.96604



# sites have coordID
coord24$coordID <- paste(-coord24$GPS.S, coord24$GPS.E)

# grids have gridID
sst16$gridID <- paste(sst16$latitude, sst16$longitude)
sst24$gridID <- paste(sst24$latitude, sst24$longitude)

# match temp grids to site coords

grids16 <- unique(sst16[,c("latitude", "longitude", "gridID")])
grids24 <- unique(sst24[,c("latitude", "longitude", "gridID")])

matches24 <- NULL
for(i in 1:nrow(coord24)){
#	i <- 1
lat <- - coord24[i, "GPS.S"]
lon <- coord24[i, "GPS.E"]
ord1 <- grids24[order(grids24$latitude, decreasing=F),]
latmatch <- ord1[findInterval(lat, ord1$latitude),"latitude"]
ord2 <- grids24[order(grids24$longitude, decreasing=F),]
lonmatch <- ord2[findInterval(lon, ord2$longitude),"longitude"]
matches24 <- rbind(matches24, data.frame(lat=latmatch, lon=lonmatch))
}

coord24$grid.lat <- matches24$lat
coord24$grid.lon <- matches24$lon
coord24$gridID <- paste(coord24$grid.lat, coord24$grid.lon)

ggplot(coord24, aes(grid.lat, -GPS.S))+geom_point()+geom_abline()
ggplot(coord24, aes(grid.lon, GPS.E))+geom_point()+geom_abline()

matches16 <- NULL
for(i in 1:nrow(coord16)){
	#i <- 1
lat <- coord16[i, "revised_lat"]
lon <- coord16[i, "revised_long"]
ord1 <- grids16[order(grids16$latitude, decreasing=F),]
#latmatch <- ord1[which.min(abs(ord1$latitude - lat)),"latitude"]
latmatch <- ord1[findInterval(lat, ord1$latitude),"latitude"]
ord2 <- grids16[order(grids16$longitude, decreasing=F),]
lonmatch <- ord2[findInterval(lon, ord2$longitude),"longitude"]
matches16 <- rbind(matches16, data.frame(lat=latmatch, lon=lonmatch))
}

coord16$grid.lat <- matches16$lat
coord16$grid.lon <- matches16$lon
coord16$gridID <- paste(coord16$grid.lat, coord16$grid.lon)

ggplot(coord16, aes(grid.lat, revised_lat))+geom_point()+geom_abline()
ggplot(coord16, aes(grid.lon, revised_long))+geom_point()+geom_abline()

#write.csv(coord16, "data/info/coordmatch16.csv")
#write.csv(coord24, "data/info/coordmatch24.csv")

###################################################
#  continous time series for surveyed reefs (2015 - 2024)
#  after download, processing using [L8 - L36]
#  then subset by gridID
#  for each region

#long <- read.csv("noaa_sst/2015grids/southern_dhw_5km_8b10_7fc1_f310.csv")
#long$time2 <- as.Date(long$time, 1,10, format="%Y-%m-%d")
#long$month <- substr(long$time, 6,7)
#long$year <- substr(long$time, 1,4)
#long$latitude <- as.numeric(long$latitude)
#long$longitude <- as.numeric(long$longitude)
#long$dhw <- as.numeric(long$CRW_DHW)
#long$sst <- as.numeric(long$CRW_SST)
#long$gridID <- paste(long$latitude, long$longitude)
#times2 <- unique(long$time2)
#weeks2 <- times2[seq(1, length(times2), 7)] # every 7 days
#long2 <- long[long$time2 %in% c(weeks2),]
#long3 <- long2[long2$gridID %in% tdf$gridID,]
# write.csv(long3, "noaa_sst/2015gridsMM/southern.csv")

# merge 
elford <- read.csv("data/noaa_sst/longtermgrids/elford.csv")
lizard <- read.csv("data/noaa_sst/longtermgrids/lizard.csv")
grenville <- read.csv("data/noaa_sst/longtermgrids/grenville.csv")
mackay <- read.csv("data/noaa_sst/longtermgrids/mackay.csv")
southern <- read.csv("data/noaa_sst/longtermgrids/southern.csv")
long.all <- rbind(elford, lizard, grenville, mackay, southern)
long.all$time2 <- as.Date(long.all$time2)

ggplot(long.all, aes(time2, dhw))+
geom_line(aes(group=gridID, col=latitude), size=0.35)

# write.csv(longtermgrids, "noaa_sst/long.all.csv")



# --------------------------------- # when was peak? 

tt <- read.csv("data/noaa_sst/longtermgrids.csv")
head(tt)

tt <- tt[tt$year==2024,]

tmax <- aggregate(dhw~gridID, tt, max)
tt$max <- tmax$dhw[match(tt$gridID, tmax$gridID)]
head(tt[tt$dhw==tt$max,])

tdf$Date2 <- as.Date(tdf$Date, format="%d/%m/%Y")
unique(tdf$Date2)


ggplot()+geom_line(data=tt, aes(time2, dhw,group=gridID))+theme(axis.text.x=element_text(angle=45, hjust=1))


