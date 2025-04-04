

library("ggplot2")
library("sf")
library("rworldmap")
library('cowplot')
library("viridis")


# --------------------------------- # temperature

gbrmax24 <- st_read("data/noaa_sst/GBR/gbr.max.2024.shp")
gbrmax22 <- st_read("data/noaa_sst/GBR/gbr.max.2022.shp")
gbrmax20 <- st_read("data/noaa_sst/GBR/gbr.max.2020.shp")
gbrmax17 <- st_read("data/noaa_sst/GBR/gbr.max.2017.shp")
gbrmax16 <- st_read("data/noaa_sst/GBR/gbr.max.2016.shp")
gbrmax02 <- st_read("data/noaa_sst/GBR/gbr.max.2002.shp")
gbrmax98 <- st_read("data/noaa_sst/GBR/gbr.max.1998.shp")

# --------------------------------- #  shapefiles 

# gbr 
load("data/GBR_map/reefs.RData")
summary(reefs)
gbr.sf<-st_as_sf(reefs)
head(gbr.sf)

# world
rmap <- getMap(resolution="high") # SpatialPolygonsDataFrame
rmap.sf <- st_as_sf(rmap)

# qld
load("data/GBR_map/qld.RData")
summary(qld)
qld.sf<-st_as_sf(qld)
head(qld.sf)

#bound <- st_read("GBR_map/AllenBoundary/boundary.geojson")
#st_layers("GBR_map/AllenBoundary/boundary.geojson")

#load("GBR_map/nrm.RData")
load("data/GBR_map/whagbr.RData")
summary(whagbr)
bound2<-st_as_sf(whagbr)
head(bound2)

towns <- data.frame(name=c("Cooktown", "Cairns","Townsville", "Mackay", "Gladstone"), lat=c(-15.472405, -16.921848, -19.277811,-21.149498, -23.859962), long=c(145.250033, 145.757270, 146.769968,149.186270, 151.256776))

# --------------------------------- # plot
  
#colz <- c("darkblue","blue", "aquamarine", "yellow", "orange", "red", "darkred", "black")
#colbreaks <- c(0,     1,        3,            5,        10,        13,     18,       20)

gbrmax24$dhw[gbrmax24$dhw>15] <- 15
gbrmax22$dhw[gbrmax22$dhw>15] <- 15
gbrmax20$dhw[gbrmax20$dhw>15] <- 15
gbrmax17$dhw[gbrmax17$dhw>15] <- 15
gbrmax16$dhw[gbrmax16$dhw>15] <- 15
gbrmax02$dhw[gbrmax02$dhw>15] <- 15
gbrmax98$dhw[gbrmax98$dhw>15] <- 15

colz <- c("darkblue","blue", "aquamarine", "yellow", "orange", "red", "darkred")
colbreaks <- c(0,     1,        2.5,            3.5,        6,        8,     15)


colvals <- colbreaks / max(colbreaks)

xlim = c(142, 153)
ylim = c(-26, -10)

max(gbrmax24$dhw)
max(gbrmax16$dhw)

dhw.lims <- c(0,15)

st_crs(gbrmax24) <- st_crs(gbr.sf)
st_crs(gbrmax22) <- st_crs(gbr.sf)
st_crs(gbrmax20) <- st_crs(gbr.sf)
st_crs(gbrmax17) <- st_crs(gbr.sf)
st_crs(gbrmax16) <- st_crs(gbr.sf)
st_crs(gbrmax02) <- st_crs(gbr.sf)
st_crs(gbrmax98) <- st_crs(gbr.sf)

map24 <- ggplot()+
geom_sf(data=gbrmax24,  aes(col=dhw), size=0.05)+
geom_sf(data=gbr.sf,  lwd=0.1, col=alpha("grey40",0.8), fill=NA)+
geom_sf(data=qld.sf, col="grey50", fill="grey95", size=0.01)+ 
 scale_colour_gradientn(colours = colz, values=colvals, limits=dhw.lims)+ # breaks=0
 coord_sf(ylim=c(ylim[1]+2.5, ylim[2]-0.3), xlim=c(xlim[1], xlim[2]))+
 ggtitle("2024")+
 theme_classic()+theme(axis.title=element_blank(), legend.title=element_text(size=6, face="bold"), legend.position=c(0.85, 0.72), legend.key.width=unit(2, "mm"), plot.title=element_text(size=9, hjust=0.1), axis.line=element_blank(), axis.ticks.x=element_line(color=NA), axis.ticks.y=element_blank(), plot.background=element_blank(), panel.background=element_blank(), axis.text.y=element_blank() , axis.text.x=element_text(color=NA) )
map24

map24b <- ggplot()+
geom_sf(data=gbrmax24,  aes(col=dhw), size=0.05)+
geom_sf(data=gbr.sf,  lwd=0.1, col=alpha("grey40",0.8), fill=NA)+
geom_sf(data=qld.sf, col="grey50", fill=NA, size=0.01)+ 
 scale_colour_gradientn(colours = colz, values=colvals, limits=dhw.lims)+ # breaks=0
 coord_sf(ylim=c(ylim[1]+2.5, ylim[2]-0.3), xlim=c(xlim[1], xlim[2]))+
 ggtitle("2024")+
 theme_classic()+theme(axis.title=element_blank(), legend.title=element_text(size=6, face="bold"), legend.position=c(0.85, 0.72), legend.key.width=unit(2, "mm"), plot.title=element_text(size=9, hjust=0.1), axis.line=element_blank(), axis.ticks.x=element_line(color=NA), axis.ticks.y=element_blank(), plot.background=element_blank(), panel.background=element_blank(), axis.text.y=element_blank() , axis.text.x=element_text(color=NA) )
map24b

map22 <- ggplot()+
#geom_raster(data = sst.max, aes(x =longitude, y = latitude, fill = dhw))+
geom_sf(data=gbrmax22,  aes(col=dhw), size=0.05)+
#geom_sf(data=bound2)+
geom_sf(data=gbr.sf,  lwd=0.1, col=alpha("grey40",0.5), fill=NA)+
geom_sf(data=qld.sf, col="grey50", fill=NA, size=0.01)+ # grey90
#geom_point(data=towns, aes(long, lat), shape=15, size=1)+
#geom_text(data=towns, aes(long-0.25, lat, label=name), hjust=1, size=1.5, fontface="bold")+
#geom_sf(data=bound2)+
# scale_colour_distiller(palette="Spectral")+
 scale_colour_gradientn(colours = colz, values=colvals, limits=dhw.lims)+ # breaks=0
 coord_sf(ylim=c(ylim[1]+2.5, ylim[2]-0.3), xlim=c(xlim[1], xlim[2]))+
  ggtitle("2022")+
 theme_classic()+theme(axis.title=element_blank(), legend.title=element_text(size=6, face="bold"), legend.position=c(0.85, 0.72), legend.key.width=unit(2, "mm"), plot.title=element_text(size=9, hjust=0.1), axis.line=element_blank(), axis.ticks.x=element_line(color=NA), axis.ticks.y=element_blank(), plot.background=element_blank(), panel.background=element_blank(), axis.text.y=element_blank() , axis.text.x=element_text(color=NA) )

map20 <- ggplot()+
#geom_raster(data = sst.max, aes(x =longitude, y = latitude, fill = dhw))+
geom_sf(data=gbrmax20,  aes(col=dhw), size=0.05)+
#geom_sf(data=bound2)+
geom_sf(data=gbr.sf,  lwd=0.1, col=alpha("grey40",0.5), fill=NA)+
geom_sf(data=qld.sf, col="grey50", fill=NA, size=0.01)+ # grey90
#geom_point(data=towns, aes(long, lat), shape=15, size=1)+
#geom_text(data=towns, aes(long-0.25, lat, label=name), hjust=1, size=1.5, fontface="bold")+
#geom_sf(data=bound2)+
# scale_colour_distiller(palette="Spectral")+
 scale_colour_gradientn(colours = colz, values=colvals, limits=dhw.lims)+ # breaks=0
 coord_sf(ylim=c(ylim[1]+2.5, ylim[2]-0.3), xlim=c(xlim[1], xlim[2]))+
  ggtitle("2020")+
 theme_classic()+theme(axis.title=element_blank(), legend.title=element_text(size=6, face="bold"), legend.position=c(0.85, 0.72), legend.key.width=unit(2, "mm"), plot.title=element_text(size=9, hjust=0.1), axis.line=element_blank(), axis.ticks.x=element_line(color=NA), axis.ticks.y=element_blank(), plot.background=element_blank(), panel.background=element_blank(), axis.text.y=element_blank() , axis.text.x=element_text(color=NA) )


map17 <- ggplot()+
#geom_raster(data = sst.max, aes(x =longitude, y = latitude, fill = dhw))+
geom_sf(data=gbrmax17,  aes(col=dhw), size=0.05)+
#geom_sf(data=bound2)+
geom_sf(data=gbr.sf,  lwd=0.1, col=alpha("grey40",0.5), fill=NA)+ # azure # grey60
geom_sf(data=qld.sf, col="grey50", fill=NA, size=0.01)+ # grey90
#geom_point(data=towns, aes(long, lat), shape=15, size=1)+
#geom_text(data=towns, aes(long-0.25, lat, label=name), hjust=1, size=1.5, fontface="bold")+
#geom_sf(data=bound2)+
# scale_colour_distiller(palette="Spectral")+
 scale_colour_gradientn(colours = colz, values=colvals, limits=dhw.lims)+ # breaks=0
 coord_sf(ylim=c(ylim[1]+2.5, ylim[2]-0.3), xlim=c(xlim[1], xlim[2]))+
  ggtitle("2017")+
 theme_classic()+theme(axis.title=element_blank(), legend.title=element_text(size=6, face="bold"), legend.position=c(0.85, 0.72), legend.key.width=unit(2, "mm"), plot.title=element_text(size=9, hjust=0.1), axis.line=element_blank(), axis.ticks.x=element_line(color=NA), axis.ticks.y=element_blank(), plot.background=element_blank(), panel.background=element_blank(), axis.text.y=element_blank() , axis.text.x=element_text(color=NA) )



map16 <- ggplot()+
#geom_raster(data = sst.max, aes(x =longitude, y = latitude, fill = dhw))+
geom_sf(data=gbrmax16,  aes(col=dhw), size=0.05)+
#geom_sf(data=bound2)+
geom_sf(data=gbr.sf,  lwd=0.1, col=alpha("grey40",0.5), fill=NA)+
geom_sf(data=qld.sf, col="grey50", fill=NA, size=0.01)+ # grey90
#geom_point(data=towns, aes(long, lat), shape=15, size=1)+
#geom_text(data=towns, aes(long-0.25, lat, label=name), hjust=1, size=1.5, fontface="bold")+
#geom_sf(data=bound2)+
# scale_colour_distiller(palette="Spectral")+
 scale_colour_gradientn(colours = colz, values=colvals, limits=dhw.lims)+ # breaks=0
 coord_sf(ylim=c(ylim[1]+2.5, ylim[2]-0.3), xlim=c(xlim[1], xlim[2]))+
  ggtitle("2016")+
  scale_x_continuous(breaks = seq(142, 151, by = 3)) +
 theme_classic()+theme(axis.text=element_text(size=6),axis.title=element_blank(), legend.title=element_blank(),  legend.key.width=unit(2, "mm"),axis.line=element_blank(),   plot.title=element_text(size=9, hjust=0.1), legend.position="left")
 


map02 <- ggplot()+
#geom_raster(data = sst.max, aes(x =longitude, y = latitude, fill = dhw))+
geom_sf(data=gbrmax02,  aes(col=dhw), size=0.05)+
#geom_sf(data=bound2)+
geom_sf(data=gbr.sf,  lwd=0.1, col=alpha("grey40",0.5), fill=NA)+
geom_sf(data=qld.sf, col="grey50", fill=NA, size=0.01)+ # grey90
#geom_point(data=towns, aes(long, lat), shape=15, size=1)+
#geom_text(data=towns, aes(long-0.25, lat, label=name), hjust=1, size=1.5, fontface="bold")+
#geom_sf(data=bound2)+
# scale_colour_distiller(palette="Spectral")+
 scale_colour_gradientn(colours = colz, values=colvals, limits=dhw.lims)+ # breaks=0
 coord_sf(ylim=c(ylim[1]+2.5, ylim[2]-0.3), xlim=c(xlim[1], xlim[2]))+
  ggtitle("2002")+
 theme_classic()+theme(axis.title=element_blank(), legend.title=element_text(size=6, face="bold"), legend.position=c(0.85, 0.72), legend.key.width=unit(2, "mm"), plot.title=element_text(size=9, hjust=0.1), axis.line=element_blank(), axis.ticks.x=element_line(color=NA), axis.ticks.y=element_blank(), plot.background=element_blank(), panel.background=element_blank(), axis.text.y=element_blank() , axis.text.x=element_text(color=NA) )
 
 

map98 <- ggplot()+
#geom_raster(data = sst.max, aes(x =longitude, y = latitude, fill = dhw))+
geom_sf(data=gbrmax98,  aes(col=dhw), size=0.05)+
#geom_sf(data=bound2)+
geom_sf(data=gbr.sf,  lwd=0.1, col=alpha("grey40",0.5), fill=NA)+
geom_sf(data=qld.sf, col="grey50", fill="white", size=0.01)+ # grey90
#geom_point(data=towns, aes(long, lat), shape=15, size=1)+
#geom_text(data=towns, aes(long-0.25, lat, label=name), hjust=1, size=1.5, fontface="bold")+
#geom_sf(data=bound2)+
# scale_colour_distiller(palette="Spectral")+
 scale_colour_gradientn(colours = colz, values=colvals, limits=dhw.lims)+ # breaks=0
 coord_sf(ylim=c(ylim[1]+2.5, ylim[2]-0.3), xlim=c(xlim[1], xlim[2]))+
  ggtitle("1998")+
 theme_classic()+theme(axis.text=element_text(size=6),axis.title=element_blank(), legend.title=element_blank(),  legend.key.width=unit(2, "mm"),axis.line=element_blank(),   plot.title=element_text(size=9, hjust=0.1), legend.position="left")


legplot <- ggplot()+
geom_sf(data=gbrmax16,  aes(col=dhw), size=0.05)+scale_colour_gradientn(colours = colz, values=colvals, limits=c(0,20))+theme(legend.title=element_text(size=6, face="bold"), legend.key.width=unit(1, "mm"), legend.key.height=unit(4, "mm"))

mapplot <- plot_grid(map16+guides(col="none"),NULL, map17+guides(col="none"),NULL,map20+guides(col="none"),NULL, map22+guides(col="none"), NULL,map24b, nrow=1, rel_widths=c(1.1,-0.65, 1,-0.65, 1,-0.65,1, -0.65,1))
mapplot


################################################################# add sites

dat <- read.csv("data/bleaching.csv")

reefs <- unique(dat[,c("Region","Reef", "GPS.S", "GPS.E")])
reefs$endlon <- reefs$GPS.E + 5
reefs$endlat <- - reefs$GPS.S

reefs2 <- unique(dat[,c("Region", "Reef")])
reefs2[,c("lat", "lon")] <- reefs[match(reefs2$Reef, reefs$Reef), c("GPS.S", "GPS.E")]
reefs2

reefs2$endlon <- reefs2$lon + 3
reefs2$endlat <- - reefs2$lat


reefs2$endlat2 <- reefs2$endlat
reefs2$endlat2[reefs2$Reef=="Monsoon"] <- reefs2$endlat2[reefs2$Reef=="Monsoon"] + 1.3
reefs2$endlat2[reefs2$Reef=="Three Reefs"] <- reefs2$endlat2[reefs2$Reef=="Three Reefs"] + 0.9
reefs2$endlat2[reefs2$Reef=="11-049"] <- reefs2$endlat2[reefs2$Reef=="11-049"] + 1

reefs2$endlat2[reefs2$Reef=="13-124"] <- reefs2$endlat2[reefs2$Reef=="13-124"] + 2
reefs2$endlat2[reefs2$Reef=="Corbett"] <- reefs2$endlat2[reefs2$Reef=="Corbett"] + 1.5
reefs2$endlat2[reefs2$Reef=="Davie"] <- reefs2$endlat2[reefs2$Reef=="Davie"] + 1

reefs2$endlat2[reefs2$Reef=="Cormrant"] <- reefs2$endlat2[reefs2$Reef=="Cormrant"] + 0.9
reefs2$endlat2[reefs2$Reef=="Lizard"] <- reefs2$endlat2[reefs2$Reef=="Lizard"] +0.4
reefs2$endlat2[reefs2$Reef=="Ribbon 8 "] <- reefs2$endlat2[reefs2$Reef=="Ribbon 8 "] - 0.3
reefs2$endlat2[reefs2$Reef=="North Direction"] <- reefs2$endlat2[reefs2$Reef=="North Direction"] -0

reefs2$endlat2[reefs2$Reef=="Elford"] <- reefs2$endlat2[reefs2$Reef=="Elford"] -0.6
reefs2$endlat2[reefs2$Reef=="Moore"] <- reefs2$endlat2[reefs2$Reef=="Moore"] - 1.1
reefs2$endlat2[reefs2$Reef=="Thetford"] <- reefs2$endlat2[reefs2$Reef=="Thetford"] -0.1
reefs2$endlat2[reefs2$Reef=="Milln"] <- reefs2$endlat2[reefs2$Reef=="Milln"] + 0.4

reefs2$endlat2[reefs2$Reef=="Broadhurst"] <- reefs2$endlat2[reefs2$Reef=="Broadhurst"] - 0.7
reefs2$endlat2[reefs2$Reef=="Davies"] <- reefs2$endlat2[reefs2$Reef=="Davies"] - 0.3
reefs2$endlat2[reefs2$Reef=="Chicken"] <- reefs2$endlat2[reefs2$Reef=="Chicken"] + 0.1

reefs2$endlat2[reefs2$Reef=="Chavel"] <- reefs2$endlat2[reefs2$Reef=="Chavel"] - 0.7
reefs2$endlat2[reefs2$Reef=="Goble"] <- reefs2$endlat2[reefs2$Reef=="Goble"] - 0.2
reefs2$endlat2[reefs2$Reef=="Bugatti"] <- reefs2$endlat2[reefs2$Reef=="Bugatti"] - 0.2

reefs2$endlat2[reefs2$Reef=="Wistari"] <- reefs2$endlat2[reefs2$Reef=="Wistari"] - 0.5
reefs2$endlat2[reefs2$Reef=="Heron"] <- reefs2$endlat2[reefs2$Reef=="Heron"] - 0
reefs2$endlat2[reefs2$Reef=="Wilson"] <- reefs2$endlat2[reefs2$Reef=="Wilson"] +0.5

head(reefs2)
reefs2$Reef[reefs2$Reef=="Cormrant"] <- "Cormorant"

 map24c <- map24+guides(col="none")+
# geom_sf(data=gbr.sf,  lwd=0.001, col="grey40", fill=NA)+
 geom_segment(data=reefs2, aes(x=lon, xend=endlon, y= -lat, yend= endlat2), size=0.05)+
 geom_text(data=reefs2, aes(x=endlon+0.01, y= endlat2, label=Reef), hjust=0, size=1.8)+
 scale_x_continuous(breaks=c(142, 146, 150, 154))+
 coord_sf(ylim=c(ylim[1]+1.75, ylim[2]+0.3), xlim=c(xlim[1]+0.2, xlim[2]+4))+ggtitle("")+ theme_classic()+theme(axis.text=element_text(size=6),axis.title=element_blank(), legend.title=element_blank(),  legend.key.width=unit(2, "mm"),axis.line=element_blank(),   plot.title=element_text(size=9, hjust=0.1), legend.position="left", panel.background=element_blank())
 map24c 
 
 map24d <- plot_grid( map24c ,NULL, plot_grid(NULL, get_legend(legplot),NULL, ncol=1, rel_heights=c(0.1, 1, 1, 0.5)),NULL, nrow=1, rel_widths=c(1, -0.2, 0.1, 0.1)) 
 map24d
 
 
 # coord_sf(ylim=c(ylim[1]+2.5, ylim[2]-0.3), xlim=c(xlim[1], xlim[2]))+ others...

mapnames <- ggplot()+
geom_sf(data=gbr.sf,  lwd=0.1, col=alpha("grey40",0.5), fill=NA)+
geom_sf(data=qld.sf, col="grey50", fill="lemonchiffon", size=0.01)+ # grey90
 geom_segment(data=reefs2, aes(x=lon, xend=endlon, y= -lat, yend= endlat2), size=0.05)+
 geom_text(data=reefs2, aes(x=endlon+0.01, y= endlat2, label=Reef), hjust=0, size=1.8)+
 coord_sf(ylim=c(ylim[1]+1.75, ylim[2]+0.3), xlim=c(xlim[1]+0.2, xlim[2]+4))+
 theme_classic()+theme(axis.text=element_text(size=6),axis.title=element_blank(), legend.title=element_blank(),  legend.key.width=unit(2, "mm"),axis.line=element_blank(),   plot.title=element_text(size=9, hjust=0.1), legend.position="left", panel.background=element_rect(fill="lightcyan"))
mapnames


################################################################# above N


n <- 6
gbrmax16$above <- ifelse(gbrmax16$dhw > n, "y", "n")
gbrmax17$above <- ifelse(gbrmax17$dhw > n, "y", "n")
gbrmax20$above <- ifelse(gbrmax20$dhw > n, "y", "n")
gbrmax22$above <- ifelse(gbrmax22$dhw > n, "y", "n")
gbrmax24$above <- ifelse(gbrmax24$dhw > n, "y", "n")

sixcol <- "slategrey"

map24e <- ggplot()+
#geom_raster(data = sst.max, aes(x =longitude, y = latitude, fill = dhw))+
geom_sf(data=gbrmax24,  aes(col=above), size=0.05)+
#geom_sf(data=bound2)+
geom_sf(data=gbr.sf,  lwd=0.1, col=alpha("grey40",0.8), fill=NA)+
geom_sf(data=qld.sf, col="grey50", fill=NA, size=0.01)+ 
#geom_point(data=towns, aes(long, lat), shape=15, size=1)+
#geom_text(data=towns, aes(long-0.25, lat, label=name), hjust=1, size=1.5, fontface="bold")+
#geom_sf(data=bound2)+
 scale_colour_manual(values=c("white", sixcol))+
 coord_sf(ylim=c(ylim[1]+2.5, ylim[2]-0.3), xlim=c(xlim[1], xlim[2]))+
 ggtitle("2024")+
 theme_classic()+theme(axis.title=element_blank(), legend.title=element_text(size=6, face="bold"), legend.position=c(0.85, 0.72), legend.key.width=unit(2, "mm"), plot.title=element_text(size=9, hjust=0.1), axis.line=element_blank(), axis.ticks.x=element_line(color=NA), axis.ticks.y=element_blank(), plot.background=element_blank(), panel.background=element_blank(), axis.text.y=element_blank() , axis.text.x=element_text(color=NA) )
map24e



map22a <- ggplot()+
#geom_raster(data = sst.max, aes(x =longitude, y = latitude, fill = dhw))+
geom_sf(data=gbrmax22,  aes(col=above), size=0.05)+
#geom_sf(data=bound2)+
geom_sf(data=gbr.sf,  lwd=0.1, col=alpha("grey40",0.5), fill=NA)+
geom_sf(data=qld.sf, col="grey50", fill=NA, size=0.01)+ # grey90
#geom_point(data=towns, aes(long, lat), shape=15, size=1)+
#geom_text(data=towns, aes(long-0.25, lat, label=name), hjust=1, size=1.5, fontface="bold")+
#geom_sf(data=bound2)+
# scale_colour_distiller(palette="Spectral")+
 scale_colour_manual(values=c("white",sixcol))+
 coord_sf(ylim=c(ylim[1]+2.5, ylim[2]-0.3), xlim=c(xlim[1], xlim[2]))+
  ggtitle("2022")+
 theme_classic()+theme(axis.title=element_blank(), legend.title=element_text(size=6, face="bold"), legend.position=c(0.85, 0.72), legend.key.width=unit(2, "mm"), plot.title=element_text(size=9, hjust=0.1), axis.line=element_blank(), axis.ticks.x=element_line(color=NA), axis.ticks.y=element_blank(), plot.background=element_blank(), panel.background=element_blank(), axis.text.y=element_blank() , axis.text.x=element_text(color=NA) )

map20a <- ggplot()+
#geom_raster(data = sst.max, aes(x =longitude, y = latitude, fill = dhw))+
geom_sf(data=gbrmax20,  aes(col=above), size=0.05)+
#geom_sf(data=bound2)+
geom_sf(data=gbr.sf,  lwd=0.1, col=alpha("grey40",0.5), fill=NA)+
geom_sf(data=qld.sf, col="grey50", fill=NA, size=0.01)+ # grey90
#geom_point(data=towns, aes(long, lat), shape=15, size=1)+
#geom_text(data=towns, aes(long-0.25, lat, label=name), hjust=1, size=1.5, fontface="bold")+
#geom_sf(data=bound2)+
# scale_colour_distiller(palette="Spectral")+
 scale_colour_manual(values=c("white", sixcol))+
 coord_sf(ylim=c(ylim[1]+2.5, ylim[2]-0.3), xlim=c(xlim[1], xlim[2]))+
  ggtitle("2020")+
 theme_classic()+theme(axis.title=element_blank(), legend.title=element_text(size=6, face="bold"), legend.position=c(0.85, 0.72), legend.key.width=unit(2, "mm"), plot.title=element_text(size=9, hjust=0.1), axis.line=element_blank(), axis.ticks.x=element_line(color=NA), axis.ticks.y=element_blank(), plot.background=element_blank(), panel.background=element_blank(), axis.text.y=element_blank() , axis.text.x=element_text(color=NA) )


map17a <- ggplot()+
#geom_raster(data = sst.max, aes(x =longitude, y = latitude, fill = dhw))+
geom_sf(data=gbrmax17,  aes(col=above), size=0.05)+
#geom_sf(data=bound2)+
geom_sf(data=gbr.sf,  lwd=0.1, col=alpha("grey40",0.5), fill=NA)+ # azure # grey60
geom_sf(data=qld.sf, col="grey50", fill=NA, size=0.01)+ # grey90
#geom_point(data=towns, aes(long, lat), shape=15, size=1)+
#geom_text(data=towns, aes(long-0.25, lat, label=name), hjust=1, size=1.5, fontface="bold")+
#geom_sf(data=bound2)+
# scale_colour_distiller(palette="Spectral")+
 scale_colour_manual(values=c("white", sixcol))+
 coord_sf(ylim=c(ylim[1]+2.5, ylim[2]-0.3), xlim=c(xlim[1], xlim[2]))+
  ggtitle("2017")+
 theme_classic()+theme(axis.title=element_blank(), legend.title=element_text(size=6, face="bold"), legend.position=c(0.85, 0.72), legend.key.width=unit(2, "mm"), plot.title=element_text(size=9, hjust=0.1), axis.line=element_blank(), axis.ticks.x=element_line(color=NA), axis.ticks.y=element_blank(), plot.background=element_blank(), panel.background=element_blank(), axis.text.y=element_blank() , axis.text.x=element_text(color=NA) )



map16a <- ggplot()+
#geom_raster(data = sst.max, aes(x =longitude, y = latitude, fill = dhw))+
geom_sf(data=gbrmax16,  aes(col=above), size=0.05)+
#geom_sf(data=bound2)+
geom_sf(data=gbr.sf,  lwd=0.1, col=alpha("grey40",0.5), fill=NA)+
geom_sf(data=qld.sf, col="grey50", fill=NA, size=0.01)+ # grey90
#geom_point(data=towns, aes(long, lat), shape=15, size=1)+
#geom_text(data=towns, aes(long-0.25, lat, label=name), hjust=1, size=1.5, fontface="bold")+
#geom_sf(data=bound2)+
# scale_colour_distiller(palette="Spectral")+
 scale_colour_manual(values=c("white", sixcol))+
 coord_sf(ylim=c(ylim[1]+2.5, ylim[2]-0.3), xlim=c(xlim[1], xlim[2]))+
  ggtitle("2016")+
  scale_x_continuous(breaks = seq(142, 151, by = 3)) +
 theme_classic()+theme(axis.text=element_text(size=6),axis.title=element_blank(), legend.title=element_blank(),  legend.key.width=unit(2, "mm"),axis.line=element_blank(),   plot.title=element_text(size=9, hjust=0.1), legend.position="left")



aboveNplot2 <- plot_grid(map16a+guides(col="none"),NULL, map17a+guides(col="none"),NULL,map20a+guides(col="none"),NULL, map22a+guides(col="none"), NULL,map24e+guides(col="none"),NULL, nrow=1, rel_widths=c(1.1,-0.65, 1,-0.65, 1,-0.65,1, -0.65,1, -0.12))


figS3 <- plot_grid(mapplot, aboveNplot2, ncol=1, labels=c("A", "B"),label_size=9)
figS3

#   ggsave( "figs/supplement/figS3.jpg",figS3,height=5.5, width=6)





