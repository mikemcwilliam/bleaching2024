
sites$Reg3 <- ifelse(sites$Region=="Cape Grenville", "I", ifelse(sites$Region=="Princess Charlotte Bay", "II", ifelse(sites$Region=="Lizard", "III", ifelse(sites$Region=="Cairns", "IV", ifelse(sites$Region=="Cape Bowling Green", "V", ifelse(sites$Region=="Hydrographers Passage", "VI", ifelse(sites$Region=="Capricorn Bunkers", "VII",NA )))))))

freqmap <- ggplot()+
#geom_raster(data = sst.max, aes(x =longitude, y = latitude, fill = dhw))+
#geom_sf(data=gbrmax24,  aes(col=dhw), size=0.05)+
#geom_sf(data=bound2)+
#geom_sf(data=gbr.sf,  lwd=0.1, col=alpha("grey40",0.8), fill=NA)+
geom_sf(data=qld.sf, col="grey50", fill="grey95", size=0.01)+ 
geom_point(data=freq.all, aes(X, Y, col=as.factor(aboveN)), size=0.01)+
#scale_colour_viridis(option="B", discrete=T)+
scale_color_manual(values=c("grey", "#fecc5c", "#fd8d3c", "#e31a1c", "#500000"))+
#scale_color_manual(values=rev(c("#b35806", "#f1a340", "#d8daeb", "#998ec3","#542788")))+
#geom_point(data=towns, aes(long, lat), shape=15, size=1)+
#geom_text(data=towns, aes(long-0.25, lat, label=name), hjust=1, size=1.5, fontface="bold")+
#geom_sf(data=bound2)+
# scale_colour_distiller(palette="Spectral")+
 #scale_colour_gradientn(colours = colz, values=colvals, limits=dhw.lims)+ # breaks=0
 coord_sf(ylim=c(ylim[1]+2.5, ylim[2]-0.3), xlim=c(xlim[1], xlim[2]))+
 labs(colour="N events\n(> 6 DHW)")+
 guides(colour=guide_legend(override.aes=list(size=3)))+
  scale_x_continuous(breaks=c(142, 146, 150, 154))+
 theme_classic()+theme(axis.text=element_text(size=6),axis.title=element_blank(),  legend.key.width=unit(1.5, "mm"),legend.key.height=unit(5, "mm"), axis.line=element_blank(),   plot.title=element_text(size=9, hjust=0.1),legend.position=c(0.85, 0.75),legend.title=element_text(size=6, face="bold") )
freqmap



reefs <- unique(tdf[,c("Region","Reef", "GPS.S", "GPS.E")])
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

reefs2$Reg3 <- sites$Reg3[match(reefs2$Region, sites$Region)]
 
circles <- aggregate(endlon~Reg3, reefs2, median) 
circles$endlat <- aggregate(endlat~Reg3, reefs2, median)$endlat
circles$endlat[circles$Reg3=="II"] <- circles$endlat[circles$Reg3=="II"]+1.5
circles$endlat[circles$Reg3=="I"] <- circles$endlat[circles$Reg3=="I"]+1
circles

 
lastmap <- ggplot()+
#geom_raster(data = sst.max, aes(x =longitude, y = latitude, fill = dhw))+
#geom_sf(data=gbrmax24,  aes(col=dhw), size=0.05)+
#geom_sf(data=bound2)+
#geom_sf(data=gbr.sf,  lwd=0.1, col=alpha("grey40",0.8), fill=NA)+
geom_sf(data=qld.sf, col="grey50", fill="grey95", size=0.01)+ 
#geom_point(data=freqs[freqs$N==6,], aes(X, Y, col=aboveN), size=0.5)+
scale_colour_viridis(direction=-1, discrete=T)+
geom_point(data=freq.all, aes(X, Y), col="grey", size=0.01)+
geom_point(data=time.all, aes(X, Y, col=as.factor(last)), size=0.01)+
 geom_segment(data=reefs2, aes(x=lon, xend=endlon, y= -lat, yend= endlat2), size=0.05)+
 geom_point(data=circles, aes(endlon, endlat), size=9, shape=21, fill="white", stroke=0.2, col="white")+ # use
  geom_text(data=circles, aes(endlon, endlat, label=Reg3), size=2.8)+ #use
 #geom_text(data=reefs2, aes(x=endlon+0.01, y= endlat2, label=Reef), hjust=0, size=1.8)+
#geom_point(data=towns, aes(long, lat), shape=15, size=1)+
#geom_text(data=towns, aes(long-0.25, lat, label=name), hjust=1, size=1.5, fontface="bold")+
#geom_sf(data=bound2)+
# scale_colour_distiller(palette="Spectral")+
 #scale_colour_gradientn(colours = colz, values=colvals, limits=dhw.lims)+ # breaks=0
 labs(colour="Years since\nlast event")+
  guides(colour=guide_legend(override.aes=list(size=3)))+
  scale_x_continuous(breaks=c(142, 146, 150))+
  coord_sf(ylim=c(ylim[1]+2.5, ylim[2]-0.3), xlim=c(xlim[1]+0.2, xlim[2]+4))+
 theme_classic()+theme(axis.text=element_text(size=6),axis.title=element_blank(),  legend.key.width=unit(1, "mm"),legend.key.height=unit(5, "mm"), axis.line=element_blank(),   plot.title=element_text(size=9, hjust=0.1),legend.position=c(0.85, 0.75),legend.title=element_text(size=6, face="bold") )
lastmap


#################################################################
# residuals..... 


sres$Reg3 <- ifelse(sres$Region=="Cape Grenville", "I", ifelse(sres$Region=="Princess Charlotte Bay", "II", ifelse(sres$Region=="Lizard", "III", ifelse(sres$Region=="Cairns", "IV", ifelse(sres$Region=="Cape Bowling Green", "V", ifelse(sres$Region=="Hydrographers Passage", "VI", ifelse(sres$Region=="Capricorn Bunkers", "VII",NA )))))))


sres$Reg2 <- as.character(sres$Region)
sres$Reg2[sres$Reg2=="Princess Charlotte Bay"] <- "Princess\nCharlotte Bay"
sres$Reg2[sres$Reg2=="Hydrographers Passage"] <- "Hydrographers\nPassage"
sres$Reg2[sres$Reg2=="Cape Bowling Green"] <- "Cape Bowling\nGreen"
sres$Reg2[sres$Reg2=="Capricorn Bunkers"] <- "Capricorn\nBunkers"
sres$Reg2[sres$Reg2=="Cape Grenville"] <- "Cape\nGrenville"

regions

sres$Reg2 <- factor(sres$Reg2, levels=rev(c("Capricorn\nBunkers","Hydrographers\nPassage","Cape Bowling\nGreen", "Cairns","Lizard","Princess\nCharlotte Bay","Cape\nGrenville")))

residav <- aggregate(bimod~Reg3, sres[sres$Zone=="Crest",], mean)
residav

# bmod? 
residplot2 <- ggplot(sres[sres$Zone=="Crest",], aes(bimod, reorder(Reef, -GPS.S)))+
#geom_vline(xintercept=0, size=1, col="white")+
geom_vline(xintercept=0, size=0.5, col="grey", linetype="dotted")+
#geom_rect(data=NULL, aes(xmin=Inf, xmax=0, ymin=-Inf, ymax=Inf), fill="#ef8a62", alpha=0.05)+
#geom_rect(data=NULL, aes(xmin=-Inf, xmax=0, ymin=-Inf, ymax=Inf), fill="#67a9cf", alpha=0.05)+
#geom_rect(data=NULL, aes(xmin=Inf, xmax=0, ymin=-Inf, ymax=Inf), fill="grey96", alpha=0.05)+
#geom_rect(data=NULL, aes(xmin=-Inf, xmax=0, ymin=-Inf, ymax=Inf), fill="grey80", alpha=0.05)+
geom_boxplot( size=0.18, outlier.size=0.1, fill="grey80")+
#geom_segment(data=residav, aes(x=bimod, xend=bimod, y=-Inf, yend=Inf), size=0.25, linetype="dashed")+
#guides(fill="none")+
#scale_fill_manual(values=rcols)+
#scale_fill_viridis(discrete=T, direction=-1)+
scale_fill_viridis()+
facet_wrap(~Reg3, ncol=1, scales="free_y", strip.position="left")+
theme_classic()+theme(axis.line.y=element_blank(), 
strip.background=element_blank(), 
strip.text.y.left=element_text(size=8, angle=0, hjust=1),
 axis.text.y=element_blank(), axis.ticks=element_blank(),axis.title.x=element_text(size=9), panel.margin.y=unit(2, "mm"), panel.background=element_rect(fill="grey96"))+
labs(y="", x="Deviation from expected\nbleaching (2024)")
residplot2

#####################################################################

theme <- theme(strip.background=element_blank(), legend.title=element_blank(), plot.background=element_blank())

f1 <- ggplot()+
geom_boxplot(data=sres[sres$Zone=="Crest",], aes(as.factor(freq2),fill=as.factor(freq2), betamod), outlier.size=0.05, size=0.1,position = position_dodge2(preserve = "single"), alpha=0.5)+ #,fill="grey95"
stat_summary(data=tres[tres$Zone=="Crest",], aes(as.factor(freq2), betamod, group=as.factor(n)),geom="line",show_guide=F)+
stat_summary(data=tres[tres$Zone=="Crest",], aes(as.factor(freq2), fill=as.factor(freq2), betamod),size=0.65,show_guide=F, shape=21, stroke=0.3)+
coord_cartesian(xlim=c(1,5.7))+
#scale_fill_viridis(discrete=T)+
scale_fill_manual(values=c("grey", "#fecc5c", "#fd8d3c", "#e31a1c", "#500000"))+
#geom_text(data=andat[andat$y=="betamod" & andat$Zone=="Crest",], aes(x=5.5, y=y4b, label=sig), fontface="bold", hjust=0, show_guide = FALSE, size=4)+
guides(fill="none")+
labs(x="N events > 6 DHW\n(2016-2023)", y="Bleaching residuals\n(quasibinomial)")+
theme_classic()+theme
f1

f2 <- ggplot()+
geom_boxplot(data=sres[sres$Zone=="Crest",], aes(as.factor(freq2),tab, fill=as.factor(freq2)), outlier.size=0.05, size=0.1,position = position_dodge2(preserve = "single"), alpha=0.5)+ #,fill="grey95"
stat_summary(data=tres[tres$Zone=="Crest",], aes(as.factor(freq2), tab, group=1), geom="line")+
stat_summary(data=tres[tres$Zone=="Crest",], aes(as.factor(freq2), tab, fill=as.factor(freq2)),size=0.55, shape=21, stroke=0.3)+
scale_fill_manual(values=c("grey", "#fecc5c", "#fd8d3c", "#e31a1c", "#500000"))+
coord_cartesian(xlim=c(1,5.7))+
guides(fill="none")+
#geom_text(data=andat[andat$y=="tab" & andat$Zone=="Crest",], aes(x=5.5, y=y4b, label=sig), 
#fontface="bold", hjust=0, show_guide = FALSE, size=5)+
labs(x="N events > 6 DHW\n(2016-2023)", y="% Acropora cover (2024)")+
theme_classic()+theme
f2

lastcols <- rev(scales::viridis_pal()(4))
lastcols

x1 <- ggplot()+
geom_boxplot(data=sres[sres$Zone=="Crest",],aes(as.factor(last2), betamod, fill=as.factor(last2)), outlier.size=0.05, size=0.1,position = position_dodge2(preserve = "single"), alpha=0.5)+ # fill="grey95"
stat_summary(data=tres[tres$Zone=="Crest",], aes(as.factor(last2), betamod,  group=1),geom="line")+
stat_summary(data=tres[tres$Zone=="Crest",], aes(as.factor(last2), betamod,fill=last2), size=0.55, shape=21, stroke=0.3)+
#scale_fill_viridis(discrete=T, direction=-1)+
scale_fill_manual(values=c(lastcols[1:3], "grey"))+
labs(x="Years since last\nevent > 6 DHW", y="Bleaching residuals\n(quasibinomial)")+
#facet_wrap(~Zone)+
guides(fill="none")+
#geom_text(data=andat2b[andat2b$y=="betamod",], aes(x=4.5, y=y4b, label=sig, size=sz), 
#fontface="bold", hjust=0, show_guide = FALSE)+
scale_size_manual(values=c(4))+guides(size="none")+
coord_cartesian(xlim=c(1,4.7))+
scale_colour_manual(values=c("#feb24c", "#de2d26", "black"))+
theme_classic()+theme
x1

x2 <- ggplot()+
geom_boxplot(data=sres[sres$Zone=="Crest",],aes(as.factor(last2), tab, fill=as.factor(last2)), outlier.size=0.05, size=0.1,position = position_dodge2(preserve = "single"), alpha=0.5)+ # fill="grey95"
stat_summary(data=tres[tres$Zone=="Crest",], aes(as.factor(last2), tab,  group=1),geom="line")+
stat_summary(data=tres[tres$Zone=="Crest",], aes(as.factor(last2), tab,fill=last2), size=0.55, shape=21, stroke=0.3)+
#scale_fill_viridis(discrete=T, direction=-1)+
scale_fill_manual(values=c(lastcols[1:3], "grey"))+
labs(x="Years since last\nevent > 6 DHW", y="% tabular Acropora")+
#facet_wrap(~Zone)+
guides(fill="none")+
#geom_text(data=andat2b[andat2b$y=="betamod",], aes(x=4.5, y=y4b, label=sig, size=sz), 
#fontface="bold", hjust=0, show_guide = FALSE)+
scale_size_manual(values=c(4))+guides(size="none")+
coord_cartesian(xlim=c(1,4.7))+
scale_colour_manual(values=c("#feb24c", "#de2d26", "black"))+
theme_classic()+theme
x2


##################################################################### heatplot
heatplot2 <- ggplot()+
geom_raster(data=out[out$i=="betamod",], aes(x=freq2, y=last2, fill=response))+
scale_radius()+
facet_wrap(~label)+
ggtitle("")+
#ggtitle("Heatwave history\nvs. susceptibility")+
coord_cartesian(xlim=c(1,4))+
labs(x="N events\n(2016-2023)", y="Max. severity (DHW)", fill="Bleaching\nresiduals\n(2024)")+
scale_fill_distiller(palette="Spectral")+
scale_y_continuous(expand=c(0,0))+scale_x_continuous(expand=c(0,0))+
theme_bw()+theme(strip.background=element_blank(), strip.text=element_blank(), legend.title=element_text(size=7), legend.text=element_text(size=8), legend.key.width=unit(2, "mm"),legend.key.height=unit(3, "mm"), legend.position="right", legend.margin=margin(1,1,1,-5), plot.title=element_text(size=8, face="bold", hjust=0.5))
heatplot2

#####################################################################

andat

fig3 <- plot_grid(
plot_grid(
NULL, 
plot_grid(freqmap, lastmap, rel_widths=c(1, 1.2), labels=c("A", "B"), label_size=9, vjust=-0.5), NULL,
plot_grid(
f1+labs(y="Bleaching residuals (2024)")+theme(axis.title=element_text(size=9), axis.line=element_line(size=0.2)), 
f2+labs(y="% Acropora (2024)")+theme(axis.title=element_text(size=9), axis.line=element_line(size=0.2)),
x1+labs(y="Bleaching residuals (2024)")+theme(axis.title=element_text(size=9), axis.line=element_line(size=0.2)), 
x2+labs(y="% Acropora (2024)")+theme(axis.title=element_text(size=9), axis.line=element_line(size=0.2)),
nrow=1, labels=c("D", "E", "F", "G"),label_size=9, vjust=0.3),
ncol=1, rel_heights=c(0.1, 1.2, 0.05, 1))
,
plot_grid(residplot2+ggtitle("Bleaching residuals")+theme(plot.title=element_text(size=8, hjust=0.5, face="bold")), heatplot2, ncol=1, rel_heights=c(1, 0.7), labels=c("C", "H"),label_size=9, vjust=c(1.5,0.5)),
rel_widths=c(1, 0.4))+
draw_text(x=0.17, y=0.98, text="Heatwave frequency", size=8, fontface="bold")+
draw_text(x=0.49, y=0.98, text="Recovery interval", size=8, fontface="bold")+
draw_text(x=0.165, y=0.2, text="***", size=8, fontface="bold")+
draw_text(x=0.34, y=0.14, text="*", size=8, fontface="bold")+
draw_text(x=0.52, y=0.35, text="***", size=8, fontface="bold")+
draw_text(x=0.7, y=0.23, text="***", size=8, fontface="bold")
fig3

# ggsave("figs/fig3code.jpg", fig3, height=5, width=7.2)






