

# heat stress history

dhwreefs <- ggplot()+
geom_histogram(data=dhw, aes(x=dhw, fill=year), col="black", size=0.1)+
scale_fill_distiller(palette="BrBG", direction=-1)+ #BrBG "PRGn"
facet_wrap(~year, ncol=1,  strip.position="right")+
scale_y_continuous(expand=c(0,0))+scale_x_continuous(expand=c(0,0))+
theme_classic()+
xlab("Degree Heating Weeks\n(DHW)")+ylab("% of reef grids")+
theme(axis.text.y=element_blank(),strip.placement = "outside",  panel.spacing = unit(-1, "lines"),plot.background=element_blank(), panel.background=element_blank(), axis.ticks.y=element_blank(), strip.background=element_blank(), strip.text.y.right=element_text(angle=0, vjust=0), axis.title=element_text(size=9), axis.line.y=element_blank())
dhwreefs

fdat2$p <- fdat2$Freq
tdat2$p <- tdat2$Freq
freqs <- rbind(cbind(fdat, type="all reefs"), cbind(fdat2, type="surveyed reefs"))

p1 <- ggplot(freqs, aes(x=Var1, y=as.numeric(p), fill=type))+
geom_bar(stat="identity", col="black", size=0.3, position = position_dodge2(width = 0.9, preserve = "single"))+
labs(x="Number of\n> 6 DHW events\n(2016 - 2024)", y="")+
scale_y_continuous(expand=c(0,0))+
#scale_fill_viridis(discrete=T, option="C")+
scale_fill_manual(values=c("black", "white"))+
#scale_fill_manual(values=c("yellow", "orange"))+
theme_classic()+theme(plot.background=element_blank(), panel.background=element_blank(), legend.title=element_blank(), legend.key.size=unit(2, "mm"), axis.title=element_text(size=9), axis.line=element_line(size=0.1))
p1


last <- rbind(cbind(tdat, type="all reefs"), cbind(tdat2, type="surveyed reefs"))

p2a <- ggplot(last, aes(x=Var1, y=as.numeric(p), fill=type))+
geom_bar( stat="identity", col="black", size=0.3, width=0.7, position = position_dodge2(width = 0.9, preserve = "single"))+
labs(x="Years since last\n> 6 DHW event \n(2016 - 2024)", y="")+
scale_fill_manual(values=c( "black", "white"))+
scale_y_continuous(expand=c(0,0))+
theme_classic()+theme(plot.background=element_blank(), panel.background=element_blank(), legend.title=element_blank(), legend.key.size=unit(2, "mm"), axis.title=element_text(size=9), axis.line=element_line(size=0.1))
p2a

#######################################
# ACROPORA
#######################################

comp1R$tlab <- factor(comp1R$tlab, levels=rev(c("2016a", "2016b", "2024")))

tabbox2 <- ggplot()+
geom_boxplot(data=comp1R[comp1R$align %in% c("tabular_Acropora"),], aes(x=cov, y=tlab, fill=tlab), outlier.size=0.1, size=0.2)+
scale_fill_manual(values=c("black", "red", "grey"))+
guides(fill="none")+
scale_x_sqrt()+
xlab("% tabular Acropora")+
theme_classic()+theme(axis.title.y=element_blank(), axis.line=element_line(size=0.2),axis.title.x=element_text(size=8))

acbox2 <- ggplot()+
geom_boxplot(data=comp1.acR, aes(x=cov, y=tlab, fill=tlab), outlier.size=0.1, size=0.2)+
scale_fill_manual(values=c("black", "red", "grey"))+
guides(fill="none")+
scale_x_sqrt()+
xlab("% Acropora")+
theme_classic()+theme(axis.title.y=element_blank(), axis.line=element_line(size=0.2), axis.title.x=element_text(size=8))

plot_grid(acbox2, tabbox2)

#######################################
# IMAGES
#######################################
library("png")
library("grid")
tab<-readPNG("figs/sils/tab.png")
tab<-rasterGrob(tab, interpolate=TRUE)
pori<-readPNG("figs/sils/pori.png")
pori<-rasterGrob(pori, interpolate=TRUE)
stag<-readPNG("figs/sils/stag.png")
stag<-rasterGrob(stag, interpolate=TRUE)
poc<-readPNG("figs/sils/poc.png")
poc<-rasterGrob(poc, interpolate=TRUE)
iso<-readPNG("figs/sils/iso.png")
iso<-rasterGrob(iso, interpolate=TRUE)
mas<-readPNG("figs/sils/mas.png")
mas<-rasterGrob(mas, interpolate=TRUE)


tcols <- c("grey", "red", "black")
names(tcols) <- c("1", "2", "3")

mdsvectors$lab <- c(nrow(mdsvectors):1)
mdsvectors$lab2 <- rownames(mdsvectors)
mdsvectors$lab2[mdsvectors$lab2=="Faviidae"] <- "Merulinidae" 
mdsvectors$lab2[mdsvectors$lab2=="Mussidae"] <- "Lobophyllidae" 
mdsvectors$lab2[mdsvectors$lab2=="staghorn_Acropora"] <- "staghorn Acropora" 
mdsvectors$lab2[mdsvectors$lab2=="tabular_Acropora"] <- "tabular Acropora" 
mdsvectors$lab2[mdsvectors$lab2=="other_Acropora"] <- "other Acropora" 
mdsvectors$lab2[mdsvectors$lab2=="other_scleractinians"] <- "other scleractinians" 


mdsvectors$MDS2b <- mdsvectors$MDS2
mdsvectors$MDS2b[mdsvectors$lab2 =="Lobophyllidae" ] <- mdsvectors$MDS2b[mdsvectors$lab2 =="Lobophyllidae" ] - 0.02
mdsvectors$MDS2b[mdsvectors$lab2 =="Isopora" ] <- mdsvectors$MDS2b[mdsvectors$lab2 =="Isopora" ] + 0.03
mdsvectors$MDS2b[mdsvectors$lab2 =="other Acropora"  ] <- mdsvectors$MDS2b[mdsvectors$lab2 =="other Acropora" ] + 0.05
mdsvectors$MDS2b[mdsvectors$lab2 =="other scleractinians"  ] <- mdsvectors$MDS2b[mdsvectors$lab2 =="other scleractinians" ] + 0
mdsvectors$MDS2b[mdsvectors$lab2 =="Poritidae"  ] <- mdsvectors$MDS2b[mdsvectors$lab2 =="Poritidae" ] - 0.02
mdsvectors$MDS1b <- mdsvectors$MDS1
mdsvectors$MDS1b[mdsvectors$lab2 =="Pocillopora"  ] <- mdsvectors$MDS1b[mdsvectors$lab2 =="Pocillopora" ] + 0.06

tcols <- c("grey55", "red", "black")

vecplot <- ggplot()+
geom_segment(data=mdsvectors, aes(x=0, xend=MDS1, y=0, yend=MDS2), col="grey")+
#geom_label(data=mdsvectors, aes(MDS1, MDS2b, label=lab2), hjust=ifelse(mdsvectors$MDS1 >0, 0, 1), size=2.5, fontface="bold", label.size=NA, alpha=0.8)+
geom_text(data=mdsvectors, aes(MDS1b, MDS2b, label=lab2), hjust=ifelse(mdsvectors$MDS1b >0, 0, 1), size=2.5, fontface="bold")+
annotation_custom(tab, xmin=-1.1, xmax=-0.3, ymin=-0.6, ymax=-0.4)+
annotation_custom(stag, xmin=-1.9, xmax=-1.2, ymin=-0.5, ymax=-0.3)+
annotation_custom(poc, xmin=-0.1, xmax=0.6, ymin=-0.45, ymax=-0.25)+
annotation_custom(pori, xmin=0.6, xmax=1.2, ymin=0.65, ymax=0.85)+
annotation_custom(mas, xmin=0.6, xmax=1.2, ymin=0.25, ymax=0.4)+
lims(x=c(-2.5, 1.8), y=c(-0.8,0.8))+
theme_bw()+guides(size="none")+theme_void()+theme(plot.margin=margin(10,10,10,10))
vecplot


arrowplot <- ggplot()+
#geom_point(data=mdspoints, aes(NMDS1, NMDS2, fill=as.factor(Time)), shape=21)+
geom_path(data=mdspoints[mdspoints$t %in% c("Apr16", "Oct16"),], aes(x=NMDS1, y=NMDS2, group=Reef), col="grey", arrow=arrow(length=unit(0.5, "mm")), size=0.4)+
geom_text(data=NULL, aes(x=-1.5, y=-1.23, label="Mass mortality (2016)"), col='grey50', size=2.3, hjust=0, fontface="bold")+ #-1
#geom_text(data=NULL, aes(x=-1.5, y=-1.1, label="Mass mortality (2016)"), col='red', size=2.3, hjust=0, fontface="bold")+
geom_text(data=NULL, aes(x=-1.5, y=-1.4, label="Reassembly (2016-2024)"), col='black', size=2.3, hjust=0, fontface="bold")+ #-1.15
#geom_path(data=mdspoints[mdspoints$Time %in% c(1, 2) & mdspoints$Reef %in% time3$Reef,], aes(x=NMDS1, y=NMDS2, group=Reef), col="red")+
geom_path(data=mdspoints[mdspoints$t %in% c("Oct16","Mar24"),], aes(x=NMDS1, y=NMDS2, group=Reef), col="black", arrow=arrow(length=unit(0.5, "mm"), ends="first"))+
lims(x=c(-1.5, 2), y=c(-1.4, 1.2))+  #y=c(-1.2, 1)
#scale_colour_manual(values=rcols)+
theme_bw()+guides(size="none")+theme(legend.position=c(0.1, 0.9), legend.title=element_blank(), panel.grid.minor=element_blank(),panel.grid.major=element_blank(), axis.title=element_text(size=7),axis.text=element_text(size=7))+guides(size="none", col="none", fill="none")
arrowplot 

densplot <- ggplot()+
#stat_density_2d(data=mdspoints, aes(NMDS1, NMDS2, fill=as.factor(Time)), breaks=c(0.15), geom="polygon", alpha=0.25)+
#stat_density_2d(data=mdspoints, aes(NMDS1, NMDS2, fill=as.factor(Time)), breaks=c(0.75), geom="polygon", alpha=0.25)+
lims(x=c(-1.5, 2), y=c(-1.4, 1.2))+ #y=c(-1.2, 1)
#stat_ellipse(data=mdspoints[mdspoints$Time %in% c(2),], aes(NMDS1, NMDS2, col=as.factor(Time)), geom="polygon", alpha=0.25, fill="red")+
stat_ellipse(data=mdspoints[mdspoints$t %in% c("Apr16", "Mar24"),], aes(NMDS1, NMDS2, fill=as.factor(t), col=as.factor(t)), geom="polygon", alpha=0.25)+
#geom_path(data=mdspoints[mdspoints$Time %in% c(2,3),], aes(x=NMDS1, y=NMDS2, group=Reef), col="black", arrow=arrow(length=unit(1, "mm")), size=0.2)+
geom_point(data=mdspoints, aes(NMDS1, NMDS2, fill=as.factor(t)), shape=21, size=1, stroke=0.1)+
#geom_path(data=avs, aes(x=NMDS1, y=NMDS2),   size=1, arrow=arrow(length=unit(1, "mm")))+
geom_text(data=NULL, aes(x=-1.5, y=-1.1, label="2016 - pre-bleaching"), col='grey50', size=2.3, hjust=0, fontface="bold")+ #-0.93
geom_text(data=NULL, aes(x=-1.5, y=-1.25, label="2016 - post-bleaching"), col="red",size=2.3, hjust=0, fontface="bold")+ #-1.06
geom_text(data=NULL, aes(x=-1.5, y=-1.4, label="2024 - pre-bleaching"), col='black', size=2.3, hjust=0, fontface="bold")+ # -1.2
scale_colour_manual(values=c("grey", "black","red"))+scale_fill_manual(values=c("grey", "black","red"))+
theme_bw()+guides(size="none")+theme(legend.position=c(0.1, 0.9), legend.title=element_blank(), panel.grid.minor=element_blank(),panel.grid.major=element_blank(), axis.title=element_text(size=7),axis.text=element_text(size=7))+guides(size="none", col="none", fill="none")
densplot


# fig 1 


two2 <- plot_grid(get_legend(p1+theme(legend.direction="horizontal")),
plot_grid(p1+guides(fill="none"), p2a+guides(fill="none")), ncol=1, rel_heights=c(0.1, 1))

fig1 <- plot_grid(NULL, 
plot_grid(plot_grid(NULL, map24c, ncol=1, rel_heights=c(-0.05, 1), labels=c("", "A"), label_size=9), NULL,
plot_grid(
plot_grid(NULL, acbox2+theme(axis.text.y=element_text(size=7.5)),tabbox2+theme(axis.text.y=element_text(size=7.5)), labels=c("B", ""), label_size=9, rel_widths=c(0.07, 1, 1), nrow=1), 
plot_grid(densplot, arrowplot, nrow=1, labels=c("C", "D"), label_size=9), 
ncol=1, rel_heights=c(0.4, 1)), 
nrow=1, rel_widths=c(1, 0.1, 1.5))
,NULL,
plot_grid(dhwreefs+guides(fill="none"),   two2, vecplot, nrow=1, rel_widths=c(0.9,1.3,1.5), labels=c("E", "F", ""), label_size=9), 
ncol=1, rel_heights=c(0.05, 1,0.1,  0.8))+
#draw_text(text="Compositional change", y=0.985, x=0.72, fontface="bold", size=10)+
#draw_text(text="2024 surveys", y=0.955, x=0.2, fontface="bold", size=10)+
#draw_text(text="Climate extremes", y=0.43, x=0.31, fontface="bold", size=10)+
draw_text(text="% of reef grids", y=0.23, x=0.25,  size=9, angle=90)+
draw_line(x=c(0.65, 0.65), y=c(0.45, 0.46), col="grey", size=0.25)+
draw_line(x=c(0.8, 0.8), y=c(0.45, 0.46), col="grey", size=0.25)+
draw_line(x=c(0.65, 0.8), y=c(0.45, 0.45), col="grey", size=0.25)+
draw_line(x=c(0.73, 0.73), y=c(0.45, 0.4), col="grey", size=0.25)+
draw_line(x=c(0.73, 0.8), y=c(0.4, 0.4), col="grey", size=0.25)+
draw_line(x=c(0.8, 0.8), y=c(0.4, 0.37), col="grey", size=0.25)
fig1


#   ggsave( "figs/fig1code.jpg",fig1,height=5.5, width=7)


