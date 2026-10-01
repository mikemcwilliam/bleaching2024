


#---------------------------------------------# FIGURE S5  - nmds of bleaching vs composition (2024)

nmdsplot1 <- ggplot()+
#geom_point(data=tdf, aes(NMDS1.2, NMDS2.2, col=pbleach*100))+
geom_point(data=tdf, aes(NMDS1.2, NMDS2.2, fill=pbleach*100), shape=21, stroke=0.1, size=1.2)+ # no size
#geom_segment(data=mdsvectors2, aes(x=0, xend=MDS1, y=0, yend=MDS2))+
#geom_label(data=mdsvectors2, aes(MDS1, MDS2, label=rownames(mdsvectors2)), hjust=ifelse(mdsvectors2$MDS1 >0, 0, 1), size=2.1, fontface="bold", label.size=NA, alpha=0.8)+
scale_radius(range=c(0.2,5))+
lims(x=c(-2, 2), y=c(-1.6,1.4))+
#scale_colour_manual(values=rcols)+
#scale_colour_viridis(option="A")+
scale_fill_viridis(option="A")+
labs(x="NMDS1", y="NMDS2")+
guides(size="none", fill=guide_colourbar(title="% bleaching"))+
theme_bw()+theme(legend.background=element_blank(),panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.background=element_rect(fill = "transparent",colour = NA), plot.background=element_rect(fill = "transparent",colour = NA), axis.title=element_text(size=7))#+facet_wrap(~Zone)
nmdsplot1

tax <- read.csv("data/info/composition_taxa.csv")
mdsvectors2$lab <- tax$label2[match(rownames(mdsvectors2), tax$taxon)]

mdsvectors2$lab[rownames(mdsvectors2)=="Other.Coral"] <- "Other Scler."
mdsvectors2$lab[rownames(mdsvectors2)=="Soft.Coral"] <- "Soft coral"

mdsvectors2$lab2 <- tax$label3[match(rownames(mdsvectors2), tax$taxon)]
mdsvectors2$lab2[rownames(mdsvectors2)=="Other.Coral"] <- ""
mdsvectors2$lab2[rownames(mdsvectors2)=="Soft.Coral"] <- "Soft coral"

mdsvectors2$length <- sqrt((abs(mdsvectors2$MDS2)^2)+(abs(mdsvectors2$MDS1)^2))

mdsvectors2$MDS2b <-  mdsvectors2$MDS2
mdsvectors2$MDS2b[mdsvectors2$lab2=="Porites massive"] <- mdsvectors2$MDS2b[mdsvectors2$lab2=="Porites massive"]-0.03
mdsvectors2$MDS2b[mdsvectors2$lab2=="Soft coral"] <- mdsvectors2$MDS2b[mdsvectors2$lab2=="Soft coral"]-0.15
mdsvectors2$MDS2b[mdsvectors2$lab2=="Pocillopora"] <- mdsvectors2$MDS2b[mdsvectors2$lab2=="Pocillopora"]+0.1
mdsvectors2$MDS2b[mdsvectors2$lab2=="Lobophyllia"] <- mdsvectors2$MDS2b[mdsvectors2$lab2=="Lobophyllia"]-0.05
mdsvectors2$MDS2b[mdsvectors2$lab2=="Dipsastrea"] <- mdsvectors2$MDS2b[mdsvectors2$lab2=="Dipsastrea"]+0.05
mdsvectors2$MDS2b[mdsvectors2$lab2=="Acropora other"] <- mdsvectors2$MDS2b[mdsvectors2$lab2=="Acropora other"]+0.05

nmdsplot2cX<-ggplot()+
#geom_point(data=tdf, aes(NMDS1.2, NMDS2.2, col=pdie))+
geom_segment(data=mdsvectors2, aes(x=0, xend=MDS1, y=0, yend=MDS2), size=0.2, col="slategrey")+
#geom_text(data=labs1, aes(MDS1, MDS2, label=lab), hjust=ifelse(labs1$MDS1 >0, 0, 1), size=2.1, fontface="bold")+
#geom_text(data=labs2, aes(MDS1, MDS2b, label=lab), hjust=ifelse(labs2$MDS1 >0, 0, 1), size=1.8, fontface="bold")+
geom_text(data=mdsvectors2, aes(MDS1, MDS2b, label=lab2, size=length), hjust=ifelse(mdsvectors2$MDS1 >0, 0, 1))+
geom_text(data=NULL, aes(x=1.15, y=0.1, label="tabular", size=1))+
geom_text(data=NULL, aes(x=1.19, y=-1.1, label="staghorn", size=1))+
geom_text(data=NULL, aes(x=-2, y=-0.92, label="branching", size=1))+
scale_radius(range=c(0.2,5))+
labs(x="NMDS1", y="NMDS2")+
#xlim(c(-2.2, 1.5))+
#lims(x=c(-2.5, 2), y=c(-1.2,0.8))+
scale_size(range=c(1.5, 3.5))+
lims(x=c(-3, 2), y=c(-1.6,1.4))+guides(size="none")+
labs(x="NMDS1", y="NMDS2")+
guides(size="none", fill=guide_colourbar(title="% bleaching"))+
#theme_bw()+theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.background=element_rect(fill = "transparent",colour = NA), plot.background=element_rect(fill = "transparent",colour = NA), legend.key.width=unit(2, "mm"), legend.title=element_text(size=8, face="bold"), axis.title=element_text(size=7))#+facet_wrap(~Zone)
theme_void()
nmdsplot2cX

figS6 <- plot_grid(nmdsplot2cX,nmdsplot1)
figS6

#     ggsave( "figs/supplement/figS6.jpg",figS6, height=2, width=6)

