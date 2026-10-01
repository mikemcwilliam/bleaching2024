


#---------------------------------------------# FIGURE S7 residual plots 
head(sres)

sres$region2 <- factor(sres$region2, levels=regions)

avlineB <- mean(sres[sres$Zone=="Crest","pbleach"], na.rm=T)

residb <- ggplot(sres[sres$Zone=="Crest",], aes(pbleach*100, reorder(Reef, -GPS.S)))+
geom_vline(xintercept=avlineB*100)+
geom_boxplot(aes(fill=region2), size=0.1, outlier.size=0.1)+
#scale_colour_manual(values=c("grey", "black"))+
facet_wrap(~region2, ncol=1, scales="free_y", strip.position="left")+
scale_fill_viridis(discrete=T, direction=-1)+
guides(fill="none")+
labs(x="% bleaching (2024)")+
theme_classic()+theme(axis.line.y=element_blank(), 
strip.background=element_blank(), 
strip.text.y.left=element_text(size=8, angle=0, hjust=1),
 axis.text.y=element_blank(), axis.ticks=element_blank(),axis.title.x=element_text(size=9), panel.margin.y=unit(2, "mm"), panel.background=element_rect(fill="grey96"), axis.title.y=element_blank())
residb


resid1 <- ggplot(sres[sres$Zone=="Crest",], aes(betamod, reorder(Reef, -GPS.S)))+
geom_vline(xintercept=0)+
geom_boxplot(aes(fill=region2), size=0.1, outlier.size=0.1)+
#scale_colour_manual(values=c("grey", "black"))+
facet_wrap(~region2, ncol=1, scales="free_y", strip.position="left")+
scale_fill_viridis(discrete=T, direction=-1)+
guides(fill="none")+
labs(x="Deviation from expected\nbleaching (2024)")+
theme_classic()+theme(axis.line.y=element_blank(), 
strip.background=element_blank(), 
strip.text.y.left=element_text(size=8, angle=0, hjust=1),
 axis.text.y=element_blank(), axis.ticks=element_blank(),axis.title.x=element_text(size=9), panel.margin.y=unit(2, "mm"), panel.background=element_rect(fill="grey96"), axis.title.y=element_blank())
resid1

avline <- mean(sres[sres$Zone=="Crest","acro"], na.rm=T)
resid2 <- ggplot(sres[sres$Zone=="Crest",], aes(acro, reorder(Reef, -GPS.S)))+
geom_vline(xintercept=avline)+
scale_fill_viridis(discrete=T, direction=-1)+
guides(fill="none")+
geom_boxplot(aes(fill=region2), size=0.1, outlier.size=0.1)+
#scale_colour_manual(values=c("grey", "black"))+
labs(x="% Acropora cover\n(2024)")+
facet_wrap(~region2, ncol=1, scales="free_y", strip.position="left")+
theme_classic()+theme(axis.line.y=element_blank(), 
strip.background=element_blank(), 
strip.text.y.left=element_text(size=8, angle=0, hjust=1),
 axis.text.y=element_blank(), axis.ticks=element_blank(),axis.title.x=element_text(size=9), panel.margin.y=unit(2, "mm"), panel.background=element_rect(fill="grey96"), axis.title.y=element_blank())



ggplot(smort, aes(region2, Lchange))+geom_boxplot()+geom_hline(yintercept=0, size=0.1)

mortregions <- ggplot(smort[smort$Zone %in% "Crest",], aes(Lchange, Reef))+
geom_vline(xintercept=0)+
scale_fill_viridis(discrete=T, direction=-1, begin=0, end=0.7)+
guides(fill="none")+
geom_boxplot(aes(fill=region2), size=0.1, outlier.size=0.1)+
#scale_colour_manual(values=c("grey", "black"))+
labs(x="Change in\n% coral cover (2024)")+
scale_x_continuous(breaks=brks, labels=labs)+
facet_wrap(~region2, ncol=1, scales="free_y", strip.position="left")+
theme_classic()+theme(axis.line.y=element_blank(), 
strip.background=element_blank(), 
strip.text.y.left=element_text(size=8, angle=0, hjust=1),
 axis.text.y=element_blank(), axis.ticks=element_blank(),axis.title.x=element_text(size=9), panel.margin.y=unit(2, "mm"), panel.background=element_rect(fill="grey96"), axis.title.y=element_blank())
mortregions

figS7 <- plot_grid(plot_grid(residb, resid1, resid2, labels=c("A", "B", "C"), label_size=9, nrow=1),
plot_grid(diffplot+labs(x=""), mortregions, labels=c("D", "E"),label_size=9), ncol=1)

figS7

#     ggsave( "figs/supplement/figS7.jpg",figS7, height=4, width=6.5)
