


mtheme <- theme(legend.text=element_text(size=7), legend.title=element_blank(), legend.key.height=unit(4, "mm"),  axis.title=element_text(size=10), legend.key.width=unit(1, "mm"), axis.line=element_line(size=0.2),plot.title=element_text(size=8, hjust=0.5, face="bold"))

labs=c(-90, -70, -50, -30, 0, 30)
brks <- 0.432 * log(labs+99.91) - 1.991 
brks
col16 <- "#97a6c4" # "goldenrod4"#"grey50"


p1 <- ggplot()+
geom_hline(yintercept=0, size=0.1)+
geom_line(data=fit16.1, aes(bl, fit), col=col16)+
geom_ribbon(data=fit16.1, aes(x=bl, ymin=fit-(se*1.95), ymax=fit+(se*1.95)), alpha=0.2, fill=col16)+
geom_point(data=lmort, aes(bl, change_use, col=year, shape=year), size=1, stroke=0.3)+
geom_line(data=fit24.1, aes(bl, fit), col="black")+
geom_ribbon(data=fit24.1, aes(bl, ymin=fit-(se*1.95), ymax=fit+(se*1.95)), alpha=0.2)+
#geom_smooth(data=dfm2, aes(bl, change_use, col=year), method="lm", formula=y~poly(x,3), size=0.4,show.legend=FALSE)+
labs(x="Prop. colonies bleached (%)", y="Change in coral cover (%)")+
scale_y_continuous(breaks=brks, labels=labs)+
scale_colour_manual(values=c(col16, "black"))+
scale_shape_manual(values=c(4, 16))+
theme_classic()+mtheme
p1

p2 <- ggplot()+
geom_hline(yintercept=0, size=0.1)+
#geom_smooth(data=lmort, aes(dhw, change_use, col=year), method="lm", formula=y~poly(x,2), size=0.4, show.legend=FALSE)+
geom_line(data=fit16.2, aes(dhw, fit), col=col16)+
geom_ribbon(data=fit16.2, aes(x=dhw, ymin=fit-(se*1.95), ymax=fit+(se*1.95)), alpha=0.2, fill=col16)+
geom_line(data=fit24.2, aes(dhw, fit), col="black")+
geom_ribbon(data=fit24.2, aes(x=dhw, ymin=fit-(se*1.95), ymax=fit+(se*1.95)), alpha=0.2)+
geom_point(data=lmort, aes(dhw, change_use, col=year, shape=year), size=1, stroke=0.3)+
labs(x="DHW (°C Weeks)", y="Change in coral cover (%)")+
scale_y_continuous(breaks=brks, labels=labs)+
scale_colour_manual(values=c(col16, "black"))+
scale_shape_manual(values=c(4, 16))+
theme_classic()+mtheme
p2


p3 <- ggplot()+
geom_hline(yintercept=0, size=0.1)+
geom_line(data=fit16.3, aes(acro2, fit), col=col16)+
geom_ribbon(data=fit16.3, aes(x=acro2, ymin=fit-(se*1.95), ymax=fit+(se*1.95)), alpha=0.2, fill=col16)+
geom_line(data=fit24.3, aes(acro2, fit), col="black")+
geom_ribbon(data=fit24.3, aes(x=acro2, ymin=fit-(se*1.95), ymax=fit+(se*1.95)), alpha=0.2)+
geom_point(data=lmort, aes(acro2, change_use, col=year, shape=year), size=1, stroke=0.3)+
#geom_smooth(data=lmort, aes(cov1, change_use, col=year), method="lm", formula=y~poly(x,1), size=0.4,show.legend=FALSE)+
labs(x=expression(paste("Initial ", italic("Acropora"), " cover")), y="Change in coral cover (%)")+
scale_y_continuous(breaks=brks, labels=labs)+
scale_colour_manual(values=c(col16, "black"))+
scale_shape_manual(values=c(4, 16))+
theme_classic()+mtheme
p3




colz <- c("darkblue","blue", "aquamarine", "yellow", "orange", "red", "darkred")
colbreaks <- c(0,     1,        2.5,            4,        6,        10,     15)

colvals <- colbreaks / max(colbreaks)

dhw.lims <- c(0,15)


history <- ggplot(m24, aes(as.factor(tlast2), Lchange))+
geom_boxplot(outlier.size=0.1, fill="grey90", size=0.2)+
geom_jitter(aes(fill=dhw), shape=21, height=0, width=0.1, stroke=0.1)+
scale_fill_viridis(option="B")+
#scale_fill_distiller(palette="Spectral")+
#scale_fill_gradientn(colours = colz, values=colvals, limits=dhw.lims)+ 
scale_y_continuous(breaks=brks, labels=labs)+
labs(x="Years since\nlast bleaching", y="Change in coral cover (%)", fill="DHW")+
theme_classic()+mtheme+theme(legend.title=element_text(size=8))
history



plot_grid(plot_grid(p1+guides(col="none", shape="none"), p2+guides(col="none",shape="none"), p3+guides(col="none",shape="none"), history, nrow=2, labels =c("a", "b", "c", "d")),get_legend(p1), rel_widths=c(1, 0.2))


fig4 <- plot_grid(plot_grid(p1+guides(col="none", shape="none"), p2+guides(col="none",shape="none"), p3+guides(col="none",shape="none"), get_legend(p1), nrow=1, rel_widths=c(1,1,1,0.2), labels =c("A", "B", "C"),  label_size=9), history+theme(axis.text.x=element_text(angle=30, hjust=1)), rel_widths=c(1,0.25), labels=c("", "D"), hjust=2, label_size=9)
fig4


#   ggsave("figs/fig4code.jpg", fig4,  height=3.2, width=9)


