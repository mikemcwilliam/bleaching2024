


col16 <- "grey65"


bplot2 <- ggplot()+
geom_point(data=sites[sites$Zone=="Crest",], aes(x=max.dhw, y=pbleach*100), shape=21, fill="black", size=0.5)+
geom_text(data=NULL, aes(x=13.7, y=100, label="2016"), col=col16, size=3)+
geom_text(data=NULL, aes(x=13.7, y=91, label="2024"), size=3)+
geom_point(data=j.av[!j.av$Reef=="12-059",], aes(DHWs, BleachDead),  col=col16, shape=4, size=1, stroke=0.3)+
geom_line(data=mod2016, aes(dhw, y), linewidth=0.5, col=col16)+
#geom_line(data=curvesCrest, aes(predictor, betafit*100), linewidth=0.5, col="black")+
geom_line(data=curvesCrest, aes(predictor, bifit*100), linewidth=0.5, col="black")+
#geom_ribbon(data=fit.datX, aes(x=max.dhw, ymin=(bifit-(bise*1.95))*100, ymax=(bifit+(bise*1.95))*100), alpha=0.2)+
#geom_ribbon(data=fit16, aes(x=DHWs, ymin=lwr, ymax=upr), alpha=0.2, fill="red")+
#geom_line(data=curves[curves$data=="tdf" & curves$Zone=="Crest",], aes(predictor, gam1.5*100), size=0.5, col="black")+
labs(x="Degree Heating Weeks", y="% bleaching")+
xlim(c(1,14))+
ylim(c(0,105))+
ggtitle("Coral bleaching\n(2024 vs 2016)")+
#scale_fill_manual(values=rcols)+scale_colour_manual(values=rcols)+
scale_x_continuous(breaks=c(0,5,10, 15), limits=c(0,15))+
theme_classic()+theme(legend.text=element_text(size=7), legend.title=element_text(size=7), legend.key.height=unit(4, "mm"),  axis.title=element_text(size=8), legend.key.width=unit(1, "mm"), axis.line=element_line(size=0.2),plot.title=element_text(size=8, hjust=0.5, face="bold"))
bplot2


dplot2 <- ggplot()+
geom_point(data=j.av2, aes(DHWs, Mortality),col=col16, shape=4, size=1, stroke=0.3)+
geom_line(data=fit.dat3, aes(x=DHWs, y=(fit*100)-2), col=col16)+
geom_point(data=sites[sites$Zone=="Crest",], aes(max.dhw, pdead*100), shape=21, fill="black", size=0.5)+
geom_text(data=NULL, aes(x=13.7, y=32, label="2016"), col=col16, size=3)+
geom_text(data=NULL, aes(x=13.7, y=40, label="2024"), size=3)+
labs(x="Degree Heating Weeks", y="% recent mortality")+
geom_line(data=fit.dat2, aes(x=max.dhw, y=fit*100))+
ggtitle("Coral mortality\n(Mar-Apr, 2024 vs 2016)")+
scale_fill_viridis()+
scale_x_continuous(limits=c(0,15), breaks=c(0,5,10,15))+
theme_classic()+theme(axis.title=element_text(size=8), axis.line=element_line(size=0.2), plot.title=element_text(size=8, hjust=0.5, face="bold"))
dplot2

####################################

head(tdf)

acroplot <- ggplot(sites, aes(acro, pbleach*100))+ #[sites$Zone=="Crest"]
geom_point(aes(fill=pbleach*100), shape=21)+
scale_x_sqrt()+
labs(x="% Acropora cover", y="% bleaching")+
scale_fill_viridis(option="A")+guides(fill="none")+
ggtitle("Acropora vs bleaching")+
theme_classic()+theme(axis.title=element_text(size=8),plot.title=element_text(size=8, hjust=0.5, face="bold"))
acroplot

mods2 <- mods[mods$x2 %in% c("Tabular Acropora","Acroporidae","Pocilliporidae","Poritidae", "NMDS1", "NMDS2"),]

effplot <- ggplot()+
geom_vline(xintercept=0)+
geom_bar(data=mods2, aes(x=slp, y=reorder(x2, -slp)), stat="identity", fill="grey", col="black", size=0.1, width=0.7)+
#geom_point(data=mods2, aes(y=slp, x=reorder(x2, -slp)))+
geom_segment(data=mods2, aes(y=x2, yend=x2, x=low, xend=upp))+
facet_wrap(~y2,ncol=1)+
#xlim(c(-0.2, 0.2))+
ggtitle("Composition &\nbleaching (2024)")+
labs(x="Effect size of\ncomposition vs bleaching", y="")+
theme_classic()+theme(strip.background=element_blank(), axis.title=element_text(size=8), plot.title=element_text(size=8, hjust=0.5, face="bold"))
effplot

effplot2 <- ggplot()+
geom_bar(data=mods2[mods2$y2=="bleaching residuals",], aes(x=slp, y=reorder(x2, -slp)), stat="identity", fill="grey", col="black", size=0.1, width=0.7)+
#geom_point(data=mods2, aes(y=slp, x=reorder(x2, -slp)))+
geom_segment(data=mods2[mods2$y2=="bleaching residuals",], aes(y=x2, yend=x2, x=low, xend=upp), size=0.2)+
#xlim(c(-0.2, 0.2))+
geom_vline(xintercept=0)+
#ggtitle("Composition & deviation\nfrom expected bleaching")+
labs(x="Effect size on\nbleaching residuals", y="")+
theme_classic()+theme(strip.background=element_blank(), axis.title=element_text(size=8), plot.title=element_text(size=8, hjust=0.5, face="bold"), plot.background=element_blank(), panel.background=element_blank())
effplot2

####################################

recplot <- ggplot()+
geom_hline(yintercept=0, col="grey")+geom_vline(xintercept=0, col="grey")+
geom_point(data=acro, aes(x=change, y=resids), size=1)+
geom_smooth(data=acro, aes(x=change, y=resids), method="lm", se=F, col="red", size=0.35, formula=y~poly(x,1))+
geom_text(data=NULL, aes(-26, 0.4, label="Heron Is."), size=3)+
geom_text(data=NULL, aes(38, 1.3, label="Lizard Is."), size=3)+
#geom_text(data=acro, aes(x=change, y=resids, label=reef))+
geom_segment(data=NULL, aes(x=34,xend=38, y=1.25,yend=1.05), size=0.24, col="grey")+
geom_segment(data=NULL, aes(x=-26,xend=-30, y=0.1,yend=0.3), size=0.24, col="grey")+
geom_segment(data=acro, aes(x=change, xend=change, y=resids-resids.se, yend=resids+resids.se))+
theme_classic()+theme(axis.text=element_text(size=8), axis.title=element_text(size=8))+
labs(x="Change in % Acropora\n(2016-2024)", y="deviation from expected\nbleaching (2024)")
recplot

####################################

fig2 <- plot_grid(plot_grid(bplot2, dplot2, labels=c("A", "B"), label_size=9), plot_grid(effplot+theme(axis.line=element_line(size=0.25)), recplot+ggtitle("Compositional change\nvs bleaching")+theme(axis.line=element_line(size=0.25), plot.title=element_text(size=8, face="bold", hjust=0.5)), rel_widths=c(1,1.2),labels=c("C", "D"), label_size=9), ncol=1, rel_heights=c(1, 1.1))
fig2

#   ggsave("figs/fig2code.jpg", fig2, height=6.2, width=5.3)



