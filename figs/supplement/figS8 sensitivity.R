


col16 <-  "#97a6c4" #"slategrey" #"goldenrod4"#"grey65" #"#1a80bb"


bplot2 <- ggplot()+
geom_ribbon(data=fit16, aes(x=DHWs, ymin=lwr, ymax=upr), alpha=0.2, fill=col16)+
geom_ribbon(data=fit.datX, aes(max.dhw, ymax=(bifit+(bise*2))*100, ymin=(bifit-bise*2)*100), alpha=0.1)+
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


effplot <- ggplot()+
geom_vline(xintercept=0)+
geom_bar(data=mods2, aes(x=slp, y=reorder(x2, -slp)), stat="identity", fill="grey", col="black", size=0.1, width=0.7)+
#geom_point(data=mods2, aes(y=slp, x=reorder(x2, -slp)))+
geom_segment(data=mods2, aes(y=x2, yend=x2, x=low, xend=upp))+
facet_wrap(~y2,ncol=1, scales="free_x")+
xlim(c(-0.4, 0.4))+
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

p2 <- ggplot()+
geom_hline(yintercept=0, size=0.1)+
#geom_smooth(data=dfm2, aes(dhw, change_use, col=year), method="lm", formula=y~poly(x,2), size=0.4, show.legend=FALSE)+
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

figS8<-plot_grid(
plot_grid(bplot2, effplot+theme(axis.line=element_line(size=0.25)), labels=c("A", "B")),
plot_grid(f1, x1, p2, nrow=1, rel_widths=c(1,1,2), labels=c("C", "D","E")),ncol=1)
figS8
#   ggsave("figs/fig2code.jpg", fig2, height=6.2, width=5.3)



