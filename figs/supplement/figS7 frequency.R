




#---------------------------------------------# FIGURE S8 extended frequenciy/recovery analysis


lastcols <- rev(scales::viridis_pal()(4))
lastcols


fS1 <- ggplot()+
geom_boxplot(data=sres, aes(as.factor(freq2), betamod, fill=as.factor(freq2)), outlier.size=0.05, size=0.1,position = position_dodge2(preserve = "single"), alpha=0.25)+
stat_summary(data=tres, aes(as.factor(freq2), betamod, group=as.factor(n)), geom="line")+
stat_summary(data=tres, aes(as.factor(freq2), betamod, fill=as.factor(freq2)),shape=21, stroke=0.21)+
facet_wrap(~Zone)+
coord_cartesian(xlim=c(1,5.7))+
guides(fill="none")+
scale_fill_manual(values=c("grey", "#fecc5c", "#fd8d3c", "#e31a1c", "#500000"))+
labs(x="N events > 6 DHW\n(2016-2023)", y="Bleaching residuals\n(quasibinomial)")+
theme_classic()+theme
fS1


lS1 <- ggplot()+
geom_boxplot(data=sres, aes(as.factor(last2), betamod, fill=as.factor(last2)), outlier.size=0.05, size=0.1,position = position_dodge2(preserve = "single"), alpha=0.25)+
stat_summary(data=tres, aes(as.factor(last2), betamod, group=as.factor(n)), geom="line")+
stat_summary(data=tres, aes(as.factor(last2), betamod, fill=as.factor(last2)),shape=21, stroke=0.21)+
facet_wrap(~Zone)+
coord_cartesian(xlim=c(1,5.7))+
guides(fill="none")+
scale_fill_manual(values=c(lastcols[1:3], "grey"))+
labs(x="Years since last\nevent > 6 DHW", y="Bleaching residuals\n(quasibinomial)")+
theme_classic()+theme
lS1

fS2 <- ggplot()+
geom_boxplot(data=sres, aes(as.factor(freq2), gam.5, fill=as.factor(freq2)), outlier.size=0.05, size=0.1,position = position_dodge2(preserve = "single"), alpha=0.25)+
stat_summary(data=tres, aes(as.factor(freq2), gam.5, group=as.factor(n)), geom="line")+
stat_summary(data=tres, aes(as.factor(freq2), gam.5, fill=as.factor(freq2)),shape=21, stroke=0.21)+
facet_wrap(~Zone)+
coord_cartesian(xlim=c(1,5.7))+
guides(fill="none")+
scale_fill_manual(values=c("grey", "#fecc5c", "#fd8d3c", "#e31a1c", "#500000"))+
labs(x="N events > 6 DHW\n(2016-2023)", y="Bleaching residuals\n(GAM)")+
theme_classic()+theme
fS2

lS2 <- ggplot()+
geom_boxplot(data=sres, aes(as.factor(last2), gam.5, fill=as.factor(last2)), outlier.size=0.05, size=0.1,position = position_dodge2(preserve = "single"), alpha=0.25)+
stat_summary(data=tres, aes(as.factor(last2), gam.5, group=as.factor(n)), geom="line")+
stat_summary(data=tres, aes(as.factor(last2), gam.5, fill=as.factor(last2)),shape=21, stroke=0.21)+
facet_wrap(~Zone)+
coord_cartesian(xlim=c(1,5.7))+
guides(fill="none")+
scale_fill_manual(values=c(lastcols[1:3], "grey"))+
labs(x="Years since last\nevent > 6 DHW", y="Bleaching residuals\n(GAM)")+
theme_classic()+theme
lS2


fS3 <- ggplot()+
geom_boxplot(data=sres, aes(as.factor(freq2), tab, fill=as.factor(freq2)), outlier.size=0.05, size=0.1,position = position_dodge2(preserve = "single"), alpha=0.25)+
stat_summary(data=tres, aes(as.factor(freq2), tab, group=as.factor(n)), geom="line")+
stat_summary(data=tres, aes(as.factor(freq2), tab, fill=as.factor(freq2)),shape=21, stroke=0.21)+
facet_wrap(~Zone)+
coord_cartesian(xlim=c(1,5.7))+
guides(fill="none")+
scale_fill_manual(values=c("grey", "#fecc5c", "#fd8d3c", "#e31a1c", "#500000"))+
labs(x="N events > 6 DHW\n(2016-2023)", y="% Acropora")+
theme_classic()+theme
fS3

lS3 <- ggplot()+
geom_boxplot(data=sres, aes(as.factor(last2), tab, fill=as.factor(last2)), outlier.size=0.05, size=0.1,position = position_dodge2(preserve = "single"), alpha=0.25)+
stat_summary(data=tres, aes(as.factor(last2), tab, group=as.factor(n)), geom="line")+
stat_summary(data=tres, aes(as.factor(last2), tab, fill=as.factor(last2)),shape=21, stroke=0.21)+
facet_wrap(~Zone)+
coord_cartesian(xlim=c(1,5.7))+
guides(fill="none")+
scale_fill_manual(values=c(lastcols[1:3], "grey"))+
labs(x="Years since last\nevent > 6 DHW", y="% tabular Acropora")+
theme_classic()+theme
lS3

figS7 <- plot_grid(fS1, lS1, fS2, lS2, fS3, lS3, labels=c("A", "B", "C", "D", "E"), label_size=9, nrow=3, align="hv", axis="lr")
figS7


#     ggsave( "figs/supplement/figS8.jpg",figS8, height=7, width=5.5)


