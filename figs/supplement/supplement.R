
setwd("~/Documents/PostDoc/09 bleaching/bleaching2024")
source("03 analysis.R")

#---------------------------------------------# FIGURE S1  - Acropora change by region

compS1 <- all[all$zone=="Crest",]
compS1 <- aggregate(cov~align+ID+site+reef+region+t+tlab+ntimes, compS1, sum)

removeR <- c("Coral Sea", "Hydrographers Passage")

compS1R <- aggregate(cov~align+reef+region+t+tlab+ntimes+region+site+ID, compS1[compS1$ntimes %in% c(2,3),], mean)
compS1R <- compS1R[!compS1$region %in% removeR, ]
compS1R$region <- factor(compS1R$region, levels=rev(regions))
compS1R$tlab <- factor(compS1R$tlab, levels=c("2016a", "2016b", "2024"))

compS1.ac <- aggregate(cov~tlab+reef+site+ID+ntimes+region+site+ID+t, compS1[compS1$align %in% c("tabular_Acropora", "staghorn_Acropora", "other_Acropora"),], sum)
compS1.acR <- aggregate(cov~tlab+reef+region+site+ID+t, compS1.ac[compS1.ac$ntimes %in% c(2,3),], mean)
compS1.acR$tlab <- factor(compS1.acR$tlab, levels=c("2016a", "2016b", "2024"))
compS1.acR$region <- factor(compS1.acR$region, levels=rev(regions))

head(comp1)
unique(compS1$align)
covs <- compS1[!c(is.na(compS1$align) | compS1$align=="soft"),]
unique(covs$align)
covS1 <-  aggregate(cov~tlab+reef+site+ID+ntimes+region+site+ID+t, covs, sum)
covS1[covS1$cov>100,]
covS1R <- aggregate(cov~tlab+reef+region+site+t, covS1[covS1$ntimes %in% c(2,3),], mean)
covS1R$tlab <- factor(covS1R$tlab, levels=c("2016a", "2016b", "2024"))
covS1R$region <- factor(covS1R$region, levels=rev(regions))
head(covS1)

tab_regions <- ggplot(compS1R[compS1R$align %in% c("tabular_Acropora"),])+
geom_boxplot(aes(x=tlab, y=cov, fill=tlab), outlier.size=0.1, size=0.1, alpha=0.5)+
stat_summary(aes(x=tlab, y=cov, group=1), geom="line")+
stat_summary(aes(x=tlab, y=cov, fill=tlab), size=0.55, shape=21, stroke=0.3)+
facet_wrap(~region, nrow=1, dir="v", strip.position="right")+
scale_fill_manual(values=c("grey", "red", "black"))+
guides(fill="none")+
scale_y_sqrt()+
ylab("% tabular Acropora")+
theme_classic()+theme(axis.title.x=element_blank(), axis.line=element_line(size=0.2),axis.title.y=element_text(size=8), strip.background=element_blank(), strip.text=element_text(size=8), axis.text.x=element_text(size=8, angle=45, hjust=1))
tab_regions

acr_regions <- ggplot(compS1.acR)+
geom_boxplot(aes(x=tlab, y=cov, fill=tlab), outlier.size=0.1, size=0.1, alpha=0.5)+
stat_summary(aes(x=tlab, y=cov, group=1), geom="line")+
stat_summary(aes(x=tlab, y=cov, fill=tlab), size=0.55, shape=21, stroke=0.3)+
facet_wrap(~region, nrow=1, strip.position="right")+
scale_fill_manual(values=c("grey", "red", "black"))+
guides(fill="none")+
scale_y_sqrt()+
ylab("% Acropora")+
theme_classic()+theme(axis.title.x=element_blank(), axis.line=element_line(size=0.2),axis.title.y=element_text(size=8), strip.background=element_blank(), strip.text=element_text(size=8), axis.text.x=element_text(size=8, angle=45, hjust=1))

cov_regions <- ggplot(covS1R)+
geom_boxplot(aes(x=tlab, y=cov, fill=tlab), outlier.size=0.1, size=0.1, alpha=0.5)+
stat_summary(aes(x=tlab, y=cov, group=1), geom="line")+
stat_summary(aes(x=tlab, y=cov, fill=tlab), size=0.55, shape=21, stroke=0.3)+
facet_wrap(~region, nrow=1, strip.position="right")+
scale_fill_manual(values=c("grey", "red", "black"))+
guides(fill="none")+
scale_y_sqrt()+
ylab("% coral cover")+
theme_classic()+theme(axis.title.x=element_blank(), axis.line=element_line(size=0.2),axis.title.y=element_text(size=8), strip.background=element_blank(), strip.text=element_text(size=8), axis.text.x=element_text(size=8, angle=45, hjust=1))


figS1 <- plot_grid(tab_regions, acr_regions, cov_regions, ncol=1, labels=c("A", "B", "C"), label_size=9)
figS1 

# check high cov... 

#     ggsave( "figs/supplement/figS1.jpg",figS1, height=7, width=6)

#---------------------------------------------# FIGURE S2  - nmds of revisited reefs

head(mdspoints)

mdsplotS2 <- ggplot()+
lims(x=xlims, y=ylims)+
geom_path(data=mdspoints[mdspoints$ntimes %in% c(3) & mdspoints$t %in% c("Apr16", "Oct16"),], aes(x=NMDS1, y=NMDS2, group=Reef), col="grey", arrow=arrow(length=unit(0.5, "mm")), size=0.4)+
geom_path(data=mdspoints[mdspoints$ntimes %in% c(3) & mdspoints$t %in% c("Oct16", "Mar24"),], aes(x=NMDS1, y=NMDS2, group=Reef), col="black", arrow=arrow(length=unit(0.5, "mm"), ends="first"), size=0.4)+
theme_bw()+theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank() )
mdsplotS2

figS2 <- plot_grid(mdsplotS2, vecplot)
figS2

#     ggsave( "figs/supplement/figS2.jpg",figS2, height=2.7, width=6)


#---------------------------------------------# FIGURE S3  - additional maps 

# see maps.R


#---------------------------------------------# FIGURE S4  - crest/zone



zones1 <- ggplot(sites, aes(x=max.dhw, y=pbleach*100, col=Zone))+
geom_point(shape=21, size=0.5)+
geom_line(data=curves[curves$data=="sites",], aes(x=predictor, y=betafit*100))+
scale_colour_manual(values=c("grey", "black"))+
geom_hline(yintercept=30, linetype="dotted")+
scale_x_log10()+
labs(x="Degree Heating Weeks", y="% bleaching")+
theme_classic()+theme(legend.title=element_blank())
zones1

sites$sev01 <- ifelse(sites$pbleach>0.3, 1,0)
slopedat <- sites[sites$Zone=="Slope",]
slopeglm <- glm(sev01 ~ max.dhw, family="binomial", data=slopedat) 
slopefit <- data.frame(max.dhw=seq(min(slopedat$max.dhw), max(slopedat$max.dhw), 0.1))
slopefit$pred <- predict(slopeglm, slopefit, type="response")
slopefit$se <- predict(slopeglm, slopefit, type="response", se=T)$se

zoneglm <- rbind(cbind(crestfit, Zone="Crest"), cbind(slopefit, Zone="Slope"))
head(zoneglm)

zones2 <- ggplot()+
geom_line(data=zoneglm, aes(x=max.dhw, y=pred, col=Zone))+
geom_ribbon(data=zoneglm, aes(x=max.dhw, ymin=pred-se, ymax=pred+se, fill=Zone), alpha=0.35)+
scale_colour_manual(values=c("grey", "black"))+scale_fill_manual(values=c("grey", "black"))+
labs(x="Degree Heating Weeks", y="Probability of severe\nreef bleaching")+
theme_classic()+theme(legend.title=element_blank())


betamod4 = betareg(pdead ~ max.dhw, data=slopedat)
fit.dat4 <- data.frame(max.dhw = seq(min(slopedat$max.dhw), max(slopedat$max.dhw), 0.1))
fit.dat4$fit <- predict(betamod4, fit.dat4)
summary(betamod4)

zonesbeta <- rbind(cbind(fit.dat4, Zone="Slope"),cbind(fit.dat2, Zone="Crest"))
zones4 <- ggplot()+
geom_point(data=sites, aes(x=max.dhw, y=pdead, col=Zone), size=0.5, shape=21)+
scale_x_log10()+
geom_line(data=zonesbeta, aes(x=max.dhw, y=fit, col=Zone))+
scale_colour_manual(values=c("grey", "black"))+
labs(x="Degree Heating Weeks", y="% recent mortality")+
theme_classic()+theme(legend.title=element_blank())
zones4


figS4 <- plot_grid(zones1+guides(col="none"), 
zones2+guides(col="none", fill="none"), 
zones4+guides(col="none"),
get_legend(zones2),
nrow=2,  labels=c("A", "B", "C", ""), align="hv", label_size=9)
figS4

#     ggsave( "figs/supplement/figS4.jpg",figS4, height=6, width=6)



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



#---------------------------------------------# FIGURE S6 recreate Hughes 2021

tph <- read.csv("data/original/Hughes2021.csv")
head(tph)
nrow(tph)
unique(tph$year)

# (0) < 1% of corals bleached, (1) 1%–10%, (2) 10%–30%, (3) 30%–60%, and (4) > 60% of corals bleached.9 
# cat 3 nd 4 used in this analysis.. >30% severe bl. 

tphmods <- NULL
for(i in unique(tph$year)){
	#i <- 1998
	sub <- tph[tph$year == i,]
	r.mod <- glm(bin.score ~ DHW, family="binomial", data=sub) 
	new<-data.frame(DHW=seq(min(sub$DHW), max(sub$DHW), 0.1), year=i)
	new$pred<-predict(r.mod, new, type="response")
	new$se<-predict(r.mod, new, type="response", se=T)$se
	tphmods <- rbind(tphmods, new)
}

sites$sev01 <- ifelse(sites$pbleach>0.3, 1,0)
crestdat <- sites[sites$Zone=="Crest",]
crestglm <- glm(sev01 ~ max.dhw, family="binomial", data=crestdat) 
crestfit <- data.frame(max.dhw=seq(min(crestdat$max.dhw), max(crestdat$max.dhw), 0.1))
crestfit$pred <- predict(crestglm, crestfit, type="response")
crestfit$se <- predict(crestglm, crestfit, type="response", se=T)$se

fit.all <- rbind(tphmods, data.frame(DHW=crestfit$max.dhw, year=2024, pred=crestfit$pred, se=crestfit$se))

fit.all$type <- ifelse(fit.all$year==2024, "in-water", "aerial")

figS5 <- ggplot()+
geom_line(data=fit.all[!fit.all$year %in% c(1998, 2002),], aes(x=DHW, y=pred, linetype=type, group=as.factor(year)), size=0.3)+
geom_ribbon(data=fit.all[!fit.all$year %in% c(1998, 2002),], aes(x=DHW, ymin=pred+se*1.96, ymax=pred-se*1.96,  fill=as.factor(year), col=as.factor(year)),  alpha=0.35, size=0.1)+
scale_linetype_manual(values=c("solid","dashed"))+
labs(x="Degree Heating Weeks", y="Probablity of severe\nreef bleaching")+
scale_fill_viridis(discrete=T, option="C")+scale_colour_viridis(discrete=T)+
theme_classic()+theme(legend.title=element_blank(), legend.key.height=unit(1, "mm"))
figS5

#     ggsave( "figs/supplement/figS5.jpg",figS5, height=3.5, width=4)


#---------------------------------------------# FIGURE S7 residual plots 

sres$Region <- factor(sres$Region, levels=rev(regions))

resid1 <- ggplot(sres[sres$Zone=="Crest",], aes(betamod, reorder(Reef, -GPS.S)))+
geom_vline(xintercept=0)+
geom_boxplot(aes(fill=Region), size=0.1, outlier.size=0.1)+
#scale_colour_manual(values=c("grey", "black"))+
facet_wrap(~Region, ncol=1, scales="free_y", strip.position="left")+
scale_fill_viridis(discrete=T, direction=-1)+
guides(fill="none")+
labs(x="Deviation from expected\nbleaching (2024)")+
theme_classic()+theme(axis.line.y=element_blank(), 
strip.background=element_blank(), 
strip.text.y.left=element_text(size=8, angle=0, hjust=1),
 axis.text.y=element_blank(), axis.ticks=element_blank(),axis.title.x=element_text(size=9), panel.margin.y=unit(2, "mm"), panel.background=element_rect(fill="grey96"), axis.title.y=element_blank())


avline <- mean(sres[sres$Zone=="Crest","acro"], na.rm=T)
resid2 <- ggplot(sres[sres$Zone=="Crest",], aes(acro, reorder(Reef, -GPS.S)))+
geom_vline(xintercept=avline)+
scale_fill_viridis(discrete=T, direction=-1)+
guides(fill="none")+
geom_boxplot(aes(fill=Region), size=0.1, outlier.size=0.1)+
#scale_colour_manual(values=c("grey", "black"))+
labs(x="% Acropora cover\n(2024)")+
facet_wrap(~Region, ncol=1, scales="free_y", strip.position="left")+
theme_classic()+theme(axis.line.y=element_blank(), 
strip.background=element_blank(), 
strip.text.y.left=element_text(size=8, angle=0, hjust=1),
 axis.text.y=element_blank(), axis.ticks=element_blank(),axis.title.x=element_text(size=9), panel.margin.y=unit(2, "mm"), panel.background=element_rect(fill="grey96"), axis.title.y=element_blank())


figS7 <- plot_grid(resid1, resid2, labels=c("A", "B"), label_size=9)
figS7

#     ggsave( "figs/supplement/figS7.jpg",figS7, height=4, width=6.5)



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

figS8 <- plot_grid(fS1, lS1, fS2, lS2, fS3, lS3, labels=c("A", "B", "C", "D", "E"), label_size=9, nrow=3, align="hv", axis="lr")
figS8


#     ggsave( "figs/supplement/figS8.jpg",figS8, height=7, width=5.5)







