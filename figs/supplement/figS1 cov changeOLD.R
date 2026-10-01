

regions <- c("Capricorn Bunkers", "Hydrographers Passage", "Cape Bowling Green", "Cairns", "Lizard", "Princess Charlotte Bay", "Cape Grenville")

#---------------------------------------------# FIGURE S1  - Acropora change by region

compS1 <- all[all$zone=="Crest",]
head(compS1)


test <- compS1
unique(test[,c("taxa", "align")])

unique(test$align)

unique(test$taxa[is.na(test$align)])


test <- test[test$align %in% c("Faviidae","Isopora","Montipora","Mussidae" , "other_Acropora"  ,     "other_scleractinians" ,"Pocillopora", "Poritidae", "Seriatopora", "staghorn_Acropora", "Stylophora", "tabular_Acropora"),]

test[test$reef=="Dugong",] # completely different to data in APRIL

unique(test$taxa[is.na(test$align)])

unique(test$ID)[order(unique(test$ID))]
unique(test$t)
unique(test$tlab)
nrow(test)
test <- aggregate(cov~align+ID+site+reef+region+t+tlab, test, sum)
nrow(test)
test <- aggregate(cov~align+site+reef+region+t+tlab, test, mean)
test <- aggregate(cov~align+reef+region+t+tlab, test, mean)
nrow(test)
head(test)

removeR <- c("Coral Sea", "Hydrographers Passage")
test <- test[!test$region %in% removeR, ]

cov1 <- aggregate(cov~reef+region+t+tlab, test, sum)

ggplot(cov1, aes(tlab, cov, fill=tlab))+geom_boxplot()+
facet_wrap(~region, scales="free_y", ncol=1)+
ylab("% coral cover")+
scale_fill_manual(values=c("grey", "red", "black"))+guides(fill="none")+
theme_classic()+theme(axis.title.x=element_blank(), axis.line=element_line(size=0.2),axis.title.y=element_text(size=8), strip.background=element_blank(), strip.text=element_text(size=8), axis.text.x=element_text(size=8, angle=45, hjust=1))


cov1[cov1$cov>100,]


#---------------------------------------------# FIGURE S1  - Acropora change by region


unique(compS1$reef)

#compS1 <- compS1[!compS1$reef %in% c("GreatDetached"),]

compS1 <- aggregate(cov~align+ID+site+reef+region+t+tlab+ntimes, compS1, sum)

removeR <- c("Coral Sea", "Hydrographers Passage")
compS1 <- compS1[!compS1$region %in% removeR, ]

compS1$SID <- paste(compS1$reef, compS1$site, compS1$zone, compS1$t)

# [compS1$ntimes %in% c(2,3),]
compS1R <- aggregate(cov~align+reef+region+t+tlab+ntimes+region+site+SID, compS1[compS1$ntimes %in% c(2,3,4),], mean)
compS1R$region <- factor(compS1R$region, levels=rev(regions))
compS1R$tlab <- factor(compS1R$tlab, levels=c("2016a", "2016b", "2024a"))
unique(compS1R$region )

#[covS1$ntimes %in% c(2,3),]
head(comp1)

compS1[compS1$SID=="Wistari Wistari_B1  Oct16",]

unique(compS1$align)
covs <- compS1[!c(is.na(compS1$align) | compS1$align=="soft"),]
unique(covs$align)

covS1 <-  aggregate(cov~tlab+reef+site+ID+ntimes+region+site+ID+t+SID, covs, sum)
covS1[covS1$cov>100,]

covS1R <- aggregate(cov~tlab+reef+region+site+t+SID, covS1[covS1$ntimes %in% c(2,3,4),], mean)
covS1R$tlab <- factor(covS1R$tlab, levels=c("2016a", "2016b", "2024a"))
covS1R$region <- factor(covS1R$region, levels=rev(regions))
head(covS1R)

covS1R

#[compS1.ac$ntimes %in% c(2,3),]
compS1.ac <- aggregate(cov~tlab+reef+site+ntimes+region+site+ID+t+SID, compS1[compS1$align %in% c("tabular_Acropora", "staghorn_Acropora", "other_Acropora"),], sum)
compS1.acR <- aggregate(cov~tlab+reef+region+site+SID+t, compS1.ac[compS1.ac$ntimes %in% c(2,3,4),], mean)
compS1.acR$tlab <- factor(compS1.acR$tlab, levels=c("2016a", "2016b", "2024a"))
compS1.acR$region <- factor(compS1.acR$region, levels=rev(regions))
compS1.acR$tcov <- covS1R$cov[match(compS1.acR$SID, covS1R$SID)]

unique(compS1$align )
#,"Mussidae","Faviidae"
compS1.po <- aggregate(cov~tlab+reef+site+ID+ntimes+region+site+ID+t+SID, compS1[compS1$align %in% c("Poritidae"),], sum)
compS1.poR <- aggregate(cov~tlab+reef+region+site+SID+t, compS1.po, mean)
compS1.poR$tlab <- factor(compS1.poR$tlab, levels=c("2016a", "2016b", "2024a"))
compS1.poR$region <- factor(compS1.poR$region, levels=rev(regions))
compS1.poR$tcov <- covS1R$cov[match(compS1.poR$SID, covS1R$SID)]

ggplot(compS1.poR)+
geom_boxplot(aes(x=tlab, y=cov/tcov, fill=tlab), outlier.size=0.1, size=0.1, alpha=0.5)+
stat_summary(aes(x=tlab, y=cov/tcov, group=1), geom="line")+
stat_summary(aes(x=tlab, y=cov/tcov, fill=tlab), size=0.55, shape=21, stroke=0.3)+
facet_wrap(~region, nrow=1, strip.position="right")+
scale_fill_manual(values=c("grey", "red", "black"))+
scale_y_sqrt()



tabdat <- compS1R[compS1R$align %in% c("tabular_Acropora"),]
tabdat$tcov <- covS1R$cov[match(tabdat$SID, covS1R$SID)]

tab_regions <- ggplot(tabdat)+
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
acr_regions

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
cov_regions

figS1 <- plot_grid(tab_regions, acr_regions, cov_regions, ncol=1, labels=c("A", "B", "C"), label_size=9)
figS1 

# check high cov... 

#     ggsave( "figs/supplement/figS1.jpg",figS1, height=7, width=6)

