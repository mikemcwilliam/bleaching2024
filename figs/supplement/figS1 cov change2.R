

all2 <- read.csv("data/composition.csv")
all2 <- all2[!all2$t == "Oct24",]
unique(all2$t)

# transect level sums
head(all2)
unique(all2$align)


all2$region3 <- ifelse(all2$region2 %in% "Princess Charlotte Bay", "Prin. Char. Bay",all2$region2)

regions3 <- c("Cape Grenville", "Prin. Char. Bay", "Cooktown", "Cairns", "Townsville", "Mackay", "Gladstone")

all2$region3 <- factor(all2$region3 , levels=regions3)



comp2 <- all2[all2$zone %in% "Crest",]

comp2$align2 <- comp2$align#ifelse(comp2$align %in% c("Faviidae", "Mussidae"), "Other massive", comp2$align)
head(comp2)

#comp2 <- comp2[!comp2$t %in% "Oct16",]

#comp2 <- comp2[!comp2$align %in% "soft",]

comp2 <- aggregate(cov~align2+ID+site+reef+region3+t+tlab+ntimes, comp2, sum)

#comp21 <- aggregate(cov~align+reef+region3+t+tlab+ntimes, comp2[comp2$ntimes %in% c(1, 2,3) & comp2$t %in% c("Apr16", "Oct16"),], sum)
#comp22 <- aggregate(cov~align+reef+region3+t+tlab+ntimes, comp2[comp2$ntimes %in% c(1, 2,3) & comp2$t %in% c("Apr24"),], mean)
#comp2R <- rbind(comp21, comp22)
#head(comp2R)

# site level 
 comp2R <- aggregate(cov~align2+reef+region3+t+tlab+ntimes, comp2[comp2$ntimes %in% c(1,2,3,4),], mean)

hist(comp2R$cov)
unique(comp2R$ntimes)
head(comp2R)
unique(comp2R$t)


comp2R$ID <- paste(comp2R$reef, comp2R$t)
comp2R <- comp2R[!comp2R$region3 %in% removeR, ]
comp2R <- comp2R[!(comp2R$t=="Apr16" & comp2R$ntimes==1), ]
comp2R <- comp2R[!comp2R$reef=="NoName",]
head(comp2R)


comp2Rb <- comp2R
comp2Rb$t <- factor(comp2Rb$t, levels=c("Apr16", "Oct16", "Apr24"))
comp2Rb$align[comp2Rb$align=="Faviidae"] <- "Merulinidiae"
comp2Rb$align[comp2Rb$align=="Mussidae"] <- "Lobophyllidae"
totcov <- aggregate(cov~reef+t, comp2Rb, sum)
comp2Rb$tot <- totcov$cov[match(paste(comp2Rb$t, comp2Rb$reef), paste(totcov$t, totcov$reef))]
comp2Rb$rcov <- comp2Rb$cov/comp2Rb$tot


comp2R$t <- factor(comp2R$t, levels=c("Apr16", "Oct16", "Apr24", "Oct24"))
head(comp2Rb)



#comp2Rb$region <- factor(comp2Rb$region, levels=rev(regions))

#rcols <- c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695")

rcols <- c("#a50026",  "#f46d43", "darkgreen", "green",  "#abd9e9", "#4575b4", "#313695")

#[comp2Rb$ntimes %in% c(3,4),]
head(comp2Rb)

[comp2Rb$ntimes %in% c(3,4),]

#[comp2Rb$ntimes %in% c(3,4),]

plotdatX1 <- comp2Rb#[comp2Rb$tlab %in% c("2024a", "2016a"),]
plotdatX1 <- plotdatX1[!plotdatX1$align2 %in% c("soft", "other_scleractinians"),]

compplotX <- ggplot(plotdatX1, aes(t,rcov, col=region3))+
stat_summary(aes( group=region3), size=0.1)+
stat_summary(aes( group=region3),  geom="line")+
facet_wrap(~align2, scale="free_y", nrow=2)+
theme_classic()+
scale_colour_manual(values=rcols)+
theme(strip.background=element_blank(), legend.title=element_blank(), axis.text=element_text(size=7))+
labs(x="timepoint", y="relative cover")
compplotX

unique(comp2Rb$align2)
tcols <- c("#bf812d", "#74add1", "#66bd63", "#dfc27d", "#c7eae5", "grey", "#c51b7d", "#8c510a", "#de77ae", "#fee090", "#92c5de", "#f1b6da", "#2166ac")
names(tcols) <- unique(comp2Rb$align2)
comp2Rb$align2 <- factor(comp2Rb$align2, levels=c("Poritidae" ,"Faviidae" , "Mussidae"  ,         "other_scleractinians",  "Isopora" ,"Montipora",      "Pocillopora","Seriatopora" ,"Stylophora", "soft" ,  "other_Acropora", "staghorn_Acropora",  "tabular_Acropora"  ))

#plotdatX <- comp2Rb[comp2Rb$ntimes %in% c(2, 3,4) & comp2Rb$tlab %in% c("2016a", "2024a"),]
plotdatX <- comp2Rb[comp2Rb$ntimes %in% c(2, 3,4) ,]
plotdatX$cov[plotdatX$region3 %in% "Gladstone" & plotdatX$tlab %in% "2016b"]<-plotdatX$cov[plotdatX$region3 %in% "Gladstone" & plotdatX$tlab %in% "2016b"]/2

compplot1 <- ggplot(plotdatX, aes(t,cov, fill=align2))+
#stat_summary(col="grey")+
stat_summary(aes( group=align2),  geom="area", size=1, position="stack")+
facet_wrap(~region3,  nrow=1, scales="free")+
theme_classic()+
scale_fill_manual(values=tcols)+
scale_y_continuous(expand=c(0,0))+scale_x_discrete(expand=c(0,0))+
theme(strip.background=element_blank(), legend.title=element_blank(), legend.key.width=unit(2, "mm"), legend.key.height=unit(2, "mm"), axis.text.x=element_text(angle=30,hjust=1), legend.position="top")+
labs(x="timepoint", y="% cover")
compplot1


covs <- aggregate(cov~reef+region3+t+tlab+ntimes, comp2Rb[!comp2Rb$align2 %in% "soft",], sum)
#covs <- aggregate(cov~reef+region+t+tlab+ntimes, comp2Rb[comp2Rb$align2 %in% "Massives",], sum)


#[covs$ntimes %in% c(2,3,4),]
covs$cov[covs$region3 %in% "Gladstone" & covs$tlab %in% "2016b"] <-covs$cov[covs$region3 %in% "Gladstone" & covs$tlab %in% "2016b"]/2

compplot2 <- ggplot(covs, aes(t,cov, col=region3))+
stat_summary(aes(group=reef), size=0.1, col="grey")+
#stat_summary(data=covs[covs$ntimes %in% c(3,4),], aes(t,cov, group=region), col="grey")+
stat_summary(aes(col=region3, group=region3))+
stat_summary(aes(col=region3, group=region3),  geom="line")+
facet_wrap(~region3, nrow=1, scales="free")+
theme_classic()+
guides(col="none")+
scale_colour_manual(values=rcols)+
theme(strip.background=element_blank(), axis.text.x=element_text(angle=30,hjust=1))+labs(x="timepoint", y="Coral cover (%)")
compplot2 


plot_grid(plot_grid(compplot1, compplot2, ncol=1, labels=c("A", "B"), rel_heights=c(1.5,1)), compplotX+guides(col="none"), rel_widths=c(2,1), labels=c("", "C"), nrow=1)

head(comp2Rb)


plot_grid(compplot1+guides(fill="none"), compplot2, compplotX+guides(col="none"), ncol=1)







