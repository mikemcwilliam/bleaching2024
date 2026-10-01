
regions2 <- c("Capricorn Bunkers", "Hydrographers Passage", "Cape Cleveland", "Cairns", "Lizard", "Princess Charlotte Bay", "Cape Grenville")

rnames <- read.csv("data/data2016/reefnames.csv")
head(rnames)

chA <- read.csv("data/data2016/Data_Global_warming_transforms_coral_reef_assemblages/TaxonomicDataA.csv")[,c(1:16)]
chO <- read.csv("data/data2016/Data_Global_warming_transforms_coral_reef_assemblages/TaxonomicDataO.csv")[,c(1:16)]
nrow(chA)
nrow(chO)
chk <- rbind(cbind(chA, month="Apr16"), cbind(chO, month="Oct16"))
chk <- melt(chk, id.var=c("ReefID", "month"))
head(chk)
chk$variable <- ifelse(chk$variable=="Other.Acropora", "other_Acropora", as.character(chk$variable))
chk$variable <- ifelse(chk$variable=="Tabular.Acropora", "tabular_Acropora", as.character(chk$variable))
chk$variable <- ifelse(chk$variable=="Other.Scleractinia", "other_scleractinians", as.character(chk$variable))
chk$variable <- ifelse(chk$variable=="Staghorn.Acropora", "staghorn_Acropora", as.character(chk$variable))
head(chk)
unique(chk$variable)


chk$region <- rnames$region[match(chk$ReefID, rnames$ReefNo)]
chk$region[chk$region=="Cape Bowling Green"]<- "Cape Cleveland"
chk$reef <- rnames$use[match(chk$ReefID, rnames$ReefNo)]
unique(chk$reef)
unique(chk$region)
chk[is.na(chk$region),]
# 11-191 and 16-014 no match (one torres strait one cairns)

chk <- chk[!is.na(chk$region),]
head(chk) 

chk[chk$region%in% "Capricorn Bunkers" & chk$month %in% "Oct16",]
chk[chk$reef%in% "Wistari" & chk$month %in% "Oct16",]
chk[chk$reef%in% "Wistari" & chk$month %in% "Apr16",]


unique(chk$variable)

head(sites)

sites[sites$Reef == "Wilson",]
#chk[chk$reef%in% "Wilson",]

new <- sites#[sites$Zone=="Crest",]
new <- aggregate(list(tab=new$tab, coral_cov=new$coral_cov, acro=new$acro), by=list(Reef=new$Reef, Region=new$Region), mean)


head(new)


new <- new[!new$Region=="Hydrographers Passage",]
new$Reef[new$Reef=="North Direction"]<- "NthDirection"
new$Reef[new$Reef=="Ribbon 8"]<- "Ribbon8"
new$Reef[new$Reef=="Three Reefs"]<- "GreatDetached"
head(new)


#, "Soft.corals" [!chk$variable %in% c("Other.sessile.fauna"),]
cov <- aggregate(value~reef+region+month, chk, sum)
covb <- rbind(cov, data.frame(reef=new$Reef, region=new$Region, month="Apr24", value=new$coral_cov))
covb$tlab <- ifelse(covb$month=="Apr16", "2016a", ifelse(covb$month=="Apr24", "2024a", ifelse(covb$month=="Oct16", "2016b", NA)))
covb$tlab <- factor(covb$tlab, levels=c("2016a", "2016b", "2024a"))
covb$region <- factor(covb$region, levels=rev(regions2))
head(covb)
covb$value <- ifelse(covb$value>100, 100, covb$value)

keep <- rnames$use[!is.na(rnames$r24)]
keep <- keep[!keep %in% "Wilson"]
head(covb)
keep
unique(new$Reef)

revisit <- covb[covb$reef %in% keep, ]
table(revisit$reef)


covplot <- ggplot()+
geom_boxplot(data=covb, aes(x=tlab, y=value, fill=tlab), outlier.size=0.1, size=0.1, alpha=0.5)+
geom_point(data=covb, aes(x=tlab, y=value), size=0.25)+
#stat_summary(data=revisit, aes(x=tlab, y=value, group=1), geom="line")+
#stat_summary(data=revisit, aes(x=tlab, y=value, fill=tlab), size=0.55, shape=21, stroke=0.3)+
geom_line(data=revisit, aes(x=tlab, y=value, group=reef), col="slategrey", size=0.25)+
#geom_point(data=covb[covb$reef %in% keep, ], aes(x=tlab, y=value,  fill=tlab), shape=21)+
facet_wrap(~region, nrow=1, strip.position="top")+
scale_fill_manual(values=c("grey", "red", "black"))+
guides(fill="none")+
scale_y_sqrt()+
#scale_y_log10()+
ylab("% coral cover")+
theme_classic()+theme(axis.title.x=element_blank(), axis.line=element_line(size=0.2),axis.title.y=element_text(size=8), strip.background=element_blank(), strip.text=element_text(size=8), axis.text.x=element_text(size=8, angle=45, hjust=1))
covplot


acr <- aggregate(value~reef+region+month, chk[chk$variable %in% c("tabular_Acropora", "staghorn_Acropora", "other_Acropora"),], sum)
head(acr)
acrb <- rbind(acr, data.frame(reef=new$Reef, region=new$Region, month="Apr24", value=new$acro))
acrb$tlab <- ifelse(acrb$month=="Apr16", "2016a", ifelse(acrb$month=="Apr24", "2024a", ifelse(acrb$month=="Oct16", "2016b", NA)))
acrb$tlab <- factor(acrb$tlab, levels=c("2016a", "2016b", "2024a"))
acrb$region <- factor(acrb$region, levels=rev(regions2))
head(acrb)

acrb$ID <- paste(acrb$reef, acrb$month)
covb$ID <- paste(covb$reef, covb$month)
acrb$tcov <- covb$value[match(acrb$ID, covb$ID)]

#acrb$value <- acrb$value/acrb$tcov ### RELATIVE OR ABSOLUTE

revisit2 <- acrb[acrb$reef %in% keep, ]
table(revisit2$reef)


acrplot <- ggplot()+
geom_boxplot(data=acrb, aes(x=tlab, y=value, fill=tlab), outlier.size=0.1, size=0.1, alpha=0.5)+
geom_point(data=acrb, aes(x=tlab, y=value), size=0.25)+
#stat_summary(data=revisit, aes(x=tlab, y=value, group=1), geom="line")+
#stat_summary(data=revisit, aes(x=tlab, y=value, fill=tlab), size=0.55, shape=21, stroke=0.3)+
geom_line(data=revisit2, aes(x=tlab, y=value, group=reef), col="slategrey", size=0.25)+
#geom_point(data=covb[covb$reef %in% keep, ], aes(x=tlab, y=value,  fill=tlab), shape=21)+
facet_wrap(~region, nrow=1, strip.position="top")+
scale_fill_manual(values=c("grey", "red", "black"))+
guides(fill="none")+
#scale_y_sqrt()+
ylab("% Acropora cover")+
theme_classic()+theme(axis.title.x=element_blank(), axis.line=element_line(size=0.2),axis.title.y=element_text(size=8), strip.background=element_blank(), strip.text=element_text(size=8), axis.text.x=element_text(size=8, angle=45, hjust=1))
acrplot

head(sites)

tab[tab$region=="Cape Grenville",]

tab <- aggregate(value~reef+region+month, chk[chk$variable %in% c("tabular_Acropora"),], sum)
#tab <- chk[chk$variable %in% c("tabular_Acropora"),c("reef", "region", "month", "value")]
head(tab)
tabb <- rbind(tab, data.frame(reef=new$Reef, region=new$Region, month="Apr24", value=new$tab))
tabb$tlab <- ifelse(tabb$month=="Apr16", "2016a", ifelse(tabb$month=="Apr24", "2024a", ifelse(tabb$month=="Oct16", "2016b", NA)))
tabb$tlab <- factor(tabb$tlab, levels=c("2016a", "2016b", "2024a"))
tabb$region <- factor(tabb$region, levels=rev(regions2))
head(tabb)

tabb$ID <- paste(tabb$reef, acrb$month)
covb$ID <- paste(covb$reef, covb$month)
tabb$tcov <- covb$value[match(tabb$ID, covb$ID)]

# tabb$value <- tabb$value/tabb$tcov ### RELATIVE OR ABSOLUTE


revisit3 <- tabb[tabb$reef %in% keep, ]
table(revisit2$reef)


tabplot <- ggplot()+
geom_boxplot(data=tabb, aes(x=tlab, y=value, fill=tlab), outlier.size=0.1, size=0.1, alpha=0.5)+
geom_point(data=tabb, aes(x=tlab, y=value), size=0.25)+
#stat_summary(data=revisit, aes(x=tlab, y=value, group=1), geom="line")+
#stat_summary(data=revisit, aes(x=tlab, y=value, fill=tlab), size=0.55, shape=21, stroke=0.3)+
geom_line(data=revisit3, aes(x=tlab, y=value, group=reef), col="slategrey", size=0.25)+
#geom_point(data=covb[covb$reef %in% keep, ], aes(x=tlab, y=value,  fill=tlab), shape=21)+
facet_wrap(~region, nrow=1, strip.position="top")+
scale_fill_manual(values=c("grey", "red", "black"))+
guides(fill="none")+
#scale_y_sqrt()+
ylab("% tabular Acropora cover")+
theme_classic()+theme(axis.title.x=element_blank(), axis.line=element_line(size=0.2),axis.title.y=element_text(size=8), strip.background=element_blank(), strip.text=element_text(size=8), axis.text.x=element_text(size=8, angle=45, hjust=1))
tabplot





plot_grid(covplot, acrplot, tabplot, ncol=1, labels=c("a", "b", "c"))










