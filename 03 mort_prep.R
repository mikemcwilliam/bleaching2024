
rm(list = ls())

library("ggplot2")
library("reshape2")
library("cowplot")
library("viridis")

# --------------------------------- # long-term mortality

all1 <- read.csv("data/composition.csv")
all <- all1[!all1$t == "Oct24",]
unique(all$t)

full <- read.csv("data/transects.csv")
head(full)
unique(full$Survey)
tdf <- full[full$Survey %in% "Apr", ]


head(full)
mort <- dcast(Transect_Code~Survey, data=full[!is.na(full$Survey),], value.var="coral_cov")
add.cols <- c("Region", "Reef", "Site", "Zone", "max.dhw", "tlast", "sumN")
mort[, add.cols]<-full[match(mort$Transect_Code, full$Transect_Code), add.cols]
mort$tlast[is.na(mort$tlast)]<-0
head(mort)
hist(mort$max.dhw)

# did not return-  no return trips to cormrant / thetford / 2 sites at buggatti 
mort <- mort[!mort$Region %in% c("Princess Charlotte Bay", "Cape Grenville"),]
mort <- mort[!mort$Reef %in% c("Cormrant", "Thetford"),]
mort <- mort[!(mort$Reef == c("Bugatti") & mort$Site %in% c("B2 - NW Slope", "B3 - NE Point Crest")),]
unique(mort$Reef)

# gaps due to name differences 
fillcols <- c("max.dhw", "tlast", "sumN")
mort[mort$Transect_Code=="WIS_B1_C3",fillcols]<-mort[mort$Transect_Code=="WIS_B1_C2",fillcols]
mort[mort$Transect_Code=="HER_B1_C3",fillcols]<-mort[mort$Transect_Code=="HER_B1_C2",fillcols]
mort

#april values
mort$pbleach <- tdf$pbleach[match(mort$Transect_Code, tdf$Transect_Code)]
mort$acro <- tdf$acro[match(mort$Transect_Code, tdf$Transect_Code)]
mort$por <- tdf$por[match(mort$Transect_Code, tdf$Transect_Code)]
head(mort)

# site-level
smort <- aggregate(.~Site+Zone+Region+Reef+sumN+tlast, na.omit(subset(mort, select=-c(Transect_Code))), mean)
head(smort)

# --------------------------------- #  2016 mortality

# USE dfm

dfm <- read.csv("data/data2016/Data_Global_warming_transforms_coral_reef_assemblages/InWaterSurveyData.csv")
head(dfm)
dfm$change <- dfm$ChangeInCover..log10.final.cover....log10.initial.cover..#*100
dfm$dhw <- dfm$X5k_DHW..degree.Centigrade.weeks.
dfm$bl <- dfm$PercentBleached....
dfm$mort1 <- dfm$InstantMortality....
dfm <- dfm[,c("ReefID", "change", "dhw", "bl", "mort1")]
head(dfm)
nrow(dfm)
dfm$ReefID[dfm$ReefID=="11-244c"]<-"11-244"
dfm$ReefID[dfm$ReefID=="14-147a"]<-"14-147"
dfm$ReefID[dfm$ReefID=="14-116b"]<-"14-116"
dfm$ReefID[dfm$ReefID=="14-086"]<-"11-191"# could be 14-086 or 11-191 but both have same change and don't align
dfm$Reef <- chk2$Reef[match(dfm$ReefID, chk2$ReefID)]

plot_grid(ggplot(dfm, aes(x=bl, y=change))+geom_point()+geom_smooth(),
ggplot(dfm, aes(x=dhw, y=change))+geom_point()+geom_smooth())

# --------------------------------- #  mort

# we can get the exact before/after cover from this. 
rnames <- read.csv("data/data2016/reefnames.csv")

apr <- read.csv("data/data2016/Data_Global_warming_transforms_coral_reef_assemblages/TaxonomicDataA.csv")[,c(1:16)]
oct <- read.csv("data/data2016/Data_Global_warming_transforms_coral_reef_assemblages/TaxonomicDataO.csv")[,c(1:16)]
head(apr)
nrow(apr)
aprL <- melt(apr, id.var="ReefID")
aprL <- aprL#[!aprL$variable %in% c("Soft.corals", "Other.sessile.fauna"),]
aprC <- aggregate(value~ ReefID, aprL, sum)
octL <- melt(oct, id.var="ReefID")
octL <- octL#$variable %in% c("Soft.corals", "Other.sessile.fauna"),]
octC <- aggregate(value~ ReefID, octL, sum)
hist(octC$value)
apr[8,]

head(apr)
AprilAc<-melt(apr[,c("ReefID", "Staghorn.Acropora", "Tabular.Acropora", "Other.Acropora")])
AprilAc <- aggregate(value~ReefID, AprilAc, sum)
AprilAc

#AprilAc <- all[all$align %in% c("other_Acropora", "staghorn_Acropora", "tabular_Acropora"),]
#AprilAc <- AprilAc[AprilAc$t %in% "Apr16",]
#AprilAc <- aggregate(cov~ID+region2+site+reef, AprilAc, sum)
#AprilAc <- aggregate(cov~region2+reef, AprilAc, mean)
#AprilAc$ReefID <- rnames$ReefNo[match(AprilAc$reef, rnames$use)]
#AprilAc$value <- AprilAc$cov
#head(AprilAc)

head(apr)
AprilPor<-melt(apr[,c("ReefID", "Poritidae")])
AprilPor <- aggregate(value~ReefID, AprilPor, sum)
AprilPor


reefs <- data.frame(both=unique(c(dfm$ReefID, apr$ReefID)))
reefs$dfm <- dfm$ReefID[match(reefs$both, dfm$ReefID)]
reefs$apr <- apr$ReefID[match(reefs$both, apr$ReefID)]
reefs$oct <- oct$ReefID[match(reefs$both, oct$ReefID)]
reefs

# find missing
octC$apr <- aprC$value[match(octC$ReefID, aprC$ReefID)]
octC$change <- log10(octC$value) - log10(octC$apr )
octC[octC$ReefID=="11-191", "change"]

dfm$aprC <- aprC$value[match(dfm$ReefID, aprC$ReefID)]
dfm$octC <- octC$value[match(dfm$ReefID, octC$ReefID)]
dfm$chk <- log10(dfm$octC)-log10(dfm$aprC)
dfm$acro <- AprilAc$value[match(dfm$ReefID, AprilAc$ReefID)]
dfm$por <- AprilPor$value[match(dfm$ReefID, AprilPor$ReefID)]

ggplot(dfm, aes(change, chk))+geom_point()+geom_abline()+geom_text(aes(label=ReefID))


# --------------------------------- #  CHANGE METRIC? 

head(smort)

# choose change metric (absolute/log)

# ABSOLUTE
head(dfm)
dfm$Achange <- (dfm$octC - dfm$aprC ) /dfm$aprC  * 100
smort$Achange <- (smort$Oct - smort$Apr) /smort$Apr * 100

# LOG
smort$change2 <-  log10(smort$Oct/smort$Apr)
smort$Lchange <- smort$change2
dfm$Lchange <- dfm$change

summary(nls(Achange ~ a* exp(b * Lchange)-c, start=c(a=1, b=2, c=100), data=dfm))

summary(nls(Lchange ~ a*log(Achange+c)+b, start=c(a=1, b=2, c=100), data=dfm))

logform <- data.frame(log=seq(-1.5, 0.2, 0.1))
logform$abs <-  100 * exp(2.3*logform$log) - 100
satform <- data.frame(abs=seq(-100, 50, 10))
satform$log <- 0.432 * log(satform$abs+99.91) - 1.991

ggplot()+geom_point(data=dfm, aes(Lchange, Achange))+
geom_point(data=smort, aes(Lchange, Achange), col="red")+geom_line(data=logform, aes(log, abs))

ggplot()+geom_point(data=dfm, aes(Achange, Lchange))+geom_line(data=satform, aes( abs, log))

# --------------------------------- # combine data

ggplot(smort, aes(as.factor(tlast), Lchange))+geom_boxplot()

sd(smort$acro, na.rm=T)
mean(smort$acro)
# check independence

c1 <- rnorm(100, 36, 17) # coral cover 1
c2 <- rnorm(100, 27, 12) # coral cover 2
AC <- rnorm(100, 24, 20) # acropora cover
log_change <- log10(c2/c1)
AC_relative <- AC/c1

ggplot(data=NULL, aes(c1, log_change))+geom_point()+geom_smooth()
summary(lm(log_change~AC)) # cover 1 not independent

ggplot(data=NULL, aes(AC, log_change))+geom_point()+geom_smooth()
summary(lm(log_change~AC)) 

# --------------------------------- # combine data? 

head(dfm)
sitesC <- smort#[smort$Zone %in% c("Crest"),] #subset(sitesC, select=-c(Zone))
sitesC <- aggregate(.~Site+Region+Reef+sumN+tlast+max.dhw+Zone, sitesC, mean)

dfm2 <- rbind(data.frame(reef=dfm$Reef, site=NA, zone="Crest",bl=dfm$bl, dhw=dfm$dhw, cov1=dfm$aprC, cov2=dfm$octC, Achange=dfm$Achange, Lchange=dfm$Lchange, year="2016", acro=dfm$acro, por=dfm$por),
data.frame(reef=sitesC$Reef, site=sitesC$Site, zone=sitesC$Zone, bl=sitesC$pbleach*100, dhw=sitesC$max.dhw, cov1=sitesC$Apr, cov2=sitesC$Oct, Achange=sitesC$Achange, Lchange=sitesC$Lchange, year="2024", acro=sitesC$acro, por=sitesC$por))
head(dfm2)

dfm$change_use <- dfm$Lchange
#sites$change_use <- sites$Lchange
sitesC$change_use <- sitesC$Lchange
dfm2$change_use <- dfm2$Lchange 

sitesC$cov1 <- sitesC$Apr
sitesC$cov2 <- sitesC$Oct


#  write.csv(dfm2, "data/mortality.csv")

