

rm(list = ls())

library("ggplot2")
library("reshape2")
library("cowplot")
library("viridis")
library("vegan")
library("sf")
se <- function(x) sqrt(var(x)/length(x))

# --------------------------------- #  data

tdf <- read.csv("data/bleaching.csv")
head(tdf)

all <- read.csv("data/composition.csv")

# --------------------------------- # site-level data


tdf$siteID <- paste(tdf$Reef, tdf$Site, tdf$Zone)
sites <- unique(tdf[,c("Region","Reef", "reef_use", "Site", "siteID", "Zone", "gridID", "GPS.S", "GPS.E", "max.dhw")])

pbleach.av <- aggregate(pbleach~siteID, tdf, mean)
sites$pbleach <- pbleach.av$pbleach[match(sites$siteID, pbleach.av$siteID)]

pdead.av <- aggregate(pdead~siteID, tdf, mean)
sites$pdead <-pdead.av$pdead[match(sites$siteID, pdead.av$siteID)]

sitepoc <- aggregate(poc~siteID, tdf, mean)
sites$poc <- sitepoc$poc[match(sites$siteID, sitepoc$siteID)]

siteac <- aggregate(acro~siteID, tdf, mean)
sites$acro <- siteac$acro[match(sites$siteID, siteac$siteID)]

sitepor <- aggregate(por~siteID, tdf, mean)
sites$por <- sitepor$por[match(sites$siteID, sitepor$siteID)]

sitetab <- aggregate(tab~siteID, tdf, mean)
sites$tab <- sitetab$tab[match(sites$siteID, sitetab$siteID) ]

head(sites)

ggplot(sites, aes(x=max.dhw, y=pbleach))+geom_point()+facet_wrap(~Zone)

# --------------------------------- # heatmaps 2024

regions <- c("Capricorn Bunkers", "Hydrographers Passage", "Cape Bowling Green", "Cairns", "Lizard", "Princess Charlotte Bay", "Cape Grenville")

source("figs/map.R")
#mapplot
#aboveNplot
map24d

# --------------------------------- # heat stress history

dhw <- read.csv("data/noaa_sst/sst_gbr.csv")
dhw1 <- read.csv("data/noaa_sst/sst_gbr_reefs.csv")
head(dhw1)


ggplot()+
geom_histogram(data=dhw, aes(x=dhw, fill=year), col="black", size=0.1)+
facet_wrap(~year, ncol=1,  strip.position="right")

dhw1$aboveN <- ifelse(dhw1$dhw> 6, 1, 0) # above 6
t.use <- dhw1[dhw1$aboveN==1,]
t.use <- t.use[!t.use$year %in% c(1998,2002),]
t.use <- t.use[!t.use$year==2024,]
time <- aggregate(year~ X + Y + gridID, t.use, max) # max year in the dataset. 
time$last <- 2024 - time$year
freq <- aggregate(aboveN ~ X + Y + gridID, dhw1[!dhw1$year %in% c(1998,2002),], sum)

fdat <- data.frame(table(freq$aboveN))
fdat$p <- fdat$Freq / sum(table(freq$aboveN)) *100
tdat <- data.frame(table(time$last))
tdat$p <- tdat$Freq / sum(table(time$last)) *100

plot_grid(ggplot(fdat, aes(x=Var1, y=p))+geom_bar(stat="identity"),
ggplot(tdat, aes(x=Var1, y=p))+geom_bar(stat="identity"))

# surveyed sites

fdat2 <- data.frame(table(tdf$sumN) / sum(table(tdf$sumN)) * 100)
tdat2 <- data.frame(table(tdf$tlast) / sum(table(tdf$tlast)) * 100)

plot_grid(ggplot(fdat2, aes(x=Var1, y=Freq))+geom_bar(stat="identity"),
ggplot(tdat2, aes(x=Var1, y=Freq))+geom_bar(stat="identity"))

# --------------------------------- # acropora change (2016-2024)

head(all)

comp1 <- all[all$zone=="Crest",]
comp1 <- aggregate(cov~align+ID+site+reef+region+t+tlab+ntimes, comp1, sum)

removeR <- c("Coral Sea", "Hydrographers Passage")

comp1R <- aggregate(cov~align+reef+region+t+tlab+ntimes, comp1[comp1$ntimes %in% c(2,3),], mean)
comp1.ac <- aggregate(cov~tlab+reef+site+ID+ntimes, comp1[comp1$align %in% c("tabular_Acropora", "staghorn_Acropora", "other_Acropora"),], sum)
comp1.acR <- aggregate(cov~tlab+reef, comp1.ac[comp1.ac$ntimes %in% c(2,3),], mean)
#comp1.ac <- aggregate(cov~t3+reef, comp1R[comp1R$taxa %in% c("tabular_Acropora", "staghorn_Acropora", "other_Acropora"),], sum)

comp1R <- comp1R[!comp1$region %in% removeR, ]

comp1R$tlab <- factor(comp1R$tlab, levels=rev(c("2016a", "2016b", "2024")))
comp1.acR$tlab <- factor(comp1.acR$tlab, levels=rev(c("2016a", "2016b", "2024")))

tabplot2 <- ggplot(comp1R[comp1R$align %in% c("tabular_Acropora"),])+
geom_boxplot(aes(y=tlab, x=cov))+scale_x_sqrt()

acplot2 <- ggplot(comp1.acR)+geom_boxplot(aes(y=tlab, x=cov))+scale_x_sqrt()

plot_grid(tabplot2, acplot2)

# --------------------------------- # mds change (2016-2024)

library("vegan")

#comp2R <- aggregate(cov~align+reef+region+t+tlab+ntimes, comp2[comp2$ntimes %in% c(2,3),], sum)

comp2 <- all[all$zone=="Crest",]
comp2 <- aggregate(cov~align+ID+site+reef+region+t+tlab+ntimes, comp2, sum)


comp21 <- aggregate(cov~align+reef+region+t+tlab+ntimes, comp2[comp2$ntimes %in% c(1, 2,3) & comp2$t %in% c("Apr16", "Oct16"),], sum)
comp22 <- aggregate(cov~align+reef+region+t+tlab+ntimes, comp2[comp2$ntimes %in% c(1, 2,3) & comp2$t %in% c("Mar24"),], mean)
comp2R <- rbind(comp21, comp22)

# comp2R <- aggregate(cov~align+reef+region+t+tlab+ntimes, comp2[comp2$ntimes %in% c(1,2,3),], sum)

comp2R$ID <- paste(comp2R$reef, comp2R$t)

comp2R <- comp2R[!comp2R$region %in% removeR, ]

comp2R <- comp2R[!(comp2R$t=="Apr16" & comp2R$ntimes==1), ]

comp2R <- comp2R[!comp2R$reef=="NoName",]
head(comp2R)

totcov <- aggregate(cov~reef, comp2R, sum)
comp2R$tot <- totcov$cov[match(comp2R$reef, totcov$reef)] 
comp2R$pcov <- comp2R$cov/comp2R$tot

head(comp2R)
comp2R <- comp2R[!comp2R$align=="soft",]

wide <- acast(comp2R, ID~align, value.var="cov") 
head(wide)

# mds
mds<-metaMDS(sqrt(wide), k=2, distance="bray", autotransform=F, trymax=1000) 
#stressplot(mds)
# saveRDS(mds, file = "data/fig1mdsOK.rds") 

mdspoints<-data.frame(scores(mds)$sites)
mdspoints$Reef <- comp2R$reef[match(rownames(mdspoints), comp2R$ID)]
mdspoints$t <- comp2R$t[match(rownames(mdspoints), comp2R$ID)]
mdspoints$ntimes <- comp2R$ntimes[match(rownames(mdspoints), comp2R$ID)]
#mdspoints$Time[is.na(mdspoints$Time)]<-3
head(mdspoints)
mdsvectors<- as.data.frame(mds$species)
mdsvectors$lab <- c(nrow(mdsvectors):1)
mdsvectors$lab2 <- rownames(mdsvectors)

mdspoints$NMDS1 <- - mdspoints$NMDS1
mdsvectors$MDS1 <- - mdsvectors$MDS1 
#mdspoints$NMDS2 <- - mdspoints$NMDS2 
#mdsvectors$MDS2 <- - mdsvectors$MDS2 

head(mdspoints)

nrow(mdspoints[mdspoints$t==3,])

expx <- 1.3
expy <- 1.3
xlims <- c(min(mdspoints$NMDS1)*expx, max(mdspoints$NMDS1)*expx)
ylims <- c(min(mdspoints$NMDS2)*expx, max(mdspoints$NMDS2)*expx)

plot_grid(
ggplot()+
lims(x=xlims, y=ylims)+
stat_ellipse(data=mdspoints[mdspoints$t %in% c("Apr16", "Mar24"),], aes(NMDS1, NMDS2,  group=as.factor(t)), geom="polygon", alpha=0.15, col="black")+
guides(fill="none")+
#geom_text(data=mdspoints, aes(NMDS1, NMDS2, label=Reef))+
geom_point(data=mdspoints, aes(NMDS1, NMDS2, fill=as.factor(t)), shape=21, size=1, stroke=0.1)
, 
ggplot()+
lims(x=xlims, y=ylims)+
geom_path(data=mdspoints[mdspoints$t %in% c("Apr16", "Oct16"),], aes(x=NMDS1, y=NMDS2, group=Reef), col="grey", arrow=arrow(length=unit(0.5, "mm")), size=0.4)+
geom_path(data=mdspoints[mdspoints$t %in% c("Oct16","Mar24"),], aes(x=NMDS1, y=NMDS2, group=Reef), col="black", arrow=arrow(length=unit(0.5, "mm"), ends="first"))
,
ggplot()+
lims(x=xlims, y=ylims)+
geom_path(data=mdspoints[mdspoints$t %in% c("Apr16","Mar24"),], aes(x=NMDS1, y=NMDS2, group=Reef), col="red", arrow=arrow(length=unit(1, "mm")))
,
ggplot()+geom_segment(data=mdsvectors, aes(x=0, xend=MDS1, y=0, yend=MDS2), col="grey")+
lims(x=xlims, y=ylims)+
geom_text(data=mdsvectors, aes(MDS1, MDS2, label=lab2), hjust=ifelse(mdsvectors$MDS1 >0, 0, 1), size=2.5, fontface="bold")
)

# --------------------------------- # fig 1

source("figs/fig1.R")
fig1

# --------------------------------- # model bleaching

library("betareg")
library("mgcv")

summary(glm(pbleach~max.dhw, family="quasibinomial", data=tdf, weights=tdf$Ncoral)) 
summary(betareg(pbleach ~ max.dhw, data=tdf, link="logit")) # sigmoidal?
summary(gam(pbleach~s(max.dhw, k=5), data=tdf))

# binomial
bidat <- sites[sites$Zone=="Crest", ]
bimod <- glm(pbleach~max.dhw, family="quasibinomial", data=bidat, weights=bidat$Ncoral)
summary(bimod)
bifit <- data.frame(max.dhw = seq(min(bidat$max.dhw), max(bidat$max.dhw), 0.1))
bifit$fit <- predict(bimod, bifit, type="response")
ggplot()+geom_line(data=bifit, aes(max.dhw, fit))+geom_point(data=bidat, aes(max.dhw, pbleach, col=Region))


AICvals <- list()
rsq <- list()
curves <- NULL
resids <- list()
datasets <- list(sites, tdf)
names(datasets) <- c("sites", "tdf")
for(i in c("sites","tdf")){
for(j in c("Crest", "Slope")){
	#i <- 2	#j <- "Crest"
	#i <- "tdf"
	#j <- "Crest"
mod.dat <- datasets[[i]]
mod.dat <- mod.dat[mod.dat$Zone==j,]
mod.dat$response <- mod.dat$pbleach
mod.dat$predictor <- mod.dat$max.dhw
#ggplot(mod.dat, aes(predictor,response))+geom_point()
# models.
lm1 <- lm(response~predictor, mod.dat)
lm2 <- lm(response~sqrt(predictor), mod.dat)
poly1 <- lm(response~poly(predictor,2), mod.dat)
gam1.5 <- gam(response~s(predictor, k=5), data=mod.dat)
gam1.3 <- gam(response~s(predictor, k=4), data=mod.dat)
expmod <- nls(response ~ a*exp(b*predictor), data=mod.dat, start=list(a=1, b=1))
expmod2 <- lm(log(response+0.01)~predictor, data=mod.dat)
logmod <- nls(response ~ b*log(predictor) + c, data=mod.dat, start=list(b=48.6, c=-21.6)) # saturating
logmod2 <- lm(response~log(predictor), data=mod.dat) # same as b*a! 
betamod = betareg(response ~ predictor, data=mod.dat, link="logit") # sigmoidal?
bimod <- glm(response~predictor, family="quasibinomial", data=mod.dat, weights=mod.dat$Ncoral) #binomial also works
# rsq
rsq[[paste(i,j)]] <- data.frame(lm1 = summary(lm1)$adj.r.squared, poly1 = summary(poly1)$adj.r.squared, gam1.3 = summary(gam1.3)$r.sq, gam1.5 = summary(gam1.5)$r.sq, logmod = summary(logmod2)$adj.r.squared, expmod = summary(expmod2)$adj.r.squared, betamod=betamod$pseudo.r.squared)
# fitted curves
fit.dat <- data.frame(predictor = seq(min(mod.dat$predictor), max(mod.dat$predictor), 0.1))
fit.dat$lm1fit <- predict(lm1, fit.dat)
fit.dat$poly1fit <- predict(poly1, fit.dat)
fit.dat$gam1.5 <- predict(gam1.5, fit.dat)
fit.dat$gam1.3 <- predict(gam1.3, fit.dat)
fit.dat$logfit <- predict(logmod,fit.dat)
fit.dat$betafit <- predict(betamod,fit.dat)
fit.dat$expfit <- predict(expmod,fit.dat)
fit.dat$logisfit <- NA # not working for slope
fit.dat$bifit <- predict(bimod,fit.dat, type="response")
# residuals
if(j=="Crest"){
logismod <- nls(response ~ SSlogis(predictor, Asym, xmid, scal), data = mod.dat)
fit.dat$logisfit <- predict(logismod, fit.dat) 
AICvals[[paste(i,j)]] <- data.frame(AIC(lm1,lm2, poly1, gam1.5, gam1.3, logismod, logmod,expmod,expmod2, logmod2,betamod, bimod), Zone=j, data=i)
resids[[paste(i,j)]]   <- cbind(mod.dat, data.frame(lm1 = residuals(lm1), gam.3 = residuals(gam1.3), gam.5 = residuals(gam1.5), poly1 = residuals(poly1), logmod=residuals(logmod), betamod=residuals(betamod), logismod=residuals(logismod),bimod = residuals(bimod)))
}else {
AICvals[[paste(i,j)]] <- data.frame(AIC(lm1, lm2,poly1, gam1.5, gam1.3,  logmod,logmod2, expmod,expmod2,betamod, bimod), Zone=j, data=i)
resids[[paste(i,j)]]  <- cbind(mod.dat, data.frame(lm1 = residuals(lm1), gam.3 = residuals(gam1.3), gam.5 = residuals(gam1.5), poly1 = residuals(poly1), logmod=residuals(logmod), betamod=residuals(betamod), logismod=NA), bimod = residuals(bimod))
}
curves <- rbind(curves, cbind(fit.dat, data=i, Zone=j))
}}

head(resids[["tdf Crest"]])

tres <- rbind(resids[["tdf Crest"]], resids[["tdf Slope"]]) # simply has the rediduals added... 
sres <- rbind(resids[["sites Crest"]], resids[["sites Slope"]])

head(tres)
nrow(tres)
nrow(sres)

head(curves)
fit.long <- melt(curves, id.var=c("predictor", "data", "Zone"))

ggplot(fit.long, aes(x=predictor, y=value))+geom_line(aes(col=variable))+facet_grid(data~Zone)

ggplot(fit.long[fit.long$data=="tdf" & fit.long$variable %in% c("lm1fit", "expfit","gam1.5", "logfit", "betafit", "logisfit", "bifit"),], aes(x=predictor, y=value*100))+
geom_line(aes(col=Zone))+
facet_wrap(~variable, scales="free", ncol=1)+
theme_classic()+theme(strip.background=element_blank(), axis.text=element_blank())

 # are the years statistically different?
 
bimodX <- glm(pbleach~max.dhw, family="quasibinomial", data=sres[sres$Zone=="Crest",], weights=sres[sres$Zone=="Crest","Ncoral"]) 
fit.datX <- data.frame(max.dhw = seq(min(sres$max.dhw), max(sres$max.dhw), 0.1))
fit.datX$bifit <- predict(bimodX,fit.datX, type="response", se=T)$fit
fit.datX$bise <- predict(bimodX,fit.datX, type="response", se=T)$se.fit

curvesCrest <- curves[curves$data=="sites" & curves$Zone=="Crest",]


# --------------------------------- above curve?

crestdat <- sres[sres$Zone=="Crest",]
betamod2 <- betareg(pbleach ~ max.dhw, data=crestdat)
crestdat$fit <- predict(betamod, crestdat)
middhw <- crestdat#[crestdat$max.dhw<10 & crestdat$max.dhw>5,]
middhw$above2024 <- ifelse(middhw$response > middhw$fit, 1, 0)
middhw$response.2016 <- ((48.6 * log(middhw$predictor)  - 21.6))/100
middhw$above2016 <- ifelse(middhw$response > middhw$response.2016, 1, 0)

sum(middhw$above2024)/length(middhw$above2024 )
sum(middhw$above2016)/length(middhw$above2016)
mean(middhw$response.2016[middhw$above2016==1])
max(middhw$response[middhw$above2024==1])
mean(middhw$response[middhw$above2024==1])
mean(middhw$response)
max(middhw$predictor)

# --------------------------------- #  2016 bleaching

bl16 <- read.csv("data/original/bleaching2016.csv")
head(bl16)

ggplot(bl16, aes(DHWs, Bleached))+geom_point()

j.av <- aggregate(BleachDead~Reef, bl16, mean)
j.av$DHWs <- aggregate(DHWs~Reef, bl16, mean)$DHWs
nrow(j.av)

mod2016 <- data.frame(dhw=seq(2, 12, 0.5))
mod2016$y <- (48.6 * log(mod2016$dhw)  - 21.6)  # original model

mod16 <- nls(BleachDead ~ b*log(DHWs) + c, data=j.av[!j.av$Reef=="12-059",], start=list(b=48.6, c=-21.6))
summary(mod16)

library("investr")

new.data <- data.frame(DHWs=seq(2, 12, 0.2))
fit16 <- cbind(new.data, data.frame(predFit(mod16, newdata = new.data, interval = "confidence", level= 0.95)))

ggplot()+
geom_point(data=j.av, aes(DHWs, BleachDead), shape=21, size=2)+
geom_line(data=mod2016, aes(dhw, y))

# --------------------------------- recent mortality

# prop dead should also be beta regression?

tdf[is.na(tdf$pbleach),]
max(tdf$pdead)

betamod3 = betareg(pdead ~ max.dhw, data=crestdat)
fit.dat2 <- data.frame(max.dhw = seq(min(crestdat$max.dhw), max(crestdat$max.dhw), 0.1))
fit.dat2$fit <- predict(betamod3, fit.dat2)
summary(betamod3)


j.av2 <- aggregate(Mortality~Reef, bl16, mean)
j.av2$DHWs <- aggregate(DHWs~Reef, bl16, mean)$DHWs
j.av2$N <- aggregate(Total~Reef, bl16, mean)$Total
nrow(j.av2)
j.av2$Mort <- j.av2$Mortality/100
#betamod5 = betareg(Mort ~ DHWs, data=j.av2)
betamod5 <- glm(Mort~DHWs, family="quasibinomial", data=j.av2, weights=j.av2$N) #binomial also works
# rsq
#betamod5 <- lm(Mort~poly(DHWs, 2), data=j.av2)
fit.dat3 <- data.frame(DHWs = seq(min(j.av2$DHWs), max(j.av2$DHWs), 0.1))
fit.dat3$fit <- predict(betamod5, fit.dat3, type="response")
summary(betamod5)

col16 <- "grey65"

ggplot()+
geom_point(data=j.av2, aes(DHWs, Mortality),col=col16, shape=4, size=1, stroke=0.3)+
geom_line(data=fit.dat3, aes(x=DHWs, y=(fit*100)-2), col=col16)+
geom_point(data=sites[sites$Zone=="Crest",], aes(max.dhw, pdead*100), shape=21, fill="black", size=0.5)+
geom_line(data=fit.dat2, aes(x=max.dhw, y=fit*100))

# --------------------------------- # composition 2024

head(all)

comp3 <- all[all$t=="Mar24",]

ctax <- read.csv("data/info/composition_taxa.csv")
comp3$group <- ctax$group[match(comp3$taxa, ctax$taxon)]
comp3$label <- ctax$label[match(comp3$taxa, ctax$taxon)]
head(comp3)

tax.sums <- aggregate(cov~taxa+group, comp3, sum)  
rare <- tax.sums$taxa[tax.sums$cov < 60 & tax.sums$group=="HC"]
rare

comp3$taxa <- ifelse(comp3$taxa %in% c(rare, "Other.Scleractinia", "Heliopora","Millepora"), "Other.Coral",comp3$taxa)
comp3$taxa <- ifelse(comp3$taxa %in% c("Chlorodesmis"), "Macroalgae",comp3$taxa)
comp3$taxa <- ifelse(comp3$group %in% c("SC", "ZO"), "Soft.Coral", comp3$taxa)
comp3 <-  comp3[!comp3$taxa == "Sand..Rubble", ]
unique(comp3$taxa)
comp3 <- aggregate(cov~ID+taxa, comp3, sum)

mdsdat2 <- acast(comp3, ID~taxa, value.var="cov") 
mdsdat2<- mdsdat2

# mds2<-metaMDS(mdsdat2, k=3, distance="bray", autotransform=FALSE, trymax=1000)
# saveRDS(mds2, file = "data/output/mds1000sqrt2_Mar.rds") 
mds2 <- readRDS("data/output/mds1000sqrt2.rds") # load to skip mds processing
mdspoints2<-data.frame(scores(mds2)$sites)
mdspoints2$transect <- gsub(" Mar24", "", rownames(mdspoints2))
mdsvectors2<- as.data.frame(mds2$species)
nrow(mdspoints2)
head(mdspoints2)
tdf[,c("NMDS1.2", "NMDS2.2")] <- mdspoints2[match(tdf$Transect_code, mdspoints2$transect), c("NMDS1", "NMDS2")]

plot_grid(
ggplot()+geom_segment(data=mdsvectors2, aes(x=0, xend=MDS1, y=0, yend=MDS2), size=0.2, col="slategrey")+
geom_text(data=mdsvectors2, aes(MDS1, MDS2, label=rownames(mdsvectors2)))
,ggplot()+ geom_point(data=tdf, aes(NMDS1.2, NMDS2.2, fill=pbleach*100), shape=21, stroke=0.1, size=1.2)+
scale_fill_viridis())

# --------------------------------- composition vs bleaching

tres[,c("NMDS1.2", "NMDS2.2")] <- mdspoints2[match(tres$Transect_code, mdspoints2$transect), c("NMDS1", "NMDS2")]

mod.dat2 <- tres#[tdf$Zone=="Crest",]
nrow(mod.dat2)

mods <- NULL
# x <- "acro_ratio"
xes <- c("poc","por", "coral_cov", "acro","tab", "NMDS1.2", "NMDS2.2")
for(x in xes){
mod.dat2$x <- scale(mod.dat2[,x])
mod1 <- lm(pbleach~x + Zone, mod.dat2)
mod2 <- lm(betamod~x+ Zone, mod.dat2)
slp1 <- coef(mod1)[2]
slp2 <- coef(mod2)[2]
conf1 <- confint(mod1)[2,]
conf2 <- confint(mod2)[2,]
mods <- rbind(mods, rbind(data.frame(y="resids", x=x, slp=slp2, low=conf2[1], upp=conf2[2]), data.frame(y="pbleach", x=x, slp=slp1, low=conf1[1], upp=conf1[2])))
}
mods
mods$y2 <- ifelse(mods$y=="pbleach", "% bleaching", "bleaching residuals")

mods$x2 <- ifelse(mods$x=="por", "Poritidae",#R
ifelse(mods$x=="tab", "Tabular Acropora", 
ifelse(mods$x=="acro", "Acroporidae", #R
ifelse(mods$x=="poc", "Pocilliporidae", #R
ifelse(mods$x=="coral_cov", "Total coral cover", 
ifelse(mods$x=="NMDS1.2", "NMDS1", 
ifelse(mods$x=="NMDS2.2", "NMDS2", mods$x)))))))

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

# --------------------------------- recovery vs bleaching

# all acro
# oct - 24
# crest only resids
# crest only acro
# different bl residuals on some reefs (davies/wilson)

comp3 <- all #[all$ntimes==3,]
comp3 <- comp3[comp3$zone=="Crest",]
comp3 <- comp3[comp3$align %in% c( "tabular_Acropora","other_Acropora", "staghorn_Acropora"),] # "other_Acropora", "staghorn_Acropora"
comp3 <- aggregate(cov~ID+site+reef+t, comp3, sum)
comp3R <- aggregate(cov~reef+t, comp3, mean)
comp3R$se <- aggregate(cov~reef+t, comp3, se)$cov
#comp3R <- comp3R[!comp3R$t %in% c("Oct16"),]
head(comp3R)

acro <- dcast(comp3R, reef~t, value.var='cov')
#acro.se <- dcast(comp3R, reef~t, value.var='se')
acro$change <- acro$Mar24 - acro$Oct16
acro

acro$reef[acro$reef=="Ribbon8"] <- "Ribbon 8 "
acro$reef[acro$reef=="NthDirection"] <-"North Direction" 

df <- tres
df$resids <- df$betamod
reefs <- aggregate(resids~Reef, df[df$Zone=="Crest",], mean) #[sites$Zone=="Crest",]
reefs$resids.se <- aggregate(resids~Reef, df[df$Zone=="Crest",], se)$resids #[sites$Zone=="Crest",]

acro$resids <- reefs$resids[match( acro$reef, reefs$Reef)]
acro$resids.se <- reefs$resids.se[match( acro$reef, reefs$Reef)]
acro$match <- reefs$Reef[match( acro$reef, reefs$Reef)]
acro <- na.omit(acro)
acro

ggplot()+
geom_hline(yintercept=0, col="grey")+geom_vline(xintercept=0, col="grey")+
geom_point(data=acro, aes(x=change, y=resids), size=1)+
geom_smooth(data=acro, aes(x=change, y=resids), method="lm", se=F, col="red", size=0.35, formula=y~poly(x,1))+
labs(x="Change in % Acropora\n(2016-2024)", y="deviation from expected\nbleaching (2024)")

# --------------------------------- # fig 2

source("figs/fig2.R")
fig2

# --------------------------------- # heatwave history (maps)

dhw <- read.csv("data/noaa_sst/sst_gbr.csv") # same as longtermgrids (tyears) but all grids
head(dhw)

# heat stress frequency / recovery interval (excluding 2024)
dhw$aboveN <- ifelse(dhw$dhw> 6, 1, 0) # above 6
df.use <- dhw[dhw$year %in% c(2016, 2017, 2020, 2022),]
t.use2 <- df.use[df.use$aboveN==1,]
time.all <- aggregate(year~ X + Y + gridID, t.use2, max) # max year in the dataset. 
time.all$last <- 2024 - time.all$year
freq.all <- aggregate(aboveN ~ X + Y + gridID, df.use, sum)


plot_grid(ggplot(freq.all, aes(X, Y, col=aboveN))+geom_point(size=0.5)+scale_colour_viridis(),
ggplot(time.all, aes(X, Y, col=last))+geom_point(size=0.5)+scale_colour_viridis(option="B", direction=-1))

# --------------------------------- # heatwave history (analysis)

# freqs taken from dhw
sres$freq <- tres$sumN_pre[match(sres$siteID, tres$siteID)]
sres$last <- tres$tlast_pre[match(sres$siteID, tres$siteID)]
sres$last[is.na(sres$last)]<- "8+"

p1 <- plot_grid(
ggplot(sres[sres$Zone=="Crest",], aes(as.factor(freq), betamod))+geom_boxplot(),
ggplot(sres[sres$Zone=="Crest",], aes(as.factor(freq), tab))+geom_boxplot(),
ggplot(sres[sres$Zone=="Crest",], aes(as.factor(last), betamod))+geom_boxplot(),
ggplot(sres[sres$Zone=="Crest",], aes(as.factor(last), tab))+geom_boxplot(),
nrow=1)
p1

tyears <- read.csv("data/noaa_sst/longtermgrids.csv")
head(tyears)
dhw.long <- aggregate(dhw~year+gridID, tyears, max) #[!tyears$year==2024,]

N <- 6
dhw.long$aboveN <- ifelse(dhw.long$dhw> N, 1, 0)
df.use <- dhw.long[dhw.long$year %in% c(2016, 2017, 2020, 2022),]
t.use <- df.use[df.use$aboveN==1,]
time <- aggregate(year~ gridID, t.use, max) # max year in the dataset. 
time$last <- 2024 - time$year
freq <- aggregate(aboveN ~ gridID, df.use, sum) 
freqs2 <- cbind(freq, N=N)
times2 <-  cbind(time, N=N)

sres$freq2 <- freqs2$aboveN[match(sres$gridID, freqs2$gridID)]
sres$last2 <- times2$last[match(sres$gridID, times2$gridID)]
sres$last2[is.na(sres$last2)]<- "8+"
tres$freq2 <- sres$freq2[match(tres$siteID, sres$siteID)]
tres$last2 <- sres$last2[match(tres$siteID, sres$siteID)]


p2<- plot_grid(
ggplot(sres[sres$Zone=="Crest",], aes(as.factor(freq2), betamod))+geom_boxplot(),
ggplot(sres[sres$Zone=="Crest",], aes(as.factor(freq2), tab))+geom_boxplot(),
ggplot(sres[sres$Zone=="Crest",], aes(as.factor(last2), betamod))+geom_boxplot(),
ggplot(sres[sres$Zone=="Crest",], aes(as.factor(last2), tab))+geom_boxplot(),
nrow=1)
p2

plot_grid(p1, p2,ncol=1)

# corbett B1 = tlast is 4 in longterm 2 in dhw () // freq is 3 in longterm 4 in dhw. DID it go above6 in 2022?

# --------------------------------- # anova of heatwave frequency


andat <- NULL
for(x in c("freq2", "last2")){
	for(z in c("Crest", "Slope")){
		for(y in c("betamod", "gam.5", "acro","tab")){
			#z <- "Crest"
			#y <- "betamod"
tres$y <- tres[,y]
tres$x	<- tres[,x]
an1 <- aov(y~x, na.omit(tres[tres$Zone==z, c("y", "x")]))
summary(an1)
pval <- summary(an1)[[1]][["Pr(>F)"]][1]
sig <- ifelse(pval>0.05, "NS", ifelse(pval<=0.05 & pval>0.01, "*", ifelse(pval<=0.01 & pval>0.001, "**", ifelse(pval<=0.001, "***", NA))))
sz <- ifelse(sig=="NS", "ns", "s")
andat <- rbind(andat, data.frame(pval, sig, n=6, Zone=z, y, x, sz))
}}}
andat


##################### HEAT PLOTS!
# sensitivity at different disturbance combinations...

avsev <- aggregate(dhw~gridID, dhw, max)
sres$avsev <- avsev$dhw[match(sres$gridID, avsev$gridID)]

out <- NULL
ys <- c("tab", "acro", "betamod", "gam.5")
	for(i in ys){
	#	i <- "tab"
mod.df <- sres[sres$Zone=="Crest",]# tdf[tdf$Zone=="Crest",] #sites[sites$Zone=="Crest",]
mod.df$response <- mod.df[,i] #mod.df$betamod #mod.df$acroR # #mod.df$betamod #$acroR #mod.df$betamod
mod.df$last2 <-mod.df$avsev #as.numeric(mod.df$last2)
mod.df$freq22 <- mod.df$freq2^2
mod.df$last22 <- as.numeric(mod.df$last2)^2
# problem. Number and Severity not independent... 
dismod <- lm(response~freq2+last2+freq22, mod.df)
new <- expand.grid(freq2=seq(min(mod.df$freq2), max(mod.df$freq2), 0.1), last2=seq(min(mod.df$last2), max(mod.df$last2), 0.1)) 
new$freq22 <- new$freq2^2
new$last22 <- new$last2^2
new$response <- predict(dismod, new)
new$response2 <- (new$response-min(new$response))/(max(new$response)-min(new$response))
new$i <- i 
out <- rbind(out, new)
}
head(out)

out$label <- ifelse(out$i=="tab", "% Tabular (2024)", ifelse(out$i=="acro", "% Acropora cover (2024)", ifelse(out$i=="gam.5", "2024 bleaching\nsusceptibility (gam)",ifelse(out$i=="betamod", "2024 bleaching\nsusceptibility (beta)", NA))))

heatplot <- ggplot()+
geom_raster(data=out, aes(x=freq2, y=last2, fill=response2))+
geom_contour(data=out[out$i=="betamod",], aes(x=freq2, y=last2, z=response), breaks=c(0),col="black", linewidth=0.1,linetype="dashed")+
geom_contour(data=out[out$i=="gam.5",], aes(x=freq2, y=last2, z=response), breaks=c(0),col="black", linewidth=0.1, linetype="dashed")+
#geom_contour(data=out[out$i=="acro",], aes(x=freq, y=avsev, z=response), breaks=c(30, 25, 35),col="black", linewidth=0.1)+
#geom_contour(data=out[out$i=="tab",], aes(x=freq, y=avsev, z=response), breaks=c(10, 15, 5),col="black", linewidth=0.1)+
scale_radius()+
facet_wrap(~label)+
#labs(x="Heatwave frequencies\n(N per decade 2015-24)", y="Heatwave severities\n(max DHW 2015-24)")+
scale_fill_distiller(palette="Spectral")+
scale_y_continuous(expand=c(0,0))+scale_x_continuous(expand=c(0,0))+
theme_bw()+theme(strip.background=element_blank(), strip.text=element_text(size=8, face="bold"), legend.title=element_blank(), legend.text=element_blank())
heatplot

# --------------------------------- # fig3

source("figs/fig3.R")
fig3


# --------------------------------- # possible fig

head(tdf)

ggplot(tres, aes(NMDS1.2, NMDS2.2))+geom_point()+stat_ellipse(aes(col=as.factor(last2)))







