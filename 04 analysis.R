
rm(list = ls())

library("ggplot2")
library("reshape2")
library("cowplot")
library("viridis")
library("vegan")
library("sf")
se <- function(x) sqrt(var(x)/length(x))

head(all)

# --------------------------------- #  data

full <- read.csv("data/transects.csv")
head(full)
unique(full$Survey)
tdf <- full[full$Survey %in% "Apr", ]

all1 <- read.csv("data/composition.csv")
all <- all1[!all1$t == "Oct24",]
unique(all$t)

# --------------------------------- # site-level data

tdf$siteID <- paste(tdf$Reef, tdf$Site, tdf$Zone)
sites <- unique(tdf[,c("Region","Reef", "reef_use", "Site", "siteID", "Zone", "gridID", "GPS.S", "GPS.E", "max.dhw", "region2")])

unique(tdf[,c("Reef", "max.dhw")])

cov.av <- aggregate(coral_cov~siteID, tdf, mean)
sites$coral_cov <- cov.av$coral_cov[match(sites$siteID, cov.av$siteID)]

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


ggplot(sites, aes(x=max.dhw, y=pbleach, col=Region))+geom_point()+facet_wrap(~Zone)

regions <- c("Cape Grenville", "Princess Charlotte Bay", "Cooktown", "Cairns", "Townsville", "Mackay", "Gladstone")

tdf$region2 <- factor(tdf$region2 , levels=regions)
sites$region2 <- factor(sites$region2 , levels=regions)
all$region2 <- factor(all$region2 , levels=regions)

ggplot(sites, aes(x=max.dhw, y=pbleach, col=region2))+geom_point()+facet_wrap(~Zone)

# --------------------------------- # heatmaps 2024

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

sum(fdat$p[fdat$Var1 %in% c(3,4,5)]) #

plot_grid(ggplot(fdat, aes(x=Var1, y=p))+geom_bar(stat="identity"),
ggplot(tdat, aes(x=Var1, y=p))+geom_bar(stat="identity"))

# surveyed sites

fdat2 <- data.frame(table(tdf$sumN) / sum(table(tdf$sumN)) * 100)
tdat2 <- data.frame(table(tdf$tlast) / sum(table(tdf$tlast)) * 100)

sum(fdat2$Freq[fdat2$Var1 %in% c( 3,4,5)])

plot_grid(ggplot(fdat2, aes(x=Var1, y=Freq))+geom_bar(stat="identity"),
ggplot(tdat2, aes(x=Var1, y=Freq))+geom_bar(stat="identity"))

# --------------------------------- # acropora change (2016-2024)

head(all)

comp1 <- all[all$zone=="Crest",]
comp1 <- aggregate(cov~align+ID+site+reef+region2+t+tlab+ntimes, comp1, sum)
removeR <- c("Coral Sea", "Mackay")
comp1R <- aggregate(cov~align+reef+region2+t+tlab+ntimes, comp1[comp1$ntimes %in% c(2,3, 4),], mean)
comp1.ac <- aggregate(cov~tlab+reef+site+ID+ntimes, comp1[comp1$align %in% c("tabular_Acropora", "staghorn_Acropora", "other_Acropora"),], sum)
comp1.acR <- aggregate(cov~tlab+reef, comp1.ac[comp1.ac$ntimes %in% c(2,3, 4),], mean)
comp1R <- comp1R[!comp1$region %in% removeR, ]
comp1R$tlab <- factor(comp1R$tlab, levels=rev(c("2016a", "2016b", "2024a")))
comp1.acR$tlab <- factor(comp1.acR$tlab, levels=rev(c("2016a", "2016b", "2024a")))

tabplot2 <- ggplot(comp1R[comp1R$align %in% c("tabular_Acropora"),])+
geom_boxplot(aes(y=tlab, x=cov))+scale_x_sqrt()

acplot2 <- ggplot(comp1.acR)+geom_boxplot(aes(y=tlab, x=cov))+scale_x_sqrt()

plot_grid(tabplot2, acplot2)

# --------------------------------- # mds change (2016-2024)

library("vegan")

# transect level sums
head(all)
unique(all$align)

comp2 <- all[all$zone %in% "Crest",]

comp2$align2 <- comp2$align
comp2 <- aggregate(cov~align2+ID+site+reef+region2+t+tlab+ntimes, comp2, sum)
comp2R <- aggregate(cov~align2+reef+region2+t+tlab+ntimes, comp2[comp2$ntimes %in% c(1,2,3,4),], mean) # site level 
comp2R$ID <- paste(comp2R$reef, comp2R$t)
comp2R <- comp2R[!comp2R$region2 %in% removeR, ]
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

totcov <- aggregate(cov~reef, comp2R, sum)
comp2R$tot <- totcov$cov[match(comp2R$reef, totcov$reef)] 
comp2R$pcov <- comp2R$cov/comp2R$tot
head(comp2R)

wide <- acast(comp2R, ID~align2, value.var="cov") 
wide2 <- wide/rowSums(wide)
head(wide2)
rowSums(wide2)

# mds
mds<-metaMDS(sqrt(wide2), k=2, distance="bray", autotransform=F, trymax=1000) 
#stressplot(mds)

mdspoints<-data.frame(scores(mds)$sites)
mdspoints$Reef <- comp2R$reef[match(rownames(mdspoints), comp2R$ID)]
mdspoints$t <- comp2R$t[match(rownames(mdspoints), comp2R$ID)]
mdspoints$ntimes <- comp2R$ntimes[match(rownames(mdspoints), comp2R$ID)]
mdsvectors<- as.data.frame(mds$species)
mdsvectors$lab <- c(nrow(mdsvectors):1)
mdsvectors$lab2 <- rownames(mdsvectors)

mdspoints$NMDS1 <- - mdspoints$NMDS1
mdsvectors$MDS1 <- - mdsvectors$MDS1 
head(mdspoints)

expx <- 1.3
expy <- 1.3
xlims <- c(min(mdspoints$NMDS1)*expx, max(mdspoints$NMDS1)*expx)
ylims <- c(min(mdspoints$NMDS2)*expx, max(mdspoints$NMDS2)*expx)

# TWELVE REEFS
reass <- data.frame(table(mdspoints[mdspoints$t %in% c("Apr16","Apr24"),]$Reef))
reass <- reass[reass$Freq==2,]
reass

reassdat <- mdspoints[mdspoints$t %in% c("Apr16","Apr24"),]

plot_grid(
ggplot()+
lims(x=xlims, y=ylims)+
stat_ellipse(data=mdspoints[mdspoints$t %in% c("Apr16", "Apr24"),], aes(NMDS1, NMDS2,  group=as.factor(t)), geom="polygon", alpha=0.15, col="black",linetype='dotted', level=0.9, fill=NA, size=0.1)+
stat_ellipse(data=mdspoints[mdspoints$t %in% c("Apr16", "Apr24"),], aes(NMDS1, NMDS2,  group=as.factor(t), fill=as.factor(t)), geom="polygon", alpha=0.15, col="black", level=0.5)+
guides(fill="none")+
#geom_text(data=mdspoints, aes(NMDS1, NMDS2, label=Reef))+
geom_point(data=mdspoints, aes(NMDS1, NMDS2, fill=as.factor(t)), shape=21, size=1, stroke=0.1)
, 
ggplot()+
lims(x=xlims, y=ylims)+
geom_path(data=mdspoints[mdspoints$t %in% c("Apr16", "Oct16"),], aes(x=NMDS1, y=NMDS2, group=Reef), col="grey", arrow=arrow(length=unit(0.5, "mm")), size=0.4)+
geom_path(data=mdspoints[mdspoints$t %in% c("Apr16","Apr24"),], aes(x=NMDS1, y=NMDS2, group=Reef), col="black", arrow=arrow(length=unit(0.5, "mm"), ends="last"))
,
ggplot()+
lims(x=xlims, y=ylims)+
geom_path(data=mdspoints[mdspoints$t %in% c("Apr16","Apr24"),], aes(x=NMDS1, y=NMDS2, group=Reef), col="red", arrow=arrow(length=unit(1, "mm")))
,
ggplot()+geom_segment(data=mdsvectors, aes(x=0, xend=MDS1, y=0, yend=MDS2), col="grey")+
lims(x=xlims, y=ylims)+
geom_text(data=mdsvectors, aes(MDS1, MDS2, label=lab2), hjust=ifelse(mdsvectors$MDS1 >0, 0, 1), size=2.5, fontface="bold")
)

# --------------------------------- # fig 1

source("figs/fig1.R")
fig1

# --------------------------------- # supplement

source("figs/supplement/figS1 cov change.R")
FigS1

# --------------------------------- # model bleaching

library("betareg")
library("mgcv")

summary(glm(pbleach~max.dhw, family="quasibinomial", data=tdf, weights=tdf$Ncoral)) 
summary(betareg(pbleach ~ max.dhw, data=tdf, link="logit")) # sigmoidal?
summary(gam(pbleach~s(max.dhw, k=5), data=tdf))

# binomial
bidat <- sites[sites$Zone=="Crest", ]
bidat <- bidat[!is.na(bidat$max.dhw),]
bimod <- glm(pbleach~max.dhw, family="quasibinomial", data=bidat, weights=bidat$Ncoral)
summary(bimod)
bifit <- data.frame(max.dhw = seq(min(bidat$max.dhw), max(bidat$max.dhw), 0.1))
bifit$fit <- predict(bimod, bifit, type="response")

ggplot()+geom_line(data=bifit, aes(max.dhw, fit))+geom_point(data=bidat, aes(max.dhw, pbleach, col=region2))


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
mod.dat <- mod.dat[!is.na(mod.dat$max.dhw),]
mod.dat <- mod.dat[!is.na(mod.dat$pbleach),]
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
fit.long <- melt(curves, id.var=c("predictor", "data", "Zone"))


ggplot(fit.long[fit.long$data=="tdf" & fit.long$variable %in% c("lm1fit", "expfit","gam1.5", "logfit", "betafit", "logisfit", "bifit"),], aes(x=predictor, y=value*100))+
geom_line(aes(col=Zone))+
facet_wrap(~variable, scales="free", ncol=1)+
theme_classic()+theme(strip.background=element_blank(), axis.text=element_blank())

crestdat <- sres[sres$Zone=="Crest",]

bimodX <- glm(pbleach~max.dhw, family="quasibinomial", data=sres[sres$Zone=="Crest",], weights=sres[sres$Zone=="Crest","Ncoral"]) 
fit.datX <- data.frame(max.dhw = seq(min(sres$max.dhw), max(sres$max.dhw), 0.1))
fit.datX$bifit <- predict(bimodX,fit.datX, type="response", se=T)$fit
fit.datX$bise <- predict(bimodX,fit.datX, type="response", se=T)$se.fit
head(fit.datX)

ggplot()+
geom_point(data=crestdat, aes(max.dhw, pbleach))+
geom_line(data=fit.datX, aes(max.dhw, bifit))+
geom_ribbon(data=fit.datX, aes(max.dhw, ymax=(bifit+(bise*2)), ymin=(bifit-bise*2)), alpha=0.1)

curvesCrest <- curves[curves$data=="sites" & curves$Zone=="Crest",]


# --------------------------------- #  2016 bleaching

library("investr")
bl16 <- read.csv("data/data2016/bleaching2016.csv")
head(bl16)

j.av <- aggregate(BleachDead~Reef, bl16, mean)
j.av$DHWs <- aggregate(DHWs~Reef, bl16, mean)$DHWs

mod2016 <- data.frame(dhw=seq(2, 12, 0.5))
mod2016$y <- (48.6 * log(mod2016$dhw)  - 21.6)  # original model

mod16 <- nls(BleachDead ~ b*log(DHWs) + c, data=j.av[!j.av$Reef=="12-059",], start=list(b=48.6, c=-21.6))
summary(mod16)

new.data <- data.frame(DHWs=seq(2, 12, 0.2))
fit16 <- cbind(new.data, data.frame(predFit(mod16, newdata = new.data, interval = "confidence", level= 0.95)))

reefs <- aggregate(pbleach~Reef, sites[sites$Zone=="Crest",], mean)
reefse <- aggregate(pbleach~Reef, sites[sites$Zone=="Crest",],se)
reefmort <- aggregate(pdead~Reef, sites[sites$Zone=="Crest",],se)
reefdhw <-aggregate(max.dhw~Reef, sites[sites$Zone=="Crest",], mean, na.rm=T)
reefs$max.dhw <- reefdhw$max.dhw[match(reefs$Reef, reefdhw$Reef)]
reefs$se <- reefse$pbleach[match(reefs$Reef, reefse$Reef)]
reefs$pdead <- reefmort$pdead[match(reefs$Reef, reefmort$Reef)]

ggplot()+
geom_point(data=j.av, aes(DHWs, BleachDead), shape=21, size=2)+
geom_line(data=mod2016, aes(dhw, y))+
geom_ribbon(data=fit16, aes(x=DHWs, ymin=lwr, ymax=upr), alpha=0.2)+
geom_point(data=crestdat, aes(max.dhw, pbleach*100, col=above2016),  size=1, col="grey")+
geom_point(data=reefs, aes(max.dhw, pbleach*100), size=2)+
geom_line(data=fit.datX, aes(max.dhw, bifit*100))+
geom_ribbon(data=fit.datX, aes(max.dhw, ymax=(bifit+(bise*2))*100, ymin=(bifit-bise*2)*100), alpha=0.1)+
theme_classic()

# --------------------------------- above or below 2016 curve?

crestdat <- sres[sres$Zone=="Crest",]
tdf$response.2016 <- ((48.6 * log(tdf$max.dhw)  - 21.6))/100
tdf$dev2016 <- tdf$pbleach - tdf$response.2016

ggplot(tdf[tdf$Zone=="Crest",], aes(dev2016, region2, group=Reef))+geom_boxplot()+geom_vline(xintercept=0)

head(fit16)
crestdat$pbleach2 <- crestdat$pbleach * 100
newdat <- data.frame(siteID =crestdat$siteID, DHWs = crestdat$max.dhw)
p <- cbind(newdat, data.frame(predFit(mod16, newdata = newdat, interval = "confidence", level= 0.95)))
crestdat[,c("fit", "lwr", "upr")] <- p[match(crestdat$siteID, p$siteID),c("fit", "lwr", "upr")]
crestdat$class <- ifelse(crestdat$pbleach2 > crestdat$upr, "above", ifelse(crestdat$pbleach2 < crestdat$lwr,"below", "consistent"))
table(crestdat$class)

freqz <- data.frame(table(crestdat[,c("class", "region2")]))

above1 <- ggplot(freqz,aes(y=region2, x=Freq, fill=class),)+geom_bar( stat="Identity")+
scale_fill_manual(values=c("red", "blue", "grey"))+
theme_classic()
above1

crestdat$diff <- crestdat$pbleach2 - crestdat$fit

# --------------------------------- # who's above?

crestdat2 <- crestdat #tdf[tdf$Zone %in% "Crest",]
check16 <- na.omit(data.frame(DHWs=crestdat2$max.dhw, Reef=crestdat2$Reef, region2=crestdat2$region2, pbleach=crestdat2$pbleach*100))
dev16 <- cbind(check16, data.frame(predFit(mod16, newdata = check16, interval = "confidence", level= 0.95)))
head(dev16)

dev16$diff <- dev16$pbleach-dev16$fit
dev16$diffL <- dev16$lwr - dev16$fit
dev16$diffU <- dev16$upr - dev16$fit

head(dev16)

# A 95% confidence interval with base R only (no extra packages)
mean_ci <- function(x) {
  n   <- length(x)
  m   <- mean(x)
  se  <- sd(x) / sqrt(n)
  err <- qt(0.975, df = n - 1) * se
  data.frame(y = m, ymin = m - err, ymax = m + err)
}

t.test(dev16[dev16$region2 %in% "Gladstone", "diff"], mu=0)
t.test(dev16[dev16$region2 %in% "Mackay", "diff"], mu=0)
t.test(dev16[dev16$region2 %in% "Townsville", "diff"], mu=0)
t.test(dev16[dev16$region2 %in% "Cairns", "diff"], mu=0)
t.test(dev16[dev16$region2 %in% "Cooktown", "diff"], mu=0)
t.test(dev16[dev16$region2 %in% "Cape Grenville", "diff"], mu=0)

dev16$region2 <- factor(dev16$region2, levels=rev(regions))

diffplot <- ggplot()+
geom_segment(data=dev16, aes(x=region2, xend=region2, y=diffU, yend=diffL), alpha=0.5, size=3, col="grey")+
geom_point(data=crestdat, aes(region2, diff), size=0.5, col="slategrey")+
#scale_x_reverse()+
geom_hline(yintercept=0)+
stat_summary(data=dev16, aes(region2, diff), fun.data = mean_ci, geom = "pointrange", colour = "#DD3497")+
labs(y="Difference in % bleaching in 2024\nfrom the 2016 prediction", x="Region")+
stat_summary(data=dev16, aes(region2, diff))+
coord_flip()+theme_classic()
diffplot

# --------------------------------- recent mortality

betamod3 = glm(pdead ~ max.dhw, family="quasibinomial", data=crestdat)
new.data2 <- data.frame(max.dhw = seq(min(crestdat$max.dhw), max(crestdat$max.dhw), 0.1))
fit.dat2 <- cbind(new.data2, predict(betamod3, new.data2))
fit.dat2 <- cbind(new.data2, data.frame(predict(betamod3, new.data2, se.fit = TRUE, type="response", interval = "confidence", level = 0.5)))
head(fit.dat2 )
summary(betamod3)

j.av2 <- aggregate(Mortality~Reef, bl16, mean)
j.av2$DHWs <- aggregate(DHWs~Reef, bl16, mean)$DHWs
j.av2$N <- aggregate(Total~Reef, bl16, mean)$Total
j.av2$Mort <- j.av2$Mortality/100
betamod5 <- glm(Mort~DHWs, family="quasibinomial", data=j.av2, weights=j.av2$N) #binomial also works # rsq
new.data3 <- data.frame(DHWs = seq(min(j.av2$DHWs), max(j.av2$DHWs), 0.1))
summary(betamod5)
fit.dat3 <- cbind(new.data3, data.frame(predict.glm(betamod5, new.data3, type="response", se.fit = TRUE, interval = "confidence", level = 0.5)))

col16 <- "#5B7553"
head(fit.dat2)
head(fit.dat3)

ggplot()+
geom_point(data=j.av2, aes(DHWs, Mortality),col=col16, shape=4, size=1, stroke=0.3)+
geom_line(data=fit.dat3, aes(x=DHWs, y=(fit*100)-2), col=col16)+
geom_ribbon(data=fit.dat3, aes(x=DHWs, ymax=((fit+se.fit)*100)-2, ymin=((fit-se.fit)*100)-2), fill=col16, col=NA, alpha=0.2)+
geom_point(data=sites[sites$Zone=="Crest",], aes(max.dhw, pdead*100), shape=21, fill="black", size=0.5)+
geom_line(data=fit.dat2, aes(x=max.dhw, y=fit*100))+
geom_ribbon(data=fit.dat2, aes(x=max.dhw, ymax=((fit+se.fit)*100), ymin=((fit-se.fit)*100)), col=NA, alpha=0.2)+
theme_classic()

comb <- rbind(data.frame(dhw=j.av2$DHW, bl=j.av$BleachDead, mort=j.av2$Mortality/100, yr="a2016"), data.frame(dhw=crestdat$max.dhw, bl=crestdat$pbleach*100, mort=crestdat$pdead, yr="b2024"))
summary(betareg(mort~dhw+yr, comb))
summary(lm(mort~dhw+yr, comb))
summary(glm(mort~dhw+yr, comb, family="quasibinomial"))

# --------------------------------- # composition 2024

comp3 <- all[all$t=="Apr24",]

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
mdspoints2$transect <- gsub(" Apr24", "", rownames(mdspoints2))
mdsvectors2<- as.data.frame(mds2$species)
tdf[,c("NMDS1.2", "NMDS2.2")] <- mdspoints2[match(tdf$Transect_Code, mdspoints2$transect), c("NMDS1", "NMDS2")]

plot_grid(
ggplot()+geom_segment(data=mdsvectors2, aes(x=0, xend=MDS1, y=0, yend=MDS2), size=0.2, col="slategrey")+
geom_text(data=mdsvectors2, aes(MDS1, MDS2, label=rownames(mdsvectors2)))
,ggplot()+ geom_point(data=tdf, aes(NMDS1.2, NMDS2.2, fill=pbleach*100), shape=21, stroke=0.1, size=1.2)+
scale_fill_viridis())

# --------------------------------- composition vs bleaching

tres[,c("NMDS1.2", "NMDS2.2")] <- mdspoints2[match(tres$Transect_Code, mdspoints2$transect), c("NMDS1", "NMDS2")]
head(tres)

mod.dat2 <- tres
mod.dat2 <- na.omit(mod.dat2[,c("pbleach","Zone", "poc","por", "coral_cov", "acro","tab", "NMDS1.2", "NMDS2.2", "betamod")])

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

ggplot(tres, aes(acro, betamod, col=Zone))+geom_point()+geom_smooth(method="lm")+scale_x_sqrt()+facet_wrap(~Zone)

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

comp3 <- all 
comp3 <- comp3[comp3$zone=="Crest",]
comp3 <- comp3[comp3$align %in% c( "tabular_Acropora","other_Acropora", "staghorn_Acropora"),] 
comp3 <- aggregate(cov~ID+site+reef+t, comp3, sum)
comp3R <- aggregate(cov~reef+t, comp3, mean)
comp3R$se <- aggregate(cov~reef+t, comp3, se)$cov
head(comp3R)

acro <- dcast(comp3R, reef~t, value.var='cov')
acro$change <- acro$Apr24 - acro$Oct16
acro

acro$reef[acro$reef=="Ribbon8"] <- "Ribbon 8"
acro$reef[acro$reef=="NthDirection"] <-"North Direction" 

df <- tres
df$resids <- df$betamod
reefs <- aggregate(resids~Reef, df[df$Zone=="Crest",], mean) #[sites$Zone=="Crest",]
reefs$resids.se <- aggregate(resids~Reef, df[df$Zone=="Crest",], se)$resids #[sites$Zone=="Crest",]

acro$resids <- reefs$resids[match( acro$reef, reefs$Reef)]
acro$resids.se <- reefs$resids.se[match(acro$reef, reefs$Reef)]
acro$match <- reefs$Reef[match( acro$reef, reefs$Reef)]
acro <- na.omit(acro)

head(crestdat)
reef16 <- aggregate(.~Reef, crestdat[,c("Reef", "response.2016", "pbleach")], mean)
reef16$diff <- reef16$pbleach -reef16$response.2016
hist(reef16$diff)

ggplot()+
geom_hline(yintercept=0, col="grey")+geom_vline(xintercept=0, col="grey")+
geom_point(data=acro, aes(x=change, y=resids), size=1)+
geom_smooth(data=acro, aes(x=change, y=resids), method="lm", se=F, col="red", size=0.35, formula=y~poly(x,1))+
labs(x="Change in % Acropora\n(2016-2024)", y="deviation from expected\nbleaching (2024)")

# --------------------------------- # fig 2

source("figs/fig2.R")
fig2

# --------------------------------- # supplement

source("figs/supplement/figS4 nmds.R")
figS4

source("figs/supplement/figS5 hughes21.R")
figS5

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

time.all2 <- unique(df.use[,c( "X", "Y", "gridID")])
time.all2$last <- time.all$last[match(time.all2$gridID, time.all$gridID)]
time.all2$last[is.na(time.all2$last)]<-"8+"
time.all2 


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


ggplot(sres, aes(freq2, last2))+geom_hex()

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
scale_radius()+
facet_wrap(~label)+
scale_fill_distiller(palette="Spectral")+
scale_y_continuous(expand=c(0,0))+scale_x_continuous(expand=c(0,0))+
theme_bw()+theme(strip.background=element_blank(), strip.text=element_text(size=8, face="bold"), legend.title=element_blank(), legend.text=element_blank())
heatplot

# --------------------------------- # fig3

source("figs/fig3.R")
fig3

# --------------------------------- # supplement

source("figs/supplement/FigS7 frequency.R")
figS7

# --------------------------------- # long-term mortality

lmort <- read.csv("data/mortality.csv")
lmort$year <- factor(lmort$year, levels=c("2016", "2024"))
head(lmort)

m16 <- lmort[lmort$year %in% "2016",]
m24 <- lmort[lmort$year %in% "2024",] # sitesC

m24 <- m24[m24$zone %in% "Crest",] 
lmort <- lmort[lmort$zone %in% "Crest",]

labs=c(-90, -70, -50, -30, 0, 30)
brks <- 0.432 * log(labs+99.91) - 1.991 
brks

plot_grid(ggplot(lmort, aes(x=year, Achange))+geom_boxplot(),
ggplot(lmort, aes(x=year, bl))+geom_boxplot())

t.test(bl~year, lmort)
t.test(Achange~year, lmort)

# --------------------------------- # mortality mods

library("mgcv")

# Bleaching-mortality

mod16.1 <- gam(change_use~s(bl, k=5), data=m16)
summary(mod16.1)
fit16.1 <- data.frame(bl = seq(min(m16$bl), max(m16$bl), 0.1))
fit16.1$fit <- predict(mod16.1, fit16.1, type="response", se=T)$fit
fit16.1$se <- predict(mod16.1, fit16.1, type="response", se=T)$se

mod24.1 <- gam(change_use~s(bl, k=4), data=m24)
summary(mod24.1)
fit24.1 <- data.frame(bl = seq(min(m24$bl), max(m24$bl), 0.01))
fit24.1$fit <- predict(mod24.1, fit24.1, type="response", se=T)$fit
fit24.1$se <- predict(mod24.1, fit24.1, type="response", se=T)$se

ggplot()+
geom_hline(yintercept=0, size=0.1)+
geom_line(data=fit16.1, aes(bl, fit))+
geom_ribbon(data=fit16.1, aes(x=bl, ymin=fit-(se*1.95), ymax=fit+(se*1.95)), alpha=0.2)+
geom_point(data=lmort, aes(bl, change_use, col=year, shape=year), size=1, stroke=0.3)+
geom_line(data=fit24.1, aes(bl, fit), col="black")+
geom_ribbon(data=fit24.1, aes(bl, ymin=fit-(se*1.95), ymax=fit+(se*1.95)), alpha=0.2)+
scale_y_continuous(breaks=brks, labels=labs)+
labs(x="Prop. colonies bleached (%)", y="Change in coral cover (%)")

# DHW -mortality

mod16.2 <- gam(change_use~s(dhw, k=5), data=m16)
summary(mod16.2)
fit16.2 <- data.frame(dhw = seq(min(m16$dhw), max(m16$dhw), 0.1))
fit16.2$fit <- predict(mod16.2, fit16.2, type="response", se=T)$fit
fit16.2$se <- predict(mod16.2, fit16.2, type="response", se=T)$se

mod24.2 <- gam(change_use~s(dhw, k=3), data=m24)
summary(mod24.2)
fit24.2 <- data.frame(dhw = seq(min(m24$dhw), max(m24$dhw), 0.1))
fit24.2$fit <- predict(mod24.2, fit24.2, type="response", se=T)$fit
fit24.2$se <- predict(mod24.2, fit24.2, type="response", se=T)$se

ggplot()+
geom_hline(yintercept=0, size=0.1)+
#geom_smooth(data=lmort, aes(dhw, change_use, col=year), method="lm", formula=y~poly(x,2), size=0.4, show.legend=FALSE)+
geom_line(data=fit16.2, aes(dhw, fit))+
geom_ribbon(data=fit16.2, aes(x=dhw, ymin=fit-(se*1.95), ymax=fit+(se*1.95)), alpha=0.2)+
geom_line(data=fit24.2, aes(dhw, fit), col="black")+
geom_ribbon(data=fit24.2, aes(x=dhw, ymin=fit-(se*1.95), ymax=fit+(se*1.95)), alpha=0.2)+
geom_point(data=lmort, aes(dhw, change_use, col=year, shape=year), size=1, stroke=0.3)+
scale_y_continuous(breaks=brks, labels=labs)+
labs(x="DHW (°C Weeks)", y="Change in coral cover (%)")

# Acropora- Mortality

#ggplot(data=lmort, aes(acro/cov1, change_use, col=year, shape=year),)+geom_point( size=1, stroke=0.3)+geom_smooth()

m16$acro2 <- m16$acro/m16$cov1
m24$acro2 <- m24$acro/m24$cov1
lmort$acro2 <- lmort$acro/lmort$cov1

#GAMS
mod16.3 <- gam(change_use~s(acro2, k=3), data=m16)
summary(mod16.3)
fit16.3 <- data.frame(acro2 = seq(min(m16$acro2), max(m16$acro2), 0.01))
fit16.3$fit <- predict(mod16.3, fit16.3, type="response", se=T)$fit
fit16.3$se <- predict(mod16.3, fit16.3, type="response", se=T)$se

mod24.3 <- gam(change_use~s(acro2, k=5), data=m24)
summary(mod24.3)
fit24.3 <- data.frame(acro2 = seq(0, max(m24$acro2, na.rm=T), 0.01))
fit24.3$fit <- predict(mod24.3, fit24.3, type="response", se=T)$fit
fit24.3$se <- predict(mod24.3, fit24.3, type="response", se=T)$se


ggplot()+
geom_hline(yintercept=0, size=0.1)+
geom_line(data=fit16.3, aes(acro2, fit))+
geom_ribbon(data=fit16.3, aes(x=acro2, ymin=fit-(se*1.95), ymax=fit+(se*1.95)), alpha=0.2)+
geom_line(data=fit24.3, aes(acro2, fit), col="black")+
geom_ribbon(data=fit24.3, aes(x=acro2, ymin=fit-(se*1.95), ymax=fit+(se*1.95)), alpha=0.2)+
geom_point(data=lmort, aes(acro2, change_use, col=year, shape=year), size=1, stroke=0.3)+
#geom_smooth(data=lmort, aes(cov1, change_use, col=year), method="lm", formula=y~poly(x,1), size=0.4,show.legend=FALSE)+
labs(x=expression(paste("Initial ", italic("Acropora"), " cover")), y="Change in coral cover (%)")+
scale_y_continuous(breaks=brks, labels=labs)


######## NEW TLAST

head(m24)
head(sites)
m24$siteID <- paste(m24$reef, m24$site, "Crest")
m24$tlast <- tdf$tlast[match(m24$siteID, tdf$siteID)]
m24$tlast[is.na(m24$tlast)]<-0

m24$tlast2 <- ifelse(m24$tlast==0, "no events",ifelse(m24$tlast==4,"four",ifelse(m24$tlast==7,"seven",NA)))
m24$tlast2 <- factor(m24$tlast2, levels=c("no events", "four", "seven"))

table(sres[,c("region2", "last")])

m24[,c("tlast", "tlast2")]

ggplot(m24, aes(as.factor(tlast2), Lchange))+
geom_boxplot(outlier.size=0.1, fill="grey90", size=0.2)+
geom_jitter(aes(fill=dhw), shape=21, height=0, width=0.1, stroke=0.1)+
scale_fill_viridis(option="B")+
scale_y_continuous(breaks=brks, labels=labs)

# --------------------------------- # fig 4

source("figs/fig4.R")
fig4

# --------------------------------- # supplement

source("figs/supplement/FigS3 zones.R")
FigS3

source("figs/supplement/FigS6 residuals.R")
figS6

source("figs/supplement/figS8 sensitivity.R")
figS8


