

rm(list = ls())

library("ggplot2")
library("reshape2")
library("cowplot")
library("viridis")
library("vegan")
library("sf")
se <- function(x) sqrt(var(x)/length(x))

# --------------------------------- # survey data

tdf <- read.csv("data/transects.csv")
belt[belt$Transect_Code %in% c("124_B2_C2", "LIZ_B4_S3"),] # Problem transects! (No corals no overlap!)
tdf <- tdf[!tdf$Transect_code %in% c("124_B2_C2", "LIZ_B4_S3"),]

belt <- read.csv("data/original/bleaching2024.csv") # long
belt$X20.40cm[belt$X20.40cm %in% c("", "`")] <- NA
belt$X20.40cm <- as.numeric(belet$X20.40cm)
belt[belt$Genus=="",] # 4 colonies... 
belt <- belt[!belt$Genus=="",]
bcols <- c("Juv...5cm.","X.20cm", "X20.40cm", "X40.60cm", "X.60cm")
belt <- subset(belt, select=-c(Total.no..Adults)) # sum minus juvs. 
belt <- melt(belt, id.var=colnames(belt)[!colnames(belt) %in% c(bcols)], variable.name="size")
belt$value[is.na(belt$value)] <- 0
head(belt)

btax <- read.csv("data/info/bleaching_taxa.csv")
belt$group <- btax$Group[match(belt$Genus, btax$Genus)]
head(belt)

# --------------------------------- # heatmaps

regions <- c("Capricorn Bunkers", "Hydrographers Passage", "Cape Bowling Green", "Cairns", "Lizard", "Princess Charlotte Bay", "Cape Grenville")

source("figs/map.R")
#mapplot
#aboveNplot
#map24d

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

plot_grid(ggplot(freq, aes(X, Y, col=aboveN))+geom_point(size=0.5)+scale_colour_viridis(),
ggplot(time, aes(X, Y, col=last))+geom_point(size=0.5)+scale_colour_viridis(option="B", direction=-1))

fdat <- data.frame(table(freq$aboveN))
fdat$p <- fdat$Freq / sum(table(freq$aboveN)) *100
tdat <- data.frame(table(time$last))
tdat$p <- tdat$Freq / sum(table(time$last)) *100

plot_grid(ggplot(fdat, aes(x=Var1, y=p))+geom_bar(stat="identity"),
ggplot(tdat, aes(x=Var1, y=p))+geom_bar(stat="identity"))

# --------------------------------- # heat stress history (surveyed)

tyears <- read.csv("data/noaa_sst/longtermgrids.csv")

# frequency
maxdhw <- aggregate(dhw~year+gridID, tyears, max) 
maxdhw$aboveN <- ifelse(maxdhw$dhw > 6,1, 0) 
duse <- maxdhw[maxdhw$year %in% c(2016, 2017, 2020, 2022, 2024),]
sumN <- aggregate(aboveN~gridID, duse, sum)
tdf$sumN <-sumN$aboveN[match(tdf$gridID, sumN$gridID)]
tuse <- maxdhw[maxdhw$aboveN==1,]
tuse <- tuse[tuse$year %in% c(2016, 2017, 2020, 2022),]
tlast <- aggregate(year~gridID, tuse, max)
tlast$tlast <- 2024 - tlast$year
tdf$tlast <- tlast$tlast[match(tdf$gridID, tlast$gridID)]

fdat2 <- data.frame(table(tdf$sumN) / sum(table(tdf$sumN)) * 100)
tdat2 <- data.frame(table(tdf$tlast) / sum(table(tdf$tlast)) * 100)

plot_grid(ggplot(fdat2, aes(x=Var1, y=Freq))+geom_bar(stat="identity"),
ggplot(tdat2, aes(x=Var1, y=Freq))+geom_bar(stat="identity"))

# --------------------------------- # bleaching categories

belt$bcat <- ifelse(belt$Coral.Health %in% c("H - Healthy (<5% Recent Mortality)", "h - Healthy (<5% Recent Mortality)", "Health",""), "none",  ifelse(belt$Coral.Health=="P - Pale", "pale",
ifelse(belt$Coral.Health == "A - <50% Bleached", "bl_a",  ifelse(belt$Coral.Health == "B - 50-99% Bleached", "bl_b", ifelse(belt$Coral.Health == "C - 100% Bleached", "bl_c",  ifelse(belt$Coral.Health == "D - 5-50% Recent mortality", "bld_d",  ifelse(belt$Coral.Health == "E - 50-99% Recent Mortality", "bld_e",  ifelse(belt$Coral.Health == "F - 100% Recent Mortality", "bld_f", NA))))))))

belt$bcat<- factor(belt$bcat, levels=c("none", "pale", "bl_a", "bl_b", "bl_c", "bld_d", "bld_e", "bld_f"))
unique(belt$bcat)

# ggplot(belt, aes(value,Transect_Code, fill=bcat))+geom_bar(stat="identity")+ scale_fill_viridis(discrete=T)+facet_wrap(~Reef, scales="free_y")+theme(axis.text.y=element_blank())

# --------------------------------- # broad bleaching categories

belt$bleachingYN <- ifelse(belt$bcat %in% c("bl_a", "bl_b", "bl_c", "bld_e", "bld_d", "bld_f"), "y", ifelse(belt$bcat %in% c("pale", "none"), "n", NA))
unique(belt$bleachingYN)

belt$palebleach <- ifelse(belt$bcat %in% c("pale","bl_a", "bl_b", "bl_c", "bld_e", "bld_d", "bld_f"), "y", ifelse(belt$bcat %in% c("none"), "n", NA))
unique(belt$palebleach)

belt$severe <- ifelse(belt$bcat %in% c("bl_b", "bl_c", "bld_e", "bld_d", "bld_f"), "y", ifelse(belt$bcat %in% c("bl_a","pale", "none"), "n", NA)) # all except A? 
unique(belt$severe)

belt$dying <- ifelse(belt$bcat %in% c("bld_e", "bld_d", "bld_f"), "y", ifelse(belt$bcat %in% c("bl_b", "bl_c", "bl_a","pale", "none"), "n", NA)) # all except A? 
unique(belt$dying)

# no soft coral / no juveniles

# subset
hc1 <- belt[belt$group=="HC",] # hard coral
hcsc <- belt[belt$group %in% c("HC", "SC"),] # hard/soft coral
adults1 <- hc1[!hc1$size=="Juv...5cm.",]
adults_sc <- hcsc[!hcsc$size=="Juv...5cm.",]
juvs1 <- hc1[!hc1$size=="Juv...5cm.",]

# total colonies
ncoral <- aggregate(value~Transect_Code, adults1, sum)
ncoralSC <- aggregate(value~Transect_Code, adults_sc, sum)
#njuvs <-  aggregate(value~Transect_Code, juvs1, sum)
tdf$Ncoral <- ncoral$value[match(tdf$Transect_code, ncoral$Transect_Code)]
tdf$NcoralSC <- ncoralSC$value[match(tdf$Transect_code, ncoralSC$Transect_Code)]
head(tdf)

# total bleached
# nbleachSC <- aggregate(value~Transect_Code, adults_sc[adults_sc$palebleach =="y",], sum)
# nsev <- aggregate(value~Transect_Code, adults1[adults1$severe =="y",], sum)
nbleach <- aggregate(value~Transect_Code, adults1[adults1$palebleach =="y",], sum)
tdf$nbleach <- nbleach$value[match(tdf$Transect_code, nbleach$Transect_Code)]
tdf$nbleach[is.na(tdf$nbleach)] <- 0
length(unique(nbleach$Transect_Code))

# total dead
ndead <- aggregate(value~Transect_Code, adults1[adults1$dying=="y",], sum)
tdf$ndead <- ndead$value[match(tdf$Transect_code, ndead$Transect_Code)]
tdf$ndead[is.na(tdf$ndead)] <- 0
length(unique(ndead$Transect_Code))

# proportions
tdf$pdead <- tdf$ndead/tdf$Ncoral
tdf$pbleach <- tdf$nbleach/tdf$Ncoral



# --------------------------------- #  2016 bleaching

bl16 <- read.csv("data/original/bleaching2016.csv")
head(bl16)

ggplot(bl16, aes(DHWs, Bleached))+geom_point()

j.av <- aggregate(BleachDead~Reef, bl16, mean)
j.av$DHWs <- aggregate(DHWs~Reef, bl16, mean)$DHWs
nrow(j.av)

mod2016 <- data.frame(dhw=seq(2, 12, 0.5))
mod2016$y <- (48.6 * log(mod2016$dhw)  - 21.6)  # 21.6

mod16 <- nls(BleachDead ~ b*log(DHWs) + c, data=j.av[!j.av$Reef=="12-059",], start=list(b=48.6, c=-21.6))
summary(mod16)

library("investr")

new.data <- data.frame(DHWs=seq(2, 12, 0.2))
fit16 <- cbind(new.data, data.frame(predFit(mod16, newdata = new.data, interval = "confidence", level= 0.95)))

j.av2 <- aggregate(Mortality~Reef, bl16, mean)
j.av2$DHWs <- aggregate(DHWs~Reef, bl16, mean)$DHWs
nrow(j.av2)


ggplot()+
geom_point(data=j.av, aes(DHWs, BleachDead), shape=21, size=2)+
geom_line(data=mod2016, aes(dhw, y))

ggplot()+geom_point(data=j.av2, aes(DHWs, Mortality))

# original model
#mod2016 <- data.frame(dhw=seq(2, 12, 0.5))
#mod2016$y <- (48.6 * log(mod2016$dhw)  - 21.6) 
#logmod16 <- nls(BleachDead~ b*log(DHWs) + c, data=mod.dat16, start=list(b=48.6, c=-21.6))

# --------------------------------- # composition 2024

comp <- read.csv("data/pit.csv") 
head(comp)

tax.sums <- aggregate(cov~variable+group, comp, sum)  
rare <- tax.sums$variable[tax.sums$cov < 60 & tax.sums$group=="HC"]
rare

comp2 <- comp
comp2$variable <- ifelse(comp2$variable %in% c(rare, "Other.Scleractinia", "Heliopora","Millepora"), "Other.Coral",comp2$variable)
comp2$variable <- ifelse(comp2$variable %in% c("Chlorodesmis"), "Macroalgae",comp2$variable)
comp2$variable <- ifelse(comp2$group %in% c("SC", "ZO"), "Soft.Coral", comp2$variable)
comp2 <-  comp2[!comp2$variable == "Sand..Rubble", ]
unique(comp2$variable)
comp2$label <- comp$label[match(comp2$variable, comp$variable)]
comp2 <- aggregate(cov~Transect+variable, comp2, sum)

mdsdat2 <- acast(comp2, Transect~variable, value.var="cov") 
mdsdat2<- mdsdat2

mds2<-metaMDS(mdsdat2, k=3, distance="bray", autotransform=FALSE, trymax=1000)
# saveRDS(mds2, file = "output/mds1000sqrt2.rds") 
mds2 <- readRDS("data/mds1000sqrt2.rds") # load to skip mds processing
mdspoints2<-data.frame(scores(mds2)$sites)
mdsvectors2<- as.data.frame(mds2$species)
nrow(mdspoints2)
tdf[,c("NMDS1.2", "NMDS2.2")] <- mdspoints2[match(tdf$Transect_code, rownames(mdspoints2)), c("NMDS1", "NMDS2")]


plot_grid(
ggplot()+geom_segment(data=mdsvectors2, aes(x=0, xend=MDS1, y=0, yend=MDS2), size=0.2, col="slategrey")+
geom_text(data=mdsvectors2, aes(MDS1, MDS2, label=rownames(mdsvectors2)))
,ggplot()+ geom_point(data=tdf, aes(NMDS1.2, NMDS2.2, fill=pbleach*100), shape=21, stroke=0.1, size=1.2))

# ------------------------------------------------ # Composition metrics 2024
 
# check overlap
mat <- data.frame(codes = unique(c(comp$Transect, tdf$Transect_code)))
nrow(mat)
mat$comp <- comp$Transect[match(mat$code, comp$Transect)]
mat$belt <- tdf$Transect_code[match(mat$code, tdf$Transect_code)]
mat # 16 comp but no belt... 2 belt but no comp

# coral cover
hc2 <- comp[comp$group %in% c("HC"),] # hard coral
t_cov <- aggregate(cov~Transect+Total.CORAL, hc2, sum)
tdf$coral_cov <- t_cov$cov[match(tdf$Transect_code, t_cov$Transect)]

# genera
unique(comp2$variable)
acro <- c("Acropora...Tabular",  "Acropora...Staghorn", "Acropora...other", "Isopora")
pocil <- c("Pocillopora", "Seriatopora", "Stylophora")
porit <- c("Porites...Branching", "Porites....Massive")

# acropora cover
acrocov <- aggregate(cov~Transect, comp2[comp2$variable %in% acro,], sum)
tdf$acro <- acrocov$cov[match(tdf$Transect_code, acrocov$Transect)]

# pocillopora cover
poccov <- aggregate(cov~Transect, comp[comp$variable %in% pocil,], sum)
tdf$poc<- poccov$cov[match(tdf$Transect_code, poccov$Transect)]
tdf$pocR<- tdf$poc/tdf$coral_cov

# porites cover
porcov <- aggregate(cov~Transect, comp2[comp2$variable %in% porit,], sum)
tdf$por <- porcov$cov[match(tdf$Transect_code, porcov$Transect)]
tdf$porR <- tdf$por/tdf$coral_cov

# tabular cover
tabcov <- aggregate(cov~Transect, comp2[comp2$variable %in% c("Acropora...Tabular"),], sum)
tdf$tab <- tabcov$cov[match(tdf$Transect_code, tabcov$Transect)]
tdf$tabR <- tdf$tab/tdf$coral_cov

head(tdf)

# --------------------------------- # 2024 bleaching


head(tdf)
#tdf$
#sites <- aggregate(cbind(Ncoral, NcoralSC, pbleach, pdead, coral_cov, acro, poc, por, tab) ~ Region + Reef + Site + Zone + GPS.S + GPS.E + coordID + gridID + siteID, data=tdf, mean)
#head(sites)

tdf$siteID <- paste(tdf$Reef, tdf$Site, tdf$Zone)
sites <- unique(tdf[,c("Region","Reef", "Site", "siteID", "Zone", "gridID", "GPS.S", "GPS.E")])
pbleach.av <- aggregate(pbleach~siteID, tdf, mean)
sites$pbleach <- pbleach.av$pbleach[match(sites$siteID, pbleach.av$siteID)]
pdead.av <- aggregate(pdead~siteID, tdf, mean)
sites$pdead <-pdead.av$pdead[match(sites$siteID, pdead.av$siteID)]
head(sites)


tyears <- read.csv("data/noaa_sst/longtermgrids.csv")
tyears$time2 <- as.Date(tyears$time2)

head(dhw)
dhw24 <- dhw[dhw$year==2024,]

max.dhw <- aggregate(dhw~gridID, dhw24, max)
tdf$max.dhw <- max.dhw$dhw[match(tdf$gridID, max.dhw$gridID)]
sites$max.dhw <- max.dhw$dhw[match(sites$gridID, max.dhw$gridID)]

ggplot(sites, aes(x=max.dhw, y=pbleach))+geom_point()+facet_wrap(~Zone)


# --------------------------------- # model bleaching

library("betareg")
library("mgcv")

# binomial
#bidat <- tdf[tdf$Zone=="Crest",]
bidat <- sites[sites$Zone=="Crest", ]
bidat$pbleach
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


tdf1 <- rbind(resids[["tdf Crest"]], resids[["tdf Slope"]])
sites1 <- rbind(resids[["sites Crest"]], resids[["sites Slope"]])
head(tdf1)
nrow(tdf1)
nrow(sites1)

head(resids[["tdf Slope"]])
ggplot(tdf1, aes(betamod, gam.5))+geom_point()+facet_wrap(~Zone)

ggplot(tdf1, aes(betamod, gam.5, col=Zone))+geom_point()+
theme_classic()+
labs(x="Residuals\n(beta regression)", y="Residuals\n(GAM)")

summary(glm(pbleach~max.dhw, family="quasibinomial", data=tdf1, weights=tdf1$Ncoral))

summary(betareg(pbleach ~ max.dhw, data=tdf1, link="logit")) # sigmoidal?

summary(gam(pbleach~s(max.dhw, k=5), data=tdf1))

summary(gam(BleachDead~s(DHWs, k=5), data=bl16a))

head(curves)
fit.long <- melt(curves, id.var=c("predictor", "data", "Zone"))

ggplot(fit.long, aes(x=predictor, y=value))+geom_line(aes(col=variable))+facet_grid(data~Zone)

ggplot(fit.long[fit.long$data=="tdf" & fit.long$variable %in% c("lm1fit", "expfit","gam1.5", "logfit", "betafit", "logisfit", "bifit"),], aes(x=predictor, y=value*100))+
geom_line(aes(col=Zone))+
facet_wrap(~variable, scales="free", ncol=1)+
theme_classic()+theme(strip.background=element_blank(), axis.text=element_blank())

 # are the years statistically different?
 
bimodX <- glm(pbleach~max.dhw, family="quasibinomial", data=sites1[sites1$Zone=="Crest",], weights=sites1[sites1$Zone=="Crest","Ncoral"]) 
fit.datX <- data.frame(max.dhw = seq(min(sites1$max.dhw), max(sites1$max.dhw), 0.1))
fit.datX$bifit <- predict(bimodX,fit.datX, type="response", se=T)$fit
fit.datX$bise <- predict(bimodX,fit.datX, type="response", se=T)$se.fit


curvesCrest <- curves[curves$data=="sites" & curves$Zone=="Crest",]


# --------------------------------- # recreate Hughes 2021

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

ggplot()+
geom_line(data=fit.all[!fit.all$year %in% c(1998, 2002),], aes(x=DHW, y=pred, linetype=type, col=as.factor(year)))+
geom_ribbon(data=fit.all[!fit.all$year %in% c(1998, 2002),], aes(x=DHW, ymin=pred+se*1.96, ymax=pred-se*1.96,  fill=as.factor(year)),  alpha=0.35, col="black", size=0.1)+
scale_linetype_manual(values=c("solid","dashed"))+
#scale_fill_viridis(discrete=T)+scale_colour_viridis(discrete=T, option="E")+
#geom_line(data=reeffit, aes(max.dhw, pred), col="black", size=1)+
#geom_ribbon(data=reeffit, aes(x=max.dhw, ymin=pred+se, ymax=pred-se), col="black", alpha=0.5)+
labs(x="Degree Heating Weeks", y="Probablity of severe\nreef bleaching")+
theme_classic()+theme(legend.title=element_blank())


# --------------------------------- residual plots

betaX <- betareg(pbleach~max.dhw, data=tdf1)
tdf1$fit <- predict(betaX, tdf1)

# ggplot(tdf1, aes(pbleach, betamod, col=Zone))+geom_point() # 

ggplot(tdf1, aes(fit, betamod, col=Zone))+geom_point()+geom_smooth() # 
summary(lm(betamod~fit, tdf1))

ggplot(tdf1, aes(max.dhw, betamod, col=Zone))+geom_point()+geom_smooth()
summary(lm(betamod~max.dhw, tdf1))

ggplot(sites1[sites1$Zone=="Crest",], aes(betamod, reorder(Reef, -GPS.S)))+geom_vline(xintercept=0)+
geom_boxplot(aes(fill=Region), size=0.1, outlier.size=0.1)


# --------------------------------- above curve?

crestdat <- sites1[sites1$Zone=="Crest",]
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

# --------------------------------- crest/slope

zones1 <- ggplot(sites1, aes(x=max.dhw, y=pbleach*100, col=Zone))+
geom_point()+
geom_line(data=curves[curves$data=="sites",], aes(x=predictor, y=betafit*100))+
scale_colour_manual(values=c("turquoise", "navyblue"))+
geom_hline(yintercept=30, linetype="dotted")+
labs(x="Degree Heating Weeks", y="% bleaching")+
theme_classic()+theme(legend.title=element_blank())

head(sites1)

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
scale_colour_manual(values=c("turquoise", "navyblue"))+scale_fill_manual(values=c("turquoise", "navyblue"))+
labs(x="Degree Heating Weeks", y="Probability of severe\nreef bleaching")+
theme_classic()+theme(legend.title=element_blank())

plot_grid(zones1+guides(col="none"), zones2, rel_widths=c(1,1.4))

head(crestfit)

# --------------------------------- recent mortality

# prop dead should also be beta regression?


tdf1[is.na(tdf1$pbleach),]
max(tdf1$pdead)

betamod3 = betareg(pdead ~ max.dhw, data=crestdat)
fit.dat2 <- data.frame(max.dhw = seq(min(crestdat$max.dhw), max(crestdat$max.dhw), 0.1))
fit.dat2$fit <- predict(betamod3, fit.dat2)
summary(betamod3)

j.av2$Mort <- j.av2$Mortality/100
j.av2$N <- aggregate(Total~Reef, james, max)$Total
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
geom_point(data=sites1[sites1$Zone=="Crest",], aes(max.dhw, pdead*100), shape=21, fill="black", size=0.5)+
geom_line(data=fit.dat2, aes(x=max.dhw, y=fit*100))


betamod4 = betareg(pdead ~ max.dhw, data=slopedat)
fit.dat4 <- data.frame(max.dhw = seq(min(slopedat$max.dhw), max(slopedat$max.dhw), 0.1))
fit.dat4$fit <- predict(betamod4, fit.dat4)
summary(betamod4)

zonesbeta <- rbind(cbind(fit.dat4, Zone="Slope"),cbind(fit.dat2, Zone="Crest"))
zones4 <- ggplot()+
geom_point(data=sites1, aes(x=max.dhw, y=pdead, col=Zone))+
geom_line(data=zonesbeta, aes(x=max.dhw, y=fit, col=Zone))+
scale_colour_manual(values=c("turquoise", "navyblue"))+
labs(x="Degree Heating Weeks", y="% recent mortality")+
theme_classic()+theme(legend.title=element_blank())
zones4

zoneplot <- plot_grid(zones1+guides(col="none"), zones2+theme(legend.position=c(0.85, 0.2)), zones4+guides(col="none"),nrow=1,  labels=c("A", "B", "C", "D"), align="hv")
zoneplot

# --------------------------------- composition vs bleaching

mod.dat2 <- tdf1#[tdf1$Zone=="Crest",]
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

idvars <- c("date", "observer" ,"reef_name","ReefNo","site", "depth", "transect_no","taxa")

apr16 <- read.csv("data/original/composition2016Apr.csv")
apr <- melt(apr16[,c(idvars, "TOTAL")], id.var=c(idvars))
apr <- apr[!apr$taxa %in% c("soft",  "other_sessile"),]
apr$transID <- paste(apr$site, apr$transect_no)
#apr$reef <- rnames$use[match(apr$reef_name, rnames$april)]
oct16 <- read.csv("data/original/composition2016Oct.csv")
oct <- melt(oct16[,c(idvars, "TOTAL")], id.var=c(idvars))
oct <- oct[!oct$taxa %in% c("soft",  "other_sessile"),]
oct$transID <- paste(oct$site, oct$transect_no)
#oct$reef <- rnames$use[match(oct$reef_name, rnames$oct)]
head(oct)
nrow(oct)


df16 <- read.csv("data/info/coordmatch16.csv")
df16$lat <- abs(df16$revised_lat)

df16$Region <- ifelse(
df16$lat <= 12.76938, "Cape Grenville", ifelse(
df16$lat > 12.76938 & df16$lat <= 13.96097, "Princess Charlotte Bay", ifelse(
df16$lat > 13.96097 & df16$lat< 16.6, "Lizard", ifelse(
df16$lat >= 16.6 & df16$lat< 18,"Cairns", ifelse(
df16$lat >= 18  & df16$lat<= 18.74226,"Cape Bowling Green", ifelse(
df16$lat < 25  & df16$lat > 18.74226, "Capricorn Bunkers", NA))))))
head(df16)

#ggplot()+geom_point(data=df16, aes(x=revised_long, y=revised_lat, fill=Region), shape=21)+geom_text(data=df16, aes(x=revised_long+2, y=revised_lat, col=Region, label=ReefName), size=2)

oct$Region <- df16$Region[match(oct$ReefNo, df16$ReefNo)]
oct$Region[oct$reef_name %in% c("Kelso", "Knife", "Fork", "Davies", "Dip")] <- "Cape Bowling Green"
oct$Region[oct$reef_name %in% c("12-059")] <- "Cape Grenville"
oct$Region[oct$reef_name %in% c("16_014")] <- "Lizard"
unique(oct[is.na(oct$Region),"reef_name"]) # ones without a region

apr$Region <- df16$Region[match(apr$ReefNo, df16$ReefNo)]
apr$Region[apr$reef_name %in% c("Magnetic Island", "Kelso Reef", "Knife Reef", "Fork Reef", "Davies Reef", "Dip Reef","Orpheus Island")] <- "Cape Bowling Green"
apr$Region[apr$reef_name %in% c("Ribbon 10", "Maxwell Reef", "Switzer", "16-014a")] <- "Lizard"
apr$Region[apr$reef_name %in% c("12-059", "Martha", "Combe", "11-203")] <- "Cape Grenville"
apr$Region[apr$reef_name %in% c("16_014", "Batt", "Low isles")] <- "Cairns"
apr$Region[apr$reef_name %in% c("Corbett", "Noddy", "Blanchard","13-077","13-122","13-093b")] <- "Princess Charlotte Bay"
apr$Region[apr$reef_name %in% c("Osprey")] <- "Coral Sea"
unique(apr[is.na(apr$Region),"reef_name"]) # ones without a region

# 2) coral cov 2016

cov <- aggregate(value~reef_name+site+transID+Region, oct, sum)
cov$cov <- (cov$value /1000) * 100
covreef <-  aggregate(cov~reef_name+Region, cov, mean)
covreef$cov.se <-  aggregate(cov~reef_name+Region, cov, se)$cov
head(covreef)

cov2 <- aggregate(value~reef_name+site+transID+Region, apr, sum)
cov2$cov <- (cov2$value /1000) * 100
covreef2 <-  aggregate(cov~reef_name+Region, cov2, mean)
covreef2$cov.se <-  aggregate(cov~reef_name+Region, cov2, se)$cov
head(covreef2)


# 3) Acro cov 2016

acro <- aggregate(value~reef_name+site+transID+Region, oct[oct$taxa %in% c("tabular_Acropora", "staghorn_Acropora", "other_Acropora"),], sum)
acro$cov <- (acro$value /1000) * 100
acroreef <-  aggregate(cov~reef_name+Region, acro, mean)
acroreef$acro.se <-  aggregate(cov~reef_name+Region, acro, se)$cov
covreef$acro <- acroreef$cov[match(covreef$reef_name, acroreef$reef_name)]
covreef$acro.se <- acroreef$acro.se[match(covreef$reef_name, acroreef$reef_name)]
head(covreef)

acro2 <- aggregate(value~reef_name+site+transID+Region, apr[apr$taxa %in% c("tabular_Acropora", "staghorn_Acropora", "other_Acropora"),], sum)
acro2$cov <- (acro2$value /1000) * 100
acroreef2 <-  aggregate(cov~reef_name+Region, acro2, mean)
acroreef2$acro.se <-  aggregate(cov~reef_name+Region, acro2, se)$cov
covreef$acroApr <- acroreef2$cov[match(covreef$reef_name, acroreef2$reef_name)]
covreef$acroApr.se <- acroreef2$acro.se[match(covreef$reef_name, acroreef2$reef_name)]
head(covreef)


# 4) tabular cov 2016

tab <- aggregate(value~reef_name+site+transID+Region, oct[oct$taxa %in% c("tabular_Acropora"),], sum)
tab$cov <- (tab$value /1000) * 100
tabreef <-  aggregate(cov~reef_name+Region, tab, mean)
tabreef$tab.se <-  aggregate(cov~reef_name+Region, tab, se)$cov
covreef$tab <- tabreef$cov[match(covreef$reef_name, tabreef$reef_name)]
covreef$tab.se <- tabreef$tab.se[match(covreef$reef_name, tabreef$reef_name)]
head(covreef)

tab2 <- aggregate(value~reef_name+site+transID+Region, apr[apr$taxa %in% c("tabular_Acropora"),], sum)
tab2$cov <- (tab2$value /1000) * 100
tabreef2 <-  aggregate(cov~reef_name+Region, tab2, mean)
tabreef2$tab.se <-  aggregate(cov~reef_name+Region, tab2, se)$cov
covreef$tabApr <- tabreef2$cov[match(covreef$reef_name, tabreef2$reef_name)]
covreef$tabApr.se <- tabreef2$tab.se[match(covreef$reef_name, tabreef2$reef_name)]
head(covreef)

df.use <- tdf1[tdf1$Zone%in% c("Crest"),]
reefs <- aggregate(coral_cov~Reef, df.use[df.use$Zone%in% c("Crest", "Slope"),], mean)
reefs$cov <- reefs$coral_cov
reefs$cov.se <- aggregate(coral_cov~Reef, df.use, se)$coral_cov
reefs$acro <- aggregate(acro~Reef, df.use, mean)$acro
reefs$acro.se <- aggregate(acro~Reef, df.use, se)$acro
reefs$tab <- aggregate(tab~Reef, df.use, mean)$tab
reefs$tab.se <- aggregate(tab~Reef, df.use, se)$tab
reefs$pbleach <- aggregate(pbleach~Reef, df.use, mean)$pbleach
reefs$pbleach.se <- aggregate(pbleach~Reef, df.use, se)$pbleach
reefs$resid <- aggregate(betamod~Reef, df.use, mean)$betamod
reefs$resid.se <- aggregate(betamod~Reef, df.use, se)$betamod

reefs$Reef2 <- reefs$Reef
reefs$Reef2[reefs$Reef2=="Ribbon 8 "] <- "Ribbon8"
reefs$Reef2[reefs$Reef2=="North Direction"] <- "NthDirection"

colss <- c("cov"  ,   "cov.se"  ,   "acro" ,   "acro.se"   ,     "tab"    , "tab.se")

reefs[,paste(colss, ".16", sep="")] <- covreef[match(reefs$Reef2, covreef$reef_name), colss]

reefs2 <- na.omit(reefs)

reefs2$acrochange <- reefs2$acro - reefs2$acro.16
# for SE... minus upper and lower... 
#reefs2$acro.upp <- (reefs2$acro + reefs2$acro.se) - (reefs2$acro.16+reefs2$acro.se.16) 
#reefs2$acro.low <- (reefs2$acro-reefs2$acro.se) - (reefs2$acro.16-reefs2$acro.se.16) 

recplot <- ggplot()+
geom_hline(yintercept=0, col="grey")+geom_vline(xintercept=0, col="grey")+
geom_point(data=reefs2, aes(x=acrochange, y=resid), size=1)+
#geom_line(data=reefs2, aes(x=acrochange, y=resid), size=1)+
geom_smooth(data=reefs2, aes(x=acrochange, y=resid), method="lm", se=F, col="red", size=0.35, formula=y~poly(x,1))+
#geom_text(data=reefs2, aes(x=acro-acro.16, y=resid, label=Reef))+
geom_text(data=NULL, aes(-26, 0.4, label="Heron Is."), size=3)+
geom_text(data=NULL, aes(38, 1.3, label="Lizard Is."), size=3)+
geom_segment(data=NULL, aes(x=34,xend=38, y=1.25,yend=1.05), size=0.24, col="grey")+
geom_segment(data=NULL, aes(x=-26,xend=-30, y=0.1,yend=0.3), size=0.24, col="grey")+
#geom_segment(data=reefs2, aes(y=resid, yend=resid, x=acro.low, xend=acro.upp))+
geom_segment(data=reefs2, aes(x=acrochange, xend=acrochange, y=resid-resid.se, yend=resid+resid.se))+
#geom_point(data=reefs, aes(x=coral_cov_pit-cov16, y=resid))+
#geom_text(data=reefs, aes(x=coral_cov_pit-cov16, y=resid, label=Reef))+
theme_classic()+theme(axis.text=element_text(size=8), axis.title=element_text(size=8))+
labs(x="Change in % Acropora\n(2016-2024)", y="deviation from expected\nbleaching (2024)")
recplot 

# --------------------------------- # heatwave history (maps)

freqs2 <- NULL
times2 <- NULL

head(dhw)

N <- 6
dhw$aboveN <- ifelse(dhw$dhw> N, 1, 0)
df14 <- dhw[dhw$year==2016,] # add in 2014 for 10+ years since bleaching
df14$year <- 2015
df14$aboveN <- 1
df.use <- dhw# rbind(dfr, df14)
t.use <- df.use[df.use$aboveN==1,]
t.use <- t.use[!t.use$year %in% c(1998,2002),]
t.use <- t.use[!t.use$year==2024,]
time <- aggregate(year~ X + Y, t.use, max) # max year in the dataset. 
time$last <- 2024 - time$year
freq <- aggregate(aboveN ~ X + Y, dhw[!dhw$year %in% c(1998,2002, 2024),], sum)
freqs2 <- rbind(freqs2, cbind(freq, N=N))
times2 <- rbind(times2, cbind(time, N=N))


head(freqs2)

plot_grid(
ggplot(freqs2[freqs2$N==6,], aes(X, Y, col=aboveN))+geom_point(size=0.5)+scale_colour_viridis()
,
ggplot(times2[times2$N==6,], aes(X, Y, col=last))+geom_point(size=0.5)+scale_colour_viridis(option="B", direction=-1)
)

sitepoc <- aggregate(poc~siteID, tdf1, mean)
sitepoc$pocR <- aggregate(pocR~siteID, tdf1, mean)$pocR
sites1[,c("poc", "pocR")] <- sitepoc[match(sites1$siteID, sitepoc$siteID),c("poc", "pocR") ]

# --------------------------------- # heatwave history (comps)


sitepoc <- aggregate(poc~siteID, tdf1, mean)
sitepoc$pocR <- aggregate(pocR~siteID, tdf1, mean)$pocR
sites1[,c("poc", "pocR")] <- sitepoc[match(sites1$siteID, sitepoc$siteID),c("poc", "pocR") ]

tdf1$pocR<- tdf1$poc/tdf1$coral_cov
tdf1$acroR <- tdf1$acro/tdf1$coral_cov
siteac <- aggregate(acro~siteID, tdf1, mean)
siteac$acroR <- aggregate(acroR~siteID, tdf1, mean)$acroR
sites1[,c("acro", "acroR")] <- siteac[match(sites1$siteID, siteac$siteID),c("acro", "acroR") ]

tdf1$porR <- tdf1$por/tdf1$coral_cov
sitepor <- aggregate(por~siteID, tdf1, mean)
sitepor$porR <- aggregate(porR~siteID, tdf1, mean)$porR
sites1[,c("por", "porR")] <- sitepor[match(sites1$siteID, sitepor$siteID),c("por", "porR") ]

tdf1$tabR <- tdf1$tab/tdf1$coral_cov
sitetab <- aggregate(tab~siteID, tdf1, mean)
sitetab$tabR <- aggregate(tabR~siteID, tdf1, mean)$tabR
sites1[,c("tab", "tabR")] <- sitetab[match(sites1$siteID, sitetab$siteID),c("tab", "tabR") ]


# TIME BETWEEN HEATWAVES>>> NUMBER OF HEATWAVES... accumulated stress?
# Cover return.(if 2016)./Acropora abuncance/ bleaching
# If 2016 --- cover return vs acropora vs bleaching... 

# -------- NUMBER OF EVENTS! 
head(tyears)

maxdhw <- aggregate(dhw~year+gridID, tyears, max) #[!tyears$year==2024,]
maxdhw 

tsums <- NULL
ssums <- NULL
for(n in c(6)){	
#n <- 2
maxdhw$aboveN <- ifelse(maxdhw$dhw > n,1, 0)
duse <- maxdhw[maxdhw$year %in% c(2016, 2017, 2020, 2022),]
duse <-  #[!maxdhw$year==2024,]
sumN <- aggregate(aboveN~gridID, duse, sum)

tdf1$sumN <-sumN$aboveN[match(tdf1$gridID, sumN$gridID)]
sites1$sumN <-sumN$aboveN[match(sites1$gridID, sumN$gridID)]
tsums <- rbind(tsums, cbind(tdf1, n))
ssums <- rbind(ssums, cbind(sites1, n))
}


### time since last bleaching

lastdat <- NULL
slastdat <- NULL
for(n2 in c(6)){
	#n2 <- 4
maxdhw$aboveN2 <- ifelse(maxdhw$dhw > n2, 1, 0)
tuse <- maxdhw[maxdhw$aboveN2==1,]
#tuse <- tuse[!tuse$year ==2024,]
tuse <- tuse[tuse$year %in% c(2016, 2017, 2020, 2022),]
tlast <- aggregate(year~gridID, tuse, max)
tlast$tlast <- 2024 - tlast$year
tlast$year

tdf1$tlast <- tlast$tlast[match(tdf1$gridID, tlast$gridID)]
tdf1$tlast
tdf1$tlast[is.na(tdf1$tlast)]<- "8"

sites1$tlast <- tlast$tlast[match(sites1$gridID, tlast$gridID)]
sites1$tlast[is.na(sites1$tlast)]<- 2024-2016
#sites1$lyear <- tlast$year[match(sites1$gridID, tlast$gridID)]
#sites1$lyear[is.na(sites1$lyear)]<- 2016
sites1$tlast[is.na(sites1$tlast)]<- "8"
lastdat <- rbind(lastdat, cbind(tdf1, n2)) 
slastdat <- rbind(slastdat, cbind(sites1, n2)) }



# aggregate(tab~variable, slong, mean)
tabmeans <- aggregate(tab~n+Zone, tsums, mean)
acmeans <- aggregate(acro~n+Zone, tsums, mean)

andat <- NULL
for(i in c(6)) {
	for(z in c("Crest", "Slope")){
		for(y in c("betamod", "gam.5", "acro","tab")){
tsums$y <- tsums[,y]			
an1 <- aov(y~sumN, tsums[tsums$Zone==z & tsums$n==i,])
summary(an1)
y4 <- mean(tsums$y[tsums$sumN==4 & tsums$Zone==z & tsums$n==i], na.rm=T)
pval <- summary(an1)[[1]][["Pr(>F)"]][1]
sig <- ifelse(pval>0.05, "NS", ifelse(pval<=0.05 & pval>0.01, "*", ifelse(pval<=0.01 & pval>0.001, "**", ifelse(pval<=0.001, "***", NA))))
sz <- ifelse(sig=="NS", "ns", "s")
andat <- rbind(andat, data.frame(pval, sig, n=i, Zone=z, y, y4, sz))
}}}
andat


andat1b <- NULL
for(i in c(6)) {
		for(y in c("betamod", "gam.5", "acro","tab")){
tsums$y <- tsums[,y]			
an1 <- aov(y~sumN, tsums[tsums$n==i,])
summary(an1)
y4 <- mean(tsums$y[tsums$sumN==4 & tsums$n==i], na.rm=T)
pval <- summary(an1)[[1]][["Pr(>F)"]][1]
sig <- ifelse(pval>0.05, "NS", ifelse(pval<=0.05 & pval>0.01, "*", ifelse(pval<=0.01 & pval>0.001, "**", ifelse(pval<=0.001, "***", NA))))
sz <- ifelse(sig=="NS", "ns", "s")
andat1b <- rbind(andat1b, data.frame(pval, sig, n=i,  y, y4, sz))
}}
andat1b


ggplot()+
geom_segment(data=tabmeans, aes(x=-Inf, xend=5.3, y=0, yend=0), col="grey")+
geom_boxplot(data=ssums, aes(as.factor(sumN), betamod), outlier.size=0.05, size=0.1,position = position_dodge2(preserve = "single"),fill="grey95")



head(lastdat)
#lastdat[lastdat$n2==6,c("acro", "Reef","Transect_code", "tlast")]
#length(lastdat[lastdat$n2==6,c("Transect_code")])
#length(lastdat[lastdat$n2==2,c("Transect_code")])

tabmeans2 <- aggregate(tab~n2+Zone, lastdat, mean)
acmeans2 <- aggregate(acro~n2+Zone, lastdat, mean)


andat2 <- NULL
for(i in c(6)) {
	for(z in c("Crest", "Slope")){
		for(y in c("betamod", "gam.5", "acro","tab")){
lastdat$y <- lastdat[,y]			
an1 <- aov(y~tlast, lastdat[lastdat$Zone==z & lastdat$n2==i,])
summary(an1)
y4 <- mean(lastdat$y[lastdat$tlast==8 & lastdat$Zone==z & lastdat$n2==i], na.rm=T)
pval <- summary(an1)[[1]][["Pr(>F)"]][1]
sig <- ifelse(pval>0.05, "NS", ifelse(pval<=0.05 & pval>0.01, "*", ifelse(pval<=0.01 & pval>0.001, "**", ifelse(pval<=0.001, "***", NA))))
sz <- ifelse(sig=="NS", "ns", "s")
andat2 <- rbind(andat2, data.frame(pval, sig, n2=i, Zone=z, y, y4, sz))
}}}
andat2

andat2b <- NULL
for(i in c(6)) {
		for(y in c("betamod", "gam.5", "acro","tab")){
lastdat$y <- lastdat[,y]			
an1 <- aov(y~tlast, lastdat[lastdat$n2==i,])
summary(an1)
y4 <- mean(lastdat$y[lastdat$tlast==8  & lastdat$n2==i], na.rm=T)
pval <- summary(an1)[[1]][["Pr(>F)"]][1]
sig <- ifelse(pval>0.05, "NS", ifelse(pval<=0.05 & pval>0.01, "*", ifelse(pval<=0.01 & pval>0.001, "**", ifelse(pval<=0.001, "***", NA))))
sz <- ifelse(sig=="NS", "ns", "s")
andat2b <- rbind(andat2b, data.frame(pval, sig, n2=i,  y, y4, sz))
}}
andat2b


#####################
#####################
##################### HEAT PLOTS!

# sensitivity at different disturbance combinations...

head(tdf1)

# model av intensity and frequency... 

head(maxdhw)

avsev <- aggregate(dhw~gridID, maxdhw, max)
tdf1$avsev <- avsev$dhw[match(tdf1$gridID, avsev$gridID)]
sites1$avsev <- avsev$dhw[match(sites1$gridID, avsev$gridID)]


out <- NULL
ys <- c("tab", "acro", "betamod", "gam.5")
	for(i in ys){
	#	i <- "tab"
mod.df <- sites1[sites1$Zone=="Crest",]# tdf1[tdf1$Zone=="Crest",] #sites1[sites1$Zone=="Crest",]
mod.df$response <- mod.df[,i] #mod.df$betamod #mod.df$acroR # #mod.df$betamod #$acroR #mod.df$betamod
mod.df$tlast <-mod.df$avsev #as.numeric(mod.df$tlast)
mod.df$sumN2 <- mod.df$sumN^2
mod.df$tlast2 <- as.numeric(mod.df$tlast)^2

# problem. Number and Severity not independent... 
#plot_grid(ggplot(mod.df, aes(freq, response))+geom_point()+geom_smooth()+geom_smooth(method="lm"),ggplot(mod.df, aes(avsev, response))+geom_point()+geom_smooth()+geom_smooth(method="lm"),ggplot(mod.df, aes(avsev, freq))+geom_point()+geom_smooth()+geom_smooth(method="lm"))

#if(i %in% c("betamod", "gam.5")){
dismod <- lm(response~sumN+tlast+sumN2, mod.df)
#summary(dismod) }
#else {
#mod.df$response2 <- mod.df$response/100
#dismod <- betareg(response2~freq+avsev+freq2, mod.df)
#summary(dismod)}
new <- expand.grid(sumN=seq(min(mod.df$sumN), max(mod.df$sumN), 0.1), tlast=seq(min(mod.df$tlast), max(mod.df$tlast), 0.1)) 
new$sumN2 <- new$sumN^2
new$tlast2 <- new$tlast^2
new$response <- predict(dismod, new)
new$response2 <- (new$response-min(new$response))/(max(new$response)-min(new$response))
new$i <- i 
out <- rbind(out, new)}


out$label <- ifelse(out$i=="tab", "% Tabular (2024)", ifelse(out$i=="acro", "% Acropora cover (2024)", ifelse(out$i=="gam.5", "2024 bleaching\nsusceptibility (gam)",ifelse(out$i=="betamod", "2024 bleaching\nsusceptibility (beta)", NA))))

heatplot <- ggplot()+
geom_raster(data=out, aes(x=sumN, y=tlast, fill=response2))+
geom_contour(data=out[out$i=="betamod",], aes(x=sumN, y=tlast, z=response), breaks=c(0),col="black", linewidth=0.1,linetype="dashed")+
geom_contour(data=out[out$i=="gam.5",], aes(x=sumN, y=tlast, z=response), breaks=c(0),col="black", linewidth=0.1, linetype="dashed")+
#geom_contour(data=out[out$i=="acro",], aes(x=freq, y=avsev, z=response), breaks=c(30, 25, 35),col="black", linewidth=0.1)+
#geom_contour(data=out[out$i=="tab",], aes(x=freq, y=avsev, z=response), breaks=c(10, 15, 5),col="black", linewidth=0.1)+
scale_radius()+
facet_wrap(~label)+
#labs(x="Heatwave frequencies\n(N per decade 2015-24)", y="Heatwave severities\n(max DHW 2015-24)")+
scale_fill_distiller(palette="Spectral")+
scale_y_continuous(expand=c(0,0))+scale_x_continuous(expand=c(0,0))+
theme_bw()+theme(strip.background=element_blank(), strip.text=element_text(size=8, face="bold"), legend.title=element_blank(), legend.text=element_blank())
heatplot 


heatplot2 <- ggplot()+
geom_raster(data=out[out$i=="betamod",], aes(x=sumN, y=tlast, fill=response))+
#geom_contour(data=out[out$i=="betamod",], aes(x=sumN, y=tlast, z=response), breaks=c(-1,0,1),col="white", linewidth=0.2, linetype="dashed")+
#geom_contour(data=out[out$i=="tab",], aes(x=sumN, y=tlast, z=response), breaks=c(0),col="black", linewidth=0.1, linetype="dashed")+
#geom_contour(data=out[out$i=="acro",], aes(x=freq, y=avsev, z=response), breaks=c(30, 25, 35),col="black", linewidth=0.1)+
#geom_contour(data=out[out$i=="tab",], aes(x=freq, y=avsev, z=response), breaks=c(10, 15, 5),col="black", linewidth=0.1)+
scale_radius()+
facet_wrap(~label)+
ggtitle("")+
#ggtitle("Heatwave history\nvs. susceptibility")+
coord_cartesian(xlim=c(1,4))+
labs(x="N events\n(2016-2023)", y="Max. severity (DHW)", fill="Bleaching\nresiduals\n(2024)")+
scale_fill_distiller(palette="Spectral")+
scale_y_continuous(expand=c(0,0))+scale_x_continuous(expand=c(0,0))+
theme_bw()+theme(strip.background=element_blank(), strip.text=element_blank(), legend.title=element_text(size=7), legend.text=element_text(size=8), legend.key.width=unit(2, "mm"),legend.key.height=unit(3, "mm"), legend.position="right", legend.margin=margin(1,1,1,-5), plot.title=element_text(size=8, face="bold", hjust=0.5))
heatplot2

# --------------------------------- # acro change

removeR <- c("Coral Sea", "Hydrographers Passage")


acro <- aggregate(value~reef_name+site+transID+Region, oct[oct$taxa %in% c("tabular_Acropora", "staghorn_Acropora", "other_Acropora"),], sum)
acro$cov <- (acro$value /1000) * 100
acroreef <-  aggregate(cov~reef_name+Region, acro, mean)
acroreef$acro.se <-  aggregate(cov~reef_name+Region, acro, se)$cov
covreef$acro <- acroreef$cov[match(covreef$reef_name, acroreef$reef_name)]
covreef$acro.se <- acroreef$acro.se[match(covreef$reef_name, acroreef$reef_name)]
head(covreef)

acro2 <- aggregate(value~reef_name+site+transID+Region, apr[apr$taxa %in% c("tabular_Acropora", "staghorn_Acropora", "other_Acropora"),], sum)
acro2$cov <- (acro2$value /1000) * 100
acroreef2 <-  aggregate(cov~reef_name+Region, acro2, mean)
acroreef2$acro.se <-  aggregate(cov~reef_name+Region, acro2, se)$cov
covreef$acroApr <- acroreef2$cov[match(covreef$reef_name, acroreef2$reef_name)]
covreef$acroApr.se <- acroreef2$acro.se[match(covreef$reef_name, acroreef2$reef_name)]
head(covreef)

tacro <- rbind(data.frame(cov=acroreef2$cov, Region=acroreef2$Region, t="1. 2016 (pre-bleaching)", t2="2016a"), data.frame(cov=acroreef$cov, Region=acroreef$Region, t="2. 2016 (post-bleaching)", t2="2016b"), data.frame(cov=sites1$acro[sites1$Zone=="Crest"], Region=sites1$Region[sites1$Zone=="Crest"], t="3. 2024 (pre-bleaching)", t2="2024"))
head(tacro)

removeR <- c("Coral Sea", "Hydrographers Passage")

#tacro$Region2 <- factor(tacro$Region, levels=c(rev(regions), "Coral Sea"))

avacroR <- aggregate(cov~t+Region, tacro[!tacro$Region %in% removeR,], mean)


tcols <- c("#66c2a5","#fc8d62","#8da0cb")
acplotR <- ggplot()+
geom_density(data=tacro[!tacro$Region %in% removeR,], aes(x=cov+1, col=t, fill=t), alpha=0.2)+
geom_segment(data=avacroR, aes(x=cov+1, xend=cov+1, yend=4, y=Inf, col=t), arrow=arrow(length=unit(1, "mm")))+
scale_x_log10()+
#scale_x_sqrt()+
facet_wrap(~Region, ncol=1)+
labs(x="% Acropora", y="N")+
scale_y_sqrt()+
#scale_y_continuous(expand=c(0,0))+
scale_fill_manual(values=tcols)+scale_colour_manual(values=tcols)+
theme_classic()+theme(strip.background=element_blank(), axis.text.y=element_blank(), axis.line.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank())
acplotR


avacro <- aggregate(cov~t, tacro[!tacro$Region %in% removeR,], mean)

tacro$logcov <- tacro$cov #log10(tacro$cov+1)
avacro$logmean <- aggregate(logcov~t, tacro[!tacro$Region %in% removeR,], mean)$logcov
avacro$se <- aggregate(logcov~t, tacro[!tacro$Region %in% removeR,], se)$logcov
avacro$n <- aggregate(logcov~t, tacro[!tacro$Region %in% removeR,], length)$logcov
avacro$low95 <- avacro$logmean - (qt(0.975, avacro$n- 1) * avacro$se)
avacro$upp95 <- avacro$logmean + (qt(0.975, avacro$n- 1) * avacro$se)
avacro$low50 <- avacro$logmean - (qt(0.85, avacro$n- 1) * avacro$se)
avacro$upp50 <- avacro$logmean + (qt(0.85, avacro$n- 1) * avacro$se)
avacro


#tcols <- c("#66c2a5","#fc8d62","#8da0cb")
tcols <- c("#01665e", "#a50026","#5e4fa2" )

acplot5 <- ggplot()+
geom_density(data=tacro[!tacro$Region %in% removeR,], aes(x=cov+1, col=t, fill=t), alpha=0.2)+
geom_segment(data=avacro, aes(x=low50, xend=upp50, yend=0.4, y=0.4, col=t), size=1, show_guide=F)+
geom_segment(data=avacro, aes(x=low95, xend=upp95, yend=0.4, y=0.4, col=t), size=0.5, show_guide=F)+
geom_point(data=avacro, aes(x=logmean, y=0.4, col=t), size=3, shape=21, fill="white", show_guide=F)+
geom_text(data=avacro, aes(x=logmean, y=0.4, col=t, label=c("1", "2", "3")), size=2, fontface="bold", show_guide=F)+
scale_x_log10()+
#scale_y_sqrt()+
facet_wrap(~t, ncol=1)+
labs(x="% Acropora", y="N")+
#scale_y_continuous(expand=c(0,0))+
scale_fill_manual(values=tcols)+scale_colour_manual(values=tcols)+
theme_classic()+
theme(axis.line.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(), strip.background=element_blank(), strip.text=element_blank(),panel.spacing = unit(-1, "lines"),plot.background=element_blank(), panel.background=element_blank(), axis.ticks.y=element_blank(), legend.title=element_blank(), legend.key.size=unit(2, "mm"))
acplot5

tacro[is.na(tacro$t2),]

# --------------------------------- # tab change


tab <- aggregate(value~reef_name+site+transID+Region, oct[oct$taxa %in% c("tabular_Acropora"),], sum)
tab$cov <- (tab$value /1000) * 100
tabreef <-  aggregate(cov~reef_name+Region, tab, mean)
tabreef$tab.se <-  aggregate(cov~reef_name+Region, tab, se)$cov
covreef$tab <- tabreef$cov[match(covreef$reef_name, tabreef$reef_name)]
covreef$tab.se <- tabreef$tab.se[match(covreef$reef_name, tabreef$reef_name)]
head(covreef)

tab2 <- aggregate(value~reef_name+site+transID+Region, apr[apr$taxa %in% c("tabular_Acropora"),], sum)
tab2$cov <- (tab2$value /1000) * 100
tabreef2 <-  aggregate(cov~reef_name+Region, tab2, mean)
tabreef2$tab.se <-  aggregate(cov~reef_name+Region, tab2, se)$cov
covreef$tabApr <- tabreef2$cov[match(covreef$reef_name, tabreef2$reef_name)]
covreef$tabApr.se <- tabreef2$tab.se[match(covreef$reef_name, tabreef2$reef_name)]
head(covreef)

ttab <- rbind(data.frame(cov=tabreef2$cov, Region=tabreef2$Region, t="1. 2016 (pre-bleaching)", t2="2016a"), data.frame(cov=tabreef$cov, Region=tabreef$Region, t="2. 2016 (post-bleaching)", t2="2016b"), data.frame(cov=sites1$tab[sites1$Zone=="Crest"],Region=sites1$Region[sites1$Zone=="Crest"], t="3. 2024 (pre-bleaching)", t2="2024"))

ttab$rel <- ttab$cov / tcov$cov

ttab$Region2 <- factor(ttab$Region, levels=c(rev(regions), "Coral Sea"))
avtab <- aggregate(cov~t, ttab[!ttab$Region %in% removeR,], mean)
avtabR <- aggregate(cov~t+Region2, ttab[!ttab$Region %in% removeR,], mean)

ttab$logcov <- ttab$cov #log10(ttab$cov+1)
avtab$logmean <- aggregate(logcov~t, ttab[!ttab$Region %in% removeR,], mean)$logcov
avtab$se <- aggregate(logcov~t, ttab[!ttab$Region %in% removeR,], se)$logcov
avtab$n <- aggregate(logcov~t, ttab[!ttab$Region %in% removeR,], length)$logcov
avtab$low95 <- avtab$logmean - (qt(0.975, avtab$n- 1) * avtab$se)
avtab$upp95 <- avtab$logmean + (qt(0.975, avtab$n- 1) * avtab$se)
avtab$low50 <- avtab$logmean - (qt(0.85, avtab$n- 1) * avtab$se)
avtab$upp50 <- avtab$logmean + (qt(0.85, avtab$n- 1) * avtab$se)
avtab


#tcols <- c("#66c2a5","#fc8d62","#8da0cb")
tcols <- c("#01665e", "#a50026","#5e4fa2" )

tabplot5 <- ggplot()+
geom_density(data=ttab[!ttab$Region %in% removeR,], aes(x=cov+1, col=t, fill=t), alpha=0.3)+
#geom_segment(data=avtab, aes(x=cov+1, xend=cov+1, yend=0.01, y=0.3, col=t), arrow=arrow(length=unit(1, "mm")))+
geom_segment(data=avtab, aes(x=low50, xend=upp50, yend=0.3, y=0.3, col=t), size=1)+
geom_segment(data=avtab, aes(x=low95, xend=upp95, yend=0.3, y=0.3, col=t), size=0.5)+
geom_point(data=avtab, aes(x=logmean, y=0.3, col=t), size=3, shape=21, fill="white")+
geom_text(data=avtab, aes(x=logmean, y=0.3, col=t, label=c("1", "2", "3")), size=2, fontface="bold")+
scale_x_log10()+
#scale_x_sqrt()+
facet_wrap(~t, ncol=1)+
#coord_cartesian(ylim=c(0,2))+
labs(x="% tabular Acropora", y="N")+
scale_y_sqrt(expand=c(0,0))+
scale_fill_manual(values=tcols)+scale_colour_manual(values=tcols)+
#scale_fill_viridis(discrete=T)+scale_colour_viridis(discrete=T)+
theme_classic()+
theme(axis.line.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(), strip.background=element_blank(), strip.text=element_blank(),panel.spacing = unit(-1, "lines"),plot.background=element_blank(), panel.background=element_blank(), axis.ticks.y=element_blank())
tabplot5 

ttab$t2 <- factor(ttab$t2, levels=rev(c("2016a", "2016b", "2024")))


tabchange <- ggplot()+
geom_boxplot(data=ttab[!ttab$Region %in% removeR,], aes(x=cov, y=t2, fill=t2), outlier.size=0.1, size=0.2)+
scale_fill_manual(values=c("black", "red", "grey"))+
guides(fill="none")+
scale_x_sqrt()+
xlab("% tabular Acropora")+
theme_classic()+theme(axis.title.y=element_blank(), axis.line=element_line(size=0.2))

tacro[is.na(tacro$t2),]

tacro$t2 <- factor(tacro$t2, levels=rev(c("2016a", "2016b", "2024")))

acrochange <- ggplot()+
geom_boxplot(data=tacro[!tacro$Region %in% removeR,], aes(x=cov, y=t2, fill=t2), outlier.size=0.1, size=0.2)+
scale_fill_manual(values=c("black", "red", "grey"))+
guides(fill="none")+
scale_x_sqrt()+
xlab("% Acropora")+
theme_classic()+theme(axis.title.y=element_blank(), axis.line=element_line(size=0.2))

plot_grid(tabchange, acrochange)

# --------------------------------- # COMBINED MDS!!! 


rnames <- read.csv("data/info/reefnames.csv")
head(rnames)

head(apr)
unique(apr$taxa)


idvars <- c("date", "observer" ,"reef_name","ReefNo","site", "depth", "transect_no","taxa")

apr16 <- read.csv("data/original/composition2016Apr.csv")
apr <- melt(apr16[,c(idvars, "TOTAL")], id.var=c(idvars))
apr <- apr[!apr$taxa %in% c("soft",  "other_sessile"),]
apr$transID <- paste(apr$site, apr$transect_no)
#apr$reef <- rnames$use[match(apr$reef_name, rnames$april)]
oct16 <- read.csv("data/original/composition2016Oct.csv")
oct <- melt(oct16[,c(idvars, "TOTAL")], id.var=c(idvars))
oct <- oct[!oct$taxa %in% c("soft",  "other_sessile"),]
oct$transID <- paste(oct$site, oct$transect_no)
#oct$reef <- rnames$use[match(oct$reef_name, rnames$oct)]
head(oct)
nrow(oct)


df16 <- read.csv("data/info/coordmatch16.csv")
df16$lat <- abs(df16$revised_lat)

df16$Region <- ifelse(
df16$lat <= 12.76938, "Cape Grenville", ifelse(
df16$lat > 12.76938 & df16$lat <= 13.96097, "Princess Charlotte Bay", ifelse(
df16$lat > 13.96097 & df16$lat< 16.6, "Lizard", ifelse(
df16$lat >= 16.6 & df16$lat< 18,"Cairns", ifelse(
df16$lat >= 18  & df16$lat<= 18.74226,"Cape Bowling Green", ifelse(
df16$lat < 25  & df16$lat > 18.74226, "Capricorn Bunkers", NA))))))
head(df16)

#ggplot()+geom_point(data=df16, aes(x=revised_long, y=revised_lat, fill=Region), shape=21)+geom_text(data=df16, aes(x=revised_long+2, y=revised_lat, col=Region, label=ReefName), size=2)

oct$Region <- df16$Region[match(oct$ReefNo, df16$ReefNo)]
oct$Region[oct$reef_name %in% c("Kelso", "Knife", "Fork", "Davies", "Dip")] <- "Cape Bowling Green"
oct$Region[oct$reef_name %in% c("12-059")] <- "Cape Grenville"
oct$Region[oct$reef_name %in% c("16_014")] <- "Lizard"
unique(oct[is.na(oct$Region),"reef_name"]) # ones without a region

apr$Region <- df16$Region[match(apr$ReefNo, df16$ReefNo)]
apr$Region[apr$reef_name %in% c("Magnetic Island", "Kelso Reef", "Knife Reef", "Fork Reef", "Davies Reef", "Dip Reef","Orpheus Island")] <- "Cape Bowling Green"
apr$Region[apr$reef_name %in% c("Ribbon 10", "Maxwell Reef", "Switzer", "16-014a")] <- "Lizard"
apr$Region[apr$reef_name %in% c("12-059", "Martha", "Combe", "11-203")] <- "Cape Grenville"
apr$Region[apr$reef_name %in% c("16_014", "Batt", "Low isles")] <- "Cairns"
apr$Region[apr$reef_name %in% c("Corbett", "Noddy", "Blanchard","13-077","13-122","13-093b")] <- "Princess Charlotte Bay"
apr$Region[apr$reef_name %in% c("Osprey")] <- "Coral Sea"
unique(apr[is.na(apr$Region),"reef_name"]) # ones without a region


# merge poc groups 
apr$cov <- (apr$value/ 1000) * 100
apr$terry <- apr$taxa
apr$terry <- ifelse(apr$terry=="P_damicornis", "Pocillopora", apr$terry)
apr$terry <- ifelse(apr$terry=="other_Pocillopora", "Pocillopora", apr$terry)
apr2 <- aggregate(cov~terry+reef_name+site+transID+Region, apr, sum)
head(apr2)
unique(apr2$terry)

oct$cov <- (oct$value/ 1000) * 100
oct$terry <- oct$taxa
oct$terry <- ifelse(oct$terry=="P_damicornis", "Pocillopora", oct$terry)
oct$terry <- ifelse(oct$terry=="other_Pocillopora", "Pocillopora", oct$terry)
oct2 <- aggregate(cov~terry+reef_name+site+transID+Region, oct, sum)
head(oct2)
unique(oct2$terry)

octreef <- aggregate(cov~reef_name+terry+Region, oct2, mean)
aprreef <- aggregate(cov~reef_name+terry+Region, apr2, sum)
nrow(octreef)/12
nrow(aprreef)/12

#removeR <- c("Coral Sea", "Hydrographers Passage")
#octreef  <- octreef[!octreef$Region %in% removeR, ]
#aprreef  <- aprreef[!aprreef$Region %in% removeR, ]
#nrow(octreef)/12
#nrow(aprreef)/12

# align 2024 taxa

comp <- read.csv("data/pit.csv") 
head(comp)

ctax <- read.csv("data/info/composition_taxa.csv")

tax.sums <- aggregate(cov~variable+group, comp, sum)  
rare <- tax.sums$variable[tax.sums$cov < 60 & tax.sums$group=="HC"]
rare

comp2 <- comp[comp$group=="HC",]

#comp2 <- comp2[!comp2$variable %in% rare,]

head(comp2)
unique(comp2$variable)

comp2$terry <- ctax$terry[match(comp2$variable, ctax$taxon)]
unique(comp2$terry )

comp3 <- aggregate(cov~terry+Transect+REGION+Site+Reef+Zone, comp2, sum)
head(comp3)

comp3 <- comp3[comp3$Zone=="Crest",]
comp3 <- aggregate(cov~terry+REGION+Reef+Site, comp3, mean)
head(comp3)

hist(comp3$cov)
unique(comp3$Site)

comp4 <- aggregate(cov~terry+REGION+Reef, comp3, mean)
head(comp4)
unique(comp4$Reef)

comp4 <- comp4[!comp4$REGION %in% removeR,]

comp4$ID <- paste(comp4$Reef, "2024")
octreef$ID <- paste(octreef$reef_name, "2016b")
aprreef$ID <- paste(aprreef$reef_name, "2016a")


all <- rbind(data.frame(ID=comp4$ID, cov=comp4$cov, terry=comp4$terry, t=3, reef=comp4$Reef),
data.frame(ID=octreef$ID, cov=octreef$cov, terry=octreef$terry, t=2, reef=octreef$reef_name),
data.frame(ID=aprreef$ID, cov=aprreef$cov, terry=aprreef$terry, t=1, reef=aprreef$reef_name))

#remove <- c("Batt","Low isles","Magnetic Island","Orpheus Island","Combe","Noddy","Martha","11-203","Osprey","Maxwell Reef","Ribbon 10","Switzer","13-077","13-093b","13-122","Blanchard","Corbett")
#all <- all[!all$reef %in% remove,]

wide <- acast(all, ID~terry, value.var="cov") 
head(wide)

# mds
mds<-metaMDS(sqrt(wide), k=2, distance="bray", autotransform=FALSE, trymax=1000)
#stressplot(mds)

mdspoints<-data.frame(scores(mds)$sites)
mdspoints$Reef <- all$reef[match(rownames(mdspoints), all$ID)]
mdspoints$t <- all$t[match(rownames(mdspoints), all$ID)]
#mdspoints$Time[is.na(mdspoints$Time)]<-3
head(mdspoints)
mdsvectors<- as.data.frame(mds$species)
mdsvectors$lab <- c(nrow(mdsvectors):1)
mdsvectors$lab2 <- rownames(mdsvectors)

plot_grid(
ggplot()+geom_segment(data=mdsvectors, aes(x=0, xend=MDS1, y=0, yend=MDS2), col="grey")+
lims(x=c(-1.5, 2), y=c(-1.2, 1))+
geom_text(data=mdsvectors, aes(MDS1, MDS2, label=lab2), hjust=ifelse(mdsvectors$MDS1 >0, 0, 1), size=2.5, fontface="bold")
,
ggplot()+
lims(x=c(-1.5, 2), y=c(-1.2, 1))+
stat_ellipse(data=mdspoints[mdspoints$t %in% c(1, 3),], aes(NMDS1, NMDS2,  group=as.factor(t)), geom="polygon", alpha=0.15, col="black")+
geom_point(data=mdspoints, aes(NMDS1, NMDS2, fill=as.factor(t)), shape=21, size=1, stroke=0.1))













