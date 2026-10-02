
rm(list = ls())

library("ggplot2")
library("reshape2")
library("cowplot")
library("viridis")
library("gridExtra")
library("sf")
se <- function(x) sqrt(var(x)/length(x))

# create two datasets. Transect-level observations (dhw, bleaching, cover1, cover2)
# site-level composition at FOUR timepoints (2016ab, 2024ab)

# ------------------------------------------ datasets

belt <- read.csv("data/original/belt.csv") 
belt$Survey <- ifelse(belt$Survey=="2024 - Bleaching", "Apr", ifelse(belt$Survey=="2024 - Mortality", "Oct", NA))
btax <- read.csv("data/info/bleaching_taxa.csv")
belt[belt$Genus=="",] # unidentified colonies... 
belt <- belt[!belt$Genus=="",]
bcols <- c("Juv...5cm.","X.20cm", "X20.40cm", "X40.60cm", "X.60cm")
belt <- subset(belt, select=-c(Total.no..Adults)) # sum minus juvs. 
belt <- melt(belt, id.var=colnames(belt)[!colnames(belt) %in% c(bcols)], variable.name="size")
belt$value[is.na(belt$value)] <- 0
belt$group <- btax$Group[match(belt$Genus, btax$Genus)]
head(belt)
unique(belt$value)
belt$value[belt$value %in% c("`", "")]<-NA
belt$value <- as.numeric(belt$value)


pit1 <- read.csv("data/original/pit.csv")
pit1$Survey <- ifelse(pit1$Survey=="2024 - Bleaching", "Apr", ifelse(pit1$Survey=="2024 - Mortality", "Oct", NA))
head(pit1)
pit1$Total.CORAL <- as.numeric(gsub("%","", pit1$Total.CORAL))
pcols <- c("Survey", "DATE","REGION","Reef","Site", "Transect", "Zone", "Site.1", "Observer", "Depth", "Complexity..0.5.", "No..CoTS", "No..CoTS.scars", "Total.CORAL", "Notes")
pit <- melt(pit1, id.var=pcols)
unique(pit$value)
pit$value[is.na(pit$value)] <- 0
pit$value[pit$value %in% c('0.00%')]<-0
pit$value <- as.numeric(pit$value)

ctax <- read.csv("data/info/composition_taxa.csv")
pit$group <- ctax$group[match(pit$variable, ctax$taxon)]
pit$label <- ctax$label[match(pit$variable, ctax$taxon)]
pit$REGION <- ifelse(pit$REGION=="Cape Cleveland", "Cape Bowling Green", pit$REGION)
pit$REGION <- ifelse(pit$REGION=="Capricorn Group", "Capricorn Bunkers", pit$REGION)
head(pit)

# ------------------------------------------ align data labels

# no return trips to cormrant / thetford / 2 sites at buggatti 
# no return to cape grenville or prin char bay

# oct corrections
belt$Transect_Code <- gsub("MILL", "MIL",belt$Transect_Code ) #
belt$Transect_Code[belt$Reef=="Milln" & belt$Site=="B1 - NW" & belt$Zone=="Crest" & belt$Transect=="1"] <- "MIL_B1_C1"
belt$Transect_Code[belt$Reef=="Milln" & belt$Site=="B2 - Lagoon" & belt$Zone=="Crest" & belt$Transect=="3"] <- "MIL_B2_C3"
belt$Transect_Code[belt$Reef=="Moore" & belt$Site=="B1 - South" & belt$Zone=="Crest" & belt$Transect=="1"] <- "MOO_B1_C1"

# apr corrections
belt$Transect[belt$Transect_Code=="HER_B2_C2"] <- "2"
belt$Transect[belt$Transect_Code=="HER_B3_C2"] <- "2"
belt$Transect[belt$Transect_Code=="CHA_B3_C2"] <- "2"
belt$Transect_Code[belt$Reef=="Heron" & belt$Site=="B3 - North Bay" & belt$Zone=="Slope" & belt$Transect=="2"] <- "HER_B3_S2"
belt$Transect_Code[belt$Reef=="Heron" & belt$Site=="B3 - North Bay" & belt$Zone=="Slope" & belt$Transect=="1"] <- "HER_B3_S1"
belt$Coral.Health[belt$Coral.Health=="h - Healthy (<5% Recent Mortality)"]<-"H - Healthy (<5% Recent Mortality)"
belt$Coral.Health[belt$Coral.Health=="Health"]<-"H - Healthy (<5% Recent Mortality)"


# sitenames different apr vs oct
aprnames <- belt[belt$Survey=="Apr",]
belt$Site <- aprnames$Site[match(belt$Transect_Code, aprnames$Transect_Code)]
belt$Reef[belt$Reef=="Ribbon 8 "]<-"Ribbon 8"
belt$Reef[belt$Reef=="Chauvel"]<-"Chavel"

head(pit)
head(belt)

# TRANSECT DF ..... 
pit$obs <- paste(pit$Transect, pit$Survey)
belt$obs <- paste(belt$Transect_Code, belt$Survey)
trans <- data.frame(obs=unique(c(pit$obs, belt$obs)))
# 616 transects in the dataset

trans$pit <- pit$obs[match(trans$obs, pit$obs)]
trans$belt <- belt$obs[match(trans$obs, belt$obs)]
trans

#old <- read.csv("data/bleachingold.csv")
#old$obs <- paste(old$Transect_code, "Apr")
#head(old)

# ------------------------------------------ add info

tdf <- trans
cols_add <- c("Survey", "Date", "Region", "Reef", "Site", "Zone", "Transect_Code")
tdf[,cols_add] <- belt[match(tdf$obs, belt$obs), cols_add]
head(pit)
cols_pit <- c("Reef", "Site", "Transect", "Depth", "Complexity..0.5.")
tdf[,paste(cols_pit, "pit", sep="_")] <- pit[match(tdf$obs, pit$obs), cols_pit]


tdf$sand <- pit1$Sand[match(tdf$Transect_pit, pit1$Transect)]
tdf$rubble <- pit1$Rubble[match(tdf$Transect_pit, pit1$Transect)]
tdf$sand[is.na(tdf$sand)]<-0
tdf$rubble[is.na(tdf$rubble)]<-0
head(tdf)

tdf <- tdf[!tdf$Transect_pit  %in% c("124_B2_C2"),] # NO BLEACHING TRANSECT (ONLY PIT)
tdf <- tdf[!tdf$Transect_Code  %in% c("THE_B1_C3"),] # NO BLEACHING TRANSECT (ONLY PIT)


# ------------------------------------------ add info

# align with grids

grids <- read.csv("data/info/grids24.csv") 
head(grids)

grids[,c("Reef", "Transect_code", "gridID")]

cols_grids <- c("GPS.S", "GPS.E", "coordID", "grid.lat", "grid.lon", "gridID")
tdf[,cols_grids] <- grids[match(tdf$Transect_Code, grids$Transect_code), cols_grids]
head(tdf)

# gaps? 
tdf[is.na(tdf$gridID),]
tdf[tdf$Transect_Code %in% "MOO_B1_C3",cols_grids] <- tdf[tdf$obs %in% "MOO_B1_C1 Apr",cols_grids]
tdf[tdf$Transect_Code %in% "HER_B1_C3",cols_grids] <- tdf[tdf$obs %in% "HER_B1_C1 Apr",cols_grids]
tdf[tdf$Transect_Code %in% "WIS_B1_C3",cols_grids] <- tdf[tdf$obs %in% "WIS_B1_C1 Apr",cols_grids]
tdf[tdf$Transect_Code %in% "WIS_B1_C3",cols_grids] <- tdf[tdf$obs %in% "WIS_B1_C1 Apr",cols_grids]

tdf[is.na(tdf$gridID),]


tdf[,c("Reef", "grid.lat","grid.lon","gridID")]



# --------------------------------- # heat stress/heat history

dhw <- read.csv("data/noaa_sst/sst_gbr.csv")

# max dhw in 2024
head(dhw)
dhw24 <- dhw[dhw$year==2024,]
max.dhw <- aggregate(dhw~gridID, dhw24, max)
tdf$max.dhw <- max.dhw$dhw[match(tdf$gridID, max.dhw$gridID)]

# heat stress frequency / recovery interval
dhw$aboveN <- ifelse(dhw$dhw> 6, 1, 0) # above 6
t.use <- dhw[dhw$aboveN==1,]
t.use <- t.use[!t.use$year %in% c(1998,2002),]
t.use <- t.use[!t.use$year==2024,]
time <- aggregate(year~ X + Y + gridID, t.use, max) # max year in the dataset. 
time$last <- 2024 - time$year
freq <- aggregate(aboveN ~ X + Y + gridID, dhw[!dhw$year %in% c(1998,2002),], sum)

tdf$tlast <- time$last[match(tdf$gridID, time$gridID)]
tdf$sumN <- freq$aboveN[match(tdf$gridID, freq$gridID)]

fdat2 <- data.frame(table(tdf$sumN))
fdat2$p <- fdat2$Freq / sum(table(tdf$sumN)) *100
tdat2 <- data.frame(table(tdf$tlast))
tdat2$p <- tdat2$Freq / sum(table(tdf$tlast)) *100

plot_grid(ggplot(fdat2, aes(x=Var1, y=p))+geom_bar(stat="identity"),
ggplot(tdat2, aes(x=Var1, y=p))+geom_bar(stat="identity"))

head(tdf)

# heat stress frequency / recovery interval (excluding 2024)
df.use <- dhw[dhw$year %in% c(2016, 2017, 2020, 2022),]
t.use2 <- df.use[df.use$aboveN==1,]
time2 <- aggregate(year~ X + Y + gridID, t.use2, max) # max year in the dataset. 
time2$last <- 2024 - time2$year
freq2 <- aggregate(aboveN ~ X + Y + gridID, df.use, sum)

tdf$tlast_pre <- time2$last[match(tdf$gridID, time2$gridID)]
tdf$sumN_pre <- freq2$aboveN[match(tdf$gridID, freq2$gridID)]

head(tdf)

tdf[,c("Reef", "grid.lat","grid.lon","gridID","max.dhw")]

# --------------------------------- # bleaching levels

belt$bcat <- ifelse(belt$Coral.Health %in% c("H - Healthy (<5% Recent Mortality)", "h - Healthy (<5% Recent Mortality)", "Health",""), "none",  ifelse(belt$Coral.Health=="P - Pale", "pale",
ifelse(belt$Coral.Health == "A - <50% Bleached", "bl_a",  ifelse(belt$Coral.Health == "B - 50-99% Bleached", "bl_b", ifelse(belt$Coral.Health == "C - 100% Bleached", "bl_c",  ifelse(belt$Coral.Health == "D - 5-50% Recent mortality", "bld_d",  ifelse(belt$Coral.Health == "E - 50-99% Recent Mortality", "bld_e",  ifelse(belt$Coral.Health == "F - 100% Recent Mortality", "bld_f", NA))))))))

belt$bcat<- factor(belt$bcat, levels=c("none", "pale", "bl_a", "bl_b", "bl_c", "bld_d", "bld_e", "bld_f"))
unique(belt$bcat)

belt$bleachingYN <- ifelse(belt$bcat %in% c("bl_a", "bl_b", "bl_c", "bld_e", "bld_d", "bld_f"), "y", ifelse(belt$bcat %in% c("pale", "none"), "n", NA))
unique(belt$bleachingYN)

belt$palebleach <- ifelse(belt$bcat %in% c("pale","bl_a", "bl_b", "bl_c", "bld_e", "bld_d", "bld_f"), "y", ifelse(belt$bcat %in% c("none"), "n", NA))
unique(belt$palebleach)

belt$severe <- ifelse(belt$bcat %in% c("bl_b", "bl_c", "bld_e", "bld_d", "bld_f"), "y", ifelse(belt$bcat %in% c("bl_a","pale", "none"), "n", NA)) # all except A? 
unique(belt$severe)

belt$dying <- ifelse(belt$bcat %in% c("bld_e", "bld_d", "bld_f"), "y", ifelse(belt$bcat %in% c("bl_b", "bl_c", "bl_a","pale", "none"), "n", NA)) # all except A? 
unique(belt$dying)

#ggplot(belt, aes(value,Transect_Code, fill=bcat))+geom_bar(stat="identity")+ scale_fill_viridis(discrete=T)+facet_wrap(~Reef, scales="free_y")+theme(axis.text.y=element_blank())

# no soft coral / no juveniles

# subset
hc1 <- belt[belt$group=="HC",] # hard coral
hcsc <- belt[belt$group %in% c("HC", "SC"),] # hard/soft coral
adults1 <- hc1[!hc1$size=="Juv...5cm.",]
adults_sc <- hcsc[!hcsc$size=="Juv...5cm.",]
juvs1 <- hc1[!hc1$size=="Juv...5cm.",]

unique(adults1$value)
# total colonies
ncoral <- aggregate(value~obs, adults1, sum)
ncoralSC <- aggregate(value~obs, adults_sc, sum)
#njuvs <-  aggregate(value~Transect_Code, juvs1, sum)
tdf$Ncoral <- ncoral$value[match(tdf$obs, ncoral$obs)]
tdf$NcoralSC <- ncoralSC$value[match(tdf$obs, ncoralSC$obs)]
head(tdf)

# total bleached
nbleachSC <- aggregate(value~obs, adults_sc[adults_sc$palebleach =="y",], sum)
# nsev <- aggregate(value~Transect_Code, adults1[adults1$severe =="y",], sum)
nbleach <- aggregate(value~obs, adults1[adults1$palebleach =="y",], sum)
tdf$nbleach <- nbleach$value[match(tdf$obs, nbleach$obs)]
tdf$nbleach[is.na(tdf$nbleach)] <- 0
tdf$nbleachSC <- nbleachSC$value[match(tdf$obs, nbleachSC$obs)]
tdf$nbleachSC[is.na(tdf$nbleachSC)] <- 0
head(tdf)

# total dead
ndead <- aggregate(value~obs, adults1[adults1$dying=="y",], sum)
tdf$ndead <- ndead$value[match(tdf$obs, ndead$obs)]
tdf$ndead[is.na(tdf$ndead)] <- 0
length(unique(ndead$obs))

# proportions
tdf$pdead <- tdf$ndead/tdf$Ncoral
tdf$pbleach <- tdf$nbleach/tdf$Ncoral
head(tdf)

# --------------------------------- # composition 2024
head(pit)
unique(pit$variable)
unique(pit$REGION)

pit$sand <- pit1$Sand[match(pit$Transect, pit1$Transect)]
pit$rubble <- pit1$Rubble[match(pit$Transect, pit1$Transect)]

# calculate cover
pit$cov <- pit$value / (100 - pit$sand - pit$rubble) * 100
t_cov <- aggregate(cov~Transect+Total.CORAL, pit[pit$group %in% c("HC"),], sum)
ggplot(pit, aes(value, cov))+geom_point()

# ------------------------------------------------ # Composition metrics 2024
 
# coral cover
hc2 <- pit[pit$group %in% c("HC"),] # hard coral
t_cov <- aggregate(cov~obs+Total.CORAL, hc2, sum)
tdf$coral_cov <- t_cov$cov[match(tdf$obs, t_cov$obs)]

# genera
unique(pit$variable)
acro <- c("Acropora...Tabular",  "Acropora...Staghorn", "Acropora...other", "Isopora")
pocil <- c("Pocillopora", "Seriatopora", "Stylophora")
porit <- c("Porites...Branching", "Porites....Massive")

# acropora cover
acrocov <- aggregate(cov~obs, pit[pit$variable %in% acro,], sum)
tdf$acro <- acrocov$cov[match(tdf$obs, acrocov$obs)]

# pocillopora cover
poccov <- aggregate(cov~obs, pit[pit$variable %in% pocil,], sum)
tdf$poc<- poccov$cov[match(tdf$obs, poccov$obs)]
#tdf$pocR<- tdf$poc/tdf$coral_cov

# porites cover
porcov <- aggregate(cov~obs, pit[pit$variable %in% porit,], sum)
tdf$por <- porcov$cov[match(tdf$obs, porcov$obs)]
#tdf$porR <- tdf$por/tdf$coral_cov

# tabular cover
tabcov <- aggregate(cov~obs, pit[pit$variable %in% c("Acropora...Tabular"),], sum)
tdf$tab <- tabcov$cov[match(tdf$obs, tabcov$obs)]
#tdf$tabR <- tdf$tab/tdf$coral_cov

head(tdf)
unique(tdf$Region)
tdf[is.na(tdf$Region),] #124_B2_C2 missed in trans?

# --------------------------------- 2016 composition data

rnames <- read.csv("data/data2016/reefnames.csv")
head(rnames)

idvars <- c("date", "observer" ,"reef_name","ReefNo","site", "depth", "transect_no","taxa")

apr16 <- read.csv("data/data2016/composition2016Apr.csv")
head(apr16)
apr <- melt(apr16[,c(idvars, "TOTAL")], id.var=c(idvars))
apr <- apr[!apr$taxa %in% c( "other_sessile"),]
apr$transID <- paste(apr$site, apr$transect_no)
apr$reef <- rnames$use[match(apr$reef_name, rnames$april)]
apr$Region <- rnames$region[match(apr$reef, rnames$use)]
head(apr)
unique(apr$variable)

oct16 <- read.csv("data/data2016/composition2016Oct.csv")
head(oct16)
unique(oct16$date)[order(unique(oct16$date))]
oct <- melt(oct16[,c(idvars, "TOTAL")], id.var=c(idvars))
oct <- oct[!oct$taxa %in% c(  "other_sessile"),]
oct$transID <- paste(oct$site, oct$transect_no)
oct$reef <- rnames$use[match(oct$reef_name, rnames$oct)]
oct$Region <- rnames$region[match(oct$reef, rnames$use)]
head(oct)
nrow(oct)
unique(oct$taxa)

comp16 <- rbind(cbind(apr, month="Apr16"), cbind(oct, month="Oct16"))
comp16$transIDt <- paste(comp16$transID, comp16$month)

unique(comp16$reef)
head(comp16)

# (merge Poc groups to align with 2024)
comp16$cov <- (comp16$value/ 1000) * 100
comp16$align <- comp16$taxa
comp16$align <- ifelse(comp16$taxa=="P_damicornis", "Pocillopora", comp16$taxa)
comp16$align <- ifelse(comp16$taxa=="other_Pocillopora", "Pocillopora", comp16$align)

comp16 <- aggregate(cov~taxa+align+reef+site+Region+transIDt+month+ReefNo, comp16, sum)
head(comp16)

reefIDs <- unique(comp16[,c("reef", "ReefNo")])

# --------------------------------- Check against other 2016 data

head(comp16)
comp16[comp16$reef=="Dugong",]

av1 <- aggregate(cov~align+transIDt+reef+site+Region+month+ReefNo, comp16, sum)
av2 <- aggregate(cov~align+reef+site+Region+month+ReefNo, av2, sum) # site level mean
av3 <- aggregate(cov~align+reef+Region+month+ReefNo, av1, sum) # reef level mean
av3[av3$reef=="Dugong",]
unique(av3$reef)
# ggplot(av3, aes(month, cov))+geom_boxplot()+scale_y_sqrt()

head(av3)

chA <- read.csv("data/data2016/Data_Global_warming_transforms_coral_reef_assemblages/TaxonomicDataA.csv")[,c(1:16)]
chO <- read.csv("data/data2016/Data_Global_warming_transforms_coral_reef_assemblages/TaxonomicDataO.csv")[,c(1:16)]
chk <- rbind(cbind(chA, month="Apr16"), cbind(chO, month="Oct16"))
chk <- melt(chk, id.var=c("ReefID", "month"))
head(chk)
head(av3)
unique(av3$align)
unique(chk$variable)
chk$variable <- ifelse(chk$variable=="Other.Acropora", "other_Acropora", as.character(chk$variable))
chk$variable <- ifelse(chk$variable=="Tabular.Acropora", "tabular_Acropora", as.character(chk$variable))
chk$variable <- ifelse(chk$variable=="Other.Scleractinia", "other_scleractinians", as.character(chk$variable))
chk$variable <- ifelse(chk$variable=="Staghorn.Acropora", "staghorn_Acropora", as.character(chk$variable))
chk$link <- paste(chk$ReefID, chk$month, chk$variable)
av3$link <- paste(av3$ReefNo, av3$month, av3$align)
av3$check <- chk$value[match(av3$link, chk$link)]
ggplot(av3, aes(cov/10, check))+geom_point()+facet_wrap(~align, scales="free")+geom_abline(slope=1)+geom_smooth(method="lm",se=F) # raw(used) vals higher than published vals for massives
head(av3)

head(chk)
unique(chk$ReefID)
unique(av3$ReefNo)
unique(av3$ReefNo)



# --------------------------------- Merge 2024 and 2016 composition

# merge 2024 taxa
pit$align <- ctax$align[match(pit$variable, ctax$taxon)]
pit$align <- ifelse(pit$group=="SC", "soft", pit$align)
unique(pit[,c("variable", "align")])
#head(pit)

head(pit)
head(comp16)


pit$reef <- rnames$use[match(pit$Reef, rnames$r24)]
pit$reef <- ifelse(is.na(pit$reef), pit$Reef, pit$reef)
unique(pit$reef)
head(pit)

# MERGE !!!
all <- rbind(data.frame(ID = comp16$transIDt, region = comp16$Region, reef = comp16$reef, site=comp16$site, zone="Crest", taxa = comp16$taxa, align=comp16$align, cov = comp16$cov, t=comp16$month), 
data.frame(ID = paste(pit$Transect, pit$Survey), region=pit$REGION, reef=pit$reef, site=pit$Site, zone=pit$Zone, taxa=pit$variable,  align=pit$align, cov=pit$value, t=paste(pit$Survey, "24", sep="")))
head(all)
unique(all$t)

freqs <- data.frame(table(unique(all[,c("t", "reef")])$reef))
freqs[freqs$Freq>2,]

ntimes <- data.frame(table(unique(all[,c("reef", "t")])$reef))
table(ntimes$Freq)
all$ntimes <-ntimes$Freq[match(all$reef, ntimes$Var1)] 

all$tlab <- ifelse(all$t=="Apr16", "2016a", ifelse(all$t=="Oct16", "2016b", ifelse(all$t=="Apr24", "2024a", ifelse(all$t=="Oct24", "2024b", NA))))
head(all)
tail(all)
unique(all$ID)
tdf$Survey


# --------------------------------- # edit regions


unique(tdf$Region)
tdf$region2 <- ifelse(tdf$Region %in% "Cairns", "Cairns", ifelse(tdf$Region %in% "Lizard", "Cooktown",ifelse(tdf$Region %in% "Capricorn Bunkers", "Gladstone", ifelse(tdf$Region %in% "Cape Cleveland", "Townsville", ifelse(tdf$Region %in% c("Hydrographers Passage"), "Mackay", tdf$Region )))))
unique(tdf$region2)


unique(all$region)
all$region2 <- ifelse(all$region %in% "Cairns", "Cairns", ifelse(all$region %in% "Lizard", "Cooktown",ifelse(all$region %in% "Capricorn Bunkers", "Gladstone", ifelse(all$region %in% "Cape Bowling Green", "Townsville", ifelse(all$region %in% c("Hydrographers Passage"), "Mackay", all$region )))))
unique(all$region2)


# --------------------------------- # save

tdf$reef_use <- rnames$use[match(tdf$Reef, rnames$r24)]
tdf$reef_use <- ifelse(is.na(tdf$reef_use), tdf$Reef, tdf$reef_use) 
head(tdf)
head(all)


write.csv(all,"data/composition.csv")
write.csv(tdf,"data/transects.csv")




# --------------------------------- # other 2016

head(chk)
rnames <- read.csv("data/data2016/reefnames.csv")
head(rnames)

chk$Region <- rnames$region[match(chk$ReefID, rnames$ReefNo)]


comp16[comp16$ReefNo %in% c(unique(chk[is.na(chk$Region),"ReefID"])),]

head(chk)

chk2 <- aggregate(value~ReefID+month+Region, chk, sum)

tdfc <- tdf[tdf$Zone=="Crest",]
chk3 <- rbind(data.frame(reef=chk2$ReefID, t = chk2$month, cov=chk2$value, region=chk2$Region), 
data.frame(reef=tdfc$Site, t="x2024", cov=tdfc$coral_cov, region=tdfc$Region))


head(chk3)


ggplot(chk3, aes(t, cov))+geom_boxplot()+facet_wrap(~region, scales="free")



chk2 <- aggregate(value~ReefID+month+Region, chk, sum)





