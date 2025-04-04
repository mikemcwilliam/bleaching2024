
rm(list = ls())

library("ggplot2")
library("reshape2")
library("cowplot")
library("viridis")
library("gridExtra")
library("sf")
se <- function(x) sqrt(var(x)/length(x))

# --------------------------------- # bleaching data (2024)

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

tdf <- read.csv("data/info/coordmatch24.csv") # transect with matched heat data
belt[belt$Transect_Code %in% c("124_B2_C2", "LIZ_B4_S3"),] #  (No corals no overlap!)
tdf <- tdf[!tdf$Transect_code %in% c("124_B2_C2", "LIZ_B4_S3"),]
nrow(tdf)

# incorrect GPS - different GPS coords same site
#change <- c("GPS.S", "GPS.E", "coordID", "grid.lat", "grid.lon", "gridID")
#tdf[tdf$Transect_code=="MOO_B3_S1", change] <- tdf[tdf$Transect_code=="MOO_B3_S2", change]

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

# --------------------------------- # composition 2024

comp <- read.csv("data/original/composition2024.csv") 
comp$Total.CORAL <- as.numeric(gsub("%","", comp$Total.CORAL))
head(comp)
nrow(comp)

# long format
pcols <- c("Year", "DATE","REGION","Reef","Site", "Transect", "Zone", "Site.1", "Observer", "Depth", "Complexity..1.5.", "No..CoTS", "No..CoTS.scars", "Total.CORAL", "Notes")
comp24 <- melt(comp, id.var=pcols)
comp24$value[is.na(comp24$value)] <- 0

comp24$sand.rub <- comp$Sand..Rubble[match(comp24$Transect, comp$Transect)]
comp24$sand.rub[is.na(comp24$sand.rub)]<-0
head(comp24)
nrow(comp24)

ctax <- read.csv("data/info/composition_taxa.csv")
comp24$group <- ctax$group[match(comp24$variable, ctax$taxon)]
comp24$label <- ctax$label[match(comp24$variable, ctax$taxon)]
ctax

# calculate cover
comp24$cov <- comp24$value / (100 - comp24$sand.rub) * 100
t_cov <- aggregate(cov~Transect+Total.CORAL, comp24[comp24$group %in% c("HC"),], sum)

comp24$REGION <- ifelse(comp24$REGION=="Cape Cleveland", "Cape Bowling Green", comp24$REGION)
comp24$REGION <- ifelse(comp24$REGION=="Capricorn Group", "Capricorn Bunkers", comp24$REGION)

head(comp24)
unique(comp24$variable)
unique(comp24$REGION)

# ------------------------------------------------ # Composition metrics 2024
 
# check overlap
mat <- data.frame(codes = unique(c(comp24$Transect, tdf$Transect_code)))
nrow(mat)
mat$comp <- comp24$Transect[match(mat$code, comp24$Transect)]
mat$belt <- tdf$Transect_code[match(mat$code, tdf$Transect_code)]
mat # 16 comp but no belt... 2 belt but no comp

# coral cover
hc2 <- comp24[comp24$group %in% c("HC"),] # hard coral
t_cov <- aggregate(cov~Transect+Total.CORAL, hc2, sum)
tdf$coral_cov <- t_cov$cov[match(tdf$Transect_code, t_cov$Transect)]

# genera
unique(comp24$variable)
acro <- c("Acropora...Tabular",  "Acropora...Staghorn", "Acropora...other", "Isopora")
pocil <- c("Pocillopora", "Seriatopora", "Stylophora")
porit <- c("Porites...Branching", "Porites....Massive")

# acropora cover
acrocov <- aggregate(cov~Transect, comp24[comp24$variable %in% acro,], sum)
tdf$acro <- acrocov$cov[match(tdf$Transect_code, acrocov$Transect)]

# pocillopora cover
poccov <- aggregate(cov~Transect, comp24[comp24$variable %in% pocil,], sum)
tdf$poc<- poccov$cov[match(tdf$Transect_code, poccov$Transect)]
#tdf$pocR<- tdf$poc/tdf$coral_cov

# porites cover
porcov <- aggregate(cov~Transect, comp24[comp24$variable %in% porit,], sum)
tdf$por <- porcov$cov[match(tdf$Transect_code, porcov$Transect)]
#tdf$porR <- tdf$por/tdf$coral_cov

# tabular cover
tabcov <- aggregate(cov~Transect, comp24[comp24$variable %in% c("Acropora...Tabular"),], sum)
tdf$tab <- tabcov$cov[match(tdf$Transect_code, tabcov$Transect)]
#tdf$tabR <- tdf$tab/tdf$coral_cov

head(tdf)
unique(tdf$Region)

# --------------------------------- 2016 composition data

rnames <- read.csv("data/info/reefnames.csv")
head(rnames)

idvars <- c("date", "observer" ,"reef_name","ReefNo","site", "depth", "transect_no","taxa")

apr16 <- read.csv("data/original/composition2016Apr.csv")
apr <- melt(apr16[,c(idvars, "TOTAL")], id.var=c(idvars))
apr <- apr[!apr$taxa %in% c( "other_sessile"),]
apr$transID <- paste(apr$site, apr$transect_no)
apr$reef <- rnames$use[match(apr$reef_name, rnames$april)]
apr$Region <- rnames$region[match(apr$reef, rnames$use)]
oct16 <- read.csv("data/original/composition2016Oct.csv")
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

# (merge Poc groups to align with 2024)
comp16$cov <- (comp16$value/ 1000) * 100
comp16$align <- ifelse(comp16$taxa=="P_damicornis", "Pocillopora", comp16$taxa)
comp16$align <- ifelse(comp16$taxa=="other_Pocillopora", "Pocillopora", comp16$align)
comp16 <- aggregate(cov~taxa+align+reef+site+Region+transIDt+month, comp16, sum)
head(comp16)

# --------------------------------- Merge 2024 and 2016 composition

# merge 2024 taxa
comp24$align <- ctax$align[match(comp24$variable, ctax$taxon)]
comp24$align <- ifelse(comp24$group=="SC", "soft", comp24$align)
unique(comp24[,c("variable", "align")])
#head(comp24)

head(comp24)
head(comp16)

#comp24 <- aggregate(value~align+Zone+Transect+Site+Reef+REGION, comp24[!is.na(comp24$align),], sum)
#comp24 <- comp24[comp24$Zone=="Crest",]
#head(comp24)

comp24$reef <- rnames$use[match(comp24$Reef, rnames$r24)]
comp24$reef <- ifelse(is.na(comp24$reef), comp24$Reef, comp24$reef)
unique(comp24$reef)

head(comp24)

# MERGE !!!
all <- rbind(data.frame(ID = comp16$transIDt, region = comp16$Region, reef = comp16$reef, site=comp16$site, zone="Crest", taxa = comp16$taxa, align=comp16$align, cov = comp16$cov, t=comp16$month), 
data.frame(ID = paste(comp24$Transect, "Mar24"), region=comp24$REGION, reef=comp24$reef, site=comp24$Site, zone=comp24$Zone, taxa=comp24$variable,  align=comp24$align, cov=comp24$value, t="Mar24"))
head(all)

ntimes <- data.frame(table(unique(all[,c("reef", "t")])$reef))
table(ntimes$Freq)
all$ntimes <-ntimes$Freq[match(all$reef, ntimes$Var1)] 

all$tlab <- ifelse(all$t=="Apr16", "2016a", ifelse(all$t=="Oct16", "2016b", ifelse(all$t=="Mar24", "2024", NA)))
head(all)
tail(all)

# --------------------------------- # save

tdf$reef_use <- rnames$use[match(tdf$Reef, rnames$r24)]
tdf$reef_use <- ifelse(is.na(tdf$reef_use), tdf$Reef, tdf$reef_use) 
head(tdf)
head(all)

write.csv(all,"data/composition.csv")
write.csv(tdf,"data/bleaching.csv")

# --------------------------------- # save for review

head(df)




# --------------------------------- 2016 composition metrics

dat16 <- aggregate(cov~reef_name+site+transIDt+Region+month, comp16, sum)

acro <- aggregate(cov~reef_name+site+transIDt, comp16[comp16$align %in% c("tabular_Acropora", "staghorn_Acropora", "other_Acropora"),], sum)
head(acro)
dat16$acro <- acro$cov[match(dat16$transIDt, acro$transIDt)]
head(dat16)







