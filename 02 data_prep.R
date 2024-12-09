
rm(list = ls())

library("ggplot2")
library("reshape2")
library("cowplot")
library("viridis")
library("gridExtra")
library("sf")
se <- function(x) sqrt(var(x)/length(x))


# --------------------------------- # belt transects (bleaching) 2024

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

# --------------------------------- # composition 2024

comp <- read.csv("data/original/composition2024.csv") 
comp$Total.CORAL <- as.numeric(gsub("%","", comp$Total.CORAL))
head(comp)
nrow(comp)

# long format
pcols <- c("Year", "DATE","REGION","Reef","Site", "Transect", "Zone", "Site.1", "Observer", "Depth", "Complexity..1.5.", "No..CoTS", "No..CoTS.scars", "Total.CORAL", "Notes")
comp2 <- melt(comp, id.var=pcols)
comp2$value[is.na(comp2$value)] <- 0

comp2$sand.rub <- comp$Sand..Rubble[match(comp2$Transect, comp$Transect)]
comp2$sand.rub[is.na(comp2$sand.rub)]<-0
head(comp2)
nrow(comp2)

ctax <- read.csv("data/info/composition_taxa.csv")
comp2$group <- ctax$group[match(comp2$variable, ctax$taxon)]
comp2$label <- ctax$label[match(comp2$variable, ctax$taxon)]
ctax

# calculate cover
comp2$cov <- comp2$value / (100 - comp2$sand.rub) * 100
t_cov <- aggregate(cov~Transect+Total.CORAL, comp2[comp2$group %in% c("HC"),], sum)

# --------------------------------- # 2024 coords / combine data
length(unique(comp$Transect))
length(unique(belt$Transect_Code))  

df <- read.csv("data/info/coordmatch24.csv")
df$siteID <- paste(df$Reef, df$Site, df$Zone)
head(df)

# missing data
belt[belt$Transect_Code %in% c("124_B2_C2", "LIZ_B4_S3"),] # Problem transects! (No corals no overlap!)
df <- df[!df$Transect_code %in% c("124_B2_C2", "LIZ_B4_S3"),]

# incorrect GPS - different GPS coords same site
change <- c("GPS.S", "GPS.E", "coordID", "grid.lat", "grid.lon", "gridID")
df[df$Transect_code=="MOO_B3_S1", change] <- df[df$Transect_code=="MOO_B3_S2", change]


# --------------------------------- 2016 composition data

rnames <- read.csv("data/info/reefnames.csv")
head(rnames)

idvars <- c("date", "observer" ,"reef_name","ReefNo","site", "depth", "transect_no","taxa")

apr16 <- read.csv("data/original/composition2016Apr.csv")
apr <- melt(apr16[,c(idvars, "TOTAL")], id.var=c(idvars))
apr <- apr[!apr$taxa %in% c("soft",  "other_sessile"),]
apr$transID <- paste(apr$site, apr$transect_no)
apr$reef <- rnames$use[match(apr$reef_name, rnames$april)]
oct16 <- read.csv("data/original/composition2016Oct.csv")
oct <- melt(oct16[,c(idvars, "TOTAL")], id.var=c(idvars))
oct <- oct[!oct$taxa %in% c("soft",  "other_sessile"),]
oct$transID <- paste(oct$site, oct$transect_no)
oct$reef <- rnames$use[match(oct$reef_name, rnames$oct)]
head(oct)
nrow(oct)


comp16 <- rbind(cbind(apr, month="Apr16"), cbind(oct, month="Oct16"))
comp16$transIDt <- paste(comp16$transID, comp16$month)

# (merge Poc groups to align with 2024)
comp16$cov <- (comp16$value/ 1000) * 100
comp16$terry <- ifelse(comp16$taxa=="P_damicornis", "Pocillopora", comp16$taxa)
comp16$terry <- ifelse(comp16$taxa=="other_Pocillopora", "Pocillopora", comp16$terry)
comp16 <- aggregate(cov~terry+reef+site+Region+transIDt+month, comp16, sum)
head(comp16)



# --------------------------------- Merge 2024 and 2016 composition

# merge 2024 taxa
comp2$terry <- ctax$terry[match(comp2$variable, ctax$taxon)]
#unique(comp[,c("variable", "terry")])
#head(comp2)
comp3 <- aggregate(value~terry+Zone+Transect+Site+Reef+REGION, comp2[!is.na(comp2$terry),], sum)
comp3 <- comp3[comp3$Zone=="Crest",]
head(comp3)

comp3$reef <- rnames$use[match(comp3$Reef, rnames$r24)]
comp3$reef <- ifelse(is.na(comp3$reef), comp3$Reef, comp3$reef)
unique(comp3$reef)

# MERGE !!!
all <- rbind(data.frame(ID = comp16$transIDt, region = comp16$Region, reef = comp16$reef, site=comp16$site, taxa = comp16$terry, cov = comp16$cov, t=comp16$month), 
data.frame(ID = comp3$Transect, region=comp3$REGION, reef=comp3$reef, site=comp3$Site, taxa=comp3$terry, cov=comp3$value, t="Mar24"))
head(all)

all$t2 <- ifelse(all$t=="Apr16", "1. 2016 (pre-bleaching)", ifelse(all$t=="Oct16", "2016 (post-bleaching)", ifelse(all$t=="Mar24", "3. 2024 (pre-bleaching)", NA)))

head(df)
head(all)

# --------------------------------- # save

write.csv(all,"data/composition.csv")
write.csv(df,"data/bleaching.csv")

#write.csv(bl2, "data/output/bleaching.csv")
#write.csv(comp2, "data/output/comp.csv")



# --------------------------------- 2016 composition metrics

dat16 <- aggregate(cov~reef_name+site+transIDt+Region+month, comp16, sum)

acro <- aggregate(cov~reef_name+site+transIDt, comp16[comp16$terry %in% c("tabular_Acropora", "staghorn_Acropora", "other_Acropora"),], sum)
head(acro)
dat16$acro <- acro$cov[match(dat16$transIDt, acro$transIDt)]
head(dat16)







