


#---------------------------------------------# FIGURE S6 recreate Hughes 2021

tph <- read.csv("data/data2016/Hughes2021.csv")
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
crestfit <- data.frame(max.dhw=seq(min(crestdat$max.dhw, na.rm=T), max(crestdat$max.dhw, na.rm=T), 0.1))
crestfit$pred <- predict(crestglm, crestfit, type="response")
crestfit$se <- predict(crestglm, crestfit, type="response", se=T)$se

fit.all <- rbind(tphmods, data.frame(DHW=crestfit$max.dhw, year=2024, pred=crestfit$pred, se=crestfit$se))

fit.all$type <- ifelse(fit.all$year==2024, "in-water", "aerial")

figS5 <- ggplot()+
geom_line(data=fit.all[!fit.all$year %in% c(1998, 2002),], aes(x=DHW, y=pred, linetype=type, group=as.factor(year)), size=0.3)+
geom_ribbon(data=fit.all[!fit.all$year %in% c(1998, 2002),], aes(x=DHW, ymin=pred+se*1.96, ymax=pred-se*1.96,  fill=as.factor(year), col=as.factor(year)),  alpha=0.35, size=0.1)+
scale_linetype_manual(values=c("solid","dashed"))+
labs(x="Degree Heating Weeks (C-weeks )", y="Probablity of severe\nreef bleaching")+
scale_fill_viridis(discrete=T, option="C")+scale_colour_viridis(discrete=T)+
theme_classic()+theme(legend.title=element_blank(), legend.key.height=unit(1, "mm"))
figS5

#     ggsave( "figs/supplement/figS5.jpg",figS5, height=3.5, width=4)
