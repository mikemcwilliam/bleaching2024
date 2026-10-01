


#---------------------------------------------# FIGURE S4  - crest/zone

slopedat <- sites[sites$Zone=="Slope",]

betamodSL = glm(pbleach ~ max.dhw, family="quasibinomial", data=slopedat)
#betamod3 = betareg(pdead ~ max.dhw, data=crestdat)
new.dataSL <- data.frame(max.dhw = seq(min(slopedat$max.dhw), max(slopedat$max.dhw), 0.1))
fit.datSL <- cbind(new.dataSL, predict(betamodSL, new.dataSL))
fit.datSL <- cbind(new.dataSL, data.frame(predict(betamodSL, new.dataSL, se.fit = TRUE, type="response", interval = "confidence", level = 0.5)))
head(fit.datSL )
summary(betamodSL)

zones1 <- ggplot()+
geom_point(data=sites, aes(x=max.dhw, y=pbleach*100, col=Zone), shape=21, size=0.5)+
geom_ribbon(data=fit.datX, aes(max.dhw, ymax=(bifit+(bise*2))*100, ymin=(bifit-bise*2)*100), alpha=0.1)+
#geom_line(data=curves[curves$data=="sites",], aes(x=predictor, y=betafit*100))+
geom_ribbon(data=fit.datSL, aes(x=max.dhw, ymax=((fit+se.fit)*100), ymin=((fit-se.fit)*100)), fill="black", col=NA, alpha=0.2)+
geom_line(data=fit.datSL, aes(x=max.dhw, y=fit*100))+
geom_line(data=fit.datX, aes(x=max.dhw, y=bifit*100), col="grey")+
scale_colour_manual(values=c("grey", "black"))+
geom_hline(yintercept=30, linetype="dotted")+
scale_x_log10()+
labs(x="Degree Heating Weeks", y="% bleaching")+
theme_classic()+theme(legend.title=element_blank())
zones1

sites$sev01 <- ifelse(sites$pbleach>0.3, 1,0)
slopedat <- sites[sites$Zone=="Slope",]
slopeglm <- glm(sev01 ~ max.dhw, family="binomial", data=slopedat) 
slopefit <- data.frame(max.dhw=seq(min(slopedat$max.dhw), max(slopedat$max.dhw), 0.1))
slopefit$pred <- predict(slopeglm, slopefit, type="response")
slopefit$se <- predict(slopeglm, slopefit, type="response", se=T)$se

sites$sev01 <- ifelse(sites$pbleach>0.3, 1,0)
crestdat <- sites[sites$Zone=="Crest",]
crestglm <- glm(sev01 ~ max.dhw, family="binomial", data=crestdat) 
crestfit <- data.frame(max.dhw=seq(min(crestdat$max.dhw, na.rm=T), max(crestdat$max.dhw, na.rm=T), 0.1))
crestfit$pred <- predict(crestglm, crestfit, type="response")
crestfit$se <- predict(crestglm, crestfit, type="response", se=T)$se

zoneglm <- rbind(cbind(crestfit, Zone="Crest"), cbind(slopefit, Zone="Slope"))
head(zoneglm)

zones2 <- ggplot()+
geom_line(data=zoneglm, aes(x=max.dhw, y=pred, col=Zone))+
geom_ribbon(data=zoneglm, aes(x=max.dhw, ymin=pred-se, ymax=pred+se, fill=Zone), alpha=0.35)+
scale_colour_manual(values=c("grey", "black"))+scale_fill_manual(values=c("grey", "black"))+
labs(x="Degree Heating Weeks", y="Probability of severe\nreef bleaching")+
theme_classic()+theme(legend.title=element_blank())


betamod4 = betareg(pdead ~ max.dhw, data=slopedat)
fit.dat4 <- data.frame(max.dhw = seq(min(slopedat$max.dhw), max(slopedat$max.dhw), 0.1))
fit.dat4$fit <- predict(betamod4, fit.dat4)
summary(betamod4)

betamodSL2 = glm(pdead ~ max.dhw, family="quasibinomial", data=slopedat)
#betamod3 = betareg(pdead ~ max.dhw, data=crestdat)
new.dataSL2 <- data.frame(max.dhw = seq(min(slopedat$max.dhw), max(slopedat$max.dhw), 0.1))
fit.datSL2 <- cbind(new.dataSL2, predict(betamodSL2, new.dataSL2))
fit.datSL2 <- cbind(new.dataSL2, data.frame(predict(betamodSL2, new.dataSL2, se.fit = TRUE, type="response", interval = "confidence", level = 0.5)))
head(fit.datSL2)
summary(betamodSL2)


zonesbeta <- rbind(cbind(fit.dat4, Zone="Slope"),cbind(fit.dat2[,c(1:2)], Zone="Crest"))

zones4 <- ggplot()+
geom_point(data=sites, aes(x=max.dhw, y=pdead*100, col=Zone), size=0.5, shape=21)+
scale_x_log10()+
geom_line(data=fit.datSL2, aes(x=max.dhw, y=fit*100))+
geom_line(data=fit.dat2, aes(x=max.dhw, y=fit*100), col="grey")+
geom_ribbon(data=fit.dat2, aes(x=max.dhw, ymax=((fit+se.fit)*100), ymin=((fit-se.fit)*100)), col=NA, alpha=0.2)+
#geom_line(data=zonesbeta, aes(x=max.dhw, y=fit*100, col=Zone))+
geom_ribbon(data=fit.datSL2, aes(x=max.dhw, ymax=((fit+se.fit)*100), ymin=((fit-se.fit)*100)), fill="black", col=NA, alpha=0.2)+
scale_colour_manual(values=c("grey", "black"))+
labs(x="Degree Heating Weeks", y="% recent mortality")+
theme_classic()+theme(legend.title=element_blank())
zones4


zones5 <-ggplot(smort, aes(max.dhw, Lchange, col=Zone))+
geom_hline(yintercept=0, size=0.1)+
geom_point(size=0.5, shape=21)+
geom_smooth(method="lm", size=0.5)+
labs(x="Degree Heating Weeks", y="Change in % cover")+
scale_y_continuous(breaks=brks, labels=labs)+
scale_colour_manual(values=c("grey", "black"))+scale_fill_manual(values=c("grey", "black"))+
theme_classic()+theme(legend.title=element_blank())
zones5



figS4 <- plot_grid(plot_grid(zones1+guides(col="none"), 
zones2+guides(col="none", fill="none"), 
zones4+guides(col="none"),
zones5+guides(col="none"),
nrow=2,  labels=c("A", "B", "C", "D"), align="hv", label_size=9), get_legend(zones1), rel_widths=c(1,0.2))
figS4

#     ggsave( "figs/supplement/figS4.jpg",figS4, height=6, width=6)
