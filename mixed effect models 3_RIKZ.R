rm(list=ls()) # ripulisce la memoria di R# carica il pacchetto "pacman", che contiene la funzione p_load():if (!require(pacman)) install.packages('pacman'); library(pacman)# altri pacchetti:p_load(doBy)p_load(emmeans)p_load(ggplot2)p_load(DHARMa, performance) # DHARMa e performance producono test e grafici diagnostici per GLM e mixed-effect modelsp_load(lme4, lmerTest, glmmTMB) # per fittare modelli mistip_load(MuMIn) # per stimare l'R-quadro dei modelli mistip_load(devtools)if (!require(AED)) install_github("romunov/AED"); library(AED)# contiene il dataset RIKZ

data(RIKZ)RIKZ$Exposure <- as.factor(RIKZ$Exposure)

ggplot(RIKZ, aes(x = NAP, y = Richness, colour = Exposure)) +    geom_point() +     geom_smooth(method = "glm", method.args=list(family = "poisson")) +    labs(x = "Height of sampling station compared to mean tidal level [m]",          y = "Species richness") +    theme(text = element_text(family="Times", size=15)) +    theme_classic()

mem1 <- glmmTMB(Richness ~ NAP * Exposure + (1 | Beach),                family="poisson",                data = RIKZ, REML=F)

# grafici diagnostici usando le funzioni di DHARMa:mem1Resids <- simulateResiduals(mem1, n=3000, use.u=F)par(mfrow = c(1, 2))plotQQunif(mem1Resids)mtext(text = "(a)", side = 3, adj = 0, line = 2)plotResiduals(mem1Resids, quantreg = T)mtext(text = "(b)", side = 3, adj = 0, line = 2)


mem1_add <- glmmTMB(Richness ~ NAP + Exposure + (1 | Beach),
                family="poisson",
                data = RIKZ, REML=F)

mem1_NAP <- glmmTMB(Richness ~ NAP + (1 | Beach),
                family="poisson",
                data = RIKZ, REML=F)

mem1_exp <- glmmTMB(Richness ~ Exposure + (1 | Beach),
                family="poisson",
                data = RIKZ, REML=F)

mem1_ranef <- glmmTMB(Richness ~ 1 + (1 | Beach),
                family="poisson",
                data = RIKZ, REML=F)

mem1_null <- glmmTMB(Richness ~ 1,
                family="poisson",
                data = RIKZ, REML=F)

AIC(mem1, mem1_add, mem1_exp, mem1_NAP, mem1_ranef, mem1_null)                                
                
mem1_reml <- glmmTMB(Richness ~ NAP + Exposure + (1 | Beach),                family="poisson",                data = RIKZ, REML=T)

pendenze <- emtrends(mem1_reml,                      pairwise ~ Exposure,                      var = "NAP")pendenze

emmeans::test(pendenze)

emmeans(mem1_reml, 			specs = ~ NAP + Exposure,			at = list(NAP = 0),
			type="response"			)

# predittori per i quali ottenere predizioni:preddata <- expand.grid(NAP = seq(from=-1.5, to=2.5, length.out=100),             Exposure = as.factor(c(8,10,11)))preddata$Beach <- rep(NA, length(preddata[,1]))# BOOTSTRAP:bb <- bootMer(mem1_reml,              FUN=function(x)predict(x, preddata, re.form=NA, type="response"),              nsim=20)# predizioni:preddata$fit <-bb$t0#se: preddata$se.fit <-apply(bb$t, 2, sd)# limiti dell'intervallo di confidenza:preddata$hici<-preddata$fit + 1.96*preddata$se.fitpreddata$loci<-preddata$fit - 1.96*preddata$se.fitplot(Richness ~ NAP, data = RIKZ,      col=as.numeric(Exposure)+1,     ylab="Richness")lines(fit ~ NAP, data = subset(preddata, Exposure=="8"), type="l", col="red", lwd=2)points(hici ~ NAP, data = subset(preddata, Exposure=="8"), type="l", col="red", lwd=2, lty="dotted")points(loci ~ NAP, data = subset(preddata, Exposure=="8"), type="l", col="red", lwd=2, lty="dotted")lines(fit ~ NAP, data = subset(preddata, Exposure=="10"), type="l", col="green", lwd=2)points(hici ~ NAP, data = subset(preddata, Exposure=="10"), type="l", col="green", lwd=2, lty="dotted")points(loci ~ NAP, data = subset(preddata, Exposure=="10"), type="l", col="green", lwd=2, lty="dotted")lines(fit ~ NAP, data = subset(preddata, Exposure=="11"), type="l", col="blue", lwd=2)points(hici ~ NAP, data = subset(preddata, Exposure=="11"), type="l", col="blue", lwd=2, lty="dotted")points(loci ~ NAP, data = subset(preddata, Exposure=="11"), type="l", col="blue", lwd=2, lty="dotted")

# rappresentazione alternativa:

plot(Richness ~ NAP, data = RIKZ,      col=as.numeric(Exposure)+1,     ylab="Richness")
     
     lines(fit ~ NAP, 
	data = subset(preddata, Exposure=="8"), 
	type="l", col="red", lwd=2
	)
lines(fit ~ NAP, 
	data = subset(preddata, Exposure=="10"), 
	type="l", col="green", lwd=2
	)
lines(fit ~ NAP, 
	data = subset(preddata, Exposure=="11"), 
	type="l", col="blue", lwd=2
	)





polygon(c(unique(preddata$NAP),          rev(unique(preddata$NAP))),        c(subset(preddata, Exposure=="8")$hici,          rev(subset(preddata, Exposure=="8")$loci)),        col=grey(level=0.5, alpha=0.5), border=NA        )lines(fit ~ NAP, data = subset(preddata, Exposure=="10"), type="l", col="green", lwd=2)polygon(c(unique(preddata$NAP),          rev(unique(preddata$NAP))),        c(subset(preddata, Exposure=="10")$hici,          rev(subset(preddata, Exposure=="10")$loci)),        col=grey(level=0.5, alpha=0.5), border=NA        )lines(fit ~ NAP, data = subset(preddata, Exposure=="11"), type="l", col="blue", lwd=2)polygon(c(unique(preddata$NAP),          rev(unique(preddata$NAP))),        c(subset(preddata, Exposure=="11")$hici,          rev(subset(preddata, Exposure=="11")$loci)),        col=grey(level=0.5, alpha=0.5), border=NA        )