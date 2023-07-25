rm(list=ls()) # ripulisce la memoria di R

data(RIKZ)



mem1 <- glmmTMB(Richness ~ NAP * Exposure + (1 | Beach),

# grafici diagnostici usando le funzioni di DHARMa:


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
                
mem1_reml <- glmmTMB(Richness ~ NAP + Exposure + (1 | Beach),

pendenze <- emtrends(mem1_reml,

emmeans::test(pendenze)

emmeans(mem1_reml, 
			type="response"

# predittori per i quali ottenere predizioni:

# rappresentazione alternativa:

plot(Richness ~ NAP, data = RIKZ, 
     
     
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





