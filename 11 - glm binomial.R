# binomial GLM

rm(list=ls()) # ripulisce la memoria di R
# carica il pacchetto "pacman", che contiene la funzione p_load():
if (!require(pacman)) install.packages(pacman); library(pacman) # altri pacchetti:
p_load(emmeans)
p_load(ggplot2)
p_load(DHARMa)

setwd("/Users/marcoplebani/Desktop/Corso_R")
df <- read.csv("my_data/stress_death.csv")
str(df)


plot(jitter(death.01, 0.1) ~ jitter(stress.per100),
	# jitter() introduce un pizzico di rumore artificiale nei dati per far sì che non siano tutti esattamente sovrapposti
	data= df,
	pch=21, cex=2,
	# grey() è una funzione che permette di scegliere la scurezza e la trasparenza del colore grigio da usare:
	bg=grey(0.7, alpha=0.5),
	xlab="Stress [%]",
	ylab="probablity of death",
	xlim=c(0,100)
	)

glm.binom1 <- glm(death.01 ~ stress.per100,
	data= df,
	family=binomial(link="logit")
	)

# GRAFICI DIAGNOSTICI

glm1Resids <- simulateResiduals(glm.binom1)
par(mfrow = c(1, 2))
# confronto tra residui osservati e quelli attesi:
plotQQunif(glm1Resids)
mtext(text = "(a)", side = 3, adj = 0, line = 2)
# andamento dei residui simulati rispetto ai valori medi stimati dal modello:
plotResiduals(glm1Resids, quantreg = T)
mtext(text = "(b)", side = 3, adj = 0, line = 2)


# MODELLO NULLO

glm.binom0 <- glm(death.01 ~ 1,
                  data= df,
                  family=binomial(link="logit")
                  )

anova(glm.binom1, glm.binom0, test="Chisq")

# draw predictions:

library(emmeans)
pred.df <- as.data.frame(emmeans(glm.binom1,
        specs=~stress.per100,
        type="response",
        at=list(stress.per100=c(0:100))))


par(mfrow=c(1,1))
# plot(jitter(death.01, 0.1) ~ jitter(stress.per100,1),
# jitter() introduce un pizzico di rumore artificiale nei dati per far sì che non siano tutti esattamente sovrapposti

plot(death.01 ~ stress.per100,
	data= df,
	pch=21, cex=2,
	# grey() è una funzione che permette di scegliere la scurezza e la trasparenza del colore grigio da usare:
	bg=grey(0.2, alpha=0.3),
	xlab="Stress [%]",
	ylab="probablity of death",
	xlim=c(0,100)
	)

points(x = pred.df$stress.per100,
	y = pred.df$prob, 
	type="l", # traccia una linea
	lwd=2 # spessore
	)
points(x = pred.df$stress.per100,
	y = pred.df$asymp.LCL, 
	type="l", # linea
	lty="dashed"
	)
points(x = pred.df$stress.per100,
       y = pred.df$asymp.UCL,
	type="l", # linea
	lty="dashed"
	)

summary(glm.binom1)
