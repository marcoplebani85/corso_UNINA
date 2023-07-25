rm(list=ls()) # ripulisce la memoria di R# carica il pacchetto "pacman", che contiene la funzione p_load():if (!require(pacman)) install.packages('pacman'); library(pacman)# altri pacchetti:p_load(doBy)p_load(emmeans)p_load(ggplot2)p_load(DHARMa, performance) # DHARMa e performance producono test e grafici diagnostici per GLM e mixed-effect modelsp_load(lme4, lmerTest, glmmTMB) # per fittare modelli mistip_load(MuMIn) # per stimare l'R-quadro dei modelli misti# definiamo la directory di lavoro:setwd("~/Desktop/Corso_R")# Simuliamo i dati in R:set.seed(667)Locality <- LETTERS[1:8]rep <- paste("rep", 1:10, sep="_")Depth <- 1:15df <- expand.grid(Locality = Locality, Depth = Depth, rep = rep)# aggiungiamo l'effetto dei random effects (random intercept):re <- data.frame(Locality = unique(df$Locality))re$ri <- rnorm(n=length(re$Locality), mean = 100, sd = 20)re$rs <- rnorm(n=length(re$Locality), mean = 3, sd = 1)dd <- merge(df, re)dd$length_mean <- dd$ri + dd$rs*dd$Depth# aggiungiamo della variabilità residua a livello delle singole osservazioni:dd$Alga_length = dd$length_mean + rnorm(n=length(dd[,1]), mean = 0, sd = 5)

ggplot(dd, aes(x = Depth, y = Alga_length, colour = Locality)) +    geom_point() + geom_smooth(method="lm") +    # etichette assi:    labs(x = "Water depth [m]", y = "Algal length [mm]") +    # dimensione e tipo di carattere:    theme(text = element_text(family="Times", size=15)) +    theme_classic()

mem1 <- lmerTest::lmer(Alga_length ~ Depth + (1 + Depth | Locality),                 data = dd, REML=F)

# grafici diagnostici usando le funzioni di DHARMa:mem1Resids <- simulateResiduals(mem1, n=3000, use.u=F)par(mfrow = c(1, 2))plotQQunif(mem1Resids)mtext(text = "(a)", side = 3, adj = 0, line = 2)plotResiduals(mem1Resids, quantreg = T)mtext(text = "(b)", side = 3, adj = 0, line = 2)

performance::check_model(mem1, check=c("qq", "linearity", "homogeneity", "outliers", "reqq"))

check_heteroscedasticity(mem1)check_homogeneity(mem1)check_outliers(mem1)

mem_fixef_ri <- lmerTest::lmer(Alga_length ~ Depth + (1 | Locality),                 data = dd, REML=F)mem_rs <- lmerTest::lmer(Alga_length ~ 1 + (1 + Depth | Locality),                 data = dd, REML=F)mem_ri <- lmerTest::lmer(Alga_length ~ 1 + (1 | Locality),                 data = dd, REML=F)lm1 <- lm(Alga_length ~ Depth, data = dd)lm0 <- lm(Alga_length ~ 1, data = dd)AIC(mem1, mem_fixef_ri, mem_rs, mem_ri, lm1, lm0)

mem1_reml <- lmerTest::lmer(Alga_length ~ Depth + (1 + Depth | Locality),                 data = dd, REML=T)

summary(mem1_reml)

performance::r2(mem1_reml)

# predittori per i quali ottenere predizioni:preddata <- data.frame(Depth = seq(from=1, to=15, length.out=100))preddata$Locality <- rep(NA, length(preddata[,1]))# BOOTSTRAP:bb <- bootMer(mem1_reml,              FUN=function(x)predict(x, preddata, re.form=NA, type="response"),              nsim=20)# predizioni:preddata$fit <- bb$t0# se:preddata$se.fit <-apply(bb$t, 2, sd)# limiti dell'intervallo di confidenza:hici<-preddata$fit + 1.96*preddata$se.fitloci<-preddata$fit - 1.96*preddata$se.fitplot(Alga_length ~ Depth, data = dd,      col=as.numeric(dd$Locality),     ylab="Algal length [mm]")
abline(a=102, b=3, lwd=3)    
     lines(fit ~ Depth, data = preddata, type="l", col="red", lwd=2)points(hici ~ Depth, data = preddata, type="l", col="red", lwd=2, lty="dotted")points(loci ~ Depth, data = preddata, type="l", col="red", lwd=2, lty="dotted")