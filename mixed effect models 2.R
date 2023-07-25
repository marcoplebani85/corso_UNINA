rm(list=ls()) # ripulisce la memoria di R

ggplot(dd, aes(x = Depth, y = Alga_length, colour = Locality)) +

mem1 <- lmerTest::lmer(Alga_length ~ Depth + (1 + Depth | Locality), 

# grafici diagnostici usando le funzioni di DHARMa:

performance::check_model(mem1, check=c("qq", "linearity", "homogeneity", "outliers", "reqq"))

check_heteroscedasticity(mem1)

mem_fixef_ri <- lmerTest::lmer(Alga_length ~ Depth + (1 | Locality), 

mem1_reml <- lmerTest::lmer(Alga_length ~ Depth + (1 + Depth | Locality), 

summary(mem1_reml)

performance::r2(mem1_reml)

# predittori per i quali ottenere predizioni:
abline(a=102, b=3, lwd=3)    
     