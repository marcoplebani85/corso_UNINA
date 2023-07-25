rm(list=ls()) # ripulisce la memoria di R

ggplot(dd, aes(x = Depth, y = Alga_length, colour = Locality)) +
    
mem1 <- lmerTest::lmer(Alga_length ~ Depth + (1 | Locality) + (1 + Depth | Season), 
                
mem1 <- glmmTMB(Alga_length ~ Depth + (1 | Locality) + (1 + Depth | Season) + corr(), 
                data = dd, REML=F)
                
# grafici diagnostici usando le funzioni di DHARMa:

performance::check_model(mem1, check=c("qq", "linearity", "homogeneity", "outliers", "reqq"))

mem_ranef <- lmerTest::lmer(Alga_length ~ 1 + (1 | Locality), 

mem1_reml <- lmerTest::lmer(Alga_length ~ Depth + (1 | Locality), 

summary(mem1_reml)

performance::r2(mem1_reml)

# predittori per i quali ottenere predizioni: