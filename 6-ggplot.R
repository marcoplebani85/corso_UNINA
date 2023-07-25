# sperimentando con ggplot

library(ggplot2)
setwd("/Users/marcoplebani/Desktop/Corso_R")
df1 <- read.delim("my_data/compare_6_species.txt")

str(df1)
df1$Species <- as.factor(df1$Species)

# creiamo il base layer:
bb0 <- ggplot(data=df1, aes(x=Species, y=body.mass)) 

bb0 + geom_boxplot() + theme_classic()

ggplot(data=df1, aes(y=body.mass)) + geom_bar()
  
bb0 + geom_violin(aes(fill=Species)) + theme_classic()

ggplot(data=df1, aes(y=body.mass, fill=Species)) + geom_density(alpha=0.5)