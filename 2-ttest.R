# Confronto tra gruppi
# Marco Plebani, 24 aprile

# c'è differenza significativa tra le specie 1 e 2?
# i nostri campioni:
gruppo1 <- c(24, 25, 26, 24.5, 23, 22, 28, 30, 24.2, 25.1)
gruppo2 <- c(30, 31, 29, 25, 26, 31.2, 32.1, 33, 32.1, 32.1)

# golden rule: always plot first!

par(mfrow=c(1, 1))

hist(gruppo1, 
     col=grey(level=0.2, alpha=0.5), 
     xlim=c(20, 35), xlab="body mass",
     main="")

hist(gruppo2, col=grey(level=0.8, alpha = 0.5), add=T)

# analisi quantitativa:
# test a due gruppi
t.test(gruppo1, gruppo2, var.equal=TRUE)

# test a un gruppo:
t.test(gruppo1, mu=20, alternative="greater")


#######
# dati appaiati
#######


prima <- gruppo1
dopo <- gruppo2

t.test(prima, dopo)
t.test(prima, dopo, paired=TRUE)



