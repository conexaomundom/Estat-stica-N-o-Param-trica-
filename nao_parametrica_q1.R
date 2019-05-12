# Verificar via simulação, o nível descritivo do teste de WIlcoxon.
# Univariado quando a amostra for proveniente de uma N(0,1)

# H0: mu = 0
# H1: mu != 0

# Tamanho da amostra.
n <- 30
# Quantas repetições a simulação vai ter.
k <- 1000
B <- 250

###############################################################################
# # Apenas com Monte Carlo.
###############################################################################
vetor1 <- NULL
vetor2 <- NULL
vetor3 <- NULL

for(i in 1:k){
  x <- rnorm(n, mean = 0, sd = 1)
    est <- wilcox.test(x, alternative = "two.sided")
    
    vetorp[i] <- est$p.value
    
    vetor1[i] <- ifelse(est$p.value < 0.1,1,0)
    vetor2[i] <- ifelse(est$p.value < 0.05 ,1,0)
    vetor3[i] <- ifelse(est$p.value < 0.01,1,0)
}
meanp <- mean(vetorp)
soma1 <- sum(vetor1)
soma2 <- sum(vetor2)
soma3 <- sum(vetor3)
soma4 <- sum(vetor1,vetor2,vetor3)


###############################################################################
# Monte Carlo com bootstrap.
################################################################################
soma1 <- NULL
soma2 <- NULL
soma3 <- NULL
soma4 <-NULL

meanp <- NULL


for(i in 1:k){
  vetor1 <- NULL
  vetor2 <- NULL
  vetor3 <- NULL
  vetorp <- NULL
  x <- rnorm(n, mean = 0, sd = 1)
  for(j in 1:B){
   amostra <-  sample(x,length(x),replace = TRUE)
  est <- wilcox.test(amostra, alternative = "two.sided")
  
  vetorp[j] <- est$p.value
  
  vetor1[j] <- ifelse(est$p.value < 0.1,1,0)
  vetor2[j] <- ifelse(est$p.value < 0.05,1,0)
  vetor3[j] <- ifelse(est$p.value < 0.01,1,0)
  }
  
  meanp[i] <- mean(vetorp)
  soma1[i] <- sum(vetor1)
  soma2[i] <- sum(vetor2)
  soma3[i] <- sum(vetor3)
  soma4[i] <- sum(vetor1,vetor2,vetor3)
}

mean(meanp)
mean(soma1)
mean(soma2)
mean(soma3)
mean(soma4)

###############################################################################
# Monte Carlo com bootstrap, para estimar o erro padrão.
###############################################################################
