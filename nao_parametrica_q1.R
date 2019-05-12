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

alpha1 <- 0.1
alpha2 <- 0.05
alpha3 <- 0.01

for(i in 1:k){
  x <- rnorm(n, mean = 0, sd = 1)
    est <- wilcox.test(x, alternative = "two.sided")
    
    vetorp[i] <- est$p.value
    
    vetor1[i] <- ifelse(est$p.value < alpha1,1,0)
    vetor2[i] <- ifelse(est$p.value < alpha2 ,1,0)
    vetor3[i] <- ifelse(est$p.value < alpha3,1,0)
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

alpha1 <- 0.1
alpha2 <- 0.05
alpha3 <- 0.01

for(i in 1:k){
#   vetor1 <- NULL
#   vetor2 <- NULL
#   vetor3 <- NULL
   vetorp <- NULL
#   meanestp <- NULL
  
   x <- rnorm(n, mean = 0, sd = 1)
  for(j in 1:250){
  amostra <-  sample(x,length(x),replace = TRUE)

  est <- wilcox.test(amostra, alternative = "two.sided")
  
  vetorp[j] <- ifelse(est$p.value == est$p.value, est$p.value,0)
  }
  # vetor1[j] <- ifelse(est$p.value < alpha1,1,0)
  # vetor2[j] <- ifelse(est$p.value < alpha2,1,0)
  # vetor3[j] <- ifelse(est$p.value < alpha3,1,0)
   meanestp[i] <- mean(vetorp)
 }
  
  # estp <- c(vetorp * vetor1, vetorp * vetor2, vetorp * vetor3)
  # 

  # meanp[i] <- mean(vetorp)
  # soma1[i] <- sum(vetor1)
  # soma2[i] <- sum(vetor2)
  # soma3[i] <- sum(vetor3)
  # soma4[i] <- sum(vetor1,vetor2,vetor3)


alpha_estimado <- mean(meanestp)
mean(soma1)
mean(soma2)
mean(soma3)
mean(soma4)

###############################################################################
# Monte Carlo com bootstrap, para estimar o erro padrão.
###############################################################################
meanp <- NULL
se_B1 <- NULL
se_B2 <- NULL
alpha1 <- 0.1
alpha2 <- 0.05
alpha3 <- 0.01

for(i in 1:k){
  vetor1 <- NULL
  vetor2 <- NULL
  vetor3 <- NULL
  vetorp <- NULL
  meanestp <- NULL
  
  x <- rnorm(n, mean = 0, sd = 1)
  for(j in 1:B){
    amostra <-  sample(x,length(x),replace = TRUE)
    est <- wilcox.test(amostra, alternative = "two.sided")
    
    vetorp[j] <- est$p.value
    
    vetor1[j] <- ifelse(est$p.value < alpha1,1,0)
    vetor2[j] <- ifelse(est$p.value < alpha2,1,0)
    vetor3[j] <- ifelse(est$p.value < alpha3,1,0)
  }
  
  estp <- c(vetorp * vetor1, vetorp * vetor2, vetorp * vetor3)
  meanestp <- mean(estp)
  se_B1[i] <- (sum( ((estp - meanestp)^2)/(B-1) ))^(1/2)
  se_B2[i] <- (sum( ((vetorp - mean(vetorp))^2)/(B-1) ))^(1/2)
}
se_B1boot <- mean(se_B1)
se_B2boot <- mean(se_B2)

###############################################################################
# Monte Carlo com bootstrap, para intervalo de confiança.
###############################################################################
intervalo_confianca1 <- c(alpha_estimado-(qnorm(1-alpha_estimado/2) * se_B1boot),
                          alpha_estimado+(qnorm(1-alpha_estimado/2) * se_B1boot))


intervalo_confianca2 <- c(alpha_estimado-(qnorm(1-alpha_estimado/2) * se_B2boot),
                          alpha_estimado+(qnorm(1-alpha_estimado/2) * se_B2boot))
