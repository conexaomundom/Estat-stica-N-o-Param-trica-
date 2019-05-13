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
# Monte Carlo com bootstrap.
################################################################################
alpha1 <- 0.1
alpha2 <- 0.05
alpha3 <- 0.01

qm1 <- NULL
qm2 <- NULL
qm3 <- NULL

which0 <- NULL
which01 <- NULL
which02 <- NULL
which03 <- NULL

m1 <- NULL
m2 <- NULL
m3 <- NULL
m4 <- NULL

for(i in 1:k){
  vetor1 <- NULL
  vetor2 <- NULL
  vetor3 <- NULL
  vetorp <- NULL
  vetorp <- NULL
  
  q1 <- NULL
  q2 <- NULL
  q3 <- NULL
  
  x <- rnorm(n, mean = 0, sd = 1)
  # O bootstrap começa aqui.
  for(j in 1:B){
    amostra <-  sample(x,length(x),replace = TRUE)
    est <- wilcox.test(amostra, alternative = "two.sided")
    
    vetorp[j] <- ifelse(est$p.value == est$p.value, est$p.value,0)
    
    vetor1[j] <- ifelse(est$p.value < alpha1,est$p.value,0)
    vetor2[j] <- ifelse(est$p.value < alpha2,est$p.value,0)
    vetor3[j] <- ifelse(est$p.value < alpha3,est$p.value,0)
    
    # A quantidade de vezes que acertou sendo a 0.1, 0.05 e 0.01
    q1[j] <- ifelse(est$p.value < alpha1,1,0)
    q2[j] <- ifelse(est$p.value < alpha2,1,0)
    q3[j] <- ifelse(est$p.value < alpha3,1,0)
  }
  
  qm1[i] <- mean(q1)
  qm2[i] <- mean(q2)
  qm3[i] <- mean(q3)
  
  which0[i] <- ifelse(length(which(vetorp == 0)) == 0,0,which(vetorp == 0))
  which01[i] <- ifelse(length(which(vetor1 == 0)) == 0,0,which(vetor1 == 0))
  which02[i] <- ifelse(length(which(vetor2 == 0)) == 0,0,which(vetor2 == 0))
  which03[i] <- ifelse(length(which(vetor3 == 0)) == 0,0,which(vetor3 == 0))
  
  m1[i] <- mean(vetorp[-which0])
  m2[i] <- mean(vetor1[-which01])
  m3[i] <- mean(vetor2[-which02])
  m4[i] <- mean(vetor3[-which03])
  
}

mean(qm1)
mean(qm2)
mean(qm3)
###############################################################################
# Monte Carlo com bootstrap, para estimar o erro padrão.
###############################################################################
alpha1 <- 0.1
alpha2 <- 0.05
alpha3 <- 0.01

which0 <- NULL
which01 <- NULL
which02 <- NULL
which03 <- NULL

m1 <- NULL
m2 <- NULL
m3 <- NULL
m4 <- NULL

for(i in 1:k){
  vetor1 <- NULL
  vetor2 <- NULL
  vetor3 <- NULL
  vetorp <- NULL
  vetorp <- NULL
  
  q1 <- NULL
  q2 <- NULL
  q3 <- NULL
  
  x <- rnorm(n, mean = 0, sd = 1)
  # O bootstrap começa aqui.
  for(j in 1:B){
    amostra <-  sample(x,length(x),replace = TRUE)
    mu <- mean(amostra)


  }
  

  
}



# se_B1[i] <- (sum( ((estp - meanestp)^2)/(B-1) ))^(1/2)
# se_B2[i] <- (sum( ((vetorp - mean(vetorp))^2)/(B-1) ))^(1/2)
# 
# se_B1boot <- mean(se_B1)
# se_B2boot <- mean(se_B2)

###############################################################################
# Monte Carlo com bootstrap, para intervalo de confiança.
###############################################################################
intervalo_confianca1 <- c(alpha_estimado-(qnorm(1-alpha_estimado/2) * se_B1boot),
                          alpha_estimado+(qnorm(1-alpha_estimado/2) * se_B1boot))


intervalo_confianca2 <- c(alpha_estimado-(qnorm(1-alpha_estimado/2) * se_B2boot),
                          alpha_estimado+(qnorm(1-alpha_estimado/2) * se_B2boot))
