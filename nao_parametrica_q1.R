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
# Monte Carlo com bootstrap, para estimar o erro padrão e .
###############################################################################

intervalo_confianca1 <- matrix(0,k,2)
mu <- NULL
mediamu <- NULL
se_B1 <- NULL
for(i in 1:k){
  x <- rnorm(n, mean = 0, sd = 1)
  # O bootstrap começa aqui.
  for(j in 1:B){
    amostra <-  sample(x,length(x),replace = TRUE)
    mu[j] <- mean(amostra)
  }
  
 mediamu[i] <- mean(mu)
 se_B1[i] <- (sum( ((mu - mediamu[i])^2)/(B-1) ))^(1/2)
 
 intervalo_confianca1[i, ] <- c(mediamu[i] - (qnorm(1-(0.05/2)) * se_B1[i]),
                                mediamu[i] + (qnorm(1-(0.05/2)) * se_B1[i]))
 
}

apply(intervalo_confianca1, 2, "mean")

a1 <- sort(intervalo_confianca1[ ,1])
b1 <- sort(intervalo_confianca1[ ,2])

ic <- c(quantile(a1,(0.05/2)), quantile(b1,1-(0.05/2)))
ic
# Esse foi para a média 

###############################################################################
# Monte Carlo com bootstrap, para alpha.
###############################################################################
which0 <- NULL
m1 <- NULL

intervalo_confianca2 <- matrix(0, k, 2)

for(i in 1:k){
  vetorp <- NULL
  
  x <- rnorm(n, mean = 0, sd = 1)
  # O bootstrap começa aqui.
  for(j in 1:B){
    amostra <-  sample(x,length(x),replace = TRUE)
    est <- wilcox.test(amostra, alternative = "two.sided")
    vetorp[j] <- ifelse(est$p.value == est$p.value, est$p.value,0)
    
  }
  
  m1[i] <- mean(vetorp)
  
  se_B2[i] <- (sum( ((vetorp - m1[i])^2)/(B-1) ))^(1/2)
  
  intervalo_confianca2[i, ] <- c(m1[i] - (qnorm(1-(0.05/2)) * se_B2[i]),
                                 m1[i] + (qnorm(1-(0.05/2)) * se_B2[i]))
  
}

apply(intervalo_confianca2, 2, "mean")

aa1 <- sort(intervalo_confianca2[ ,1])
bb1 <- sort(intervalo_confianca2[ ,2])

ic1 <- c(quantile(aa1,(0.05/2)), quantile(bb1,1-(0.05/2)))
ic1
