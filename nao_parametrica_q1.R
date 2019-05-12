# Verificar via simulação, o nível descritivo do teste de WIlcoxon.
# Univariado quando a amostra for proveniente de uma N(0,1)

# H0: mu = 0
# H1: mu != 0

# Tamanho da amostra.
n <- 30
# Quantas repetições a simulação vai ter.
k <- 10000
B <- 250
vetor1 <- NULL
vetor2 <- NULL
vetor3 <- NULL

for(i in 1:k){
  vetor1 <- NULL
  vetor2 <- NULL
  vetor3 <- NULL
  x <- rnorm(n, mean = 0, sd = 1)
  for(i in 1:B){
   amostra <-  sample(x,length(x),replace = TRUE)
  est <- wilcox.test(amostra, alternative = "two.sided")
  vetor1[i] <- ifelse(est$p.value < 0.1,1,0)
  vetor2[i] <- ifelse(est$p.value < 0.05 ,1,0)
  vetor3[i] <- ifelse(est$p.value < 0.01,1,0)
  }
  
  sum(vetor1)
  sum(vetor2)
  sum(vetor3)
  sum(vetor1,vetor2,vetor3)
}
sum(vetor1)
sum(vetor2)
sum(vetor3)
sum(vetor1,vetor2,vetor3)
