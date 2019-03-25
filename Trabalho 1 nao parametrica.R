

install.packages("MVA")
# install.packages("MVN")
install.packages("moments")
install.packages("robustbase")
install.packages("mvoutlier")

library(moments)
library(robustbase)
library(mvoutlier)

####### Banco de dados Casas
ficheiro = file.choose()
banco = read.table(file = ficheiro, header=TRUE, sep = ";", dec = ".")
banco

attach(banco)
names(banco)
########################################################################
# A distribuição do preço de venda é a mesma do preço anunciado.
# essas duas veriaveis estão em milhares de dolares
Preco_de_Venda
hist(Preco_de_Venda)
summary(Preco_de_Venda)
# A media nao esta muito longe da mediana
var(Preco_de_Venda)
# variancia alta  5296.443 ne

library(nortest)
shapiro.test(Preco_de_Venda)
lillie.test(Preco_de_Venda)

library(MVA)

qqnorm(Preco_de_Venda); qqline(Preco_de_Venda, col = 2)


# Preco anunciado 
Preco_Anunciado
hist(Preco_Anunciado)
summary(Preco_Anunciado)
# A media nao esta muito longe da mediana
var(Preco_Anunciado)
# variancia alta  6301.617 ne

library(nortest)
shapiro.test(Preco_Anunciado)
lillie.test(Preco_Anunciado)
qqnorm(Preco_Anunciado); qqline(Preco_Anunciado, col = 2)

# nenhum dos dois bancos seguem normalidade 

# O objetivo deste teste ´e comprovar se duas amostras foram extra´ıdas
# de uma mesma popula¸c˜ao (ou de popula¸c˜oes com a mesma distribui¸c˜ao). Trata-se da vers˜ao para duas amostras do teste de Kolmogorov para bondade do ajuste.
