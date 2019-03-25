

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
length(Preco_Anunciado)
# Para testar se Preco_de_Venda e Preco_Anunciado têm a mesma distribuição faz-se o 
# teste de Smirnov para duas amostras, o teste é feito para captar diferenças 
# nos parâmetros de locacao e no formato da distribuição. Como o tamanho da amostra
# é 50 nem é grande nem pequeno, então será feito dois testes esse de Smirnov
# e o teste Qui-quadrado que também é usado para avaliar se as amostras foram 
# sao de populações coma mesma distribuição. ###
## analisar a questão das frequencias esperadas para o uso do teste qui-quadrado.

chisq.test(Preco_de_Venda, Preco_Anunciado)
# no teste qui-quadrado obtivemos o p-valor de p-value = 0.2401, não rejeitando a 
# hipotese nula, ou seja, nao rejeitando a hipotese que as populações têm a mesma 
# distribuição
ks.test(Preco_de_Venda, Preco_Anunciado)
# o p-valor desse teste foi 0.7112, ao nivel de significancia de 5% nao rejeitamos a 
#hipotese nula, ou seja, nao rejeitamos a hipotese que os dados seguem a mesma 
# distribuição.

library(nortest)
shapiro.test(Preco_Anunciado)
lillie.test(Preco_Anunciado)
qqnorm(Preco_Anunciado); qqline(Preco_Anunciado, col = 2)

# nenhum dos dois bancos seguem normalidade 

# O objetivo deste teste ´e comprovar se duas amostras foram extra´ıdas
# de uma mesma popula¸c˜ao (ou de popula¸c˜oes com a mesma distribui¸c˜ao). Trata-se da vers˜ao para duas amostras do teste de Kolmogorov para bondade do ajuste.



