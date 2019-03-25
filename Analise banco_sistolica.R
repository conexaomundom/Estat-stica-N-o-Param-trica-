
install.packages("MVA")
# install.packages("MVN")
install.packages("moments")
install.packages("robustbase")
install.packages("mvoutlier")
install.packages("BSDA")
library(BSDA)


library(moments)
library(robustbase)
library(mvoutlier)

####### Banco de dados Casas
# ficheiro = file.choose()
# banco = read.table(file = ficheiro, header=TRUE, sep = ";", dec = ".")
SISTOLICA

attach(SISTOLICA)
names(SISTOLICA)
####################################################################
# O que você pode dizer acerca da distribuição dos dados? E o que isso implica?
detach(SISTOLICA)
library(psych)
pairs(SISTOLICA[, 1:4])
pairs.panels(SISTOLICA[ , 1:4])
pairs.panels(SISTOLICA[ , 5:8])

###
medias_SISTOLICA <- apply(SISTOLICA, 2, "mean")
medias_SISTOLICA

# normalidade_shapiro_ <- apply(SISTOLICA, 2, shapiro.test)
# library(nortest)
# normalidade_lillie_ <- apply(SISTOLICA, 2, lillie.test)
# os grficos vao ser aqueles mesmo

# Nada posso afirmar sobre, até pq nenhum teste pode ser aplicado.

# Existe associa¸c˜ao entre gˆenero e ra¸ca?
