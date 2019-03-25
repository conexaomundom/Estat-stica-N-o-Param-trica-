
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

######################################################
# Existe associaçaõ entre genero e raça?

# H0: Nao xiste associaçaõ entre genero e raça
# H1: Existe associaçaõ entre genero e raça
attach(SISTOLICA)

sexo = ifelse(SISTOLICA$Sexo == "Masculino", 1, 0)
raca = ifelse(SISTOLICA$Raça == "Branca", 1, 0)
matriz <- matrix(c(0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1), 24, 2, byrow = FALSE)
chisq.test(matriz)

# NA REALIDADDE CREIO EU QUE AQUI SE APLIQUE O TESTE EXATO DE FISHER

fisher.test(matriz)

# Rejeito H0, ou sjea, existe associação entre genero e raça????????.