rm(list = ls())

#***************# Bibliotecas #***************#
library(BSDA)
library(moments)
library(robustbase)
library(mvoutlier)
library(nortest)
library(psych)


#***************# Chamando o Banco #***************#

library(readxl)
Q1 <- read_excel("C:/Users/kelfa/Desktop/Q1.xls", 
                 col_types = c("numeric", "numeric", "text", 
                               "text", "numeric", "numeric", "numeric", 
                               "numeric"))
View(Q1) # Para vizualizar o bando em outra aba

banco_q1 <- Q1
names(banco_q1)
attach(banco_q1)

#***************# Analise descritiva dos dados #***************#

dia1_8h[which(is.na(dia1_8h))] <- median(dia1_8h,na.rm = TRUE)
dia1_12h[which(is.na(dia1_12h))] <- median(dia1_12h,na.rm = TRUE)
dia2_8h[which(is.na(dia2_8h))] <- median(dia2_8h,na.rm = TRUE)
# por se tratar de valores NA's em um banco de apenas 28 observações,
# optei por imputar os dados nos valores NA's, por criterio usamos a mediana para substituir os NA's
# pois focamos em um valor central dos dados

prop.table(table(sexo)) # temos que apenas 17,86% da amostra são do sexo feminino, logo, em sua maioria o banco e formado pelo sexo masculino
prop.table(table(fumante)) # temos que aproximadamente 60,71% da amostra são fumantes

med.media <- matrix(c(summary(idade)[c(3,4)], summary(dia1_8h)[c(3,4)], summary(dia1_12h)[c(3,4)], 
               summary(dia2_8h)[c(3,4)], summary(dia2_12h)[c(3,4)]), ncol = 2, byrow = TRUE)
desvi <- c(sd(idade), sd(dia1_8h), sd(dia1_12h), sd(dia2_8h), sd(dia2_12h))

tab <- data.frame(med.media[,1],med.media[,2],desvi, row.names = c("idade", "Dia1 8h", "Dia1 12h","Dia2 8h", "Dia2 12h"))
names(tab) <- c("Mediana","Média", "Desvio Padrão")
tab

pairs.panels(banco_q1[-c(1,3,4)])

par(mfrow=c(3,2))
hist(idade, main = " "); hist(dia1_8h, main = " "); hist(dia1_12h, main = " "); hist(dia2_8h, main = " "); hist(dia2_12h, main = " ") 

#***************# Testendo normalidade #***************#

library(nortest)
lillie_q1 <- apply(banco_q1[-c(1,3,4)],2,lillie.test)
shapiro_q1 <- apply(banco_q1[-c(1,3,4)],2,shapiro.test)

tab_lilli <- c(lillie_q1$idade$p.value, lillie_q1$dia1_8h$p.value, lillie_q1$dia1_12h$p.value,
               lillie_q1$dia2_8h$p.value, lillie_q1$dia2_12h$p.value)
tab_shap <- c(shapiro_q1$idade$p.value, shapiro_q1$dia1_8h$p.value, shapiro_q1$dia1_12h$p.value,
              shapiro_q1$dia2_8h$p.value, shapiro_q1$dia2_12h$p.value)
tab1 <- data.frame(tab_lilli,tab_shap, row.names = c("idade", "Dia1 8h", "Dia1 12h","Dia2 8h", "Dia2 12h"))
names(tab1) <- c("Teste Lilliefors","Teste Shapiro-Wilk")
tab1

#***************# teste qui quradrado homogeneidade #***************#
table(sexo,fumante)
fisher.test(sexo, fumante)
chisq.test(sexo, fumante)$expected

table(idade,fumante)
fisher.test(idade, fumante)
chisq.test(idade, fumante)$expected

#***************# testando diferenca entre as temperaturas no tempo #***************#

tabp <- c(
  wilcox.test(dia1_8h, dia1_12h, paired = TRUE)$p.value,
  wilcox.test(dia1_8h, dia2_8h, paired = TRUE)$p.value,
  wilcox.test(dia1_8h, dia2_12h, paired = TRUE)$p.value,
  wilcox.test(dia1_12h, dia2_8h, paired = TRUE)$p.value,
  wilcox.test(dia1_12h, dia2_12h, paired = TRUE)$p.value,
  wilcox.test(dia2_8h, dia2_12h, paired = TRUE)$p.value
)

#***************# Chamando o Banco #***************#

library(readxl)
latas <- read_excel("latas.xls")
View(latas)

names(latas)
Q2 <- latas
attach(Q2)

#***************# verificando as variaveis #***************#

pairs.panels(Q2)

med.media1 <- matrix(c(summary(LATAS109)[c(3,4)], summary(LATAS111)[c(3,4)]), ncol = 2, byrow = TRUE)
desvi1 <- c(sd(LATAS109), sd(LATAS111))

tabq2 <- data.frame(med.media1[,1],med.media1[,2],desvi1, row.names = c("LATAS109", "LATAS111"))
names(tabq2) <- c("Mediana","Média", "Desvio Padrão")
tabq2


library(nortest)
lillie.test(LATAS109)
shapiro.test(LATAS109)
qqnorm(LATAS109);qqline(LATAS109, col = 2)

lillie.test(LATAS111)
shapiro.test(LATAS111)
qqnorm(LATAS111);qqline(LATAS111, col = 2)

#normalidade rejeitada em ambos, assim teremos que optar por testes não parametricos

wilcox.test(LATAS109,LATAS111, paried = TRUE)
