
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

#####################################################################################
## Realizando testes nao parametricos
#####################################################################################
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

##########################
# Existe diferença no parÂmetro de loca¸c˜ao da press˜ao sangu´ınea entre os gˆeneros?

# H0: NÃO Existe diferença no parÂmetro de loca¸c˜ao da press˜ao sangu´ınea entre os gˆeneros?
# H1: Existe diferença no parÂmetro de loca¸c˜ao da press˜ao sangu´ınea entre os gˆeneros
names(SISTOLICA)
SIGN.test(sexo, Pre_Exerc_sem_Estresse,alternative = "two.sided")
wilcox.test(sexo, Pre_Exerc_sem_Estresse, paired = TRUE)

# Em ambos os testes Rejeitamos H0 com p-valores de 1.192e-07 e 1.938e-05 para o teste do sinal e 
# teste de wilcoxon respectivamente. Rejeitando H0, rejeitamos a hipótese que não existe diferença 
# no parÂmetro de loca¸c˜ao da press˜ao sangu´ınea entre os gˆeneros. Desta forma podendo afirmar que 
# Existe diferença no parÂmetro de loca¸c˜ao da press˜ao sangu´ınea entre os gˆeneros.


# Entre quais tipo de press˜ao sangu´ınea existe diferen¸ca em relaçao ao parˆametro de locaçaõ?
names(SISTOLICA)
# Vejo sentido comparar pre com pos e nao pre com pre, e pos com pos, inclusive nao cruzando os
# motivos do estresse até porque nao tera uma interpretacao plausível para tal relação.

# pre e pos de exercicios sem estresse
SIGN.test(Pre_Exerc_sem_Estresse,Pos_Exerc_sem_Estresse, alternative = "two.sided")
wilcox.test(Pre_Exerc_sem_Estresse,Pos_Exerc_sem_Estresse, paired = TRUE)


# Em ambos os testes não Rejeitamos H0 com p-valores de 0.6776 e 0.9031 para o teste do sinal e 
# teste de wilcoxon respectivamente.  Não Rejeitando H0,  não rejeitamos a hipótese que não existe diferença 
# no parÂmetro de  presso sangu´ínea pre e pos sem estresse. Desta forma podendo afirmar que 
# não xiste diferença no parÂmetro de loca¸c˜ao da press˜ao sangu´ınea entre os gˆeneros.


# pre e pos de exercicios com estresse Matematica
SIGN.test(Pre_Exerc_com_Estresse_Matematica,Pos_Exerc_com_Estresse_Matematica, alternative = "two.sided")
wilcox.test(Pre_Exerc_com_Estresse_Matematica,Pos_Exerc_com_Estresse_Matematica, paired = TRUE)
# Ao nível de significancia de 5% em ambos os testes Rejeitamos H0 com p-valores de 0.0002772 e 0.0001815
# para o teste do sinal e teste de wilcoxon respectivamente. Rejeitando H0, rejeitamos a hipótese que não 
# existe diferença no parÂmetro de loca¸c˜ao da press˜ao sangu´ınea pre e pos de exercicios com estresse 
# Matematica. Desta forma podendo afirmar que Existe diferença no parÂmetro de loca¸c˜ao da press˜ao sangu´ınea 
# pre e pos de exercicios sem estresse Matematica.

# pre e pos de exercicios com estresse Verbal
SIGN.test(Pre_Exerc_com_Estresse_Verbal,Pos_Exerc_com_Estresse_Verbal, alternative = "two.sided")
wilcox.test(Pre_Exerc_com_Estresse_Verbal,Pos_Exerc_com_Estresse_Verbal, paired = TRUE)
# Ao nível de significancia de 5% em ambos os testes Rejeitamos H0 com p-valores de 1.192e-07 e 1.933e-05
# para o teste do sinal e teste de wilcoxon respectivamente. Rejeitando H0, rejeitamos a hipótese que não 
# existe diferença no parÂmetro de loca¸c˜ao da press˜ao sangu´ınea pre e pos de exercicios com estresse 
# Verbal. Desta forma podendo afirmar que Existe diferença no parÂmetro de loca¸c˜ao da press˜ao sangu´ınea 
# pre e pos de exercicios sem estresse Verbal.

  
##### Podemos afirmar que a press˜ao sangu´ınea aumenta ap´os o exerc´ıcio?

# Apos realizar os testes para verificar se ha diferenças vamos calcular se aumenta apos o exercicio
# vamos calcular se pos exercicio com estresse matemarica e por exercicio com estresse verbal é maior que 
# pre exercicios com esses tipor de estresse. Não vamos fazer com o pre e pós exercicio sem estresse
# pois o teste anterior apontou que nao há diferenca nessas variaveis.

# H0: A pressao sanguinea é maior pre exercicio com estresse matematica >=0
# H!: A pressao sanguinea é maior pos exercicio com estresse matematica <0

wilcox.test(Pre_Exerc_com_Estresse_Matematica,Pos_Exerc_com_Estresse_Matematica,
            paired = TRUE, alternative = "less")
# AO realizar o teste de wilcoxon para saber se apressao sanguinea é maior pos exercicio
# com estresse matematica o p-valor resultou em 0.9999 rejeitando a hipotese nula, ou seja,
# podemos afirmar com base na amostra que a pressão sanguinea aumenta após o exercicio com
# estresse Matematica.

wilcox.test(Pre_Exerc_com_Estresse_Verbal,Pos_Exerc_com_Estresse_Verbal,
            paired = TRUE, alternative = "less")
# Ao realizar o teste de wilcoxon para saber se apressao sanguinea é maior pos exercicio
# com estresse matematica o p-valor resultou em um valor extremamente proximo de 1 rejeitando 
# a hipotese nula, ou seja, podemos afirmar com base na amostra que a pressão sanguinea aumenta 
# após o exercicio com estresse Verbal.

  
# Quais diferentes press˜oes sangu´ıneas podemos dizer que tˆem a mesma distribui¸c˜ao?
# "Pre_Exerc_sem_Estresse"           
# "Pre_Exerc_com_Estresse_Matematica"
# "Pre_Exerc_com_Estresse_Verbal"    
# "Pos_Exerc_sem_Estresse"           
# "Pos_Exerc_com_Estresse_Matematica"
# "Pos_Exerc_com_Estresse_Verbal"    

apply(SISTOLICA[ ,3:8], 2, chisq.test)

chisq.test(Pre_Exerc_sem_Estresse, Pre_Exerc_com_Estresse_Matematica)$p.value
chisq.test(Pre_Exerc_sem_Estresse, Pre_Exerc_com_Estresse_Verbal)$p.value
chisq.test(Pre_Exerc_sem_Estresse, Pos_Exerc_sem_Estresse)$p.value
chisq.test(Pre_Exerc_sem_Estresse, Pos_Exerc_com_Estresse_Matematica)$p.value
chisq.test(Pre_Exerc_sem_Estresse, Pos_Exerc_com_Estresse_Verbal)$p.value

chisq.test(Pre_Exerc_com_Estresse_Matematica, Pre_Exerc_com_Estresse_Verbal)$p.value
chisq.test(Pre_Exerc_com_Estresse_Matematica, Pos_Exerc_sem_Estresse)$p.value
chisq.test(Pre_Exerc_com_Estresse_Matematica, Pos_Exerc_com_Estresse_Matematica)$p.value
chisq.test(Pre_Exerc_com_Estresse_Matematica, Pos_Exerc_com_Estresse_Verbal)$p.value
chisq.test(Pre_Exerc_com_Estresse_Verbal, Pos_Exerc_sem_Estresse)$p.value

chisq.test(Pre_Exerc_com_Estresse_Verbal, Pos_Exerc_com_Estresse_Matematica)$p.value
chisq.test(Pre_Exerc_com_Estresse_Verbal, Pos_Exerc_com_Estresse_Verbal)$p.value
chisq.test(Pos_Exerc_com_Estresse_Matematica, Pos_Exerc_com_Estresse_Verbal)$p.value
# Se o p-valor for maior que 5% não rejeito H0, nao rejeto a hipotese que 
# os dados seguem a mesma distribuição podendo assim afirmar que ambos seguem 
# a mesma distribuiçaõ

# Pre_Exerc_sem_Estresse, Pre_Exerc_com_Estresse_Matematica tem mesma distribuição, com p-valdor de 0.2560619
# Pre_Exerc_sem_Estresse, Pre_Exerc_com_Estresse_Verbal tem mesma distribuição, com p-valdor de 0.2124031
# Pre_Exerc_sem_Estresse, Pos_Exerc_sem_Estresse tem mesma distribuição, com p-valdor de 0.2560619
# Pre_Exerc_sem_Estresse, Pos_Exerc_com_Estresse_Matematica tem mesma distribuição, com p-valdor de 0.2720914
# Pre_Exerc_sem_Estresse, Pos_Exerc_com_Estresse_Verbal tem mesma distribuição, com p-valdor de 0.2560619
# Pre_Exerc_com_Estresse_Matematica, Pre_Exerc_com_Estresse_Verbal têm mesma distribuição, com p-valdor de 0.2720914


ks.test(Pre_Exerc_sem_Estresse, Pre_Exerc_com_Estresse_Matematica)$p.value
ks.test(Pre_Exerc_sem_Estresse, Pre_Exerc_com_Estresse_Verbal)$p.value
ks.test(Pre_Exerc_sem_Estresse, Pos_Exerc_sem_Estresse)$p.value
ks.test(Pre_Exerc_sem_Estresse, Pos_Exerc_com_Estresse_Matematica)$p.value
ks.test(Pre_Exerc_sem_Estresse, Pos_Exerc_com_Estresse_Verbal)$p.value
ks.test(Pre_Exerc_com_Estresse_Matematica, Pre_Exerc_com_Estresse_Verbal)$p.value
ks.test(Pre_Exerc_com_Estresse_Matematica, Pos_Exerc_sem_Estresse)$p.value
ks.test(Pre_Exerc_com_Estresse_Matematica, Pos_Exerc_com_Estresse_Matematica)$p.value
ks.test(Pre_Exerc_com_Estresse_Matematica, Pos_Exerc_com_Estresse_Verbal)$p.value
ks.test(Pre_Exerc_com_Estresse_Verbal, Pos_Exerc_sem_Estresse)$p.value
ks.test(Pre_Exerc_com_Estresse_Verbal, Pos_Exerc_com_Estresse_Matematica)$p.value
ks.test(Pre_Exerc_com_Estresse_Verbal, Pos_Exerc_com_Estresse_Verbal)$p.value
ks.test(Pos_Exerc_com_Estresse_Matematica, Pos_Exerc_com_Estresse_Verbal)$p.value



#####################################################################################
## Realizando testes PARAMETRICOS
#####################################################################################
