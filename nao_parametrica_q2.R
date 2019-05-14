
# Regressao nao parametrica
install.packages("ISLR")
install.packages("np")
install.packages("gam")
install.packages("sm")

library(sm)
library(ISLR)
library(np)
library(gam)

####################################
## Banco Wage
####################################

library(ISLR)
data(Wage)
attach(Wage)

####################################
## Modelo linear
####################################

modelo_glm1 <- glm(wage ~ ns(year) + ns(age,5) + education, family = gaussian(link = "identity"), data = Wage)
fit_modelo_glm1 <- summary(modelo_glm1)

modelo_glm2 <- glm(wage ~ ns(year) + ns(age,5) + education, family = gaussian(link = "log"), data = Wage)
fit_modelo_glm2 <- summary(modelo_glm2)

modelo_glm3 <- glm(wage ~ ns(year) + ns(age,5) + education, family = gaussian(link = "inverse"), data = Wage)
fit_modelo_glm3 <- summary(modelo_glm3)

desvc1 <- fit_modelo_glm1$deviance/fit_modelo_glm1$dispersion
desvc2 <- fit_modelo_glm2$deviance/fit_modelo_glm2$dispersion
desvc3 <- fit_modelo_glm3$deviance/fit_modelo_glm3$dispersion

# Teste de adequação global.
desvc1 < qchisq(0.95, fit_modelo_glm1$df.residual)
desvc2 < qchisq(0.95, fit_modelo_glm2$df.residual)
desvc3 < qchisq(0.95, fit_modelo_glm3$df.residual)
# Todos os três modelos passaram pelo teste de adequação global.

d <- c(desvc1, desvc2, desvc3)
which(d == min(d))
# O modelo com distribuição normal e função de ligação inversa.

AIC(modelo_glm3)
BIC(modelo_glm3)

cbind(a,b)

####################################
## Modelo Kernel
####################################
par(mfrow = c(2,2))



# ajust1 = sm.regression(year, wage)
# ajust2 = sm.regression(age, wage)
ajust3 = sm.regression(cbind(year,age), wage)


####################################
## Modelo gam
####################################
modelo_gam2 <- gam(wage ~ s(year,4) + s(age,5) + education, data = Wage)
par(mfrow = c(1,3))
plot(modelo_gam2, se = TRUE, col = "blue")
# interpretação pagina 284.

# O p-valor de anos(variável year) e idade(a variável age) corresponde a hipótese nula de
# da relação linear versus a alternativa da relação não linear. O largo p-valor para year

AIC(modelo_gam2)
BIC(modelo_gam2)

a <- c(AIC(modelo_glm3), AIC(modelo_gam2))
b <- c(BIC(modelo_glm3),  BIC(modelo_gam2))
a
b


