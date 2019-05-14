# Regressao nao parametrica
install.packages("ISLR")
install.packages("np")
install.packages("gam")

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

a <- c(AIC(modelo_glm1), AIC(modelo_glm2), AIC(modelo_glm3))
b <- c(BIC(modelo_glm1), BIC(modelo_glm2), BIC(modelo_glm3))

cbind(a,b)

residuos_modelo_glm <- modelo_glm3$residuals
fit_modelo_glm <- modelo_glm3$fitted.values

# Verificando a Normalidade
library(nortest)
qqnorm(residuos_modelo_glm); qqline(residuos_modelo_glm)
shapiro.test(residuos_modelo_glm)
lillie.test(residuos_modelo_glm)

# Verificando  a Homocedasticidade
plot(residuos_modelo_glm, fit_modelo_glm)

# Verificando função de ligação
plot(fit_modelo_glm, wage)

# Verificando a Independecia dos Erros
acf(residuos_modelo_glm)
# A partir há dois lags que cairam fora do intervalo de confiança
# porém como as correlações foram abaixo de 0.2 então podemos
# considerar que a suposição de autocorrelação não foi violada.

####################################
## Modelo Kernel
####################################
par(mfrow = c(2,2))

plot(Hours, Fissures, xlim = c(400,4600)); abline(lm(Fissures ~ Hours), data = fissBD)
ajust1 = sm.regression(Hours, Fissures)

plot(Turbines, Fissures, xlim = c(10,80)); abline(lm(Fissures ~ Turbines), data = fissBD)
ajust2 = sm.regression(Turbines, Fissures)

ajust3 = sm.regression(cbind(Hours,Turbines), Fissures)


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

residuos_modelo_gam2 <- modelo_gam2$residuals
fit_modelo_gam2  <- modelo_gam2$fitted.values

# Verificando a Normalidade
library(nortest)
qqnorm(residuos_modelo_gam2); qqline(residuos_modelo_gam2)
shapiro.test(residuos_modelo_gam2)
lillie.test(residuos_modelo_gam2)

# Verificando  a Homocedasticidade
plot(residuos_modelo_gam2, fit_modelo_gam2)

# Verificando função de ligação
plot(fit_modelo_gam2, wage)

# Verificando a Independecia dos Erros
acf(residuos_modelo_gam2)

