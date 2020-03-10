library(ggplot2)
library(ggfortify)
library(forecast)
library(urca)
library(astsa)
library(fma)
library(expsmooth)

?uselec
autoplot(uselec)
ggseasonplot(uselec)

summary(uselec)

ndiffs(uselec)
nsdiffs(uselec)

modelo1 <- auto.arima(uselec, ic = "aic")
modelo2 <- auto.arima(uselec, ic = "bic")
modelo3 <- auto.arima(uselec, ic = "aic", d = 1)
modelo4 <- auto.arima(uselec, ic = "bic", d = 1)
modelo5 <- auto.arima(uselec, ic = "bic", d = 1, D = 1)

matrix(c(modelo1$aic,modelo1$aicc,modelo1$bic,
         modelo2$aic,modelo2$aicc,modelo2$bic,
         modelo3$aic,modelo3$aicc,modelo3$bic,
         modelo4$aic,modelo4$aicc,modelo4$bic,
         modelo5$aic,modelo5$aicc,modelo5$bic),
         5,3, byrow = TRUE, dimnames = list(c("modelo_aic",
         "modelo_bic", "modelo_aic_d1","modelo_bic_d1",
         "modelo_bic_d1D1"), c("aic", "aicc", "bic")))
# De acordo com a matriz formada pelos valores dos critérios de seleção de
# modelos, o modelo com melhor resultado foi o modelo selecionado pelo aic, porém
#  teve outro modelo com desempenho muito igual que foi o mesmo modelo 
# escolhido pelo aic porem adicionando d = 1, como resultou no teste feito 
#  antes que resultou que a série necessitava de uma diferenciação para 
# tornala estacionária.
# com o aic, bic, com d = 1, e com D = 1 também.



d_sarima1 <- sarima(uselec, 1,1,2, 0,1,1, 12)
d_sarima2 <- sarima(uselec, 1,1,0, 1,0,0, 12)
d_sarima3 <- sarima(uselec, 1,1,0, 1,1,0, 12)

# matrix(c(d_sarima1$AIC,d_sarima1$AICc,d_sarima1$BIC,
# d_sarima2$AIC,d_sarima2$AICc,d_sarima2$BIC,
#          d_sarima3$AIC,d_sarima3$AICc,d_sarima3$BIC,
#          d_sarima4$AIC,d_sarima4$AICc,d_sarima4$BIC,
#          d_sarima5$AIC,d_sarima5$AICc,d_sarima5$BIC),
#        5,3, byrow = TRUE, dimnames = list(c("modelo_aic",
#         "modelo_bic", "modelo_aic_d1","modelo_bic_d1",
#        "modelo_bic_d1D1"), c("aic", "aicc", "bic")))


# Faço análise de residuos 

d_sarima1 <- sarima(uselec, 1,1,2, 0,1,1, 12)
d_sarima2 <- sarima(uselec, 1,1,0, 1,0,0, 12)
d_sarima3 <- sarima(uselec, 1,1,0, 1,1,0, 12)

# Vamos lá, no primeiro modelo os residuos estão dentro de
# um intervalo desejado, entre -2, e 3, tranquilo, no plot
# do acf todos os lags estão abaixo das linhas do intervalo
# de confiança, ou seja, a hipotese de serídos não 
# correlacionados não foi violada, no QQPLot eu não sei dizer
# porque segue a reta de normalidade apenas no meio, e nos 
# extremos os pontos não estão nem dentro do envelope.

# o modelos que passaria na análise de resíudos seria o primeiro
# modelo que inclusive está bem ajustado de acordo com o teste
# de Ljung-Box todos os pontos estão com p-valores altos.
# E também foi o único modelo em que todos parametros estimados
# foram significativos a qualquer nível nominal.


####
# Mas vamos seguir com os três modelos para a previsão
# e ver qual modelo preve de forma melhor.
####

# Começo os modelos de previsão.

###############
# METODOS DE PREVISAO
#############

uselec2 <- window(uselec, start = 1985, end = c(1996,10)) #truncar a serie retirando o ultimo ano
uselec3 <- window(uselec, start = c(1995,11)) # ultimo ano da serie

###############
###############
###############
###############

#sarimax
#a) dummy
d = rep(0, length(uselec))
d[3] = 1

fit1_xreg_dummy <- sarima(uselec, 1,1,2, 0,1,1, 12, xreg = d) # Passou nos testes, mas aceitavel
fit2_xreg_dummy <- sarima(uselec, 1,1,0, 1,0,0, 12, xreg = d) # Não passou nos testes, mas NÃO aceitavel
fit3_xreg_dummy <- sarima(uselec, 1,1,0, 1,1,0, 12, xreg = d) # Não passou nos testes, mas NÃO aceitavel

#b) tendencia
t = decompose(uselec)$trend

fit1_xreg_tend <- sarima(uselec, 1,1,2, 0,1,1, 12, xreg = t) # Não passou nos testes, mas NÃO aceitavel
fit2_xreg_tend <- sarima(uselec, 1,1,0, 1,0,0, 12, xreg = t) # Não passou nos testes, mas NÃO aceitavel
fit3_xreg_tend <- sarima(uselec, 1,1,0, 1,1,0, 12, xreg = t) # Não passou nos testes, mas NÃO aceitavel


#c) sazionalidade
s = decompose(uselec)$seasonal
fit1_xreg_seasonal <- sarima(uselec, 1,1,2, 0,1,1, 12, xreg = s) # Passou nos testes, aceitavel
fit2_xreg_seasonal <- sarima(uselec, 1,1,0, 1,0,0, 12, xreg = s) # Não passou nos testes, mas NÃO aceitavel
fit3_xreg_seasonal <- sarima(uselec, 1,1,0, 1,1,0, 12, xreg = s) # Não passou nos testes, mas NÃO aceitavel

###############
###############
###############
###############
 
fit.sarima1.uselec2=arima(uselec2, order = c(1,1,2), seasonal = list(order = c(0,1,1)))
fit.sarima2.uselec2=arima(uselec2, order = c(2, 1, 0), seasonal = list(order = c(1, 0, 0)))

pred.uselec.sarima1 <- predict(fit.sarima1.uselec2,n.ahead=12, se.fit=T)
pred.uselec.sarima1$pred

pred.uselec.sarima2 <- predict(fit.sarima2.uselec2,n.ahead=12, se.fit=T)
pred.uselec.sarima2$pred

accuracy(pred.uselec.sarima1$pred, uselec3)[1, c(3,2,5)]
accuracy(pred.uselec.sarima2$pred, uselec3)[1, c(3,2,5)]

############# tendencia, dummy, sazionalidade

t2 = t[-c(177:188)]
d2 = d[-c(177:188)]
s2 = s[-c(177:188)]

t3 = t[c(177:188)]
d3 = d[c(177:188)]
s3 = s[c(177:188)]

fit.sarima1.t.uselec2=arima(uselec2, order = c(2, 1, 0), seasonal = list(order = c(2, 0, 0)), xreg = t2)
fit.sarima2.t.uselec2=arima(uselec2, order = c(2, 1, 0), seasonal = list(order = c(1, 0, 0)), xreg = t2)
fit.sarima1.d.uselec2=arima(uselec2, order = c(2, 1, 0), seasonal = list(order = c(2, 0, 0)), xreg = d2)
fit.sarima2.d.uselec2=arima(uselec2, order = c(2, 1, 0), seasonal = list(order = c(1, 0, 0)), xreg = d2)
fit.sarima1.s.uselec2=arima(uselec2, order = c(2, 1, 0), seasonal = list(order = c(2, 0, 0)), xreg = s2)
fit.sarima2.s.uselec2=arima(uselec2, order = c(2, 1, 0), seasonal = list(order = c(1, 0, 0)), xreg = s2)


pred.uselec2.sarima1.t <- predict(fit.sarima1.t.uselec2,n.ahead=12,xreg=t3, newxreg=t3)
pred.uselec2.sarima2.t <- predict(fit.sarima2.t.uselec2,n.ahead=12,xreg=t3, newxreg=t3)
pred.uselec2.sarima1.d <- predict(fit.sarima1.d.uselec2,n.ahead=12,xreg=d3, newxreg=d3)
pred.uselec2.sarima2.d <- predict(fit.sarima2.d.uselec2,n.ahead=12,xreg=d3, newxreg=d3)
pred.uselec2.sarima1.s <- predict(fit.sarima1.s.uselec2,n.ahead=12,xreg=s3, newxreg=s3)
pred.uselec2.sarima2.s <- predict(fit.sarima2.s.uselec2,n.ahead=12,xreg=s3, newxreg=s3)


pred.modelo.aic<- accuracy(pred.uselec.sarima1$pred, uselec3)[1, c(3,2,5)]
pred.modelo.bic<- accuracy(pred.uselec.sarima2$pred, uselec3)[1, c(3,2,5)]
pred.modelo.aic.t<- accuracy(pred.uselec2.sarima1.t$pred, uselec3)[1, c(3,2,5)]
pred.modelo.bic.t<- accuracy(pred.uselec2.sarima2.t$pred, uselec3)[1, c(3,2,5)]
pred.modelo.aic.d<- accuracy(pred.uselec2.sarima1.d$pred, uselec3)[1, c(3,2,5)]
pred.modelo.bic.d<- accuracy(pred.uselec2.sarima2.d$pred, uselec3)[1, c(3,2,5)]
pred.modelo.aic.s<- accuracy(pred.uselec2.sarima1.s$pred, uselec3)[1, c(3,2,5)]
pred.modelo.bic.s<- accuracy(pred.uselec2.sarima2.s$pred, uselec3)[1, c(3,2,5)]


matrix(c(pred.modelo.aic[1],pred.modelo.aic[2],pred.modelo.aic[3],
         pred.modelo.bic[1],pred.modelo.bic[2],pred.modelo.bic[3],
         pred.modelo.aic.t[1],pred.modelo.aic.t[2],pred.modelo.aic.t[3],
         pred.modelo.bic.t[1],pred.modelo.bic.t[2],pred.modelo.bic.t[3],
         pred.modelo.aic.d[1],pred.modelo.aic.d[2],pred.modelo.aic.d[3],
         pred.modelo.bic.d[1],pred.modelo.bic.d[2],pred.modelo.bic.d[3],
         pred.modelo.aic.s[1],pred.modelo.aic.s[2],pred.modelo.aic.s[3],
         pred.modelo.bic.s[1],pred.modelo.bic.s[2],pred.modelo.bic.s[3]),
       8,3,byrow = TRUE,dimnames = list(c("modelo.aic", "modelo.bic",
                                          "modelo.aic.t", "modelo.bic.t",
                                          "modelo.aic.d", "modelo.bic.d",
                                          "modelo.aic.s", "modelo.bic.s"), c("MAE", "RMSE", "MAPE")))

# Previsão excluindo as 12 ultimas observações

length(uselec2)
length(uselec3)

uselecfit1 <- meanf(uselec2, h = 12)
uselecfit2 <- rwf(uselec2, h = 12)
uselecfit3 <- rwf(uselec2, h = 12, drift = TRUE)
uselecfit4 <- snaive(uselec2, h = 12)

autoplot(window(uselec, start = 1949)) +
  autolayer(uselecfit1, series = "Mean", PI = FALSE) +
  autolayer(uselecfit2, series = "Naive", PI = FALSE) +
  autolayer(uselecfit3, series = "Naive com drift", PI = FALSE) +
  autolayer(uselecfit4, series = "Naive com sazonalidade", PI = FALSE) +
  ggtitle("Previsão para o número totl de passageiros") +
  guides(colour = guide_legend(title = "PrevisãO"))

accuracy(uselecfit1, uselec3)[1, c(3,2,5)]

accuracy(uselecfit2, uselec3)[1, c(3,2,5)]

accuracy(uselecfit3, uselec3)[1, c(3,2,5)]

accuracy(uselecfit4, uselec3)[1, c(3,2,5)]
# Quem ganhou foi o terceiro modelo.  

##############################
# Alisamento exponencial
#############################
# Alisamento expoenncial aditivo e multiplicativo
air <- window(uselec, start = 1980)
fit1 <- hw(uselec2, seasonal = "additive", h = 12)
fit2 <- hw(uselec2, seasonal = "multiplicative", h = 12)

autoplot(air) +
  autolayer(fit1, series = "HW - aditivo", PI = FALSE) +
  autolayer(fit2, series = "HW - multiplicativo", PI = FALSE) +
  guides(colou = guide_legend(title = "Forecast"))

# Vamos comparar as previsões - Métodos (mean, naive, drift) e AE

pred.uselec2.ae.ad <- accuracy(fit1$mean, uselec3)[1, c(3,2,5)]
pred.uselec2.ae.mu <- accuracy(fit2$mean, uselec3)[1, c(3,2,5)]
pred.uselec2.meanf <- accuracy(uselecfit1$mean, uselec3)[1, c(3,2,5)]
pred.uselec2.rwf <- accuracy(uselecfit2$mean, uselec3)[1, c(3,2,5)]
pred.uselec2.rwf.drift <- accuracy(uselecfit3$mean, uselec3)[1, c(3,2,5)]
pred.uselec2.snive <- accuracy(uselecfit4$mean, uselec3)[1, c(3,2,5)]

matrix(c(pred.uselec2.ae.ad[1],pred.uselec2.ae.ad[2],pred.uselec2.ae.ad[3],
         pred.uselec2.ae.mu[1],pred.uselec2.ae.mu[2],pred.uselec2.ae.mu[3],
         pred.uselec2.meanf[1],pred.uselec2.meanf[2],pred.uselec2.meanf[3],
         pred.uselec2.rwf[1],pred.uselec2.rwf[2],pred.uselec2.rwf[3],
         pred.uselec2.rwf.drift[1],pred.uselec2.rwf.drift[2],pred.uselec2.rwf.drift[3],
         pred.uselec2.snive[1],pred.uselec2.snive[2],pred.uselec2.snive[3]),
       6,3,byrow = TRUE,dimnames = list(c("Alisamento exponencial aditivo","Alisamento exponencial mutiplicativo",
                                          "Média", "Naive", "Naive drift", "Naive sazonal"),
                                        c("MAE", "RMSE", "MAPE")))

