
#########################################################################
############# Modelo de Séries Temporais ################################
############# Instação de Pacotes

install.packages(c("BETS","urca","TSA","forecast","lmtest","normtest","FinTS","xlsx"))
###########################################################################

############ Carregar Pacotes ###########################################
library(BETS)         # Brazilian Economic Times Series
library(urca)         # Unit Root and Cointegration Test for Times Series Data
library(TSA)          # Times Series Analysi
library(forecast)     # Forecasting Functions for Time Series and Linear Models
library(lmtest)       # Testing Linear Regression Models
library(normtest)     # Test for Normality
library(FinTS)        # Companion to Tsay, Analysis of Financial Times Series
library(xlsx)         # Read, Write, Format Excel
#######################################################################

############ Análise Exploratória de Séries Temporais #################
############ Leitura da Base Dados AirPassenger ##################################

# Monthly Airline Passenger Numbers 1949-1960
data("AirPassengers")

############ Gráfico Simples ####################################
ts.plot(AirPassengers,
        col='blue',
        main='Venda de Bilhetes Aéreos no USA (em milhares $)',
        ylab= "Passagens Aéreas",
        xlab= "Anos")
grid(col='darkred', lwd=1)

########### Comentários ##########################################
# 1) Ao observar o gráfico, percebe-se uma tendência crescente do numero de passageiros
# 2) Indício de comportamento sazonal (março e julho)
# 3) Oscilações (jan a out, comportamento crescente) + (nov a dez, decrescente)
# 4) Variância a distância entre os meses esta aumentando


############ Gráfico Mensal #######################################
monthplot(AirPassengers,
          ylab= "Vendas de Passagens Aéreas",
          xlab= "Anos")


############ Decomposição da Série Temporal #######################
# a função decompose usa filtros de médias móveis p/ decompor em 3
# 1) Tendência + ciclco
# 2) Sazonalidade
# 3) Erro (resíduo)


plot(decompose(AirPassengers),
     ylab= "Vendas de Passagens Aéreas",
     xlab= "Anos")

############# Testar a Estacionaridade da Parte Não sazonal ########
# Existem 4 maneiras
# 1) Análise Gráfica
# 2) Comparação da Média/Variância em diferentes períodos
# 3) Observação da FAC (Função de Autocorrelação)
# 4) Teste da Raiz Unitária

############### Função de Autocorrelação(FAC) #############
corrgram(AirPassengers, lag.max= 36, 
         type = "correlation", 
         mode = "simple",
         ci = 0.95, 
         style = "plotly", 
         knit = TRUE)

############## Teste de Raiz Unitária(RU) ################
# Existem 4 testes
# 1) Argumented Dickey Fuller (ADF)
# 2) Phillips-Peron (PP)
# 3) KwaiatKowski-Phillips-Schimidt-Schin (KPSS)
# 4) Dickey Fuller GLS (DF-GLS)

# Teste de Dickey Fuller Aumentado (ADF)

# Hipóteses
# H0 : a série temporal Possui uma Raiz Unitária
# H1 : a série temporal Não Possui uma Raiz Unitária


# Teste ADF
adf.drift <- ur.df(y = AirPassengers, type = c("drift"),
                   lags = 24, selectlags = "AIC") 

# Estatística do Teste
adf.drift@teststat

# Valores Tabulados p/ Mackinnon(1996)
adf.drift@cval

# Conclusão: a série Não é Estacionária, ou seja, A série tem raiz unitária

# Equação Ajustada
# regressão da primeira diferença da série contra sua defasagem
summary(adf.drift)

# FAC dos Resíduos
corrgram(adf.drift@res, lag.max = 36)


# Descobrir o número de Diferenciações necesárias p/ tornar a série Estacionária

# Fazendo 1 Diferenciação Dado original
Diferenca1 <- ts.plot()

# FAC com 1 diferença 
corrgram(diff(AirPassengers, lag = 1, differences = 1),
         lag.max = 36)

# Após aplicar 1 diferenciação na ST conclusões:
# 1) Aparenta está estacionaria na média
# 2) variância é crescente ao longo do tempo.


# Pressuposto da Teoria Box & Jenkins
# Série seja estacionária na Variância

# Passar o log na ST

# Logaritmo da Séie 
ts.plot(diff(log(AirPassengers), lag = 1, differences = 1))

# Logaritmo da Série com 1 diferença
corrgram(diff(log(AirPassengers), lag = 1, differences = 1),
         lag.max = 48)

# Série Estacionária tando Média + Variância


############### Testar a Estacionariedade da Parte Sazonal ###########

# FAC apresenta um decréscimo lento, indicando Não Estacionariedade na parte sazonal

# Corrigir este problema
# 1) diferenciar a parte sazonal

# Logaritmo da Série com diferença sazonal e Não sazonal
corrgram(diff(diff(log(AirPassengers), lag = 1, differences = 1), 
                  lag = 12, differences = 1), lag.max = 48)

#  FAC apresenta cortes bruscos nos lags 1 e 12
# Conclusão: Não apresenta decrescimento lento (parte zanonal e não sazaonal) 

# Refazer o Teste de Raiz Unitária

# Teste ADF com Diferença Sazonal + Não Sazonal
adf.drift2 <- ur.df(y= diff(diff(log(AirPassengers), 
                        lag = 1), lag = 12),
                        type = "drift", lags = 24, selectlags = "AIC")
# Estatística do Teste
adf.drift2@teststat

# Valores Tabulados p/ Mackinnon(1996)
adf.drift2@cval


# FAC dos Resíduos
corrgram(adf.drift2@res, lag.max = 36)


# Regressão da primeira diferença da série contra sua defasagem
summary(adf.drift2)



########## MODELAGEM ###########
# Usar o Pacote Forecast

# FAC
corrgram(diff(diff(log(AirPassengers), lag = 1, differences = 1), 
              lag = 12, differences = 1), lag.max = 48)

# FACP
corrgram(diff(diff(log(AirPassengers), lag = 1, differences = 1), 
              lag = 12, differences = 1), type = "partial", lag.max = 48)

# Modelo 1
Mod1<- fit.air <- Arima(AirPassengers, order = c(1,1,1), 
                   seasonal = c(1,1,1), method = "ML", lambda = 0)
fit.air

# Teste DE Significância p/ Modelo SARIMA(1,1,1)(1,1,1)_12
t_test(Mod1)

# Modelo 2
Mod2 <- fit.air <- Arima(AirPassengers, order = c(0,1,1), 
                 seasonal = c(0,1,1), method = "ML", lambda = 0)

# Teste DE Significância p/ Modelo SARIMA(0,1,1)(0,1,1)_12
t_test(Mod2)


# Diagnostico
diad <- tsdiag(Mod1, gof.lag = 20)

# Autocorrelação dos Resíduos/ Teste Liun & Box




