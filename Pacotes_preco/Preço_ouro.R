gold= read_csv("gold.csv")

gold$Month= months(gold$Date)
gold$Year= format(gold$Date, format="%y")

data= data.frame(aggregate(Close~Month+Year, gold, mean))

data2= data$Close

data3= ts(data2, start=c(2000,04), end=c(2022,09), frequency = 12)

data3

# Visualizando a serie
#============================================================================

plot(data3, col = "blue", lwd = 2)
abline(v= c(2021,9), col = "red", lwd = 2)

# Dividindo dados em conjuntos de treinamento e de teste
#   - Treinamento: meses de 2000,4 a 2021,9
#   - Teste: meses de 2021,10 a 2022,8
train_set <- window(data3, end = c(2021,9))
test_set <- window(data3, start = c(2021,10))

# Treinando a RN
#============================================================================
set.seed(1)
fit <- nnetar(train_set,
              p = 1,            # no. lags AR simples da entrada
              P = 1,            # no. lags AR sazonais da entrada
              size = 5,         # no. neuronios na camada escondida
              repeats = 20,     # no. de RNAs rodadas com diferentes pesos iniciais**
              scale.inputs = T) # opcao de escalonagem da entrada

# ** Na hora de prever são n (= repeats) previsoes feitas e a funcao 
#    'forecast()' computa a média dessas n previsoes

# Prevendo com a RN
#============================================================================
fcast <- forecast(fit, PI = T, level = c(95), h = 36, bootstrap = T)
# Nota: parametro 'PI = T' pede o computo de intervalos de previsao. O default
#       é simular os erros de uma distribuição normal, a menos que se use
#       o parametro 'bootstrap = T' que faz os erros serem simulados por bootstrap
fcast

plot(fcast, col = "black", lwd = 2)
abline(v= c(2022,9), col = "red", lwd = 2)
lines(data3, col = "black")

accuracy(test_set,fcast$mean)
# Obs: tambem funciona accuracy(x,fcast$mean), sem definir test_set

# Adicionado grafico da previsao dentro da amostra
#============================================================================
lines(fit$fitted, col = 'red')
# Obs: isto nao e estritamente necessario, mas util didaticamente

# Vendo series de resultados
#============================================================================
head(cbind(data3,fit$fitted,fcast$mean))
cbind(data3,fit$fitted,fcast$mean)


