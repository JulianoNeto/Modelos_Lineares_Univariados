
library(quantmod)
library(fBasics)
library(ggplot2)
library(scales)
library(gridExtra)
library(forecast)
library(Quandl)
library(urca)
library(stargazer)

## Série Temporal da Brasken

getSymbols('BRKM5.SA')

autoplot(BRKM5.SA$BRKM5.SA.Adjusted)+
  labs(title='Ação da Braskem S.A. negociada na Bovespa')


## Estacionariedade

BRKM.rtn = diff(log(BRKM5.SA$BRKM5.SA.Adjusted))

autoplot(BRKM.rtn)+
  labs(title='Log retornos da ação da Braskem S.A.')

## Ruído Branco

set.seed(1)
y <- rnorm(100)
plot(y, type = "l")

## Passeio Aleatório

x <- w <- rnorm(1000)
for (t in 2:1000) x[t] <- x[t - 1] + w[t]
plot(x, type = "l")

## Passeio Aleatório com Drift

set.seed(123456)
e <- rnorm(500)
trd <- 1:500
rw.wd <- 0.5*trd + cumsum(e)
plot(rw.wd)

## Funções de Autocorrelação

url = 'http://faculty.chicagobooth.edu/ruey.tsay/teaching/introTS/ch2data.zip'
temp = tempfile()
download.file(url, temp)
da = unzip(temp, files='m-dec12910.txt')
da = read.table(da,header=T)
d10=da$dec10 # select the Decile 10 returns
dec10 = ts(d10,frequency=12,start=c(1967,1))
g1 = autoplot(dec10)+labs(title='Retornos simples de um portfólio')
g2 = ggAcf(dec10, main='Função de Autocorrelação dos retornos')
grid.arrange(g1, g2, ncol=1)
