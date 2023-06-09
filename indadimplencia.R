### analise da taxa de inandimplencia das opera��es de credito maranhao
### elabora��o: Paulo Roberto Carneiro de S�

# pacotes necessarios:
library(BETS)
library(dygraphs)
library(ggplot2)
library(fpp2)
library(FitAR)
library(tseries)
library(forecast)

######################################################################################################
creditoma <- BETSget(15934) 
print(creditoma)
class(creditoma)
View(creditoma)

creditoma2= window(creditoma, start = c(2010, 1), end = c(2020, 3))
autoplot(creditoma)
autoplot(creditoma2)

dcreditoma2 = diff(creditoma2)
# estatisticas basicas
summary(creditoma2)
hist(creditoma2) #Histograma

### plot basico lembrar que em class(), ele j� indicou que era ts = serie
# temporal
plot(creditoma2)

# pelo pacote dygraph d� mais op��es
library(dygraphs)
help("dygraph")

dygraph(creditoma2, main = "Taxa de indadimpl�ncia das opera��es de cr�dito total no Maranh�o <br> (Mensal) janeiro de 2010 a mar�o de 2020") %>%                 
        dyAxis("x", drawGrid = TRUE) %>% 
        dyEvent("2010-1-01", "2010", labelLoc = "bottom")%>% 
        dyEvent("2012-1-01", "2012", labelLoc = "bottom")%>% 
        dyEvent("2014-1-01", "2014", labelLoc = "bottom")%>% 
        dyEvent("2016-1-01", "2016", labelLoc = "bottom")%>% 
        dyEvent("2018-1-01", "2018", labelLoc = "bottom")%>% 
        dyEvent("2020-1-01", "2020", labelLoc = "bottom")%>% 
        dyOptions(drawPoints = TRUE, pointSize = 2)

########################################################################################################################

#suaviza��o simples pela m�dia 

plot(aggregate(creditoma2, FUN = mean)) #suaviza��o simples pela media

########################################################################################################################

#avaliando residuos

# analise dos residuos - criacao do modelo preditivo de serie temporal 
prev = auto.arima(creditoma2)

print(prev$residuals) #valores dos residuo

autoplot(prev$residuals) #plotagem dos residuos
hist(prev$residuals) #histograma, distrbuicao
var(prev$residuals, na.rm = TRUE) #varian�a excluindo valores NA #[1] 0.02899597
mean(as.vector(prev$residuals), na.rm = TRUE) #media #[1] -0.01171754

acf(prev$residuals, na.action = na.pass) #diagrama de correlacao dos residuais 

checkresiduals(prev) #teste de correla��o dos residuais (Ljung-box text)
#Ljung-Box test

#data:  Residuals from ARIMA(0,1,0)(1,0,0)[12]
#Q* = 35.584, df = 23, p-value = 0.04547

#Model df: 1.   Total lags used: 24

shapiro.test(prev$residuals) #teste de correlacao shapiro teste

#########################################################################################################

#teste de estacionariedade 

x = Box.test(creditoma2, type = "Ljung-Box") #teste de estacionariedade Ljung-Box
print(x)































































































































