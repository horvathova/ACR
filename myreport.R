#' ---
#' title: "Analýza časových radov"
#' output:
#'    html_document:
#'      toc: true
#'      theme: default
#' ---
#' 
#' # 1. Základný popis časového radu
#'
#' Časový rad znázorňuje ročné zmeny globálnych teplôt v rokoch 1880-1985. Jednotka teploty je stupeň Celzia a jednotka času je rok. Dahný rad obsahuje 106  

library(dplyr)
setwd(choose.dir(default = "D:\\Documents\\R\\Project", caption = "Select folder"))
MyData <- read.table('temp.csv', header=TRUE, sep=";")
xlabel <- "Year"
ylabel <- "Temperature °C"
plot(MyData$Year ,MyData$Annual.changes, xlab = xlabel, ylab = ylabel, type="l", main = "Annual changes in global temperature")
abline(h = 0)

matica <- matrix(c(max(MyData$Annual.changes),min(MyData$Annual.changes),mean(MyData$Annual.changes),median(MyData$Annual.changes), sd(MyData$Annual.changes)), ncol=1)
colnames(matica) <- c("Hodnoty")
rownames(matica) <- c("Max","Min","Stredna hodnota","Median", "Smerodajna odchylka")
matica <- as.table(matica)
matica

#' # 2. Dekompozícia časového radu: trend a sezónna zložka

zac <- MyData[1, "Year"]
koniec <- MyData[nrow(MyData), "Year"]

MyDataSplit <- split(MyData, MyData$Year-zac >(koniec - zac)*0.80,-2) %>%
  setNames(c("train", "eval"))
head(MyData$eval, 3)

plot(Annual.changes ~ Year, do.call(rbind, MyDataSplit), type="n")  # definuj vykreslovaciu oblast ale nekresli
lines(Annual.changes ~ Year, MyDataSplit$train)
lines(Annual.changes ~ Year, MyDataSplit$eval, col="red")
legend("bottomright", legend=c("training set", "evaluation set"), lty=1, col=c(1,2))

## Dekompozícia
trainAnnualChanges <- ts(MyDataSplit$train$Annual.changes, start=MyDataSplit$train$Year[1], frequency=4)
plot(decompose(trainAnnualChanges))
plot(decompose(trainAnnualChanges, type="multiplicative"))
## Trend 
#' ### Linearny
model <- list()
(model$linear <- lm(Annual.changes ~ Year, MyDataSplit$train))
summary(model$linear) # sumarna statistika
confint(model$linear) # interval spolahlivosti pre parametre
MyDataSplit$lin <- model$linear$fitted.values

#' ### Kvadraticky
(model$kvad <- lm(Annual.changes ~ Year + I(Year^2), MyDataSplit$train))
summary(model$kvad) # sumarna statistika
MyDataSplit$kvad <- model$kvad$fitted.values # modelove hodnoty

#' ### Exponenciálny
#'Nema zmysel
#'


#' ### Porovnanie
plot(Annual.changes ~ Year, MyDataSplit$train, main="Linearny a kvadraticky trend")
lines(MyDataSplit$train$Year, MyDataSplit$lin, col="blue", lwd=3)
lines(MyDataSplit$train$Year, MyDataSplit$kvad , col="red", lwd=2)
legend("topleft", legend=c("data", "linear trend", "quadratic"), pch=c(1,NA,NA), col=c("black","blue","red"), lty=c(0,1,1), lwd=c(1,3,2))

#' ### Rezíduá
#' Lin
MyDataSplit$train$LinearRez <- model$lin$residuals
plot(LinearRez ~ Year, MyDataSplit$train, type="l", main="Linearne rezidua")
#' Kvad
MyDataSplit$train$KvadRez <- model$kvad$residuals
plot(KvadRez ~ Year, MyDataSplit$train, type="l", main="Kvadraticke rezidua")


#' Rezidualny rozptyl
#' Transformacia do logaritmickej mierky
plot(log(Annual.changes) ~ Year, MyDataSplit$train)

grep("loT", names(MyDataSplit$train), value=T) %>% 
  sapply(function(x) var(MyDataSplit$train$loGDP - MyDataSplit$train[[x]])) %>% 
  round(4)
