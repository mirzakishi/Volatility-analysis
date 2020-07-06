# This script is developed in order to obtain the results of 
# Russian Machinery Stock Market Volatility. 

#1 Import libraries
library('readxl')
library('ggplot2')
library('dplyr')
library('xtable')
library('aTSA')
library('tseries')
library('forecast')

#2 Data download 
data = as.data.frame(read_excel(path = 'Data_1.xlsx')) # download data
data[, 1] = as.Date.POSIXct(data[, 1], format = '%Y-%m-%d') # convert time to POSIX

#3 Data with log returns
data2 = data
for (j in c(2:6)){
  data2[, j] = log(data2[, j] / lag(data2[, j]))
}
data2 = data2[complete.cases(data2), ]

#4 Descriptive Visualization  
plot_price = function(data, ticker, type, saved = FALSE){
  if (type == 'price'){
    title = paste('Динамика цены ', ticker, sep = '')
    y = 'Цена закрытия'
  } else if (type == 'return'){
    title = paste('Динамика логдоходности ', ticker, sep = '')
    y = 'Логдоходность'
  }
  plot = ggplot(data[, c('TRADEDATE', ticker)], aes(x = get('TRADEDATE'), y = get(ticker))) + 
    geom_line() + theme_bw() + labs(x = 'Дата', y = y, 
                                    title = title)
  if (saved == TRUE) {
    ggsave(paste('plot_', type, '_', ticker, '.png', sep = ''), 
           plot = last_plot(), device = NULL,
           scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
           dpi = 300, limitsize = TRUE)
    
  }
  return(plot)
}

for (ticker in c('ZILL', 'SVAV', 'KMAZ', 'GAZA', 'OMZZ_p' )){
  plot_price(data, ticker, 'price', saved = TRUE)
  plot_price(data2, ticker, 'return', saved = TRUE)
}

#5 Check for Stationarity
stationarity_check = function(ticker, type){
  stat_result = aTSA::adf.test(data2[, ticker])
  if (type == '1'){
    data = stat_result$type1
  } else if (type == '2'){
    data = stat_result$type2
  } else {
    data = stat_result$type3
  }
  output = xtable(data, 
                  type = 'latex')
  print(output, file = paste('adf_test_', type, '_', ticker, '.tex', sep = ''), 
        floating = getOption("xtable.floating", FALSE)) 
}
for (j in c('ZILL', 'SVAV', 'GAZA', 'OMZZ_p', 'KMAZ')){
  for (k in c('1', '2', '3')){
    stationarity_check(j, k)
  }
}


#6 Normality / Skewness / Kurtosis 
distr_return = function(data, ticker, saved = FALSE){
  ggplot(data = data, 
         aes(get(ticker))) + geom_histogram() + theme_bw() + labs(x = ticker, y = 'Частота', 
                                                                  title = paste('Распределение логдоходностей ', ticker, 
                                                                                sep = '')) + 
    stat_function(fun=dnorm,
                  color='grey',
                  args=list(mean=mean(data[, ticker]), 
                            sd=sd(data[, ticker])))
  if (saved == TRUE) {
    ggsave(paste('hist_', '_', ticker, '.png', sep = ''), 
           plot = last_plot(), device = NULL,
           scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
           dpi = 300, limitsize = TRUE)
    
  }
}

for (j in c('ZILL', 'SVAV', 'KMAZ', 'GAZA', 'OMZZ_p')){
  distr_return(data2, j, saved = TRUE)
}

#7 Shapiro-Wilk
normality_test = function(data, ticker){
  result_shapiro = shapiro.test(data[, ticker])
  stat_shapiro = round(as.numeric(result_shapiro$statistic), 3)
  pvalue_shapiro = format(round(as.numeric(result_shapiro$p.value), digits = 3))
  row = c(ticker, stat_shapiro, pvalue_shapiro)
  return(row)
  
}
results = normality_test(data2, 'ZILL')
for (j in c('SVAV', 'KMAZ', 'GAZA', 'OMZZ_p')){
  results = rbind(results, normality_test(data2, j))
}
results = as.data.frame(results)
names(results) = c('ticker', 'Shapiro-Wilk', 'p.value')
rownames(results) = c()
print(
  xtable(results, digits = 3), file = 'shapiro_wilk.tex', 
  floating = getOption("xtable.floating", FALSE))



#8 ARMA modeling
png('acf_zill.png', width = 350, height = 350)
plot(acf(data2[, 'ZILL']), main = 'ZILL')
dev.off()
png('pacf_zill.png', width = 350, height = 350)
plot(pacf(data2[, 'ZILL']), main = 'ZILL')
dev.off()
auto.arima(data2[, 'ZILL'])
model_zill = arima(data2[, 'ZILL'], c(1, 0, 2))
model_zill$aic  # -2683.678

png('acf_svav.png', width = 350, height = 350)
plot(acf(data2[, 'SVAV']), main = 'SVAV')
dev.off()
png('pacf_svav.png', width = 350, height = 350)
plot(pacf(data2[, 'SVAV']), main = 'SVAV')
dev.off()
auto.arima(data2[, 'SVAV'])
model_svav = arima(data2[, 'SVAV'], c(5, 0, 0))
model_svav$aic  # -3968.313

png('acf_kmaz.png', width = 350, height = 350)
plot(acf(data2[, 'KMAZ']), main = 'KMAZ')
dev.off()
png('pacf_kmaz.png', width = 350, height = 350)
plot(pacf(data2[, 'KMAZ']), main = 'KMAZ')
dev.off()
auto.arima(data2[, 'KMAZ'])
model_kmaz = arima(data2[, 'KMAZ'], c(3, 0, 3))
model_kmaz$aic # -3915.562

png('acf_gaza.png', width = 350, height = 350)
plot(acf(data2[, 'GAZA']), main = 'GAZA')
dev.off()
png('pacf_gaza.png', width = 350, height = 350)
plot(pacf(data2[, 'GAZA']), main = 'GAZA')
dev.off()
auto.arima(data2[, 'GAZA'])
model_gaza = arima(data2[, 'GAZA'], c(0, 0, 1))
model_gaza$aic # -3592.234

png('acf_omzz_p.png', width = 350, height = 350)
plot(acf(data2[, 'OMZZ_p']), main = 'OMZZ_p')
dev.off()
png('pacf_omzz_p.png', width = 350, height = 350)
plot(pacf(data2[, 'OMZZ_p']), main = 'OMZZ_p')
dev.off()
auto.arima(data2[, 'OMZZ_p'])
model_omzz_p = arima(data2[, 'OMZZ_p'], c(0, 0, 0))
model_omzz_p$aic # -2245.244

#9 Check for white noise
ZILL = Box.test(model_zill$residuals, type='Ljung', lag=log(length(model_zill$residuals)))
SVAV = Box.test(model_svav$residuals, type='Ljung', lag=log(length(model_svav$residuals)))
KMAZ = Box.test(model_kmaz$residuals, type='Ljung', lag=log(length(model_kmaz$residuals)))
GAZA = Box.test(model_gaza$residuals, type='Ljung', lag=log(length(model_gaza$residuals)))
OMZZ_p = Box.test(model_omzz_p$residuals, type='Ljung', lag=log(length(model_omzz_p$residuals)))

row_combine = function(boxes, ticker){
  row = cbind(ticker, round(as.numeric(boxes$statistic), 3), round(as.numeric(boxes$p.value), 3))
  rownames(row) = NULL  
  return(row)
}

whitenoisecheck = row_combine(get('ZILL'), 'ZILL')
for (j in c('SVAV', 'KMAZ', 'GAZA', 'OMZZ_p')){
  whitenoisecheck = rbind(whitenoisecheck, row_combine(get(j), j))
}
whitenoisecheck = as.data.frame(whitenoisecheck)
names(whitenoisecheck) = c('Ticker', 'Box-Ljung', 'p.value')
print(xtable(whitenoisecheck), file = 'whitenoise.tex', 
      floating = getOption("xtable.floating", FALSE))

#10 Search ARCH effect 
zill_arch = xtable(round(arch.test(model_zill), digits = 3))
print(zill_arch, file = 'zill_arch.tex', floating = getOption('xtable.floating', FALSE))


svav_arch = xtable(round(arch.test(model_svav), digits = 3))
print(svav_arch, file = 'svav_arch.tex', floating = getOption('xtable.floating', FALSE))

kmaz_arch = xtable(round(arch.test(model_kmaz), digits = 3))
print(kmaz_arch, file = 'kmaz_arch.tex', floating = getOption('xtable.floating', FALSE))

gaza_arch = xtable(round(arch.test(model_gaza), digits = 3))
print(gaza_arch, file = 'gaza_arch.tex', floating = getOption('xtable.floating', FALSE))

omzz_p_arch = xtable(round(arch.test(model_omzz_p), digits = 3))
print(omzz_p_arch, file = 'omzz_p_arch.tex', floating = getOption('xtable.floating', FALSE))

#11 GARCH / ARCH modeling 
acf(model_zill$residuals ** 2)
pacf(model_zill$residuals ** 2)

acf(model_svav$residuals ** 2)
pacf(model_svav$residuals ** 2)

acf(model_kmaz$residuals ** 2)
pacf(model_kmaz$residuals ** 2)

acf(model_gaza$residuals ** 2)
pacf(model_gaza$residuals ** 2)

acf(model_omzz_p$residuals ** 2)
pacf(model_omzz_p$residuals ** 2)

g_zill = garch(model_zill$residuals, order = c(1, 1))
AIC(g_zill) # -3004.358
g_svav = garch(model_svav$residuals, order = c(1, 1))
AIC(g_svav) # -4196.233
g_kmaz = garch(model_kmaz$residuals, order = c(1, 1))
AIC(g_kmaz) # -3956.576
g_gaza = garch(model_gaza$residuals, order = c(1, 1))
AIC(g_gaza) # -3818.366
g_omzz_p = garch(model_omzz_p$residuals, order = c(2, 1))
AIC(g_omzz_p) # -2686.196

#12 Ranking
ranking = rbind(
  c(round(mean((g_zill$fitted.values)[,1], na.rm = TRUE), 5), 'ZILL'),
  c(round(mean((g_svav$fitted.values)[,1], na.rm = TRUE), 5), 'SVAV'),
  c(round(mean((g_kmaz$fitted.values)[,1], na.rm = TRUE), 5), 'KMAZ'),
  c(round(mean((g_gaza$fitted.values)[,1], na.rm = TRUE), 5), 'GAZA'),
  c(round(mean((g_omzz_p$fitted.values)[,1], na.rm = TRUE), 5), 'OMZZ_p')
)
ranking = as.data.frame(ranking)
ranking = ranking[order(ranking[, 1]), ]
names(ranking) = c('Средняя волатильность', 'Тикер')
rownames(ranking) = NULL

print(xtable(ranking), file = 'ranking.tex', floating = getOption('xtable.floating', FALSE))

#13 Plots 
install.packages("fGarch")
library(fGarch)

pr_zill = garchFit(formula = ~arma(1,2) + garch(1,1), data = data2[, 'ZILL'], trace = F)
png('pr_zill.png', width = 450, height = 350, res = 100)
plot_pr_zill = predict(pr_zill, 2, plot = TRUE, crit_val = 2)
dev.off()

pr_svav = garchFit(formula = ~arma(5,0) + garch(1,1), data = data2[, 'SVAV'], trace = F)
png('pr_svav.png', width = 450, height = 350, res = 100)
plot_pr_svav = predict(pr_svav, 2, plot = TRUE, crit_val = 2)
dev.off()

pr_kmaz = garchFit(formula = ~arma(3,3) + garch(1,1), data = data2[, 'KMAZ'], trace = F)
png('pr_kmaz.png', width = 450, height = 350, res = 100)
plot_pr_kmaz = predict(pr_kmaz, 2, plot = TRUE, crit_val = 2)
dev.off()

pr_gaza = garchFit(formula = ~arma(0,1) + garch(1,1), data = data2[, 'GAZA'], trace = F)
png('pr_gaza.png', width = 450, height = 350, res = 100)
plot_pr_gaza = predict(pr_gaza, 2, plot = TRUE, crit_val = 2)
dev.off()

pr_omzz_p = garchFit(formula = ~arma(0,0) + garch(2,1), data = data2[, 'OMZZ_p'], trace = F)
png('pr_omzz_p.png', width = 450, height = 350, res = 100)
plot_pr_omzz_p = predict(pr_omzz_p, 2, plot = TRUE, crit_val = 2)
dev.off()

# The end of project 