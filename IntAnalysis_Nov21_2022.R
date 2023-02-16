library(readxl) #to import from .xlsx
library(xts) #needed to convert to xts (time series)
library(urca)#needed for ur.df
library(astsa) #needed for acf2 function
library(forecast)#auto.arima
library(TSA) #for prewhiten
library(stargazer) #Exporting to LaTeX

####Data import and cleaning####

#This has data from 3 Apr 2018 to 31 Mar 2021
alldata = read_xlsx("C:/Users/sbnidhir/OneDrive - University Of Houston/MPhil Thesis/Data/data18-21.xlsx", 
                    col_types = c("date","numeric","numeric","numeric","numeric",
                    "numeric","numeric","numeric","numeric","numeric","numeric",
                    "numeric","numeric","numeric","numeric"))
str(alldata) #exploring the structure of the data

##Converting data to time series##
all_ts = as.xts(alldata[,-1], order.by = alldata$Date)
#This data is for all business days in India. Some of these dates were holidays 
#in the USA and therefore the variables pertaining to these dates have NAs. The
#following replaces NAs with previous value
all_ts$US10yr = na.locf(all_ts$US10yr)
all_ts$EFFR = na.locf(all_ts$EFFR)
all_ts$Liquidity = all_ts$Liquidity/100000 #as number is very large, scaling down to lakhs of crores 

#Importing data on dates of auctions and announcements
dummies = read_xlsx("C:/Users/sbnidhir/OneDrive - University Of Houston/MPhil Thesis/Data/Twist_dummies.xlsx", 
                    col_types = c("date","numeric", "numeric","numeric"))
dummies_ts = as.xts(dummies[,-1], order.by = dummies$Date)

#Calculating empirical slope
slope102 = all_ts$`10yr` - all_ts$`2yr`
slope101 = all_ts$`10yr` - all_ts$`1yr`
slope103 = all_ts$`10yr` - all_ts$`3mo`

####Summary Stats####
##preintervention summary##
summary(all_ts[1:402])
summary(cbind(slope102,slope101,slope103)[1:402])
apply(all_ts[1:402],2,sd)
sd(slope102[1:402]);sd(slope101[1:402]);sd(slope103[1:402])

##intervention summary##
summary(all_ts[403:728])
summary(cbind(slope102,slope101,slope103)[403:728])
apply(all_ts[403:728],2,sd)
sd(slope102[403:728]);sd(slope101[403:728]);sd(slope103[403:728])

####Plotting data####

par(mfrow = c(2,1))
plot(all_ts$`10yr`[364:728], main = "10yr yield")
plot(all_ts$`1yr`[364:728], main = "1yr yield")

####Checking Stationarity####
#Unit roots are tested with the ADF test and the residuals from the ADF regression
#are checked to be white noise. These results are presented across two tables. 

#Checking stationarity on preintervention data
adf102 = ur.df(slope102[1:402], type = "trend", selectlags = "AIC") 
#drift is insignificant while trend is significant
summary(adf102)
acf2(adf102@res) #checks that the ADF fit results in white noise errors
#null of unit root cannot be rejected even when trend term is avoided in ADF test.
adf102 = ur.df(na.omit(diff(slope102[1:402])), type = "trend", 
               selectlags = "AIC") #stationary in first differences
summary(adf102)

adf101 = ur.df(slope101[1:402], type = "trend", selectlags = "AIC") 
#drift and trend are insignificant. but trend ADF is reported
summary(adf101) #null of unit root cannot be rejected.
acf2(adf101@res)
adf101 = ur.df(na.omit(diff(slope101[1:402])), type = "trend", 
               selectlags = "AIC") #stationary in first differences
summary(adf101)

adf103 = ur.df(slope103[1:402], type = "trend", selectlags = "AIC") 
#drift and trend are insignificant
summary(adf103) #null of unit root cannot be rejected
acf2(adf103@res)
adf103 = ur.df(na.omit(diff(slope103[1:402])), type = "trend", 
               selectlags = "AIC")
summary(adf103)

adfNSS = ur.df(-all_ts$B1[1:402], type = "trend", selectlags = "AIC") 
#drift is significant while trend is insignificant
summary(adfNSS) #null of unit root can be rejected
acf2(adfNSS@res)
adfNSS = ur.df(na.omit(diff(-all_ts$B1[1:402])), type = "trend", 
               selectlags = "AIC")
summary(adfNSS)

adfwacr = ur.df(all_ts$WACR[1:402], type = "trend", selectlags = "AIC") 
#drift and trend are both significant
summary(adfwacr) #null of unit root cannot be rejected.
acf2(adfwacr@res)
adfwacr = ur.df(na.omit(diff(all_ts$WACR[1:402])), type = "trend", 
                selectlags = "AIC") 
summary(adfwacr) #stationary at first diff. drift and trend not significant.

adfeffr = ur.df(all_ts$EFFR[1:402], type = "trend", selectlags = "AIC") 
#drift not significant, but trend is significant
summary(adfeffr) #null of unit root cannot be rejected even when 
#drift and trend are both not included.
acf2(adfeffr@res)
adfeffr = ur.df(na.omit(diff(all_ts$EFFR[1:402])), type = "trend", selectlags = "AIC")
summary(adfeffr) #stationary at first diff.

adfus10yr = ur.df(all_ts$US10yr[1:402], type = "trend", selectlags = "AIC") 
#drift and trend is significant 
summary(adfus10yr) #null of unit root cannot be rejected.
acf2(adfus10yr@res)
adfus10yr = ur.df(na.omit(diff(all_ts$US10yr[1:402])), type = "trend", 
                  selectlags = "AIC")
summary(adfus10yr) #stationary at first diff. drift and trend not significant.

adfliquidity = ur.df(all_ts$Liquidity[1:402], type = "trend", selectlags = "AIC") 
#trend is significant while drift is not significant
summary(adfliquidity) #null of unit root can be rejected even when there is drift and trend 
acf2(adfliquidity@res)#stationary at first diff (expected as stationary at levels)

adf10yr = ur.df(all_ts$`10yr`[1:402], type = "trend", selectlags = "AIC") 
#drift and trend are both significant 
summary(adf10yr) #null of unit root is rejected
acf2(adf10yr@res)
adf10yr = ur.df(na.omit(diff(all_ts$`10yr`[1:402])), type = "trend", 
               selectlags = "AIC") 
summary(adf10yr)#stationary at first diff (expected as stationary at levels)

adf1yr = ur.df(all_ts$`1yr`[1:402], type = "trend", selectlags = "AIC") 
#drift and trend are both significant
summary(adf1yr) #null of unit root cannot be rejected
acf2(adf1yr@res)
adf1yr = ur.df(na.omit(diff(all_ts$`1yr`[1:402])), type = "trend", 
               selectlags = "AIC") 
summary(adf1yr)#shows stationarity at first difference


##Checking stationarity on intervention data##
adf102 = ur.df(slope102[403:728], type = "none", selectlags = "AIC") 
#trend and drift are insignificant. null of unit root cannot be rejected
#even when trend and drift term is avoided in ADF test
summary(adf102)
acf2(adf102@res) #checks that the ADF fit results in white noise errors
adf102 = ur.df(na.omit(diff(slope102[403:728])), type = "trend", 
               selectlags = "AIC") 
summary(adf102)#stationary in first differences

adf101 = ur.df(slope101[403:728], type = "trend", selectlags = "AIC") 
#drift and trend are insignificant. but trend ADF is reported
summary(adf101) #null of unit root cannot be rejected.
acf2(adf101@res)
adf101 = ur.df(na.omit(diff(slope101[403:728])), type = "trend", 
               selectlags = "AIC") 
summary(adf101)#stationary in first differences

adf103 = ur.df(slope103[403:728], type = "trend", selectlags = "AIC") 
#drift significant, trend insignificant
summary(adf103) #null of unit root cannot be rejected
acf2(adf103@res)
adf103 = ur.df(na.omit(diff(slope103[403:728])), type = "trend", 
               selectlags = "AIC")
summary(adf103)#stationary in first differences

adfNSS = ur.df(-all_ts$B1[403:728], type = "trend", selectlags = "AIC") 
#drift is significant while trend is insignificant
summary(adfNSS) #null of unit root can be rejected
acf2(adfNSS@res)
adfNSS = ur.df(na.omit(diff(-all_ts$B1[403:728])), type = "trend", 
               selectlags = "AIC")
summary(adfNSS)#stationary in first differences(expected as stationary at levels)

adfwacr = ur.df(all_ts$WACR[403:728], type = "trend", selectlags = "AIC") 
#drift and trend are both insignificant. result same when these are dropped.
summary(adfwacr) #null of unit root cannot be rejected.
acf2(adfwacr@res)
adfwacr = ur.df(na.omit(diff(all_ts$WACR[403:728])), type = "trend", 
                selectlags = "AIC") 
summary(adfwacr)#stationary at first diff. drift and trend not significant.

adfliquidity = ur.df(all_ts$Liquidity[403:728], type = "trend", selectlags = "AIC") 
#trend is insignificant while drift is significant
summary(adfliquidity) #null of unit root can be rejected even when there is drift and trend 
acf2(adfliquidity@res)
adfliquidity = ur.df(na.omit(diff(all_ts$Liquidity[403:728])), type = "trend", 
                     selectlags = "AIC") 
summary(adfliquidity)#stationary at first diff.

adfus10yr = ur.df(all_ts$US10yr[403:728], type = "trend", selectlags = "AIC") 
#drift insignificant,trend is significant 
summary(adfus10yr) #null of unit root cannot be rejected.
acf2(adfus10yr@res)
adfus10yr = ur.df(na.omit(diff(all_ts$US10yr[403:728])), type = "trend", 
                  selectlags = "AIC") 
summary(adfus10yr)#stationary at first diff. drift and trend significant.

adfeffr = ur.df(all_ts$EFFR[403:728], type = "trend", selectlags = "AIC") 
#drift,trend not significant.
summary(adfeffr) 
#null of unit root cannot be rejected even when drift and trend are both not included.
acf2(adfeffr@res)
adfeffr = ur.df(na.omit(diff(all_ts$EFFR[403:728])), type = "trend", selectlags = "AIC")
summary(adfeffr) #stationary at first diff.

adf10yr = ur.df(all_ts$`10yr`[403:728], type = "trend", selectlags = "AIC") 
#drift and trend are insignificant 
summary(adf10yr) #null of unit root cannot be rejected
acf2(adf10yr@res)
adf10yr = ur.df(na.omit(diff(all_ts$`10yr`[403:728])), type = "trend", selectlags = "AIC") 
summary(adf10yr) #stationary at first diff.

adf1yr = ur.df(all_ts$`1yr`[403:728], type = "trend", selectlags = "AIC") 
#drift and trend are insignificant
summary(adf1yr) #null of unit root cannot be rejected
acf2(adf1yr@res)
adf1yr = ur.df(na.omit(diff(all_ts$`1yr`[403:728])), type = "trend", selectlags = "AIC") 
summary(adf1yr)#shows stationarity at first difference

####CCF generation from preintervention data####

#10-2yr CCF
auto.arima(slope102[1:402])#suggest 3,1,3
acf2(na.omit(diff(slope102[1:402]))) #ACF and PACF at 1st lag significant
ar102 = arima(slope102[1:402], order = c(0,1,1)) 
ar102
acf2(ar102$residuals)

par(mfrow = c(2,2), mai = c(0.7,0.7,0.5,0.1))
fitwhite = residuals(Arima(slope102[1:402], model = ar102))
fitwhite1 = residuals(Arima(all_ts$WACR[1:402], model = ar102))
ccf(fitwhite1,fitwhite, ylab = "CCF", xlab = "", main = "10yr-2yr ~ WACR", lag.max = 15)
fitwhite1 = residuals(Arima(all_ts$Liquidity[1:402], model = ar102))
ccf(fitwhite1,fitwhite, ylab = "", xlab = "", main = "10yr-2yr ~ Liquidity", lag.max = 15)
fitwhite1 = residuals(Arima(all_ts$EFFR[1:402], model = ar102))
ccf(fitwhite1,fitwhite, ylab = "CCF", main = "10yr-2yr ~ EFFR", lag.max = 15)
fitwhite1 = residuals(Arima(all_ts$US10yr[1:402], model = ar102))
ccf(fitwhite1,fitwhite, ylab = "", main = "10yr-2yr ~ US10yr", lag.max = 15)


#10-1yr CCF
auto.arima(slope101[1:402])#suggest 3,1,3
acf2(na.omit(diff(slope101[1:402]))) #ACF and PACF at 1st lag significant
ar101 = arima(slope102[1:402], order = c(0,1,1)) #This is better than AR1. ARMA11 makes both insignificant.
ar101
acf2(ar101$residuals)

par(mfrow = c(2,2), mai = c(0.7,0.7,0.5,0.1))
fitwhite = residuals(Arima(slope102[1:402], model = ar101))
fitwhite1 = residuals(Arima(all_ts$WACR[1:402], model = ar101))
ccf(fitwhite1,fitwhite, ylab = "CCF", xlab = "", main = "10yr-1yr ~ WACR", lag.max = 15)
fitwhite1 = residuals(Arima(all_ts$Liquidity[1:402], model = ar101))
ccf(fitwhite1,fitwhite, ylab = "", xlab = "", main = "10yr-1yr ~ Liquidity", lag.max = 15)
fitwhite1 = residuals(Arima(all_ts$EFFR[1:402], model = ar101))
ccf(fitwhite1,fitwhite, ylab = "CCF", main = "10yr-1yr ~ EFFR", lag.max = 15)
fitwhite1 = residuals(Arima(all_ts$US10yr[1:402], model = ar101))
ccf(fitwhite1,fitwhite, ylab = "", main = "10yr-1yr ~ US10yr", lag.max = 15)

#10-3mo CCF
auto.arima(slope103[1:402])#suggest 3,1,2
acf2(na.omit(diff(slope103[1:402]))) #ACF and PACF at 2nd lag significant
ar103 = arima(slope103[1:402], order = c(2,1,2)) #This is better than auto.arima model.
ar103
acf2(ar103$residuals)

par(mfrow = c(2,2), mai = c(0.7,0.7,0.5,0.1))
fitwhite = residuals(Arima(slope103[1:402], model = ar103))
fitwhite1 = residuals(Arima(all_ts$WACR[1:402], model = ar103))
ccf(fitwhite1,fitwhite, ylab = "CCF", xlab = "", main = "10yr-3mo ~ WACR", lag.max = 15)
fitwhite1 = residuals(Arima(all_ts$Liquidity[1:402], model = ar103))
ccf(fitwhite1,fitwhite, ylab = "", xlab = "", main = "10yr-3mo ~ Liquidity", lag.max = 15)
fitwhite1 = residuals(Arima(all_ts$EFFR[1:402], model = ar103))
ccf(fitwhite1,fitwhite, ylab = "CCF", main = "10yr-3mo ~ EFFR", lag.max = 15)
fitwhite1 = residuals(Arima(all_ts$US10yr[1:402], model = ar103))
ccf(fitwhite1,fitwhite, ylab = "", main = "10yr-3mo ~ US10yr", lag.max = 15)

#NSS slope CCF
auto.arima(-all_ts$B1[1:402])#suggest 2,1,4
acf2(na.omit(diff(-all_ts$B1[1:402]))) #ACF at 1,2,4,6,12 and PACF at 1,4,5,6,7,12 lag significant
arb1 = arima(-all_ts$B1[1:402], order = c(2,1,4))
arb1
acf2(arb1$residuals)


par(mfrow = c(2,2), mai = c(0.7,0.7,0.5,0.1))
fitwhite = residuals(Arima(-all_ts$B1[1:402], model = arb1))
fitwhite1 = residuals(Arima(all_ts$WACR[1:402], model = arb1))
ccf(fitwhite1,fitwhite, ylab = "CCF", xlab = "", main = "NSS slope ~ WACR", lag.max = 15)
fitwhite1 = residuals(Arima(all_ts$Liquidity[1:402], model = arb1))
ccf(fitwhite1,fitwhite, ylab = "", xlab = "", main = "NSS slope ~ Liquidity", lag.max = 15)
fitwhite1 = residuals(Arima(all_ts$EFFR[1:402], model = arb1))
ccf(fitwhite1,fitwhite, ylab = "CCF", main = "NSS slope ~ EFFR", lag.max = 15)
fitwhite1 = residuals(Arima(all_ts$US10yr[1:402], model = arb1))
ccf(fitwhite1,fitwhite, ylab = "", main = "NSS slope ~ US10yr", lag.max = 15)


#10yr CCF
auto.arima(all_ts$`10yr`[1:402])#suggest 3,1,3. but other models I try have lower AIC and BIC
acf2(na.omit(diff(all_ts$`10yr`[1:402]))) #ACF and PACF at 1st and 2nd and 8th lag significant
ar10yr = arima(all_ts$`10yr`[1:402], order = c(2,1,0), fixed = c(0,NA))
#As AR1 term is insignificant, only 2nd lag is included. 1st lag forced to 0. 
ar10yr
acf2(ar10yr$residuals)

par(mfrow = c(2,2), mai = c(0.7,0.7,0.5,0.1))
fitwhite = residuals(Arima(all_ts$`10yr`[1:402], model = ar10yr))
fitwhite1 = residuals(Arima(all_ts$WACR[1:402], model = ar10yr))
ccf(fitwhite1,fitwhite, ylab = "CCF",xlab = "", main = "10yr ~ WACR",lag.max = 15)
fitwhite1 = residuals(Arima(all_ts$Liquidity[1:402], model = ar10yr))
ccf(fitwhite1,fitwhite, ylab = "", xlab = "", main = "10yr ~ Liquidity",lag.max = 15)
fitwhite1 = residuals(Arima(all_ts$EFFR[1:402], model = ar10yr))
ccf(fitwhite1,fitwhite,ylab = "CCF",  main = "10yr ~ EFFR",lag.max = 15)
fitwhite1 = residuals(Arima(all_ts$US10yr[1:402], model = ar10yr))
ccf(fitwhite1,fitwhite,ylab = "", main = "1yr ~ US10yr",lag.max = 15)

#The code now uses prewhiten function from TSA package. CCF similar to ccf function.
#magnitudes are slightly different, but significance and direction are the same.
#But prewhiten throws up an error on the model used in 1yr yield.
#prewhiten(y = all_ts$`10yr`,x = all_ts$WACR, x.model = ar10yr,lag.max = 15, main = "10yr ~ WACR",xlab = "")
#prewhiten(y = all_ts$`10yr`,x=all_ts$US10yr, x.model = ar10yr, lag.max = 15, main = "10yr ~ US 10yr",xlab = "", ylab = "")
#prewhiten(y = all_ts$`10yr`[1:402],x =all_ts$EFFR[1:402],lag.max = 15, x.model = ar10yr, main = "10yr ~ EFFR")
#prewhiten(y = all_ts$`10yr`[1:402],x=all_ts$Liquidity[1:402], x.model = ar10yr,lag.max = 15, main = "10yr ~ Liquidity", ylab = "")

#1yr CCF
auto.arima(na.omit(diff(all_ts$`1yr`[1:402])))#suggest 1,2,2
acf2(na.omit(diff(all_ts$`1yr`[1:402]))) #ACF and PACF at 6th lag significant
ar1yr = arima(all_ts$`1yr`[1:402], order = c(1,1,1),seasonal = list(order = c(1,0,0), period = 6),fixed = c(0,NA,NA)) 
ar1yr
acf2(ar1yr$residuals)

par(mfrow = c(2,2), mai = c(0.7,0.7,0.5,0.1))
fitwhite = residuals(Arima(all_ts$`1yr`[1:402], model = ar1yr))
fitwhite1 = residuals(Arima(all_ts$WACR[1:402], model = ar1yr))
ccf(fitwhite1,fitwhite, ylab = "CCF",xlab = "", main = "1yr ~ WACR",lag.max = 15)
fitwhite1 = residuals(Arima(all_ts$Liquidity[1:402], model = ar1yr))
ccf(fitwhite1,fitwhite, ylab = "", xlab = "", main = "1yr ~ Liquidity",lag.max = 15)
fitwhite1 = residuals(Arima(all_ts$EFFR[1:402], model = ar1yr))
ccf(fitwhite1,fitwhite,ylab = "CCF",  main = "1yr ~ EFFR",lag.max = 15)
fitwhite1 = residuals(Arima(all_ts$US10yr[1:402], model = ar1yr))
ccf(fitwhite1,fitwhite,ylab = "", main = "1yr ~ US10yr",lag.max = 15)

####Identifying transfer function models####

##Running OLS with covariates and then determining model from residuals##

#10-2 slope transfer function model
reg102 = lm(slope102[4:402] ~ all_ts$Liquidity[4:402] + all_ts$EFFR[1:399])# + all_ts$US10yr[3:401] )
summary(reg102)
acf2(diff(reg102$residuals))
auto.arima(reg102$residuals)#suggest ARIMA 3,1,0. ARIMA(1,1,1) when liquidity is used
arima(reg102$residuals, order = c(1,1,0))#AR2 is better than auto.arima suggested. I use 1,1,0 as AR2 turns
#insignificant in ARIMAX model. AR2 is insignificant when using liquidity

model102 = arima(slope102[4:402], order = c(1,1,0), xreg = cbind(all_ts$Liquidity[4:402], na.omit(zlag(all_ts$EFFR[1:402],3)))) #either liq or WACR can be used. We use WACR
model102 #US10yr and WACR not significant. 

#10-1 slope transfer function model
#Below WACR(-2), Liq(1,-3), EFFR(-11), US(-6)
reg101 = lm(slope101[12:402] ~ all_ts$WACR[10:400] + all_ts$Liquidity[12:402] + 
              all_ts$Liquidity[9:399] + all_ts$EFFR[1:391] + all_ts$US10yr[6:396]  )
summary(reg101)
acf2(diff(reg101$residuals))
auto.arima(reg101$residuals)#suggest ARIMA 3,1,0
arima(reg101$residuals, order = c(2,1,0)) #this is not better than auto.arima

model101 = arima(slope101[12:402], order = c(1,1,0), xreg = cbind(na.omit(zlag(all_ts$WACR[10:402],2)),
                                                                  all_ts$Liquidity[12:402], na.omit(zlag(all_ts$EFFR[1:402],11)), na.omit(zlag(all_ts$US10yr[6:402],6)) ))
model101 #The transfer function model identified

#10-3 slope transfer function model
#WACR(-2,-7,-14), Liq, EFFR(-3), US(-6,-8)
reg103 = lm(slope103[15:402] ~ all_ts$WACR[13:400] + all_ts$WACR[8:395] + all_ts$WACR[1:388] 
            +all_ts$EFFR[12:399] +all_ts$Liquidity[15:402] + all_ts$US10yr[9:396]+ all_ts$US10yr[7:394] )
summary(reg103)
acf2(diff(reg103$residuals))
auto.arima(reg103$residuals)#suggest ARIMA 1,0,1. but I use 0,0,1 for model 103 as AR turns up insignificant
arima(reg103$residuals, order = c(0,1,1)) #auto.arima shows stationary with zero mean
#covariates for below is WACR(-2), EFFR(-3), liquidity, US10yr(-6). So liq from 2, diff slope from 1,  
model103 = arima(slope103[7:402], order = c(0,1,1), xreg = cbind( na.omit(zlag(all_ts$WACR[5:402],2)), 
                                                                  na.omit(zlag(all_ts$EFFR[4:402],3)), all_ts$Liquidity[7:402], 
                                                                  na.omit(zlag(all_ts$US10yr[1:402],6)) ) ) #na.omit(zlag(all_ts$US10yr[7:402],8)) na.omit(zlag(all_ts$WACR[8:402],7)),  na.omit(zlag(all_ts$WACR[1:402],14))
model103 # all significant

#NSS slope transfer function model
#WACR(0,-1,-2,-4,-6,-7), liq, US10yr
regNSS = lm(-all_ts$B1[8:402] ~ all_ts$WACR[8:402] + all_ts$WACR[7:401] + all_ts$WACR[6:400]
            + all_ts$WACR[4:398] + all_ts$WACR[2:396]+ all_ts$WACR[1:395] + all_ts$Liquidity[8:402])# + all_ts$US10yr[8:402])
summary(regNSS)
acf2(regNSS$residuals)
auto.arima(regNSS$residuals)#suggest ARIMA(1,1,0)
arima(regNSS$residuals, order = c(1,1,0)) #auto.arima gives better fit
modelNSS = arima(-all_ts$B1[8:402], order = c(1,1,0), xreg = cbind( all_ts$WACR[8:402],
                                                                    na.omit(zlag(all_ts$WACR[7:402],1)), na.omit(zlag(all_ts$WACR[6:402],2)), na.omit(zlag(all_ts$WACR[4:402],4)),
                                                                    na.omit(zlag(all_ts$WACR[2:402],6)),  na.omit(zlag(all_ts$WACR[1:402],7)), all_ts$Liquidity[8:402]) )

modelNSS

#10yr yield transfer function model
reg10 = lm(all_ts$`10yr`[1:402] ~ all_ts$Liquidity[1:402] + all_ts$US10yr[1:402])
summary(reg10)
acf2(diff(reg102$residuals))
auto.arima(reg102$residuals)#suggest ARIMA 1,1,1.
arima(reg102$residuals, order = c(0,1,1))#MA1 is better than auto.arima suggested.
model10 = arima(all_ts$`10yr`[1:402], order = c(0,1,1), xreg = cbind(all_ts$Liquidity[1:402],all_ts$US10yr[1:402])) 
model10 

#1yr yield transfer function model
reg1 = lm(all_ts$`1yr`[12:402] ~ all_ts$Liquidity[12:402] + all_ts$EFFR[1:391])
summary(reg1)
acf2(diff(reg1$residuals))
auto.arima(reg1$residuals)#suggest ARIMA 1,1,1. Still best when checked against AR1 and MA1
arima(reg1$residuals, order = c(0,1,1))
model1 = arima(all_ts$`1yr`[12:402], order = c(0,1,1), xreg = cbind(all_ts$Liquidity[12:402],na.omit(zlag(all_ts$EFFR[1:402],11)) ) ) 
model1 

##Exporting as tables to LaTeX##
tab_head = c("10yr - 2yr","10yr - 1yr","10yr - 3mo", "NSS slope")
writeClipboard(stargazer(model102,model101, model103, modelNSS, df=TRUE, out.header = FALSE,header = FALSE,
                         title = "Modelling preintervention data", dep.var.labels  = tab_head, align = TRUE))
tab_head_y = c("10yr","1yr")
writeClipboard(stargazer(model10,model1, df=TRUE, out.header = FALSE,header = FALSE,
                         title = "Modelling preintervention data", dep.var.labels  = tab_head_y, align = TRUE))

#####Intervention Analysis#####

#10-2slope int analysis
int102comb = arimax(na.omit(diff(slope102[402:728])),order = c(1,0,0),include.mean = F, 
                    xreg = cbind( na.omit(diff(all_ts$Liquidity[402:728])), na.omit(zlag(diff(all_ts$EFFR[399:728]),3))),
                    xtransf = cbind(dummies_ts$AuctionD,dummies_ts$AnnounceD), transfer = list(c(0,0),c(0,0)) )
int102comb #note that arima and arimax functions give the same thing

#10-1slope int analysis
int101comb = arimax(na.omit(diff(slope101[402:728])),order = c(1,0,0),include.mean = F, xreg = cbind(na.omit(diff(zlag(all_ts$WACR[400:728],2))), na.omit(diff(all_ts$Liquidity[402:728])), na.omit(diff(zlag(all_ts$EFFR[391:728],11))), na.omit(diff(zlag(all_ts$US10yr[396:728],6))) ),
                    xtransf = cbind(dummies_ts$AuctionD,dummies_ts$AnnounceD), transfer = list(c(0,0),c(0,0)) )
int101comb #Intervention analysis results

#10-3slope int analysis
int103comb = arimax(na.omit(diff(slope103[402:728])), order = c(0,0,1),include.mean = F, 
                    xreg = cbind(dWACR_2 = na.omit(diff(zlag(all_ts$WACR[400:728],2))),
                                 dEFFR_3 = na.omit(diff(zlag(all_ts$EFFR[399:728],3))), dUS10yr_6 = na.omit(diff(zlag(all_ts$US10yr[396:728],6))),
                                 na.omit(diff(all_ts$Liquidity[402:728])) ), xtransf =  cbind(dummies_ts$AnnounceD,dummies_ts$AuctionD), 
                    transfer = list(c(0,0),c(0,0)) )
int103comb

#NSS slope int analysis
intNSScomb = arimax(na.omit(diff(-all_ts$B1[402:728])),order = c(1,0,0),include.mean = F,
                    xreg = cbind( na.omit(diff(all_ts$WACR[402:728])), na.omit(diff(zlag(all_ts$WACR[401:728],1))),
                                  na.omit(diff(zlag(all_ts$WACR[400:728],2))), na.omit(diff(zlag(all_ts$WACR[398:728],4))),
                                  na.omit(diff(zlag(all_ts$WACR[396:728],6))),  na.omit(diff(zlag(all_ts$WACR[395:728],7))), 
                                  na.omit(diff(all_ts$Liquidity[402:728])) ),
                    xtransf = cbind(dummies_ts$AnnounceD, dummies_ts$AuctionD), transfer = list(c(0,0), c(0,0)) )
intNSScomb

#10yr yield int analysis
int10comb = arimax(na.omit(diff(all_ts$`10yr`[402:728])),order = c(0,0,1),include.mean = F, 
                   xreg = cbind( na.omit(diff(all_ts$Liquidity[402:728])), na.omit(diff(all_ts$US10yr[402:728])) ),
                   xtransf = cbind(dummies_ts$AuctionD,dummies_ts$AnnounceD), transfer = list(c(0,0),c(0,0)) )
int10comb

#1yr yield int analysis
int1comb = arimax(na.omit(diff(all_ts$`1yr`[402:728])),order = c(0,0,1),include.mean = F, 
                  xreg = cbind(dLiq = na.omit(diff(all_ts$Liquidity[402:728])),dEFFR_11 = na.omit(diff(zlag(all_ts$WACR[391:728],11))) ),
                  xtransf = cbind(dummies_ts$AuctionD,dummies_ts$AnnounceD), transfer = list(c(0,0),c(0,0)) )
int1comb

###Exporting to Latex Table
##there is an error with both arima and arimax here. Table manually created
writeClipboard(stargazer(int102comb,int101comb, int103comb, intNSScomb, out.header = FALSE,header = FALSE,
                         title = "Impact of Special OMOs on slopes", dep.var.labels  = tab_head, align = TRUE))
writeClipboard(stargazer(int10comb,int1comb, out.header = FALSE,header = FALSE,
                         title = "Impact of Special OMOs on yields", dep.var.labels  = tab_head_y, align = TRUE))



