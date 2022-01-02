rm(list=ls())

setwd("C://Users//frank//Desktop//ECO8086//TP2")

############################################################################
########################### QUESTION 1 #####################################

# For block 1
# Log of Industrial production LIP
# unemployment rate            UNEMP
# log of CPI                   LCPI
# log of commidity price index LPCOM

# For block 2
# Fereral funds rate           FFR
# Shadow Federal funds rate    SR

# For block 3
# log of nonborrowed reserves  LNBR
# log of total reserves        LTR
# log of M1                    LM1

library(readxl)

# ----importing the data from csv--------
dt.M <- read_excel('Data.Q1.xlsx')    #MONTHLY DATA

summary(dt.M)

#we have some missing data in our data set. In order to impute those missing
# data, we are going to use an interative PCA with 2 principal components.
library(missMDA)
library(FactoMineR)
impute = imputePCA(dt.M[,2:10], ncp=2)
dt.M = cbind(dt.M[,1], impute$completeObs)
write.table(dt.M, file = "Q1.csv")

LIP.ts = ts(dt.M$LIP, start = c(1965,1), end = c(2020,6), frequency = 12)
UNEMP.ts = ts(dt.M$UNEMP, start = c(1965,1), end = c(2020,6), frequency = 12)
LCPI.ts = ts(dt.M$LCPI, start = c(1965,1), end = c(2020,6), frequency = 12)
LPCOM.ts = ts(dt.M$LPCOM, start = c(1965,1), end = c(2020,6), frequency = 12)
FFR.ts = ts(dt.M$FFR, start = c(1965,1), end = c(2020,6), frequency = 12)
LM1.ts = ts(dt.M$LM1, start = c(1965,1), end = c(2020,6), frequency = 12)
LNBR.ts = ts(dt.M$LNBR, start = c(1965,1), end = c(2020,6), frequency = 12)
LTR.ts = ts(dt.M$LTR, start = c(1965,1), end = c(2020,6), frequency = 12)
SR.ts = ts(dt.M$SR, start = c(1965,1), end = c(2020,6), frequency = 12)

# lets arrange our data into blocks like mentioned earlier
n1 = cbind(LIP.ts, UNEMP.ts, LPCOM.ts, LCPI.ts)
n2 = as.matrix(FFR.ts)
n3 = cbind(LM1.ts, LNBR.ts, LTR.ts)

X = cbind(n1,n2,n3)
colnames(X) = c("LIP", "UNEMP", "LPCOM", "LCPI", "FFR", "LM1", "LNBR", "LTR")

# lets create the restrictions like CEE(1999)
Amat = diag(8)
Amat[1,1] = NA
Amat[2,1] = NA
Amat[2,2] = NA
Amat[3,1] = NA
Amat[3,2] = NA
Amat[3,3] = NA
Amat[4,1] = NA
Amat[4,2] = NA
Amat[4,3] = NA
Amat[4,4] = NA
Amat[5,1] = NA
Amat[5,2] = NA
Amat[5,3] = NA
Amat[5,4] = NA
Amat[5,5] = NA
Amat[6,1] = NA
Amat[6,2] = NA
Amat[6,3] = NA
Amat[6,4] = NA
Amat[6,5] = NA
Amat[6,6] = NA
Amat[7,1] = NA
Amat[7,2] = NA
Amat[7,3] = NA
Amat[7,4] = NA
Amat[7,5] = NA
Amat[7,6] = NA
Amat[7,7] = NA
Amat[8,1] = NA
Amat[8,2] = NA
Amat[8,3] = NA
Amat[8,4] = NA
Amat[8,5] = NA
Amat[8,6] = NA
Amat[8,7] = NA
Amat[8,8] = NA


# lag order slection
library(vars)
lagselect = VARselect(X, lag.max = 10, type = "both")
lagselect
# SC chooses 2 lags

# lets estimate the model
VAR = VAR(X, p = 3, season = NULL, exog = NULL, type = "const")
summary(VAR)

SVAR = SVAR(VAR, Amat = Amat, Bmat = NULL, hessian = T, estmethod = c("scoring", "direct"))
summary(SVAR)

# Impulse Response Functions
IRF1 = irf(SVAR, impulse = "FFR", response = "LIP", n.ahead = 50, ci = 0.90)
plot(IRF1)
IRF2 = irf(SVAR, impulse = "FFR", response = "UNEMP", n.ahead = 50, ci = 0.90)
plot(IRF2)
IRF3 = irf(SVAR, impulse = "FFR", response = "LPCOM", n.ahead = 50, ci = 0.90)
plot(IRF3)
IRF4 = irf(SVAR, impulse = "FFR", response = "LCPI", n.ahead = 50, ci = 0.90)
plot(IRF4)
IRF5 = irf(SVAR, impulse = "FFR", response = "FFR", n.ahead = 50, ci = 0.90)
plot(IRF5)
IRF6 = irf(SVAR, impulse = "FFR", response = "LM1", n.ahead = 50, ci = 0.90)
plot(IRF6)
IRF7 = irf(SVAR, impulse = "FFR", response = "LNBR", n.ahead = 50, ci = 0.90)
plot(IRF7)
IRF8 = irf(SVAR, impulse = "FFR", response = "LTR", n.ahead = 50, ci = 0.90)
plot(IRF8)

# Variance decomposition
VD = fevd(SVAR, n.ahead = 10)
plot(VD)


# lets split our data into 3 samples
X.1965 = X[1:216,]      # 1965,1 to 1982,12
X.1983.1 = X[217:516,]   # 1983,1 to 2007,12
X.1983.2 = X[217:666,]   # 1983,1 to 2020,6

# lets estimate the model for 1965
VAR.1965 = VAR(X.1965, p = 2, season = NULL, exog = NULL, type = "const")
summary(VAR.1965)

SVAR.1965 = SVAR(VAR.1965, Amat = Amat, Bmat = NULL, hessian = T, estmethod = c("scoring", "direct"))
SVAR.1965

# Impulse Response Functions
IRF1.1965 = irf(SVAR.1965, impulse = "FFR", response = "LIP", n.ahead = 50, ci = 0.90)
plot(IRF1.1965)
IRF2.1965 = irf(SVAR.1965, impulse = "FFR", response = "UNEMP", n.ahead = 50, ci = 0.90)
plot(IRF2.1965)
IRF3.1965 = irf(SVAR.1965, impulse = "FFR", response = "LPCOM", n.ahead = 50, ci = 0.90)
plot(IRF3.1965)
IRF4.1965 = irf(SVAR.1965, impulse = "FFR", response = "LCPI", n.ahead = 50, ci = 0.90)
plot(IRF4.1965)
IRF5.1965 = irf(SVAR.1965, impulse = "FFR", response = "FFR", n.ahead = 50, ci = 0.90)
plot(IRF5.1965)
IRF6.1965 = irf(SVAR.1965, impulse = "FFR", response = "LM1", n.ahead = 50, ci = 0.90)
plot(IRF6.1965)
IRF7.1965 = irf(SVAR.1965, impulse = "FFR", response = "LNBR", n.ahead = 50, ci = 0.90)
plot(IRF7.1965)
IRF8.1965 = irf(SVAR.1965, impulse = "FFR", response = "LTR", n.ahead = 50, ci = 0.90)
plot(IRF8.1965)

# Variance decomposition
VD.1965 = fevd(SVAR.1965, n.ahead = 10)
plot(VD.1965)


# lets estimate the model for 1983.1
VAR.1983.1 = VAR(X.1983.1, p = 2, season = NULL, exog = NULL, type = "const")
summary(VAR.1983.1)

SVAR.1983.1 = SVAR(VAR.1983.1, Amat = Amat, Bmat = NULL, hessian = T, estmethod = c("scoring", "direct"))
SVAR.1983.1

# Impulse Response Functions
IRF1.1983.1 = irf(SVAR.1983.1, impulse = "FFR", response = "LIP", n.ahead = 50, ci = 0.90)
plot(IRF1.1983.1)
IRF2.1983.1 = irf(SVAR.1983.1, impulse = "FFR", response = "UNEMP", n.ahead = 50, ci = 0.90)
plot(IRF2.1983.1)
IRF3.1983.1 = irf(SVAR.1983.1, impulse = "FFR", response = "LPCOM", n.ahead = 50, ci = 0.90)
plot(IRF3.1983.1)
IRF4.1983.1 = irf(SVAR.1983.1, impulse = "FFR", response = "LCPI", n.ahead = 50, ci = 0.90)
plot(IRF4.1983.1)
IRF5.1983.1 = irf(SVAR.1983.1, impulse = "FFR", response = "FFR", n.ahead = 50, ci = 0.90)
plot(IRF5.1983.1)
IRF6.1983.1 = irf(SVAR.1983.1, impulse = "FFR", response = "LM1", n.ahead = 50, ci = 0.90)
plot(IRF6.1983.1)
IRF7.1983.1 = irf(SVAR.1983.1, impulse = "FFR", response = "LNBR", n.ahead = 50, ci = 0.90)
plot(IRF7.1983.1)
IRF8.1983.1 = irf(SVAR.1983.1, impulse = "FFR", response = "LTR", n.ahead = 50, ci = 0.90)
plot(IRF8.1983.1)

# Variance decomposition
VD.1983.1 = fevd(SVAR.1983.1, n.ahead = 10)
plot(VD.1983.1)


# lets estimate the model for 1983.2
VAR.1983.2 = VAR(X.1983.2, p = 2, season = NULL, exog = NULL, type = "const")
summary(VAR.1983.2)

SVAR.1983.2 = SVAR(VAR.1983.2, Amat = Amat, Bmat = NULL, hessian = T, estmethod = c("scoring", "direct"))
SVAR.1983.2

# Impulse Response Functions
IRF1.1983.2 = irf(SVAR.1983.2, impulse = "FFR", response = "LIP", n.ahead = 50, ci = 0.90)
plot(IRF1.1983.2)
IRF2.1983.2 = irf(SVAR.1983.2, impulse = "FFR", response = "UNEMP", n.ahead = 50, ci = 0.90)
plot(IRF2.1983.2)
IRF3.1983.2 = irf(SVAR.1983.2, impulse = "FFR", response = "LPCOM", n.ahead = 50, ci = 0.90)
plot(IRF3.1983.2)
IRF4.1983.2 = irf(SVAR.1983.2, impulse = "FFR", response = "LCPI", n.ahead = 50, ci = 0.90)
plot(IRF4.1983.2)
IRF5.1983.2 = irf(SVAR.1983.2, impulse = "FFR", response = "FFR", n.ahead = 50, ci = 0.90)
plot(IRF5.1983.2)
IRF6.1983.2 = irf(SVAR.1983.2, impulse = "FFR", response = "LM1", n.ahead = 50, ci = 0.90)
plot(IRF6.1983.2)
IRF7.1983.2 = irf(SVAR.1983.2, impulse = "FFR", response = "LNBR", n.ahead = 50, ci = 0.90)
plot(IRF7.1983.2)
IRF8.1983.2 = irf(SVAR.1983.2, impulse = "FFR", response = "LTR", n.ahead = 50, ci = 0.90)
plot(IRF8.1983.2)

# Variance decomposition
VD.1983.2 = fevd(SVAR.1983.2, n.ahead = 10)
plot(VD.1983.2)


# lets estimate the same models except now, lets replace FFR with SR
n2.2 = as.matrix(SR.ts)

X.2 = cbind(n1,n2.2,n3)
colnames(X.2) = c("LIP", "UNEMP", "LPCOM", "LCPI", "SR", "LM1", "LNBR", "LTR")

X.1983.2.2 = X.2[217:666,]   # 1983,1 to 2020,6

# lets estimate the model for 1983.2 with the shadow rate
VAR.1983.2.2 = VAR(X.1983.2.2, p = 2, season = NULL, exog = NULL, type = "const")
summary(VAR.1983.2.2)

SVAR.1983.2.2 = SVAR(VAR.1983.2.2, Amat = Amat, Bmat = NULL, hessian = T, estmethod = c("scoring", "direct"))
SVAR.1983.2.2

# Impulse Response Functions
IRF1.1983.2.2 = irf(SVAR.1983.2.2, impulse = "SR", response = "LIP", n.ahead = 50, ci = 0.90)
plot(IRF1.1983.2.2)
IRF2.1983.2.2 = irf(SVAR.1983.2.2, impulse = "SR", response = "UNEMP", n.ahead = 50, ci = 0.90)
plot(IRF2.1983.2.2)
IRF3.1983.2.2 = irf(SVAR.1983.2.2, impulse = "SR", response = "LPCOM", n.ahead = 50, ci = 0.90)
plot(IRF3.1983.2.2)
IRF4.1983.2.2 = irf(SVAR.1983.2.2, impulse = "SR", response = "LCPI", n.ahead = 50, ci = 0.90)
plot(IRF4.1983.2.2)
IRF5.1983.2.2 = irf(SVAR.1983.2.2, impulse = "SR", response = "FFR", n.ahead = 50, ci = 0.90)
plot(IRF5.1983.2.2)
IRF6.1983.2.2 = irf(SVAR.1983.2.2, impulse = "SR", response = "LM1", n.ahead = 50, ci = 0.90)
plot(IRF6.1983.2.2)
IRF7.1983.2.2 = irf(SVAR.1983.2.2, impulse = "SR", response = "LNBR", n.ahead = 50, ci = 0.90)
plot(IRF7.1983.2.2)
IRF8.1983.2.2 = irf(SVAR.1983.2.2, impulse = "SR", response = "LTR", n.ahead = 50, ci = 0.90)
plot(IRF8.1983.2.2)

# Variance decomposition
VD.1983.2.2 = fevd(SVAR.1983.2.2, n.ahead = 10)
plot(VD.1983.2.2)


############################################################################
########################### QUESTION 2 #####################################

# ----importing the data from csv--------
dt.Q <- read_excel('Data_General_Gvt.xlsx')    #QUARTERLY DATA


library(lubridate)
# ---- creating the date
dt.Q$...1 <- as.Date(dt.Q$...1,"%Y/%m/%d")
class(dt.Q$...1)

# ----- lets rename the date variable in our data set
names(dt.Q)[1] = "Date"

# lets create a new data frame where our data set starts at the 1st quarter 
# of 1960 and ends at the 3rd quarter of 2015
data = dt.Q[53:275,]

# lets create goverment spending and add it to our new data frame
G = data$wage+data$durable+data$nondurable+data$service+data$structure+data$equip+data$intel
data = cbind(data,G)

# lets do the same but for taxes now
T = data$receipt - data$transfer - data$interest_pay - data$subsidy
data = cbind(data,T)

# lets create our data
data$G.n = ((data$G/data$deflator)*1000000000.0*100.0)/(as.numeric(data$pop)*1000.0)
data$T.n = ((data$T/data$deflator)*1000000000.0*100.0)/(as.numeric(data$pop)*1000.0)
Y.n = data$gdp*1000000000/(as.numeric(data$pop)*1000)
data = cbind(data,Y.n)

# lets create our data into time series
G.ts = ts(data$G.n, start = c(1960,1), end = c(2015,3), frequency = 4)
T.ts = ts(data$T.n, start = c(1960,1), end = c(2015,3), frequency = 4)
Y.ts = ts(data$Y.n, start = c(1960,1), end = c(2015,3), frequency = 4)

# lets put our data into log
l.G.ts = log(G.ts)
l.T.ts = log(T.ts)
l.Y.ts = log(Y.ts)

# lets store our new variables inside a vector
Xi = cbind(l.G.ts, l.T.ts, l.Y.ts)
colnames(Xi) = c("G", "T", "Y")

Xii = cbind(l.T.ts, l.G.ts, l.Y.ts)
colnames(Xii) = c("T", "G", "Y")

Xiii = cbind(l.Y.ts, l.G.ts, l.T.ts)
colnames(Xiii) = c("Y", "G", "T")

Amat = diag(3)
Amat[1,1] = NA
Amat[2,1] = NA
Amat[2,2] = NA
Amat[3,1] = NA
Amat[3,2] = NA
Amat[3,3] = NA
Amat

library(vars)
# lets estimate the model VAR(1)
VARi = VAR(Xi, p = 1, season = NULL, exog = NULL, type = "const")
summary(VARi)

SVARi = SVAR(VARi, Amat = Amat, Bmat = NULL, hessian = T, estmethod = c("scoring", "direct"))
SVARi

# Impulse Response Functions
IRF1i = irf(SVARi, impulse = "G", response = "G", n.ahead = 20, ci = 0.90)
plot(IRF1i)
IRF2i = irf(SVARi, impulse = "G", response = "T", n.ahead = 20, ci = 0.90)
plot(IRF2i)
IRF3i = irf(SVARi, impulse = "G", response = "Y", n.ahead = 20, ci = 0.90)
plot(IRF3i)
IRF4i = irf(SVARi, impulse = "T", response = "G", n.ahead = 20, ci = 0.90)
plot(IRF4i)
IRF5i = irf(SVARi, impulse = "T", response = "T", n.ahead = 20, ci = 0.90)
plot(IRF5i)
IRF6i = irf(SVARi, impulse = "T", response = "Y", n.ahead = 20, ci = 0.90)
plot(IRF6i)

# Variance decomposition
VDi = fevd(SVARi, n.ahead = 10)
plot(VDi)


# lets estimate the model
VARii = VAR(Xii, p = 1, season = NULL, exog = NULL, type = "const")
summary(VARii)

SVARii = SVAR(VARii, Amat = Amat, Bmat = NULL, hessian = T, estmethod = c("scoring", "direct"))
SVARii

# Impulse Response Functions
IRF1ii = irf(SVARii, impulse = "G", response = "G", n.ahead = 20, ci = 0.90)
plot(IRF1ii)
IRF2ii = irf(SVARii, impulse = "G", response = "T", n.ahead = 20, ci = 0.90)
plot(IRF2ii)
IRF3ii = irf(SVARii, impulse = "G", response = "Y", n.ahead = 20, ci = 0.90)
plot(IRF3ii)
IRF4ii = irf(SVARii, impulse = "T", response = "G", n.ahead = 20, ci = 0.90)
plot(IRF4ii)
IRF5ii = irf(SVARii, impulse = "T", response = "T", n.ahead = 20, ci = 0.90)
plot(IRF5ii)
IRF6ii = irf(SVARii, impulse = "T", response = "Y", n.ahead = 20, ci = 0.90)
plot(IRF6ii)

# Variance decomposition
VDii = fevd(SVARii, n.ahead = 10)
plot(VDii)


# lets estimate the model
VARiii = VAR(Xiii, p = 1, season = NULL, exog = NULL, type = "const")
summary(VARiii)

SVARiii = SVAR(VARiii, Amat = Amat, Bmat = NULL, hessian = T, estmethod = c("scoring", "direct"))
SVARiii

# Impulse Response Functions
IRF1iii = irf(SVARiii, impulse = "G", response = "G", n.ahead = 20, ci = 0.90)
plot(IRF1iii)
IRF2iii = irf(SVARiii, impulse = "G", response = "T", n.ahead = 20, ci = 0.90)
plot(IRF2iii)
IRF3iii = irf(SVARiii, impulse = "G", response = "Y", n.ahead = 20, ci = 0.90)
plot(IRF3iii)
IRF4iii = irf(SVARiii, impulse = "T", response = "G", n.ahead = 20, ci = 0.90)
plot(IRF4iii)
IRF5iii = irf(SVARiii, impulse = "T", response = "T", n.ahead = 20, ci = 0.90)
plot(IRF5iii)
IRF6iii = irf(SVARiii, impulse = "T", response = "Y", n.ahead = 20, ci = 0.90)
plot(IRF6iii)

# Variance decomposition
VDiii = fevd(SVARiii, n.ahead = 10)
plot(VDiii)
