# load the packages that can read SAS files and can run white standard error
library(haven)
library(MASS)

# load data
data<-read_sas("regdata.sas7bdat")
file<-read_sas("mktdata.sas7bdat")
df<-read_sas("stkdata.sas7bdat")

# extract the company we want to analysis
AXP<-unlist(data[c(601:720),10])
GE<-unlist(data[c(2041:2160),10])
UBS<-unlist(data[c(5641:5760),10])

# list type data can't fit in lm model in R, need to be adjusted
MKTRF<- unlist(file["MKTRF"])
SMB<- unlist(file["SMB"])
HML<- unlist(file["HML"])

D_axp<- unlist(data[c(601:720),9])
D_ge<-unlist(data[c(2041:2160),9])
D_ubs<-unlist(data[c(5641:5760),9])

# set up dummy variables
DMKTRF_axp<-D_axp*MKTRF
DMKTRF_ge<-D_ge*MKTRF
DMKTRF_ubs<-D_ubs*MKTRF
DHML_axp<-D_axp*HML
DHML_ge<-D_ge*HML
DHML_ubs<-D_ubs*HML
DSMB_axp<-D_axp*SMB
DSMB_ge<-D_ge*SMB
DSMB_ubs<-D_ubs*SMB


# CAPM & 3 factor model
summary(lm(AXP~MKTRF))
summary(lm(GE~MKTRF))
summary(lm(UBS~MKTRF))
summary(lm(AXP~MKTRF+HML+SMB))
summary(lm(GE~MKTRF+HML+SMB))
summary(lm(UBS~MKTRF+HML+SMB))

# check collinearity
summary(lm(MKTRF~HML+SMB,data=file))
summary(lm(HML~MKTRF+SMB,data=file))
summary(lm(SMB~MKTRF+HML,data=file))

# chow test
summary(lm(AXP~MKTRF+SMB+HML+D_axp+DMKTRF_axp+DSMB_axp+DHML_axp))
summary(lm(GE~MKTRF+SMB+HML+D_ge+DMKTRF_ge+DSMB_ge+DHML_ge))
summary(lm(UBS~MKTRF+SMB+HML+D_ubs+DMKTRF_ubs+DSMB_ubs+DHML_ubs))

# set the regression model to run F test
axp<-lm(AXP~MKTRF)
axp_1<-lm(AXP~MKTRF+SMB+HML)
axp_2<- lm(AXP~MKTRF+SMB+HML+D_axp+DMKTRF_axp+DSMB_axp+DHML_axp)
ge<-lm(GE~MKTRF)
ge_1<-lm(GE~MKTRF+SMB+HML)
ge_2<- lm(GE~MKTRF+SMB+HML+D_ge+DMKTRF_ge+DSMB_ge+DHML_ge)
ubs<-lm(UBS~MKTRF)
ubs_1<-lm(UBS~MKTRF+SMB+HML)
ubs_2<- lm(UBS~MKTRF+SMB+HML+D_ubs+DMKTRF_ubs+DSMB_ubs+DHML_ubs)

# F test
anova(axp,axp_1)
anova(ge,ge_1)
anova(ubs,ubs_1)
anova(axp_1,axp_2)
anova(ge_1,ge_2)
anova(ubs_1,ubs_2)

# set the data for white test
error_axp<-residuals(lm(AXP~MKTRF))
error_ge<-residuals(lm((GE~MKTRF)))
UBS<-replace(UBS,83,0.03509)

# the missing value of the 83th month of UBS will cause problem in the process of running white test and 
# serial correlation, so we find the actual data 3.509% and fill it in, then re-run the CAPM regression 
# and get the residuals

error_ubs<-residuals(lm(UBS~MKTRF))
error2_axp<-error_axp^2
error2_ge<-error_ge^2
error2_ubs<-error_ubs^2
mktrf2<-MKTRF^2

# white test
summary(lm(error2_axp~MKTRF+mktrf2))
summary(lm(error2_ge~MKTRF+mktrf2))
summary(lm(error2_ubs~MKTRF+mktrf2))

# white standard error
summary(rlm(AXP~MKTRF))
summary(rlm(GE~MKTRF))
summary(rlm(UBS~MKTRF))

# set data for serial correlation
lag_axp<-vector()
lag_ge<-vector()
lag_ubs<-vector()
lag_axp[1]<-0
lag_ge[1]<-0
lag_ubs[1]<-0
lag_axp[2:120]<-error_axp[1:119]
lag_ge[2:120]<-error_ge[1:119]
lag_ubs[2:120]<-error_ubs[1:119]

# run serial correlation
summary(lm(error_axp~MKTRF+lag_axp))
summary(lm(error_ge~MKTRF+lag_ge))
summary(lm(error_ubs~MKTRF+lag_ubs))