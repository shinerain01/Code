#Simple OLS regression and then check for issues
#Individual variable tests

rm(list=ls(all=TRUE)) 
perl <- "C:/Perl/bin/perl.exe"
library(rJava)
library(plyr)
library(openxlsx)
library(car)
library(ggplot2)
library(plm)
library(dplyr)
#library(broom)
library(tidyr)
library(stringr)
library(purrr)
library(caret)


Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")

wd="C:\\Users\\shine\\Documents\\IFs\\Data"
setwd(wd)
file="Base Data and Computations.xlsx"
data=readWorkbook(file,sheet=2,colNames=TRUE)
varlist=c("GDP2011PCPPP","Population15to64Mill","EdPubGovtPctGDP","GovtHlPctGDP","LifExpect",
          "EdYearsage15Total","EdQualPriallAvgScrWBGA","EdQualSecallavgscrWBGA",
          "EDQUALAG15","Freedomecon","GovernanceEffect","Corruption","RoadsPavedPct",
          "EnElecAccessNationalPct","ICTFixedBroadband","GovConPctGDP","RandDPctGDP","GovtMilPctGDP","Exports",
          "Imports","EdTerGradRate1stDegreeTotalPct",
          "esecLowrVoc","esecupprvoc")

variable=subset(data,Year>=1990 & Year<=2015,select=c("Country","Year","GDP2011PCPPP","GDP2011","Population15to64Mill","EdPubGovtPctGDP","GovtHlPctGDP","LifExpect",
                                                      "EdYearsage15Total","EdQualPriallAvgScrWBGA","EdQualSecallavgscrWBGA",
                                                      "EDQUALAG15","Freedomecon","GovernanceEffect","Corruption","RoadsPavedPct",
                                                      "EnElecAccessNationalPct","ICTFixedBroadband","GovConPctGDP","RandDPctGDP","GovtMilPctGDP","Exports",
                                                      "Imports","EdTerGradRate1stDegreeTotalPct",
                                                      "esecLowrVoc","esecupprvoc"))

#counting observations for each country each variables
CountryCount=variable %>%  
  group_by(Country) %>% 
  summarise_all(funs(sum(!is.na(.))))
write.csv(CountryCount,"Observations_raw.csv")

TotalCount=variable %>%  
  summarise_all(funs(sum(!is.na(.))))

#write.csv(variable,"variable.csv")
# fill in unbalanced data, change GDP to log term
pdata=mutate(variable,log(variable$GDP2011PCPPP))
names(pdata)=c(names(variable),"LogPPP")
pdata$GDP2011=NULL
#transform GDP (and per capita) data to log values
pdata=transform(pdata,GDP2011PCPPP=log(pdata$GDP2011PCPPP))

#function to calculate lag term
lagoutcome=function(x){
  for (i in 1:length(x)){
    if (names(x[i]) %in% varlist){
      x[[i]]=lag(x[[i]])
    }
  }
  return (x)
}

#Replace original variables with lag terms
regdata=pdata %>%
  group_by(Country) %>% 
  do(lagoutcome(.))

#regression data: LogPPP is the log term of GDP per capita for current time, GDP terms are log lag term. All other variables are lag terms
#Add Dependent variable in regdata and remove logPPP 
regdata=transform(regdata,Y=LogPPP-GDP2011PCPPP)
regdata$LogPPP=NULL
regdata=pdata.frame(regdata,index=c("Country","Year"))

#Change NaN, Inf and -Inf to NA
for (i in 1:length(regdata)){
  regdata[,i][which(is.nan(regdata[,i]))]=NA
  regdata[,i][which(!is.finite(regdata[,i]))]=NA
}

#Remove the cases when Y=NA
YNA <- which(is.na(regdata$Y))
regdata=regdata[c(-YNA),]

#Individual IV
IndReg=regdata %>%
  gather(key=IV,value=IV.value,varlist[[1]]:varlist[[length(varlist)]])%>%
  group_by(IV)%>%
  nest()%>% 
  mutate(model = map(data, ~lm(Y ~ IV.value, data = .,na.action=na.omit)))

RegEst=IndReg%>%
  mutate(tidy = map(model, broom::tidy))%>%
  unnest(tidy, .drop = T)

RegGlance=IndReg%>%
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance,.drop = T)%>%
  arrange(r.squared)


#Stepwise regression
library(leaps)
#fmla <- as.formula(paste("Y ~ ", paste(varlist, collapse= "+")))
#models <- regsubsets(fmla, data = regdata, nvmax = 22,
#                     method = "leapForward")
#summary(models)

#stepwide regression.
#Pick IVs with high sample size, run regression

#Full model not working, pick variables with high samle size
Sample=TotalCount %>%
  gather(key=IV,value=counts,c(varlist[[1]]:varlist[[length(varlist)]])) %>%
  filter(IV!="GDP2011")

high=Sample %>%
  filter(counts>3000)

ModelH=as.formula(paste("Y ~ ", paste(high$IV, collapse= "+")))
OLS1=lm(formula = ModelH,data = regdata,na.action = na.omit)
Colinear1=vif(OLS1)
which(Colinear>5)

#Add additional variables
medium=Sample %>%
  filter(counts<=3000&counts>2000)
ModelM=as.formula(paste("Y ~ ", paste(c(high$IV,medium$IV),collapse= "+")))
OLS2=lm(formula = ModelM,data = regdata,na.action = na.omit)
Colinear2=vif(OLS2)






FE=plm(formula = samleS, data = regdata, model = "within")

#Check for autocorrelation (Breusch-Godfrey test)
pbgtest(FE)
#check for heteroskedasticity (Breush-Pagan test)
plmtest(FE,type = "bp")
#Check for multicollinearity




