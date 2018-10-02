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
#regdata=pdata.frame(regdata,index=c("Country","Year"))

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
#library(leaps)
#fmla <- as.formula(paste("Y ~ ", paste(varlist, collapse= "+")))
#models <- regsubsets(fmla, data = regdata, nvmax = 22,
#                     method = "leapForward")
#summary(models)

#stepwide regression.

#Full model not working, pick IVs with high samle size and then moving forward

Sample=TotalCount %>%
  gather(key=IV,value=counts,c(varlist[[1]]:varlist[[length(varlist)]])) %>%
  filter(IV!="GDP2011")
high=Sample %>%
  filter(counts>3000)
medium=Sample %>%
  filter(counts<=3000&counts>1500)
low=Sample %>%
  filter(counts<=1500)

#Group by group
ModelH=as.formula(paste("Y ~ ", paste(high$IV, collapse= "+")))
OLS1=lm(formula = ModelH,data = regdata,na.action = na.omit)
vif_val=as.data.frame(vif(OLS1)) 
IVnew=maxvif_elim(vif_val,10)

ModelNew=as.formula(paste("Y ~ ", paste(c(IVnew,medium$IV), collapse= "+")))
OLS=lm(formula = ModelNew,data = regdata,na.action = na.omit)
vif_val=as.data.frame(vif(OLS)) 
IVnew=maxvif_elim(vif_val,10)

for (i in 1:4){
  ModelNew=as.formula(paste("Y ~ ", paste(c(IVnew,low$IV[5]), collapse= "+")))
  tryCatch({
    OLS=lm(formula = ModelNew,data = regdata,na.action = na.omit)
    ld.vars <- attributes(alias(OLS)$Complete)$dimnames[[1]]
    if(is.null(ld.vars)==FALSE){
      IVnew=all.vars(ModelNew[[3]])
      IVnew=IVnew[-which(IVnew %in% ld.vars)]
      formNew <- as.formula(paste("Y ~ ", paste(IVnew, collapse= "+")))
      OLS=lm(formula = formNew,data = regdata,na.action = na.omit)}
    vif_val=as.data.frame(vif(OLS)) 
    IVnew=maxvif_elim(vif_val,10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "i=",low$IV[i], "\n")})
}
Model=as.formula(paste("Y ~ ", paste(IVnew, collapse= "+")))
OLS=lm(formula = Model,data = regdata,na.action = na.omit)
summary(OLS)
IVnew

#all group together then problematic ones
ForAll=as.formula(paste("Y ~ ", paste(high$IV,medium$IV,collapse= "+")))
OLS=lm(formula = ForAll,data = regdata,na.action = na.omit)
vif_val=as.data.frame(vif(OLS)) 
IVnew=maxvif_elim(vif_val,10)
for (i in 1:nrow(left)){
  ModelNew=as.formula(paste("Y ~ ", paste(c(IVnew,left$IV[i]), collapse= "+")))
  tryCatch({
    OLS=lm(formula = ModelNew,data = regdata,na.action = na.omit)
    ld.vars <- attributes(alias(OLS)$Complete)$dimnames[[1]]
    if(is.null(ld.vars)==FALSE){
      IVnew=all.vars(ModelNew[[3]])
      IVnew=IVnew[-which(IVnew %in% ld.vars)]
      formNew <- as.formula(paste("Y ~ ", paste(IVnew, collapse= "+")))
      OLS=lm(formula = formNew,data = regdata,na.action = na.omit)}
    vif_val=as.data.frame(vif(OLS)) 
    IVnew=maxvif_elim(vif_val,10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "i=",left$IV[i], "\n")})
}
Model=as.formula(paste("Y ~ ", paste(IVnew, collapse= "+")))
OLS=lm(formula = Model,data = regdata,na.action = na.omit)
summary(OLS)
IVnew

#One by one
IVnew=Sample$IV[1]
for (i in 2:nrow(Sample)){
  ModelNew=as.formula(paste("Y ~ ", paste(c(IVnew,Sample$IV[i]), collapse= "+")))
  tryCatch({
    OLS=lm(formula = ModelNew,data = regdata,na.action = na.omit)
    ld.vars <- attributes(alias(OLS)$Complete)$dimnames[[1]]
     if(is.null(ld.vars)==FALSE){
       IVnew=all.vars(ModelNew[[3]])
       IVnew=IVnew[-which(IVnew %in% ld.vars)]
       formNew <- as.formula(paste("Y ~ ", paste(IVnew, collapse= "+")))
       OLS=lm(formula = formNew,data = regdata,na.action = na.omit)}
    vif_val=as.data.frame(vif(OLS)) 
    IVnew=maxvif_elim(vif_val,10)
  }, error=function(e){cat("ERROR :",conditionMessage(e),  "i=",Sample$IV[i], "\n")})
}
Model=as.formula(paste("Y ~ ", paste(IVnew, collapse= "+")))
OLS=lm(formula = Model,data = regdata,na.action = na.omit)
summary(OLS)
IVnew

#Add all variables first then remove multicolliearity
IVnew=Sample$IV[1]
for (i in 2:nrow(Sample)){
  ModelNew=as.formula(paste("Y ~ ", paste(c(IVnew,Sample$IV[i]), collapse= "+")))
  tryCatch({
    OLS=lm(formula = ModelNew,data = regdata,na.action = na.omit)
    IVnew=all.vars(ModelNew[[3]])
  },error=function(e){cat("ERROR :",conditionMessage(e),  "i=",Sample$IV[i], "\n")})
}
ld.vars <- attributes(alias(OLS)$Complete)$dimnames[[1]]
if(is.null(ld.vars)==FALSE){
  IVnew=all.vars(ModelNew[[3]])
  IVnew=IVnew[-which(IVnew %in% ld.vars)]}
formNew <- as.formula(paste("Y ~ ", paste(IVnew, collapse= "+")))
OLS=lm(formula = formNew,data = regdata,na.action = na.omit)
vif_val=as.data.frame(vif(OLS)) 
IVFinal=maxvif_elim(vif_val,10)
formF <- as.formula(paste("Y ~ ", paste(IVFinal, collapse= "+")))
OLS=lm(formula = formF,data = regdata,na.action = na.omit)
summary(OLS)
IVFinal


#Function to remove high VIF values one by one till all VIFs values are within threshold
maxvif_elim=function(vifvalue,thresh){
  names(vifvalue)="vifVal"
  vifvalue$IV=rownames(vifvalue)
  IVs <-vifvalue$IV
  vif_max<-max(vifvalue$vifVal, na.rm = TRUE)
  if(vif_max<thresh){
    print(vifvalue)
    cat('\n')
    return (IVs) 
  } else{
    while(vif_max >= thresh){
      max_row<-which(vifvalue$vifVal==vif_max)
      print(vifvalue)
      cat('\n')
      cat('removed: ',vifvalue[max_row,2],vif_max,'\n\n')
      vifvalue=vifvalue[-max_row,]
      ModelNew=as.formula(paste("Y ~ ", paste(vifvalue$IV, collapse= "+")))
      OLS=lm(formula = ModelNew,data = regdata,na.action = na.omit)
      vifvalue=as.data.frame(vif(OLS)) 
      names(vifvalue)="vifVal"
      vifvalue$IV=rownames(vifvalue)
      IVs <-vifvalue$IV
      vif_max<-max(vifvalue$vifVal, na.rm = TRUE)
      print(vifvalue)
      cat('\n')

     if(vif_max<thresh) break
      
    }
    
      return(vifvalue$IV)
  }  
}









