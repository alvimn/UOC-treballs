library("gdata")
library("stargazer")
library("xtable")
library("utils")
library("texreg")
library("pastecs")
library("data.table")

library("plm")
library("lme4")
library("lqmm")
library("qrLMM")
library("psych")
library("ggplot2")
library("car")
library("knitLatex")
library("dplyr")

library(gridExtra)
library(grid)
library(lattice)

#install.packages("devtools")
library(devtools)
library(tibble)
library(sandwich)
library(lmtest)
library(car) 
library(fmsb)
library(corrplot)

library("rJava")
library("openxlsx")
library("reshape2")
#library("xlsx")
#install_github("easyGgplot2", "kassambara")
library("easyGgplot2")

Directory.Data.Reading<-"C:/ALBERTO/3-DOCTORADO/DATOS/2 - PAPER/GENERADOS desde R/"


#...............................................................................................................
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#...............................................................................................................
# 02-02-2018: Variables que contienen regresiones realizadas con BBDD hasta 2015
#...............................................................................................................
#    BBDD utilizada para realizar las regresiones con datos hasta 2015
#REGRESSION.Data.BBDD2015<-REGRESSION.Data

#QUANTILE.Regression.Menor.1996.2<-QUANTILE.Regression
# .....................
#QUANTILE.Regression<-QUANTILE.Regression.Complete1
#QUANTILE.Regression<-QUANTILE.Regression.Complete.2
#QUANTILE.Regression<-QUANTILE.Regression.Complete.3
# .....................
#QUANTILE.Regression<-QUANTILE.Regression.Menor.1991
#QUANTILE.Regression<-QUANTILE.Regression.Menor.1996
#QUANTILE.Regression<-QUANTILE.Regression.Menor.1996.2
#QUANTILE.Regression<-QUANTILE.Regression.Menor.1996.3
# .....................
#QUANTILE.Regression<-QUANTILE.Regression.Mayor.1990
#QUANTILE.Regression<-QUANTILE.Regression.Mayor.1995
#QUANTILE.Regression<-QUANTILE.Regression.Mayor.1995.2
#QUANTILE.Regression<-QUANTILE.Regression.Mayor.1995.3
#.......................

#...............................................................................................................
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#...............................................................................................................
#
# Bases de datos actualizadas para realizar nuevo panel con datos hasta 2016

# BBDD_OECD_Goods_Transport
# BBDD_OECD_Passenger_Transport
# BBDD_OECD_OIL_Road
# BBDD.OECD.Oil.Prices
# BBDD_WB_WDI


#View(REGRESSION.Data)
View(BBDD_OECD_Passenger_Transport)
View(BBDD_OECD_Goods_Transport)
View(BBDD_OECD_OIL_Road)
View(BBDD.OECD.Oil.Prices)
View(BBDD_WB_WDI)



fClean.Structure <-function(fDATA)
{
  # We check it out for spurious data
  # We create a new structure without hidden internal structure data which impairs plm calculus
  if (is.null(dim(fDATA)))
  {
    return(fDATA)
  }
  else{
        fnames.TEMP<-colnames(fDATA) 
        rm(DATA.Temp)
        DATA.Temp<-fDATA[,1]
        for (i in 2:dim(fDATA)[2])
        {
          DATA.Temp<-cbind(DATA.Temp, fDATA[,i])
        }
        colnames(DATA.Temp)<-fnames.TEMP
        return(DATA.Temp)
     }
}



# FUNCTION FOR PANEL UNIT ROOT TESTS
fpurtest <- function(VARIABLE, INDEX, SUMMARY)
{
  # For dealing with NA data
  # http://r.789695.n4.nabble.com/purtest-and-missing-values-plm-package-td4689458.html
  
  if (SUMMARY){
    # -----------------------------------------------------------------------
    message(" --------------------------------------------------------------------------")
    message("--------------   Levin-Lin-Chu Unit-Root Test  ----------------------------")
    message(" --------------------------------------------------------------------------")
    print(summary(purtest(VARIABLE , index = INDEX, pmax = 4, test = "levinlin", exo = "none")))
    print("----------------------- INTERCEPT --------------------------------------------")
    print(summary(purtest(VARIABLE , index = INDEX, pmax = 4, test = "levinlin", exo = "intercept")))
    print("----------------------- TREND ------------------------------------------------")
    print(summary(purtest(VARIABLE , index = INDEX, pmax = 4, test = "levinlin", exo = "trend")))
    
    # -----------------------------------------------------------------------
    message(" --------------------------------------------------------------------------")
    message("--------------   Maddala-Wu Unit-Root Test  -------------------------------")
    message(" --------------------------------------------------------------------------")
    print(summary(purtest(VARIABLE , index = INDEX, pmax = 4, test = "madwu", exo = "none")))
    print("----------------------- INTERCEPT ------------------------------------")
    print(summary(purtest(VARIABLE , index = INDEX, pmax = 4, test = "madwu", exo = "intercept")))
    print("----------------------- TREND ----------------------------------------")
    print(summary(purtest(VARIABLE , index = INDEX, pmax = 4, test = "madwu", exo = "trend")))
    
    # -----------------------------------------------------------------------
    message(" --------------------------------------------------------------------------")
    message("--------------   Hadri Test  ----------------------------------------------")
    message(" --------------------------------------------------------------------------")
    #purtest(VARIABLE , index = INDEX, pmax = 4, test = "hadri", exo = "none")
    print("----------------------- INTERCEPT --------------------------------------------")
    print(purtest(VARIABLE , index = INDEX, pmax = 4, test = "hadri", exo = "intercept"))
    print("----------------------- TREND ------------------------------------------------")
    print(purtest(VARIABLE , index = INDEX, pmax = 4, test = "hadri", exo = "trend"))
    
    # ----------------------------------------------------------------------- 
    message(" --------------------------------------------------------------------------")
    message("--------------   Im-Pesaran-Shin Unit-Root Test  --------------------------")
    message(" --------------------------------------------------------------------------")
    # purtest(VARIABLE , index = INDEX, pmax = 4, test = "ips", exo = "none")
    print("----------------------- INTERCEPT --------------------------------------------")
    print(summary(purtest(VARIABLE , index = INDEX, pmax = 4, test = "ips", exo = "intercept")))
    print("----------------------- TREND ------------------------------------------------")
    print(summary(purtest(VARIABLE , index = INDEX, pmax = 4, test = "ips", exo = "trend")))
    
    # -----------------------------------------------------------------------
  }
  else{
    # -----------------------------------------------------------------------
    message(" --------------------------------------------------------------------------")
    message("--------------   Levin-Lin-Chu Unit-Root Test  ----------------------------")
    message(" --------------------------------------------------------------------------")
    print(purtest(VARIABLE , index = INDEX, pmax = 4, test = "levinlin", exo = "none"))
    print("----------------------- INTERCEPT ------------------------------------")
    print(purtest(VARIABLE , index = INDEX, pmax = 4, test = "levinlin", exo = "intercept"))
    print("----------------------- TREND ----------------------------------------")
    print(purtest(VARIABLE , index = INDEX, pmax = 4, test = "levinlin", exo = "trend"))
    
    # -----------------------------------------------------------------------
    message(" --------------------------------------------------------------------------")
    message("--------------   Maddala-Wu Unit-Root Test  -------------------------------")
    message(" --------------------------------------------------------------------------")
    print(purtest(VARIABLE , index = INDEX, pmax = 4, test = "madwu", exo = "none"))
    print("----------------------- INTERCEPT ------------------------------------")
    print(purtest(VARIABLE , index = INDEX, pmax = 4, test = "madwu", exo = "intercept"))
    print("----------------------- TREND ----------------------------------------")
    print(purtest(VARIABLE , index = INDEX, pmax = 4, test = "madwu", exo = "trend"))
    
    # -----------------------------------------------------------------------
    message(" --------------------------------------------------------------------------")
    message("--------------   Hadri Test  ----------------------------------------------")
    message(" --------------------------------------------------------------------------")
    #purtest(VARIABLE , index = INDEX, pmax = 4, test = "hadri", exo = "none")
    print("----------------------- INTERCEPT ------------------------------------")
    print(purtest(VARIABLE , index = INDEX, pmax = 4, test = "hadri", exo = "intercept"))
    print("----------------------- TREND ----------------------------------------")
    print(purtest(VARIABLE , index = INDEX, pmax = 4, test = "hadri", exo = "trend"))
    
    # ----------------------------------------------------------------------- 
    message(" --------------------------------------------------------------------------")
    message("--------------   Im-Pesaran-Shin Unit-Root Test  --------------------------")
    message(" --------------------------------------------------------------------------")
    # purtest(VARIABLE , index = INDEX, pmax = 4, test = "ips", exo = "none")
    print("----------------------- INTERCEPT ------------------------------------")
    print(purtest(VARIABLE , index = INDEX, pmax = 4, test = "ips", exo = "intercept"))
    print("----------------------- TREND ----------------------------------------")
    print(purtest(VARIABLE , index = INDEX, pmax = 4, test = "ips", exo = "trend"))
    
    # -----------------------------------------------------------------------
  }
}

# *************************************************************************************************************************************************
# *************************************************************************************************************************************************
# *************************************************************************************************************************************************
#  WE SELECT VARIABLES FOR USING THEM AS EXOGENOUS REGRESSORS
# *************************************************************************************************************************************************
# *************************************************************************************************************************************************
# *************************************************************************************************************************************************

VARS.BBDD_WB_WDI<-c(
  "Country","Year",
  "WB-Population, total",
  "WB-Population in urban agglomerations of more than 1 million",
  "WB-Population in the largest city (% of urban population)",
  "WB-Population in urban agglomerations of more than 1 million (% of total population)",
  "WB-Urban population",
  "WB-Rural population",
  "WB-Urban population (% of total population)",
  "WB-Rural population (% of total population)",
  "WB-Population density (people per sq. km of land area)",
  "WB-Manufacturing, value added (% of GDP)",
  "WB-Manufacturing, value added (constant 2010 US$)",
  "WB-Industry (including construction), value added (constant 2010 US$)",
  "WB-Households and NPISHs Final consumption expenditure, PPP (constant 2011 international $)",
  "WB-GDP, PPP (constant 2011 international $)",
  "WB-GDP per capita growth (annual %)",
  "WB-GDP growth (annual %)",
  "WB-CO2 emissions (kg per 2011 PPP $ of GDP)",
  "WB-CO2 emissions from transport (% of total fuel combustion)",
  "WB-Railways, passengers carried (million passenger-km)",
  "WB-Railways, goods transported (million ton-km)")

View(BBDD_WB_WDI[,VARS.BBDD_WB_WDI])


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -------------------------------------------------------------------------------------------------------------------------------------------------
# EMPLOYMENT: WB (World Bank)
# -------------------------------------------------------------------------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
names(BBDD_WB_WDI)
VARS.EMPLOYMENT.BBDD_WB_WDI<-c(
  "Country","Year",
  "WB-Vulnerable employment, total (% of total employment) (modeled ILO estimate)",
  "WB-Labor force with advanced education (% of total working-age population with advanced education)" )

#View(BBDD_WB_WDI[,VARS.EMPLOYMENT.BBDD_WB_WDI])


# -------------------------------------------------------------------------------------------------------------------------------------------------
names(BBDD_WB_SDG)
VARS.EMPLOYMENT.BBDD_WB_SDG<-c(
  "Country","Year",
  "WB-Unemployment, total (% of total labor force) (modeled ILO estimate)"
  #"WB-Unemployment, total (% of total labor force) (national estimate)"
)

#View(BBDD_WB_SDG[,VARS.EMPLOYMENT.BBDD_WB_SDG])




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -------------------------------------------------------------------------------------------------------------------------------------------------
# POVERTY & INEQUALITY: WB (World Bank)
# -------------------------------------------------------------------------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
names(BBDD_WB_WDI)
VARS.WEALTH.BBDD_WB_WDI<-c(
  "Country","Year",
  "WB-GINI index (World Bank estimate)"
)

#View(BBDD_WB_WDI[,VARS.WEALTH.BBDD_WB_WDI])
# -------------------------------------------------------------------------------------------------------------------------------------------------

names(BBDD_WB_PE)
VARS.WEALTH.BBDD_WB_PE<-c(
  "Country","Year",
  "WB-GINI index (World Bank estimate)",
  "WB-GNI (constant 2010 US$)"
)

View(BBDD_WB_PE[,VARS.WEALTH.BBDD_WB_PE])

# -------------------------------------------------------------------------------------------------------------------------------------------------



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -------------------------------------------------------------------------------------------------------------------------------------------------
# GEOGRAPHY
# -------------------------------------------------------------------------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

names(BBDD_WB_WDI)
VARS.GEO.BBDD_WB_WDI<-c(
  "Country","Year",
  "WB-Land area (sq. km)"
)
#View(BBDD_WB_WDI[,VARS.GEO.BBDD_WB_WDI])



# .................................................................................................................................................
# .................................................................................................................................................
# .................................................................................................................................................



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -------------------------------------------------------------------------------------------------------------------------------------------------
# OIL CONSUMPTION
# -------------------------------------------------------------------------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# In the case of OIL CONSUMPTION it is necessary to create a new Database which outcome is the ADDITION of total fuels
names(BBDD_OECD_OIL_Road)

View(BBDD_OECD_OIL_Road[,VARS.BBDD_OECD_OIL_Road])

# ................................................................................................................................
# We add data  Biogasoline data for Denmark cause it is incomplete from 2012 to 2015
# Data for Denmark is confidential

View(BBDD_OECD_OIL_Road[which(BBDD_OECD_OIL_Road[,"Country"]=="Denmark"),])


#Ya no es necesario arreglar los datos de Dinamarca
#for(Year in 2012:2018)
#{
#  BioDiesel.TEMP<-BBDD_OECD_OIL_Road[which(BBDD_OECD_OIL_Road[,"Country"]=="Denmark" & BBDD_OECD_OIL_Road[,"Year"]==Year),"OECD-Biodiesels (kt)"]
#  Diesel.TEMP<-BBDD_OECD_OIL_Road[which(BBDD_OECD_OIL_Road[,"Country"]=="Denmark" & BBDD_OECD_OIL_Road[,"Year"]==Year),"OECD-Gas/diesel oil excl. biofuels (kt)"]
  
#  Gasoline.TEMP<-BBDD_OECD_OIL_Road[which(BBDD_OECD_OIL_Road[,"Country"]=="Denmark" & BBDD_OECD_OIL_Road[,"Year"]==Year),"OECD-Motor gasoline excl. biofuels (kt)"]
  
#  PORCENTAJE.BioGasoline=(BioDiesel.TEMP)/(BioDiesel.TEMP+Diesel.TEMP)
  
#  BioGasoline<-PORCENTAJE.BioGasoline*Gasoline.TEMP/(1-PORCENTAJE.BioGasoline)
  
#  BBDD_OECD_OIL_Road[which(BBDD_OECD_OIL_Road[,"Country"]=="Denmark" & BBDD_OECD_OIL_Road[,"Year"]==Year),"OECD-Biogasoline (kt)"]<- BioGasoline
#}

dim(BBDD_OECD_OIL_Road)

dim(na.omit(BBDD_OECD_OIL_Road))
View(na.omit(BBDD_OECD_OIL_Road))
BBDD_OECD_OIL_Road<-na.omit(BBDD_OECD_OIL_Road)


VARS.BBDD_OECD_OIL_Road<-c(
  "OECD-Biogasoline (kt)",
  "OECD-Biodiesels (kt)",
  "OECD-Gas/diesel oil excl. biofuels (kt)",
  "OECD-Motor gasoline excl. biofuels (kt)",
  "OECD-Liquefied petroleum gases (LPG) (kt)",
  "OECD-Other liquid biofuels (kt)"
)



TOTAL.ROAD.Fuel.Consumption<-rowSums(subset(BBDD_OECD_OIL_Road, select = VARS.BBDD_OECD_OIL_Road))
#Countries.Fuel<-subset(BBDD_OECD_OIL_Road, select = c("Country","Year"))
#BBDD.TOTAL.ROAD.Fuel.Consumption<-(cbind(Countries.Fuel,TOTAL.ROAD.Fuel.Consumption))
rm(BBDD.TOTAL.ROAD.Fuel.Consumption)
BBDD.TOTAL.ROAD.Fuel.Consumption<-(cbind(BBDD_OECD_OIL_Road,TOTAL.ROAD.Fuel.Consumption))

names(BBDD.TOTAL.ROAD.Fuel.Consumption)
BBDD.TOTAL.ROAD.Fuel.Consumption<-BBDD.TOTAL.ROAD.Fuel.Consumption[,c("Country","Year",
                                    "TOTAL.ROAD.Fuel.Consumption",
                                    "OECD-Gas/diesel oil excl. biofuels (kt)",
                                    "OECD-Motor gasoline excl. biofuels (kt)",
                                    "OECD-Biodiesels (kt)",
                                    "OECD-Biogasoline (kt)",
                                    "OECD-Other liquid biofuels (kt)",
                                    "OECD-Liquefied petroleum gases (LPG) (kt)"
                                    )]

colnames(BBDD.TOTAL.ROAD.Fuel.Consumption)[colnames(BBDD.TOTAL.ROAD.Fuel.Consumption)==
                                             "TOTAL.ROAD.Fuel.Consumption"]<-"Total Road Fuel Consumption (kt)"
dim(BBDD.TOTAL.ROAD.Fuel.Consumption)
FUEL.CONSUMPTION.Data<-BBDD.TOTAL.ROAD.Fuel.Consumption
#View(BBDD.TOTAL.ROAD.Fuel.Consumption)

FUEL.Temp<-(FUEL.CONSUMPTION.Data[which(FUEL.CONSUMPTION.Data[,"Country"] %in% Countries.FINAL.QA),])
#levels(droplevels(FUEL.Temp[,"Country"]))
dim(FUEL.Temp)
levels(droplevels(FUEL.Temp$Country))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -------------------------------------------------------------------------------------------------------------------------------------------------
# OIL PRICES
# -------------------------------------------------------------------------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(OIL.PRICES.Data)
names(BBDD.OECD.Oil.Prices)
# We select just data expressed on (USD/unit using PPP)
Fuel.Names.Vars<-names(BBDD.OECD.Oil.Prices)[grep("(USD/unit using PPP)",names(BBDD.OECD.Oil.Prices), ignore.case = TRUE)]
# We remove elctricity prices data
Fuel.Names.Vars<-Fuel.Names.Vars[-grep("(MWh)", Fuel.Names.Vars, ignore.case = TRUE)]


VARS.BBDD.OECD.Oil.Prices<-c(
  "Country","Year",
  Fuel.Names.Vars
  #"OECD-Automotive diesel (litre)-Industry-Total price (USD/unit using PPP)"
)

OIL.PRICES.Data<-BBDD.OECD.Oil.Prices[,VARS.BBDD.OECD.Oil.Prices]
dim(na.omit(OIL.PRICES.Data))
View(OIL.PRICES.Data)
# View(BBDD.OECD.Oil.Prices[,VARS.BBDD.OECD.Oil.Prices])




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -------------------------------------------------------------------------------------------------------------------------------------------------
# MACROECONOMIC AGGREGATES: Begin
# -------------------------------------------------------------------------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
names(BBDD_WB_WDI.GDP)
VARS.BBDD_WB_WDI.GDP<-c(
  "Country","Year",
  "WB-GDP, PPP (constant 2011 international $)",
  "WB-Industry, value added (% of GDP)",
  "WB-Manufacturing, value added (% of GDP)"
  )
# View(BBDD_WB_WDI.GDP[,VARS.BBDD_WB_WDI.GDP])
# -------------------------------------------------------------------------------------------------------------------------------------------------
names(BBDD_OECD.VAR.GDP_PPP.Merged)
VARS.BBDD_OECD.VAR.GDP_PPP.Merged<-c(
  "Country","Year",
  "OECD-Gross domestic product (expenditure approach) Constant prices, constant PPPs, OECD base year 2010 Millions US Dollar"
)
# View(BBDD_OECD.VAR.GDP_PPP.Merged[,VARS.BBDD_OECD.VAR.GDP_PPP.Merged])
# -------------------------------------------------------------------------------------------------------------------------------------------------
names(BBDD_WB_SDG)
# GNI: Gross National Income
VARS.WEALTH.BBDD_WB_SDG<-c(
  "Country","Year",
  "WB-GNI per capita, PPP (constant 2011 international $)"
)

# View(BBDD_WB_SDG[,VARS.WEALTH.BBDD_WB_SDG])
# -------------------------------------------------------------------------------------------------------------------------------------------------

names(BBDD_OECD_MA_NAccounts.VAR.SELECTED)
# GNI: Gross National Income
VARS.BBDD_OECD_MA_NAccounts.VAR.SELECTED<-c(
  "Country","Year",
  "OECD-Actual individual consumption, at 2010 prices and PPPs, billions US dollars //*//-P41VPVOB",
  "OECD-GDP at 2010 constant prices and PPPs, billions US dollars //*//-GDPVPVOB",
  "OECD-Actual individual consumption, percentage of GDP //*//-P41S",
  "OECD-GDP per capita, at constant 2010 prices and PPPs, US dollars //*//-GDPHVPVOB"
)

View(BBDD_OECD_MA_NAccounts.VAR.SELECTED[,VARS.BBDD_OECD_MA_NAccounts.VAR.SELECTED])
# -------------------------------------------------------------------------------------------------------------------------------------------------

rm(ME.AGGREGATES.Data)
ME.AGGREGATES.Data <-BBDD_OECD_MA_NAccounts.VAR.SELECTED[,VARS.BBDD_OECD_MA_NAccounts.VAR.SELECTED]

#View(BBDD_OECD_MA_NAccounts.VAR.SELECTED[,VARS.BBDD_OECD_MA_NAccounts.VAR.SELECTED])

#...........................................................................................................
# We figure out GDP growth on %

ME.AGGREGATES.Data.GDP<-ME.AGGREGATES.Data[,"OECD-GDP at 2010 constant prices and PPPs, billions US dollars //*//-GDPVPVOB"]

rm(DIF.Temp)
DIF.Temp<-diff(ME.AGGREGATES.Data.GDP,1)
DIF.Temp<-append(DIF.Temp,NA,after=length(DIF.Temp))

rm(PERCTG.Temp)
PERCTG.Temp<-DIF.Temp/ME.AGGREGATES.Data.GDP*100
head(PERCTG.Temp)
tail(PERCTG.Temp)

rm(PERCTG.GDP)
PERCTG.GDP<-append(PERCTG.Temp,NA, after=0)
PERCTG.GDP<-PERCTG.GDP[1:length(PERCTG.GDP)-1]
length(PERCTG.GDP)
tail(PERCTG.GDP)

head(PERCTG.GDP)

# We remove PERCTG.GDP data for the first value corresponding to every Country. We do this
# cause firsth value is NA
Country.2.Check<-ME.AGGREGATES.Data[1,"Country"]

for(i in 1:length(ME.AGGREGATES.Data.GDP))
{
  if(Country.2.Check==ME.AGGREGATES.Data[i,"Country"]){}
    else
    {
      PERCTG.GDP[i]<-NA
      Country.2.Check=ME.AGGREGATES.Data[i,"Country"]
     }
}

ME.AGGREGATES.Data<-(cbind(ME.AGGREGATES.Data,PERCTG.GDP))
dim(ME.AGGREGATES.Data)

names(ME.AGGREGATES.Data)
colnames(ME.AGGREGATES.Data)
colnames(ME.AGGREGATES.Data)[colnames(ME.AGGREGATES.Data)=="PERCTG.GDP"]<-"% Growth GDP"
View(ME.AGGREGATES.Data)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -------------------------------------------------------------------------------------------------------------------------------------------------
# MACROECONOMIC AGGREGATES: End
# -------------------------------------------------------------------------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -------------------------------------------------------------------------------------------------------------------------------------------------
# POPULATION: WB (World Bank)
# -------------------------------------------------------------------------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
names(BBDD_WB_WDI)
VARS.POPULATION.BBDD_WB_WDI<-c(
  "Country","Year",
  "WB~WDI-Population, total",
  "WB~WDI-Population in largest city",
  "WB~WDI-Population in urban agglomerations of more than 1 million (% of total population)",
  "WB~WDI-Rural population (% of total population)",
  #"WB~WDI-Urban population (% of total)",
  "WB~WDI-Urban population (% of total population)",
  "WB~WDI-Population density (people per sq. km of land area)"
  
)
 View(BBDD_WB_WDI[,VARS.POPULATION.BBDD_WB_WDI])

# -------------------------------------------------------------------------------------------------------------------------------------------------

names(BBDD_WB_SDG)
VARS.POPULATION.BBDD_WB_SDG<-c(
  "Country","Year",
  "WB-Urban population (% of total)",
  "WB-Urban population"
)

POPULATION.Data<-BBDD_WB_WDI[,VARS.POPULATION.BBDD_WB_WDI]
#View(BBDD_WB_SDG[,VARS.POPULATION.BBDD_WB_SDG])




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -------------------------------------------------------------------------------------------------------------------------------------------------
# LOGISTICS: WB (World Bank)
# -------------------------------------------------------------------------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# -------------------------------------------------------------------------------------------------------------------------------------------------
names(BBDD_WB_PE)
VARS.LOGISTICS.BBDD_WB_PE<-c(
  "Country","Year",
  "WB-Logistics performance index: Competence and quality of logistics services (1=low to 5=high)",
  "WB-Logistics performance index: Overall (1=low to 5=high)"
)
#View(BBDD_WB_PE[,VARS.LOGISTICS.BBDD_WB_PE])
# -------------------------------------------------------------------------------------------------------------------------------------------------
names(BBDD_WB_IDARMS)
VARS.LOGISTICS.BBDD_WB_IDARMS<-c(
  "Country","Year",
  "WB-Logistics performance index: Overall (1=low to 5=high)"
)
#View(BBDD_WB_IDARMS[,VARS.LOGISTICS.BBDD_WB_IDARMS])
#levels(droplevels(BBDD_WB_IDARMS[,VARS.LOGISTICS.BBDD_WB_IDARMS]$Country))


names(BBDD_WB_WDI)
VARS.LOGISTICS.BBDD_WB_WDI<-c(
  "Country","Year",
  "WB~WDI-Logistics performance index: Competence and quality of logistics services (1=low to 5=high)",
  "WB~WDI-Logistics performance index: Overall (1=low to 5=high)",
  "WB~WDI-Logistics performance index: Quality of trade and transport-related infrastructure (1=low to 5=high)"
)
View(BBDD_WB_WDI[,VARS.LOGISTICS.BBDD_WB_WDI])



WB.WDI.Logistics.Data<-BBDD_WB_WDI[,VARS.LOGISTICS.BBDD_WB_WDI]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -------------------------------------------------------------------------------------------------------------------------------------------------
# INFRAESTRUCTURE: WEF (World Economic Forum)
# -------------------------------------------------------------------------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# -------------------------------------------------------------------------------------------------------------------------------------------------
names(BBDD_WEF.GCI.Transport)
VARS.BBDD_WEF.GCI.Transport<-c(
"Country","Year",
"WEF-Quality of roads, 1-7 (best)",
"WEF-Quality of railroad infrastructure, 1-7 (best)"
)

#View(BBDD_WEF.GCI.Transport[,VARS.BBDD_WEF.GCI.Transport])

INFRASTRUCTURE.Data<-BBDD_WEF.GCI.Transport[,VARS.BBDD_WEF.GCI.Transport]




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -------------------------------------------------------------------------------------------------------------------------------------------------
# TRANSPORT: Begin
# -------------------------------------------------------------------------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ............................................................................................................
# WB (World Bank) TRANSPORT DATA
# ............................................................................................................


# BBDD_WB_WDI is a BETTER DATA BASE than BBDD_WB_SDG cause it time spreads wider

names(BBDD_WB_WDI)
# This is to figure out Which variables contain the pattern "Rail"
names(BBDD_WB_WDI)[grep("Rail",names(BBDD_WB_WDI), ignore.case = TRUE)]
names(BBDD_WB_WDI)[grep("Road",names(BBDD_WB_WDI), ignore.case = TRUE)]
VARS.TRANSPORT.BBDD_WB_WDI<-c(
  "Country","Year",
  "WB-Railways, goods transported (million ton-km)",
  "WB-Railways, passengers carried (million passenger-km)"
)

#View(BBDD_WB_WDI[,VARS.TRANSPORT.BBDD_WB_WDI])


# -------------------------------------------------------------------------------------------------------------------------------------------------
names(BBDD_WB_SDG)
VARS.TRANSPORT.BBDD_WB_SDG<-c(
  "Country","Year",
  "WB-Railways, goods transported (million ton-km)",
  "WB-Railways, passengers carried (million passenger-km)"
)

#View(BBDD_WB_SDG[,VARS.TRANSPORT.BBDD_WB_SDG])


# -------------------------------------------------------------------------------------------------------------------------------------------------
rm(TRANSPORT.Data)
TRANSPORT.Data<-BBDD_WB_WDI[,VARS.TRANSPORT.BBDD_WB_WDI]
#View(TRANSPORT.Data)



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# OLD
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#View(BBDD_OECD_Goods_Transport ) #OK
#View(BBDD_OECD_Passenger_Transport ) 
rm(BBDD.OECD.Goods.Passenger.Transport)
BBDD.OECD.Goods.Passenger.Transport<-merge(BBDD_OECD_Passenger_Transport, BBDD_OECD_Goods_Transport,  by=c("Country","Year"), all=TRUE)
names(BBDD.OECD.Goods.Passenger.Transport)

# This is to figure out Which variables contain the pattern "Rail"
names(BBDD.OECD.Goods.Passenger.Transport)[grep("Rail",names(BBDD.OECD.Goods.Passenger.Transport), ignore.case = TRUE)]
# This is to figure out Which variables contain the pattern "Road"
names(BBDD.OECD.Goods.Passenger.Transport)[grep("Road",names(BBDD.OECD.Goods.Passenger.Transport), ignore.case = TRUE)]
VARS.TRANSPORT.BBDD.OECD.Goods.Passenger.Transport<-c(
  "Country","Year",
  "OECD-Rail passenger transport",
  "OECD-Rail freight transport"
)

Rail.Names<-names(BBDD.OECD.Goods.Passenger.Transport)[grep("Rail",names(BBDD.OECD.Goods.Passenger.Transport), ignore.case = TRUE)]
Road.Names<-names(BBDD.OECD.Goods.Passenger.Transport)[grep("Road",names(BBDD.OECD.Goods.Passenger.Transport), ignore.case = TRUE)]

View(BBDD.OECD.Goods.Passenger.Transport[,c("Country","Year",Rail.Names)])
View(BBDD.OECD.Goods.Passenger.Transport[,c("Country","Year",Road.Names)])





#...........................................................................................................................
# BEGIN: Transport BBDD Construction & CORRELATION ASSESSMENT. We figure out correlation between Road and Rail data
#...........................................................................................................................

#View(BBDD.OECD.Goods.Passenger.Transport[,VARS.TRANSPORT.BBDD.OECD.Goods.Passenger.Transport])
rm(VARS.TRANSPORT.BBDD.OECD.Goods.Passenger.Transport.Correlation)
VARS.TRANSPORT.BBDD.OECD.Goods.Passenger.Transport.Correlation<-c("Country","Year",
  "OECD-Road freight transport",
  "OECD-Road passenger transport",
  "OECD-Rail freight transport",
  "OECD-Rail passenger transport")

dim(na.omit(BBDD.OECD.Goods.Passenger.Transport[,
                                        VARS.TRANSPORT.BBDD.OECD.Goods.Passenger.Transport.Correlation]))

BBDD.OECD.Transport.Selected<-BBDD.OECD.Goods.Passenger.Transport[,
                                    VARS.TRANSPORT.BBDD.OECD.Goods.Passenger.Transport.Correlation]
cor(na.omit(BBDD.OECD.Transport.Selected[3:6]))
names(BBDD.OECD.Transport.Selected)
#...........................................................................................................................
# END: Transport BBDD Construction & CORRELATION ASSESSMENT. We figure out correlation between Road and Rail data
#...........................................................................................................................




#...........................................................................................................................
# BEGIN: Transport & MA AGGREGATES BBDD Construction
#...........................................................................................................................


rm(TRANSPORT.GDP)
names(ME.AGGREGATES.Data)
names(BBDD.OECD.Transport.Selected)

TRANSPORT.GDP<-merge(ME.AGGREGATES.Data, BBDD.OECD.Transport.Selected,  by=c("Country","Year"), all=TRUE)
colnames(TRANSPORT.GDP)

# We just take Variables we want to work with
VARS.TRANSPORT.GDP<-c("Country","Year",
  "OECD-GDP at 2010 constant prices and PPPs, billions US dollars //*//-GDPVPVOB",
  #"OECD-Road freight transport",
  #"OECD-Road passenger transport",
  "OECD-Rail freight transport",
  "OECD-Rail passenger transport")

TRANSPORT.GDP<-TRANSPORT.GDP[,VARS.TRANSPORT.GDP]
colnames(TRANSPORT.GDP)

# We change and simplified the name of GDP field
colnames(TRANSPORT.GDP)<-c("Country","Year",
  "OECD-GDP",VARS.TRANSPORT.GDP[4:length(VARS.TRANSPORT.GDP)] )

dim(TRANSPORT.GDP)
dim(na.omit(TRANSPORT.GDP))


#...........................................................................................................................
# END: Transport & MA AGGREGATES BBDD Construction
#...........................................................................................................................



#...........................................................................................................................
# BEGIN: Transport & MA AGGREGATES Correlation Assessment
#...........................................................................................................................



rm(COR.DATA.Temp)
COR.DATA.Temp<-merge(REGRESSION.Data.YEAR.Split, TRANSPORT.GDP,  by=c("Country","Year"), all=TRUE)
COR.DATA.Temp<-na.omit(COR.DATA.Temp)
dim(((COR.DATA.Temp)))
View(((COR.DATA.Temp)))
colnames(COR.DATA.Temp)

rm(RRR)
RRR<-merge(REGRESSION.Data.YEAR.Split, COR.DATA.Temp,  by=c("Country","Year"), all=TRUE)
dim(RRR)
View(RRR[which(is.na(RRR[,19])),])



cor(log(na.omit(TRANSPORT.GDP[3:5])),method = c("pearson"))
cor(log(na.omit(TRANSPORT.GDP[3:5])),method = c( "spearman"))


VIF(lm(log(TRANSPORT.GDP[,c(3)]) ~ log(TRANSPORT.GDP[,c(4)])))
VIF(lm(log(TRANSPORT.GDP[,c(3)]) ~ log(TRANSPORT.GDP[,c(5)])))
VIF(lm(log(TRANSPORT.GDP[,c(4)]) ~ log(TRANSPORT.GDP[,c(5)])))


VIF(lm(log(COR.DATA.Temp[,c(3)]) ~ log(COR.DATA.Temp[,c(13)])))

cor(na.omit(log(COR.DATA.Temp[,c(3,12)])),method = c("pearson"))

cor(na.omit(log(COR.DATA.Temp[,c(11,13)])),method = c("pearson"))
cor(na.omit(log(COR.DATA.Temp[,c(5,12,13)])),method = c("spearman"))


#...........................................................................................................................
# END: Transport & MA AGGREGATES Correlation Assessment
#...........................................................................................................................



#...........................................................................................................................
# TIME SPAN PER COUNTRY
#...........................................................................................................................

rm(Summary.group.Freight)
Summary.group.Freight<-describeBy(COR.DATA.Temp$Year,
                          group=droplevels(COR.DATA.Temp$Country) ,mat=TRUE)
Summary.group.Freight<-Summary.group.Freight[,c("group1","n","min","max")]
colnames(Summary.group.Freight)<-c("Country","n","Year min","Year max")
Summary.group[,c(1,2,3,4)]
Summary.group.Freight[,c(1,2,3,4)]
#xtable(Summary.group[,c(1,2,3,4)])




# -------------------------------------------------------------------------------------------------------------------------------------------------
names(BBDD_WB_SDG)
VARS.TRANSPORT.BBDD_WB_SDG<-c(
  "Country","Year",
  "WB-Railways, goods transported (million ton-km)",
  "WB-Railways, passengers carried (million passenger-km)"
)

#View(BBDD_WB_SDG[,VARS.TRANSPORT.BBDD_WB_SDG])


# ..................................................................................................................................................
# We select the transport database to use: BBDD_WB_WDI or BBDD.OECD.Goods.Passenger.Transport
# ..................................................................................................................................................

# -------------------------------------------------------------------------------------------------------------------------------------------------
# Con datos base WB
TRANSPORT.Data<-BBDD_WB_WDI[,VARS.TRANSPORT.BBDD_WB_WDI]
#View(TRANSPORT.Data)


# Con datos base Datos OECDE en lugar de WB
TRANSPORT.Data<-BBDD.OECD.Goods.Passenger.Transport[,VARS.TRANSPORT.BBDD.OECD.Goods.Passenger.Transport]


# ..................................................................................................................................................
# We FILL omitted values  for Belgium & Denmark
# ..................................................................................................................................................

rm(TRANSPORT.Data.Temp)
TRANSPORT.Data.Temp<-merge(BBDD.OECD.Goods.Passenger.Transport[,VARS.TRANSPORT.BBDD.OECD.Goods.Passenger.Transport],
                           BBDD_WB_WDI[,VARS.TRANSPORT.BBDD_WB_WDI],   by=c("Country","Year"), all=TRUE)

colnames(TRANSPORT.Data.Temp)

TRANSPORT.Data.Temp<-TRANSPORT.Data.Temp[,c("Country","Year","OECD-Rail passenger transport","WB-Railways, passengers carried (million passenger-km)",
                                         "OECD-Rail freight transport","WB-Railways, goods transported (million ton-km)")]


TRANSPORT.Data.Temp<-cbind(TRANSPORT.Data.Temp,TRANSPORT.Data.Temp[,"OECD-Rail passenger transport"]/TRANSPORT.Data.Temp[,"WB-Railways, passengers carried (million passenger-km)"])
TRANSPORT.Data.Temp<-cbind(TRANSPORT.Data.Temp,TRANSPORT.Data.Temp[,"OECD-Rail freight transport"]/TRANSPORT.Data.Temp[,"WB-Railways, goods transported (million ton-km)"])

TRANSPORT.Data.Temp<-cbind(TRANSPORT.Data.Temp,TRANSPORT.Data.Temp[,"OECD-Rail freight transport"]/TRANSPORT.Data.Temp[,"OECD-Rail passenger transport"])
TRANSPORT.Data.Temp<-cbind(TRANSPORT.Data.Temp,TRANSPORT.Data.Temp[,"WB-Railways, goods transported (million ton-km)"]/TRANSPORT.Data.Temp[,"WB-Railways, passengers carried (million passenger-km)"])

rm(NAMES.TRANSPORT.Temp)
NAMES.TRANSPORT.Temp<-colnames(TRANSPORT.Data.Temp)[1:6]
NAMES.TRANSPORT.Temp
NAMES.TRANSPORT.Temp[7]<-"Ratio passenger"
NAMES.TRANSPORT.Temp[8]<-"Ratio freight"
NAMES.TRANSPORT.Temp[9]<-"Ratio OCDE"
NAMES.TRANSPORT.Temp[10]<-"Ratio WB"
colnames(TRANSPORT.Data.Temp)<-NAMES.TRANSPORT.Temp

#View(TRANSPORT.Data.Temp)

Countries.2.Search<-c("Belgium","Denmark")
#View(TRANSPORT.Data.Temp[which(TRANSPORT.Data.Temp[,"Country"] %in% Countries.2.Search),])
#View(TRANSPORT.Data[which(TRANSPORT.Data[,"Country"]  %in% Countries.2.Search),])

# We change data for BELGIUM
# Belgium passenger transport. We set as value the one coming from BBDD_WB_WDI data base

TRANSPORT.Data[which(TRANSPORT.Data[,"Country"]=="Belgium" & 
                       TRANSPORT.Data[,"Year"]==2012),"OECD-Rail passenger transport"]<-10848


# Belgium freight transport
for(year in 2012:2016)
{
  TRANSPORT.Data[which(TRANSPORT.Data[,"Country"]=="Belgium" & 
                         TRANSPORT.Data[,"Year"]==year),"OECD-Rail freight transport"]<-
                         TRANSPORT.Data[which(TRANSPORT.Data[,"Country"]=="Belgium" &
                                                TRANSPORT.Data[,"Year"]==year),"OECD-Rail passenger transport"]*0.605
}
  
View(TRANSPORT.Data.Temp[which(TRANSPORT.Data.Temp[,"Country"] %in% Countries.2.Search),])
View(TRANSPORT.Data[which(TRANSPORT.Data[,"Country"]  %in% Countries.2.Search),])

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -------------------------------------------------------------------------------------------------------------------------------------------------
# TRANSPORT: End
# -------------------------------------------------------------------------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~






# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ............................................................................................................
# OECD TRANSPORT DATA: NEW DATA 17-11-2019
# BEGIN
# ............................................................................................................
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#-----------------------------------------------------------------------------------
# We rearrange all variables from BBDD_OECD_Passenger_Transport

names(BBDD_OECD_Passenger_Transport)
rm(BBDD_OECD_Passenger_Transport.Cleaned)

OECD.PT.Passenger.Total<-names(BBDD_OECD_Passenger_Transport)[grepl("Total", ignore.case=TRUE, names(BBDD_OECD_Passenger_Transport))]
OECD.PT.Passenger<-names(BBDD_OECD_Passenger_Transport)[grepl("Passenger-kilometres", ignore.case=TRUE, names(BBDD_OECD_Passenger_Transport))&
                                                          !grepl("Total", ignore.case=TRUE, names(BBDD_OECD_Passenger_Transport))]

BBDD_OECD_Passenger_Transport.Cleaned<-BBDD_OECD_Passenger_Transport[,c("Country","Year",OECD.PT.Passenger.Total,OECD.PT.Passenger)]
names(BBDD_OECD_Passenger_Transport.Cleaned)
#View(BBDD_OECD_Passenger_Transport)


#-----------------------------------------------------------------------------------
# We rearrange all variables from BBDD_OECD_Goods_Transport

names(BBDD_OECD_Goods_Transport)
rm(BBDD_OECD_Goods_Transport.Cleaned)

OECD.GT.Tonnes.Total<-names(BBDD_OECD_Goods_Transport)[grepl("Total", ignore.case=TRUE, names(BBDD_OECD_Goods_Transport))&
                                                         grepl("Tonnes-kilometres", ignore.case=TRUE, names(BBDD_OECD_Goods_Transport))]
OECD.GT.Tonnes<-names(BBDD_OECD_Goods_Transport)[grepl("Tonnes-kilometres", ignore.case=TRUE, names(BBDD_OECD_Goods_Transport))&
                                                   !grepl("Total", ignore.case=TRUE, names(BBDD_OECD_Goods_Transport))]

OECD.GT.NotTonnes<-names(BBDD_OECD_Goods_Transport)[!grepl("Tonnes-kilometres|Country|Year|Total", ignore.case=TRUE, names(BBDD_OECD_Goods_Transport))]

BBDD_OECD_Goods_Transport.Cleaned<-BBDD_OECD_Goods_Transport[,c("Country","Year",OECD.GT.Tonnes.Total,OECD.GT.Tonnes,OECD.GT.NotTonnes)]
names(BBDD_OECD_Goods_Transport.Cleaned)
#View(BBDD_OECD_Goods_Transport)

#-----------------------------------------------------------------------------------
# We rearrange all variables from BBDD_OECD_STI_Traffic

names(BBDD_OECD_STI_Traffic)
rm(BBDD_OECD_STI_Traffic.Cleaned)

OECD.STI.Rail.Total<-names(BBDD_OECD_STI_Traffic)[grepl("Total", ignore.case=TRUE, names(BBDD_OECD_STI_Traffic))&
                                                    grepl("rail", ignore.case=TRUE, names(BBDD_OECD_STI_Traffic))&
                                                    grepl("Tonnes-kilometres", ignore.case=TRUE, names(BBDD_OECD_STI_Traffic))]

OECD.STI.Rail<-names(BBDD_OECD_STI_Traffic)[grepl("rail", ignore.case=TRUE, names(BBDD_OECD_STI_Traffic))&
                                              !grepl("Total", ignore.case=TRUE, names(BBDD_OECD_STI_Traffic))]


OECD.STI.Road.Total<-names(BBDD_OECD_STI_Traffic)[grepl("Total", ignore.case=TRUE, names(BBDD_OECD_STI_Traffic))&
                                                    grepl("road", ignore.case=TRUE, names(BBDD_OECD_STI_Traffic))&
                                                    grepl("Tonnes-kilometres", ignore.case=TRUE, names(BBDD_OECD_STI_Traffic))]

OECD.STI.Road<-names(BBDD_OECD_STI_Traffic)[grepl("road", ignore.case=TRUE, names(BBDD_OECD_STI_Traffic))&
                                              !grepl("Total", ignore.case=TRUE, names(BBDD_OECD_STI_Traffic))]


OECD.STI.waterways.Total<-names(BBDD_OECD_STI_Traffic)[grepl("Total", ignore.case=TRUE, names(BBDD_OECD_STI_Traffic))&
                                                         grepl("waterways", ignore.case=TRUE, names(BBDD_OECD_STI_Traffic))&
                                                         grepl("Tonnes-kilometres", ignore.case=TRUE, names(BBDD_OECD_STI_Traffic))]

OECD.STI.waterways<-names(BBDD_OECD_STI_Traffic)[grepl("waterways", ignore.case=TRUE, names(BBDD_OECD_STI_Traffic))&
                                                   !grepl("Total", ignore.case=TRUE, names(BBDD_OECD_STI_Traffic))]

OECD.STI.Passenger<-names(BBDD_OECD_STI_Traffic)[grepl("Passenger-kilometres|Vehicle-kilometres", ignore.case=TRUE, names(BBDD_OECD_STI_Traffic))]


BBDD_OECD_STI_Traffic.Cleaned<-BBDD_OECD_STI_Traffic[,c("Country","Year",
                                                        OECD.STI.Passenger,
                                                        OECD.STI.Rail.Total,OECD.STI.Rail,
                                                        OECD.STI.Road.Total,OECD.STI.Road,
                                                        OECD.STI.waterways.Total,OECD.STI.waterways)]
names(BBDD_OECD_STI_Traffic.Cleaned)
#View(BBDD_OECD_STI_Traffic)



#-----------------------------------------------------------------------------------
# Variables to exclude and rearrange from BBDD_OECD_Road_PI


names(BBDD_OECD_Road_PI)
rm(BBDD_OECD_Road_PI.Cleaned)
#BBDD_OECD_Road_PI.Cleaned<-BBDD_OECD_Road_PI
BBDD_OECD_Road_PI.Cleaned<-BBDD_OECD_Road_PI

# Variables to exclude from BBDD_OECD_Road_PI
OECD.Road.PI.remove<-names(BBDD_OECD_Road_PI)[grepl("fatalities|emissions|deliveries|Country|Year", ignore.case=TRUE, names(BBDD_OECD_Road_PI))]

# Variables to rearrange from BBDD_OECD_Road_PI
OECD.Road.PI.Share.roads<-names(BBDD_OECD_Road_PI)[grepl("in total road network", ignore.case=TRUE, names(BBDD_OECD_Road_PI))]
OECD.Road.PI.Share.infrastructure<-names(BBDD_OECD_Road_PI)[grepl("infrastructure", ignore.case=TRUE, names(BBDD_OECD_Road_PI))]
OECD.Road.PI.Share.registrations<-names(BBDD_OECD_Road_PI)[grepl("registrations", ignore.case=TRUE, names(BBDD_OECD_Road_PI))]
OECD.Road.PI.Share.passenger.transport<-names(BBDD_OECD_Road_PI)[grepl("passenger transport", ignore.case=TRUE, names(BBDD_OECD_Road_PI))&
                                                                   grepl("share", ignore.case=TRUE, names(BBDD_OECD_Road_PI))]

BBDD_OECD_Road_PI.Cleaned<-BBDD_OECD_Road_PI.Cleaned[,!(names(BBDD_OECD_Road_PI.Cleaned) %in% OECD.Road.PI.remove)]
BBDD_OECD_Road_PI.Cleaned<-BBDD_OECD_Road_PI.Cleaned[,!(names(BBDD_OECD_Road_PI.Cleaned) %in% OECD.Road.PI.Share.roads)]
BBDD_OECD_Road_PI.Cleaned<-BBDD_OECD_Road_PI.Cleaned[,!(names(BBDD_OECD_Road_PI.Cleaned) %in% OECD.Road.PI.Share.infrastructure)]
BBDD_OECD_Road_PI.Cleaned<-BBDD_OECD_Road_PI.Cleaned[,!(names(BBDD_OECD_Road_PI.Cleaned) %in% OECD.Road.PI.Share.registrations)]
BBDD_OECD_Road_PI.Cleaned<-BBDD_OECD_Road_PI.Cleaned[,!(names(BBDD_OECD_Road_PI.Cleaned) %in% OECD.Road.PI.Share.passenger.transport)]

BBDD_OECD_Road_PI.Cleaned<-BBDD_OECD_Road_PI[,c("Country","Year",OECD.Road.PI.Share.roads,names(BBDD_OECD_Road_PI.Cleaned),
                                                OECD.Road.PI.Share.passenger.transport,OECD.Road.PI.Share.registrations,
                                                OECD.Road.PI.Share.infrastructure)]
names(BBDD_OECD_Road_PI.Cleaned)
View(BBDD_OECD_Road_PI.Cleaned)


#-----------------------------------------------------------------------------------
# We rearrange data

names(BBDD_OECD_Rail)
rm(BBDD_OECD_Rail.Cleaned)
OECD.Rail.remove<-names(BBDD_OECD_Rail)[grepl("electrified|CO2", ignore.case=TRUE, names(BBDD_OECD_Rail))]
BBDD_OECD_Rail.Cleaned<-BBDD_OECD_Rail[,!(names(BBDD_OECD_Rail) %in% OECD.Rail.remove)]
names(BBDD_OECD_Rail.Cleaned)


#-----------------------------------------------------------------------------------
#...............................................
# We merge all transport data

rm(TRANSPORT.Data.Cleaned)
TRANSPORT.Data.Cleaned<-merge(BBDD_OECD_Passenger_Transport.Cleaned, BBDD_OECD_Goods_Transport.Cleaned,   by=c("Country","Year"), all=TRUE)
TRANSPORT.Data.Cleaned<-merge(TRANSPORT.Data.Cleaned, BBDD_OECD_STI_Traffic.Cleaned,   by=c("Country","Year"), all=TRUE)
TRANSPORT.Data.Cleaned<-merge(TRANSPORT.Data.Cleaned, BBDD_OECD_Road_PI.Cleaned,   by=c("Country","Year"), all=TRUE)
TRANSPORT.Data.Cleaned<-merge(TRANSPORT.Data.Cleaned, BBDD_OECD_Rail.Cleaned,   by=c("Country","Year"), all=TRUE)

names(TRANSPORT.Data.Cleaned)
#View(TRANSPORT.Data.Cleaned)



#Names.Transport.Temp<-names(TRANSPORT.Data.Cleaned)[grepl("road", ignore.case=TRUE, names(TRANSPORT.Data.Cleaned))&
#                                grepl("passenger", ignore.case=TRUE, names(TRANSPORT.Data.Cleaned))]


#View(TRANSPORT.Data.Cleaned[,c("Country","Year",Names.Transport.Temp)])

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ............................................................................................................
# OECD TRANSPORT DATA: NEW DATA 17-11-2019
# END
# ............................................................................................................
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ............................................................................................................
# WORLD BANK DATA: NEW DATA 17-11-2019
# BEGIN
# ............................................................................................................
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

names(BBDD_WB_WDI)

WB.WDI.Logistics.names<-names(BBDD_WB_WDI)[grepl("Logistics", ignore.case=TRUE, names(BBDD_WB_WDI))]
WB.WDI.Population.names<-names(BBDD_WB_WDI)[grepl("population", ignore.case=TRUE, names(BBDD_WB_WDI))&
                                         !(grepl("force", ignore.case=TRUE, names(BBDD_WB_WDI)))]
  
rm(WB.WDI.Logistics.Data)
WB.WDI.Logistics.Data<-BBDD_WB_WDI[,c("Country","Year",WB.WDI.Logistics.names)]
  
rm(WB.WDI.Population.Data)
WB.WDI.Population.Data<-BBDD_WB_WDI[,c("Country","Year",WB.WDI.Population.names)]


# *************************************************************************************************************************************************
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -------------------------------------------------------------------------------------------------------------------------------------------------
# FINAL DATA SELECTED FOR THE REGRESSION. THIS IS POTENTIAL DATA TO BE USED
# -------------------------------------------------------------------------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# *************************************************************************************************************************************************
rm(REGRESSION.Data)
rm(Y.Data)
rm(X.Data)
Y.Data<-FUEL.CONSUMPTION.Data

#View(Y.Data)
#View(FUEL.CONSUMPTION.Data)

#X.Data<-merge(INFRASTRUCTURE.Data, TRANSPORT.Data,   by=c("Country","Year"), all=TRUE)

#View(OIL.PRICES.Data)
#View(ME.AGGREGATES.Data)
#View(TRANSPORT.Data)
#View(POPULATION.Data)

X.Data<-merge(ME.AGGREGATES.Data, OIL.PRICES.Data,  by=c("Country","Year"), all=TRUE)
X.Data<-merge(X.Data, TRANSPORT.Data,  by=c("Country","Year"), all=TRUE)
X.Data<-merge(X.Data, POPULATION.Data,  by=c("Country","Year"), all=TRUE)
X.Data<-merge(X.Data, LOGISTICS.Data,  by=c("Country","Year"), all=TRUE)
X.Data<-merge(X.Data, INFRASTRUCTURE.Data,  by=c("Country","Year"), all=TRUE)

# View(X.Data)
# Rail passenger transport en TRANSPORT.Data  incompleta para US

dim(X.Data)
dim(na.omit(X.Data))
#X.Data<-na.omit(X.Data)
#X.Temp<-(X.Data[which(X.Data[,"Country"] %in% Countries.FINAL.QA),])
#dim(X.Temp)
#dim(na.omit(Y.Data))
# View((na.omit(X.Data)))
#View((na.omit(Y.Data)))
rm(REGRESSION.Data)
REGRESSION.Data<-merge(Y.Data, X.Data,  by=c("Country","Year"), all=FALSE)
REGRESSION.Data[,"Country"]<-factor(REGRESSION.Data[,"Country"])
REGRESSION.Data[,"Year"]<-as.numeric(REGRESSION.Data[,"Year"])

ls.str(REGRESSION.Data)
str(REGRESSION.Data)
dim(REGRESSION.Data)
dim(na.omit(REGRESSION.Data))

# If we implement a REGRESSION.Data<-na.omit(REGRESSION.Data) before selecting the variables we will loose
# significant data for the regression

View(na.omit(REGRESSION.Data))
names(REGRESSION.Data)
levels(droplevels(REGRESSION.Data$Country))


Actual.Individual.Consumption.xCapita<-REGRESSION.Data[,"OECD-GDP per capita, at constant 2010 prices and PPPs, US dollars //*//-GDPHVPVOB"]*
  REGRESSION.Data[,"OECD-Actual individual consumption, percentage of GDP //*//-P41S" ]/100

#Actual.Individual.Consumption.xCapita<-REGRESSION.Data[,"OECD-GDP at 2010 constant prices and PPPs, billions US dollars //*//-GDPVPVOB"]

REGRESSION.Data<-cbind(REGRESSION.Data, Actual.Individual.Consumption.xCapita)
dim(REGRESSION.Data)


#REGRESSION.Data.First.Paper<-REGRESSION.Data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .................................................................................................................................................
# CORRELATION ASSESSMENT AMONG VARIABLES
# http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r
# .................................................................................................................................................
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ...............................................................................................................................................
# We assess correlation among FUEL PRICES
# ...............................................................................................................................................


colnames(REGRESSION.Data)

cor(REGRESSION.Data[,"Actual.Individual.Consumption.xCapita"], 
    REGRESSION.Data[,"OECD-GDP at 2010 constant prices and PPPs, billions US dollars //*//-GDPVPVOB"],
     method = c("pearson"))

cor(REGRESSION.Data[,"% Growth GDP"], 
    REGRESSION.Data[,"OECD-GDP at 2010 constant prices and PPPs, billions US dollars //*//-GDPVPVOB"],
    method = c("pearson"))

cor.test(REGRESSION.Data[,"Actual.Individual.Consumption.xCapita"], 
    REGRESSION.Data[,"OECD-GDP at 2010 constant prices and PPPs, billions US dollars //*//-GDPVPVOB"],
    method = c("pearson", "kendall", "spearman"))

cor(REGRESSION.Data[,"OECD-Rail passenger transport"], 
         REGRESSION.Data[,"Actual.Individual.Consumption.xCapita"],
         method = c("pearson", "kendall", "spearman"))

cor(REGRESSION.Data[,"OECD-Rail freight transport"], 
    REGRESSION.Data[,"OECD-GDP at 2010 constant prices and PPPs, billions US dollars //*//-GDPVPVOB"],
    method = c("pearson", "kendall", "spearman"))

cor(REGRESSION.Data[,"OECD-Rail passenger transport"], 
    REGRESSION.Data[,"OECD-GDP at 2010 constant prices and PPPs, billions US dollars //*//-GDPVPVOB"],
    method = c("pearson", "kendall", "spearman"))




# Correlation for Diesel and Gasoline prices
colnames(BBDD.OECD.Oil.Prices)
Prices.Temp<-BBDD.OECD.Oil.Prices[,c("Country","Year",
"OECD-Automotive diesel (litre)-Households-Total price (USD/unit using PPP)", 
"OECD-Premium leaded gasoline (litre)-Households-Total price (USD/unit using PPP)" ,
"OECD-Premium unleaded 98 RON (litre)-Households-Total price (USD/unit using PPP)",
"OECD-Regular leaded gasoline (litre)-Households-Total price (USD/unit using PPP)",
"OECD-Regular unleaded gasoline (litre)-Households-Total price (USD/unit using PPP)")]

View(Prices.Temp)

colnames(Prices.Temp)

colnames(Prices.Temp)<-c("Country","Year","Diesel","PLG","PUG","RLG","RUG")

Prices.Temp.1<-na.omit(Prices.Temp[,c("Diesel","PLG")])
cor(Prices.Temp.1)
dim(Prices.Temp.1)

Prices.Temp.2<-na.omit(Prices.Temp[,c("Diesel","PUG")])
cor(Prices.Temp.2)

Prices.Temp.3<-na.omit(Prices.Temp[,c("Diesel","RLG")])
cor(Prices.Temp.3)

Prices.Temp.4<-na.omit(Prices.Temp[,c("Diesel","RUG")])
cor(Prices.Temp.4)[,2][1]

rm(Correlation.Matrix)
Correlation.Matrix<-data.frame("Premium unleaded 98 RON"=cor(Prices.Temp.2)[,2][1],
                               "Regular unleaded gasoline"=cor(Prices.Temp.4)[,2][1],
                               "Premium leaded gasoline"=cor(Prices.Temp.1)[,2][1],
                               "Regular leaded gasoline"=cor(Prices.Temp.3)[,2][1])

Correlation.Matrix
xtable(Correlation.Matrix)

# ...............................................................................................................................................
# We assess correlation among REGRESSORS
# ...............................................................................................................................................

rm(CORRELATION.Data)
colnames(REGRESSION.Data)
CORRELATION.Data<-log(REGRESSION.Data[,c(9,7,10:dim(REGRESSION.Data)[2],4,5)])

CORRELATION.Data.cor<-na.omit(CORRELATION.Data)


colnames(CORRELATION.Data)
# colnames(CORRELATION.Data)<-c("C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14")
colnames(CORRELATION.Data)<-c("Diesel Price","GDP/capita","Rail Pass","Rail Freight","% Urb>1M","% Urb Pop","Pop Dens",
                              "AIC/capita","AIC","GDP")
#colnames(REGRESSION.Data)
cor(CORRELATION.Data.cor)[c(1,8),]
cor(CORRELATION.Data.cor)

xtable(cor(CORRELATION.Data.cor))

colnames(REGRESSION.Data)

# if we include correlation data for %GDP
CORRELATION.Data<-cbind(CORRELATION.Data,REGRESSION.Data[,8])
colnames(CORRELATION.Data)<-c("Diesel Price","GDP/capita","Rail Pass","Rail Freight","% Urb>1M","% Urb Pop","Pop Dens",
                              "AIC/capita","AIC","GDP","%GDP")
colnames(CORRELATION.Data)
cor(CORRELATION.Data)

# ...............................................................................................................................................


rm(REGRESSION.Data.PLOT)
REGRESSION.Data.PLOT<-cbind(REGRESSION.Data[,"Country"],CORRELATION.Data)
colnames(REGRESSION.Data.PLOT)<-c("Country","Diesel_Price","GDP_capita","Rail_Pass","Rail_Freight","%_Urb_>1M","%_Urb_Pop",
                              "Pop_Dens","AIC_capita","AIC","GDP","%_GDP")

# We check it out for spurious data
# We create a new structure without hidden internal structure data which impairs plm calculus
# REGRESSION.Data.PLOT<-fClean.Structure(REGRESSION.Data.PLOT)

REGRESSION.Data.PLOT[,"Country"]<-droplevels(REGRESSION.Data.PLOT[,1])

scatterplot(GDP ~ Rail_Freight ,data= REGRESSION.Data.PLOT)
plot(GDP ~ Rail_Freight  ,data= REGRESSION.Data.PLOT  )
xyplot(GDP ~ Rail_Freight|Country,
       data =  REGRESSION.Data.PLOT,
       type = "b",
       xlab = "Rail Freight",
       ylab = "GDP")


# ...............................................................................................................................................



# *************************************************************************************************************************************************
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# We select final data here
# 21-07-2021
# BEGIN:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# *************************************************************************************************************************************************

# OECD: Road fuel Consumption
names(BBDD.TOTAL.ROAD.Fuel.Consumption)

# OECD: Fuel Prices
names(BBDD.OECD.Oil.Prices)


# OECD: Gathered data for transport
names(TRANSPORT.Data.Cleaned)

# OECD: Gathered Macro Aggregates for transport
names(ME.AGGREGATES.Data)

# WORLD BANK: Gathered data from World Bank

# WORLD BANK: World Development Indicators

names(BBDD_WB_WDI)
names(WB.WDI.Logistics.Data)
names(WB.WDI.Population.Data)
WB.WDI.Logistics

# We merge all data
rm(REGRESSION.Data.New)
REGRESSION.Data.New<-merge(BBDD.TOTAL.ROAD.Fuel.Consumption, BBDD.OECD.Oil.Prices,  by=c("Country","Year"), all=TRUE)
REGRESSION.Data.New<-merge(REGRESSION.Data.New, ME.AGGREGATES.Data,  by=c("Country","Year"), all=TRUE)
REGRESSION.Data.New<-merge(REGRESSION.Data.New, WB.WDI.Population.Data,  by=c("Country","Year"), all=TRUE)
REGRESSION.Data.New<-merge(REGRESSION.Data.New, TRANSPORT.Data.Cleaned,  by=c("Country","Year"), all=TRUE)
REGRESSION.Data.New<-merge(REGRESSION.Data.New, WB.WDI.Logistics.Data,  by=c("Country","Year"), all=TRUE)
REGRESSION.Data.New<-merge(REGRESSION.Data.New, BBDD_WEF.GCI.Transport,  by=c("Country","Year"), all=TRUE)
dim(REGRESSION.Data.New)

names(REGRESSION.Data.New)
View(REGRESSION.Data.New)
str(REGRESSION.Data.New)


file.writing.SFA.Final.Data<-paste(BBDD.FINAL.writing_directory, "SFA-TRANSPORT-FINAL-Data.csv",sep="")
write.csv(REGRESSION.Data.New, file = file.writing.SFA.Final.Data, sep=";", dec=".", col.names="TRUE")




REGRESSION.Data.Vars.Selected<-c("Country", "Year",
                                 "Total Road Fuel Consumption (kt)",
                                 "OECD-Automotive diesel (litre)-Households-Total price (USD/unit using PPP)",
                                 "OECD-GDP at 2010 constant prices and PPPs, billions US dollars //*//-GDPVPVOB",
                                 "% Growth GDP",
                                 "OECD~TM~PT-Rail passenger transport /-/ Passenger-kilometres",
                                 "OECD~TM~FT-Rail freight transport /-/ Tonnes-kilometres",
                                 "WB~WDI-Population in urban agglomerations of more than 1 million (% of total population)",
                                 "WB~WDI-Urban population (% of total population)",
                                 "WB~WDI-Population density (people per sq. km of land area)")

dim(REGRESSION.Data.New[,REGRESSION.Data.Vars.Selected])
REGRESSION.Data.New.End<-na.omit(REGRESSION.Data.New[,REGRESSION.Data.Vars.Selected])
dim(REGRESSION.Data.New.End)
View(REGRESSION.Data.New.End)


# *************************************************************************************************************************************************
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# We select final data here
# 17-11-2019
# END:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# *************************************************************************************************************************************************














# .................................................................................................................................................
# *************************************************************************************************************************************************
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -------------------------------------------------------------------------------------------------------------------------------------------------
# BEGIN: DATA FINALLY SELECTED FOR PANEL DATA REGRESSIONS
# -------------------------------------------------------------------------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# *************************************************************************************************************************************************
rm(Y.PD.Regression)
rm(X.PD.Regression)
rm(Y.PD)
rm(X.PD)
rm(REGRESSION.Data.YEAR.Split)
#REGRESSION.Data.ORIG<-REGRESSION.Data

REGRESSION.Data.YEAR.Split<-REGRESSION.Data[which(REGRESSION.Data[,"Year"]>1995),]
#REGRESSION.Data.YEAR.Split<-REGRESSION.Data[which(REGRESSION.Data[,"Year"]>1991 & REGRESSION.Data[,"Year"]<2013),]
REGRESSION.Data.YEAR.Split<-REGRESSION.Data
#dim(REGRESSION.Data.YEAR.Split)

#Y.PD.Regression<-subset(REGRESSION.Data, select = c("TOTAL.ROAD.Fuel.Consumption"))
#Y.PD.Regression<-REGRESSION.Data.YEAR.Split[,c("TOTAL.ROAD.Fuel.Consumption")]

colnames(REGRESSION.Data.YEAR.Split)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DATA SELECTION
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DATA USED ON FIRST VERSION OF PAPER
# X.PD.Regression<-REGRESSION.Data[,c(8,7,9:13)]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#...............................................................
# Orig data without %GDP
      #X.PD.Regression<-REGRESSION.Data.YEAR.Split[,c(8,5,9,11:13)]
#REGRESSION.Data.YEAR.Split<-REGRESSION.Data.YEAR.Split[,c(1,2,3,9,5,10,12:14)]

# Data with %GDP
#REGRESSION.Data.YEAR.Split<-REGRESSION.Data.YEAR.Split[,c(1,2,3,9,5,8,10,12:14)]

# Data with %GDP and OECD-Rail freight transport
#REGRESSION.Data.YEAR.Split<-REGRESSION.Data.YEAR.Split[,c(1,2,3,9,5,8,10:14)]
      #X.PD.Regression<-REGRESSION.Data.YEAR.Split[,c(9,5,10,12:14)]

# Orig data without %GDP
REGRESSION.Data.Vars.Selected<-c("Country", "Year",
                                "Total Road Fuel Consumption (kt)",
                                "OECD-Automotive diesel (litre)-Households-Total price (USD/unit using PPP)",
                                "OECD-GDP at 2010 constant prices and PPPs, billions US dollars //*//-GDPVPVOB",
                                "OECD-Rail passenger transport",
                                "WB-Population in urban agglomerations of more than 1 million (% of total population)",
                                "WB-Urban population (% of total population)",
                                "WB-Population density (people per sq. km of land area)")

# Data with %GDP 
REGRESSION.Data.Vars.Selected<-c("Country", "Year",
                                "Total Road Fuel Consumption (kt)",
                                "OECD-Automotive diesel (litre)-Households-Total price (USD/unit using PPP)",
                                "OECD-GDP at 2010 constant prices and PPPs, billions US dollars //*//-GDPVPVOB",
                                "% Growth GDP",
                                "OECD-Rail passenger transport",
                                "WB-Population in urban agglomerations of more than 1 million (% of total population)",
                                "WB-Urban population (% of total population)",
                                "WB-Population density (people per sq. km of land area)")

# Data with %GDP and OECD-Rail freight transport
REGRESSION.Data.Vars.Selected<-c("Country", "Year",
                                "Total Road Fuel Consumption (kt)",
                                "OECD-Automotive diesel (litre)-Households-Total price (USD/unit using PPP)",
                                "OECD-GDP at 2010 constant prices and PPPs, billions US dollars //*//-GDPVPVOB",
                                "% Growth GDP",
                                "OECD-Rail passenger transport",
                                "OECD-Rail freight transport",
                                "WB-Population in urban agglomerations of more than 1 million (% of total population)",
                                "WB-Urban population (% of total population)",
                                "WB-Population density (people per sq. km of land area)")


# We select the variables
REGRESSION.Data.YEAR.Split<-REGRESSION.Data.YEAR.Split[,REGRESSION.Data.Vars.Selected]
colnames(REGRESSION.Data.YEAR.Split)
View(REGRESSION.Data.YEAR.Split)
                                



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
dim(REGRESSION.Data.YEAR.Split)
REGRESSION.Data.YEAR.Split<-na.omit(REGRESSION.Data.YEAR.Split)
dim(REGRESSION.Data.YEAR.Split)

X.PD.Regression<-REGRESSION.Data.YEAR.Split[,4:dim(REGRESSION.Data.YEAR.Split)[2]] 
# We check it out for spurious data
# We create a new structure without hidden internal structure data which impairs plm calculus
X.PD.Regression<-fClean.Structure(X.PD.Regression)

dim(X.PD.Regression)
colnames(X.PD.Regression)
dput(X.PD.Regression)

# X.PD.Regression<-cbind(X.PD.Regression, Actual.Individual.Consumption.xCapita)
#colnames(X.PD.Regression)
Y.PD.Regression<-REGRESSION.Data.YEAR.Split[,"Total Road Fuel Consumption (kt)"]
length(Y.PD.Regression)
# We check it out for spurious data
# dput(Y.PD.Regression)
Y.PD.Regression<-fClean.Structure(Y.PD.Regression)


rm(Summary.group)
Summary.group<-describeBy(REGRESSION.Data.YEAR.Split$Year,
                          group=droplevels(REGRESSION.Data.YEAR.Split$Country) ,mat=TRUE)
Summary.group<-Summary.group[,c("group1","n","min","max")]
colnames(Summary.group)<-c("Country","n","Year min","Year max")
#xtable(Summary.group[,c(1,2,3,4)])
Summary.group

#Summary.group.1978.1995<-Summary.group
#Summary.group.1996.2016<-Summary.group
#Summary.group.1978.2016<-Summary.group

rm(Summary.group.MERGED)
Summary.group.MERGED<-merge(Summary.group.1978.1995, Summary.group.1996.2016,  by=c("Country"), all=TRUE)
Summary.group.MERGED<-merge(Summary.group.MERGED, Summary.group.1978.2016,  by=c("Country"), all=TRUE)
Summary.group.MERGED
Summary.group.MERGED.Table<-xtable(Summary.group.MERGED[,c(1:10)], digits=0)
align(Summary.group.MERGED.Table) <- "ll|ccc|ccc|ccc|" 
Summary.group.MERGED.Table


#REGRESSION.Data<-na.omit(REGRESSION.Data)
#Countries.FINAL.QA<-levels(droplevels(REGRESSION.Data[,"Country"]))

# ............................................................................................................
# We write a file with final data REGRESSION.Data
file_writing_REGRESSION.Data<-paste(BBDD_FINAL_DATA_writing_directory, "BBDD_REGRESSION_Data.csv",sep="")
write.csv(REGRESSION.Data.YEAR.Split, file = file_writing_REGRESSION.Data)
# ............................................................................................................



# https://www.mail-archive.com/search?l=r-help@r-project.org&q=subject:%22Re%5C%3A+%5C%5BR%5C%5D+length+of+variable+in+mlogit%22&o=newest&f=1

# View(Y.PD.Regression)
# View(X.PD.Regression)

# summary(Y.PD.Regression)
# summary(X.PD.Regression)


#.....................................................................................................................
# In case we divide Total Vuel consumption by GDP

#GDP.Selection<-"OECD-Gross domestic product (expenditure approach) Constant prices, constant PPPs, OECD base year 2010 Millions US Dollar"
GDP.Selection<-"OECD-Actual individual consumption, at 2010 prices and PPPs, billions US dollars //*//-P41VPVOB"

DEPENDENT.Var<-paste("Total Road Fuel Consumption/","GDP cte prices, cte PPPs, OECD base year 2010 Millions US Dollar")

Y.PD.Div.GDP<-Y.PD.Regression/REGRESSION.Data.YEAR.Split[,GDP.Selection]
#Y.PD<-Y.PD.Regression/REGRESSION.Data.YEAR.Split[,GDP.Selection]
#.....................................................................................................................


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FINAL DATA USED FOR REGRESSION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# We change Y.PD.Regression and X.PD.Regression names here just in order to have shorter names variables for being used in the
# regression formulae
rm(Y.PD)
rm(X.PD)
rm(Y.PD.log)
rm(X.PD.log)

#Y.PD and X.PD are shorter names of Y.PD.Regression and X.PD.Regression for being 
#included in regressions in case we don't take logarithms

Y.PD<-Y.PD.Regression
X.PD<-X.PD.Regression

Y.PD.log<-log(Y.PD)
X.PD.log<-log(X.PD)
# View(X.PD.log)

# In case we use variable  % Growth GDP (% Growth GDP) we can not take log of this variable
# cause some values on being negative will provide NaN
rm(X.PD.log)
#X.PD.log<-log(subset(X.PD, select=-PERCTG.GDP))
colnames(X.PD)
X.PD.log<-log(X.PD[,-which(colnames(X.PD)=="% Growth GDP")])


X.PD.log<-cbind(X.PD.log,X.PD[,"% Growth GDP"])

colnames(X.PD.log)<-c("OECD-Automotive diesel (litre)-Households-Total price (USD/unit using PPP)",
  "OECD-GDP at 2010 constant prices and PPPs, billions US dollars //*//-GDPVPVOB",
  "OECD-Rail passenger transport",
  #"OECD-Rail freight transport",
  "WB-Population in urban agglomerations of more than 1 million (% of total population)",
  "WB-Urban population (% of total)",
  "WB-Population density (people per sq. km of land area)",
  "% Growth GDP")

X.PD.log<-X.PD.log[,c("OECD-Automotive diesel (litre)-Households-Total price (USD/unit using PPP)",
                      "OECD-GDP at 2010 constant prices and PPPs, billions US dollars //*//-GDPVPVOB",
                      "% Growth GDP",
                      "OECD-Rail passenger transport",
                      #"OECD-Rail freight transport",
                      "WB-Population in urban agglomerations of more than 1 million (% of total population)",
                      "WB-Urban population (% of total)",
                      "WB-Population density (people per sq. km of land area)")]


X.PD.log<-fClean.Structure(X.PD.log)
Y.PD.log<-fClean.Structure(Y.PD.log)

dim(X.PD.log)
length(Y.PD.log)

colnames(X.PD.log)
View(X.PD.log)
str(Y.PD)
str(X.PD)
cor(X.PD.log)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# *************************************************************************************************************************************************
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -------------------------------------------------------------------------------------------------------------------------------------------------
# END: FINAL DATA FOR PANEL DATA REGRESSIONS
# -------------------------------------------------------------------------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# *************************************************************************************************************************************************










# *************************************************************************************************************************************************
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------------------------------------------------------------------------------------------------------------------------
# BEGIN: PANEL DATA REGRESSIONS
# -------------------------------------------------------------------------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# *************************************************************************************************************************************************

colnames(X.PD.log)

xyplot(Y.PD.log ~ Year|Country,
       data =  pd.ROAD.Fuel.Data,
       type = "b",
       xlab = "Year",
       ylab = "log consumption")

#Panel Data Dimension
pdim(REGRESSION.Data.YEAR.Split, index=c("Country","Year"))

rm(Plm.REGRESSION.Data.YEAR.Split)
Plm.REGRESSION.Data.YEAR.Split<-cbind(REGRESSION.Data.YEAR.Split[,c("Country","Year")],Y.PD.log,X.PD.log)


# Data Set for function plm
rm(pd.ROAD.Fuel.Data)
#pd.ROAD.Fuel.Data <- plm.data(REGRESSION.Data.YEAR.Split, index=c("Country","Year"))
pd.ROAD.Fuel.Data <- plm.data(Plm.REGRESSION.Data.YEAR.Split, index=c("Country","Year"))
pdim(pd.ROAD.Fuel.Data)
str(pd.ROAD.Fuel.Data)

#pd.regression.fixed.1<-pd.regression.fixed


pd.regression.fixed<-plm(Y.PD.log ~  X.PD.log,  model= "within", 
                             index=c("Country","Year"), na.action=na.omit, data=pd.ROAD.Fuel.Data)

pd.regression.random<-plm(Y.PD.log ~ X.PD.log, model= "random", index=c("Country","Year"),
                          na.action=na.omit, data=pd.ROAD.Fuel.Data)

pd.regression.fd<-plm(Y.PD.log ~ X.PD.log, model= "fd", index=c("Country","Year"),
                          na.action=na.omit, data=pd.ROAD.Fuel.Data)


# Regression for General FGLS 
pd.regression.fixed.gmm<-pggls(Y.PD.log ~  X.PD.log  ,
                                   model= "within",  index=c("Country","Year"),
                                   na.action=na.omit, data=pd.ROAD.Fuel.Data)

summary(pd.regression.fixed.gmm.gdp)
pd.regression.fixed.gmm.gdp$coefficients
#pd.regression.fixed.gdp
#pd.regression.random.gdp


SS2<-stargazer( pd.regression.fixed, pd.regression.random,
           type="text",
           align=TRUE, ci.level=0.95, digits=5,
           column.labels=c("Fixed Effects","Random Effects","First Differences"),
           header=TRUE, no.space=TRUE,  single.row=TRUE,
           ci=TRUE, title="TITULO",
           dep.var.caption="DEPENDENT VARIABLE",
           dep.var.labels=DEPENDENT.Var,
           covariate.labels=colnames(X.PD),
           style="commadefault")
# An alternative way of presenting data
screenreg(list(pd.regression.fixed,
               pd.regression.random),
               digit=3, custom.model.names=c("FE","RE"))

# Data for GeneralFGLS Regression         
pd.regression.fixed.gmm.gdp<-pggls(Y.PD.log ~  X.PD.log  ,
                                   model= "within",  index=c("Country","Year"),
                                   na.action=na.omit, data=pd.ROAD.Fuel.Data)

summary(pd.regression.fixed.gmm.gdp)
pd.regression.fixed.gmm.gdp$coefficients



# Latex output
texreg(list(pd.regression.fixed, pd.regression.random ))
summary(pd.regression.random)


# Panel data RESIDUALS Normality Assesment
hist(residuals(pd.regression.fixed), breaks=50)
lines(density((residuals(pd.regression.fixed))),col=2)
qqnorm(residuals(pd.regression.fixed))
qqline(residuals(pd.regression.fixed), col = 2)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PANEL DATA TESTS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .............................................................................................................
# We test for random effects model vs fixed effects
# .............................................................................................................
# Hausman test:  null hypothesis is that the preferred model is random effects vs. 
# the alternative the fixed effects
# It basically tests whether the unique errors (ui) are correlated with
# the regressors, the null hypothesis is they are not
# if p<0.05 then use fixed effects
phtest(pd.regression.random, pd.regression.fixed)

coeftest(pd.regression.fixed)
coeftest(pd.regression.fixed, vcov.=vcovHC)

# .............................................................................................................
# Testing for time-fixed effects
# .............................................................................................................
pd.regression.fixed.gdp<-plm(Y.PD.log ~  X.PD.log ,
                              model= "within", index=c("Country","Year"),
                              na.action=na.omit, data=pd.ROAD.Fuel.Data)

pd.regression.pooling.gdp<-plm(Y.PD.log ~  X.PD.log +  factor(Year)   ,
                             model= "pooling", index=c("Country","Year"),
                             na.action=na.omit, data=pd.ROAD.Fuel.Data)

pd.regression.fixed.gdp.time<-plm(Y.PD.log ~  X.PD.log  ,
                             model= "within", effect = "time", index=c("Country","Year"),
                             na.action=na.omit, data=pd.ROAD.Fuel.Data)


screenreg(list(pd.regression.fixed.gdp,
               pd.regression.pooling.gdp, 
               pd.regression.fixed.gdp.time,
               pd.regression.fixed.gmm.gdp),               
          digit=7, custom.model.names=c("FE GDP","POOLING GDP","FE GDP TIME"))


pFtest(pd.regression.fixed.gdp.time, pd.regression.pooling.gdp)
# Time fixed effects should be used cause p-value = 4.095e-10

plmtest(pd.regression.pooling.gdp, effect=c("time"), type=("bp")) 
  
plmtest(pd.regression.fixed.gdp, effect=c("time"), type=("ghm")) 

fixef(pd.regression.fixed.gdp.time)


# We test for multicolinearity

pd.multicolinearity.pooling<-plm(X.PD.log[,7] ~  X.PD.log[,-7],
                               model= "pooling", na.action=na.omit, data=pd.ROAD.Fuel.Data)

multicolinearity.pooling<-plm(X.PD.log[,4] ~  X.PD.log[,1]+X.PD.log[,2]+X.PD.log[,3]+
                                X.PD.log[,5]+X.PD.log[,6]+X.PD.log[,7]
                             ,model= "pooling", data=pd.ROAD.Fuel.Data)

car::vif(multicolinearity.pooling)
VIF(pd.multicolinearity.pooling)


stargazer( pd.multicolinearity.pooling, 
           type="text",
           align=TRUE, ci.level=0.95, digits=3,
           column.labels=c("Multicolinearity"),
           header=TRUE, no.space=TRUE,  single.row=FALSE,
           ci=TRUE, title="TITULO",
           dep.var.caption="DEPENDENT VARIABLE",
           #dep.var.labels=DEPENDENT.Var,
           #covariate.labels=c("Beta 1","Beta 2","Beta 3","Beta 4","Beta 5","Beta 6","Beta 7"),
           style="commadefault")


stargazer( pd.regression.fixed.gdp, pd.regression.pooling.gdp, 
           type="text",
           align=TRUE, ci.level=0.95, digits=3,
           column.labels=c("Multicolinearity"),
           header=TRUE, no.space=TRUE,  single.row=FALSE,
           ci=TRUE, title="TITULO",
           dep.var.caption="DEPENDENT VARIABLE",
           #dep.var.labels=DEPENDENT.Var,
           #covariate.labels=c("Beta 1","Beta 2","Beta 3","Beta 4","Beta 5","Beta 6","Beta 7"),
           style="commadefault")



# .............................................................................................................
#
# .............................................................................................................


# .............................................................................................................
#
# .............................................................................................................


# .............................................................................................................
#
# .............................................................................................................



# ------------------------------------------------------------------------------------------------------------------------------
# ..............................................................................................................................
# Mixed effects regression
# ..............................................................................................................................
# ------------------------------------------------------------------------------------------------------------------------------

lme.regression.fixed = lmer(Y.PD.log ~ X.PD.log + Country + (1|Country),
                            data=pd.ROAD.Fuel.Data)


#pd.regression.fixed.1978.2016<-pd.regression.fixed
#lme.regression.fixed.1978.2016<-lme.regression.fixed
#pd.regression.random.1978.2016<-pd.regression.random

#pd.regression.fixed.1978.1995<-pd.regression.fixed
#lme.regression.fixed.1978.1995<-lme.regression.fixed
#pd.regression.random.1978.1995<-pd.regression.random


#pd.regression.fixed.1996.2016<-pd.regression.fixed
#lme.regression.fixed.1996.2016<-lme.regression.fixed
#pd.regression.random.1996.2016<-pd.regression.random



phtest(pd.regression.random.1978.2016, pd.regression.fixed.1978.2016)
phtest(pd.regression.random.1978.1995, pd.regression.fixed.1978.1995)
phtest(pd.regression.random.1996.2016, pd.regression.fixed.1996.2016)



stargazer( pd.regression.fixed.1978.2016, lme.regression.fixed.1978.2016, pd.regression.random.1978.2016,
           #pd.regression.fixed.1996.2016, lme.regression.fixed.1996.2016, pd.regression.random.1996.2016,
           #pd.regression.fixed.1978.1995, lme.regression.fixed.1978.1995, pd.regression.random.1978.1995,
           type="text",
           align=TRUE, ci.level=0.95, digits=3,
           column.labels=c("Fixed Effects","Mixed Effects", "Random Effects",
                          "Fixed Effects","Mixed Effects", "Random Effects",
                          "Fixed Effects","Mixed Effects", "Random Effects"),
           header=TRUE, no.space=TRUE,  single.row=FALSE,
           ci=TRUE, title="TITULO",
           dep.var.caption="DEPENDENT VARIABLE",
           #dep.var.labels=DEPENDENT.Var,
           covariate.labels=c("Beta 1","Beta 2","Beta 3","Beta 4","Beta 5","Beta 6","Beta 7"),
           style="commadefault")


stargazer( pd.regression.fixed.1978.2016, lme.regression.fixed.1978.2016, pd.regression.random.1978.2016,
           type="text",
           align=TRUE, ci.level=0.95, digits=3,
           column.labels=c("Fixed Effects","Mixed Effects", "Random Effects"),
           header=TRUE, no.space=TRUE,  single.row=FALSE,
           ci=TRUE, title="TITULO",
           dep.var.caption="DEPENDENT VARIABLE",
           #dep.var.labels=DEPENDENT.Var,
           covariate.labels=c("Beta 1","Beta 2","Beta 3","Beta 4","Beta 5","Beta 6","Beta 7"),
           style="commadefault")



texreg(list(pd.regression.fixed.1978.2016, lme.regression.fixed.1978.2016, pd.regression.random.1978.2016) ,
          digits=3, use.ci = TRUE, ci.force.level = 0.95, ci.force = c(TRUE, TRUE, TRUE), label = "tab:3")




screenreg(list(pd.regression.fixed.1978.2016, lme.regression.fixed.1978.2016, pd.regression.random.1978.2016),
          digits=3, use.ci = TRUE, ci.level = 0.95,  stars = c(0.1, 0.05, 0.01),  booktabs = TRUE,
          ci.force = c(TRUE, TRUE, TRUE))


# *************************************************************************************************************************************************
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------------------------------------------------------------------------------------------------------------------------
# END: PANEL DATA REGRESSIONS
# -------------------------------------------------------------------------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# *************************************************************************************************************************************************



# *************************************************************************************************************************************************
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------------------------------------------------------------------------------------------------------------------------
# BEGIN: QUANTILE PANEL DATA REGRESSIONS
# -------------------------------------------------------------------------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# *************************************************************************************************************************************************


# ------------------------------------------------------------------------------------------------------------------------------
# qrLMM: Quantile Regression for Linear Mixed-Effects Models
# ------------------------------------------------------------------------------------------------------------------------------


# rm(REGRESSION.Data.TEMP)
# colnames(REGRESSION.Data)
# REGRESSION.Data.TEMP<-cbind(REGRESSION.Data[,c(1,2)],
#                            log(REGRESSION.Data[,c(3:14)]))
# colnames(REGRESSION.Data.TEMP)

# NAMES.REGRESSION.Data.Temp<-colnames(REGRESSION.Data.TEMP)

# NAMES.REGRESSION.Data.Temp
# colnames(REGRESSION.Data.TEMP)<-c("Country","Year","Consumption","A4","A5","A6","A7","A8","A9","A10","A11","A12","A13","A14")
# colnames(REGRESSION.Data.TEMP)

# ...................................................................
# yy = REGRESSION.Data.TEMP[,"Consumption"]
# yy<- fClean.Structure(yy)
# xx = cbind(1,REGRESSION.Data.TEMP[,c("A4","A7","A8","A9","A10","A11","A12","A13")])
# xx = cbind(1,REGRESSION.Data.TEMP[,c("A8","A9","A10","A11","A12","A13","A5","A14")])
# .......................................................................................................
# Possibly we will have to remove Variable REGRESSION.Data.TEMP and substitute it by REGRESSION.Data
# I guess REGRESSION.Data.TEMP is not necessary


rm(REGRESSION.Data.TEMP)
REGRESSION.Data.TEMP<-REGRESSION.Data.YEAR.Split

rm(yy)
rm(xx)
rm(zz)
rm(groups)

yy = Y.PD.log
yy<- fClean.Structure(yy)
#xx = cbind(1,REGRESSION.Data.TEMP[,c("A4","A7","A8","A9","A10","A11","A12","A13")])

X.PD.log.Quantile<-X.PD.log

#colnames(X.PD.log.Quantile)<-c("A1","A2","A3","A4","A5","A6")
#colnames(X.PD.log.Quantile)<-c("A1","A2","A3","A4","A5","A6","A7")
colnames(X.PD.log.Quantile)<-c("A1","A2","A3","A4","A5","A6","A7","A8")
xx = cbind(1,X.PD.log.Quantile)

xx<-fClean.Structure(xx)
groups <-droplevels(REGRESSION.Data.TEMP[,"Country"])
zz<-cbind(rep(1,length(groups)))

dim(xx)
length(yy)
length(zz)

#zz<-cbind(rep(1,626))
rm(QUANTILE.Regression)
QUANTILE.Regression=QRLMM(yy,xx,zz,groups, p = c(seq(0.05,0.95,0.05)),MaxIter=500,M=20)



#QUANTILE.Regression.Complete.2016.FREIGHT.2<-QUANTILE.Regression
#QUANTILE.Regression.Complete.2016.FREIGHT.1<-QUANTILE.Regression
#QUANTILE.Regression.Mayor.1996.2016.FREIGHT.1<-QUANTILE.Regression
#QUANTILE.Regression.Menor.1996.FREIGHT.1<-QUANTILE.Regression


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# With data till 2016
#QUANTILE.Regression.Complete.2016.1<-QUANTILE.Regression
#QUANTILE.Regression.Mayor.1995.2016.1<-QUANTILE.Regression
#QUANTILE.Regression.Menor.1996.2016.1<-QUANTILE.Regression
#QUANTILE.Regression.Complete.2016.2<-QUANTILE.Regression
#QUANTILE.Regression.Menor.1996.2016.2<-QUANTILE.Regression
#QUANTILE.Regression.Mayor.1995.2016.2<-QUANTILE.Regression
#QUANTILE.Regression.Complete.2016.2<-QUANTILE.Regression

QUANTILE.Regression<-QUANTILE.Regression.Complete.2016.2
QUANTILE.Regression<-QUANTILE.Regression.Menor.1996.2016.2
QUANTILE.Regression<-QUANTILE.Regression.Mayor.1995.2016.2


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# With data till 2015
# .....................
#QUANTILE.Regression.Menor.1996.2<-QUANTILE.Regression
# .....................
#QUANTILE.Regression<-QUANTILE.Regression.Complete1
#QUANTILE.Regression<-QUANTILE.Regression.Complete.2
#QUANTILE.Regression<-QUANTILE.Regression.Complete.3
# .....................
#QUANTILE.Regression<-QUANTILE.Regression.Menor.1991
#QUANTILE.Regression<-QUANTILE.Regression.Menor.1996
#QUANTILE.Regression<-QUANTILE.Regression.Menor.1996.2
#QUANTILE.Regression<-QUANTILE.Regression.Menor.1996.3
# .....................
#QUANTILE.Regression<-QUANTILE.Regression.Mayor.1990
#QUANTILE.Regression<-QUANTILE.Regression.Mayor.1995
#QUANTILE.Regression<-QUANTILE.Regression.Mayor.1995.2
#QUANTILE.Regression<-QUANTILE.Regression.Mayor.1995.3
#.......................

#QUANTILE.Regression<-QUANTILE.Regression.Menor.1996.3


screenreg(list(pd.regression.fixed),digits=4)

#QUANTILE.GDP.Back.Up<-QUANTILE.Regression

# ---------------------------------------------------------------------------------------------------------------
# .............................................................................................................
# WE PLOT THE REBOUND EFFECT ESTIMATED
# .............................................................................................................
# ---------------------------------------------------------------------------------------------------------------

# https://www.safaribooksonline.com/library/view/r-graphics-cookbook/9781449363086/ch04.html

# .............................................................................................................
# WE PLOT BETA1 ESTIMATED
# .............................................................................................................


rm(sigma.quantile)

sigma.quantile<-data.frame()
for(i in 1:9)
{
  sigma.quantile[i,"Quantile"]<-0.1*i
  # Beta 2
  sigma.quantile[i,"Sigma"]<-QUANTILE.Regression[[i]]$res$sigma
}

sigma.quantile

ggplot(sigma.quantile, aes(x=Quantile, y=Sigma)) +
    geom_line()+
  scale_x_continuous(breaks=c( seq(0,1,0.05)))+
  #scale_y_continuous(breaks=c( seq(0,-0.20,-0.0125)))+
  #ggtitle("Rebound effect")
  labs(title="Sigma quantile distribution ")





#QUANTILE.Regression<-QUANTILE.Regression.Mayor.1990
#QUANTILE.Regression<-QUANTILE.Regression.Menor.1991
#QUANTILE.Regression<-QUANTILE.Regression.Menor.1996
#QUANTILE.Regression<-QUANTILE.Regression.Complete


for (i in 1:19)
{
  print("................................................................................")
  print(cat("............................ QUANTILE ",i*0.05," ........................................"))
  print("................................................................................")
  print(QUANTILE.Regression[[i]]$res$table)
}

#beta.quantile.Without.PERCTGDP<-beta.quantile



rm(beta.quantile)
beta.quantile<-data.frame()
for(i in 1:19)
{
  # Quantile
  beta.quantile[i,"Quantile"]<-0.05*i
  # Beta 2
  beta.quantile[i,"Estimate"]<-QUANTILE.Regression[[i]]$res$table[2,1]
  # Std Error
  beta.quantile[i,"Std_Error"]<-QUANTILE.Regression[[i]]$res$table[2,2]
  # Lim inf
  beta.quantile[i,"Inferior"]<-QUANTILE.Regression[[i]]$res$table[2,3]
  # Lim sup
  beta.quantile[i,"Superior"]<-QUANTILE.Regression[[i]]$res$table[2,4]
  #  Pr(>|z|)
  beta.quantile[i,"Pr_Z"]<-QUANTILE.Regression[[i]]$res$table[2,6]
}

beta.quantile


ggplot(beta.quantile, aes(x=Quantile, y=Estimate)) +
  geom_line(aes(y=Inferior), colour="grey50", linetype="dotted") +
  geom_line(aes(y=Superior), colour="grey50", linetype="dotted") +
  geom_ribbon(aes(ymin=Inferior, ymax=Superior),
              alpha=0.25, fill="red")+
  geom_line()+
  scale_x_continuous(breaks=c( seq(0,1,0.05)))+
  scale_y_continuous(breaks=c( seq(2,-0.20,-0.0125)))+
  #ggtitle("Rebound effect")
  labs(title="Road energy consumption rebound effect quantile distribution ")
  #geom_hline(yintercept = -0.0329 , colour = "grey50", size = 1, linetype="dashed")

#geom_smooth(span = 0.2)


# Shaded region
ggplot(beta.quantile, aes(x=Quantile, y=Estimate)) +
  geom_ribbon(aes(ymin=Inferior, ymax=Superior),
              alpha=0.3) +
  geom_line()+
  #stat_smooth()+
  scale_x_continuous(breaks=c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30,
                              0.35, 0.40, 0.45, 0.50, 0.55, 0.60,
                              0.65,0.70, 0.75, 0.80, 0.85, 0.90,
                              0.95))


# .............................................................................................................
# WE PLOT THE REBOUND EFFECT ESTIMATED
# .............................................................................................................

rm(rebound.quantile)
rebound.quantile<-data.frame()
for(i in 1:19)
{
  # Quantile
  rebound.quantile[i,"Quantile"]<-0.05*i
  # Beta 2
  rebound.quantile[i,"Estimate"]<-QUANTILE.Regression[[i]]$res$table[2,1]*(-1)*100
  # Std Error
  rebound.quantile[i,"Std_Error"]<-QUANTILE.Regression[[i]]$res$table[2,2]
  # Lim inf
  rebound.quantile[i,"Inferior"]<-QUANTILE.Regression[[i]]$res$table[2,3]*(-1)*100
  # Lim sup
  rebound.quantile[i,"Superior"]<-QUANTILE.Regression[[i]]$res$table[2,4]*(-1)*100
  #  Pr(>|z|)
  rebound.quantile[i,"Pr_Z"]<-QUANTILE.Regression[[i]]$res$table[2,6]
}

rebound.quantile

#https://stackoverflow.com/questions/13297995/changing-font-size-and-direction-of-axes-text-in-ggplot2
ggplot(rebound.quantile, aes(x=Quantile, y=Estimate)) +
  geom_line(aes(y=Inferior), colour="grey50", linetype="dotted") +
  geom_line(aes(y=Superior), colour="grey50", linetype="dotted") +
  geom_ribbon(aes(ymin=Inferior, ymax=Superior),
              alpha=0.25, fill="red")+
  geom_line(size=0.75)+
  scale_x_continuous(breaks=c( seq(0.05,0.95,0.05)))+
  scale_y_continuous(breaks=c( seq(-8,25,1)))+
  #ggtitle("Rebound effect")
  labs(title="Road fuel energy consumption 1978 - 1995",
       y="% Rebound Effect")+
    theme(axis.text.x = element_text(colour="grey20",size=18,angle=90,hjust=.5,vjust=.5,face="plain"),
          axis.title.x = element_text(colour="grey20",size=22,angle=0,hjust=.5,vjust=.5,face="plain"),
          axis.title.y = element_text(colour="grey20",size=22,angle=90,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="grey20",size=18,angle=0,hjust=1,vjust=0,face="plain"),
          text = element_text(size=16))
    

#geom_hline(yintercept = -0.0329 , colour = "grey50", size = 1, linetype="dashed")




# ---------------------------------------------------------------------------------------------------------------
# .............................................................................................................
# WE MAKE CSV TABLES WITH THE REBOUND EFFECT ESTIMATED
# IN ORDER TO CREATE A LATEX FILE
# .............................................................................................................
# ---------------------------------------------------------------------------------------------------------------

# ..............................................................
# We use this in order to creat a CSV file to ease the construction of
# a latex table
# ..............................................................
# This is for the betas

#NUMERO.BETAS<-8
NUMERO.BETAS<-9


rm(beta.quantile.ALL)
beta.quantile.ALL<-data.frame()
for(i in 1:9)
{
  for(j in 1:NUMERO.BETAS)
  {
    # Beta j
    beta.quantile.ALL[i,j]<-QUANTILE.Regression[[i*2]]$res$table[j,1]
  }
  # Quantile
  #beta.quantile.ALL[i,"Quantile"]<-0.1*i  
}

beta.quantile.ALL<-t(beta.quantile.ALL)

beta.quantile.ALL
file_writing_Betas<-paste(OUTCOMES_directory, "Quantile Betas.csv",sep="")
write.csv(beta.quantile.ALL, file = file_writing_Betas)

#创创创创创创创创创创创创创创创创创创创创创创创
# This is for the Standard Errors
rm(serror.quantile.ALL)
serror.quantile.ALL<-data.frame()
for(i in 1:9)
{
  
  for(j in 1:NUMERO.BETAS)
  {
    # Beta j
    serror.quantile.ALL[i,j]<-QUANTILE.Regression[[i*2]]$res$table[j,2]
  }
  # Quantile
  #serror.quantile.ALL[i,"Quantile"]<-0.1*i  
}

serror.quantile.ALL<-t(serror.quantile.ALL)

serror.quantile.ALL
file_writing_SE<-paste(OUTCOMES_directory, "Quantile SE.csv",sep="")
write.csv(serror.quantile.ALL, file = file_writing_SE)

#创创创创创创创创创创创创创创创创创创创创创创创
# This is for Probability>Z

rm(P.Z.quantile.ALL)
P.Z.quantile.ALL<-data.frame()
for(i in 1:9)
{
  
  for(j in 1:NUMERO.BETAS)
  {
    # Beta j
    P.Z.quantile.ALL[i,j]<-QUANTILE.Regression[[i*2]]$res$table[j,6]
  }
  # Quantile
  #P.Z.quantile.ALL[i,"Quantile"]<-0.1*i  
}

P.Z.quantile.ALL<-t(P.Z.quantile.ALL)

P.Z.quantile.ALL
file_writing_PZ<-paste(OUTCOMES_directory, "Quantile PZ.csv",sep="")
write.csv(P.Z.quantile.ALL, file = file_writing_PZ)



#创创创创创创创创创创创创创创创创创创创创创创创
# This is for AIC
rm(AIC.quantile.ALL)
AIC.quantile.ALL<-data.frame()
for(i in 1:9)
{
  AIC.quantile.ALL[i,"Quantile"]<-i
  AIC.quantile.ALL[i,"AIC"]<-QUANTILE.Regression[[i*2]]$res$AIC

}

AIC.quantile.ALL
file_writing_AIC<-paste(OUTCOMES_directory, "Quantile AIC.csv",sep="")
write.csv(AIC.quantile.ALL, file = file_writing_AIC)

#创创创创创创创创创创创创创创创创创创创创创创创
# This is for BIC
rm(BIC.quantile.ALL)
BIC.quantile.ALL<-data.frame()
for(i in 1:9)
{
  BIC.quantile.ALL[i,"Quantile"]<-i
  BIC.quantile.ALL[i,"BIC"]<-QUANTILE.Regression[[i*2]]$res$BIC
  
}

BIC.quantile.ALL
file_writing_BIC<-paste(OUTCOMES_directory, "Quantile BIC.csv",sep="")
write.csv(BIC.quantile.ALL, file = file_writing_BIC)
#创创创创创创创创创创创创创创创创创创创创创创创
# This is for loglik
rm(loglik.quantile.ALL)
loglik.quantile.ALL<-data.frame()
for(i in 1:9)
{
  loglik.quantile.ALL[i,"Quantile"]<-i
  loglik.quantile.ALL[i,"loglik"]<-QUANTILE.Regression[[i*2]]$res$loglik
  
}

loglik.quantile.ALL
file_writing_loglik<-paste(OUTCOMES_directory, "Quantile loglik.csv",sep="")
write.csv(loglik.quantile.ALL, file = file_writing_loglik)
# .............................................................................................................
# ---------------------------------------------------------------------------------------------------------------


# ..............................................................

QUANTILE.Regression[[1]]$res$table

# Quantiles 0.1 , 0.2, 0.3, 0.4 ............
QUANTILE.Regression[[2]]$res$table[,c(1,2,6)]
QUANTILE.Regression[[4]]$res$table[,c(1,2,6)]
QUANTILE.Regression[[6]]$res$table[,c(1,2,6)]
QUANTILE.Regression[[8]]$res$table[,c(1,2,6)]
QUANTILE.Regression[[10]]$res$table[,c(1,2,6)]
QUANTILE.Regression[[12]]$res$table[,c(1,2,6)]
QUANTILE.Regression[[14]]$res$table[,c(1,2,6)]
QUANTILE.Regression[[16]]$res$table[,c(1,2,6)]
QUANTILE.Regression[[18]]$res$table[,c(1,2,6)]

QUANTILE.Regression[[2]]$res$AIC
QUANTILE.Regression[[4]]$res$AIC
QUANTILE.Regression[[6]]$res$AIC
QUANTILE.Regression[[8]]$res$AIC
QUANTILE.Regression[[10]]$res$AIC
QUANTILE.Regression[[12]]$res$AIC
QUANTILE.Regression[[14]]$res$AIC
QUANTILE.Regression[[16]]$res$AIC
QUANTILE.Regression[[18]]$res$AIC

QUANTILE.Regression[[2]]$res$BIC
QUANTILE.Regression[[4]]$res$BIC
QUANTILE.Regression[[6]]$res$BIC
QUANTILE.Regression[[8]]$res$BIC
QUANTILE.Regression[[10]]$res$BIC
QUANTILE.Regression[[12]]$res$BIC
QUANTILE.Regression[[14]]$res$BIC
QUANTILE.Regression[[16]]$res$BIC
QUANTILE.Regression[[18]]$res$BIC


QUANTILE.Regression[[2]]$res$loglik
QUANTILE.Regression[[4]]$res$loglik
QUANTILE.Regression[[6]]$res$loglik
QUANTILE.Regression[[8]]$res$loglik
QUANTILE.Regression[[10]]$res$loglik
QUANTILE.Regression[[12]]$res$loglik
QUANTILE.Regression[[14]]$res$loglik
QUANTILE.Regression[[16]]$res$loglik
QUANTILE.Regression[[18]]$res$loglik


# Quantiles 0.05 , 0.15, 0.25, 0.35 ............
QUANTILE.Regression[[1]]$res$table[,c(1,2,6)]
QUANTILE.Regression[[3]]$res$table[,c(1,2,6)]
QUANTILE.Regression[[5]]$res$table[,c(1,2,6)]
QUANTILE.Regression[[7]]$res$table[,c(1,2,6)]
QUANTILE.Regression[[9]]$res$table[,c(1,2,6)]
QUANTILE.Regression[[11]]$res$table[,c(1,2,6)]
QUANTILE.Regression[[13]]$res$table[,c(1,2,6)]
QUANTILE.Regression[[15]]$res$table[,c(1,2,6)]
QUANTILE.Regression[[17]]$res$table[,c(1,2,6)]
QUANTILE.Regression[[19]]$res$table[,c(1,2,6)]


stargazer(QUANTILE.Regression[[19]]$res$table[,c(1,2,6)],
          summary=FALSE,
          type="latex", align=TRUE,  digits=3, colnames=TRUE,
          column.labels=c("M10","M20"),
          header=TRUE, no.space=FALSE,  single.row=FALSE,
          #title="Akaike Information Criteria and BIC",
          dep.var.caption="DEPENDENT VARIABLE",
          style="commadefault")

for(i in 8:9)
{
  #print(xtable())
  stargazer(QUANTILE.Regression[[i*2]]$res$table[,c(1,2,6)],
            summary=FALSE,
            type="latex", align=TRUE,  digits=3, colnames=TRUE,
            column.labels=c("M10","M20"),
            header=TRUE, no.space=FALSE,  single.row=FALSE,
            #title="Akaike Information Criteria and BIC",
            dep.var.caption="DEPENDENT VARIABLE",
            style="commadefault")
}



PRUEBAS2<-QUANTILE.Regression
# We compare models through the AKAIKE INFORMATION CRITERIA
for(i in 1:19)
{
  stargazer( c(PRUEBAS2[[i]]$res$AIC, PRUEBAS2[[i]]$res$BIC),
             summary=FALSE,
             type="text", align=TRUE,  digits=3, colnames=TRUE,
             column.labels=c("M10","M20"),
             header=TRUE, no.space=FALSE,  single.row=FALSE,
             title="Akaike Information Criteria and BIC",
             dep.var.caption="DEPENDENT VARIABLE",
             style="commadefault")
  #, covariate.labels=c("","Value","Std. Error","Pr(> | z| )"))
  #print("------------------------------------------------")
}



PRUEBAS3[[2]]$res$table[,c(1,2,6)]
PRUEBAS2[[2]]$res$table[,c(1,2,6)]


for(i in 1:3)
{
  stargazer( QUANTILE.Regression[[i]]$res$table[,c(1,2,6)],
             summary=FALSE,
             type="latex", align=TRUE,  digits=3, colnames=TRUE,
             column.labels=c("Fixed Effects","Random Effects","First Differences"),
             header=TRUE, no.space=FALSE,  single.row=TRUE,
             title="Covariate Estimates Quantile ",
             dep.var.caption="DEPENDENT VARIABLE",
             style="commadefault")
  #, covariate.labels=c("","Value","Std. Error","Pr(> | z| )"))
  
}

# http://www.statisticshowto.com/probability-and-statistics/hypothesis-testing/t-score-vs-z-score/


# ******************************************************************************************************************************
# ******************************************************************************************************************************
# ******************************************************************************************************************************

# ------------------------------------------------------------------------------------------------------------------------------

# lqmm: Laplace Quantile Mixed model

# ------------------------------------------------------------------------------------------------------------------------------

# ...........................................................................................

lqmm.Data<-Plm.REGRESSION.Data.YEAR.Split
colnames(lqmm.Data)
dim(lqmm.Data)
colnames(lqmm.Data)<-c("Country", "Year","Y.PD.log","A1","A2","A3","A4","A5","A6","A7")

# We change control parameters
lqmmControl(loop_max_iter = 800, loop_tol_ll = 1e-04, loop_tol_theta = 1e-02)

Rebound.lqmm<-lqmm(Y.PD.log ~ A1+A2+A3+A4+A5+A6+A7, random = ~ 1, group= Country,
                   tau=c(seq(0.1,0.9, by=0.1)), type="robust", nK=11, 
                   data=lqmm.Data )

Rebound.lqmm$control$LP_tol_ll <- 1e-4
Rebound.lqmm$control$LP_max_iter <- 4000
summary(Rebound.lqmm, R = 100, seed = 52)

# it is better to use type="robust" (Laplace random effects) than type="normal" (Gaussian random effects)

screenreg(list(Rebound.lqmm),digits=3)
texreg(list(Rebound.lqmm),digits=3,
       caption = "Multiple model types, custom names, and single row.")
       #custom.model.names = "lqmm model")

fit.boot <- boot(Rebound.lqmm)
str(fit.boot)
# ...........................................................................................


# *************************************************************************************************************************************************
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------------------------------------------------------------------------------------------------------------------------
# END: QUANTILE PANEL DATA REGRESSIONS
# -------------------------------------------------------------------------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# *************************************************************************************************************************************************









# *************************************************************************************************************************************************
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -------------------------------------------------------------------------------------------------------------------------------------------------
# NORMALITY: TESTS & STATISTICS
# BEGINNING
# *************************************************************************************************************************************************
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -------------------------------------------------------------------------------------------------------------------------------------------------
# https://stats.stackexchange.com/questions/3136/how-to-perform-a-test-using-r-to-see-if-data-follows-normal-distribution

# -------------------------------------------------------------------------------------------------------------------------------------------------
# VARIABLE Y
# -------------------------------------------------------------------------------------------------------------------------------------------------

# Density distributions for every Country
rm(REGRESSION.Data.Plot.Hist)
REGRESSION.Data.Plot.Hist<-REGRESSION.Data.YEAR.Split[,c("Country","Year","TOTAL.ROAD.Fuel.Consumption")]
head(REGRESSION.Data.Plot.Hist)
names(REGRESSION.Data.Plot.Hist)
# We transform data structure in order to plot an histogram panel with function multi.hist
REGRESSION.Data.Plot<-fGenBBDDPanel.2.Histogram(REGRESSION.Data.Plot.Hist, REGRESSION.Data.Plot)

# normal fits and density distributions

multi.hist(log(REGRESSION.Data.Plot[,2:17]),dcol= c("blue","red"),dlty=c( "solid", "dotted"),main="") 
multi.hist(log(REGRESSION.Data.Plot[,18:26]),dcol= c("blue","red"),dlty=c( "solid", "dotted"),main="") 

# ..................................................................................................


hist(Y.PD,probability=T, main="Histogram of normal
     data",xlab="Approximately normally distributed data", breaks=40)
lines(density((Y.PD)),col=2)

hist(Y.PD.log ,probability=T, main="log (Total Road energy consumption distribution)",
     xlab="", breaks=100)
lines(density(log(Y.PD)),col=2)

plot(density((Y.PD)))
plot(density(log(Y.PD)))

#plot(density((Y.PD.Div.GDP)))
#plot(density(log(Y.PD.Div.GDP)))

# Normality test
shapiro.test(Y.PD)
shapiro.test(log(Y.PD))

#shapiro.test(Y.PD.Div.GDP)
#shapiro.test(log(Y.PD.Div.GDP))

## Plot using a qqplot
qqnorm(Y.PD);qqline(Y.PD, col = 2)
qqnorm(Y.PD.log);qqline(Y.PD.log, col = 2)

#qqnorm(Y.PD.Div.GDP);qqline(Y.PD.Div.GDP, col = 2)
#qqnorm(log(Y.PD.Div.GDP));qqline(log(Y.PD.Div.GDP), col = 2)

# ...............................................................................
# We remove data from the UNITED STATES

CC<-REGRESSION.Data.YEAR.Split[which(REGRESSION.Data.YEAR.Split$Country!="United States"),]
Y.PD.NOT_US<-CC[,c("TOTAL.ROAD.Fuel.Consumption")]
Y.PD.NOT_US<-fClean.Structure(Y.PD.NOT_US)

hist(log(Y.PD.NOT_US),probability=T, main="Histogram of normal
     data",xlab="Approximately normally distributed data", breaks=40)
lines(density(log(Y.PD.NOT_US)),col=2)
shapiro.test(log(Y.PD.NOT_US))


# We display a histogram per Country
# http://www.sthda.com/english/wiki/ggplot2-histogram-easy-histogram-graph-with-ggplot2-r-package
# ggplot2.histogram(data=REGRESSION.Data.YEAR.Split, xName='TOTAL.ROAD.Fuel.Consumption',
                  # groupName='Country', legendPosition="top",
                  # faceting=TRUE, facetingVarNames="Country",
                  # facetingDirection="horizontal")




# -------------------------------------------------------------------------------------------------------------------------------------------------
# VARIABLE X
# -------------------------------------------------------------------------------------------------------------------------------------------------
colnames(X.PD)
X.Normality.Test<-X.PD[,"OECD-Automotive diesel (litre)-Households-Total price (USD/unit using PPP)"]
# X.Normality.Test<-X.PD[,"WB-Railways, goods transported (million ton-km)"]
# X.Normality.Test<-X.PD[,"WB-Railways, passengers carried (million passenger-km)"]
# X.Normality.Test<-X.PD[,"WB-Population in urban agglomerations of more than 1 million (% of total population)"]
# X.Normality.Test<-X.PD[,"WB-Urban population (% of total)"]
# X.Normality.Test<-X.PD[,"WB-Population density (people per sq. km of land area)"]
# X.Normality.Test<-X.PD[,"Actual.Individual.Consumption.xCapita"]


hist(X.Normality.Test,probability=T, main="Histogram of normal
     data",xlab="Approximately normally distributed data", breaks=50)
lines(density((X.Normality.Test)),col=2)

hist(log(X.Normality.Test),probability=T, main="Histogram of log
     data",xlab="Approximately normally distributed data", breaks=50)
lines(density(log(X.Normality.Test)),col=2)

plot(density((X.Normality.Test)))
plot(density(log(X.Normality.Test)))

# Normality test
shapiro.test(X.Normality.Test)
shapiro.test(log(X.Normality.Test))

## Plot using a qqplot
qqnorm(X.Normality.Test);qqline(X.Normality.Test, col = 2)
qqnorm(log(X.Normality.Test));qqline(log(X.Normality.Test), col = 2)





# -------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------------------
# STATISTICS
# -------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------------------

#View(REGRESSION.Data.YEAR.Split)
pd.ROAD.Fuel.Data
dim(pd.ROAD.Fuel.Data)

# https://www.princeton.edu/~otorres/sessions/s2r.pdf
View(stat.desc(REGRESSION.Data.YEAR.Split))
View(stat.desc(REGRESSION.Data.YEAR.Split, basic=TRUE, desc=TRUE, norm=TRUE, p=0.95))

#ONA<-stat.desc( log(REGRESSION.Data.YEAR.Split[,c(3:dim(REGRESSION.Data.YEAR.Split)[2])]) , basic=TRUE, desc=TRUE, norm=TRUE, p=0.95)
rm(STATISTICS.Full.Data)
rm(t_STATISTICS.Full.Data)
rm(Full.Data)
Full.Data<-cbind(Y.PD.log,X.PD.log)
colnames(Full.Data)

STATISTICS.Full.Data<-stat.desc( Full.Data , basic=TRUE, desc=TRUE, norm=TRUE, p=0.95)

# transpose
t_STATISTICS.Full.Data <- transpose(STATISTICS.Full.Data)
#View(t_STATISTICS.Full.Data)
# get row and colnames in order
colnames(t_STATISTICS.Full.Data) <- rownames(STATISTICS.Full.Data)

Statistics.Names<-c("log(Total Road Fuel)",
   #"GDP 2010 PPP US$",
  "log(Automov diesel Total price Households, PPP, US$)",
  "log(GDP at 2010 cte prices PPPs, US dollars$)",
  "% GDP",
  "log(Railways, passengers carried", 
  "log(% Pop urban agglomerations > 1 million)",
  "log(Urban population (% of total))",
  "log(Population density)")

# Interpretation for skew.2SE & kurt.2SE
# http://www.pelagicos.net/BIOL4090_6090/lectures/Biol4090_6090_Fa17_Lecture11.pdf

rownames(t_STATISTICS.Full.Data) <- Statistics.Names
View(t_STATISTICS.Full.Data)

LATEX.STATISTICS.Full.Data1<-xtable(t_STATISTICS.Full.Data[,c(1,4:14)])
#LATEX.STATISTICS.Full.Data1<-xtable(t_STATISTICS.Full.Data[,c(1,4:6)])
#LATEX.STATISTICS.Full.Data2<-xtable(t_STATISTICS.Full.Data[,c(7:10)])
#LATEX.STATISTICS.Full.Data3<-xtable(t_STATISTICS.Full.Data[,c(11:14)])

LATEX.STATISTICS.Full.Data4<-xtable(t_STATISTICS.Full.Data[,c(15:20)])
#LATEX.STATISTICS.Full.Data4<-xtable(t_STATISTICS.Full.Data[,c(15:18)])
#LATEX.STATISTICS.Full.Data5<-xtable(t_STATISTICS.Full.Data[,c(19:20)])
# rownames(LATEX.STATISTICS.Full.Data)<-Statistics.Names
LATEX.STATISTICS.Full.Data1
LATEX.STATISTICS.Full.Data2
LATEX.STATISTICS.Full.Data3

LATEX.STATISTICS.Full.Data4
LATEX.STATISTICS.Full.Data5


colnames(Full.Data)
hist(Full.Data[,1], breaks=50)
lines(density(Full.Data[,1]),col=2)
plot(density((Full.Data[,1])))

hist(Full.Data[,2], breaks=50)
lines(density(Full.Data[,2]),col=2)
plot(density((Full.Data[,2])))

hist(Full.Data[,3], breaks=50)
lines(density(Full.Data[,3]),col=2)
plot(density((Full.Data[,3])))

hist(Full.Data[,4], breaks=50)
lines(density(Full.Data[,4]),col=2)
plot(density((Full.Data[,5])))

hist(Full.Data[,5], breaks=50)
lines(density(Full.Data[,5]),col=2)
plot(density((Full.Data[,5])))

hist(Full.Data[,6], breaks=50)
lines(density(Full.Data[,6]),col=2)
plot(density((Full.Data[,6])))

hist(Full.Data[,7], breaks=50)
lines(density(Full.Data[,6]),col=2)
plot(density((Full.Data[,6])))

hist(Full.Data[,8], breaks=50)
lines(density(Full.Data[,6]),col=2)
plot(density((Full.Data[,6])))

# ---------------------------------------------------------------------------------------------------
# ----- Summary Statistics per Country --------------------------
# ---------------------------------------------------------------------------------------------------

Summary.group<-describeBy(REGRESSION.Data.YEAR.Split$Year, group=droplevels(REGRESSION.Data.YEAR.Split$Country) ,mat=TRUE)
Summary.group<-Summary.group[,c("group1","n","min","max")]
colnames(Summary.group)<-c("Country","n","Year min","Year max")
xtable(Summary.group[,c(1,2,3,4)])
Summary.group

dim(REGRESSION.Data.YEAR.Split)

# ...............................................................................
# We remove data from the UNITED STATES to figure out Normality without US data

REGRESSION.Data.YEAR.Split.NOT_US<-REGRESSION.Data.YEAR.Split[which(REGRESSION.Data.YEAR.Split$Country!="United States"),]
Y.PD.NOT_US<-CC[,c("TOTAL.ROAD.Fuel.Consumption")]
Y.PD.NOT_US<-fClean.Structure(Y.PD.NOT_US)

ONA.NOT_US<-stat.desc( log(REGRESSION.Data.YEAR.Split.NOT_US[,c(3:dim(REGRESSION.Data.YEAR.Split.NOT_US)[2])]) , basic=TRUE, desc=TRUE, norm=TRUE, p=0.95)


t_ONA.NOT_US <- transpose(ONA.NOT_US)
# get row and colnames in order
colnames(t_ONA.NOT_US) <- rownames(ONA.NOT_US)


Statistics.Names<-c("Total Fuel",
                    "Actual individual consumption, PPP 2010, US$",
                    "GDP 2010 PPP US$",
                    "Actual individual consumption, %GDP",
                    "GDP x capita, PPP 2010, US$",
                    "Automov diesel Total price Households, PPP, US$",
                    "Railways, goods transported (million ton-km)",
                    "Railways, passengers carried (million passenger-km)",
                    "% Population in urban agglomerations > 1 million",
                    "Urban population (% of total)",
                    "Population density (people/sq. km of land area)")

rownames(t_ONA.NOT_US) <- Statistics.Names
t_ONA.NOT_US



# ---------------------------------------------------------------------------------------------------
# ----- Correlation among used Variables --------------------------
# ---------------------------------------------------------------------------------------------------

rm(Covariates.Correlation.Matrix)
dim(X.PD)
Covariates.Correlation.Matrix <- cor(X.PD)
Covariates.Correlation.Names<-c("log(Automov diesel Total price Households, PPP, US$)",
                                "log(GDP at 2010 cte prices PPPs, US dollars$)",
                                "% GDP",
                                "log(Railways, passengers carried)",
                                "log(Rail freight transport)",
                                "log(% Pop urban agglomerations > 1 million)",
                                "log(Urban population (% of total))",
                                "log(Population density)")

colnames(Covariates.Correlation.Matrix) <- Covariates.Correlation.Names
xtable(Covariates.Correlation.Matrix)

# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
corrplot(cor(xx[,2:9]), method = "ellipse")
corrplot(cor(xx[,2:9]), method = "number")
corrplot(cor(xx[,2:9]), method = "pie")
corrplot(cor(xx[,2:9]), method = "pie", type="upper")

library(psych)
pairs.panels((xx[,2:9]), 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)


# ******************************************************************************************************************************
# ******************************************************************************************************************************
# ******************************************************************************************************************************
# NORMALITY: TESTS & STATISTICS
# END
# ******************************************************************************************************************************
# ******************************************************************************************************************************
# ******************************************************************************************************************************







# ******************************************************************************************************************************
# ******************************************************************************************************************************
# ******************************************************************************************************************************



# ------------------------------------------------------------------------------------------------------------------------------

# Laplace Quantile Mixed model

# ------------------------------------------------------------------------------------------------------------------------------


colnames(REGRESSION.Data)
REGRESSION.Data.TEMP<-cbind(REGRESSION.Data[,c(1,2)],
                            log(REGRESSION.Data[,c(3,8,4,5,7,9,10)]),
                            REGRESSION.Data[,c(6,11:13)])
NAMES.REGRESSION.Data.Temp<-colnames(REGRESSION.Data.TEMP)

NAMES.REGRESSION.Data.Temp
colnames(REGRESSION.Data.TEMP)<-c("Country","Year","Consumption","A4","A5","A6","A7","A8","A9","A10","A11","A12","A13")

lqmm.fixed <- lqmm(fixed = Consumption ~ A4+A7+A8+A9+A10+A11+A12+A13, random = ~ 1, group = Country,
                   data = REGRESSION.Data.TEMP, tau = c(0.2,0.5,0.9) , nK = 7, type = "robust")


coef(lqmm.fixed)
lqmm.fixed$control$LP_tol_ll <- 1e-3
lqmm.fixed$control$LP_max_iter <- 1000
summary(lqmm.fixed, R = 100, seed = 52)

screenreg(list(lqmm.fixed),digits=4)
NAMES.REGRESSION.Data.Temp

summary(lqmm.fixed)
system.time(print(summary(lqmm.fixed)))
#screenreg(list(lme.regression.fixed,lqmm.fixed),digits=4)

ranef(lqmm.fixed)
coef(lqmm.fixed)
#..................................................
# PANEL DATA
pd.regression.fixed<-plm(Consumption ~ A4+ A7 + A8+A9+A10+A11+A12+A13,  model= "within", index=c("Country","Year"), na.action=na.omit, data=REGRESSION.Data.TEMP)
screenreg(list(lqmm.fixed),digits=4)
screenreg(list(pd.regression.fixed),digits=4)
#..................................................
# LMER
lme.regression.fixed = lmer(Consumption ~  A4+ A7 + A8+A9+A10+A11+A12+A13+ (1|Country) , data=REGRESSION.Data.TEMP)
screenreg(list(lme.regression.fixed,pd.regression.fixed),digits=4)
# .........................................

#screenreg(list(lme.regression.fixed,lqmm.fixed),digits=4)
screenreg(list(lqmm.fixed),digits=4)

ranef(lqmm.fixed)
coef(lqmm.fixed)
lqmm.fixed


# Salida en latex
texreg(lqmm.fixed, label = "tab:3", digits=4)
texreg(lqmm.fixed)



# .............................................................................................

# --------------------------------------------------------------------------------------------------------------------------------------------------------------

# qrLMM: Quantile Regression for Linear Mixed-Effects Models

pd.ROAD.Fuel.Data[,"Country"]

Inic.Betas<-c(-3.47861978, -0.16667569, 0.02933401, -0.07130486, -0.62606823, 0.58874970, 1.05094171, 1.23500789)
#
qrLMM.fixed<- QRLMM(Y.PD, cbind(1,X.PD), cbind(pd.ROAD.Fuel.Data[,"Country"]), pd.ROAD.Fuel.Data[,"Country"], 
                    beta=Inic.Betas, p = c(0.2,0.5,0.8), MaxIter=50, M=10)

qrLMM.fixed.02
qrLMM.fixed.q10
print(qrLMM.fixed)
summary(qrLMM.fixed)

str(qrLMM.fixed.02)
qrLMM.fixed.02$conv$teta


install.packages("Hmisc")
library("Hmisc")
latex(qrLMM.fixed)





# --------------------------------------------------------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------------------------------------------------------
#View(diff(X.PD, lag = 1, differences = 1))

#X.PD.Diff1<-diff(X.PD.Regression, lag = 1, differences = 1)
#Y.PD.Diff1<-diff(Y.PD.Regression, lag = 1, differences = 1)

X.PD.Diff1<-diff(X.PD, lag = 1, differences = 1)
Y.PD.Diff1<-diff(Y.PD, lag = 1, differences = 1)

pd.regression.fixed.Diff1<-plm(Y.PD.Diff1 ~  X.PD.Diff1,  model= "within", index=c("Country","Year"), na.action=na.omit, data=pd.ROAD.Fuel.Data)
pd.regression.random.Diff1<-plm(Y.PD.Diff1 ~ X.PD.Diff1, model= "random", index=c("Country","Year"), na.action=na.omit, data=pd.ROAD.Fuel.Data)
pd.regression.fd.Diff1<-plm(Y.PD.Diff1 ~ X.PD.Diff1, model= "fd", index=c("Country","Year"), na.action=na.omit, data=pd.ROAD.Fuel.Data)

stargazer( pd.regression.fixed.Diff1, pd.regression.random.Diff1, 
          type="text", align=TRUE, ci.level=0.95, digits=3,
          column.labels=c("Fixed Effects","Random Effects","First Differences"),
          header=TRUE, no.space=TRUE,  single.row=TRUE,
          ci=TRUE, title="TITULO",
          dep.var.caption="DEPENDENT VARIABLE",
          #dep.var.labels=DEPENDENT.Var,
          covariate.labels=colnames(X.PD),
          style="commadefault")

#View(X.PD.Diff1)
summary(pd.regression.fixed.Diff1)
summary(pd.regression.random.Diff1)





ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2,10,20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)
screenreg(lm.D9)  # print model output to the R console
plotreg(lm.D9)    # plot model output as a diagram





