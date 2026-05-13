#.......................................................................................................................
# BBDD utilizada para realizar las regresiones con datos hasta 2015. El fichero de precios no obstante
# tiene datos hasta 2016. El nuevo los tiene hasta 2017

# BBDD.OECD.Oil.Prices.BBDD2016<-BBDD.OECD.Oil.Prices
# BBDD.OECD.Oil.Prices.BBDD2017<-BBDD.OECD.Oil.Prices
#.......................................................................................................................


# R how to change factor name
# http://www.cookbook-r.com/Manipulating_data/Renaming_levels_of_a_factor/


#install.packages("stargazer") #Use this to install it, do this only once
#install.packages("gdata")
#install.packages("xlsx")
#install.packages("openxlsx")
library("gdata")
library("stargazer")
library("rJava")
library("xlsx")
#library("openxlsx")

#rm(list=ls())


# *******************************************************************************************************
#                                            BBDD Actualizadas
# *******************************************************************************************************





# *******************************************************************************************************
#                                            FUNCTIONS
# *******************************************************************************************************

fGenDataPanel <- function(Countries,Years,DataFile)
{
  fData<- as.data.frame(matrix(nrow = Years*(Countries), ncol = 3))
  for (country in 1:Countries)
  {
    for (year in 1:Years)
    {
      # 1st we write country
      fData[NUM_Years*(country-1)+year,1]<-as.character(((DataFile[country+1,1])))
      # 2nd we write year
      fData[NUM_Years*(country-1)+year,2]<-(DataFile[1,year+1])
      #  3rd we write data
      fData[NUM_Years*(country-1)+year,3]<-(DataFile[country+1,year+1])
    }
  }
  return(fData)
}



fGenBBDDPanel.1Dimension <- function(SOURCEData, BBBDD_SOURCEData, SOURCE)
{
  # PRE: SOURCEData columns must be "Country","Year","Variable", "Value"
  # SOURCEData: Original Data Base to be transformed into Panel Data format
  # BBBDD_SOURCEData: Final Data Base on Panel Data format
  # SOURCE: Name of the source the data comes from...  Ex: OCDE, EUROSTAT, WB, EC... 
  
  #SOURCEData$Variable<-as.factor(SOURCEData$Variable)
  #SOURCEData$Country<-as.factor(SOURCEData$Country)
  
  LEVELS<-levels(as.factor(SOURCEData$Variable))
  print("VALOR LEVELS-->")
  print( LEVELS)
  # We generate SOURCEData files
  rm(BBBDD_SOURCEData)
  LEVELS<-levels(droplevels(as.factor(SOURCEData$Variable)))
  BBBDD_SOURCEData<-SOURCEData[which(SOURCEData[,"Variable"]==LEVELS[1]),]
  VAR_NAME<-paste(SOURCE, LEVELS[1],sep="-")
  colnames(BBBDD_SOURCEData)<-c("Country","Year","Variable",VAR_NAME)
  BBBDD_SOURCEData<-subset(BBBDD_SOURCEData, select = -c(Variable))
  #View(BBBDD_SOURCEData)
  if (length(LEVELS) == 1)
  {
    return(BBBDD_SOURCEData)
  }
  
  for (i in 2:length(LEVELS))
  {
    A<-SOURCEData[which(SOURCEData[,"Variable"]==LEVELS[i]),]
    VAR_NAME<-paste(SOURCE, LEVELS[i],sep="-")
    colnames(A)<-c("Country","Year","Variable",VAR_NAME)
    A<-subset(A, select = -c(Variable))
    BBBDD_SOURCEData<-merge(BBBDD_SOURCEData, A,  by=c("Country","Year"), all=TRUE)
  }
  return(BBBDD_SOURCEData) 
}



fGenBBDDPanel.2.Histogram <- function(SOURCEData, BBBDD_SOURCEData)
{
  # This function is created for obtaining a histogram panel 
  # PRE: SOURCEData columns must be "Country","Year","Variable"
  # SOURCEData: Original Data Base to be transformed into Panel Data format for histogram purposes
  # BBBDD_SOURCEData: Final Data Base on Panel Data format
  
  LEVELS<-levels(as.factor(SOURCEData$Country))
  print("VALOR LEVELS-->")
  print( LEVELS)
  # We generate SOURCEData files
  rm(BBBDD_SOURCEData)
  LEVELS<-levels(droplevels(as.factor(SOURCEData$Country)))
  BBBDD_SOURCEData<-SOURCEData[which(SOURCEData[,"Country"]==LEVELS[1]),]
  colnames(BBBDD_SOURCEData)<-c("Country","Year",LEVELS[1])
  BBBDD_SOURCEData<-subset(BBBDD_SOURCEData, select = -c(Country))
  
  #View(BBBDD_SOURCEData)
  if (length(LEVELS) == 1)
  {
    return(BBBDD_SOURCEData)
  }
  
  for (i in 2:length(LEVELS))
  {
    A<-SOURCEData[which(SOURCEData[,"Country"]==LEVELS[i]),]
    colnames(A)<-c("Country","Year",LEVELS[i])
    A<-subset(A, select = -c(Country))
    BBBDD_SOURCEData<-merge(BBBDD_SOURCEData, A,  by=c("Year"), all=TRUE)
  }
  return(BBBDD_SOURCEData) 
}




# *******************************************************************************************************
#                                              COMMON VARIABLES
# *******************************************************************************************************

# CASA

BBDD_OECD_reading_directory <- "C:/AAP/DOCTORADO/1 -PAPER/1-OK-BBDD/OECD/"
BBDD_OECD_writing_directory <- "C:/AAP/DOCTORADO/1 -PAPER/1-OK-BBDD/OECD/GENERADOS desde R/"

BBDD_EUROSTAT_reading_directory <- "C:/AAP/DOCTORADO/1 -PAPER/1-OK-BBDD/EUROSTAT/"
BBDD_EUROSTAT_writing_directory <- "C:/AAP/DOCTORADO/1 -PAPER/1-OK-BBDD/EUROSTAT/GENERADOS desde R/"

BBDD_WD_reading_directory <- "C:/AAP/DOCTORADO/1 -PAPER/1-OK-BBDD/WORLD BANK/"
BBDD_WD_writing_directory <- "C:/AAP/DOCTORADO/1 -PAPER/1-OK-BBDD/WORLD BANK/GENERADOS desde R/"
WB.Graphs.writing.directory <- "C:/AAP/DOCTORADO/1 -PAPER/1-OK-BBDD/WORLD BANK/GENERADOS desde R/Graphs/"

BBDD_EC_reading_directory <- "C:/AAP/DOCTORADO/1 -PAPER/1-OK-BBDD/EC- European Commission/"
BBDD_EC_writing_directory <- "C:/AAP/DOCTORADO/1 -PAPER/1-OK-BBDD/EC- European Commission/GENERADOS desde R/"

BBDD_WEF_reading_directory <- "C:/AAP/DOCTORADO/1 -PAPER/1-OK-BBDD/WEFORUM/"
BBDD_WEF_writing_directory <- "C:/AAP/DOCTORADO/1 -PAPER/1-OK-BBDD/WEFORUM/GENERADOS desde R/"

BBDD_FINAL_DATA_reading_directory <- "C:/AAP/DOCTORADO/1 -PAPER/1-OK-BBDD/OUTCOMES/FINAL REGRESSION DATA/"
BBDD_FINAL_DATA_writing_directory <- "C:/AAP/DOCTORADO/1 -PAPER/1-OK-BBDD/OUTCOMES/FINAL REGRESSION DATA/"

OECD.Graphs.writing.directory <- "C:/AAP/DOCTORADO/1 -PAPER/1-OK-BBDD/OECD/GENERADOS desde R/Graphs/"
OUTCOMES_directory<-"C:/AAP/DOCTORADO/1 -PAPER/1-OK-BBDD/OUTCOMES/"

# UB
BBDD_OECD_reading_directory <- "C:/Users/alberto.antoran/Dropbox/DOCTORADO/1 - DATOS/1 - PAPER/2017/OECD/"
BBDD_OECD_writing_directory <- "C:/Users/alberto.antoran/Dropbox/DOCTORADO/1 - DATOS/1 - PAPER/2017/OECD/GENERADOS desde R/"

BBDD_EUROSTAT_reading_directory <- "C:/Users/alberto.antoran/Dropbox/DOCTORADO/1 - DATOS/1 - PAPER/2017/EUROSTAT/"
BBDD_EUROSTAT_writing_directory <- "C:/Users/alberto.antoran/Dropbox/DOCTORADO/1 - DATOS/1 - PAPER/2017/EUROSTAT/GENERADOS desde R/"

BBDD_WD_reading_directory <- "C:/Users/alberto.antoran/Dropbox/DOCTORADO/1 - DATOS/1 - PAPER/2017/WORLD BANK/"
BBDD_WD_writing_directory <- "C:/Users/alberto.antoran/Dropbox/DOCTORADO/1 - DATOS/1 - PAPER/2017/WORLD BANK/GENERADOS desde R/"
WB.Graphs.writing.directory <- "C:/Users/alberto.antoran/Dropbox/DOCTORADO/1 - DATOS/1 - PAPER/2017/WORLD BANK/GENERADOS desde R/Graphs/"


BBDD_EC_reading_directory <- "C:/Users/alberto.antoran/Dropbox/DOCTORADO/1 - DATOS/1 - PAPER/2017/EC- European Commission/"
BBDD_EC_writing_directory <- "C:/Users/alberto.antoran/Dropbox/DOCTORADO/1 - DATOS/1 - PAPER/2017/EC- European Commission/GENERADOS desde R/"

BBDD_WEF_reading_directory <- "C:/Users/alberto.antoran/Dropbox/DOCTORADO/1 - DATOS/1 - PAPER/2017/WEFORUM/"
BBDD_WEF_writing_directory <- "C:/Users/alberto.antoran/Dropbox/DOCTORADO/1 - DATOS/1 - PAPER/2017/WEFORUM/GENERADOS desde R/"

OECD.Graphs.writing.directory <- "C:/Users/alberto.antoran/Dropbox/DOCTORADO/1 - DATOS/1 - PAPER/2017/OECD/GENERADOS desde R/Graphs/"
OUTCOMES_directory<-"C:/Users/alberto.antoran/Dropbox/DOCTORADO/1 - DATOS/1 - PAPER/2017/OUTCOMES/"

BBDD_FINAL_DATA_reading_directory <- "C:/Users/alberto.antoran/Dropbox/DOCTORADO/1 - DATOS/1 - PAPER/DOCUMENTO FINAL/FINAL REGRESSION DATA/"
BBDD_FINAL_DATA_writing_directory <- "C:/Users/alberto.antoran/Dropbox/DOCTORADO/1 - DATOS/1 - PAPER/DOCUMENTO FINAL/FINAL REGRESSION DATA/"



# ........................ BBDD ACTUALIZADA EN 2021 ................................................................................

# UB 

BBDD_OECD_reading_directory <- "C:/Users/alberto.antoran/Dropbox/DOCTORADO/1 - DATOS/2 - PAPER/OCDE/"
BBDD_OECD_writing_directory <- "C:/Users/alberto.antoran/Dropbox/DOCTORADO/1 - DATOS/2 - PAPER/OCDE/GENERADOS desde R/"

BBDD_WD_reading_directory<-"C:/Users/alberto.antoran/Dropbox/DOCTORADO/1 - DATOS/2 - PAPER/WORLD BANK/"
BBDD_WD_writing_directory<-"C:/Users/alberto.antoran/Dropbox/DOCTORADO/1 - DATOS/2 - PAPER/WORLD BANK/GENERADOS desde R/"

BBDD_WEF_reading_directory <- "C:/Users/alberto.antoran/Dropbox/DOCTORADO/1 - DATOS/2 - PAPER/WEFORUM/"
BBDD_WEF_writing_directory <- "C:/Users/alberto.antoran/Dropbox/DOCTORADO/1 - DATOS/2 - PAPER/WEFORUM/GENERADOS desde R/"

# CASA 

BBDD_OECD_reading_directory <- "C:/ALBERTO/3-DOCTORADO/DATOS/2 - PAPER/OECD/"
BBDD_OECD_writing_directory <- "C:/ALBERTO/3-DOCTORADO/DATOS/2 - PAPER/OECD/GENERADOS desde R/"

BBDD_WD_reading_directory<-"C:/ALBERTO/3-DOCTORADO/DATOS/2 - PAPER/WORLD BANK/"
BBDD_WD_writing_directory<-"C:/ALBERTO/3-DOCTORADO/DATOS/2 - PAPER/WORLD BANK/GENERADOS desde R/"

BBDD_WEF_reading_directory <- "C:/ALBERTO/3-DOCTORADO/DATOS/2 - PAPER/WEFORUM/"
BBDD_WEF_writing_directory <- "C:/ALBERTO/3-DOCTORADO/DATOS/2 - PAPER/WEFORUM/GENERADOS desde R/"

BBDD.FINAL.writing_directory<- "C:/ALBERTO/3-DOCTORADO/DATOS/2 - PAPER/GENERADOS desde R/"

# *******************************************************************************************************
# -------- DATABASES CREATED 
# *******************************************************************************************************

# # --- OECD  ------------------------------------------------------------------------------------------ 

# BBDD_OECD_OIL_Road            --> BBDD_OECD_OIL_Road.csv   BBDD from OECD with Road oil data consumption 
#                                             
# BBDD_OECD_Road_PI             --> BBDD_OECD_Road_PI.csv    BBDD from OECD with Road PERFORMANCES INDICATORS related with 
#                                                            freight, passengers, infrastructure, Kms etc
#
# BBDD_OECD_Rail                --> BBDD_OECD_Rail.csv       BBDD from OECD with railways data

# BBDD_OECD_Vehicles            --> BBDD_OECD_Vehicles.csv  BBDD from OECD with data about passenger cars & 
#                                                            motorcycles registrations  

# BBDD_OECD_STI_Traffic         --> BBDD_OECD_STI_Traffic.csv  BBDD from OECD data about rail, road and waterway
#                                                               goods transport and passenger traffic

# BBDD_OECD_STI_Registrations   --> BBDD_OECD_STI_Registrations.csv BBDD from OECD with first registrations of 
#                                                                    brand new goods vehicles, passenger cars & road vehicles 

# BBDD_OECD_Goods_Transport     --> BBDD_OECD_Goods_Transport.csv   BBDD from OECD with Road, Rail, Maritime freight 
#                                                                   and containers data

# BBDD_OECD_Passenger_Transport --> BBDD_OECD_Passenger_Transport.csv  BBDD from OECD with road and rail million 
#                                                                      passenger-km for buses, passenger carse, rail

# BBDD_OECD_Transport_Safety    --> BBDD_OECD_Transport_Safety.csv    BBDD from OECD with data of fatalities & injuries

# BBDD_OECD_Transport_Infrastructure  -->  BBDD_OECD_Transport_Infrastructure.csv  BDD from OECD with data of infrastructure
#                                                                                  investment & maintenance in Rails, Roads, 
#                                                                                  Airports, Ports

# BBDD_OECD_OIL_Road            
# BBDD_OECD_Road_PI             
# BBDD_OECD_Rail                
# BBDD_OECD_Vehicles            
# BBDD_OECD_STI_Traffic         
# BBDD_OECD_STI_Registrations   
# BBDD_OECD_Goods_Transport     
# BBDD_OECD_Passenger_Transport 
# BBDD_OECD_Transport_Infrastructure



# --- EUROSTAT  ------------------------------------------------------------------------------------------


# BBDD_EUROSTAT_ROAD_OIL    -->  BBDD_EUROSTAT_ROAD_OIL.csv BBDD from EUROSTAT with Road oil data consumption



# --- WORLDBANK  ------------------------------------------------------------------------------------------



# --- EUROPEAN COMMISSION  ------------------------------------------------------------------------------------------




# *******************************************************************************************************




# *******************************************************************************************************
#******************************************************************************************************** 
# -------------------------------------------------------------------------------------------------------
# --- DATABASES CONSTRUCTION
# -------------------------------------------------------------------------------------------------------
# *******************************************************************************************************
#********************************************************************************************************



#************************************************************************************************************************************************************
#************************************************************************************************************************************************************
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                                     WORLD ECONOMIC FORUM
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#************************************************************************************************************************************************************
#************************************************************************************************************************************************************


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#---------------------------------------------------------------------------------------------------------------------------------------------------
#                               CREACIÓN BBDD  WEFORUM - WORLD ECONOMIC FORUM - GCI (GLOBAL COMPETITIVENES INDEX)
# --------------------------------------------------------------------------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# We read CSV file
rm(WEF.GCI.Transport)
WB_file_WDI.GDP<-"WEFORUM - Infrastructure Quality Indexes.csv"
file_WEF.GCI.Transport<-paste(BBDD_WEF_reading_directory, WB_file_WDI.GDP,sep="")
WEF.GCI.Transport<- read.csv(file_WEF.GCI.Transport, header=TRUE, sep=";", dec=",")
# View(na.omit(BBDD_WEF.GCI.Transport))

rm(BBDD_WEF.GCI.Transport)
BBDD_WEF.GCI.Transport<-fGenBBDDPanel.1Dimension(WEF.GCI.Transport, BBDD_WEF.GCI.Transport, "WEF")
# View(BBDD_WEF.GCI.Transport)


#************************************************************************************************************************************************************
#************************************************************************************************************************************************************
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                                     WORLD BANK DATABASE
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#************************************************************************************************************************************************************
#************************************************************************************************************************************************************



# ----------------------------------------------------------------------------------------------------------------------------------
#             ULTIMO: 24-07-2021 CREACIÓN BBDD WORLD-BANK  con datos World Development Indicators (WDI)
# ----------------------------------------------------------------------------------------------------------------------------------

# We read CSV file
rm(WB_WDI)
#WB_file_WDI<-"WORLD BANK - World Development Indicators - Column Variables.csv"
WB_file_WDI<-"WORLD BANK - World Development Indicators.csv"
#BBDD_WD_reading_directory <- "C:/Users/alberto.antoran/Dropbox/DOCTORADO/1 - DATOS/2 - PAPER/WORLD BANK/"

#BBDD_WD_reading_directory <- "C:/AAP/DOCTORADO/2 - PAPER/BBDD/WORLD BANK/"
file_WB_WDI<-paste(BBDD_WD_reading_directory, WB_file_WDI,sep="")
WB_WDI<- read.csv(file_WB_WDI, header=TRUE, sep=",", dec=".", check.names = FALSE)

head(WB_WDI)
colnames(WB_WDI)
str(WB_WDI)
View(WB_WDI)


#WB_WDI.Orig<-WB_WDI


COLUMN.Names<-colnames(WB_WDI)
COLUMN.Names[1]<-"Country"
COLUMN.Names[2]<-"Year"
colnames(WB_WDI)<-COLUMN.Names
levels(as.factor(WB_WDI$Country))

# We remove empty data Countries
ROWS_TO_REMOVE<-which(WB_WDI[,"Country"]=="")
WB_WDI<-WB_WDI[-ROWS_TO_REMOVE,]

rm(Columns.VarNames)
Columns.VarNames<-colnames(WB_WDI)
Columns.VarNames[3:length(Columns.VarNames)]<-paste("WB~WDI", Columns.VarNames[3:length(Columns.VarNames)],sep="-")
colnames(WB_WDI)<-Columns.VarNames
#View(WB_WDI)
str(WB_WDI)

rm(BBDD_WB_WDI)
BBDD_WB_WDI<-WB_WDI
str(BBDD_WB_WDI)

#View(BBDD_WB_WDI)

View(BBDD_WB_WDI[, c(1,2,grep("urban", ignore.case=TRUE, colnames(BBDD_WB_WDI)))])


#...................................................................................................................





# ----------------------------------------------------------------------------------------------------------------------------------
#             PRIMERA UTILIZADA - CREACIÓN BBDD WORLD-BANK  con datos World Development Indicators (WDI)
# ----------------------------------------------------------------------------------------------------------------------------------


# We read CSV file
rm(WB_WDI)
WB_file_WDI<-"WORLD BANK - World Development Indicators.csv"
file_WB_WDI<-paste(BBDD_WD_reading_directory, WB_file_WDI,sep="")
WB_WDI<- read.csv(file_WB_WDI, header=TRUE, sep=",", dec=".")

head(WB_WDI)
names(WB_WDI)
View(WB_WDI)
colnames(WB_WDI)

# We select only the variables we are interested in
WB_WDI<-WB_WDI[,c("ï..Country.Name","Time","Series.Name","Value")]
colnames(WB_WDI)<-c("Country","Year","Variable","Value")
head(WB_WDI)
levels(WB_WDI$Country)[1]

# We remove empty data Countries
ROWS_TO_REMOVE<-which(WB_WDI[,"Country"]=="")
WB_WDI<-WB_WDI[-ROWS_TO_REMOVE,]
# We remove empty data Values
ROWS_TO_REMOVE<-which(WB_WDI[,"Variable"]=="")
WB_WDI<-WB_WDI[-ROWS_TO_REMOVE,]



# ----------------------------------------------------------------------------------------------------------------------------------
#                                CREACIÓN BBDD WORLD-BANK  con datos World Development Indicators (WDI)
# ----------------------------------------------------------------------------------------------------------------------------------
rm(BBDD_WB_WDI)

LEVELS_WB_WDI<-levels(droplevels(WB_WDI$Variable))
BBDD_WB_WDI<-WB_WDI[which(WB_WDI[,"Variable"]==LEVELS_WB_WDI[1]),]
VAR_NAME<-paste("~WDI",LEVELS_WB_WDI[1],sep="-")
colnames(BBDD_WB_WDI)<-c("Country","Year","Variable",VAR_NAME)
BBDD_WB_WDI<-subset(BBDD_WB_WDI, select = -c(Variable))

for (i in 2:length(LEVELS_WB_WDI))
{
  A<-WB_WDI[which(WB_WDI[,"Variable"]==LEVELS_WB_WDI[i]),]
  VAR_NAME<-paste("~WDI",LEVELS_WB_WDI[i],sep="-")
  colnames(A)<-c("Country","Year","Variable",VAR_NAME)
  A<-subset(A, select = -c(Variable))
  BBDD_WB_WDI<-merge(BBDD_WB_WDI, A,  by=c("Country","Year"), all=TRUE)
}

#View(BBDD_WB_WDI[which(BBDD_WB_WDI[,"Country"]=="Spain"),])

WB.Temp.Indicators<-BBDD_WB_WDI[which(BBDD_WB_WDI[,"Country"]=="Spain"),]

colnames(BBDD_WB_WDI)
# Pruebas para comparar datos de las diferentes BBDD

WDI_VARS2BEREAD<-c("WB-Railways, goods transported (million ton-km)", "WB-Railways, passengers carried (million passenger-km)")
OECD_STI_Traffic_VARS2BEREAD<-c("OECD-Total rail passengers transport in million passenger-km",
                    "OECD-Total rail goods transport in million tonne-km")
OECD_Goods_Transport_VARS2BEREAD<-c("OECD-Rail freight in million tonne-km")


D1<-BBDD_WB_WDI[which(BBDD_WB_WDI[,"Country"]=="Spain"),c("Country","Year",WDI_VARS2BEREAD)]
D2<-BBDD_OECD_STI_Traffic[which(BBDD_OECD_STI_Traffic[,"Country"]=="Spain"), c("Country","Year",OECD_STI_Traffic_VARS2BEREAD)]
D3<-BBDD_OECD_Goods_Transport[which(BBDD_OECD_Goods_Transport[,"Country"]=="Spain"), c("Country","Year",OECD_Goods_Transport_VARS2BEREAD)]

rm(D4)
D4<-merge(D1,D2 ,  by=c("Country","Year"), all=TRUE)
rm(D5)
D5<-merge(D4, D3,  by=c("Country","Year"), all=TRUE)

View(D5)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#---------------------------------------------------------------------------------------------------------------------------------------------------
#                                    CREACIÓN BBDD  WORLD-BANK  WORLD DEVELOPMENT INDICATORS - GDP
# --------------------------------------------------------------------------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# We read CSV file
rm(WB_WDI.GDP)
#WB_file_WDI.GDP<-"WORLD BANK - World_Development_Indicators - SEP 2017.csv"
WB_file_WDI.GDP<-"WORLD BANK - World Development Indicators.csv"
file_WB_WDI.GDP<-paste(BBDD_WD_reading_directory, WB_file_WDI.GDP,sep="")
WB_WDI.GDP<- read.csv(file_WB_WDI.GDP, header=TRUE, sep=",", dec=".")
names(WB_WDI.GDP)

#WB_file_WDI.GDP<-"Data_Extract_From_World_Development_Indicators - SEP 2017 GDP+Population.csv"
#file_WB_WDI.GDP<-paste(BBDD_WD_reading_directory, WB_file_WDI.GDP,sep="")
#WB_WDI.GDP<- read.csv(file_WB_WDI.GDP, header=TRUE, sep=",", dec=".")
#levels(WB_WDI.GDP[,"Series.Name"])

# We select only the variables we are interested in
#WB_WDI.GDP<-WB_WDI.GDP[,c("Country.Name","Time","ï..Series.Name","Value")]
WB_WDI.GDP<-WB_WDI.GDP[,c("ï..Country.Name","Time","Series.Name","Value")]
colnames(WB_WDI.GDP)<-c("Country","Year","Variable","Value")
head(WB_WDI.GDP)
str(WB_WDI.GDP$Country)
WB_WDI.GDP$Country <- as.factor(WB_WDI.GDP$Country)
levels(WB_WDI.GDP$Country)
levels(WB_WDI.GDP$Country)[1]

#View(WB_WDI.GDP)
#head(WB_WDI.GDP)
#names(WB_WDI.GDP)


# We remove empty data Countries
ROWS_TO_REMOVE<-which(WB_WDI.GDP[,"Country"]=="")
WB_WDI.GDP<-WB_WDI.GDP[-ROWS_TO_REMOVE,]
# We remove empty data Values
# ROWS_TO_REMOVE<-which(WB_WDI.GDP[,"Variable"]=="")
# WB_WDI.GDP<-WB_WDI.GDP[-ROWS_TO_REMOVE,]

# ----------------------------------------------------------------------------------------------------------------------------------
#                                CREACIÓN BBDD BBDD_WB_WDI.GDP
# ----------------------------------------------------------------------------------------------------------------------------------
rm(BBDD_WB_WDI.GDP)
View(WB_WDI.GDP)
BBDD_WB_WDI.GDP<-fGenBBDDPanel.1Dimension(WB_WDI.GDP, BBDD_WB_WDI.GDP, "WB")
View(BBDD_WB_WDI.GDP)


# We create this BBDD just for obtaining GDP Values for all Countries so as to study OIL ROAD
# consumption per GDP
# Data used on initial BBDD WITHOUT 2016 Data
#BBDD_WB_WDI.GDP.Selection<-BBDD_WB_WDI.GDP[,c("Country","Year",
#                   "WB-GDP, PPP (constant 2011 international $)",
#                   "WB-CO2 emissions (kg per 2011 PPP $ of GDP)",
#                   "WB-Household final consumption expenditure, etc. (% of GDP)")]

# Data used on BBDD WITH 2016 Data
BBDD_WB_WDI.GDP.Selection<-BBDD_WB_WDI.GDP[,c("Country","Year",
                                              "WB-GDP, PPP (constant 2011 international $)",
                                              "WB-CO2 emissions from transport (% of total fuel combustion)",
                                              "WB-Households and NPISHs final consumption expenditure (% of GDP)")]

#View(BBDD_WB_WDI.GDP.Selection)

#BBDD_WB_WDI.GDP.Selection.BBDD2015<-BBDD_WB_WDI.GDP.Selection


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#--------------------------------------------------------------------------------------------------------------------------
#                                    CREACIÓN BBDD  WORLD-BANK  Poverty & Equity (PE)
# -------------------------------------------------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# We read CSV file
rm(WB_PE)
WB_file_PE<-"WORLD BANK - Povety & Equity.csv"
file_WB_PE<-paste(BBDD_WD_reading_directory, WB_file_PE,sep="")
WB_PE<- read.csv(file_WB_PE, header=TRUE, sep=",", dec=".")

head(WB_PE)
names(WB_PE)

# We select only the variables we are interested in
WB_PE<-WB_PE[,c("Country","Year","ï..Series.Name","Value")]
colnames(WB_PE)<-c("Country","Year","Variable","Value")
head(WB_PE)
levels(WB_PE$Country)
levels(WB_PE$Country)[1]

# We remove empty data Countries
ROWS_TO_REMOVE<-which(WB_PE[,"Country"]=="")
WB_PE<-WB_PE[-ROWS_TO_REMOVE,]
# We remove empty data Values
# ROWS_TO_REMOVE<-which(WB_PE[,"Variable"]=="")
# WB_PE<-WB_PE[-ROWS_TO_REMOVE,]

rm(BBDD_WB_PE)

LEVELS_WB_PE<-levels(droplevels(WB_PE$Variable))
BBDD_WB_PE<-WB_PE[which(WB_PE[,"Variable"]==LEVELS_WB_PE[1]),]
VAR_NAME<-paste("WB",LEVELS_WB_WDI[1],sep="-")
colnames(BBDD_WB_PE)<-c("Country","Year","Variable",VAR_NAME)
BBDD_WB_PE<-subset(BBDD_WB_PE, select = -c(Variable))

for (i in 2:length(LEVELS_WB_PE))
{
  A<-WB_PE[which(WB_PE[,"Variable"]==LEVELS_WB_PE[i]),]
  VAR_NAME<-paste("WB",LEVELS_WB_WDI[i],sep="-")
  colnames(A)<-c("Country","Year","Variable",VAR_NAME)
  A<-subset(A, select = -c(Variable))
  BBDD_WB_PE<-merge(BBDD_WB_PE, A,  by=c("Country","Year"), all=TRUE)
}

#View(BBDD_WB_PE[which(BBDD_WB_PE[,"Country"]=="Spain"),])

#--------------------------------------------------------------------------------------------------------------------------
#                     CREACIÓN BBDD  WORLD-BANK IDA Results Measurement System. BBDD sol para paises subdesarrollados
# -------------------------------------------------------------------------------------------------------------------------
# We read CSV file
rm(WB_IDARMS)
WB_file_SDG<-"WORLD BANK - IDA Results Measurement System.csv"
file_WB_IDARMS<-paste(BBDD_WD_reading_directory, WB_file_SDG,sep="")
WB_IDARMS<- read.csv(file_WB_IDARMS, header=TRUE, sep=",", dec=".")

head(WB_IDARMS)
names(WB_IDARMS)

# We select only the variables we are interested in
WB_IDARMS<-WB_IDARMS[,c("ï..Country.Name","Time","Series.Name","Value")]
colnames(WB_IDARMS)<-c("Country","Year","Variable","Value")
head(WB_IDARMS)
levels(WB_IDARMS$Country)[1]

# We remove empty data Countries
ROWS_TO_REMOVE<-which(WB_IDARMS[,"Country"]=="")
WB_IDARMS<-WB_IDARMS[-ROWS_TO_REMOVE,]
# We remove empty data Values
ROWS_TO_REMOVE<-which(WB_IDARMS[,"Variable"]=="")
WB_IDARMS<-WB_IDARMS[-ROWS_TO_REMOVE,]

rm(BBDD_WB_IDARMS)

LEVELS_WB_IDARMS<-levels(droplevels(WB_IDARMS$Variable))
BBDD_WB_IDARMS<-WB_IDARMS[which(WB_IDARMS[,"Variable"]==LEVELS_WB_IDARMS[1]),]
VAR_NAME<-paste("WB",LEVELS_WB_IDARMS[1],sep="-")
colnames(BBDD_WB_IDARMS)<-c("Country","Year","Variable",VAR_NAME)
BBDD_WB_IDARMS<-subset(BBDD_WB_IDARMS, select = -c(Variable))

for (i in 2:length(LEVELS_WB_IDARMS))
{
  A<-WB_IDARMS[which(WB_IDARMS[,"Variable"]==LEVELS_WB_IDARMS[i]),]
  VAR_NAME<-paste("WB",LEVELS_WB_IDARMS[i],sep="-")
  colnames(A)<-c("Country","Year","Variable",VAR_NAME)
  A<-subset(A, select = -c(Variable))
  BBDD_WB_IDARMS<-merge(BBDD_WB_IDARMS, A,  by=c("Country","Year"), all=TRUE)
}




#View(BBDD_WB_IDARMS[which(BBDD_WB_IDARMS[,"Country"]=="Pakistan"),])

#--------------------------------------------------------------------------------------------------------
#                                  CREACIÓN BBDD WORLD-BANK - SDG - Sustainable Development Goals
# --------------------------------------------------------------------------------------------------------
# We read CSV file
rm(WB_SDG)
WB_file_SDG<-"WORLD-BANK - SDG - Sustainable Development Goals.csv"
file_WB_SDG<-paste(BBDD_WD_reading_directory, WB_file_SDG,sep="")
WB_SDG<- read.csv(file_WB_SDG, header=TRUE, sep=",", dec=".")

head(WB_SDG)
names(WB_SDG)

# We select only the variables we are interested in
WB_SDG<-WB_SDG[,c("ï..Country.Name","Time","Series.Name","Value")]
colnames(WB_SDG)<-c("Country","Year","Variable","Value")
head(WB_SDG)
levels(WB_SDG$Country)[1]

# We remove empty data Countries
ROWS_TO_REMOVE<-which(WB_SDG[,"Country"]=="")
WB_SDG<-WB_SDG[-ROWS_TO_REMOVE,]
# We remove empty data Values
ROWS_TO_REMOVE<-which(WB_SDG[,"Variable"]=="")
WB_SDG<-WB_SDG[-ROWS_TO_REMOVE,]

rm(BBDD_WB_SDG)

LEVELS_WB_SDG<-levels(droplevels(WB_SDG$Variable))
BBDD_WB_SDG<-WB_SDG[which(WB_SDG[,"Variable"]==LEVELS_WB_SDG[1]),]
VAR_NAME<-paste("WB",LEVELS_WB_SDG[1],sep="-")
colnames(BBDD_WB_SDG)<-c("Country","Year","Variable",VAR_NAME)
BBDD_WB_SDG<-subset(BBDD_WB_SDG, select = -c(Variable))

for (i in 2:length(LEVELS_WB_SDG))
{
  A<-WB_SDG[which(WB_SDG[,"Variable"]==LEVELS_WB_SDG[i]),]
  VAR_NAME<-paste("WB",LEVELS_WB_SDG[i],sep="-")
  colnames(A)<-c("Country","Year","Variable",VAR_NAME)
  A<-subset(A, select = -c(Variable))
  BBDD_WB_SDG<-merge(BBDD_WB_SDG, A,  by=c("Country","Year"), all=TRUE)
}



#View(BBDD_WB_SDG[which(BBDD_WB_SDG[,"Country"]=="Spain"),])


#--------------------------------------------------------------------------------------------------------
#                                  CREACIÓN BBDD WORLD-BANK - EMISSIONS
# --------------------------------------------------------------------------------------------------------
# We read CSV file
rm(WB_EMISSIONS)
WB_file_EMISSIONS<-"WORLD-BANK - GHG Emissions.csv"
file_WB_EMISSIONS<-paste(BBDD_WD_reading_directory, WB_file_EMISSIONS,sep="")
WB_EMISSIONS<- read.csv(file_WB_EMISSIONS, header=TRUE, sep=",", dec=".")

head(WB_EMISSIONS)
names(WB_EMISSIONS)





#************************************************************************************************************************************************************
#************************************************************************************************************************************************************
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# -------------------------------------------------------------------------------------------------------
# EDGAR EMISSIONS
# -------------------------------------------------------------------------------------------------------
#************************************************************************************************************************************************************
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#************************************************************************************************************************************************************
#************************************************************************************************************************************************************

file_EDGAR_EMISSIONS <-"EDGAR - Emissions.xls"
file_EDGAR_EMISSIONS<-paste(BBDD_EC_reading_directory, file_EDGAR_EMISSIONS ,sep="")

# -------------------------------------------------------------------------------------------------------
# EDGAR EMISSIONS: Total Emissions
# -------------------------------------------------------------------------------------------------------

rm(BBDD_EDGAR_Emissions.Total)
rm(EDGAR_Emissions_DATA)

EDGAR_Emissions_DATA <-read.xls(file_EDGAR_EMISSIONS, sheet = "Total", header=FALSE)


MIN_Year<-min(EDGAR_Emissions_DATA[1,2:ncol(EDGAR_Emissions_DATA)])
MAX_Year<-max(EDGAR_Emissions_DATA[1,2:ncol(EDGAR_Emissions_DATA)])
Factor_Countries<-levels(EDGAR_Emissions_DATA[,1])

NUM_Years <- (MAX_Year-MIN_Year+1)
NUM_Countries <-length(Factor_Countries)

BBDD_EDGAR_Emissions.Total<-fGenDataPanel(NUM_Countries,NUM_Years,EDGAR_Emissions_DATA)
colnames(BBDD_EDGAR_Emissions.Total)<-c("Country", "Year","Total_Emissions")

# -------------------------------------------------------------------------------------------------------
# EDGAR EMISSIONS: Transport Emissions
# -------------------------------------------------------------------------------------------------------

rm(BBDD_EDGAR_Emissions.Transport)
rm(EDGAR_Emissions_DATA)

EDGAR_Emissions_DATA <-read.xls(file_EDGAR_EMISSIONS, sheet = "Transport", header=FALSE)

MIN_Year<-min(EDGAR_Emissions_DATA[1,2:ncol(EDGAR_Emissions_DATA)])
MAX_Year<-max(EDGAR_Emissions_DATA[1,2:ncol(EDGAR_Emissions_DATA)])
Factor_Countries<-levels(EDGAR_Emissions_DATA[,1])

NUM_Years <- (MAX_Year-MIN_Year+1)
NUM_Countries <-length(Factor_Countries)

BBDD_EDGAR_Emissions.Transport<-fGenDataPanel(NUM_Countries,NUM_Years,EDGAR_Emissions_DATA)
colnames(BBDD_EDGAR_Emissions.Transport)<-c("Country", "Year","Transport_Emissions")

# -------------------------------------------------------------------------------------------------------
# EDGAR EMISSIONS: Road Emissions
# -------------------------------------------------------------------------------------------------------
rm(BBDD_EDGAR_Emissions.Road)
rm(EDGAR_Emissions_DATA)

EDGAR_Emissions_DATA <-read.xls(file_EDGAR_EMISSIONS, sheet = "Road", header=FALSE)

MIN_Year<-min(EDGAR_Emissions_DATA[1,2:ncol(EDGAR_Emissions_DATA)])
MAX_Year<-max(EDGAR_Emissions_DATA[1,2:ncol(EDGAR_Emissions_DATA)])
Factor_Countries<-levels(EDGAR_Emissions_DATA[,1])

NUM_Years <- (MAX_Year-MIN_Year+1)
NUM_Countries <-length(Factor_Countries)

BBDD_EDGAR_Emissions.Road<-fGenDataPanel(NUM_Countries,NUM_Years,EDGAR_Emissions_DATA)
colnames(BBDD_EDGAR_Emissions.Road)<-c("Country", "Year","Road_Emissions")

head(BBDD_EDGAR_Emissions.Total)
head(BBDD_EDGAR_Emissions.Transport)
head(BBDD_EDGAR_Emissions.Road)

# -----------------------------------------------------------------------------------------------------
# EDGAR EMISSIONS - MERGING ALL DATA IN BBDD_EDGAR_EMISSIONS
# -------------------------------------------------------------------------------------------------------

rm(BBDD_EDGAR_EMISSIONS)
BBDD_EDGAR_EMISSIONS<-merge(BBDD_EDGAR_Emissions.Total, BBDD_EDGAR_Emissions.Transport, by=c("Country","Year"), all=TRUE)
BBDD_EDGAR_EMISSIONS<-merge(BBDD_EDGAR_EMISSIONS, BBDD_EDGAR_Emissions.Road, by=c("Country","Year"), all=TRUE)
BBDD_EDGAR_EMISSIONS[which(BBDD_EDGAR_EMISSIONS[,"Country"]=="Spain"),]

BBDD_EDGAR_EMISSIONS[,"Country"]<-as.factor(BBDD_EDGAR_EMISSIONS[,"Country"])
BBDD_EDGAR_EMISSIONS[,"Year"]<-as.integer(BBDD_EDGAR_EMISSIONS[,"Year"])
str(BBDD_EDGAR_EMISSIONS)



#************************************************************************************************************************************************************
#************************************************************************************************************************************************************
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                                     EUROPEAN COMMISSION (EC) DATABASE
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#************************************************************************************************************************************************************
#************************************************************************************************************************************************************


# -------------------------------------------------------------------------------------------------------
file_EC_TRANSPORT <-"EC-Transport Data.xlsx"
file_EC_TRANSPORT<-paste(BBDD_EC_reading_directory, file_EC_TRANSPORT ,sep="")

# -------------------------------------------------------------------------------------------------------
#                                             EC - RAIL PASSENGER KM
# -------------------------------------------------------------------------------------------------------
rm(BBDD_EC_PassKmRail)
rm(EC_PassKmRail_DATA)

EC_PassKmRail_DATA <-read.xls(file_EC_TRANSPORT, sheet = "EC-rail_billion_Passenger_km", header=FALSE)

MIN_Year<-min(EC_PassKmRail_DATA[1,2:ncol(EC_PassKmRail_DATA)])
MAX_Year<-max(EC_PassKmRail_DATA[1,2:ncol(EC_PassKmRail_DATA)])
Factor_Countries<-levels(EC_PassKmRail_DATA[,1])

NUM_Years <- (MAX_Year-MIN_Year+1)
NUM_Countries <-length(Factor_Countries)

BBDD_EC_PassKmRail<-fGenDataPanel(NUM_Countries,NUM_Years,EC_PassKmRail_DATA)
colnames(BBDD_EC_PassKmRail)<-c("Country", "Year","EC-RAIL Billion Pass Km")

#View(BBDD_EC_PassKmRail)


# -------------------------------------------------------------------------------------------------------
#                                            EC - RAIL BILLION TONNES KM
# -------------------------------------------------------------------------------------------------------
rm(BBDD_EC_TonnesKmRail)
rm(EC_TonnesKmRail_DATA)

EC_TonnesKmRail_DATA <-read.xls(file_EC_TRANSPORT, sheet = "EC-rail_billion_Tonnes_km", header=FALSE)

MIN_Year<-min(EC_TonnesKmRail_DATA[1,2:ncol(EC_TonnesKmRail_DATA)])
MAX_Year<-max(EC_TonnesKmRail_DATA[1,2:ncol(EC_TonnesKmRail_DATA)])
Factor_Countries<-levels(EC_TonnesKmRail_DATA[,1])

NUM_Years <- (MAX_Year-MIN_Year+1)
NUM_Countries <-length(Factor_Countries)

BBDD_EC_TonnesKmRail<-fGenDataPanel(NUM_Countries,NUM_Years,EC_TonnesKmRail_DATA)
colnames(BBDD_EC_TonnesKmRail)<-c("Country", "Year","EC-RAIL Billion Tonnes Km")

#View(BBDD_EC_TonnesKmRail)

# -------------------------------------------------------------------------------------------------------
#                                          EC - TRAM & METRO PASSENGER KM
# -------------------------------------------------------------------------------------------------------
rm(BBDD_EC_PassKm_Tram_Metro)
rm(EC_PassKm_Tram_Metro_DATA)

EC_PassKm_Tram_Metro_DATA <-read.xls(file_EC_TRANSPORT, sheet = "EC-Tram_metro_billion_Pass_Km", header=FALSE)

MIN_Year<-min(EC_PassKm_Tram_Metro_DATA[1,2:ncol(EC_PassKm_Tram_Metro_DATA)])
MAX_Year<-max(EC_PassKm_Tram_Metro_DATA[1,2:ncol(EC_PassKm_Tram_Metro_DATA)])
Factor_Countries<-levels(EC_PassKm_Tram_Metro_DATA[,1])

NUM_Years <- (MAX_Year-MIN_Year+1)
NUM_Countries <-length(Factor_Countries)

BBDD_EC_PassKm_Tram_Metro<-fGenDataPanel(NUM_Countries,NUM_Years,EC_PassKm_Tram_Metro_DATA)
colnames(BBDD_EC_PassKm_Tram_Metro)<-c("Country", "Year","EC-Tram&metro Billion Pass Km")

#View(BBDD_EC_PassKm_Tram_Metro)

# -------------------------------------------------------------------------------------------------------
#                                      EC - BUSES & COACHES PASSENGER KM
# -------------------------------------------------------------------------------------------------------
rm(BBDD_EC_PassKm_Buses_Coaches)
rm(EC_PassKm_Buses_Coaches_DATA)

EC_PassKm_Buses_Coaches_DATA <-read.xls(file_EC_TRANSPORT, sheet = "EC-Bus&coaches_billion_Pass_Km", header=FALSE)

MIN_Year<-min(EC_PassKm_Buses_Coaches_DATA[1,2:ncol(EC_PassKm_Buses_Coaches_DATA)])
MAX_Year<-max(EC_PassKm_Buses_Coaches_DATA[1,2:ncol(EC_PassKm_Buses_Coaches_DATA)])
Factor_Countries<-levels(EC_PassKm_Buses_Coaches_DATA[,1])

NUM_Years <- (MAX_Year-MIN_Year+1)
NUM_Countries <-length(Factor_Countries)

BBDD_EC_PassKm_Buses_Coaches<-fGenDataPanel(NUM_Countries,NUM_Years,EC_PassKm_Buses_Coaches_DATA)
colnames(BBDD_EC_PassKm_Buses_Coaches)<-c("Country", "Year","EC-Bus&coaches Billion Pass Km")

#View(BBDD_EC_PassKm_Buses_Coaches)

# -------------------------------------------------------------------------------------------------------
#                                   EC - cars PASSENGER KM
# -------------------------------------------------------------------------------------------------------
rm(BBDD_EC_PassKm_Cars)
rm(EC_PassKm_Cars_DATA)

EC_PassKm_Cars_DATA <-read.xls(file_EC_TRANSPORT, sheet = "EC-Cars_Pass_Km", header=FALSE)

MIN_Year<-min(EC_PassKm_Cars_DATA[1,2:ncol(EC_PassKm_Cars_DATA)])
MAX_Year<-max(EC_PassKm_Cars_DATA[1,2:ncol(EC_PassKm_Cars_DATA)])
Factor_Countries<-levels(EC_PassKm_Cars_DATA[,1])

NUM_Years <- (MAX_Year-MIN_Year+1)
NUM_Countries <-length(Factor_Countries)

BBDD_EC_PassKm_Cars<-fGenDataPanel(NUM_Countries,NUM_Years,EC_PassKm_Cars_DATA)
colnames(BBDD_EC_PassKm_Cars)<-c("Country", "Year","EC-CARS Pass Km")

#View(BBDD_EC_PassKm_Cars)


# -------------------------------------------------------------------------------------------------------
#                                EC - MERGING ALL DATA IN BBDD_EC_TRANSPORT
# -------------------------------------------------------------------------------------------------------

rm(BBDD_EC_TRANSPORT)
BBDD_EC_TRANSPORT<-merge(BBDD_EC_PassKmRail, BBDD_EC_TonnesKmRail, by=c("Country","Year"), all=TRUE)
BBDD_EC_TRANSPORT<-merge(BBDD_EC_TRANSPORT, BBDD_EC_PassKm_Tram_Metro, by=c("Country","Year"), all=TRUE)
BBDD_EC_TRANSPORT<-merge(BBDD_EC_TRANSPORT, BBDD_EC_PassKm_Buses_Coaches, by=c("Country","Year"), all=TRUE)
BBDD_EC_TRANSPORT<-merge(BBDD_EC_TRANSPORT, BBDD_EC_PassKm_Cars, by=c("Country","Year"), all=TRUE)

TOTAL_Pass_Kms<-rowSums((BBDD_EC_TRANSPORT[,c("EC-RAIL Billion Pass Km","EC-Bus&coaches Billion Pass Km",
                                              "EC-Tram&metro Billion Pass Km","EC-CARS Pass Km")]))

# -------------------------------------------------------------------------------------------------------
#                                        EC - ADDING % Pass Km
# -------------------------------------------------------------------------------------------------------

A<-BBDD_EC_TRANSPORT[,"EC-RAIL Billion Pass Km"]/TOTAL_Pass_Kms*100
B<-BBDD_EC_TRANSPORT[,"EC-Bus&coaches Billion Pass Km"]/TOTAL_Pass_Kms*100
C<-BBDD_EC_TRANSPORT[,"EC-Tram&metro Billion Pass Km"]/TOTAL_Pass_Kms*100
D<-BBDD_EC_TRANSPORT[,"EC-CARS Pass Km"]/TOTAL_Pass_Kms*100

E<-data.frame(BBDD_EC_TRANSPORT,A,B,C,D)

colnames(E)<-c(colnames(BBDD_EC_TRANSPORT),"EC- % RAIL Pass Km / TOTAL", 
               "EC- % Bus&coaches Pass Km / TOTAL",
               "EC- % Tram&metro Pass Km / TOTAL",
               "EC- % CARS Pass Km / TOTAL")
BBDD_EC_TRANSPORT<-E
# We set Country column as factor
BBDD_EC_TRANSPORT[,"Country"]<-as.factor(BBDD_EC_TRANSPORT[,"Country"])
#View(BBDD_EC_TRANSPORT)




#************************************************************************************************************************************************************
#************************************************************************************************************************************************************
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                                     EUROSTAT DATABASE
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#************************************************************************************************************************************************************
#************************************************************************************************************************************************************


#************************************************************************************************************************************************************
# -----------------------------------------------------------------------------------------------------------------------------------------
#                                     EUROSTAT - ROAD OIL CONSUMPTION DATABASE 
# ------------------------------------------------------------------------------------------------------------------------------------------
#************************************************************************************************************************************************************


# We read CSV file
file_EUROSTAT_road<-"ENERGY/EUROSTAT - Road Oil - Total Units.csv"
file_EUROSTAT_road<-paste(BBDD_EUROSTAT_reading_directory, file_EUROSTAT_road,sep="")
OIL_EUROSTAT_ROADdata<- read.csv(file_EUROSTAT_road, header=TRUE, sep=",", dec=".")

# We modify structure of Values due to the fact that Values are strings (factors) instead of numerical Values
OIL_EUROSTAT_ROADdata$Value<-as.numeric(gsub(",", "", OIL_EUROSTAT_ROADdata$Value))
head(OIL_EUROSTAT_ROADdata)

# We select only the variables we are interested in
OIL_EUROSTAT_ROADdata<-OIL_EUROSTAT_ROADdata[,c("GEO","TIME","PRODUCT","UNIT","Value")]
colnames(OIL_EUROSTAT_ROADdata)<-c("Country","Year","Variable","Unit","Value")
head(OIL_EUROSTAT_ROADdata)


# ------------------------------------------------------------------------------------------------------------------------------
#                                    EUROSTAT: CREACIÓN BBDD Consumo conmbustibles carretera
# ------------------------------------------------------------------------------------------------------------------------------

rm(BBDD_EUROSTAT_ROAD_OIL)

LEVELS_ROAD_OIL<-levels(OIL_EUROSTAT_ROADdata$Variable)
LEVELS_ROAD_OIL_UNIT<-levels(OIL_EUROSTAT_ROADdata$Unit)

BBDD_EUROSTAT_ROAD_OIL<-OIL_EUROSTAT_ROADdata[which(OIL_EUROSTAT_ROADdata[,"Variable"]==LEVELS_ROAD_OIL[1] & 
                                                      OIL_EUROSTAT_ROADdata[,"Unit"]==LEVELS_ROAD_OIL_UNIT[1]),]
colnames(BBDD_EUROSTAT_ROAD_OIL)<-
  c("Country","Year","Variable", "Unit", paste("EUROSTAT",LEVELS_ROAD_OIL[1],LEVELS_ROAD_OIL_UNIT[1],sep="-"))

BBDD_EUROSTAT_ROAD_OIL<-subset(BBDD_EUROSTAT_ROAD_OIL, select = -c(Variable,Unit))

for (j in 1:length(LEVELS_ROAD_OIL_UNIT))
{
  #First_item <- (j-1)*length(LEVELS_TI)+1
  
  for (i in 1:length(LEVELS_ROAD_OIL))
  {
    if ( (j*i)==1) next
    A<-OIL_EUROSTAT_ROADdata[which(OIL_EUROSTAT_ROADdata[,"Variable"]==LEVELS_ROAD_OIL[i] &
                                     OIL_EUROSTAT_ROADdata[,"Unit"]==LEVELS_ROAD_OIL_UNIT[j]),]
    colnames(A)<-c("Country","Year","Variable","Unit", paste("EUROSTAT",LEVELS_ROAD_OIL[i], LEVELS_ROAD_OIL_UNIT[j],sep="-"))
    #print(paste(LEVELS_TI[i],LEVELS_TI_UNIT[j],sep="-"))
    A<-subset(A, select = -c(Variable,Unit))
    BBDD_EUROSTAT_ROAD_OIL<-merge(BBDD_EUROSTAT_ROAD_OIL, A,  by=c("Country","Year"), all=TRUE)
  }
}


# We create external data files


# COMPARATIVAS PARA CHEQUEAR ENTRE DATOS EUROSTAT Y DATOS OECDE
A<-BBDD_EUROSTAT_ROAD_OIL[which(BBDD_EUROSTAT_ROAD_OIL[,"Country"]=="Spain"),]
B<-BBDD_OECD_OIL_Road[which(BBDD_OECD_OIL_Road[,"Country"]=="Spain"),]
C<-merge(B, A,  by=c("Country","Year"), all=TRUE)

A<-BBDD_EUROSTAT_ROAD_OIL[which(BBDD_EUROSTAT_ROAD_OIL[,"Year"]>=1990),]
B<-BBDD_OECD_OIL_Road[which(BBDD_OECD_OIL_Road[,"Year"]>=1990),]
C<-merge(B, A,  by=c("Country","Year"), all=TRUE)

DiffGasoline<-C[,"OECD-Motor gasoline excl. biofuels (kt)"]- C[,"EUROSTAT-Gasoline (without bio components)-Thousand tonnes"]
DiffDiesel<-C[,"OECD-Gas/diesel oil excl. biofuels (kt)"]-C[,"EUROSTAT-Gas/diesel oil (without bio components)-Thousand tonnes"]

C<-cbind(C,DiffGasoline,DiffDiesel)

# Visualizamos los datos que son diferentes
C[which(C[,"DiffGasoline"]!=0 | C[,"DiffDiesel"]!=0), c("Country","Year","DiffGasoline","DiffDiesel")]

#View(na.omit(C[,c("Country","Year","OECD-Motor gasoline excl. biofuels (kt)","EUROSTAT-Gasoline (without bio components)-Thousand tonnes", "DiffGasoline",
#"OECD-Gas/diesel oil excl. biofuels (kt)", "EUROSTAT-Gas/diesel oil (without bio components)-Thousand tonnes","DiffDiesel")]))


#******************************************************************************************************************************************
# ----------------------------------------------------------------------------------------------------------------------------------------
#                                                EUROSTAT - ROAD EQUIPMENT
# -----------------------------------------------------------------------------------------------------------------------------------------
#******************************************************************************************************************************************

# ------------------------------------------------------------------------------------------------------------------------------
#                                EUROSTAT - PASSENGER CARS x 1000 INHABITANTS Table - EUROSTAT_PassCars
# ------------------------------------------------------------------------------------------------------------------------------

# We read CSV file
rm(EUROSTAT_PassCars)
file_EUROSTAT_PassCars<-"/TRANSPORT/ROAD/Equipment/EUROSTAT - Passenger cars per 1 000 inhabitants.csv"
file_EUROSTAT_PassCars<-paste(BBDD_EUROSTAT_reading_directory, file_EUROSTAT_PassCars,sep="")
EUROSTAT_PassCars<- read.csv(file_EUROSTAT_PassCars, header=TRUE, sep=",", dec=".")

# We modify structure of Values due to the fact that Values are strings (factors) instead of numerical Values
EUROSTAT_PassCars$Value<-as.numeric(gsub(",", "", EUROSTAT_PassCars$Value))

head(EUROSTAT_PassCars)
tail(EUROSTAT_PassCars)
names(EUROSTAT_PassCars)
dim(EUROSTAT_PassCars)
length(EUROSTAT_PassCars$Variable)

EUROSTAT_PassCars<-EUROSTAT_PassCars[,c("GEO","TIME","UNIT","Value")]
colnames(EUROSTAT_PassCars)<-c("Country","Year","Variable","EUROSTAT-Passenger cars per 1000 inhabitants")

rm(BBDD_EUROSTAT_PassCars)
BBDD_EUROSTAT_PassCars<-subset(EUROSTAT_PassCars, select = -c(Variable))



# -------------------------------------------------------------------------------------------------------------------
#                    EUROSTAT - PASSENGER CARS by age Table - "EUROSTAT - Passenger cars by age"
# -------------------------------------------------------------------------------------------------------------------


# We read CSV file
rm(EUROSTAT_PassCars_byAge)
file_EUROSTAT_PassCars_byAge<-"/TRANSPORT/ROAD/Equipment/EUROSTAT - Passenger cars by age.csv"
file_EUROSTAT_PassCars_byAge<-paste(BBDD_EUROSTAT_reading_directory, file_EUROSTAT_PassCars_byAge,sep="")
EUROSTAT_PassCars_byAge<- read.csv(file_EUROSTAT_PassCars_byAge, header=TRUE, sep=",", dec=".")

# We modify structure of Values due to the fact that Values are strings (factors) instead of numerical Values
EUROSTAT_PassCars_byAge$Value<-as.numeric(gsub(",", "", EUROSTAT_PassCars_byAge$Value))

# head(EUROSTAT_PassCars_byAge)
# tail(EUROSTAT_PassCars_byAge)
# names(EUROSTAT_PassCars_byAge)
# dim(EUROSTAT_PassCars_byAge)
# length(EUROSTAT_PassCars_byAge$Variable)

EUROSTAT_PassCars_byAge<-EUROSTAT_PassCars_byAge[,c("GEO","TIME","AGE","Value")]
colnames(EUROSTAT_PassCars_byAge)<-c("Country","Year","Variable","Value")
head(EUROSTAT_PassCars_byAge)

rm(BBDD_EUROSTAT_PassCars_byAge)

LEVELS_EUROSTAT_PassCars_byAge<-levels(droplevels(EUROSTAT_PassCars_byAge$Variable))
BBDD_EUROSTAT_PassCars_byAge<-
  EUROSTAT_PassCars_byAge[which(EUROSTAT_PassCars_byAge[,"Variable"]==LEVELS_EUROSTAT_PassCars_byAge[1]),]
VAR_NAME<-paste("EUROSTAT",LEVELS_EUROSTAT_PassCars_byAge[1],sep="-")
colnames(BBDD_EUROSTAT_PassCars_byAge)<-c("Country","Year","Variable",VAR_NAME)
BBDD_EUROSTAT_PassCars_byAge<-subset(BBDD_EUROSTAT_PassCars_byAge, select = -c(Variable))

for (i in 2:length(LEVELS_EUROSTAT_PassCars_byAge))
{
  A<-EUROSTAT_PassCars_byAge[which(EUROSTAT_PassCars_byAge[,"Variable"]==LEVELS_EUROSTAT_PassCars_byAge[i]),]
  VAR_NAME<-paste("EUROSTAT",LEVELS_EUROSTAT_PassCars_byAge[i],sep="-")
  colnames(A)<-c("Country","Year","Variable",VAR_NAME)
  A<-subset(A, select = -c(Variable))
  BBDD_EUROSTAT_PassCars_byAge<-merge(BBDD_EUROSTAT_PassCars_byAge, A,  by=c("Country","Year"), all=TRUE)
}


#View(BBDD_EUROSTAT_PassCars_byAge)

# ------------------------------------------------------------------------------------------------------------------
#              EUROSTAT - EUROSTAT - Passenger cars, by type of motor energy and size of engine DATABASE 
# ------------------------------------------------------------------------------------------------------------------

rm(BBDD_EUROSTAT_PassCars_Motor)
# We read CSV file
file_EUROSTAT_PassCars_Motor<-"/TRANSPORT/ROAD/Equipment/EUROSTAT - Passenger cars, by type of motor energy and size of engine.csv"
file_EUROSTAT_PassCars_Motor<-paste(BBDD_EUROSTAT_reading_directory, file_EUROSTAT_PassCars_Motor,sep="")
EUROSTAT_PassCars_Motor_DATA<- read.csv(file_EUROSTAT_PassCars_Motor, header=TRUE, sep=",", dec=".")

# We modify structure of Values due to the fact that Values are strings (factors) instead of numerical Values
EUROSTAT_PassCars_Motor_DATA$Value<-as.numeric(gsub(",", "", EUROSTAT_PassCars_Motor_DATA$Value))
head(EUROSTAT_PassCars_Motor_DATA)

# We select only the variables we are interested in
EUROSTAT_PassCars_Motor_DATA<-EUROSTAT_PassCars_Motor_DATA[,c("GEO","TIME","PROD_NRG","ENGINE","Value")]
colnames(EUROSTAT_PassCars_Motor_DATA)<-c("Country","Year","Fuel","Engine","Value")
head(EUROSTAT_PassCars_Motor_DATA)

#colnames(OIL_EUROSTAT_ROADdata)<-c("Country","Year","Variable","Unit","Value")

names(EUROSTAT_PassCars_Motor_DATA)


# -----------------------------------------------------------------------------------------------------------------------
#                CREACIÓN BBDD EUROSTAT - Passenger cars, by type of motor energy and size of engine DATABASE  
# -----------------------------------------------------------------------------------------------------------------------
rm(BBDD_EUROSTAT_PassCars_Motor)


LEVELS_PASSENGER_CARS_FUEL<-levels(EUROSTAT_PassCars_Motor_DATA$Fuel)
LEVELS_PASSENGER_CARS_ENGINE<-levels(EUROSTAT_PassCars_Motor_DATA$Engine)

BBDD_EUROSTAT_PassCars_Motor<-EUROSTAT_PassCars_Motor_DATA[which(EUROSTAT_PassCars_Motor_DATA[,"Fuel"]==LEVELS_PASSENGER_CARS_FUEL[1] & 
                                                      EUROSTAT_PassCars_Motor_DATA[,"Engine"]==LEVELS_PASSENGER_CARS_ENGINE[1]),]
colnames(BBDD_EUROSTAT_PassCars_Motor)<-
  c("Country","Year","Fuel", "Engine", paste("EUROSTAT",LEVELS_PASSENGER_CARS_FUEL[1],LEVELS_PASSENGER_CARS_ENGINE[1],sep="-"))

BBDD_EUROSTAT_PassCars_Motor<-subset(BBDD_EUROSTAT_PassCars_Motor, select = -c(Fuel,Engine))

for (j in 1:length(LEVELS_PASSENGER_CARS_ENGINE))
{
  #First_item <- (j-1)*length(LEVELS_TI)+1
  
  for (i in 1:length(LEVELS_PASSENGER_CARS_FUEL))
  {
    if ( (j*i)==1) next
    A<-EUROSTAT_PassCars_Motor_DATA[which(EUROSTAT_PassCars_Motor_DATA[,"Fuel"]==LEVELS_PASSENGER_CARS_FUEL[i] &
                                     EUROSTAT_PassCars_Motor_DATA[,"Engine"]==LEVELS_PASSENGER_CARS_ENGINE[j]),]
    colnames(A)<-c("Country","Year","Fuel","Engine",
                   paste("EUROSTAT",LEVELS_PASSENGER_CARS_FUEL[i], LEVELS_PASSENGER_CARS_ENGINE[j],sep="-"))
    #print(paste(LEVELS_TI[i],LEVELS_TI_UNIT[j],sep="-"))
    A<-subset(A, select = -c(Fuel, Engine))
    BBDD_EUROSTAT_PassCars_Motor<-merge(BBDD_EUROSTAT_PassCars_Motor, A,  by=c("Country","Year"), all=TRUE)
  }
}


# We create external data files

#View(BBDD_EUROSTAT_PassCars_Motor)

# --------------------------------------------------------------------------------------------------------
# ------ CREACIÓN BBDD EUROSTAT - Road data equipment - BBDD_EUROSTAT_ROAD_EQUIPMENT
# --------------------------------------------------------------------------------------------------------
rm(BBDD_EUROSTAT_ROAD_EQUIPMENT)
BBDD_EUROSTAT_ROAD_EQUIPMENT<-merge(BBDD_EUROSTAT_PassCars, BBDD_EUROSTAT_PassCars_byAge,  by=c("Country","Year"), all=TRUE)
BBDD_EUROSTAT_ROAD_EQUIPMENT<-merge(BBDD_EUROSTAT_ROAD_EQUIPMENT, BBDD_EUROSTAT_PassCars_Motor,  by=c("Country","Year"), all=TRUE)




#View(BBDD_EUROSTAT_ROAD_EQUIPMENT)
# --------------------------------------------------------------------------------------------------------
#************************************************************************************************************************************************************
#************************************************************************************************************************************************************



#***************************************************************************************************************************************
# -------------------------------------------------------------------------------------------------------------------------------------
#                                     EUROSTAT - GDP AND MAIN COMPONENTS
# --------------------------------------------------------------------------------------------------------------------------------------
#***************************************************************************************************************************************

rm(BBDD_EUROSTAT_GDP_and_COMPONENTS)
# We read CSV file
file_EUROSTAT_GDP_and_COMPONENTS<-"/MACROECONOMIC AGGREGATES/EUROSTAT - GDP and main components.csv"
file_EUROSTAT_GDP_and_COMPONENTS<-paste(BBDD_EUROSTAT_reading_directory, file_EUROSTAT_GDP_and_COMPONENTS,sep="")
EUROSTAT_GDP_and_COMPONENTS_DATA<- read.csv(file_EUROSTAT_GDP_and_COMPONENTS, header=TRUE, sep=",", dec=".")

# We modify structure of Values due to the fact that Values are strings (factors) instead of numerical Values
EUROSTAT_GDP_and_COMPONENTS_DATA$Value<-as.numeric(gsub(",", "", EUROSTAT_GDP_and_COMPONENTS_DATA$Value))
head(EUROSTAT_GDP_and_COMPONENTS_DATA)

# We select only the variables we are interested in
EUROSTAT_GDP_and_COMPONENTS_DATA<-EUROSTAT_GDP_and_COMPONENTS_DATA[,c("GEO","TIME","NA_ITEM","UNIT","Value")]
colnames(EUROSTAT_GDP_and_COMPONENTS_DATA)<-c("Country","Year","Variable","Unit","Value")
head(EUROSTAT_GDP_and_COMPONENTS_DATA)
names(EUROSTAT_GDP_and_COMPONENTS_DATA)



# --------------------------------------------------------------------------------------------------------
# ------ CREACIÓN BBDD GDP AND MAIN COMPONENTS 
# --------------------------------------------------------------------------------------------------------
rm(BBDD_EUROSTAT_GDP_and_COMPONENTS)


LEVELS_GDP_and_COMPONENTS_Variable<-levels(EUROSTAT_GDP_and_COMPONENTS_DATA$Variable)
LEVELS_GDP_and_COMPONENTS_Unit<-levels(EUROSTAT_GDP_and_COMPONENTS_DATA$Unit)

BBDD_EUROSTAT_GDP_and_COMPONENTS<-EUROSTAT_GDP_and_COMPONENTS_DATA[which(EUROSTAT_GDP_and_COMPONENTS_DATA[,"Variable"]==LEVELS_GDP_and_COMPONENTS_Variable[1] & 
                                                                   EUROSTAT_GDP_and_COMPONENTS_DATA[,"Unit"]==LEVELS_GDP_and_COMPONENTS_Unit[1]),]
colnames(BBDD_EUROSTAT_GDP_and_COMPONENTS)<-
  c("Country","Year","Variable", "Unit", paste("EUROSTAT",LEVELS_GDP_and_COMPONENTS_Variable[1],LEVELS_GDP_and_COMPONENTS_Unit[1],sep="-"))

BBDD_EUROSTAT_GDP_and_COMPONENTS<-subset(BBDD_EUROSTAT_GDP_and_COMPONENTS, select = -c(Variable,Unit))

for (j in 1:length(LEVELS_GDP_and_COMPONENTS_Unit))
{
  #First_item <- (j-1)*length(LEVELS_TI)+1
  
  for (i in 1:length(LEVELS_GDP_and_COMPONENTS_Variable))
  {
    if ( (j*i)==1) next
    A<-EUROSTAT_GDP_and_COMPONENTS_DATA[which(EUROSTAT_GDP_and_COMPONENTS_DATA[,"Variable"]==LEVELS_GDP_and_COMPONENTS_Variable[i] &
                                            EUROSTAT_GDP_and_COMPONENTS_DATA[,"Unit"]==LEVELS_GDP_and_COMPONENTS_Unit[j]),]
    colnames(A)<-c("Country","Year","Variable","Unit",
                   paste("EUROSTAT",LEVELS_GDP_and_COMPONENTS_Variable[i], LEVELS_GDP_and_COMPONENTS_Unit[j],sep="-"))
    #print(paste(LEVELS_TI[i],LEVELS_TI_UNIT[j],sep="-"))
    A<-subset(A, select = -c(Variable, Unit))
    BBDD_EUROSTAT_GDP_and_COMPONENTS<-merge(BBDD_EUROSTAT_GDP_and_COMPONENTS, A,  by=c("Country","Year"), all=TRUE)
  }
}

#View(BBDD_EUROSTAT_GDP_and_COMPONENTS)



#**************************************************************************************************************************************
# -------------------------------------------------------------------------------------------------------
#                                               EUROSTAT - EMPLOYMENT
# -------------------------------------------------------------------------------------------------------
#**************************************************************************************************************************************

rm(BBDD_EUROSTAT_EMPLOYMENT)
# We read CSV file  
file_EUROSTAT_EMPLOYMENT<-"/POPULATION/EUROSTAT - Unemployment by sex and age - annual average.csv"
file_EUROSTAT_EMPLOYMENT<-paste(BBDD_EUROSTAT_reading_directory, file_EUROSTAT_EMPLOYMENT,sep="")
EUROSTAT_EMPLOYMENT_DATA<- read.csv(file_EUROSTAT_EMPLOYMENT, header=TRUE, sep=",", dec=".")

# We modify structure of Values due to the fact that Values are strings (factors) instead of numerical Values
EUROSTAT_EMPLOYMENT_DATA$Value<-as.numeric(gsub(",", "", EUROSTAT_EMPLOYMENT_DATA$Value))
head(EUROSTAT_EMPLOYMENT_DATA)

# We select only the variables we are interested in
EUROSTAT_EMPLOYMENT_DATA<-EUROSTAT_EMPLOYMENT_DATA[,c("GEO","TIME","UNIT","SEX","AGE","Value")]
colnames(EUROSTAT_EMPLOYMENT_DATA)<-c("Country","Year","Variable","Sex","Age","Value")
head(EUROSTAT_EMPLOYMENT_DATA)
names(EUROSTAT_EMPLOYMENT_DATA)



# --------------------------------------------------------------------------------------------------------
# ------ CREACIÓN BBDD EMPLOYMENT
# --------------------------------------------------------------------------------------------------------
rm(BBDD_EUROSTAT_EMPLOYMENT)

LEVELS_EMPLOYMENT_Variable<-levels(EUROSTAT_EMPLOYMENT_DATA$Variable)
LEVELS_EMPLOYMENT_Sex<-levels(EUROSTAT_EMPLOYMENT_DATA$Sex)
LEVELS_EMPLOYMENT_Age<-levels(EUROSTAT_EMPLOYMENT_DATA$Age)

BBDD_EUROSTAT_EMPLOYMENT<-EUROSTAT_EMPLOYMENT_DATA[which(EUROSTAT_EMPLOYMENT_DATA[,"Variable"]==LEVELS_EMPLOYMENT_Variable[1] & 
                                                         EUROSTAT_EMPLOYMENT_DATA[,"Sex"]==LEVELS_EMPLOYMENT_Sex[1] &
                                                         EUROSTAT_EMPLOYMENT_DATA[,"Age"]==LEVELS_EMPLOYMENT_Age[1]),]

NOMBRE_VARIABLE<-paste("EUROSTAT - UNEMPLOYMENT",LEVELS_EMPLOYMENT_Variable[1],
      LEVELS_EMPLOYMENT_Sex[1],"AGE: ",sep="-")

NOMBRE_VARIABLE<-paste(NOMBRE_VARIABLE, LEVELS_EMPLOYMENT_Age[1])
colnames(BBDD_EUROSTAT_EMPLOYMENT)<-c("Country","Year","Variable","Sex","Age", NOMBRE_VARIABLE)
BBDD_EUROSTAT_EMPLOYMENT<-subset(BBDD_EUROSTAT_EMPLOYMENT, select = -c(Variable, Sex, Age))


for(k in 1: length(LEVELS_EMPLOYMENT_Age))
{
  for (j in 1:length(LEVELS_EMPLOYMENT_Sex))
  {
    #First_item <- (j-1)*length(LEVELS_TI)+1
    
    for (i in 1:length(LEVELS_EMPLOYMENT_Variable))
    {
      if ( (k*j*i)==1) next
      A<-EUROSTAT_EMPLOYMENT_DATA[which(EUROSTAT_EMPLOYMENT_DATA[,"Variable"]==LEVELS_EMPLOYMENT_Variable[i] &
                                                  EUROSTAT_EMPLOYMENT_DATA[,"Sex"]==LEVELS_EMPLOYMENT_Sex[j] &
                                                  EUROSTAT_EMPLOYMENT_DATA[,"Age"]==LEVELS_EMPLOYMENT_Age[k]),]
      
      NOMBRE_VARIABLE<-paste("EUROSTAT - UNEMPLOYMENT",LEVELS_EMPLOYMENT_Variable[i],
                             LEVELS_EMPLOYMENT_Sex[j],"AGE: ",sep="-")
      NOMBRE_VARIABLE<-paste(NOMBRE_VARIABLE, LEVELS_EMPLOYMENT_Age[k])
      colnames(A)<-c("Country","Year","Variable","Sex","Age", NOMBRE_VARIABLE)
      
      #print(paste(LEVELS_TI[i],LEVELS_TI_UNIT[j],sep="-"))
      A<-subset(A, select = -c(Variable, Sex, Age))
      BBDD_EUROSTAT_EMPLOYMENT<-merge(BBDD_EUROSTAT_EMPLOYMENT, A,  by=c("Country","Year"), all=TRUE)
    }
  }
}



#**********************************************************************************************************************************
# ---------------------------------------------------------------------------------------------------------------------------------
#                                             EUROSTAT - POPULATION
# ---------------------------------------------------------------------------------------------------------------------------------
#**********************************************************************************************************************************

rm(BBDD_EUROSTAT_POPULATION)
# We read CSV file
file_EUROSTAT_POPULATION<-"/POPULATION/EUROSTAT - Population on 1 January by age group and sex.csv"
file_EUROSTAT_POPULATION<-paste(BBDD_EUROSTAT_reading_directory, file_EUROSTAT_POPULATION,sep="")
EUROSTAT_POPULATION_DATA<- read.csv(file_EUROSTAT_POPULATION, header=TRUE, sep=",", dec=".")

# We modify structure of Values due to the fact that Values are strings (factors) instead of numerical Values
EUROSTAT_POPULATION_DATA$Value<-as.numeric(gsub(",", "", EUROSTAT_POPULATION_DATA$Value))
head(EUROSTAT_POPULATION_DATA)

# We select only the variables we are interested in
EUROSTAT_POPULATION_DATA<-EUROSTAT_POPULATION_DATA[,c("GEO","TIME","AGE","SEX","Value")]
colnames(EUROSTAT_POPULATION_DATA)<-c("Country","Year","Age","Sex","Value")
head(EUROSTAT_POPULATION_DATA)
names(EUROSTAT_POPULATION_DATA)


# --------------------------------------------------------------------------------------------------------
# ------                            CREACIÓN BBDD EUROSTAT POPULATION
# --------------------------------------------------------------------------------------------------------
rm(BBDD_EUROSTAT_POPULATION)

LEVELS_POPULATION_Age<-levels(EUROSTAT_POPULATION_DATA$Age)
LEVELS_POPULATION_Sex<-levels(EUROSTAT_POPULATION_DATA$Sex)

BBDD_EUROSTAT_POPULATION<-EUROSTAT_POPULATION_DATA[which(EUROSTAT_POPULATION_DATA[,"Age"]==LEVELS_POPULATION_Age[1] & 
                                                                           EUROSTAT_POPULATION_DATA[,"Sex"]==LEVELS_POPULATION_Sex[1]),]

NOMBRE_VARIABLE<-paste("EUROSTAT",LEVELS_POPULATION_Age[1],"SEX: ",sep="-")
NOMBRE_VARIABLE<-paste(NOMBRE_VARIABLE, LEVELS_POPULATION_Sex[1], sep="-")
colnames(BBDD_EUROSTAT_POPULATION)<- c("Country","Year","Age", "Sex", NOMBRE_VARIABLE)

BBDD_EUROSTAT_POPULATION<-subset(BBDD_EUROSTAT_POPULATION, select = -c(Age,Sex))

for (j in 1:length(LEVELS_POPULATION_Sex))
{
  #First_item <- (j-1)*length(LEVELS_TI)+1
  
  for (i in 1:length(LEVELS_POPULATION_Age))
  {
    if ( (j*i)==1) next
    A<-EUROSTAT_POPULATION_DATA[which(EUROSTAT_POPULATION_DATA[,"Age"]==LEVELS_POPULATION_Age[i] &
                                                EUROSTAT_POPULATION_DATA[,"Sex"]==LEVELS_POPULATION_Sex[j]),]
    NOMBRE_VARIABLE<-paste("EUROSTAT",LEVELS_POPULATION_Age[i],"SEX: ", sep="-")
    NOMBRE_VARIABLE<-paste(NOMBRE_VARIABLE, LEVELS_POPULATION_Sex[j])
    colnames(A)<-c("Country","Year","Age","Sex", NOMBRE_VARIABLE)
    #print(paste(LEVELS_TI[i],LEVELS_TI_Sex[j],sep="-"))
    A<-subset(A, select = -c(Age, Sex))
    BBDD_EUROSTAT_POPULATION<-merge(BBDD_EUROSTAT_POPULATION, A,  by=c("Country","Year"), all=TRUE)
  }
}


#View(BBDD_EUROSTAT_POPULATION)





#************************************************************************************************************************************************************
#************************************************************************************************************************************************************
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                                       OCDE DATABASE
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#************************************************************************************************************************************************************
#************************************************************************************************************************************************************




#********************************************************************************************************
#********************************************************************************************************
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# -------------------------------------------------------------------------------------------------------
#                                         OCDE: OECD - BASES DATOS DE MACROECONOMIC AGGREGATES (MA) 
# -------------------------------------------------------------------------------------------------------
# oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#********************************************************************************************************
#********************************************************************************************************




#********************************************************************************************************
# -------------------------------------------------------------------------------------------------------
# OCDE: OECD - MACROECONOMIC AGGREGATES (MA) - Value added and its components by activity
#
#              WARNING!!!!!!!!: ES POSIBLE QUE ESTA BASE DE DATOS SEA DE POCA UTILIDAD
#                               MOTIVO POR EL CUAL DE MOMENTO NO LA CONSTRUYO EN FORMATO PANEL DATA
# -------------------------------------------------------------------------------------------------------
#********************************************************************************************************

OCEDfile_MA_VAdd_Components<-"/MACROECONOMIC AGGREGATES/CSV/OECD - Value added and its components by activity.csv"
file_MA_VAdd_Components<-paste(BBDD_OECD_reading_directory, OCEDfile_MA_VAdd_Components,sep="")
MA_VAdd_Components.Data<- read.csv(file_MA_VAdd_Components)
#View(MA_VAdd_Components.Data)
tail(MA_VAdd_Components.Data)
colnames(MA_VAdd_Components.Data)

# We remove data so as to read available data more neatly
levels(MA_VAdd_Components.Data$ï..LOCATION)
levels(MA_VAdd_Components.Data$Country)
levels(MA_VAdd_Components.Data$TRANSACT)
levels(MA_VAdd_Components.Data$Transaction)
levels(MA_VAdd_Components.Data$ACTIVITY)
levels(MA_VAdd_Components.Data$Activity)
levels(MA_VAdd_Components.Data$Measure)
levels(MA_VAdd_Components.Data$MEASURE)
levels(MA_VAdd_Components.Data$TIME)
levels(MA_VAdd_Components.Data$Year)
levels(MA_VAdd_Components.Data$Unit.Code)
levels(MA_VAdd_Components.Data$Unit)
levels(MA_VAdd_Components.Data$PowerCode.Code)
levels(MA_VAdd_Components.Data$PowerCode)
levels(MA_VAdd_Components.Data$Reference.Period.Code)
levels(MA_VAdd_Components.Data$Reference.Period)
levels(MA_VAdd_Components.Data$Value)
levels(MA_VAdd_Components.Data$Flag.Codes)
levels(MA_VAdd_Components.Data$Flags)

dim(MA_VAdd_Components.Data)
# We make a copy of the original data
MA_VAdd_Components.DataOrig<-MA_VAdd_Components.Data

MA_VAdd_Components.Data<-(subset(MA_VAdd_Components.Data, select=
                                   -c(ï..LOCATION,TRANSACT,ACTIVITY, MEASURE, TIME,Unit.Code,PowerCode.Code,Reference.Period.Code,Flag.Codes) ))

#View(MA_VAdd_Components.Data)

#??? Due to the significan data available (>600), and possibly not useful,
# We take data ONLY for Transport & Total Activitiy

MA_VAdd_Components.Data.Transport.Total<-MA_VAdd_Components.DataOrig[
  which(
    (MA_VAdd_Components.DataOrig$Activity=="Transport, storage and communication") |
      (MA_VAdd_Components.DataOrig$Activity=="Total activity")
  )
  ,]

#View(MA_VAdd_Components.Data.Transport.Total)



# We create a colum with a unique value
colnames(MA_VAdd_Components.Data.Transport.Total)
Value.LABEL<-paste("Transaction", "Activity", "Measure", "Unit", "PowerCode", "Reference.Period",sep="-")
Value.LABEL
# We construct a unique identifying code
VAR.CODE<-paste(MA_VAdd_Components.Data.Transport.Total$Transaction, MA_VAdd_Components.Data.Transport.Total$Activity,
                MA_VAdd_Components.Data.Transport.Total$Measure, MA_VAdd_Components.Data.Transport.Total$Unit,
                MA_VAdd_Components.Data.Transport.Total$PowerCode, MA_VAdd_Components.Data.Transport.Total$Reference.Period, 
                sep=" //*//-")

MA_VAdd_Components.Data.Transport.Total<-(cbind(MA_VAdd_Components.Data.Transport.Total, VAR.CODE))

colnames(MA_VAdd_Components.Data.Transport.Total)
# We take data we consider useful
MA_VAdd_Components.Data.Transport.Total<-MA_VAdd_Components.Data.Transport.Total[,c("Country","Year","VAR.CODE","Value")]
# We rename columns name
colnames(MA_VAdd_Components.Data.Transport.Total)<-c("Country","Year","Variable","Value")

# View(MA_VAdd_Components.Data.Transport.Total)


# We write a text file with all fields for easing field selection task
LEVELS_OECD_MA_VAdd_Components.Transport.Total<-levels(MA_VAdd_Components.Data.Transport.Total$Variable)

file_writing_MA_VAdd_Components<-paste(BBDD_OECD_writing_directory, "BBDD_OECD_MA_VAdd_Components-VARFIELDS.csv",sep="")
write.csv(LEVELS_OECD_MA_VAdd_Components.Transport.Total, file = file_writing_MA_VAdd_Components)

levels(MA_VAdd_Components.Data.Transport.Total[,"Country"])
LEVELS_OECD_MA_VAdd_Components.Transport.Total



#********************************************************************************************************
# -------------------------------------------------------------------------------------------------------
# OCDE: OECD - MACROECONOMIC AGGREGATES (MA) - National Accounts at a glance (MA_NAccounts)
# -------------------------------------------------------------------------------------------------------
#********************************************************************************************************

rm(MA_NAccounts.Data)
OCEDfile_MA_NAccounts<-"/MACROECONOMIC AGGREGATES/CSV/OECD - National Accounts at a glance.csv"
file_MA_NAccounts<-paste(BBDD_OECD_reading_directory, OCEDfile_MA_NAccounts,sep="")
MA_NAccounts.Data<- read.csv(file_MA_NAccounts)
View(MA_NAccounts.Data)
tail(MA_NAccounts.Data)
colnames(MA_NAccounts.Data)

#MA_NAccounts.Data<-MA_NAccounts.DataOrig

#Before removing columns we check it out data contained 
levels(as.factor(MA_NAccounts.Data$ï..LOCATION))
levels(as.factor(MA_NAccounts.Data$Country))
levels(as.factor(MA_NAccounts.Data$INDICATOR))
levels(as.factor(MA_NAccounts.Data$Indicator))
levels(MA_NAccounts.Data$TIME)
levels(MA_NAccounts.Data$Time)
levels(MA_NAccounts.Data$Unit.Code)
levels(MA_NAccounts.Data$Unit)
levels(MA_NAccounts.Data$PowerCode.Code)
levels(MA_NAccounts.Data$PowerCode)
levels(MA_NAccounts.Data$Reference.Period.Code)
levels(MA_NAccounts.Data$Reference.Period)
levels(MA_NAccounts.Data$Value)
levels(MA_NAccounts.Data$Flag.Codes)
levels(MA_NAccounts.Data$Flags)

dim(MA_NAccounts.Data)

# We remove data so as to read available data more neatly
#MA_NAccounts.Data<-MA_NAccounts.DataOrig
MA_NAccounts.DataOrig<-MA_NAccounts.Data
MA_NAccounts.Data<-(subset(MA_NAccounts.Data, select=
                             -c(ï..LOCATION,TIME,Unit.Code,PowerCode.Code,Reference.Period,Reference.Period.Code,Flag.Codes) ))


colnames(MA_NAccounts.Data)
# We take data we consider useful
MA_NAccounts.Data<-MA_NAccounts.Data[,c("Country","Time","Indicator","INDICATOR","Value")]
# We construct a unique identifying code
VAR.CODE<-paste(MA_NAccounts.Data$Indicator, MA_NAccounts.Data$INDICATOR,sep=" //*//-")
MA_NAccounts.Data<-(cbind(MA_NAccounts.Data,VAR.CODE))
colnames(MA_NAccounts.Data)
View(MA_NAccounts.Data)

colnames(MA_NAccounts.Data)<-c("Country","Year","VariableTemp","Variable.CODE","Value","Variable")
MA_NAccounts.Data<-MA_NAccounts.Data[,c("Country","Year","Variable","Value")]
#View(MA_NAccounts.Data)
MA_NAccounts.Data$Variable
levels(as.factor(MA_NAccounts.Data$Variable))
View(MA_NAccounts.Data)


# -----------------------------------------------------------------------------------------------------------
# ---- OCDE: CREACIÓN BBDD con Macroeconomic Aggregates
# -----------------------------------------------------------------------------------------------------------


# We generate data files
# rm(BBDD_OECD_MA_NAccounts)
# LEVELS_OECD_MA_NAccounts<-levels(MA_NAccounts.Data$Variable)
# BBDD_OECD_MA_NAccounts<-MA_NAccounts.Data[which(MA_NAccounts.Data[,"Variable"]==LEVELS_OECD_MA_NAccounts[1]),]
# VAR_NAME<-paste("OECD",LEVELS_OECD_MA_NAccounts[1],sep="-")
# colnames(BBDD_OECD_MA_NAccounts)<-c("Country","Year","Variable",VAR_NAME)
# BBDD_OECD_MA_NAccounts<-subset(BBDD_OECD_MA_NAccounts, select = -c(Variable))
# for (i in 2:length(LEVELS_OECD_MA_NAccounts))
#{
#  A<-MA_NAccounts.Data[which(MA_NAccounts.Data[,"Variable"]==LEVELS_OECD_MA_NAccounts[i]),]
#  VAR_NAME<-paste("OECD",LEVELS_OECD_MA_NAccounts[i],sep="-")
#  colnames(A)<-c("Country","Year","Variable",VAR_NAME)
#  A<-subset(A, select = -c(Variable))
#  BBDD_OECD_MA_NAccounts<-merge(BBDD_OECD_MA_NAccounts, A,  by=c("Country","Year"), all=TRUE)
#}

rm(BBDD_OECD_MA_NAccounts)
BBDD_OECD_MA_NAccounts<-fGenBBDDPanel.1Dimension(MA_NAccounts.Data,BBDD_OECD_MA_NAccounts,"OECD~NA~NAG")

file_writing_MA_NAccounts<-paste(BBDD_OECD_writing_directory, "BBDD_OECD_MA_NAccounts.csv",sep="")
write.csv(BBDD_OECD_MA_NAccounts, file = file_writing_MA_NAccounts)

#View(BBDD_OECD_MA_NAccounts)
dim(BBDD_OECD_MA_NAccounts)
dim(distinct(BBDD_OECD_MA_NAccounts))



# Additionally we write a text file with all fields for easing field selection task
file_writing_MA_NAccounts<-paste(BBDD_OECD_writing_directory, "FIELDS ANALYSIS/BBDD_OECD_MA_NAccounts-VARFIELDS.csv",sep="")
write.csv(colnames(BBDD_OECD_MA_NAccounts), file = file_writing_MA_NAccounts)

#View(MA_NAccounts.DataOrig)
#.....................................................................................................................................
# THIS STRUCTURE WAS USED INITIALLY TO CHOOSE POTENTIAL VARIABLES TO INCLUDE IN THE ANALYSIS
# WAS USED WITH  DATA UNTILL 2016 FOR FIRST VERSION OF PAPER 
#.....................................................................................................................................
# We choose variables we reckon may be used on priority 1
# MA_NAccount.VARS.SELECTED.PRIORITY1<-
#  c(
#    "OECD-Actual individual consumption per capita, current PPPs, OECD = 100 //*//-P41HCPIXOE",
#    "OECD-Actual individual consumption, at 2010 prices and PPPs, billions US dollars //*//-P41VPVOB",
#    "OECD-Actual individual consumption, percentage of GDP //*//-P41S",
#    "OECD-Consumption of fixed capital, percentage of GDP //*//-K1S",
#    "OECD-Exports of goods and services, percentage of GDP //*//-P6S",
#    "OECD-GDP at 2010 constant prices and PPPs, billions US dollars //*//-GDPVPVOB",
#    "OECD-GDP per capita, at constant 2010 prices and PPPs, US dollars //*//-GDPHVPVOB",
#    "OECD-GDP per capita, current PPPs, OECD = 100 //*//-GDPHCPIXOE",
#    "OECD-General government expenditure by function, environment protection, percentage of GDP //*//-TLYCG050GS13S",
#    "OECD-Gross fixed capital formation, Computer software, percentage of GFCF //*//-P51N1122SP51",
#    "OECD-Gross fixed capital formation, Transport equipment, percentage of total GFCF //*//-P51N11131SP51",
#    "OECD-Gross fixed capital formation, contribution to GDP growth //*//-P51CG",
#    "OECD-Gross household adjusted disposable income per capita, US dollars, current prices and current PPPs //*//-B7GS14_S15HCPC",
#    "OECD-Gross household disposable income per capita, US dollars, current prices and current PPPs //*//-B6GS14_S15HCPC",
#    "OECD-Gross value added, Agriculture, forestry and fishing , percentage of total activity //*//-B1GVASB1G",
#    "OECD-Gross value added, Construction , percentage of total activity //*//-B1GVFSB1G",
#    "OECD-Gross value added, Distributive trade, repairs, transport; accommodation and food service activities, annual growth rates in percentage //*//-B1GVG_IG",   
#    "OECD-Gross value added, Distributive trade, repairs, transport; accommodation and food service activities, Contribution to GVA growth //*//-B1GVG_ICG",         
#    "OECD-Gross value added, Distributive trade, repairs, transport; accommodation and food service activities, percentage of total activity //*//-B1GVG_ISB1G",  
#    "OECD-Gross value added, Financial and insurance activities, Contribution to GVA growth //*//-B1GVKCG",
#    "OECD-Gross value added, Industry, including energy , percentage of total activity //*//-B1GVB_ESB1G",
#    "OECD-Gross value added, Industry, percentage of total activity //*//-B1GVB_FSB1G",
#    "OECD-Gross value added, Information and communication, percentage of total activity //*//-B1GVJSB1G",
#    "OECD-Gross value added, Other services, percentage of total activity //*//-B1GVR_USB1G",
#    "OECD-Gross value added, Professional, scientific, technical, administration and support services activities, percentage of total activity //*//-B1GVM_NSB1G",
#    "OECD-Gross value added, Public administration, defence, education, human health and social work activities, percentage of total activity //*//-B1GVO_QSB1G",
#    "OECD-Gross value added, Real estate activities, percentage of total activity //*//-B1GVLSB1G",
#    "OECD-Gross value added, of which : Manufacturing , percentage of total activity //*//-B1GVCSB1G",
#    "OECD-Household final consumption expenditure, percentage of GDP //*//-P31S14_S15S",
#    "OECD-Imports of goods and services, percentage of GDP //*//-P7S",
#    "OECD-Individual consumption expenditure, general government, percentage of GDP //*//-P31S13S",
#    "OECD-Intermediate consumption, percentage of GDP //*//-P2S13S",
#    "OECD-Net household saving, percentage of households net disposable income //*//-B8NS14_S15SB6NS14",
#    "OECD-Of which: Gross fixed capital formation, Information and communication technology, percentage of total GFCF //*//-P51NICTSP51",
#    "OECD-Population, National concept, thousands //*//-POPNC",
#    "OECD-Purchasing power parities for GDP //*//-PPPGDP",
#    "OECD-Purchasing power parities for actual individual consumption //*//-PPPP41",
#    "OECD-Real Gross domestic product (GDP), volume, 2001=100 //*//-GDPVIXOB",
#    "OECD-Real net national income (NNI), year 2010 = 100 //*//-B5NVIXOB",
#    "OECD-Social benefits and social transfers in kind for products supplied to HH via market producers, paid by government, percentage of GDP //*//-D62_D631XXS13S",
#    "OECD-Social benefits and social transfers in kind, percentage of GDP //*//-D62_D63PS13S",
#    "OECD-Social benefits and social transfers, percentage of total expenditure of general government (GG) //*//-D62_D63PS13STE",
#   "OECD-Social benefits other than social transfers in kind, percentage of GDP //*//-D62PS13S",
#    "OECD-Social benefits other than social transfers in kind, percentage of total expenditure of GG //*//-D62PS13STE",
#    "OECD-Social contributions, percentage of GDP //*//-D61RS13S",
#    "OECD-Taxes on production and imports, percentage of GDP //*//-D2RS13S",
#   "OECD-Total expenditure of general government, percentage of GDP //*//-TES13S",
#    "OECD-Total general government (GG) revenue, percentage of GDP //*//-TRS13S",
#    "OECD-Total taxes, percentage of GDP //*//-D2D5D91RS13S",
#    "OECD-Volume index of GDP per capita, OECD = 100 in 2010, at 2010 price levels and PPPs //*//-GDPHVPIXOEOB"
#  )

# We choose variables we reckon may be used on priority 2
# MA_NAccount.VARS.SELECTED.PRIORITY2<-
#  c(
#    "OECD-Gross domestic product (GDP), current PPPs, billions US dollars //*//-GDPCPC",
#    "OECD-Gross domestic product (GDP), volume, annual growth rates, percentage //*//-GDPG",
#    "OECD-Gross fixed capital formation, percentage of GDP //*//-P51S",
#    "OECD-Gross fixed capital formation, percentage of GDP //*//-P51S13S",
#    "OECD-Land of households per capita, current PPPS, US dollars //*//-AN211NS14_S15HCPC",
#    "OECD-Net National Income (NNI) per capita, at current prices and PPPs, OECD=100 //*//-B5NHCPIXOE",
#    "OECD-Net capital stock, volume, year 2010 = 100 //*//-AN11NVIXOB",
#    "OECD-Real  household net adjusted disposable income, deflated by actual individual consumption, annual growth rates in percentage //*//-B7NS14_S15DEFG",
#    "OECD-Social contributions received by central government, percentage of GDP //*//-D61RS1311S",
#    "OECD-Social contributions received by local government, percentage of GDP //*//-D61RS1313S",
#    "OECD-Social contributions received by social security funds, percentage of GDP //*//-D61RS1314S",
#    "OECD-Social contributions received by state government, percentage of GDP //*//-D61RS1312S"
#  )


#BBDD_OECD_MA_NAccounts.VAR.SELECTED<-BBDD_OECD_MA_NAccounts[,c("Country","Year",
#                                                               MA_NAccount.VARS.SELECTED.PRIORITY1,
#                                                               MA_NAccount.VARS.SELECTED.PRIORITY2)]
#
#.....................................................................................................................................

colnames(BBDD_OECD_MA_NAccounts)

#We search for "GDP"
colnames(BBDD_OECD_MA_NAccounts)[grep("GDP", ignore.case=TRUE, colnames(BBDD_OECD_MA_NAccounts))]

#MA_NAccount.VARS.SELECTED.Simplified<-c(
#  "Country","Year",
#  "OECD~NA~NAG-Actual individual consumption, at 2010 prices and PPPs, billions US dollars //*//-P41VPVOB",
#  "OECD~NA~NAG-GDP at 2010 constant prices and PPPs, billions US dollars //*//-GDPVPVOB",
#  "OECD~NA~NAG-Actual individual consumption, percentage of GDP //*//-P41S",
#  "OECD~NA~NAG-GDP per capita, at constant 2010 prices and PPPs, US dollars //*//-GDPHVPVOB"
#)

BBDD_OECD_MA_NAccounts.VAR.SELECTED<-BBDD_OECD_MA_NAccounts[,c("Country","Year", MA_NAccount.VARS.SELECTED.Simplified )]

#BBDD_OECD_MA_NAccounts.VAR.SELECTED.First.Paper<-BBDD_OECD_MA_NAccounts.VAR.SELECTED
View(BBDD_OECD_MA_NAccounts)
BBDD_OECD_MA_NAccounts.VAR.SELECTED<-BBDD_OECD_MA_NAccounts



#********************************************************************************************************
# -------------------------------------------------------------------------------------------------------
# OCDE: OECD - MACROECONOMIC AGGREGATES (MA) - Disposable income and net lending
# -------------------------------------------------------------------------------------------------------
#********************************************************************************************************

OCEDfile_MA_DispIncome<-"/MACROECONOMIC AGGREGATES/CSV/OECD - Disposable income and net lending.csv"
file_MA_DispIncome<-paste(BBDD_OECD_reading_directory, OCEDfile_MA_DispIncome,sep="")
MA_DispIncome.Data<- read.csv(file_MA_DispIncome)
MA_DispIncome.Data.Orig <- MA_DispIncome.Data

#View(MA_DispIncome.Data)
tail(MA_DispIncome.Data)
colnames(MA_DispIncome.Data)

MA_DispIncome.VAR.CODE<-paste(MA_DispIncome.Data$Transaction, MA_DispIncome.Data$Measure,
                              MA_DispIncome.Data$Reference.Period, MA_DispIncome.Data$PowerCode, MA_DispIncome.Data$Unit,
                              sep=" ")
MA_DispIncome.Data<-(cbind(MA_DispIncome.Data, MA_DispIncome.VAR.CODE))
colnames(MA_DispIncome.Data)
MA_DispIncome.Data<-(subset(MA_DispIncome.Data, select=
                              -c(ï..LOCATION, TRANSACT, MEASURE, TIME,Unit.Code,PowerCode.Code,
                                  Reference.Period.Code,Flag.Codes, Flags) ))

MA_DispIncome.Data<-MA_DispIncome.Data[,c("Country","Year","MA_DispIncome.VAR.CODE","Value")]
# We rename columns name
colnames(MA_DispIncome.Data)<-c("Country","Year","Variable","Value")


# -----------------------------------------------------------------------------------------------------------
# ---- OCDE: CREACIÓN BBDD con Macroeconomic Aggregates
# -----------------------------------------------------------------------------------------------------------



# LEVELS_OECD_MA_DispIncome<-levels(MA_DispIncome.Data$Variable)

# We generate data files
# rm(BBDD_OECD_MA_DispIncome)
# LEVELS_OECD_MA_DispIncome<-levels(MA_DispIncome.Data$Variable)
# BBDD_OECD_MA_DispIncome<-MA_DispIncome.Data[which(MA_DispIncome.Data[,"Variable"]==LEVELS_OECD_MA_DispIncome[1]),]
# VAR_NAME<-paste("OECD",LEVELS_OECD_MA_DispIncome[1],sep="-")
# colnames(BBDD_OECD_MA_DispIncome)<-c("Country","Year","Variable",VAR_NAME)
# BBDD_OECD_MA_DispIncome<-subset(BBDD_OECD_MA_DispIncome, select = -c(Variable))
# View(BBDD_OECD_MA_DispIncome)
# for (i in 2:length(LEVELS_OECD_MA_DispIncome))
# {
#   A<-MA_DispIncome.Data[which(MA_DispIncome.Data[,"Variable"]==LEVELS_OECD_MA_DispIncome[i]),]
#   VAR_NAME<-paste("OECD",LEVELS_OECD_MA_DispIncome[i],sep="-")
#   colnames(A)<-c("Country","Year","Variable",VAR_NAME)
#   A<-subset(A, select = -c(Variable))
#   BBDD_OECD_MA_DispIncome<-merge(BBDD_OECD_MA_DispIncome, A,  by=c("Country","Year"), all=TRUE)
# }

#BBDD_OECD_MA_DispIncome.First.Paper<-BBDD_OECD_MA_DispIncome
BBDD_OECD_MA_DispIncome<-fGenBBDDPanel.1Dimension(MA_DispIncome.Data,BBDD_OECD_MA_DispIncome,"OECD")


# We write a text file with all fields for easing field selection task
file_writing_MA_DispIncome<-paste(BBDD_OECD_writing_directory, "FIELDS ANALYSIS/BBDD_OECD_MA_DispIncome-VARFIELDS.csv",sep="")
write.csv(colnames(BBDD_OECD_MA_DispIncome), file = file_writing_MA_DispIncome)


# RESEÑAS PARA ANALIZAR:

# OCEDfile_MA_DispIncome --> Constant prices, OECD base year      Reference Period 2010
# BBDD_EUROSTAT_GDP_and_COMPONENTS  --> "EUROSTAT-Gross domestic product at market prices-Chain linked volumes (2010), million euro"


#********************************************************************************************************
# -------------------------------------------------------------------------------------------------------
# OCDE: OECD - MACROECONOMIC AGGREGATES (MA) - GDP US constant prices, constant PPPs 
# -------------------------------------------------------------------------------------------------------
#********************************************************************************************************

# --------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------

OCDEfile_MA_GDP_PPP<-"/MACROECONOMIC AGGREGATES/CSV/OECD - GDP US constant prices, constant PPPs.csv"
file_MA_GDP_PPP<-paste(BBDD_OECD_reading_directory, OCDEfile_MA_GDP_PPP,sep="")
MA_GDP_PPP.Data<- read.csv(file_MA_GDP_PPP)
MA_GDP_PPP.Data.Orig <- MA_GDP_PPP.Data

#View(MA_GDP_PPP.Data)

MA_GDP_PPP.VAR.CODE<-paste(MA_GDP_PPP.Data$Transaction, MA_GDP_PPP.Data$Measure,
                           MA_GDP_PPP.Data$Reference.Period, MA_GDP_PPP.Data$PowerCode,
                           MA_GDP_PPP.Data$Unit,
                           sep=" ")
MA_GDP_PPP.Data<-(cbind(MA_GDP_PPP.Data, MA_GDP_PPP.VAR.CODE))
colnames(MA_GDP_PPP.Data)
MA_GDP_PPP.Data<-(subset(MA_GDP_PPP.Data, select=
                           -c(ï..LOCATION, TRANSACT, MEASURE, TIME,Unit.Code,PowerCode.Code,
                               Reference.Period.Code,Flag.Codes, Flags) ))

MA_GDP_PPP.Data<-MA_GDP_PPP.Data[,c("Country","Year","MA_GDP_PPP.VAR.CODE","Value")]
# We rename columns name
colnames(MA_GDP_PPP.Data)<-c("Country","Year","Variable","Value")


# -----------------------------------------------------------------------------------------------------------
# ---- OCDE: CREACIÓN BBDD con Macroeconomic Aggregates GDP Gross domestic product (expenditure approach)
#            Constant prices, PPP, US dollars

# -----------------------------------------------------------------------------------------------------------
#BBDD_OECD_MA_GDP_PPP.First.Paper<-BBDD_OECD_MA_GDP_PPP
BBDD_OECD_MA_GDP_PPP<-fGenBBDDPanel.1Dimension(MA_GDP_PPP.Data, BBDD_OECD_MA_GDP_PPP, "OCDE")

#BBDD_OECD_MA_GDP_PPP.Merged<-merge(BBDD_OECD_MA_GDP_PPP.First.Paper, BBDD_OECD_MA_GDP_PPP, by=c("Country","Year"), all=TRUE)

#View(BBDD_OECD_MA_GDP_PPP.Merged)




#********************************************************************************************************
#********************************************************************************************************
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# -------------------------------------------------------------------------------------------------------
#                                          OCDE: TRANSPORT 
# -------------------------------------------------------------------------------------------------------
# oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#********************************************************************************************************
#********************************************************************************************************





#********************************************************************************************************
# -------------------------------------------------------------------------------------------------------
# OCDE: PERFORMANCE Indicators (PI) DATABASE
# -------------------------------------------------------------------------------------------------------
#********************************************************************************************************

#PIdata<- read.csv("C:/Users/Alberto/Dropbox/DOCTORADO/1 - DATOS/1 - PAPER/2017/OCDE - Performance Variables.csv")

rm(PIdata)
#OCEDfile_PI<-"TRANSPORT/OCDE - Performance Indicators.csv"
OCEDfile_PI<-"TRANSPORT/OECD - Transport performance indicators.csv"

file_PI<-paste(BBDD_OECD_reading_directory, OCEDfile_PI,sep="")
PIdata<- read.csv(file_PI)
dim(PIdata)
names(PIdata)
#View(PIdata)

length(PIdata$Variable)
head(PIdata)
PIdata<-PIdata[,c("Country","Year","Indicator","Value")]
colnames(PIdata)<-c("Country","Year","Variable","Value")
#levels(PIdata$Variable)
LEVELS_PI<-levels(as.factor(PIdata$Variable))


# --------------------------------------------------------------------------------------------------------
# ------ OCDE: CREACIÓN BBDD con datos "road"
# --------------------------------------------------------------------------------------------------------
# grep con AND 
# http://stackoverflow.com/questions/13187414/r-grep-is-there-an-and-operator
#
#LEVELS_RAIL<-LEVELS[grepl("(?=.*Rail)(?=.*investment)", LEVELS, perl = TRUE)]
# http://stackoverflow.com/questions/18237852/grep-in-r-using-or-and-not
# grep with NOT
# http://stackoverflow.com/questions/18237852/grep-in-r-using-or-and-not

LEVELS_PI_ROAD<-LEVELS_PI[grep("road",ignore.case=TRUE,LEVELS_PI)]
Number_items_road<-length(LEVELS_PI_ROAD)
rm(BBDD_OECD_Road_PI)

BBDD_OECD_Road_PI<-PIdata[which(PIdata[,"Variable"]==LEVELS_PI_ROAD[1]),]
VAR_NAME<-paste("OECD~TM~PI",LEVELS_PI_ROAD[1],sep="-")
colnames(BBDD_OECD_Road_PI)<-c("Country","Year","Variable",VAR_NAME)
BBDD_OECD_Road_PI<-subset(BBDD_OECD_Road_PI, select = -c(Variable))

for (i in 2:Number_items_road)
{
  A<-PIdata[which(PIdata[,"Variable"]==LEVELS_PI_ROAD[i]),]
  VAR_NAME<-paste("OECD~TM~PI",LEVELS_PI_ROAD[i],sep="-")
  colnames(A)<-c("Country","Year","Variable", VAR_NAME)
  A<-subset(A, select = -c(Variable))
  BBDD_OECD_Road_PI<-merge(BBDD_OECD_Road_PI, A,  by=c("Country","Year"), all=TRUE)
}

View(BBDD_OECD_Road_PI)
colnames(BBDD_OECD_Road_PI)
#BBDD_OECD_Road_PI.First.Paper<-BBDD_OECD_Road_PI


# --------------------------------------------------------------------------------------------------------
# ------ OCDE: CREACIÓN BBDD con datos "rail"
# --------------------------------------------------------------------------------------------------------

LEVELS_PI_RAIL<-LEVELS_PI[grep("rail", ignore.case=TRUE, LEVELS_PI)]
Number_items_rail<-length(LEVELS_PI_RAIL)

rm(BBDD_OECD_Rail)
BBDD_OECD_Rail<-PIdata[which(PIdata[,"Variable"]==LEVELS_PI_RAIL[1]),]
VAR_NAME<-paste("OECD~TM~PI",LEVELS_PI_RAIL[1],sep="-")
colnames(BBDD_OECD_Rail)<-c("Country","Year","Variable",VAR_NAME)
BBDD_OECD_Rail<-subset(BBDD_OECD_Rail, select = -c(Variable))

for (i in 2:Number_items_rail)
{
  A<-PIdata[which(PIdata[,"Variable"]==LEVELS_PI_RAIL[i]),]
  VAR_NAME<-paste("OECD~TM~PI",LEVELS_PI_RAIL[i],sep="-")
  colnames(A)<-c("Country","Year","Variable",VAR_NAME)
  A<-subset(A, select = -c(Variable))
  BBDD_OECD_Rail<-merge(BBDD_OECD_Rail, A,  by=c("Country","Year"), all=TRUE)
}

#BBDD_OECD_Rail.First.Paper<-BBDD_OECD_Rail
View(BBDD_OECD_Rail)
colnames(BBDD_OECD_Rail)


# --------------------------------------------------------------------------------------------------------
# ------ OCDE: CREACIÓN BBDD con datos "Vehicles"
# --------------------------------------------------------------------------------------------------------

#grep(paste(Search_pattern,collapse="|"),  ignore.case=TRUE, LEVELS)
#intersect(grep(paste(Search_pattern,collapse="|"),  ignore.case=TRUE, LEVELS),
#          grep("road|Road",invert=TRUE, ignore.case=TRUE, LEVELS))

# We seach for vehicle data not included in road
Search_pattern<-c("car","vehicle","motorcycle")
LEVELS_PI_VEHICLES<-LEVELS_PI[intersect(grep(paste(Search_pattern,collapse="|"),  ignore.case=TRUE, LEVELS_PI),
                                        grep("road|Road",invert=TRUE, ignore.case=TRUE, LEVELS_PI))]
Number_items_vehicles<-length(LEVELS_PI_VEHICLES)

rm(BBDD_OECD_Vehicles)
BBDD_OECD_Vehicles<-PIdata[which(PIdata[,"Variable"]==LEVELS_PI_VEHICLES[1]),]
VAR_NAME<-paste("OECD~TM~PI",LEVELS_PI_VEHICLES[1],sep="-")
colnames(BBDD_OECD_Vehicles)<-c("Country","Year","Variable",VAR_NAME)
BBDD_OECD_Vehicles<-subset(BBDD_OECD_Vehicles, select = -c(Variable))

for (i in 2:Number_items_vehicles)
{
  A<-PIdata[which(PIdata[,"Variable"]==LEVELS_PI_VEHICLES[i]),]
  VAR_NAME<-paste("OECD~TM~PI",LEVELS_PI_VEHICLES[i],sep="-")
  colnames(A)<-c("Country","Year","Variable", VAR_NAME)
  A<-subset(A, select = -c(Variable))
  BBDD_OECD_Vehicles<-merge(BBDD_OECD_Vehicles, A,  by=c("Country","Year"), all=TRUE)
}

BBDD_OECD_Vehicles[,"Country"]<-as.factor(BBDD_OECD_Vehicles[,"Country"])

View(BBDD_OECD_Vehicles)
#BBDD_OECD_Vehicles.First.Paper<-BBDD_OECD_Vehicles







#********************************************************************************************************
# -------------------------------------------------------------------------------------------------------
# OCDE: SHORT TERM Indicators (STI) DATABASE
# -------------------------------------------------------------------------------------------------------
#********************************************************************************************************
rm(STIdata)
OCEDfile_STI<-"TRANSPORT/OECD - Short-term transport indicators.csv"
file_STI<-paste(BBDD_OECD_reading_directory, OCEDfile_STI,sep="")
STIdata<- read.csv(file_STI)

dim(STIdata)
length(STIdata$Variable)
head(STIdata)
STIdata<-STIdata[,c("Country","Time","Variable","Unit","Value")]
colnames(STIdata)<-c("Country","Year","Variable","Unit","Value")
STIdata[,"Variable"]<-as.factor((paste(STIdata[,"Variable"],STIdata[,"Unit"],sep=" /-/ ")))
STIdata<-subset(STIdata, select = -c(Unit))

#levels(PIdata$Variable)
LEVELS_STI<-levels(STIdata$Variable)
LEVELS_STI
# We remove data which is not expressed yearly
######!BBDD_OECD_STI_Traffic[,"Year"] %in% Years_Range
Years_Range<-seq(1994,2020,by=1)
STIdata <- STIdata[STIdata[,"Year"] %in% Years_Range,]
View(STIdata)



# --------------------------------------------------------------------------------------------------------
# ------ OCDE: CREACIÓN BBDD OCED STI con datos traffic | transport
# --------------------------------------------------------------------------------------------------------


LEVELS_STI_TRAFFIC<-LEVELS_STI[grep("transport|traffic", ignore.case=TRUE, LEVELS_STI)]
Number_items_traffic<-length(LEVELS_STI_TRAFFIC)

rm(BBDD_OECD_STI_Traffic)
BBDD_OECD_STI_Traffic<-STIdata[which(STIdata[,"Variable"]==LEVELS_STI_TRAFFIC[1]),]
VAR_NAME<-paste("OECD~TM~STI",LEVELS_STI_TRAFFIC[1],sep="-")
colnames(BBDD_OECD_STI_Traffic)<-c("Country","Year","Variable",VAR_NAME)
BBDD_OECD_STI_Traffic<-subset(BBDD_OECD_STI_Traffic, select = -c(Variable))

for (i in 2:Number_items_traffic)
{
  A<-STIdata[which(STIdata[,"Variable"]==LEVELS_STI_TRAFFIC[i]),]
  VAR_NAME<-paste("OECD~TM~STI",LEVELS_STI_TRAFFIC[i],sep="-")
  colnames(A)<-c("Country","Year","Variable",VAR_NAME)
  A<-subset(A, select = -c(Variable))
  BBDD_OECD_STI_Traffic<-merge(BBDD_OECD_STI_Traffic, A,  by=c("Country","Year"), all=TRUE)
}

names(BBDD_OECD_STI_Traffic)
View(BBDD_OECD_STI_Traffic)
#BBDD_OECD_STI_Traffic.First.Paper<-BBDD_OECD_STI_Traffic



# --------------------------------------------------------------------------------------------------------
# ------ OCDE: CREACIÓN BBDD con datos "vehicle registrations"
# --------------------------------------------------------------------------------------------------------

LEVELS_STI_REGISTRATIONS<-LEVELS_STI[grep("registrations", ignore.case=TRUE, LEVELS_STI)]
Number_items_registrations<-length(LEVELS_STI_REGISTRATIONS)

rm(BBDD_OECD_STI_Registrations)
BBDD_OECD_STI_Registrations<-PIdata[which(STIdata[,"Variable"]==LEVELS_STI_REGISTRATIONS[1]),]
VAR_NAME<-paste("OECD~TM~STI",LEVELS_STI_REGISTRATIONS[1],sep="-")
colnames(BBDD_OECD_STI_Registrations)<-c("Country","Year","Variable",VAR_NAME)
BBDD_OECD_STI_Registrations<-subset(BBDD_OECD_STI_Registrations, select = -c(Variable))

for (i in 2:Number_items_registrations)
{
  A<-STIdata[which(STIdata[,"Variable"]==LEVELS_STI_REGISTRATIONS[i]),]
  VAR_NAME<-paste("OECD~TM~STI",LEVELS_STI_REGISTRATIONS[i],sep="-")
  colnames(A)<-c("Country","Year","Variable", VAR_NAME)
  A<-subset(A, select = -c(Variable))
  BBDD_OECD_STI_Registrations<-merge(BBDD_OECD_STI_Registrations, A,  by=c("Country","Year"), all=TRUE)
}

#View(BBDD_OECD_STI_Registrations)

#BBDD_OECD_STI_Registrations.First.Paper<-BBDD_OECD_STI_Registrations




#********************************************************************************************************
# -------------------------------------------------------------------------------------------------------
# OCDE: Transport measurement - Goods transport(GT) DATABASE
# -------------------------------------------------------------------------------------------------------
#********************************************************************************************************
rm(GTdata)
OCEDfile_GT<-"TRANSPORT/OECD - Goods transport.csv"
file_GT<-paste(BBDD_OECD_reading_directory, OCEDfile_GT,sep="")
GTdata<- read.csv(file_GT)


dim(GTdata)
length(GTdata$Variable)
head(GTdata)
names(GTdata)
GTdata<-GTdata[,c("Country","Year","Variable","Unit","Value")]
GTdata[,"Variable"]<-as.factor((paste(GTdata[,"Variable"],GTdata[,"Unit"],sep=" /-/ ")))
GTdata<-subset(GTdata, select = -c(Unit))
#View(GTdata)

LEVELS_GT<-levels(GTdata$Variable)

# --------------------------------------------------------------------------------------------------------
# ------ OCDE: CREACIÓN BBDD con datos freight transport, road, rail, pipelines & maritime
# --------------------------------------------------------------------------------------------------------

# OECD~TM~FM stands for OECD~Transport Measurement database.
# Section Freight TRansport

Number_items_goods_transport<-length(LEVELS_GT)
rm(BBDD_OECD_Goods_Transport)
BBDD_OECD_Goods_Transport<-GTdata[which(GTdata[,"Variable"]==LEVELS_GT[1]),]
VAR_NAME<-paste("OECD~TM~FT",LEVELS_GT[1],sep="-")
colnames(BBDD_OECD_Goods_Transport)<-c("Country","Year","Variable",VAR_NAME)
BBDD_OECD_Goods_Transport<-subset(BBDD_OECD_Goods_Transport, select = -c(Variable))

for (i in 2:Number_items_goods_transport)
{
  A<-GTdata[which(GTdata[,"Variable"]==LEVELS_GT[i]),]
  VAR_NAME<-paste("OECD~TM~FT",LEVELS_GT[i],sep="-")
  colnames(A)<-c("Country","Year","Variable",VAR_NAME)
  A<-subset(A, select = -c(Variable))
  BBDD_OECD_Goods_Transport<-merge(BBDD_OECD_Goods_Transport, A,  by=c("Country","Year"), all=TRUE)
}

colnames(BBDD_OECD_Goods_Transport)
View(BBDD_OECD_Goods_Transport)
#BBDD_OECD_Goods_Transport.First.Paper<-BBDD_OECD_Goods_Transport



#********************************************************************************************************
# -------------------------------------------------------------------------------------------------------
# OCDE: Transport measurement - Passenger transport(PT) DATABASE
# -------------------------------------------------------------------------------------------------------
#********************************************************************************************************


# --------------------------------------------------------------------------------------------------------
# ------ OCDE: CREACIÓN BBDD con datos passenger Km 
# --------------------------------------------------------------------------------------------------------
# OECD~TM~FM stands for OECD~Transport Measurement database.
# Section Passenger TRansport

#OCEDfile_PT<-"TRANSPORT/OCDE - Passenger transport.csv"
OCEDfile_PT<-"TRANSPORT/OECD - Passenger transport.csv"
rm(PTdata)
file_PT<-paste(BBDD_OECD_reading_directory, OCEDfile_PT,sep="")
PTdata<- read.csv(file_PT)

dim(PTdata)
length(PTdata$Variable)
names(PTdata)
head(PTdata)
PTdata<-PTdata[,c("Country","Year","Variable","Unit","Value")]
PTdata[,"Variable"]<-as.factor((paste(PTdata[,"Variable"],PTdata[,"Unit"],sep=" /-/ ")))
PTdata<-subset(PTdata, select = -c(Unit))
#levels(PIdata$Variable)
LEVELS_PT<-levels(PTdata$Variable)



rm(BBDD_OECD_Passenger_Transport)
BBDD_OECD_Passenger_Transport<-PTdata[which(PTdata[,"Variable"]==LEVELS_PT[1]),]
VAR_NAME<-paste("OECD~TM~PT",LEVELS_PT[1],sep="-")
colnames(BBDD_OECD_Passenger_Transport)<-c("Country","Year","Variable",VAR_NAME)
BBDD_OECD_Passenger_Transport<-subset(BBDD_OECD_Passenger_Transport, select = -c(Variable))

for (i in 2:length(LEVELS_PT))
{
  A<-PTdata[which(PTdata[,"Variable"]==LEVELS_PT[i]),]
  VAR_NAME<-paste("OECD~TM~PT",LEVELS_PT[i],sep="-")
  colnames(A)<-c("Country","Year","Variable",VAR_NAME)
  A<-subset(A, select = -c(Variable))
  BBDD_OECD_Passenger_Transport<-merge(BBDD_OECD_Passenger_Transport, A,  by=c("Country","Year"), all=TRUE)
}

View(BBDD_OECD_Passenger_Transport)

#BBDD_OECD_Passenger_Transport.2019<-BBDD_OECD_Passenger_Transport

#BBDD_OECD_Passenger_Transport.First.Paper<-BBDD_OECD_Passenger_Transport
colnames(BBDD_OECD_Passenger_Transport)
cor.BBDD_OECD_Passenger_Transport<-na.omit(BBDD_OECD_Passenger_Transport)
cor(cor.BBDD_OECD_Passenger_Transport[6],cor.BBDD_OECD_Passenger_Transport[5],method = c("pearson"))


Temp.Road_PI<-BBDD_OECD_Road_PI[,c("Country", "Year","OECD-Road traffic in thousand vehicle-km per road motor vehicle")]
Temp.Passenger_Transport<-BBDD_OECD_Passenger_Transport[,c("Country","Year","OECD-Road passenger transport")]

Temp.Passenger_Transport.Merged<-merge(Temp.Road_PI,Temp.Passenger_Transport,  by=c("Country","Year"), all=TRUE)
Temp.Passenger_Transport.Merged<-na.omit(Temp.Passenger_Transport.Merged)

cor(Temp.Passenger_Transport.Merged[3],Temp.Passenger_Transport.Merged[4],method = c("pearson"))

#View(Temp.Passenger_Transport.Merged)

#********************************************************************************************************
# -------------------------------------------------------------------------------------------------------
# OCDE - Transport Infrastructure (TI) DATABASE
# -------------------------------------------------------------------------------------------------------
#********************************************************************************************************

OCEDfile_TI<-"TRANSPORT/OCDE - Transport Infrastructure.csv"
file_TI<-paste(BBDD_OECD_reading_directory, OCEDfile_TI,sep="")
TIdata<- read.csv(file_TI)

dim(TIdata)
length(TIdata$Variable)
head(TIdata)

#TIdata<-TIdata[,c("Country","Year","Variable","UNIT","Value")]
TIdata<-TIdata[,c("Country","Year","Variable","Measure","Value")]
colnames(TIdata)<-c("Country","Year","Variable","UNIT","Value")
#levels(PIdata$Variable)
LEVELS_TI<-levels(TIdata$Variable)
LEVELS_TI_UNIT<-levels(TIdata$UNIT)



# -----------------------------------------------------------------------------------------------------------
# ---- OCDE: CREACIÓN BBDD con datos infrastructure maintainance & investment spending depending on CURRENCY
# -----------------------------------------------------------------------------------------------------------
rm(BBDD_OECD_Transport_Infrastructure)

BBDD_OECD_Transport_Infrastructure<-TIdata[which(TIdata[,"Variable"]==LEVELS_TI[1] & 
                                                   TIdata[,"UNIT"]==LEVELS_TI_UNIT[1]),]
colnames(BBDD_OECD_Transport_Infrastructure)<-
  c("Country","Year","Variable", "UNIT", paste("OECD",LEVELS_TI[1],LEVELS_TI_UNIT[1],sep="-"))

BBDD_OECD_Transport_Infrastructure<-subset(BBDD_OECD_Transport_Infrastructure, select = -c(Variable,UNIT))

for (j in 1:length(LEVELS_TI_UNIT))
{
  #First_item <- (j-1)*length(LEVELS_TI)+1
  
  for (i in 1:length(LEVELS_TI))
  {
    if ( (j*i)==1) next
    A<-TIdata[which(TIdata[,"Variable"]==LEVELS_TI[i] & TIdata[,"UNIT"]==LEVELS_TI_UNIT[j]),]
    colnames(A)<-c("Country","Year","Variable","UNIT", paste("OECD",LEVELS_TI[i],LEVELS_TI_UNIT[j],sep="-"))
    #print(paste(LEVELS_TI[i],LEVELS_TI_UNIT[j],sep="-"))
    A<-subset(A, select = -c(Variable,UNIT))
    BBDD_OECD_Transport_Infrastructure<-merge(BBDD_OECD_Transport_Infrastructure, A,  by=c("Country","Year"), all=TRUE)
  }
}

#BBDD_OECD_Transport_Infrastructure.First.Paper<-BBDD_OECD_Transport_Infrastructure
View(BBDD_OECD_Transport_Infrastructure)
names(BBDD_OECD_Transport_Infrastructure)


#********************************************************************************************************
# -------------------------------------------------------------------------------------------------------
# OCDE: OECD - Transport Safety (TS) DATABASE
# -------------------------------------------------------------------------------------------------------
#********************************************************************************************************

OCEDfile_TS<-"TRANSPORT/OECD - Transport safety.csv"
file_TS<-paste(BBDD_OECD_reading_directory, OCEDfile_TS,sep="")
TSdata<- read.csv(file_TS)

dim(TSdata)
length(TSdata$Variable)
head(TSdata)
TSdata<-TSdata[,c("Country","Year","Variable","Value")]
colnames(TSdata)<-c("Country","Year","Variable","Value")
#levels(PIdata$Variable)


# --------------------------------------------------------------------------------------------------------
# ------ OCDE: CREACIÓN BBDD con Transport Safety 
# --------------------------------------------------------------------------------------------------------

rm(BBDD_OECD_Transport_Safety)
LEVELS_EUROSTAT_PassCars<-levels(TSdata$Variable)
BBDD_OECD_Transport_Safety<-TSdata[which(TSdata[,"Variable"]==LEVELS_EUROSTAT_PassCars[1]),]
VAR_NAME<-paste("OECD",LEVELS_EUROSTAT_PassCars[1],sep="-")
colnames(BBDD_OECD_Transport_Safety)<-c("Country","Year","Variable",VAR_NAME)
BBDD_OECD_Transport_Safety<-subset(BBDD_OECD_Transport_Safety, select = -c(Variable))

for (i in 2:length(LEVELS_EUROSTAT_PassCars))
{
  A<-TSdata[which(TSdata[,"Variable"]==LEVELS_EUROSTAT_PassCars[i]),]
  VAR_NAME<-paste("OECD",LEVELS_EUROSTAT_PassCars[i],sep="-")
  colnames(A)<-c("Country","Year","Variable",VAR_NAME)
  A<-subset(A, select = -c(Variable))
  BBDD_OECD_Transport_Safety<-merge(BBDD_OECD_Transport_Safety, A,  by=c("Country","Year"), all=TRUE)
}

#View(BBDD_OECD_Transport_Safety)
#BBDD_OECD_Transport_Safety.First.Paper<-BBDD_OECD_Transport_Safety

#********************************************************************************************************
# -------------------------------------------------------------------------------------------------------
# OCDE: OECD - Transport Safety (TS) DATABASE
# -------------------------------------------------------------------------------------------------------
#********************************************************************************************************

OCEDfile_IRTAD<-"TRANSPORT/OECD - IRTAD.csv"
file_IRTAD<-paste(BBDD_OECD_reading_directory, OCEDfile_IRTAD,sep="")
IRTADdata<- read.csv(file_IRTAD)

dim(IRTADdata)
length(IRTADdata$Variable)
head(IRTADdata)
levels(IRTADdata$Age.group)

rm(BBDD_OECD_Transport_Safety)
LEVELS_EUROSTAT_PassCars<-levels(TSdata$Variable)





#********************************************************************************************************
#********************************************************************************************************
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# -------------------------------------------------------------------------------------------------------
#                                         OECD ENERGY 
# -------------------------------------------------------------------------------------------------------
# oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#********************************************************************************************************
#********************************************************************************************************


#********************************************************************************************************
# -------------------------------------------------------------------------------------------------------
#  OECD - ENERGY DEMAND
# -------------------------------------------------------------------------------------------------------
#********************************************************************************************************



rm(OIL_OECD_ROADdata)
OECD_CSVfile_fuel_road<-"ENERGY/OCDE - Fuel - Road.csv"

file_OECD_road<-paste(BBDD_OECD_reading_directory, OECD_CSVfile_fuel_road,sep="")
OIL_OECD_ROADdata<- read.csv(file_OECD_road, header=TRUE, sep=",", dec=".")

# We check it out if data has been correctly downloaded
head(OIL_OECD_ROADdata)
#View(OIL_OECD_ROADdata)
# We get data which flow is "Road"
OIL_OECD_ROADdata<-OIL_OECD_ROADdata[which(OIL_OECD_ROADdata[,"Flow"]=="Road"),]
# We remove Field "Road" once that we assure we only have Road data
OIL_OECD_ROADdata<-OIL_OECD_ROADdata[,c("Country","Time","Product","Value")]
names(OIL_OECD_ROADdata)<-c("Country","Time","Variable","Value")
# If dim is 1000000 that means data has been trunkated
dim(OIL_OECD_ROADdata)
head(OIL_OECD_ROADdata)

glimpse(OIL_OECD_ROADdata)
str(OIL_OECD_ROADdata)


OIL_OECD_ROADdata[,"Variable"]<-as.factor(OIL_OECD_ROADdata[,"Variable"])
OIL_OECD_ROADdata[,"Country"]<-as.factor(OIL_OECD_ROADdata[,"Country"])
#OIL_OECD_ROADdata[,"Time"]<-as.factor(OIL_OECD_ROADdata[,"Time"])


# We check it out existing levels
levels(OIL_OECD_ROADdata$Variable)
LEVELS_OECD_Road<-levels(OIL_OECD_ROADdata$Variable)
levels(OIL_OECD_ROADdata$Country)
levels(OIL_OECD_ROADdata$Time)
range(OIL_OECD_ROADdata$Time)
colnames(OIL_OECD_ROADdata)


# We check it out which possible Gasoline or Diesel elements are in vector Product
# LEVELS_OECD_OIL_Road<-LEVELS_OECD_Road[grep("diesel|gasoline", ignore.case=TRUE, LEVELS_OECD_Road)]

OIL_Road_Fuels<-c("Motor gasoline excl. biofuels (kt)","Biogasoline (kt)",
                  "Gas/diesel oil excl. biofuels (kt)", "Biodiesels (kt)",
                  "Liquefied petroleum gases (LPG) (kt)", "Other liquid biofuels (kt)")

OIL_OECD_ROADdata<-OIL_OECD_ROADdata[which(OIL_OECD_ROADdata$Variable %in% OIL_Road_Fuels),]
# We remove levels which are not any longer in use
LEVELS_OECD_OIL_Road<-levels(droplevels(OIL_OECD_ROADdata$Variable))

rm(BBDD_OECD_OIL_Road)
BBDD_OECD_OIL_Road<-OIL_OECD_ROADdata[which(OIL_OECD_ROADdata[,"Variable"]==LEVELS_OECD_OIL_Road[1]),]
VAR_NAME<-paste("OECD",LEVELS_OECD_OIL_Road[1],sep="-")
colnames(BBDD_OECD_OIL_Road)<-c("Country","Year","Variable",VAR_NAME)
BBDD_OECD_OIL_Road<-subset(BBDD_OECD_OIL_Road, select = -c(Variable))

for (i in 2:length(LEVELS_OECD_OIL_Road))
{
  A<-OIL_OECD_ROADdata[which(OIL_OECD_ROADdata[,"Variable"]==LEVELS_OECD_OIL_Road[i]),]
  VAR_NAME<-paste("OECD",LEVELS_OECD_OIL_Road[i],sep="-")
  colnames(A)<-c("Country","Year","Variable",VAR_NAME)
  A<-subset(A, select = -c(Variable))
  BBDD_OECD_OIL_Road<-merge(BBDD_OECD_OIL_Road, A,  by=c("Country","Year"), all=TRUE)
}

View(BBDD_OECD_OIL_Road[BBDD_OECD_OIL_Road[,"Country"]=="Denmark",])
View(BBDD_OECD_OIL_Road[BBDD_OECD_OIL_Road[,"Country"]=="Spain",])
#BBDD_OECD_OIL_Road.First.Paper<-BBDD_OECD_OIL_Road
View(BBDD_OECD_OIL_Road.First.Paper[BBDD_OECD_OIL_Road.First.Paper[,"Country"]=="Spain",])



#BBDD_OECD_OIL_Road.2021
#BBDD_OECD_OIL_Road.2019
#Compara<-merge(BBDD_OECD_OIL_Road.2021, BBDD_OECD_OIL_Road.2019,  by=c("Country","Year"), all=TRUE)



#********************************************************************************************************
# -------------------------------------------------------------------------------------------------------
# -------------  OECD: ENERGY PRICES IN US Dollars read from a single FILE -------------------------------------------------------
# -------------  DEVELOPED ON UPDATING DATA FOR NEW DATABASE UNTIL 2016. Prices data are till 2020
# -------------------------------------------------------------------------------------------------------
#********************************************************************************************************

rm(OIL.OECD.PRICES.data.1978.2020)
#OECD.CSVfile.fuel.road<-"ENERGY/IEA - International Energy Agency/PRICES/OECD - Energy prices in US dollars 1978 - 2017.csv"
#OECD.CSVfile.fuel.road<-"ENERGY/IEA - International Energy Agency/PRICES/OECD - Energy prices in US dollars 1978 - 2018.csv"
OECD.CSVfile.fuel.road<-"ENERGY/IEA - International Energy Agency/PRICES/OECD - Energy prices in US dollars 1978 - 2020.csv"

file.OECD.PRICES<-paste(BBDD_OECD_reading_directory, OECD.CSVfile.fuel.road,sep="")
OIL.OECD.PRICES.data.1978.2020<- read.csv(file.OECD.PRICES, header=TRUE, sep=",", dec=".")

# We remove Fields we don't use
OIL.OECD.PRICES.data.1978.2020<-OIL.OECD.PRICES.data.1978.2020[,c("Country","Time","Product","Sector","Flow","Value")]
dim(OIL.OECD.PRICES.data.1978.2020)

# We change field names
colnames(OIL.OECD.PRICES.data.1978.2020)<-c("Country", "Year","Product","Sector","Flow","Value")

# We remove possible duplicated data
OIL.OECD.PRICES.data.1978.2020<-OIL.OECD.PRICES.data.1978.2020[!duplicated(OIL.OECD.PRICES.data.1978.2020),]
dim(OIL.OECD.PRICES.data.1978.2020)

# We remove data of Yearly quarters
# OIL.OECD.PRICES.data2<-OIL.OECD.PRICES.data[which(!startsWith(OIL.OECD.PRICES.data$Year, "Q", trim=TRUE, ignore.case=TRUE)),]
OIL.OECD.PRICES.data.1978.2020<-OIL.OECD.PRICES.data.1978.2020[which(!grepl("^Q",OIL.OECD.PRICES.data.1978.2020$Year)),]
dim(OIL.OECD.PRICES.data.1978.2020)

# We sort out data
OIL.OECD.PRICES.data.1978.2020<-OIL.OECD.PRICES.data.1978.2020[
  with(OIL.OECD.PRICES.data.1978.2020, order(Country,Flow,Sector,Product,Year)),]

# We remove data containing NA values
OIL.OECD.PRICES.data.1978.2020<-OIL.OECD.PRICES.data.1978.2020[!is.na(OIL.OECD.PRICES.data.1978.2020$Value),]


colnames(OIL.OECD.PRICES.data.1978.2020)
Variable<-paste(OIL.OECD.PRICES.data.1978.2020$Product, OIL.OECD.PRICES.data.1978.2020$Sector,
                OIL.OECD.PRICES.data.1978.2020$Flow,sep="-")

OIL.OECD.PRICES.data.1978.2020<-cbind(OIL.OECD.PRICES.data.1978.2020,Variable)
OIL.OECD.PRICES.data.1978.2020<-subset(OIL.OECD.PRICES.data.1978.2020, select = -c(Product, Sector, Flow))
OIL.OECD.PRICES.data.1978.2020<-OIL.OECD.PRICES.data.1978.2020[,c("Country","Year","Variable","Value")]

#View(OIL.OECD.PRICES.data.1978.2020)
dim(OIL.OECD.PRICES.data.1978.2020)
str(OIL.OECD.PRICES.data.1978.2020)
glimpse(OIL.OECD.PRICES.data.1978.2020)

colnames(OIL.OECD.PRICES.data.1978.2020)
levels(OIL.OECD.PRICES.data.1978.2020[,3])

OIL.OECD.PRICES.data.1978.2020[,"Country"]<-as.factor(OIL.OECD.PRICES.data.1978.2020[,"Country"])
OIL.OECD.PRICES.data.1978.2020[,"Variable"]<-as.factor(OIL.OECD.PRICES.data.1978.2020[,"Variable"])
OIL.OECD.PRICES.data.1978.2020[,"Year"]<-as.integer(OIL.OECD.PRICES.data.1978.2020[,"Year"])
View(OIL.OECD.PRICES.data.1978.2020)



#********************************************************************************************************
# ------------- OCDE: ENERGY PRICES IN US Dollars -------------------------------------------------------
# ------------- ORIGINALLY USED. Prices data are till 2016
#********************************************************************************************************

OECD.CSVfile.fuel.road<-"ENERGY/IEA - International Energy Agency/PRICES/OECD - Energy prices in US dollars 1978 - 1990.csv"
file.OECD.PRICES<-paste(BBDD_OECD_reading_directory, OECD.CSVfile.fuel.road,sep="")
OIL.OECD.PRICES.data.1978<- read.csv(file.OECD.PRICES, header=TRUE, sep=",", dec=".")

OECD.CSVfile.fuel.road<-"ENERGY/IEA - International Energy Agency/PRICES/OECD - Energy prices in US dollars 1991 - 2005.csv"
file.OECD.PRICES<-paste(BBDD_OECD_reading_directory, OECD.CSVfile.fuel.road,sep="")
OIL.OECD.PRICES.data.1991<- read.csv(file.OECD.PRICES, header=TRUE, sep=",", dec=".")

OECD.CSVfile.fuel.road<-"ENERGY/IEA - International Energy Agency/PRICES/OECD - Energy prices in US dollars 2006 - 2017.csv"
file.OECD.PRICES<-paste(BBDD_OECD_reading_directory, OECD.CSVfile.fuel.road,sep="")
OIL.OECD.PRICES.data.2006<- read.csv(file.OECD.PRICES, header=TRUE, sep=",", dec=".")


# We remove Field "PRICES" once that we assure we only have PRICES data
OIL.OECD.PRICES.data.1978<-OIL.OECD.PRICES.data.1978[,c("Country","Time","Product","Sector","Flow","Value")]
OIL.OECD.PRICES.data.1991<-OIL.OECD.PRICES.data.1991[,c("Country","Time","Product","Sector","Flow","Value")]
OIL.OECD.PRICES.data.2006<-OIL.OECD.PRICES.data.2006[,c("Country","Time","Product","Sector","Flow","Value")]

colnames(OIL.OECD.PRICES.data.1978)<-c("Country", "Year","Product","Sector","Flow","Value")
colnames(OIL.OECD.PRICES.data.1991)<-c("Country", "Year","Product","Sector","Flow","Value")
colnames(OIL.OECD.PRICES.data.2006)<-c("Country", "Year","Product","Sector","Flow","Value")

levels(droplevels(OIL.OECD.PRICES.data.2006$Product))
levels(droplevels(OIL.OECD.PRICES.data.1978$Sector))
levels(droplevels(OIL.OECD.PRICES.data.1978$Flow))

Sector<-c("Households","Industry")
Product<-c("Automotive diesel (litre)","Premium leaded gasoline (litre)", 
           "Premium unleaded 95 RON (litre)", 
           "Premium unleaded 98 RON (litre)",  
           "Regular leaded gasoline (litre)",  
           "Regular unleaded gasoline (litre)",
           "Liquefied petroleum gas (litre)")

dim(OIL.OECD.PRICES.data.1978)
OIL.OECD.PRICES.data.1978<-OIL.OECD.PRICES.data.1978[which(OIL.OECD.PRICES.data.1978$Product %in% Product),]
dim(OIL.OECD.PRICES.data.1978)
OIL.OECD.PRICES.data.1978<-OIL.OECD.PRICES.data.1978[which(OIL.OECD.PRICES.data.1978$Sector %in% Sector),]
dim(OIL.OECD.PRICES.data.1978)

dim(OIL.OECD.PRICES.data.1991)
OIL.OECD.PRICES.data.1991<-OIL.OECD.PRICES.data.1991[which(OIL.OECD.PRICES.data.1991$Product %in% Product),]
dim(OIL.OECD.PRICES.data.1991)
OIL.OECD.PRICES.data.1991<-OIL.OECD.PRICES.data.1991[which(OIL.OECD.PRICES.data.1991$Sector %in% Sector),]
dim(OIL.OECD.PRICES.data.1991)

dim(OIL.OECD.PRICES.data.2006)
OIL.OECD.PRICES.data.2006<-OIL.OECD.PRICES.data.2006[which(OIL.OECD.PRICES.data.2006$Product %in% Product),]
dim(OIL.OECD.PRICES.data.2006)
OIL.OECD.PRICES.data.2006<-OIL.OECD.PRICES.data.2006[which(OIL.OECD.PRICES.data.2006$Sector %in% Sector),]
dim(OIL.OECD.PRICES.data.2006)

# We merge different databases 
OIL.OECD.PRICES.data.1978.2016<-rbind(OIL.OECD.PRICES.data.1978,OIL.OECD.PRICES.data.1991,OIL.OECD.PRICES.data.2006)
OIL.OECD.PRICES.data.1978.2016<-OIL.OECD.PRICES.data.1978.2016[!is.na(OIL.OECD.PRICES.data.1978.2016$Value),]

dim(OIL.OECD.PRICES.data.1978.2016)
# We remove possible duplicated data
OIL.OECD.PRICES.data.1978.2016<-OIL.OECD.PRICES.data.1978.2016[!duplicated(OIL.OECD.PRICES.data.1978.2016),]
dim(OIL.OECD.PRICES.data.1978.2016)

# We remove data of Yearly quarters
# OIL.OECD.PRICES.data.1978.20162<-OIL.OECD.PRICES.data.1978.2016[which(!startsWith(OIL.OECD.PRICES.data.1978.2016$Year, "Q", trim=TRUE, ignore.case=TRUE)),]
OIL.OECD.PRICES.data.1978.2016<-OIL.OECD.PRICES.data.1978.2016[which(!grepl("^Q",OIL.OECD.PRICES.data.1978.2016$Year)),]
dim(OIL.OECD.PRICES.data.1978.2016)

# We sort out data
OIL.OECD.PRICES.data.1978.2016<-OIL.OECD.PRICES.data.1978.2016[
  with(OIL.OECD.PRICES.data.1978.2016, order(Country,Flow,Sector,Product,Year)),]

#View(OIL.OECD.PRICES.data.1978.2016)
#View(OIL.OECD.PRICES.data.1978.2016[which((OIL.OECD.PRICES.data.1978.2016$Flow=="Total price (USD/unit using PPP)")|
#                                 (OIL.OECD.PRICES.data.1978.2016$Flow=="Total price (USD/unit)")),])

colnames(OIL.OECD.PRICES.data.1978.2016)
Variable<-paste(OIL.OECD.PRICES.data.1978.2016$Product, OIL.OECD.PRICES.data.1978.2016$Sector, OIL.OECD.PRICES.data.1978.2016$Flow,sep="-")

OIL.OECD.PRICES.data.1978.2016<-cbind(OIL.OECD.PRICES.data.1978.2016,Variable)
OIL.OECD.PRICES.data.1978.2016<-subset(OIL.OECD.PRICES.data.1978.2016, select = -c(Product, Sector, Flow))
OIL.OECD.PRICES.data.1978.2016<-OIL.OECD.PRICES.data.1978.2016[,c("Country","Year","Variable","Value")]
View(OIL.OECD.PRICES.data.1978.2016)

# .....................................................................................................................








# We select which Database we want to use
OIL.OECD.PRICES.data<-OIL.OECD.PRICES.data.1978.2020
#OIL.OECD.PRICES.data<-OIL.OECD.PRICES.data.1978.2016

# !!!!!!!!!!!!!!!!!!!!!!!!!! WARNING !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Year field MAY BE a character and not a number
# !!!!!!!!!!!!!!!!!!!!!!!!!! WARNING !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# We generate Data Base
rm(BBDD.OECD.Oil.Prices)
str(OIL.OECD.PRICES.data)
BBDD.OECD.Oil.Prices<-fGenBBDDPanel.1Dimension(OIL.OECD.PRICES.data, BBDD.OECD.Oil.Prices, "OECD")
View(BBDD.OECD.Oil.Prices)
#BBDD.OECD.Oil.Prices.US<-BBDD.OECD.Oil.Prices
#typeof(BBDD.OECD.Oil.Prices.US$Year)

file_writing_OECD.Oil.Prices<-paste(BBDD_OECD_writing_directory, "BBDD_OECD_Oil_Prices.csv",sep="")
write.csv(BBDD.OECD.Oil.Prices, file = file_writing_OECD.Oil.Prices)

# We assess the number of cells without NA on Diesel Prices
names(BBDD.OECD.Oil.Prices)
BBDD.OECD.Oil.Prices.Diesel<-BBDD.OECD.Oil.Prices[,c("Country","Year",
                                                     "OECD-Automotive diesel (litre)-Households-Total price (USD/unit using PPP)")]
RowsNA<-which(!is.na(BBDD.OECD.Oil.Prices.Diesel[,"OECD-Automotive diesel (litre)-Households-Total price (USD/unit using PPP)"]))
BBDD.OECD.Oil.Prices.Diesel<-BBDD.OECD.Oil.Prices.Diesel[RowsNA,]
dim(BBDD.OECD.Oil.Prices.Diesel)
View(BBDD.OECD.Oil.Prices.Diesel)

#BBDD.OECD.Oil.Prices.First.Paper<-BBDD.OECD.Oil.Prices
#View(BBDD.OECD.Oil.Prices.First.Paper)

#................................
#Comparación precios May vs Julio
#................................
BBDD.OECD.Oil.Prices.Diesel.July2020<-BBDD.OECD.Oil.Prices.Diesel
dim(BBDD.OECD.Oil.Prices.Diesel.July2020)

print("-------- May 2020 -------------------")
dim(BBDD.OECD.Oil.Prices.Diesel.May2020)
str(BBDD.OECD.Oil.Prices.Diesel.May2020)



print("-------- July 2020 -------------------")
dim(BBDD.OECD.Oil.Prices.Diesel.July2020)
str(BBDD.OECD.Oil.Prices.Diesel.July2020)

Diesel.May.July.2020 <- merge(BBDD.OECD.Oil.Prices.Diesel.May2020,BBDD.OECD.Oil.Prices.Diesel.July2020,
               by=c("Country","Year"))

View(Diesel.May.July.2020)
#BBDD.OECD.Oil.Prices.Diesel.2016<-BBDD.OECD.Oil.Prices.Diesel
#BBDD.OECD.Oil.Prices.Diesel.2017<-BBDD.OECD.Oil.Prices.Diesel

#..................................
# We make the correlation for Disel Prices from both BBDD in order to assess data ressemblance
#BBDD.OECD.Oil.Prices.Diesel<-merge(BBDD.OECD.Oil.Prices.Diesel.2016, BBDD.OECD.Oil.Prices.Diesel.2017,
#                                   by=c("Country","Year"), all=TRUE)
#DISEL.cor<-na.omit(BBDD.OECD.Oil.Prices.Diesel)
#cor(DISEL.cor[,3], DISEL.cor[,4],   method = c("pearson"))
#..................................


colnames(BBDD.OECD.Oil.Prices)

# Correlation for industry and Household prices
BBDD.OECD.Oil.Prices.cor<-BBDD.OECD.Oil.Prices[,c("OECD-Automotive diesel (litre)-Households-Total price (USD/unit using PPP)",
                                                    "OECD-Automotive diesel (litre)-Industry-Total price (USD/unit using PPP)")]
BBDD.OECD.Oil.Prices.cor<-na.omit(BBDD.OECD.Oil.Prices.cor)
cor(BBDD.OECD.Oil.Prices.cor[1],BBDD.OECD.Oil.Prices.cor[2],method = c("pearson"))

View(BBDD.OECD.Oil.Prices.cor)
str(BBDD.OECD.Oil.Prices)

range(BBDD.OECD.Oil.Prices[,"Year"])
levels(BBDD.OECD.Oil.Prices[,"Country"])

glimpse(BBDD.OECD.Oil.Prices)



#********************************************************************************************************
#********************************************************************************************************
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# -------------------------------------------------------------------------------------------------------
#                                         OECD ENERGY - EMISSIONS
# -------------------------------------------------------------------------------------------------------
# oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#********************************************************************************************************
#********************************************************************************************************




# ************************************************************************************************************
# ------------- OCDE: EMISSIONS  OECD - Per Capita CO2 Emissions by Sector ------------------------------
# ************************************************************************************************************
OECD.CSVfile.emissions.sector<-"ENERGY/IEA - International Energy Agency/EMISSIONS/OECD - Per Capita CO2 Emissions by Sector.csv"
file.OECD.EMISSIONS.sector<-paste(BBDD_OECD_reading_directory, OECD.CSVfile.emissions.sector,sep="")
OIL.OECD.Emissions.sector<- read.csv(file.OECD.EMISSIONS.sector, header=TRUE, sep=",", dec=".")

names(OIL.OECD.Emissions.sector)
OIL.OECD.Emissions.sector<-OIL.OECD.Emissions.sector[,c("Country","Time","Allocation","Flow","Value")]

colnames(OIL.OECD.Emissions.sector)
Variable<-paste(OIL.OECD.Emissions.sector$Allocation, OIL.OECD.Emissions.sector$Flow,sep=" - ")
OIL.OECD.Emissions.sector<-cbind(OIL.OECD.Emissions.sector,Variable)
OIL.OECD.Emissions.sector<-subset(OIL.OECD.Emissions.sector, select = -c(Allocation, Flow))
OIL.OECD.Emissions.sector<-OIL.OECD.Emissions.sector[,c("Country","Time","Variable","Value")]
colnames(OIL.OECD.Emissions.sector)<-c("Country","Year","Variable","Value")

#View(OIL.OECD.Emissions.sector)

# We generate Data Base
rm(BBDD.OECD.Oil.Emissions.sector)
BBDD.OECD.Oil.Emissions.sector<-fGenBBDDPanel.1Dimension(OIL.OECD.Emissions.sector, BBDD.OECD.Oil.Emissions.sector, "OECD")
View(BBDD.OECD.Oil.Emissions.sector)

colnames(BBDD.OECD.Oil.Emissions.sector)
Vars.Selection<-c("Country","Year",
                  "OECD-Emissions by sector (MtCO2) - Transport",
                  "OECD-Per capita emissions by sector (kgCO2/capita) - Transport",
                  "OECD-Emissions with electricity and heat allocated to consuming sectors (MtCO2) - Transport",
                  "OECD-Per capita emissions with electricity and heat allocated to consuming sectors (kgCO2/capita) - Transport",
                  "OECD-Emissions by sector (MtCO2) - of which: road",
                  "OECD-Per capita emissions by sector (kgCO2/capita) - of which: road",
                  "OECD-Emissions with electricity and heat allocated to consuming sectors (MtCO2) - of which: road",
                  "OECD-Per capita emissions with electricity and heat allocated to consuming sectors (kgCO2/capita) - of which: road")

BBDD.OECD.Oil.Emissions.sector.SELEC<-BBDD.OECD.Oil.Emissions.sector[,Vars.Selection]
#View(BBDD.OECD.Oil.Emissions.sector.SELEC)

levels(BBDD.OECD.Oil.Emissions.sector.SELEC$Country)


BBDD.OECD.Oil.Emissions.sector.SUM<-rowSums(BBDD.OECD.Oil.Emissions.sector[,c(12,13,14,18,19,20)],na.rm=FALSE)
BBDD.OECD.Oil.Emissions.sector.SUM<-cbind(BBDD.OECD.Oil.Emissions.sector[,c(1,2)],BBDD.OECD.Oil.Emissions.sector.SUM)
BBDD.OECD.Oil.Emissions.sector.SUM[which(BBDD.OECD.Oil.Emissions.sector.SUM[,"Country"]=="Spain"),]
View(BBDD.OECD.Oil.Emissions.sector.SUM)

#********************************************************************************************************
# ------------- OCDE: EMISSIONS  OECD - OECD - Indicators for CO2 Emissions from Fuel Combustion --------------------------------------
#********************************************************************************************************


# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#       Esta BBDD no es útil pues no tiene datos desglosados para carretera
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

rm(OIL.OECD.Emissions.Fuel)
OECD.CSVfile.emissions.Fuel<-"ENERGY/IEA - International Energy Agency/EMISSIONS/OECD - Indicators for CO2 Emissions from Fuel Combustion.csv"
file.OECD.EMISSIONS.Fuel<-paste(BBDD_OECD_reading_directory, OECD.CSVfile.emissions.Fuel,sep="")
OIL.OECD.Emissions.Fuel<- read.csv(file.OECD.EMISSIONS.Fuel, header=TRUE, sep=",", dec=".")

names(OIL.OECD.Emissions.Fuel)
OIL.OECD.Emissions.Fuel<-OIL.OECD.Emissions.Fuel[,c("Country","Time","Flow","Value")]

View(OIL.OECD.Emissions.Fuel)
colnames(OIL.OECD.Emissions.Fuel)
colnames(OIL.OECD.Emissions.Fuel)<-c("Country","Year","Variable","Value")


# We generate Data Base
rm(BBDD.OECD.Oil.Emissions.Fuel)
BBDD.OECD.Oil.Emissions.Fuel<-fGenBBDDPanel.1Dimension(OIL.OECD.Emissions.Fuel, BBDD.OECD.Oil.Emissions.Fuel, "OECD")
#View(BBDD.OECD.Oil.Emissions.Fuel)

colnames(BBDD.OECD.Oil.Emissions.Fuel)
levels(BBDD.OECD.Oil.Emissions.Fuel$Country)


#********************************************************************************************************
# ------------- OCDE: EMISSIONS  OECD - OECD - Emissions of CO2, CH4, N2O, HFCs, PFCs and SF6 -----------------------------------------
#********************************************************************************************************


rm(OECD.Emissions.Pollutants)
OECD.CSVfile.emissions.Pollutants<-"ENERGY/IEA - International Energy Agency/EMISSIONS/OECD - Emissions of CO2, CH4, N2O, HFCs, PFCs and SF6.csv"
file.OECD.EMISSIONS.Pollutants<-paste(BBDD_OECD_reading_directory, OECD.CSVfile.emissions.Pollutants,sep="")
OECD.Emissions.Pollutants<- read.csv(file.OECD.EMISSIONS.Pollutants, header=TRUE, sep=",", dec=".")

names(OECD.Emissions.Pollutants)
OECD.Emissions.Pollutants<-OECD.Emissions.Pollutants[,c("Country","Time","Variable","Value")]
#View(OECD.Emissions.Pollutants)


# We generate Data Base
rm(BBDD.OECD.Emissions.Pollutants)
BBDD.OECD.Emissions.Pollutants<-fGenBBDDPanel.1Dimension(OECD.Emissions.Pollutants, BBDD.OECD.Emissions.Pollutants, "OECD")
#View(BBDD.OECD.Emissions.Pollutants)

colnames(BBDD.OECD.Emissions.Pollutants)
levels(BBDD.OECD.Emissions.Pollutants$Country)



#View(BBDD.OECD.Oil.Prices.Diesel)
#View(BBDD.OECD.Oil.Emissions.sector)
#View(BBDD.OECD.Oil.Emissions.Fuel)
#View(BBDD.OECD.Emissions.Pollutants)



#********************************************************************************************************
# ------------- OECD - IPCC Fuel Combustion Emissions (2006 Guidelines)----------------------------------------------------------------
#********************************************************************************************************

rm(OECD.Emissions.IPCC)
OECD.CSVfile.emissions.IPCC<-"ENERGY/IEA - International Energy Agency/EMISSIONS/OECD - IPCC Fuel Combustion Emissions (2006 Guidelines).csv"
file.OECD.EMISSIONS.IPCC<-paste(BBDD_OECD_reading_directory, OECD.CSVfile.emissions.IPCC,sep="")
OECD.Emissions.IPCC<- read.csv(file.OECD.EMISSIONS.IPCC, header=TRUE, sep=",", dec=".")

names(OECD.Emissions.IPCC)
OECD.Emissions.IPCC<-OECD.Emissions.IPCC[,c("Country","Time","Product","Flow","Value")]

levels(OECD.Emissions.IPCC$Product)
levels(OECD.Emissions.IPCC$Flow)

Variable<-paste(OECD.Emissions.IPCC$Product, OECD.Emissions.IPCC$Flow,sep=" - ")
OECD.Emissions.IPCC<-cbind(OECD.Emissions.IPCC,Variable)
OECD.Emissions.IPCC<-subset(OECD.Emissions.IPCC, select = -c(Product, Flow))
OECD.Emissions.IPCC<-OECD.Emissions.IPCC[,c("Country","Time","Variable","Value")]
colnames(OECD.Emissions.IPCC)<-c("Country","Year","Variable","Value")

rm(BBDD.OECD.Emissions.IPCC)
BBDD.OECD.Emissions.IPCC<-fGenBBDDPanel.1Dimension(OECD.Emissions.IPCC, BBDD.OECD.Emissions.IPCC, "OECD")
#View(BBDD.OECD.Emissions.IPCC)
names(BBDD.OECD.Emissions.IPCC)

#********************************************************************************************************
# ------------- OECD - CO2 emissions by product and flow 2001 -2017----------------------------------------------------------------
#********************************************************************************************************





rm(OECD.Emissions.Product_AND_Flow)
OECD.CSVfile.Emissions.Product_AND_Flow<-"ENERGY/Not fuels/World Energy Balances 2016.csv"
file.OECD.EMISSIONS.Product_AND_Flow<-paste(BBDD_OECD_reading_directory, OECD.CSVfile.Emissions.Product_AND_Flow,sep="")
OECD.Emissions.Product_AND_Flow<- read.csv(file.OECD.EMISSIONS.Product_AND_Flow, header=TRUE, sep=",", dec=".")

names(OECD.Emissions.Product_AND_Flow)
OECD.Emissions.Product_AND_Flow<-OECD.Emissions.Product_AND_Flow[,c("Country","Time","Product","Flow","Value")]

levels(OECD.Emissions.Product_AND_Flow$Product)
levels(OECD.Emissions.Product_AND_Flow$Flow)


OECD.Emissions.Product_AND_Flow<-OECD.Emissions.Product_AND_Flow[which(OECD.Emissions.Product_AND_Flow[,"Product"]=="Total"),]

Variable<-paste(OECD.Emissions.Product_AND_Flow$Product, OECD.Emissions.Product_AND_Flow$Flow,sep=" - ")
OECD.Emissions.Product_AND_Flow<-cbind(OECD.Emissions.Product_AND_Flow,Variable)

OECD.Emissions.Product_AND_Flow<-subset(OECD.Emissions.Product_AND_Flow, select = -c(Product, Flow))
OECD.Emissions.Product_AND_Flow<-OECD.Emissions.Product_AND_Flow[,c("Country","Time","Variable","Value")]
colnames(OECD.Emissions.Product_AND_Flow)<-c("Country","Year","Variable","Value")

rm(BBDD.OECD.Emissions.Product_AND_Flow)
BBDD.OECD.Emissions.Product_AND_Flow<-fGenBBDDPanel.1Dimension(OECD.Emissions.Product_AND_Flow, BBDD.OECD.Emissions.Product_AND_Flow, "OECD")
#View(BBDD.OECD.Emissions.IPCC)
names(BBDD.OECD.Emissions.Product_AND_Flow)

BBDD.OECD.Emissions.Product_AND_Flow[which(BBDD.OECD.Emissions.Product_AND_Flow[,"Country"]=="France"),]

levels(BBDD.OECD.Emissions.Product_AND_Flow$Country)

# A.Transport<-BBDD.OECD.Emissions.Product_AND_Flow[,c("Country","Year","OECD-Total - Road","OECD-Total - Transport")]
A.Transport<-cbind(BBDD.OECD.Emissions.Product_AND_Flow[,c("Country","Year")],rowSums(BBDD.OECD.Emissions.Product_AND_Flow[,c(3:41)]))
A.Transport[which(A.Transport[,"Country"]=="France"),]

names(BBDD.OECD.Emissions.Product_AND_Flow)








# ************************************************************************************************************
# ************************************************************************************************************
# ------------------------------------------------------------------------------------------------------------
# ------------------------- MERGING & CONSTRUCTING DATABASE WITH US GPD PPP Constant prices ------------------
# ------------------------------------------------------------------------------------------------------------
# ************************************************************************************************************
# ************************************************************************************************************
#  09-09-2017

# Bases datos a evaluar
# BBDD_OECD_MA_NAccounts.VAR.SELECTED
# BBDD_OECD_MA_DispIncome
# BBDD_OECD_MA_GDP_PPP


BBDD_OECD_MA_NAccounts.VAR.SELECTED.Countries<-levels(BBDD_OECD_MA_NAccounts.VAR.SELECTED$Country)
BBDD_OECD_MA_DispIncome.Countries<-levels(BBDD_OECD_MA_DispIncome$Country)
BBDD_OECD_MA_GDP_PPP.Countries<-levels(BBDD_OECD_MA_GDP_PPP$Country)

#View(BBDD_OECD_MA_NAccounts.VAR.SELECTED)
#View(BBDD_OECD_MA_DispIncome)
#View(BBDD_OECD_MA_GDP_PPP)

colnames(BBDD_OECD_MA_NAccounts.VAR.SELECTED)
colnames(BBDD_OECD_MA_DispIncome)
colnames(BBDD_OECD_MA_GDP_PPP)

BBDD_OECD_MA_NAccounts.VAR.GDP_PPP<-BBDD_OECD_MA_NAccounts.VAR.SELECTED[,c("Country","Year", "OECD-GDP at 2010 constant prices and PPPs, billions US dollars //*//-GDPVPVOB")]
BBDD_OECD_MA_NAccounts.VAR.GDP_PPP[,3]<-1000*BBDD_OECD_MA_NAccounts.VAR.GDP_PPP[,3]
BBDD_OECD_MA_GDP_PPP.VAR.GDP_PPP<-BBDD_OECD_MA_GDP_PPP[,c("Country","Year","OCDE-Gross domestic product (expenditure approach) Constant prices, constant PPPs, OECD base year 2010 Millions US Dollar")]
BBDD_OECD_MA_DispIncome.VAR.GDP_PPP<-BBDD_OECD_MA_DispIncome[,c("Country","Year",
                                                                "OECD-Gross domestic product  Constant prices, constant PPPs, OECD base year 2010 Millions US Dollar")]

#View(BBDD_OECD_MA_NAccounts.VAR.GDP_PPP)
#View(BBDD_OECD_MA_GDP_PPP.VAR.GDP_PPP)
#View(BBDD_OECD_MA_DispIncome.VAR.GDP_PPP)

rm(BBDD_OECD.VAR.GDP_PPP)
BBDD_OECD.VAR.GDP_PPP<-merge(BBDD_OECD_MA_DispIncome.VAR.GDP_PPP, BBDD_OECD_MA_GDP_PPP.VAR.GDP_PPP, by=c("Country","Year"), all=TRUE)
BBDD_OECD.VAR.GDP_PPP<-merge(BBDD_OECD.VAR.GDP_PPP, BBDD_OECD_MA_NAccounts.VAR.GDP_PPP, by=c("Country","Year"), all=TRUE)
BBDD_OECD.VAR.GDP_PPP.Merged<-BBDD_OECD.VAR.GDP_PPP

#View(BBDD_OECD.VAR.GDP_PPP.Merged)

# We take a list of the Countries contained in all previous databases
GDP_PPP.Countries<-sort(levels(BBDD_OECD.VAR.GDP_PPP.Merged$Country))

# We take just the average value and remove data with NAs
# We should consider if taking the average is the best option
rm(Colum.Names)
BBDD_OECD.VAR.GDP_PPP.Merged <- transform(BBDD_OECD.VAR.GDP_PPP.Merged[,-c(3:5)],
                                          Col4 = rowMeans(BBDD_OECD.VAR.GDP_PPP.Merged[,c(3:5)], na.rm = TRUE))
Colum.Names<-colnames(BBDD_OECD.VAR.GDP_PPP.Merged)
#Colum.Names[3]<-"OECD-Gross domestic product  Constant prices, constant PPPs, OECD base year 2010 Millions US Dollar"
Colum.Names[3]<-"OECD-Gross domestic product (expenditure approach) Constant prices, constant PPPs, OECD base year 2010 Millions US Dollar"
colnames(BBDD_OECD.VAR.GDP_PPP.Merged)<-Colum.Names
#View(BBDD_OECD.VAR.GDP_PPP.Merged) 
colnames(BBDD_OECD.VAR.GDP_PPP.Merged)



# INIC: ESTA PARTE PODEMOS BORRARLA
# ---------------------------------------------------
# We remove rows with all values NAs
# rm(AAA)
# AAA<- is.na(BBDD_OECD.VAR.GDP_PPP[,3:5])

# rm(BBB)
# BBB<-BBDD_OECD.VAR.GDP_PPP[1,]
# for (i in 1:dim(BBDD_OECD.VAR.GDP_PPP)[1])
# {
#   if(AAA[i,1] & AAA[i,2] & AAA[i,3]) next
#  BBB <- rbind(BBB,BBDD_OECD.VAR.GDP_PPP[i,])
# }
# BBB<-BBB[-1,]
# BBDD_OECD.VAR.GDP_PPP2 <- BBB 
# We take the average of the 3 GDPs
# rm(Colum.Names)
# BBDD_OECD.VAR.GDP_PPP2 <- transform(BBDD_OECD.VAR.GDP_PPP2[,-c(3:5)],
#                                    Col4 = rowMeans(BBDD_OECD.VAR.GDP_PPP2[,c(3:5)], na.rm = TRUE))
# Colum.Names<-colnames(BBDD_OECD.VAR.GDP_PPP2)
# Colum.Names[3]<-"OECD-Gross domestic product  Constant prices, constant PPPs, OECD base year 2010 Millions US Dollar"
# Colum.Names[3]<-"OECD-Gross domestic product (expenditure approach) Constant prices, constant PPPs, OECD base year 2010 Millions US Dollar"
# colnames(BBDD_OECD.VAR.GDP_PPP2)<-Colum.Names
# View(BBDD_OECD.VAR.GDP_PPP2) 
# colnames(BBDD_OECD.VAR.GDP_PPP2)
# --------------------------------------------------- 
# FIN: ESTA PARTE PODEMOS BORARARLA









# **********************************************************************************************************
# **********************************************************************************************************
# **********************************************************************************************************
# **********************************************************************************************************
# **********************************************************************************************************
# **********************************************************************************************************
# ---------------------------- FIN LECTURA BÁSICA DATOS BASES DE DATOS -------------------------------------
# **********************************************************************************************************
# **********************************************************************************************************
# **********************************************************************************************************
# **********************************************************************************************************
# **********************************************************************************************************
# **********************************************************************************************************




# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ------------- OCDE: ENERGY PRICES----------------------------------------------------------------------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# 1167 conversion toe litro para diesel

OECD_CSVfile_fuel_PRICES<-"ENERGY/OECD - Energy prices in national currency per toe.csv"
file_OECD_PRICES<-paste(BBDD_OECD_reading_directory, OECD_CSVfile_fuel_PRICES,sep="")
OIL_OECD_PRICESdata<- read.csv(file_OECD_PRICES, header=TRUE, sep=",", dec=".")

# We check it out if data has been correctly downloaded
head(OIL_OECD_PRICESdata)

# We remove Field "PRICES" once that we assure we only have PRICES data
OIL_OECD_PRICESdata<-OIL_OECD_PRICESdata[,c("Country","Time","Product","Sector","Flow","Value")]
dim(OIL_OECD_PRICESdata)
#names(OIL_OECD_PRICESdata)<-c("Country","Time","Variable","Value")
# If dim is 1.000.000 that means data has been trunkated
OIL_OECD_PRICESdata<-OIL_OECD_PRICESdata[which(!is.na(OIL_OECD_PRICESdata$Value)),]

# HASTA AQUÍ OK!!!!!!!!!!!!!!!!!!!!!

dim(OIL_OECD_PRICESdata)
head(OIL_OECD_PRICESdata)

levels(OIL_OECD_PRICESdata$Country)

View(OIL_OECD_PRICESdata)


# We check it out existing levels
levels(OIL_OECD_PRICESdata$Variable)
LEVELS_OECD_PRICES<-levels(OIL_OECD_PRICESdata$Variable)
levels(OIL_OECD_PRICESdata$Country)
levels(OIL_OECD_PRICESdata$Time)
range(OIL_OECD_PRICESdata$Time)

# We check it out which possible Gasoline or Diesel elements are in vector Product
LEVELS_OECD_OIL_PRICES<-LEVELS_OECD_PRICES[grep("diesel|gasoline", ignore.case=TRUE, LEVELS_OECD_PRICES)]

OIL_PRICES_Fuels<-c("Motor gasoline excl. biofuels (kt)","Biogasoline (kt)",
                    "Gas/diesel oil excl. biofuels (kt)", "Biodiesels (kt)")

OIL_OECD_PRICESdata<-OIL_OECD_PRICESdata[which(OIL_OECD_PRICESdata$Variable %in% OIL_PRICES_Fuels),]
length(OIL_OECD_PRICESdata$Variable)
# We remove levels which are not any longer in use
LEVELS_OECD_OIL_PRICES<-levels(droplevels(OIL_OECD_PRICESdata$Variable))

rm(BBDD_OECD_OIL_PRICES)
BBDD_OECD_OIL_PRICES<-OIL_OECD_PRICESdata[which(OIL_OECD_PRICESdata[,"Variable"]==LEVELS_OECD_OIL_PRICES[1]),]
VAR_NAME<-paste("OECD",LEVELS_OECD_OIL_PRICES[1],sep="-")
colnames(BBDD_OECD_OIL_PRICES)<-c("Country","Year","Variable",VAR_NAME)
BBDD_OECD_OIL_PRICES<-subset(BBDD_OECD_OIL_PRICES, select = -c(Variable))

for (i in 2:length(LEVELS_OECD_OIL_PRICES))
{
  A<-OIL_OECD_PRICESdata[which(OIL_OECD_PRICESdata[,"Variable"]==LEVELS_OECD_OIL_PRICES[i]),]
  VAR_NAME<-paste("OECD",LEVELS_OECD_OIL_PRICES[i],sep="-")
  colnames(A)<-c("Country","Year","Variable",VAR_NAME)
  A<-subset(A, select = -c(Variable))
  BBDD_OECD_OIL_PRICES<-merge(BBDD_OECD_OIL_PRICES, A,  by=c("Country","Year"), all=TRUE)
}


# ******************************************************************************************************************************
# ******************************************************************************************************************************
#                                      OECD PRICES
# ******************************************************************************************************************************
# ******************************************************************************************************************************



OECD_fuel_PRICES.US.PPP<-"ENERGY/PRICES/OECD - Energy prices in PPP US dollars.csv"

file_fuel_PRICES.US.PPP<-paste(BBDD_OECD_reading_directory, OECD_fuel_PRICES.US.PPP,sep="")
OECD_fuel_PRICES.US.PPP.data<- read.csv(file_fuel_PRICES.US.PPP, header=TRUE, sep=",", dec=".")

# We check it out if data has been correctly downloaded
head(OECD_fuel_PRICES.US.PPP.data)

# We remove Field "PRICES" once that we assure we only have PRICES data
OECD_fuel_PRICES.US.PPP.data<-OECD_fuel_PRICES.US.PPP.data[,c("Country","Time","Product","Sector","Flow","Value")]
dim(OECD_fuel_PRICES.US.PPP.data)
#names(OIL_OECD_PRICESdata)<-c("Country","Time","Variable","Value")
# If dim is 1.000.000 that means data has been trunkated
OECD_fuel_PRICES.US.PPP.data<-OECD_fuel_PRICES.US.PPP.data[which(!is.na(OECD_fuel_PRICES.US.PPP.data$Value)),]
OECD_fuel_PRICES.US.PPP.data.INIC<-OECD_fuel_PRICES.US.PPP.data
levels(droplevels(OECD_fuel_PRICES.US.PPP.data$Product))


OECD_fuel_PRICES.US.PPP.data<-OECD_fuel_PRICES.US.PPP.data.INIC
Fuels<-c("Automotive diesel (litre)","Premium leaded gasoline (litre)","Premium unleaded 98 RON (litre)",
         "Premium unleaded 95 RON (litre)","Regular leaded gasoline (litre)","Regular unleaded gasoline (litre)")

Fuels<-c("Automotive diesel (litre)")

OECD_fuel_PRICES.US.PPP.data<-OECD_fuel_PRICES.US.PPP.data[which(OECD_fuel_PRICES.US.PPP.data[,"Product"] %in% Fuels),]

levels(droplevels(OECD_fuel_PRICES.US.PPP.data[,"Product"]))
levels(droplevels(OECD_fuel_PRICES.US.PPP.data[,"Country"]))

dim(OECD_fuel_PRICES.US.PPP.data)
dim(OECD_fuel_PRICES.US.PPP.data.INIC)
View(OECD_fuel_PRICES.US.PPP.data)


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::






# *********************************************************************************************************
# ---------------------------------------------------------------------------------------------------------
# COMPARACIÓN DATOS GDP ENTRE BBDD OCDE y EUROSTAT
# ---------------------------------------------------------------------------------------------------------
# *********************************************************************************************************


# ----------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------

# Comparación datos GDP entre OCDE y EUROSTAT desde BBDD OECDE BBDD_OECD_MA_DispIncome y EUROSTAT: BBDD_EUROSTAT_GDP_and_COMPONENTS

TEMP.OCDE.GDP.EuroCountries<-MA_DispIncome.Data.Orig[which(MA_DispIncome.Data.Orig[,"Unit"]=="Euro"),]

OECD_GDP_CLV<-BBDD_OECD_MA_DispIncome[,c("Country", "Year", "OECD-Gross domestic product  Constant prices, national base year 2010 Millions Euro")]
EUROSTAT_GDP_CLV<-BBDD_EUROSTAT_GDP_and_COMPONENTS[,c("Country", "Year","EUROSTAT-Gross domestic product at market prices-Chain linked volumes (2010), million euro")]
LEVELS_EuroCountries<-levels(droplevels(TEMP.OCDE.GDP.EuroCountries[,"Country"]))
OECD_GDP_CLV.EuroCountries<-OECD_GDP_CLV[which(OECD_GDP_CLV$Country %in% LEVELS_EuroCountries),]

TEMP.GDP<-merge(OECD_GDP_CLV.EuroCountries, EUROSTAT_GDP_CLV,  by=c("Country","Year"), all=TRUE)
View(TEMP.GDP)


BBDD_OECD_MA_DispIncome[which(BBDD_OECD_MA_DispIncome$Country=="Belgium"),"OECD-Gross domestic product  Constant prices, national base year 2010 Millions Euro"]
View(BBDD_OECD_MA_DispIncome[which(BBDD_OECD_MA_DispIncome$Country=="Belgium"),])

View(MA_DispIncome.Data.Orig[which(MA_DispIncome.Data.Orig[,"Country"]=="Belgium"),])
View(MA_DispIncome.Data.Orig[which(MA_DispIncome.Data.Orig[,"Country"]=="Japan"),])

View(MA_DispIncome.Data.Orig[which(MA_DispIncome.Data.Orig[,"Country"]=="Japan" & 
                                     MA_DispIncome.Data.Orig[,"Measure"]=="Constant prices, constant PPPs, OECD base year" &
                                     MA_DispIncome.Data.Orig[,"Transaction"]=="Gross domestic product "
),])

#  La varialbe "Constant prices, constant PPPs, OECD base year" parece la más adecuada para hacer comparaciones
# de GDP por países

levels(MA_DispIncome.Data.Orig$Country)


# ----------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------






# ----------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------

# We check it out fuel available data

TEMP<-merge(BBDD_EUROSTAT_ROAD_OIL, BBDD_OECD_OIL_Road,  by=c("Country","Year"), all=TRUE)
View(TEMP)

colnames(TEMP)
# We take only fuel data without NAs
Data_2be_used<-c("Country","Year","OECD-Gas/diesel oil excl. biofuels (kt)","OECD-Motor gasoline excl. biofuels (kt)",
                 "EUROSTAT-Gas/diesel oil (without bio components)-Thousand tonnes","EUROSTAT-Gasoline (without bio components)-Thousand tonnes")

Data_2Display<-!(is.na(TEMP[,"OECD-Gas/diesel oil excl. biofuels (kt)"]) &
                   is.na(TEMP[,"EUROSTAT-Gas/diesel oil (without bio components)-Thousand tonnes"]))

Fuel_Useful_Data<-TEMP[Data_2Display, Data_2be_used]

View(Fuel_Useful_Data)
dim(Fuel_Useful_Data)
summary(Fuel_Useful_Data[,"Country"])


library(psych)
View(describeBy(Fuel_Useful_Data,Fuel_Useful_Data$Country, mat=TRUE))
describeBy(Fuel_Useful_Data,Fuel_Useful_Data$Country, mat=FALSE)
mean(Fuel_Useful_Data[which(Fuel_Useful_Data[,"Country"]=="Zambia"),"OECD-Motor gasoline excl. biofuels (kt)"])
sd(Fuel_Useful_Data[which(Fuel_Useful_Data[,"Country"]=="Zambia"),"OECD-Motor gasoline excl. biofuels (kt)"])

# ------------------------------
OECD.OIL.Data_2be_used<-c("Country","Year","OECD-Gas/diesel oil excl. biofuels (kt)","OECD-Motor gasoline excl. biofuels (kt)")


# ------------------------------


Fuel_Useful_Data_Spain<-Fuel_Useful_Data[which(Fuel_Useful_Data[,"Country"]=="Spain"),]

describeBy(Fuel_Useful_Data_Spain, Fuel_Useful_Data_Spain$Country, mat=FALSE)

Fuel_Useful_Data_TEMP<-Fuel_Useful_Data

colnames(Fuel_Useful_Data_TEMP)<-c("Country","Year","OECD_Diesel","OECD_Gasoline",
                                   "EUROSTAT_Diesel","EUROSTAT_Gasoline")

colnames(Fuel_Useful_Data_Spain)<-c("Country","Year","OECD_Diesel","OECD_Gasoline",
                                    "EUROSTAT_Diesel","EUROSTAT_Gasoline")


# We check it out which data is different between OECD and EUROSTAT database

DiffGasoline<-Fuel_Useful_Data[,"OECD-Motor gasoline excl. biofuels (kt)"]- Fuel_Useful_Data[,"EUROSTAT-Gasoline (without bio components)-Thousand tonnes"]
DiffDiesel<-Fuel_Useful_Data[,"OECD-Gas/diesel oil excl. biofuels (kt)"]-Fuel_Useful_Data[,"EUROSTAT-Gas/diesel oil (without bio components)-Thousand tonnes"]

C<-na.omit(cbind(DiffGasoline,DiffDiesel))

# Visualizamos los datos que son diferentes
C[which(C[,"DiffGasoline"]!=0 | C[,"DiffDiesel"]!=0), c("DiffGasoline","DiffDiesel")]


library(ggplot2)
ggplot(data = Fuel_Useful_Data_Spain, aes(x = Year, y = OECD_Diesel)) + geom_point() +
  stat_smooth(method = "lm", se = FALSE) + facet_wrap(~Country) + ggtitle("TOE TOTAL PETROLEUM / GDP")








# http://www.oecd-ilibrary.org.are.uab.cat/energy/data/iea-energy-prices-and-taxes-statistics_eneprice-data-en

# http://stats.oecd.org.are.uab.cat/viewhtml.aspx?datasetcode=END_TOE&lang=en#

# http://www.oecd-ilibrary.org.are.uab.cat/search?option1=titleAbstract&option2=&value2=&option3=&value3=&option4=&value4=&option5=&value5=&option6=&value6=&option7=&value7=&option8=&value8=&option9=&value9=&option10=&value10=&option11=&value11=&option12=&value12=&option13=&value13=&option14=&value14=&option15=&value15=&option16=&value16=&option17=&value17=&option22=excludeKeyTableEditions&value22=true&option18=sort&value18=&form_name=quick&discontin=factbooks&value1=End-use+prices%3A+Energy+prices+in+national+currency+per+toe


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::





# ******************************************************************************************************************************
# ******************************************************************************************************************************
#                             POTENCIALES VARIABLES CON LA QUE TRABAJAR
# ******************************************************************************************************************************
# ******************************************************************************************************************************

# ------------------------------------------------------------------------------
# BBDD_OECD_MA_NAccounts
# ------------------------------------------------------------------------------


















