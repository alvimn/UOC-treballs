
# File on progress

library("psych")
library("formattable")
library("tibble")

library("gdata")
library("stargazer")
library("xtable")
library("utils")
library("texreg")
library("pastecs")
library("data.table")

library(formattable)
library(lmtest)
library(diffdf)
library(corrplot)

library("plm")
library("lme4")
library("lqmm")
library("qrLMM")
library(AER)

library("ggplot2")
#install_github("easyGgplot2", "kassambara")
library("easyGgplot2")
library("graphics")
library("expss")


library(gridExtra)
library(grid)
library(lattice)

#install.packages("devtools")
library(devtools)
library(sandwich)

library(car) 
library(fmsb)


library("rJava")
library("openxlsx")
library("reshape2")
#library("xlsx")

library("pastecs")
library("data.table")


library("knitLatex")
library("dplyr")


#install.packages("devtools")
library(devtools)

library(fmsb)

library("Hmisc")
source("http://www.sthda.com/upload/rquery_cormat.r")

library("rJava")
library("openxlsx")
library("reshape2")
#library("xlsx")




# .................................................................................................
#Directory where raw data is saved

# ALBERTO: Directory with raw data
# Back Up directory
Directory.Data.Reading<-"C:/Users/alber/Dropbox/RESEARCH - SFA/data/Back up/2019/"

# Directory with last data
Directory.Data.Reading<-"C:/Users/alber/Dropbox/RESEARCH - SFA/data/"


# GIANCARLO: You need to change this directory
Directory.Data.Reading<-"C:/Users/gianc/Dropbox/RESEARCH - SFA/"
# .................................................................................................

# We read data from .csv file
rm(DATA.REGRESSION.SFA)
Data.file<-"SFA-TRANSPORT-FINAL-Data.csv"


#file.DATA.REGRESSION.SFA<-paste(Directory.Data.Reading, 'data/',Data.file,sep="")
file.DATA.REGRESSION.SFA<-paste(Directory.Data.Reading, Data.file,sep="")
DB.REGRESSION.SFA<- read.csv(file.DATA.REGRESSION.SFA, header=TRUE, sep=",", dec=".", check.names = FALSE)

dim(DB.REGRESSION.SFA)
str(DB.REGRESSION.SFA)
View(DB.REGRESSION.SFA)

#We remove first column without data
DB.REGRESSION.SFA<-DB.REGRESSION.SFA[,-which(names(DB.REGRESSION.SFA)=="")]


# -------------------------------------------
# We study Mexico data cause we have noticed data for road passenger transport it is incomplete,
# (just for buses but not for cars) so on 2019 version we were using variables improperly
levels(as.factor(DB.REGRESSION.SFA[,"Country"]))

Countries.SFA<-levels(as.factor((DB.REGRESSION.SFA[,"Country"])))
Countries.SFA[grepl("Mexico", Countries.SFA, ignore.case = TRUE)]
Road.Vars<-names(DB.REGRESSION.SFA)[grepl("Road", names(DB.REGRESSION.SFA), ignore.case = TRUE)]
Road.Vars

rm(DB.REGRESSION.SFA.Mexico)
DB.REGRESSION.SFA.Mexico<-(DB.REGRESSION.SFA[which(DB.REGRESSION.SFA[,"Country"]=="Mexico"),])
View(DB.REGRESSION.SFA.Mexico[,c("Country", "Year",Road.Vars)])
 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# REGRESSORS SELECTION

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

colnames(DB.REGRESSION.SFA)







#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# BEGIN: LOGISTICS DATA ASSESSMENT

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# .................................................................................................
# Data with quality OR logistics
Quality.Vars<-names(DB.REGRESSION.SFA)[grepl("quality",names(DB.REGRESSION.SFA), ignore.case = TRUE) ]
Logistics.Vars<-names(DB.REGRESSION.SFA)[grepl("logistics",names(DB.REGRESSION.SFA), ignore.case = TRUE) ]

dim(na.omit(DB.REGRESSION.SFA[,c("Country","Year",Quality.Vars)]))
dim(na.omit(DB.REGRESSION.SFA[,c("Country","Year",Logistics.Vars)]))
View(na.omit(DB.REGRESSION.SFA[,c("Country","Year",Logistics.Vars)]))

# Data with logistics and infrastructure quality is scarce
# .................................................................................................

# Data selection
# Again, we make a preliminay  variables assessment using a subset of countries in order
# to figure out the available data and as a consequence, the final variables to use
colnames(DB.REGRESSION.SFA)
rm(DB.REGRESSION.SFA.2.Assess)
DB.REGRESSION.SFA.2.Assess<-DB.REGRESSION.SFA[which(DB.REGRESSION.SFA[,"Country"] %in% Coutries.2.Assess),]
View(DB.REGRESSION.SFA.2.Assess[,c("Country","Year","OECD~TM~FT-Total inland freight transport /-/ Tonnes-kilometres",
                                   Road.freight.Vars, Rail.freight.Vars)])

Road.freight.Vars
Rail.freight.Vars

#Variables selection

# "OECD~TM~FT-Road freight transport /-/ Tonnes-kilometres" --> Advantage of this variable is that 
# we have data from 1970. But IT IS NOT QUITE USEFUL cause train data begins on 1994

names(DB.REGRESSION.SFA)[grepl("Tonnes-kilometres",names(DB.REGRESSION.SFA), ignore.case = TRUE) &
                           grepl("transport",names(DB.REGRESSION.SFA), ignore.case = TRUE) ]

# COMENTARIO A BORRAR CUANDO ESTÉ ARREGLADO: Es neceario modificar la base de datos de Transporte para incluir los datos 
# de Share de modos de transporte






#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# BEGIN: DATA SELECTION WITH INLAND PASSENGER TRANSPORT

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

rm(Road.passenger.Vars)
rm(Road.freight.Vars)

# Data with passenger & road
Road.passenger.Vars<-names(DB.REGRESSION.SFA)[grepl("passenger",names(DB.REGRESSION.SFA), ignore.case = TRUE)&
                           grepl("road",names(DB.REGRESSION.SFA), ignore.case = TRUE)]

Road.freight.Vars<-names(DB.REGRESSION.SFA)[grepl("freight",names(DB.REGRESSION.SFA), ignore.case = TRUE) &
                                              grepl("road",names(DB.REGRESSION.SFA), ignore.case = TRUE)]
Road.Vars
Road.passenger.Vars
Road.freight.Vars
dim(na.omit(DB.REGRESSION.SFA[,c("Country","Year",Road.passenger.Vars)]))
View(na.omit(DB.REGRESSION.SFA[,c("Country","Year",Road.passenger.Vars)]))


# .................................................................................................
# Data with passenger & rail
Rail.passenger.Vars<-names(DB.REGRESSION.SFA)[grepl("passenger",names(DB.REGRESSION.SFA), ignore.case = TRUE)&
                           grepl("rail",names(DB.REGRESSION.SFA), ignore.case = TRUE)]
Rail.passenger.Vars

dim(na.omit(DB.REGRESSION.SFA[,c("Country","Year",Rail.passenger.Vars)]))
dim(na.omit(DB.REGRESSION.SFA[,c("Country","Year","OECD~TM~PT-Rail passenger transport /-/ Passenger-kilometres")]))
View(na.omit(DB.REGRESSION.SFA[,c("Country","Year",Rail.passenger.Vars)]))
View((DB.REGRESSION.SFA[,c("Country","Year",Rail.passenger.Vars)]))


# .................................................................................................
# Data selection
# We make a preliminay  variables assessment using a subset of countries in order
# to figure out the available data and as a consequence, the final variables to use
rm(Coutries.2.Assess)
Coutries.2.Assess<-c("Spain","France","Italy", "Germany", "Mexico")
rm(DB.REGRESSION.SFA.2.Assess)
DB.REGRESSION.SFA.2.Assess<-DB.REGRESSION.SFA[which(DB.REGRESSION.SFA[,"Country"] %in% Coutries.2.Assess),]
View(DB.REGRESSION.SFA.2.Assess[,c("Country","Year", Road.passenger.Vars, Rail.passenger.Vars )])

#..........................
#Variables to use
rm(FINAL.Passenger.Rail.Vars)
#After assessing Rail passenger variables, this is the proper one in order to make inland % distribution
FINAL.Passenger.Rail.Vars<-c("OECD~TM~PT-Rail passenger transport /-/ Passenger-kilometres")

View(DB.REGRESSION.SFA.2.Assess[,c("Country","Year",Road.passenger.Vars, FINAL.Passenger.Rail.Vars )])

#..........................
# We figure out a % distribution for the transport modes
DB.REGRESSION.SFA.2.Assess<-DB.REGRESSION.SFA.2.Assess[,c("Country","Year",
                                                          Road.passenger.Vars, 
                                                          FINAL.Passenger.Rail.Vars )]
colnames(DB.REGRESSION.SFA.2.Assess)

rm(Total.Inland.Passenger.Transport)
Total.Inland.Passenger.Transport<-rowSums(DB.REGRESSION.SFA.2.Assess[,c("OECD~TM~PT-Road passenger transport by buses and coaches /-/ Passenger-kilometres",
                                                                        "OECD~TM~PT-Road passenger transport by passenger cars /-/ Passenger-kilometres",
                                                                        "OECD~TM~PT-Rail passenger transport /-/ Passenger-kilometres")])
rm(Perctg.bus.Passenger.Transport)
Perctg.bus.Passenger.Transport<-DB.REGRESSION.SFA.2.Assess[,"OECD~TM~PT-Road passenger transport by buses and coaches /-/ Passenger-kilometres"]/Total.Inland.Passenger.Transport

rm(Perctg.car.Passenger.Transport)
Perctg.car.Passenger.Transport<-DB.REGRESSION.SFA.2.Assess[,"OECD~TM~PT-Road passenger transport by passenger cars /-/ Passenger-kilometres"]/Total.Inland.Passenger.Transport

rm(Perctg.rail.Passenger.Transport)
Perctg.rail.Passenger.Transport<-DB.REGRESSION.SFA.2.Assess[,"OECD~TM~PT-Rail passenger transport /-/ Passenger-kilometres"]/Total.Inland.Passenger.Transport


DB.REGRESSION.SFA.2.Assess<-cbind(DB.REGRESSION.SFA.2.Assess,
                                  Total.Inland.Passenger.Transport,
                                  Perctg.bus.Passenger.Transport,
                                  Perctg.car.Passenger.Transport,
                                  Perctg.rail.Passenger.Transport)
View(DB.REGRESSION.SFA.2.Assess)


# We figure out a % distribution for the transport modes FOR THE WHOLE DATABASE


#DB.REGRESSION.SFA<-DB.REGRESSION.SFA[,c("Country","Year",Road.passenger.Vars, FINAL.Passenger.Rail.Vars )]
#colnames(DB.REGRESSION.SFA)

rm(TOTAL.Inland.Passenger.Transport)
TOTAL.Inland.Passenger.Transport<-rowSums(DB.REGRESSION.SFA[,c("OECD~TM~PT-Road passenger transport by buses and coaches /-/ Passenger-kilometres",
                                                                        "OECD~TM~PT-Road passenger transport by passenger cars /-/ Passenger-kilometres",
                                                                        "OECD~TM~PT-Rail passenger transport /-/ Passenger-kilometres")])
#rm(Perctg.BUS.Passenger.Transport)
#Perctg.BUS.Passenger.Transport<-DB.REGRESSION.SFA[,"OECD~TM~PT-Road passenger transport by buses and coaches /-/ Passenger-kilometres"]/Total.Inland.Passenger.Transport

# We use a Temp variable cause other way R has some problem with the operation
rm(Perctg.BUS.Passenger.Transport)
rm(Temp.Pass.BUS)
Temp.Pass.BUS<-DB.REGRESSION.SFA[,"OECD~TM~PT-Road passenger transport by buses and coaches /-/ Passenger-kilometres"]
Perctg.BUS.Passenger.Transport<-Temp.Pass.BUS/TOTAL.Inland.Passenger.Transport

rm(Perctg.CAR.Passenger.Transport)
rm(Temp.Pass.CARS)
Temp.Pass.CARS<-DB.REGRESSION.SFA[,"OECD~TM~PT-Road passenger transport by passenger cars /-/ Passenger-kilometres"]
Perctg.CAR.Passenger.Transport<-Temp.Pass.CARS/TOTAL.Inland.Passenger.Transport

rm(Perctg.RAIL.Passenger.Transport)
rm(Temp.Pass.RAIL)
Temp.Pass.RAIL<-DB.REGRESSION.SFA[,"OECD~TM~PT-Rail passenger transport /-/ Passenger-kilometres"]
Perctg.RAIL.Passenger.Transport<-Temp.Pass.RAIL/TOTAL.Inland.Passenger.Transport



# VERY IMPORTANT!!!!! It is necessary to create a new variable (DB.Passenger.Temp) and not add Perctg columns
# straightforward over the variable DB.REGRESSION.SFA. Other way there will be problems if we execute the
# cbind operation several times and we don't get aware
rm(DB.Passenger.Temp)
DB.Passenger.Temp<-DB.REGRESSION.SFA[,c("Country","Year",Road.passenger.Vars,
                       "OECD~TM~PT-Rail passenger transport /-/ Passenger-kilometres")]
DB.Passenger.Temp<-cbind(DB.Passenger.Temp,
                                  TOTAL.Inland.Passenger.Transport,
                                  Perctg.BUS.Passenger.Transport,
                                  Perctg.CAR.Passenger.Transport,
                                  Perctg.RAIL.Passenger.Transport)

# We check it out everythins is right

View(DB.Passenger.Temp)         

colnames(DB.Passenger.Temp)

DB.Passenger.Temp<-na.omit(DB.Passenger.Temp)
rm(Summary.Passenger.Data.Span)
Summary.Passenger.Data.Span<-describeBy(DB.Passenger.Temp$Year,
                                      group=droplevels(as.factor(DB.Passenger.Temp$Country)) ,mat=TRUE)
Summary.Passenger.Data.Span<-Summary.Passenger.Data.Span[,c("group1","n","min","max")]
colnames(Summary.Passenger.Data.Span)<-c("Country","n","Year min","Year max")
Summary.Passenger.Data.Span
dim(DB.Passenger.Temp)



#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# END: DATA SELECTION WITH INLAND PASSENGER TRANSPORT

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# BEGIN: DATA SELECTION WITH INLAND FREIGHT TRANSPORT

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# .................................................................................................
# Data with freight & Transport
# We assess this in order to figure out the composition of total inland freight transport
rm(Transport.freight.Vars)
Transport.freight.Vars<-names(DB.REGRESSION.SFA)[grepl("freight",names(DB.REGRESSION.SFA), ignore.case = TRUE) &
                                                 grepl("transport",names(DB.REGRESSION.SFA), ignore.case = TRUE) |
                                                   grepl("pipelines",names(DB.REGRESSION.SFA), ignore.case = TRUE) ]
Transport.freight.Vars
dim(na.omit(DB.REGRESSION.SFA[,c("Country","Year",Transport.freight.Vars)]))
View(na.omit(DB.REGRESSION.SFA[,c("Country","Year",Transport.freight.Vars)]))
View((DB.REGRESSION.SFA[,c("Country","Year",Transport.freight.Vars)]))

.............................................................................................
# Data with freight & road
Road.freight.Vars<-names(DB.REGRESSION.SFA)[grepl("freight",names(DB.REGRESSION.SFA), ignore.case = TRUE) &
                           grepl("road",names(DB.REGRESSION.SFA), ignore.case = TRUE)]
Road.freight.Vars

dim(na.omit(DB.REGRESSION.SFA[,c("Country","Year",Road.freight.Vars)]))
dim(na.omit(DB.REGRESSION.SFA[,c("Country","Year","OECD~TM~FT-Road freight transport /-/ Tonnes-kilometres")]))

View(na.omit(DB.REGRESSION.SFA[,c("Country","Year",Road.freight.Vars)]))
View((DB.REGRESSION.SFA[,c("Country","Year",Road.freight.Vars)]))


# .................................................................................................
# Data with freight & rail
Rail.freight.Vars<-names(DB.REGRESSION.SFA)[grepl("freight",names(DB.REGRESSION.SFA), ignore.case = TRUE) &
                           grepl("rail",names(DB.REGRESSION.SFA), ignore.case = TRUE)&
                           grepl("transport",names(DB.REGRESSION.SFA), ignore.case = TRUE) ]
Rail.freight.Vars
dim(na.omit(DB.REGRESSION.SFA[,c("Country","Year",Rail.freight.Vars)]))
View(na.omit(DB.REGRESSION.SFA[,c("Country","Year",Rail.freight.Vars)]))

# .................................................................................................
# Data with freight & Water
rm(Water.freight.Vars)
Water.freight.Vars<-names(DB.REGRESSION.SFA)[grepl("freight",names(DB.REGRESSION.SFA), ignore.case = TRUE)&
                                               grepl("water",names(DB.REGRESSION.SFA), ignore.case = TRUE)]

Water.freight.Vars
View((DB.REGRESSION.SFA[,c("Country","Year",Water.freight.Vars)]))


# We assign value 0 to Water freight with NAs cause we assume its value is 0
rm(Temp.DB.REGRESSION.SFA)
Temp.DB.REGRESSION.SFA<-DB.REGRESSION.SFA
# https://stackoverflow.com/questions/19379081/how-to-replace-na-values-in-a-table-for-selected-columns
Temp.DB.REGRESSION.SFA[Water.freight.Vars][is.na(Temp.DB.REGRESSION.SFA[Water.freight.Vars])] <- 0
# We check it out NAs changes have been implemnted properly
diffdf::diffdf(Temp.DB.REGRESSION.SFA,DB.REGRESSION.SFA)

View((Temp.DB.REGRESSION.SFA[,c("Country","Year",Water.freight.Vars)]))

# .................................................................................................
# Data with Pipelines

rm(Pipelines.freight.Vars)
Pipelines.freight.Vars<-names(DB.REGRESSION.SFA)[grepl("Pipelines",names(DB.REGRESSION.SFA), ignore.case = TRUE)]

Pipelines.freight.Vars
View((DB.REGRESSION.SFA[,c("Country","Year",Pipelines.freight.Vars)]))


Temp.DB.REGRESSION.SFA[Pipelines.freight.Vars][is.na(Temp.DB.REGRESSION.SFA[Pipelines.freight.Vars])] <- 0
# We check it out NAs changes have been implemnted properly
diffdf::diffdf(Temp.DB.REGRESSION.SFA,DB.REGRESSION.SFA)

View((Temp.DB.REGRESSION.SFA[,c("Country","Year",Pipelines.freight.Vars)]))


# .................................................................................................


# .................................................................................................
# DATA ASSESSMENT
# .................................................................................................
# We assess which variables use as to obtain the Total inland freight and why differences may accrue
# between the Total value of the OECD data and the one obtained by adding variables
rm(TOTAL.Inland.Freight)
TOTAL.Inland.Freight<-rowSums(Temp.DB.REGRESSION.SFA[,c("OECD~TM~FT-Inland waterways freight transport /-/ Tonnes-kilometres",
                                                   "OECD~TM~FT-Rail freight transport /-/ Tonnes-kilometres",
                                                   "OECD~TM~FT-Road freight transport /-/ Tonnes-kilometres",
                                                   "OECD~TM~FT-Pipelines transport /-/ Tonnes-kilometres")])

rm(DB.REGRESSION.SFA.Freight)
DB.REGRESSION.SFA.Freight<-Temp.DB.REGRESSION.SFA[,c("Country","Year",Transport.freight.Vars, 
                                                Rail.freight.Vars, Road.freight.Vars,Water.freight.Vars,
                                                 Pipelines.freight.Vars)]
colnames(DB.REGRESSION.SFA.Freight)
rm(Diferent.Total.Freight)
Diferent.Total.Freight<-ifelse(DB.REGRESSION.SFA.Freight[,9]==TOTAL.Inland.Freight,"True","False")
rm(DB.Freight.Temp)
DB.Freight.Temp<-(add_column(DB.REGRESSION.SFA.Freight, TOTAL.Inland.Freight, .after = 8))

dim(DB.Freight.Temp[which(Diferent.Total.Freight=="False"),])
dim(DB.Freight.Temp[which(Diferent.Total.Freight=="True"),])

# After assessing the differences between total freight data obtained by us and total freight data
# provided by OECD we conclude that differences are for omission on adding pipeline freight on a few
# data. We will use 
 View(DB.Freight.Temp[which(Diferent.Total.Freight=="False"),])

View(na.omit(DB.Freight.Temp[,c(1:6,9:10)]))
dim(na.omit(DB.Freight.Temp[,c(1:6,9:10)]))
rm(DB.Freight.Temp.Span)
DB.Freight.Temp.Span<-na.omit(DB.Freight.Temp[,c(1:6,9:10)])

rm(Summary.Freight.Data.Span)
Summary.Freight.Data.Span<-describeBy(DB.Freight.Temp.Span$Year,
                              group=droplevels(as.factor(DB.Freight.Temp.Span$Country)) ,mat=TRUE)
Summary.Freight.Data.Span<-Summary.Freight.Data.Span[,c("group1","n","min","max")]
colnames(Summary.Freight.Data.Span)<-c("Country","n","Year min","Year max")
Summary.Freight.Data.Span
dim(DB.Freight.Temp.Span)

# .................................................................................................
# % Percentage distribution for  freight: Road; rail; Water; Pipeline
# .................................................................................................

colnames(DB.Freight.Temp[,c(1:6,9:10)])
rm(TOTAL.Inland.Freight)

View(DB.Freight.Temp[,c("Country","Year", "TOTAL.Inland.Freight",
                               "OECD~TM~FT-Road freight transport /-/ Tonnes-kilometres",
                               "OECD~TM~FT-Rail freight transport /-/ Tonnes-kilometres",
                               "OECD~TM~FT-Inland waterways freight transport /-/ Tonnes-kilometres",
                               "OECD~TM~FT-Pipelines transport /-/ Tonnes-kilometres")])


# We use a Temp variable cause other way R has some problem with the operation
rm(Perctg.ROAD.Freight)
rm(Temp.ROAD.Freight)
Temp.ROAD.Freight<-DB.Freight.Temp[, "OECD~TM~FT-Road freight transport /-/ Tonnes-kilometres"]
Perctg.ROAD.Freight<-Temp.ROAD.Freight/TOTAL.Inland.Freight

rm(Perctg.RAIL.Freight)
rm(Temp.RAIL.Freight)
Temp.RAIL.Freight<-DB.Freight.Temp[,"OECD~TM~FT-Rail freight transport /-/ Tonnes-kilometres"]
Perctg.RAIL.Freight<-Temp.RAIL.Freight/TOTAL.Inland.Freight

rm(Perctg.WATER.Freight)
rm(Temp.WATER.Freight)
Temp.WATER.Freight<-DB.Freight.Temp[,"OECD~TM~FT-Inland waterways freight transport /-/ Tonnes-kilometres"]
Perctg.WATER.Freight<-Temp.WATER.Freight/TOTAL.Inland.Freight

rm(Perctg.PIPELINES.Freight)
rm(Temp.PIPELINES.Freight)
Temp.PIPELINES.Freight<-DB.Freight.Temp[,"OECD~TM~FT-Pipelines transport /-/ Tonnes-kilometres"]
Perctg.PIPELINES.Freight<-Temp.PIPELINES.Freight/TOTAL.Inland.Freight

colnames(DB.Freight.Temp)
DB.Freight.Temp<-cbind(DB.Freight.Temp,
                      Perctg.ROAD.Freight,
                      Perctg.RAIL.Freight,
                      Perctg.WATER.Freight,
                      Perctg.PIPELINES.Freight)


DB.Freight.Temp<-DB.Freight.Temp[,c("Country","Year",
                              "TOTAL.Inland.Freight",
                              "OECD~TM~FT-Inland waterways freight transport /-/ Tonnes-kilometres",
                              "OECD~TM~FT-Rail freight transport /-/ Tonnes-kilometres",
                              "OECD~TM~FT-Road freight transport /-/ Tonnes-kilometres",
                              "OECD~TM~FT-Pipelines transport /-/ Tonnes-kilometres",
                              "OECD~TM~FT-Total inland freight transport /-/ Tonnes-kilometres",
                              "Perctg.ROAD.Freight",
                              "Perctg.RAIL.Freight",
                              "Perctg.WATER.Freight",
                              "Perctg.PIPELINES.Freight")]

colnames(DB.Freight.Temp)
# We check it out everythins is right

View(DB.Freight.Temp)

DB.Freight.Temp<-(na.omit(DB.Freight.Temp))
dim(DB.Freight.Temp)

# FALTA POR ARREGLAR LOS NAS DE LAS VARIABLES CON WATER Y PIPEPINES
# ES NECESARIO REALIZAR UN ESTUDIO DE LOS NAs DE PIPELINES




#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# END: INLAND FREIGHT TRANSPORT DATA SELECTION 

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# VERY IMPORTANT!!!!! It is necessary to create a new variable (DB.TRANSPORT.SFA.END) and not add Perctg columns
# straightforward over the variable DB.REGRESSION.SFA. Other way there will be problems if we execute the
# cbind operation several times and we don't get aware

# We merge passenger and freight transport data
rm(DB.TRANSPORT.SFA.END)
DB.TRANSPORT.SFA.END<-merge(DB.Passenger.Temp, DB.Freight.Temp,  by=c("Country","Year"), all=TRUE)
colnames(DB.TRANSPORT.SFA.END)
View(DB.TRANSPORT.SFA.END)
dim(DB.TRANSPORT.SFA.END)
dim(na.omit(DB.TRANSPORT.SFA.END))
View(na.omit(DB.TRANSPORT.SFA.END))
DB.TRANSPORT.SFA.END<-na.omit(DB.TRANSPORT.SFA.END)
dim(DB.TRANSPORT.SFA.END)


rm(Summary.TRANSPORT.Data.Span)
Summary.TRANSPORT.Data.Span<-describeBy(DB.TRANSPORT.SFA.END$Year,
                                            group=droplevels(as.factor(DB.TRANSPORT.SFA.END$Country)) ,mat=TRUE)
Summary.TRANSPORT.Data.Span<-Summary.TRANSPORT.Data.Span[,c("group1","n","min","max")]
colnames(Summary.TRANSPORT.Data.Span)<-c("Country","n","Year min","Year max")
Summary.TRANSPORT.Data.Span

Summary.Passenger.Data.Span
Summary.Freight.Data.Span


#Theres 

rm(Transport.Variables.Assessment)
Transport.Variables.Assessment<-colnames(DB.TRANSPORT.SFA.END)[c(1:6,12:16)]

View((DB.REGRESSION.SFA[,c("Country","Year",Transport.Variables.Assessment)]))








#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# BEGIN: DATA SELECTION WITH MACROECONOMIC AGGREGATES (GDP)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# .................................................................................................
# Data with GDP
#GDP.Vars<-names(DB.REGRESSION.SFA)[grepl("OECD",names(DB.REGRESSION.SFA), ignore.case = TRUE) &
#                                             grepl("GDP",names(DB.REGRESSION.SFA), ignore.case = TRUE) & 
#                                              grepl("PPP",names(DB.REGRESSION.SFA), ignore.case = TRUE) &
#                                              grepl("constant",names(DB.REGRESSION.SFA), ignore.case = TRUE)]


GDP.Vars<-c(#"WB~WDI-GDP, PPP (constant 2017 international $)",
            
  
            "OECD~NA~NAG-GDP at 2015 constant prices and PPPs, billions US dollars //*//-GDPVPVOB",
            "OECD~NA~NAG-Actual individual consumption, at 2015 prices and PPPs, billions US dollars //*//-P41VPVOB",
            "OECD~NA~NAG-GDP per capita, at constant 2015 prices and PPPs, US dollars //*//-GDPHVPVOB",
            #"WB~WDI-GDP per capita, PPP (constant 2017 international $)",
            #"WB~WDI-GDP per capita (constant 2010 US$)",
            
            "OECD~NA~NAG-Actual individual consumption, percentage of GDP //*//-P41S",
            "OECD~NA~NAG-Household final consumption expenditure, percentage of GDP //*//-P31S14_S15S",
            
            #"WB~WDI-Services, value added (% of GDP)",
            #"WB~WDI-Agriculture, forestry, and fishing, value added (% of GDP)",
            #"WB~WDI-Industry (including construction), value added (% of GDP)",
            #"WB~WDI-Manufacturing, value added (% of GDP)",
            #"WB~WDI-Research and development expenditure (% of GDP)",
            
            "WB~WDI-GDP growth (annual %)",
           
             # We remove "OECD- % Growth GDP" cause "WB~WDI-GDP growth (annual %)" provides a few more data
            #"OECD- % Growth GDP",
            #"WB~WDI-GDP per capita growth (annual %)",
            
            #"WB~WDI-GDP per unit of energy use (constant 2017 PPP $ per kg of oil equivalent)",
            
            "WB~WDI-Population, total",    
            "WB~WDI-Urban population (% of total population)",
            "WB~WDI-Urban population growth (annual %)",
            "WB~WDI-Population in the largest city (% of urban population)",
            "WB~WDI-Population in urban agglomerations of more than 1 million (% of total population)",
            "WB~WDI-Population density (people per sq. km of land area)"
             
            )

# Es deseable encontrar información con series más largas de la distribución % del GDP. Consultar datos OECD


GDP.Vars
dim(na.omit(DB.REGRESSION.SFA[,c("Country","Year",GDP.Vars)]))
View(na.omit(DB.REGRESSION.SFA[,c("Country","Year",GDP.Vars)]))
View((DB.REGRESSION.SFA[,c("Country","Year",GDP.Vars)]))

# For assessing individual variables
dim(na.omit(DB.REGRESSION.SFA[,c("Country","Year",GDP.Vars[1])]))
View(na.omit(DB.REGRESSION.SFA[,c("Country","Year",GDP.Vars[1])]))


# We assess data available
rm(DB.MACROAggretes.SFA.END)
DB.MACROAggretes.SFA.END<-na.omit(DB.REGRESSION.SFA[,c("Country","Year",GDP.Vars)])
View(DB.MACROAggretes.SFA.END)

rm(Summary.MACROAggretes.Data.Span)
Summary.MACROAggretes.Data.Span<-describeBy(DB.MACROAggretes.SFA.END$Year,
                                         group=droplevels(as.factor(DB.MACROAggretes.SFA.END$Country)) ,mat=TRUE)
Summary.MACROAggretes.Data.Span<-Summary.MACROAggretes.Data.Span[,c("group1","n","min","max")]
colnames(Summary.MACROAggretes.Data.Span)<-c("Country","n","Year min","Year max")
Summary.MACROAggretes.Data.Span

dim(DB.MACROAggretes.SFA.END)
levels(droplevels(as.factor(DB.MACROAggretes.SFA.END$Country)))
length(levels(droplevels(as.factor(DB.MACROAggretes.SFA.END$Country))))

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# END: DATA SELECTION WITH MACROECONOMIC AGGREGATES (GDP)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# We assess Transport Data with Macro Aggreates

#DB.Passenger.Temp
#DB.TRANSPORT.SFA.END
#DB.Freight.Temp

rm(DB.REGRESSION.SFA.END)
DB.REGRESSION.SFA.END<-merge(DB.TRANSPORT.SFA.END, DB.MACROAggretes.SFA.END,  by=c("Country","Year"), all=TRUE)
dim(DB.REGRESSION.SFA.END)
dim(na.omit(DB.REGRESSION.SFA.END))
View(DB.REGRESSION.SFA.END)

rm(DB.REGRESSION.SFA.END.Temp)
DB.REGRESSION.SFA.END.Temp<-na.omit(DB.REGRESSION.SFA.END)


rm(Summary.Regression.Data.Span)
Summary.Regression.Data.Span<-describeBy(DB.REGRESSION.SFA.END.Temp$Year,
                                         group=droplevels(as.factor(DB.REGRESSION.SFA.END.Temp$Country)) ,mat=TRUE)
Summary.Regression.Data.Span<-Summary.Regression.Data.Span[,c("group1","n","min","max")]
colnames(Summary.Regression.Data.Span)<-c("Country","n","Year min","Year max")
Summary.Regression.Data.Span
dim(DB.REGRESSION.SFA.END.Temp)

#levels(as.factor(DB.REGRESSION.SFA.END.Temp$Country))
levels(droplevels(as.factor(DB.REGRESSION.SFA.END.Temp$Country)))
length(levels(droplevels(as.factor(DB.REGRESSION.SFA.END.Temp$Country))))


colnames(DB.REGRESSION.SFA.END)

#CORRELATION ASSESSMENT
formattable(cor((na.omit(DB.REGRESSION.SFA.END.Temp[,c(3:7,11)]))))

formattable(cor((na.omit(DB.REGRESSION.SFA.END.Temp[,c(7,11)]))))

formattable(cor((na.omit(DB.REGRESSION.SFA.END.Temp[,c(7,11,21)]))))

# Con datos GDP per capita la correlación no es tan grande
formattable(cor((na.omit(DB.REGRESSION.SFA.END.Temp[,c(7,11,21)]))))

# Correlación valores absolutos transporte
formattable(cor((na.omit(DB.REGRESSION.SFA.END.Temp[,c(13,14,21)]))))
# Correlación porcentajes transportes
formattable(cor((na.omit(DB.REGRESSION.SFA.END.Temp[,c(17,18,21)]))))
formattable(cor((na.omit(DB.REGRESSION.SFA.END.Temp[,c(17,18,22)]))))

formattable(cor((na.omit(DB.REGRESSION.SFA.END.Temp[,c(21:24)]))))

View(DB.REGRESSION.SFA.END)
colnames(DB.REGRESSION.SFA.END)

View(DB.REGRESSION.SFA.END.Temp[,c(1,2,21,22,27)])

# Comparamos datos per capita OECD y manuales
rm(df1)
df1<-DB.REGRESSION.SFA.END.Temp
View(cbind(df1[,c(1,2,21,22,27)],df1[,21]*1000000000/df1[,27]))


Summary.Passenger.Data.Span
Summary.Freight.Data.Span
Summary.MACROAggretes.Data.Span
dim(DB.TRANSPORT.SFA.END)
Summary.Regression.Data.Span
dim(DB.REGRESSION.SFA.END.Temp)

Countries.TRANSPORT<-levels(droplevels(as.factor(DB.TRANSPORT.SFA.END[,"Country"])))
Contries.MACRO<-levels(droplevels(as.factor(DB.MACROAggretes.SFA.END$Country)))
intersect(Countries.TRANSPORT,Contries.MACRO)



View(BBDD_WB_WDI[, c(1,2,grep("GDP", ignore.case=TRUE, colnames(BBDD_WB_WDI)))])



# We assess available data for "WB~WDI-GDP, PPP (constant 2017 international $)"
dim(na.omit(BBDD_WB_WDI[,c(1,2,42)]))

rm(df.WDI)
df.WDI<-na.omit(BBDD_WB_WDI[,c(1,2,42)])

Countries.WDI.Macro<-levels(droplevels(as.factor(df.WDI[,"Country"])))
View(df.WDI)

intersect(Countries.TRANSPORT,Countries.WDI.Macro)
Countries.TRANSPORT[which(Countries.TRANSPORT %in% Countries.WDI.Macro)]


# Energy variables preselection
rm(DB.Temp.Energy.Prices)
DB.Temp.Energy.Prices<-DB.REGRESSION.SFA[,c("Country", "Year","Total Road Fuel Consumption (kt)",
                     "OECD-Automotive diesel (litre)-Households-Total price (USD/unit using PPP)")]
DB.Temp.Energy.Prices<-na.omit(DB.Temp.Energy.Prices)
dim(DB.Temp.Energy.Prices)
View(DB.Temp.Energy.Prices)


rm(Summary.ENERGY.Span)
Summary.ENERGY.Span<-describeBy(DB.Temp.Energy.Prices$Year,
                                         group=droplevels(as.factor(DB.Temp.Energy.Prices$Country)) ,mat=TRUE)
Summary.ENERGY.Span<-Summary.ENERGY.Span[,c("group1","n","min","max")]
colnames(Summary.ENERGY.Span)<-c("Country","n","Year min","Year max")
Summary.ENERGY.Span
dim(DB.Temp.Energy.Prices)


rm(Countries.ENERGY)
Countries.ENERGY<-levels(droplevels(as.factor(DB.Temp.Energy.Prices$Country)))
length(Countries.ENERGY)



# FINAL Total data with Energy Prices and Fuel Consumption
rm(DB.Temp.TOTAL_DATA)
DB.Temp.TOTAL_DATA<-merge(DB.Temp.Energy.Prices, DB.REGRESSION.SFA.END.Temp,  by=c("Country","Year"), all=TRUE)
DB.Temp.TOTAL_DATA<-na.omit(DB.Temp.TOTAL_DATA)
dim(DB.Temp.TOTAL_DATA)
colnames(DB.Temp.TOTAL_DATA)
View(DB.Temp.TOTAL_DATA)

rm(Summary.TOTAL_DATA.Span)
Summary.TOTAL_DATA.Span<-describeBy(DB.Temp.TOTAL_DATA$Year,
                                group=droplevels(as.factor(DB.Temp.TOTAL_DATA$Country)) ,mat=TRUE)
Summary.TOTAL_DATA.Span<-Summary.TOTAL_DATA.Span[,c("group1","n","min","max")]
colnames(Summary.TOTAL_DATA.Span)<-c("Country","n","Year min","Year max")
Summary.TOTAL_DATA.Span
dim(DB.Temp.TOTAL_DATA)

# If we remove Countries with incomplete data
COUNTRIES.NOT.2.Asses<-c("Austria", "Canada", "Belgium")
dim(DB.Temp.TOTAL_DATA[which( !(DB.Temp.TOTAL_DATA[,"Country"] %in% COUNTRIES.NOT.2.Asses) ),])


Countries.TOTAL_DATA<-levels(droplevels(as.factor(DB.Temp.TOTAL_DATA$Country)))
length(Countries.TOTAL_DATA)

length(Countries.TRANSPORT)
Countries.TOTAL_DATA[which(Countries.TOTAL_DATA %in% Countries.TRANSPORT)]
Countries.TOTAL_DATA[which(Countries.TOTAL_DATA %in% Countries.ENERGY)]
Countries.ENERGY[which(!(Countries.ENERGY %in% Countries.TOTAL_DATA))]

#https://www.marsja.se/how-to-use-in-in-r/
Countries.TRANSPORT[which((Countries.TRANSPORT %in% Countries.ENERGY))]
Countries.TRANSPORT[which(!(Countries.TRANSPORT %in% Countries.ENERGY))]




# DATA SPAN ALL VARIABLES

Summary.Passenger.Data.Span
dim(DB.Passenger.Temp)

Summary.Freight.Data.Span
dim(na.omit(DB.Freight.Temp))

Summary.TRANSPORT.Data.Span
dim(DB.TRANSPORT.SFA.END)


Summary.MACROAggretes.Data.Span
dim(DB.MACROAggretes.SFA.END)


Summary.Regression.Data.Span
dim(DB.REGRESSION.SFA.END.Temp)

Summary.ENERGY.Span
dim(DB.Temp.Energy.Prices)
View(DB.Temp.Energy.Prices)
levels(droplevels(as.factor(DB.Temp.Energy.Prices[,"Country"])))


Summary.TOTAL_DATA.Span
dim(DB.Temp.TOTAL_DATA)
View(DB.Temp.TOTAL_DATA)


#........................
rm(A.Energy.Transport)
A.Energy.Transport<-merge(DB.Temp.Energy.Prices, DB.TRANSPORT.SFA.END,  by=c("Country","Year"), all=TRUE)
A.Energy.Transport<-(na.omit(A.Energy.Transport))
View(A.Energy.Transport)
levels(droplevels(as.factor(A.Energy.Transport[,"Country"])))


rm(A.Energy.Transport.MacroAgg)
A.Energy.Transport.MacroAgg<-merge(DB.MACROAggretes.SFA.END, A.Energy.Transport,  by=c("Country","Year"), all=TRUE)
A.Energy.Transport.MacroAgg<-(na.omit(A.Energy.Transport.MacroAgg))
dim(A.Energy.Transport.MacroAgg)
levels(droplevels(as.factor(A.Energy.Transport.MacroAgg[,"Country"])))


# Data Span Summary 
rm(Summary.A.Energy.Transport.MacroAgg)
Summary.A.Energy.Transport.MacroAgg<-describeBy(A.Energy.Transport.MacroAgg$Year,
                                         group=droplevels(as.factor(A.Energy.Transport.MacroAgg$Country)) ,mat=TRUE)
Summary.A.Energy.Transport.MacroAgg<-Summary.A.Energy.Transport.MacroAgg[,c("group1","n","min","max")]
colnames(Summary.A.Energy.Transport.MacroAgg)<-c("Country","n","Year min","Year max")
Summary.A.Energy.Transport.MacroAgg
dim(A.Energy.Transport.MacroAgg)

# Latex Output
xtable(Summary.A.Energy.Transport.MacroAgg)


# .................................................................................
# PRUEBA DE PANEL DATA
#https://www.econometrics-with-r.org/10-rwpd.html




# https://gdemin.github.io/expss/

A.Energy.Transport.MacroAgg$Total.Fuel.GDP<-with(A.Energy.Transport.MacroAgg, Total.Fuel.GDP<-Total.Fuel/GDP)
colnames(A.Energy.Transport.MacroAgg)

COLNAMES.Modification<-c("Country",
  "Year",
  "GDP", 
  "GDPxCapita",
  "Perctg.Act.ind.consum",
  "Perctg.Household.final.consum",
  "GDP.Growth",
  "GDP.Growth.x.Capita",
  "Population",
  "Perctg.Urban.Pop",
  "Urban.Pop.Growth",
  "Pop.Largest.City",
  "Pop.Urban.Agglo",
  "Pop.Density",
  "Total.Fuel",
  "Price.Diesel",
  "Road.Passenger",
  "Road.Passenger.Buses",
  "Road.Passenger.Cars",
  "Rail.Passenger",
  "Total.Inland.Passenger",
  "Perctg.BUS.Passenger",
  "Perctg.CAR.Passenger",
  "Perctg.RAIL.Passenger",
  "Total.Inland.Freight",
  "Water.Freight",
  "Rail.Freight",
  "Road.Freight",
  "Pipelines.Transport",
  "Total.Inland.Transport",
  "Perctg.ROAD.Freight",
  "Perctg.RAIL.Freight",
  "Perctg.WATER.Freight",
  "Perctg.PIPELINES.Freight",
  "Total.Fuel.GDP"
  )

colnames(A.Energy.Transport.MacroAgg) <- COLNAMES.Modification

# https://gdemin.github.io/expss/
# https://cran.r-project.org/web/packages/expss/vignettes/labels-support.html
#https://rpubs.com/JavidelaCarrera/R-VignetteSTDS

A.Energy.Transport.MacroAgg = apply_labels(A.Energy.Transport.MacroAgg,
                       Country = "Country",
                       Year = "Year", 
                       GDP =  "OECD~NA~NAG-GDP at 2015 constant prices and PPPs, billions US dollars //*//-GDPVPVOB",
                       GDPxCapita = "OECD~NA~NAG-GDP per capita, at constant 2015 prices and PPPs, US dollars //*//-GDPHVPVOB",
                       Perctg.Act.ind.consum = "OECD~NA~NAG-Actual individual consumption, percentage of GDP //*//-P41S",
                       Perctg.Household.final.consum = "OECD~NA~NAG-Household final consumption expenditure, percentage of GDP //*//-P31S14_S15S",
                       GDP.Growth = "WB~WDI-GDP growth (annual %)",
                       GDP.Growth.x.Capita = "WB~WDI-GDP per capita growth (annual %)",
                       Population = "WB~WDI-Population, total",
                       Perctg.Urban.Pop =  "WB~WDI-Urban population (% of total population)",
                       Urban.Pop.Growth = "WB~WDI-Urban population growth (annual %)",
                       Pop.Largest.City = "WB~WDI-Population in the largest city (% of urban population)",
                       Pop.Urban.Agglo = "WB~WDI-Population in urban agglomerations of more than 1 million (% of total population)",
                       Pop.Density = "WB~WDI-Population density (people per sq. km of land area)",
                       Total.Fuel = "Total Road Fuel Consumption (kt)",
                       Price.Diesel = "OECD-Automotive diesel (litre)-Households-Total price (USD/unit using PPP)",
                       Road.Passenger = "OECD~TM~PT-Road passenger transport /-/ Passenger-kilometres",
                       Road.Passenger.Buses = "OECD~TM~PT-Road passenger transport by buses and coaches /-/ Passenger-kilometres",
                       Road.Passenger.Cars = "OECD~TM~PT-Road passenger transport by passenger cars /-/ Passenger-kilometres",
                       Rail.Passenger = "OECD~TM~PT-Rail passenger transport /-/ Passenger-kilometres",
                       Total.Inland.Passenger = "TOTAL.Inland.Passenger.Transport",
                       Perctg.BUS.Passenger = "Perctg.BUS.Passenger.Transport",
                       Perctg.CAR.Passenger = "Perctg.CAR.Passenger.Transport",
                       Perctg.RAIL.Passenger = "Perctg.RAIL.Passenger.Transport",
                       Total.Inland.Freight = "TOTAL.Inland.Freight",
                       Water.Freight = "OECD~TM~FT-Inland waterways freight transport /-/ Tonnes-kilometres",
                       Rail.Freight = "OECD~TM~FT-Rail freight transport /-/ Tonnes-kilometres",
                       Road.Freight = "OECD~TM~FT-Road freight transport /-/ Tonnes-kilometres",
                       Pipelines.Transport = "OECD~TM~FT-Pipelines transport /-/ Tonnes-kilometres",
                       Total.Inland.Transport = "OECD~TM~FT-Total inland freight transport /-/ Tonnes-kilometres",
                       Perctg.ROAD.Freight = "Perctg.ROAD.Freight",
                       Perctg.RAIL.Freight = "Perctg.RAIL.Freight",
                       Perctg.WATER.Freight = "Perctg.WATER.Freight",
                       Perctg.PIPELINES.Freight = "Perctg.PIPELINES.Freight",
                       Total.Fuel.GDP = "Total Fuel / GDP"
)



View(A.Energy.Transport.MacroAgg[,c("Country", "Year","Total.Fuel","GDP","Total.Fuel.GDP", "Total.Inland.Freight","Total.Inland.Transport",
                                    "Perctg.RAIL.Freight","Perctg.ROAD.Freight")])


colnames(A.Energy.Transport.MacroAgg) 

str(A.Energy.Transport.MacroAgg) 
var_lab(A.Energy.Transport.MacroAgg)

rm(PD.TOTAL.Data)
PD.TOTAL.Data <- pdata.frame(A.Energy.Transport.MacroAgg, index=c("Country", "Year"))
pdim(PD.TOTAL.Data)
dim(PD.TOTAL.Data)


#...................................................................................
# BEGIN: Correlation Assessment
#...................................................................................
colnames(PD.TOTAL.Data)
formattable(cor((na.omit(PD.TOTAL.Data[,c("Total.Inland.Freight","GDPxCapita")]))))
formattable(cor((na.omit(PD.TOTAL.Data[,c("Pop.Urban.Agglo","Pop.Density")]))))
formattable(cor((na.omit(PD.TOTAL.Data[,c("GDPxCapita","Total.Inland.Passenger")]))))
formattable(cor((na.omit(PD.TOTAL.Data[,c("GDPxCapita","Total.Inland.Freight")]))))
formattable(cor((na.omit(PD.TOTAL.Data[,c("Population","Total.Inland.Freight")]))))
formattable(cor((na.omit(PD.TOTAL.Data[,c("GDPxCapita","Price.Diesel")]))))
formattable(cor((na.omit(PD.TOTAL.Data[,c("GDP","Perctg.ROAD.Freight")]))))

colnames(A.Energy.Transport.MacroAgg)
VARS.Corr.Assessment<-c("GDP","Total.Inland.Freight","Total.Inland.Passenger",
                        "Price.Diesel","Total.Fuel",
                        "GDP.Growth","GDP.Growth.x.Capita","Perctg.Household.final.consum",
                        "Perctg.Act.ind.consum",
                        "Pop.Urban.Agglo","Pop.Largest.City","Pop.Density",
                        "Perctg.RAIL.Passenger","Perctg.BUS.Passenger","Perctg.CAR.Passenger",
                        "Perctg.RAIL.Freight","Perctg.ROAD.Freight","Perctg.WATER.Freight","Perctg.PIPELINES.Freight")
                                                
formattable(cor((A.Energy.Transport.MacroAgg[,VARS.Corr.Assessment]),method = c("spearman")))
formattable(cor((A.Energy.Transport.MacroAgg[,VARS.Corr.Assessment]),method = c("pearson")))
Correlation.Matrix.Regressors<-formattable(cor((A.Energy.Transport.MacroAgg[,VARS.Corr.Assessment]),method = c("pearson")))



source("C:/ALBERTO/3-DOCTORADO/DATOS/1 - PAPER/FICHEROS R/rquery_cormat.R")

#source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(A.Energy.Transport.MacroAgg[,VARS.Corr.Assessment])
rquery.cormat(A.Energy.Transport.MacroAgg[,VARS.Corr.Assessment])
rquery.cormat(A.Energy.Transport.MacroAgg[,3:dim(A.Energy.Transport.MacroAgg)[2]])


rcorr(A.Energy.Transport.MacroAgg[,VARS.Corr.Assessment])

# LaTeX output
xtable(Correlation.Matrix.Regressors)

View(A.Energy.Transport.MacroAgg[,c("Country","Year",VARS.Corr.Assessment)])
dim(A.Energy.Transport.MacroAgg[,c("Country","Year",VARS.Corr.Assessment)])
dim(na.omit(A.Energy.Transport.MacroAgg[,c("Country","Year",VARS.Corr.Assessment)]))


#.....................

VARS.Formula1<-c("Total.Fuel", "Total.Fuel.GDP", "GDP", "Price.Diesel",
                 "GDP.Growth", "GDP.Growth.x.Capita",
                 "Perctg.RAIL.Passenger", "Perctg.BUS.Passenger", "Perctg.RAIL.Freight",
                 "Perctg.Household.final.consum", "Perctg.Act.ind.consum", 
                 "Pop.Urban.Agglo", "Pop.Largest.City", "Pop.Density")

rquery.cormat(A.Energy.Transport.MacroAgg[,VARS.Formula1])
# https://www.displayr.com/how-to-create-a-correlation-matrix-in-r/
# Correlation.Matrix.Regressors<-rcorr(as.matrix(A.Energy.Transport.MacroAgg[,VARS.Corr.Assessment]))
# https://rstudio-pubs-static.s3.amazonaws.com/240657_5157ff98e8204c358b2118fa69162e18.html
Correlation.Matrix.Regressors<-rcorr(as.matrix(A.Energy.Transport.MacroAgg[,VARS.Corr.Assessment]))
Correlation.Matrix.Regressors$r
Correlation.Matrix.Regressors<-round(Correlation.Matrix.Regressors$r,2)
xtable(round(Correlation.Matrix.Regressors$r,3))

View(A.Energy.Transport.MacroAgg[which(A.Energy.Transport.MacroAgg[,"Perctg.PIPELINES.Freight" ]==0),])


# https://rstudio-pubs-static.s3.amazonaws.com/240657_5157ff98e8204c358b2118fa69162e18.html
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(Correlation.Matrix.Regressors, method = "color", col = col(200),  
         type = "upper", order = "hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "darkblue", tl.srt = 45, #Text label color and rotation
         # Combine with significance level
         p.mat = p_mat, sig.level = 0.01,  
         # hide correlation coefficient on the principal diagonal
         diag = FALSE 
)
# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
corrplot(Correlation.Matrix.Regressors,  method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 0.8, order = 'AOE', diag=FALSE)

#...................................................................................
# END: Correlation Assessment
#...................................................................................

2182+2509+2032+1000+400



# https://cmdlinetips.com/2019/02/how-to-make-grouped-boxplots-with-ggplot2/
# https://stackoverflow.com/questions/13297995/changing-font-size-and-direction-of-axes-text-in-ggplot2

#...................................................................................
# BEGIN: PLOT Assessment
#...................................................................................

ggplot(A.Energy.Transport.MacroAgg, aes(x=Country,y=Perctg.CAR.Passenger,  fill=Country)) +
  geom_boxplot() + geom_jitter(width=0.1,alpha=0.2) +
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle=60, hjust=1))+
  ggtitle("% CAR PASSENGER")+
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="bottom",
                                                        legend.box="horizontal")

ggplot(A.Energy.Transport.MacroAgg, aes(x=Country,y=Perctg.RAIL.Passenger,  fill=Country)) +
  geom_boxplot() + geom_jitter(width=0.1,alpha=0.2) +
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle=60, hjust=1))+
        ggtitle("% RAIL PASSENGER")+
        theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position="bottom",
                                                             legend.box="horizontal")

ggplot(A.Energy.Transport.MacroAgg, aes(x=Country,y=Perctg.BUS.Passenger, fill=Country)) +
  geom_boxplot() + geom_jitter(width=0.1,alpha=0.2) +
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle=60, hjust=1))+
        ggtitle("% BUS PASSENGER")+
        theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position="bottom",
                                                             legend.box="horizontal")


ggplot(A.Energy.Transport.MacroAgg, aes(x=Country,y=Perctg.ROAD.Freight, fill=Country)) +
  geom_boxplot() + geom_jitter(width=0.1,alpha=0.2) +
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle=60, hjust=1))+
  ggtitle("% ROAD FREIGHT")+
  theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position="bottom",
                                                       legend.box="horizontal")



ggplot(A.Energy.Transport.MacroAgg, aes(x=Country,y=Perctg.RAIL.Freight, fill=Country)) +
  geom_boxplot() + geom_jitter(width=0.1,alpha=0.2) +
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle=60, hjust=1))+
        ggtitle("% RAIL FREIGHT")+
        theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position="bottom",
                                                             legend.box="horizontal")


ggplot(A.Energy.Transport.MacroAgg, aes(x=Country,y=Perctg.PIPELINES.Freight, fill=Country)) +
  geom_boxplot() + geom_jitter(width=0.1,alpha=0.2) +
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle=60, hjust=1)) +
      ggtitle("% PIPELINES FREIGHT")+
      # Title centered
      theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position="bottom",
                                                           legend.box="horizontal")

ggplot(A.Energy.Transport.MacroAgg, aes(x=Country,y=Perctg.WATER.Freight, fill=Country)) +
  geom_boxplot() + geom_jitter(width=0.1,alpha=0.2) +
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle=60, hjust=1)) +
  ggtitle("% WATER FREIGHT")+
  # Title centered
  theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position="bottom",
                                                       legend.box="horizontal")


# Fuel/GDP Assessment

ggplot(A.Energy.Transport.MacroAgg, aes(x=Country,y=Total.Fuel.GDP, fill=Country)) +
  geom_boxplot() + geom_jitter(width=0.1,alpha=0.2) +
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle=60, hjust=1)) +
  ggtitle("TOTAL FUEL/GDP")+
  # Title centered
  theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position="bottom",
                                                       legend.box="horizontal")


#----------------------------------------------------
# TIME EVOLUTION ASSESSMENT

# FUEL

ggplot(data = A.Energy.Transport.MacroAgg,  aes(x = Year, y = Total.Fuel)) + geom_point() +
  stat_smooth(method = "lm", se = FALSE) + facet_wrap(~Country) + ggtitle("TOTAL FUEL")

ggplot(data = A.Energy.Transport.MacroAgg,  aes(x = Year, y = Total.Fuel)) + geom_point() +
  stat_smooth(method = "lm", se = FALSE) + facet_wrap(~Country) + ggtitle("TOTAL FUEL")

ggplot(data = A.Energy.Transport.MacroAgg[-which(A.Energy.Transport.MacroAgg[,"Country"]=="United States"),],
       aes(x = Year, y = Total.Fuel)) + geom_point() +
  stat_smooth(method = "lm", se = FALSE) + facet_wrap(~Country) + ggtitle("TOTAL FUEL (NOT US )")

ggplot(data = A.Energy.Transport.MacroAgg[which(A.Energy.Transport.MacroAgg[,"Country"]=="Belgium"),],
       aes(x = Year, y = Total.Fuel)) + geom_point() +
  stat_smooth(method = "lm", se = FALSE) + facet_wrap(~Country) + ggtitle("TOTAL FUEL")






# FUEL / GDP

ggplot(data = A.Energy.Transport.MacroAgg,  aes(x = Year, y = Total.Fuel.GDP)) + geom_point() +
  stat_smooth(method = "lm", se = FALSE) + facet_wrap(~Country) + ggtitle("TOTAL FUEL / GDP")

ggplot(data = A.Energy.Transport.MacroAgg[-which(A.Energy.Transport.MacroAgg[,"Country"]=="United States"),],
       aes(x = Year, y = Total.Fuel.GDP)) + geom_point() +
  stat_smooth(method = "lm", se = FALSE) + facet_wrap(~Country) + ggtitle("TOTAL FUEL / GDP")

ggplot(data = A.Energy.Transport.MacroAgg[which(A.Energy.Transport.MacroAgg[,"Country"]=="Belgium"),],
       aes(x = Year, y = Total.Fuel.GDP)) + geom_point() +
  stat_smooth(method = "lm", se = FALSE) + facet_wrap(~Country) + ggtitle("TOTAL FUEL / GDP")


# FREIGHT AND PASSENGER DATA
ggplot(data = A.Energy.Transport.MacroAgg[which(A.Energy.Transport.MacroAgg[,"Country"]=="United States"),],
       aes(x = Year, y = Road.Freight)) + geom_point() +
  stat_smooth(method = "lm", se = FALSE) + facet_wrap(~Country) + ggtitle("% ROAD FREIGHT")

ggplot(data = A.Energy.Transport.MacroAgg[which(A.Energy.Transport.MacroAgg[,"Country"]=="United States"),],
       aes(x = Year, y = Perctg.RAIL.Passenger)) + geom_point() +
  stat_smooth(method = "lm", se = FALSE) + facet_wrap(~Country) + ggtitle("% RAIL PASSENGER")


# % RAIL PASSENGER
ggplot(data = A.Energy.Transport.MacroAgg,
       aes(x = Year, y = Perctg.RAIL.Passenger)) + geom_point() +
  stat_smooth(method = "lm", se = FALSE) + facet_wrap(~Country) + ggtitle("% RAIL PASSENGER")

ggplot(data = A.Energy.Transport.MacroAgg[-which(A.Energy.Transport.MacroAgg[,"Country"]=="United States"),],
       aes(x = Year, y = Perctg.RAIL.Passenger)) + geom_point() +
  stat_smooth(method = "lm", se = FALSE) + facet_wrap(~Country) + ggtitle("% RAIL PASSENGER")


#...................................................................................
# END: PLOT Assessment
#...................................................................................




# We remove data from US
Countries.2.remove<-c("United States")
rm(PD.TOTAL.Data.Temp)
PD.TOTAL.Data.Temp<-PD.TOTAL.Data
PD.TOTAL.Data.Temp<-PD.TOTAL.Data[(which(!(PD.TOTAL.Data[,"Country"] %in% Countries.2.remove))),]
#PD.TOTAL.Data.Temp<-PD.TOTAL.Data[which(as.integer(PD.TOTAL.Data[,"Year"]) > 10 ),]
dim(PD.TOTAL.Data.Temp)
pdim(PD.TOTAL.Data.Temp)
View(PD.TOTAL.Data.Temp)

levels(droplevels(as.factor(PD.TOTAL.Data.Temp[,"Country"])))

# Formula: Substracting 1 we remove the constant
Formula1<- Total.Fuel.GDP ~ GDP +
  #GDP.Growth +
  GDP.Growth.x.Capita +
  Perctg.Household.final.consum +
  #Perctg.Act.ind.consum+
  #Total.Inland.Freight +
  Pop.Urban.Agglo + Pop.Density +
  Price.Diesel+
  Perctg.RAIL.Passenger + Perctg.BUS.Passenger + Perctg.RAIL.Freight -1

Formula2<- log(Total.Fuel) ~  #log(GDP)+
  #GDP.Growth +
  #GDP.Growth.x.Capita +
  #Perctg.Household.final.consum +
  #Perctg.Act.ind.consum+
  #Total.Inland.Freight +
  log(Price.Diesel)+
  log(Pop.Urban.Agglo) + log(Pop.Density)+
  
  log(Perctg.RAIL.Passenger) + log(Perctg.BUS.Passenger)+
  + log(Perctg.RAIL.Freight)


PD.Total.Fuel.within<-plm(Formula2 , data=PD.TOTAL.Data.Temp, index=c("Country", "Year"), 
                          model ="within", method="twoways")
PD.Total.Fuel.random<-plm(Formula2 , data=PD.TOTAL.Data.Temp, index=c("Country", "Year"), 
                          model ="random",method="twoways")

PD.Total.Fuel.First.Diff<-plm(Formula2, data=PD.TOTAL.Data.Temp, index=c("Country", "Year"), 
                              model = "fd",method="twoways")

# SOME PROBLEMS: system is computationally singular: reciprocal condition number
# https://stats.stackexchange.com/questions/90020/random-effects-model-with-plm-system-is-computationally-singular-error


summary(fixef(PD.Total.Fuel.within, type = "level"))
summary(coef(PD.Total.Fuel.within))



#https://towardsdatascience.com/getting-familiar-with-rmarkdown-stargazer-3853831a3918
stargazer( PD.Total.Fuel.within, PD.Total.Fuel.random, PD.Total.Fuel.First.Diff,
           type="text",
           align=TRUE, ci.level=0.95, digits=3,
           column.labels=c("Fixed: Two ways effect","Random: Two ways effect","First Differences"),
           header=TRUE, no.space=TRUE,  single.row=TRUE,
           ci=FALSE, title="PANEL DATA for preliminary covariates Assessment",
           style="commadefault",
           dep.var.caption="TOTAL FUEL / GDP",
           font.size = "small",
           report = "vcs*",
           notes.append = TRUE,
           notes.align = "l",
           t.auto = T,
           p.auto = T,
           notes= c("GIANCARLO: This regressions have the single purpose",
           "of making a preliminary assessment of covariates"))
           #notes = c("datasets::freeny", "lm() function", "vcovHC(type = 'HC1')-Robust SE"))
           
#dim(A.Energy.Transport.MacroAgg[-which(A.Energy.Transport.MacroAgg[,"Country"]=="United States"),])



# Tendency visual Assessment
ggplot(data = A.Energy.Transport.MacroAgg[-which(A.Energy.Transport.MacroAgg[,"Country"]=="United States"),],
       aes(x = Year, y = Total.Fuel)) + geom_point() +
  stat_smooth(method = "lm", se = FALSE) + facet_wrap(~Country) + ggtitle("TOE TOTAL PETROLEUM / GDP")


# RANDOM MODELS

PD.Total.Fuel.random<-plm(Formula1 , data=PD.TOTAL.Data.Temp, index=c("Country", "Year"), 
                          model ="random", random.method = "swar")
PD.Total.Fuel.random.swar<-plm(Formula1 , data=PD.TOTAL.Data.Temp, index=c("Country", "Year"), 
                          model ="random", random.method = "walhus")

PD.Total.Fuel.random.nerlove<-plm(Formula1, data=PD.TOTAL.Data.Temp, index=c("Country", "Year"), 
                              model = "random", random.method = "nerlove")

PD.Total.Fuel.random.ht<-plm(Formula1, data=PD.TOTAL.Data.Temp, index=c("Country", "Year"), 
                                  model = "random", random.method = "ht")

stargazer( PD.Total.Fuel.random, PD.Total.Fuel.random.swar, PD.Total.Fuel.random.nerlove,
           PD.Total.Fuel.random.ht,
           type="latex",
           align=TRUE, ci.level=0.95, digits=2,
           column.labels=c("Random: Swar","Random: Walhus","Random: Nerlove","Random: Ht"),
           header=TRUE, no.space=TRUE,  single.row=TRUE,
           ci=FALSE, title="PANEL DATA for preliminary covariates Assessment",
           style="commadefault",
           dep.var.caption="TOTAL FUEL",
           font.size = "small",
           report = "vcs*",
           notes.append = TRUE,
           notes.align = "l",
           t.auto = T,
           p.auto = T,
           notes= c("GIANCARLO: This regressions have the single purpose",
                    "of making a preliminary assessment of covariates"))
#notes = c("datasets::freeny", "lm() function", "vcovHC(type = 'HC1')-Robust SE"))






#PANEL DATA TESTS

pbgtest(PD.Total.Fuel.within, order=2)
pdwtest(PD.Total.Fuel.within, order = 2)

plmtest(PD.Total.Fuel.within, c("time"), type=("bp"))
plmtest(PD.Total.Fuel.within, c("twoways"), type=("bp"))

phtest(PD.Total.Fuel.within, PD.Total.Fuel.random)
pFtest(PD.Total.Fuel.within, PD.Total.Fuel.random)


# How to deal with time tendency
# https://www.researchgate.net/post/Panel_data_with_a_time_trend-How_do_we_make_sure_it_is_accounted_for



file.writing.SemSFAFinal.Data<-paste(Directory.Data.Reading, "/SemSFA-Data-October-2021.csv",sep="")
write.csv(A.Energy.Transport.MacroAgg, file = file.writing.SemSFAFinal.Data, sep=";", dec=".",  col.names = "FALSE")




