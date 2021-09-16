##############################################################################################################
# 
# 
# Program to Do Ranfom Forest analysis on the Data from Debasish Paper 
# 
# 
#     Saha, Debasish, Jason P. Kaye, Arnab Bhowmik, Mary Ann Bruns, John M. Wallace, and Armen R. Kemanian. n.d. "Organic Fertility         Inputs Synergistically Increase Denitrification-Derived Nitrous Oxide Emissions in Agroecosystems." Ecological Applications n         /a (n/a): e02403. https://doi.org/10.1002/eap.2403.
# 
#
#  " https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/eap.2403 "
# 
#  Felipe Montes 2021/09/15
# 
# 
# 
# 
############################################################################################################### 



###############################################################################################################
#                             Tell the program where the package libraries are stored                        
###############################################################################################################


#  Tell the program where the package libraries are  #####################

.libPaths("C:/Felipe/SotwareANDCoding/R_Library/library")  ;


###############################################################################################################
#                             Setting up working directory  Loading Packages and Setting up working directory                        
###############################################################################################################


#      set the working directory

# readClipboard() Willow Rock Spring\\SkyCap_SelectionTrial\\DataCollection") ;   # 

"https://pennstateoffice365.sharepoint.com/:f:/s/StrategicTillageAndN2O/Ehl9Lh_gza5FiOtKIyDD7MQBOKFdFk6h_k4EEYEktWJUYw?e=uYLqL0"

setwd("C:\\Felipe\\CCC Based Experiments\\OrganicTransitions_N2OROSE\\ROSE_N2O_DEBASISH_DATA")

###############################################################################################################
#                            Install the packages that are needed                       
###############################################################################################################



###############################################################################################################
#                           load the libraries that are needed   
###############################################################################################################

library(openxlsx)

library(lattice)


library(randomForest)





###############################################################################################################
#                          Load the data into a data frame
###############################################################################################################

ROSE.N2O.Data.1<-read.xlsx(xlsxFile="C:\\Felipe\\CCC Based Experiments\\OrganicTransitions_N2OROSE\\ROSE_N2O_DEBASISH_DATA\\EAP20-0519 data.xlsx", sheet = "data", detectDates=F) ;

str(ROSE.N2O.Data.1)
names(ROSE.N2O.Data.1)[c(1,3,6,7,8,9,10,11,12,13,14,15,16,17)]<-c("Record" , "Plot", "Manure.Residue" , "Cover.Crop.Biomass", "Legume.Biomas" , "Grass.Biomass" , "VWC" , "Soil.T" , "Air.T" , "Precip2d" , "Bulk.Density" , "WFPS" , "N2O.Flux", "CO2.Flux") ;


ROSE.N2O.Data.1$Date.as.Date<-as.Date(ROSE.N2O.Data.1$Date, format= "%m/%d/%Y") ;

### There is a problem with the dates

ROSE.N2O.Data.1[which(is.na(ROSE.N2O.Data.1$Date.as.Date)),];

### convert the dates with problems from excel numeric format to date

ROSE.N2O.Data.1[which(is.na(ROSE.N2O.Data.1$Date.as.Date)),c("Date.as.Date")]<-as.Date(as.numeric((ROSE.N2O.Data.1[which(is.na(ROSE.N2O.Data.1$Date.as.Date)),c("Date")])), origin = "1899-12-30") ;

# Some of the dates are not well converted and the century is interpreted as the day.

ROSE.N2O.Data.1[which(ROSE.N2O.Data.1$Date.as.Date < as.Date("2016-01-01")),c( "Date", "Date.as.Date")]


ROSE.N2O.Data.1[which(ROSE.N2O.Data.1$Date.as.Date < as.Date("2016-01-01")),c("Date.as.Date")]<-as.Date(ROSE.N2O.Data.1[which(ROSE.N2O.Data.1$Date.as.Date < as.Date("2016-01-01")),c("Date")], format="%m/%d/%y") ;

#  check

ROSE.N2O.Data.1[which(ROSE.N2O.Data.1$Date.as.Date < as.Date("2016-01-01")),c( "Date", "Date.as.Date")]

###############################################################################################################
#                         Try Random Forests
###############################################################################################################

### There are entries without N2O data. They need to be excluded from the analysis

ROSE.N2O.Data.1[which(is.na(ROSE.N2O.Data.1$N2O.Flux)),];


ROSE.N2O.Data.2<-ROSE.N2O.Data.1[which(!is.na(ROSE.N2O.Data.1$N2O.Flux)),] ;

# Check 

ROSE.N2O.Data.2[which(is.na(ROSE.N2O.Data.2$N2O.Flux)),];



str(ROSE.N2O.Data.2)


N2O.rf<-randomForest(N2O.Flux~Main.Crop + Cropping.System + Manure.Residue + Cover.Crop.Biomass + Legume.Biomas + Grass.Biomass + VWC + Soil.T + Air.T + Precip2d +  Bulk.Density +  WFPS +  CO2.Flux, data=ROSE.N2O.Data.2) ;

# There is one entry with no CO2 Flux data

ROSE.N2O.Data.2[which(is.na(ROSE.N2O.Data.2$CO2.Flux)),];

# try random forest without Co2 Flux as explanatory Variable

N2O.rf<-randomForest(N2O.Flux ~ Main.Crop + Cropping.System + Manure.Residue + Cover.Crop.Biomass + Legume.Biomas + Grass.Biomass + VWC + Soil.T + Air.T + Precip2d +  Bulk.Density +  WFPS + Date.as.Date, data=ROSE.N2O.Data.2, importance=T) ;

## Exploring the results

print(N2O.rf)
varImpPlot(N2O.rf)

# Extract the record without Co2 and try with CO2  flux as explanatory value

ROSE.N2O.Data.3<-ROSE.N2O.Data.2[which(!is.na(ROSE.N2O.Data.2$CO2.Flux)),];

# check

ROSE.N2O.Data.3[which(is.na(ROSE.N2O.Data.3$CO2.Flux)),];


N2O.rf<-randomForest(N2O.Flux ~ Main.Crop + Cropping.System + Manure.Residue + Cover.Crop.Biomass + Legume.Biomas + Grass.Biomass + VWC + Soil.T + Air.T + Precip2d +  Bulk.Density +  WFPS + Date.as.Date + CO2.Flux , data=ROSE.N2O.Data.3, importance=T) ;

print(N2O.rf)
varImpPlot(N2O.rf)
partialPlot(x=N2O.rf, pred.data=ROSE.N2O.Data.3, x.var=Air.T, plot=T)
partialPlot(x=N2O.rf, pred.data=ROSE.N2O.Data.3, x.var= CO2.Flux, plot=T)
partialPlot(x=N2O.rf, pred.data=ROSE.N2O.Data.3, x.var= Soil.T, plot=T)


## Adding day of the year as a predictor variable

ROSE.N2O.Data.3$DOY<-as.POSIXlt(ROSE.N2O.Data.3$Date.as.Date)[,"yday"] ;

str(ROSE.N2O.Data.3)

N2O.rf<-randomForest(N2O.Flux ~ Main.Crop + Cropping.System + Manure.Residue + Cover.Crop.Biomass + Legume.Biomas + Grass.Biomass + VWC + Soil.T + Air.T + Precip2d +  Bulk.Density +  WFPS + CO2.Flux +DOY , data=ROSE.N2O.Data.3, importance=T) ;

print(N2O.rf)
varImpPlot(N2O.rf)
partialPlot(x=N2O.rf, pred.data=ROSE.N2O.Data.3, x.var=Air.T, plot=T)
partialPlot(x=N2O.rf, pred.data=ROSE.N2O.Data.3, x.var= CO2.Flux, plot=T)
partialPlot(x=N2O.rf, pred.data=ROSE.N2O.Data.3, x.var= Soil.T, plot=T)
partialPlot(x=N2O.rf, pred.data=ROSE.N2O.Data.3, x.var= DOY, plot=T)


ROSE.N2O.Data.3[ROSE.N2O.Data.3$DOY > 300,]
