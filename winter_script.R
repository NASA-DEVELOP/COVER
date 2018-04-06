#**************************************************************************************
#WINTER CALIBRATION Script
#winter_script.R
#Author: Sunita Yadav
#Date: May 15, 2017    (v 1.0)
#Version: Aug 31, 2017 (v 2.0)
#
#Purpose: Use this script to predict cover crop productivity using field data.
#Steps: The calibration file for each season was developed by extracting the max NDVI
#       value by field for each corresponding year using LANDSAT 5 data; this file
#       was created for the 3 counties for which the USDA had field data.
#       Subsequently, this script develops models associating NDVI to crop productivity
#       and predicts biomass, percent ground cover, and N content for current year data 
#       using the model developed.
#         
#USAGE: 1) Run the models first and analyze the statistics, decide which model makes sense
#        for your data. The calibration file is the same throughout and covers the period
#        2006-2012.
#       2) Then use the model to predict for the current year's data set
#
#CAUTION: Please replace directory paths and filenames if needed in global variables. In this code,
#         the same path is used for all files.
#         - MAIN INPUT FILES: Winter_LT5_all.csv (winter calibration file)
#                             current year field level data file (e.g. L8_20142015.csv)
#         - MAIN OUTPUT FILES: winter_modelResults.txt  (summary of the calibration model)
#                              winter_modelPlots.pdf    (plots of the calibration model)
#                              prediction_winterNEW_1.csv (main prediction file for current year)
#                              
#**************************************************************************************

#import R libraries
install.packages("car")
install.packages("plyr")

#**************************************************************************************
# GLOBAL VARIABLES
#**************************************************************************************
#set working directory for location of the WINTER calibration file and outputs
#substitute your directory path here

setwd("C:/Rscripts/DEVELOP/SpringTerm/model_files")

#set global variable for the spring calibration input file
wtr_calibrationF = "Winter_LT5_all.csv"

#set global variable for the spring satellite image file for the current year
wtr_currentyearF="L8_20142015.csv"

#set global variable for spring prediction file, to be uploaded into MDA database
wtr_predictionF="prediction_winterNEW_2.csv"

#set filename for model plots
plotsFile = "winter_modelPlots.pdf"
#--------------------------------------------------------------------------------------


#**************************************************************************************
# GLOBAL FUNCTION: WRITE MODEL OUTPUTS TO A FILE
#**************************************************************************************
writeResults.F <- function(output.Header, model.Name) {
  #create an empty file and append to it
  sink(file="winter_modelResults.txt", append=TRUE)
  print("******************************************************************************", rownames=FALSE)
  print(output.Header, rownames=FALSE)
  print("******************************************************************************", rownames=FALSE)
  summary(model.Name)   #write the model summary to a txt file
}
#**************************************************************************************


#**************************************************************************************
#PROCESS WINTER DATA
#The calibration data file includes field data for winter cover crops collected 
#by the partner organization from 2006-2012
#The winter calibration file remains the same; includes years from 2006-2012
#**************************************************************************************

#read data file for WINTER calibration data
winter_all <- read.csv(file=wtr_calibrationF, head=TRUE,sep=",")
#nrow(winter_all)

#--------------------------------------------------------------------------
#extract only the columns needed
winter_all2 <- winter_all[ ,c("CLUID", "max_ndvi","L5date", "LECO_N", "BIOMASS_KG", "SQRTBiomas", "SAMPLE_DAT", "CROP_PLANT", 
                              "PLANTING_D", "PLANTING_M")]

#check columns by printing the first row to screen
head(winter_all2,1)
#colnames(winter_all2)      #print column names

#--------------------------------------------------------------------------
#In this section, remove nodata values and zeroes, 

dim(winter_all2)
#replace zeroes with nodata value
winter_all2[winter_all2=="0"] <- NA
winter_all3 = na.exclude(winter_all2)
dim(winter_all3)

#--------------------------------------------------------------------------
#FORMAT DATES: The dates need to be in the correct format to calculate GDD
#Calculate Growing Degree Days from Planting Date
#--------------------------------------------------------------------------
img_date <- as.Date(winter_all3$L5date, '%m/%d/%Y')
plant_date <- as.Date(winter_all3$PLANTING_D, '%m/%d/%Y')

winter_all3$GDDcalc <- as.numeric(difftime((as.Date(winter_all3$L5date, '%m/%d/%Y')), (as.Date(winter_all3$PLANTING_D, '%m/%d/%Y')), units="days"))
#head(winter_all3, 5)
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#Reconfigure planting method into 2 groups and separate NA 
#Direct Seed/Soil:-  BCLD (Broadcast followed by a light disk/tillage), Conventional, NoTill, vertical tillage
#Broadcast:- Aerial, Aerial Air, Aerial Ground, BCSC (Broadcast followed by stalk chopping), Broadcast
#---------------------------------------------------------------------------------

#check how many levels are there in satellite data
levels(winter_all3$PLANTING_M)

winter_all3$new_plantMethod <- as.character(winter_all3$PLANTING_M)
winter_all3$new_plantMethod[winter_all3$PLANTING_M=="AERIAL"] <- "BROADCAST"
winter_all3$new_plantMethod[winter_all3$PLANTING_M=="BCSC"] <- "BROADCAST"
winter_all3$new_plantMethod[winter_all3$PLANTING_M=="BROADCAST"] <- "BROADCAST"
winter_all3$new_plantMethod[winter_all3$PLANTING_M=="NOTILL"] <- "DIRECTSEED"
winter_all3$new_plantMethod[winter_all3$PLANTING_M=="BCLD"] <- "DIRECTSEED"
winter_all3$new_plantMethod[winter_all3$PLANTING_M=="CONVENTIONAL"] <- "DIRECTSEED"
winter_all3$new_plantMethod[winter_all3$PLANTING_M=="na"] <- NA
table(winter_all3$new_plantMethod)       #check how many types in each planting method
#head(winter_all3, 5)

#refactorize, set planting method as a factor
winter_all3$new_plantMethod <- as.factor(winter_all3$new_plantMethod)
levels(winter_all3$new_plantMethod)
table(winter_all3$new_plantMethod)     #check planting method types again

#--------------------------------------------------------------------------
#CHECK DATA NORMALITY
#--------------------------------------------------------------------------
#check normality of data, ndvi
qqnorm(winter_all3$max_ndvi)
qqline(winter_all3$max_ndvi)
hist(winter_all3$max_ndvi)
# Shapiro-Wilks test
shapiro.test(winter_all3$max_ndvi)

#check normality of data, biomass
qqnorm(winter_all3$BIOMASS_KG)
qqline(winter_all3$BIOMASS_KG)
hist(winter_all3$BIOMASS_KG)
# Shapiro-Wilks test
shapiro.test(winter_all3$BIOMASS_KG)

#check normality of data, biomass, log
qqnorm(log(winter_all3$BIOMASS_KG))
qqline(log(winter_all3$BIOMASS_KG))
hist(log(winter_all3$BIOMASS_KG))
# Shapiro-Wilks test
x1log <- log(winter_all3$BIOMASS_KG)
shapiro.test(x1log)

#check normality of data, sqrt_biomass
qqnorm(winter_all3$SQRTBiomas)
qqline(winter_all3$SQRTBiomas)
hist(winter_all3$SQRTBiomas)
# Shapiro-Wilks test
shapiro.test(winter_all3$SQRTBiomas)

#check normality of data, LECO_N
qqnorm(winter_all3$LECO_N)
qqline(winter_all3$LECO_N)
hist(winter_all3$LECO_N)
# Shapiro-Wilks test
shapiro.test(winter_all3$LECO_N)

#check normality of data, GDDcalc
qqnorm(winter_all3$GDDcalc)
qqline(winter_all3$GDDcalc)
hist(winter_all3$GDDcalc)
# Shapiro-Wilks test
shapiro.test(winter_all3$GDDcalc)

#--------------------------------------------------------------------------
# THE MODELS - LM model used
# Biomass: log of biomass was predicted by NDVI, GDD and planting method
# Nitrogen content: not used, not a useful model
#--------------------------------------------------------------------------

#exclude nodata values, make a new dataframe
nrow(winter_all3)
winter_all4 = na.exclude(winter_all3)
nrow(winter_all4)

#the lm model for log(BIOMASS)
winter.lm = lm(log(BIOMASS_KG) ~ max_ndvi + GDDcalc + as.factor(new_plantMethod), data = winter_all4)
#summary(winter.lm)             #check model summary
#---------------------------------------------------------
#write the model output to a file using a global function
writeResults.F("Winter model results to predict log(Biomass) by NDVI, GDD and Planting Method", winter.lm)
closeAllConnections()
#test residuals, to test model for normality
qqnorm(resid(winter.lm))
qqline(resid(winter.lm))
#----save plots to a pdf file
pdf(file=plotsFile)
#plot model results
par(mfrow=c(1,2))
plot(winter.lm)
dev.off()   #close pdf file

#-----------------------------------------
#this will not be used anymore as it was a poor model
#simple linear regression, field nitrogen percent predicted by NDVI, GDD, plantMethod
#fit4.lm = lm(LECO_N ~ max_ndvi + as.factor(PLANTING_M), data = winter_all4)
#summary(fit4.lm)
#test residuals, to test model for normality
#qqnorm(resid(fit4.lm))
#qqline(resid(fit4.lm))

#********************************************************************************
#--------------------------------------------------------------------------------
#MAKE PREDICTIONS FOR WINTER DATA
#PREDICT WITH THE MODEL ABOVE USING CURRENT YEAR DATA
#The file used here can be changed for each year **double-check filename in global varriable section above
#--------------------------------------------------------------------------------

#READ CURRENT YEAR DATA: file used here should be the current year data file
wtr_predict1 <- read.csv(file=wtr_currentyearF,head=TRUE,sep=",")
#nrow(wtr_predict)
head(wtr_predict1, 5)
dim(wtr_predict1)

#remove duplicated rows in original MDA dataset
wtr_predict = wtr_predict1[!duplicated(wtr_predict1$CLUID_1), ]
dim(wtr_predict)  

#--------------------------------------------------------------------------
#extract only the columns needed
winter_L8_pred <- wtr_predict[ ,c("CLUID_1", "winter_ndvi","winter_date", "PreviousCrop", "PlantingMethod", "WhenPlanted", "CALCACRES")]

#check columns
head(winter_L8_pred, 5)
colnames(winter_L8_pred)

#change column names to match calibration data
names(winter_L8_pred)[names(winter_L8_pred)=="winter_ndvi"] <- "max_ndvi"
names(winter_L8_pred)[names(winter_L8_pred)=="PreviousCrop"] <- "CROP_PLANT"
#names(winter_L8_pred)[names(winter_L8_pred)=="PlantingMethod"] <- "PLANTING_M"
head(winter_L8_pred, 5)

#--------------------------------------------------------------------------
##Calculate Growing Degree Days from Planting Date and sensor date

winter_L8_pred$GDDcalc <- as.numeric(difftime((as.Date(winter_L8_pred$winter_date, '%m/%d/%Y')), (as.Date(winter_L8_pred$WhenPlanted, '%m/%d/%Y')), units = c("days")))
#head(winter_L8_pred, 5)

#--------------------------------------------------------------------------
##Reconfigure planting method into 2 groups, for both 
#Direct Seed/Soil:-  BCLD (Broadcast followed by a light disk/tillage), Conventional, NoTill, vertical tillage
#Broadcast:- Aerial, Aerial Air, Aerial Ground, BCSC (Broadcast followed by stalk chopping), Broadcast
#--------------------------------------------------------------------------

#check how many levels there are in satellite data
levels(winter_L8_pred$PlantingMethod)

##Reconfigure planting method into 2 groups, for both 
#make new column
winter_L8_pred$new_plantMethod <- as.character(spring_L8_pred$PlantingMethod)
winter_L8_pred$new_plantMethod[winter_L8_pred$PlantingMethod=="Aerial Air"] <- "BROADCAST"
winter_L8_pred$new_plantMethod[winter_L8_pred$PlantingMethod=="Aerial Ground"] <- "BROADCAST"
winter_L8_pred$new_plantMethod[winter_L8_pred$PlantingMethod=="Broadcast Stalkchop"] <- "BROADCAST"
winter_L8_pred$new_plantMethod[winter_L8_pred$PlantingMethod=="Broadcast"] <- "BROADCAST"
winter_L8_pred$new_plantMethod[winter_L8_pred$PlantingMethod=="Broadcast Light Tillage"] <- "DIRECTSEED"
winter_L8_pred$new_plantMethod[winter_L8_pred$PlantingMethod=="Conventional"] <- "DIRECTSEED"
winter_L8_pred$new_plantMethod[winter_L8_pred$PlantingMethod=="No-till"] <- "DIRECTSEED"
winter_L8_pred$new_plantMethod[winter_L8_pred$PlantingMethod=="Vertical Tillage"] <- "DIRECTSEED"
winter_L8_pred$new_plantMethod[winter_L8_pred$PlantingMethod=="na"] <- NA
table(winter_L8_pred$new_plantMethod)
#count(winter_L8_pred$PlantingMethod)
head(winter_L8_pred, 5)        #check a few rows

#refactorize
winter_L8_pred$new_plantMethod <- as.factor(winter_L8_pred$new_plantMethod)
levels(winter_L8_pred$new_plantMethod)
table(winter_L8_pred$new_plantMethod)

#------------------------
#You have to remove the extra levels before any calculation:
#If you have planting methods not listed above or in calibration data, 
# remove those using code below
#id <- which(!(winter_L8_pred$new_plantMethod %in% levels(winter_all2$new_plantMethod)))
#winter_L8_pred$new_plantMethod[id] <- NA

#--------------------------------------------------------------------------
#PREDICTIONS FOR WINTER
#--------------------------------------------------------------------------

#predict using NEW DATA, setup prediction file
rm (L8_winterNEW)
L8_winterNEW <- data.frame(winter_L8_pred$CLUID_1)
L8_winterNEW$max_ndvi <- winter_L8_pred$max_ndvi
L8_winterNEW$GDDcalc <- winter_L8_pred$GDDcalc
L8_winterNEW$PLANTING_METHOD <- winter_L8_pred$new_plantMethod
L8_winterNEW$winter_date <- winter_L8_pred$winter_date
head(L8_winterNEW, 5)

#This is where the PREDICTION MODEL runs
#predict biomass based on model developed earlier in the script
L8_winterNEW$preds <- predict(winter.lm, newdata = winter_L8_pred)
#head(L8_winterNEW, 5)
#reverse transform to remove the log for biomass from model
L8_winterNEW$exp_preds <- exp(L8_winterNEW$preds)
head(L8_winterNEW, 5)
dim(L8_winterNEW)

# Change column names to integrate with MDA database
colnames(L8_winterNEW) = c("CLUID", "max_ndvi", "GDD", "PlantingMethod", "winter_date", "biomassPred", "biomassExp")
#-----------------------------------------------------------------------
#predictions for N% using model

#predictions for N% using 2% of Biomass, use the transformed number
L8_winterNEW$predN <- (L8_winterNEW$biomassExp*0.02)
#head(L8_winterNEW, 5)

# Remove rows with max_ndvi of zero
#L8_winterNEW = L8_winterNEW[which(L8_winterNEW$max_ndvi != 0), ]

# Change column names to integrate with MDA database
colnames(L8_winterNEW) = c("CLUID", "max_ndvi", "GDD", "PlantingMethod", "winter_date", "biomassPred", "biomassExp", "N%")
head(L8_winterNEW, 5)

# Write table
write.csv(L8_winterNEW, file = wtr_predictionF)
#write.table(L8_winter2014, file = "prediction_winter2014_3.csv", col.names=T, sep=",")

#--------------------------------------------------------------------------------
# THE END
#--------------------------------------------------------------------------------
