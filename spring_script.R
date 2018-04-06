#**************************************************************************************
#SPRING CALIBRATION Script
#spring_script.R
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
#         
#         - MAIN INPUT FILES: spring_LT5_all.csv (winter calibration file)
#                             current year field level data file (e.g. L8_20142015.csv)
#         - MAIN OUTPUT FILES: spring_modelResults.txt  (summary of the calibration model)
#                              spring_modelPlots.pdf    (plots of the calibration model)
#                              prediction_springNEW_1.csv (main prediction file for current year)
#**************************************************************************************

#install R libraries
install.packages("car")
install.packages("plyr")

#**************************************************************************************
# GLOBAL VARIABLES
#**************************************************************************************
#set the working directory for outputs, this needs to be changed by user
#setwd("C:/Rscripts/DEVELOP/SpringTerm/model_files")
setwd("C:/Users/Sunita/Desktop/Jobs/DEVELOP/app_Spring2017/SpringTerm/model_files")


#set global variable for the spring calibration input file
spr_calibrationF = "Spring_LT5_all.csv"

#set global variable for the spring satellite image file for the current year
spr_currentyearF="L8_20142015.csv"

#set global variable for spring prediction file, to be uploaded into MDA database
spr_predictionF="prediction_springNEW_1.csv"

#set filename for model plots
plotsFile = "spring_modelPlots.pdf"
#--------------------------------------------------------------------------------------


#**************************************************************************************
# GLOBAL FUNCTION: WRITE MODEL OUTPUTS TO A FILE
#**************************************************************************************
writeResults.F <- function(output.Header, model.Name) {
   #create an empty file and append to it
   sink(file="spring_modelResults.txt", append=TRUE)
   print("******************************************************************************", rownames=FALSE)
   print(output.Header, rownames=FALSE)
   print("******************************************************************************", rownames=FALSE)
   summary(model.Name)   #write the model summary to a txt file
}
#**************************************************************************************


#**************************************************************************************
#PROCESS SPRING DATA
#The calibration data file includes field data for spring cover crops collected 
#by the partner org organization from 2006-2012
#The spring calibration file remains the same; includes years from 2006-2012
#**************************************************************************************

#read data file for spring calibration data
#spring_all <- read.csv(file="Spring_LT5_all.csv",head=TRUE,sep=",")
spring_all <- read.csv(file=spr_calibrationF,head=TRUE,sep=",")
#nrow(spring_all)

#----------------------------------------------
#extract only the columns needed
spring_all2 <- spring_all[ ,c("CLUID", "max_ndvi","L5date", "LECO_N", "BIOMASS_KG", "SQRTBiomas", "SAMPLE_DAT", "CROP_PLANT", 
                              "PLANTING_D", "PLANTING_M")]

#check columns by printing the first row to screen
head(spring_all2,1)
colnames(spring_all2)

#-------------------------------------------------
#In this section, remove nodata values and zeroes

nrow(spring_all2)
#replace zeroes with nodata value
spring_all2[spring_all2=="0"] <- NA
nrow(spring_all2)
#exclude nodata values, make a new dataframe
spring_all3 = na.exclude(spring_all2)
nrow(spring_all3)

#--------------------------------------------------------------------------
#FORMAT DATES: The dates need to be in the correct format to calculate GDD
#Calculate Growing Degree Days from Planting Date
#--------------------------------------------------------------------------
img_date <- as.Date(spring_all3$L5date, '%m/%d/%Y')
plant_date <- as.Date(spring_all3$PLANTING_D, '%m/%d/%Y')

spring_all3$GDDcalc <- as.numeric(difftime((as.Date(spring_all3$L5date, '%m/%d/%Y')), (as.Date(spring_all3$PLANTING_D, '%m/%d/%Y')), units = c("days")))
head(spring_all3, 5)

#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#Reconfigure planting method into 2 groups and separate NA 
#Direct Seed/Soil:-  BCLD (Broadcast followed by a light disk/tillage), Conventional, NoTill, vertical tillage
#Broadcast:- Aerial, Aerial Air, Aerial Ground, BCSC (Broadcast followed by stalk chopping), Broadcast
#---------------------------------------------------------------------------------

#check how many levels are there in satellite data
levels(spring_all3$PLANTING_M)

spring_all3$new_plantMethod <- as.character(spring_all3$PLANTING_M)
spring_all3$new_plantMethod[spring_all3$PLANTING_M=="AERIAL"] <- "BROADCAST"
spring_all3$new_plantMethod[spring_all3$PLANTING_M=="BCSC"] <- "BROADCAST"
spring_all3$new_plantMethod[spring_all3$PLANTING_M=="BROADCAST"] <- "BROADCAST"
spring_all3$new_plantMethod[spring_all3$PLANTING_M=="NOTILL"] <- "DIRECTSEED"
spring_all3$new_plantMethod[spring_all3$PLANTING_M=="BCLD"] <- "DIRECTSEED"
spring_all3$new_plantMethod[spring_all3$PLANTING_M=="CONVENTIONAL"] <- "DIRECTSEED"
spring_all3$new_plantMethod[spring_all3$PLANTING_M=="na"] <- NA
#table(spring_all3$new_plantMethod)     #check how many types in each planting method
#table(spring_all3$PLANTING_M)          #can count and check if numbers match up
#head(spring_all3, 5)

#refactorize, set planting method as a factor
spring_all3$new_plantMethod <- as.factor(spring_all3$new_plantMethod)
levels(spring_all3$new_plantMethod)
table(spring_all3$new_plantMethod)     #check planting method types again

#--------------------------------------------------------------------------
#CHECK DATA NORMALITY
#--------------------------------------------------------------------------
#check normality of data, ndvi
qqnorm(spring_all3$max_ndvi)
qqline(spring_all3$max_ndvi)
hist(spring_all3$max_ndvi)
# Shapiro-Wilks test
shapiro.test(spring_all3$max_ndvi)

#check normality of data, biomass
qqnorm(spring_all3$BIOMASS_KG)
qqline(spring_all3$BIOMASS_KG)
hist(spring_all3$BIOMASS_KG)
# Shapiro-Wilks test
shapiro.test(spring_all3$BIOMASS_KG)

#check normality of data, biomass, log
qqnorm(log(spring_all3$BIOMASS_KG))
qqline(log(spring_all3$BIOMASS_KG))
hist(log(spring_all3$BIOMASS_KG))
# Shapiro spring_all3
x1log <- log(spring_all3$BIOMASS_KG)
shapiro.test(x1log)

#check normality of data, sqrt_biomass
qqnorm(spring_all3$SQRTBiomas)
qqline(spring_all3$SQRTBiomas)
hist(spring_all3$SQRTBiomas)
# Shapiro-Wilks test
shapiro.test(spring_all3$SQRTBiomas)

#check normality of data, LECO_N
qqnorm(spring_all3$LECO_N)
qqline(spring_all3$LECO_N)
hist(spring_all3$LECO_N)
# Shapiro-Wilks test
shapiro.test(spring_all3$LECO_N)

#--------------------------------------------------------------------------
# THE MODELS - LM model used
# Biomass: log of biomass was predicted by NDVI, GDD and planting method
# Nitrogen content: predicted by NDVI, GDD and planting method
#--------------------------------------------------------------------------

#exclude nodata values, make a new dataframe
nrow(spring_all3)
spring_all4 = na.exclude(spring_all3)
nrow(spring_all4)

#the lm model for log(BIOMASS)
spr1.lm = lm(log(BIOMASS_KG) ~ max_ndvi + GDDcalc + as.factor(new_plantMethod), data = spring_all4)
#summary(spr1.lm)
#---------------------------------------------------------
#write the model output to a file using a global function
writeResults.F("Spring model results to predict log(Biomass) by NDVI, GDD and Planting Method", spr1.lm)
closeAllConnections()
#test residuals, to test model for normality
qqnorm(resid(spr1.lm))
qqline(resid(spr1.lm))
#----save plots to a pdf file
pdf(file=plotsFile)
#check the residuals
par(mfrow=c(1,2))
plot(spr1.lm)
dev.off()   #close pdf file

#----------------------------------------------------
#this will not be used anymore for N prediction as it was a poor model
#simple linear regression, field nitrogen percent predicted by NDVI, GDD, plantMethod
#spr2.lm = lm(LECO_N ~ max_ndvi + GDDcalc + as.factor(new_plantMethod), data = spring_all4)
#write the model output to a file
#summary(spr2.lm)
#writeResults.F("Model results to predict N% by NDVI, GDD and Planting Method", spr2.lm)
#closeAllConnections()
#test residuals, to test model for normality
#qqnorm(resid(spr2.lm))
#qqline(resid(spr2.lm))
#check the residuals
#par(mfrow=c(1,2))
#plot(spr1.lm)


#********************************************************************************
#--------------------------------------------------------------------------------
#MAKE PREDICTIONS FOR SPRING DATA
#PREDICT WITH THE MODEL ABOVE USING CURRENT YEAR DATA
#The file used here can be changed for each year **double-check filename
#--------------------------------------------------------------------------------

#READ CURRENT YEAR DATA: the file used here should be the current year data file
spr_predict1 <- read.csv(file=spr_currentyearF,head=TRUE,sep=",")
nrow(spr_predict1)
#head(spr_predict1, 5)
#colnames(spr_predict1)      #check column names

#remove duplicated rows in original MDA dataset
spr_predict = spr_predict1[!duplicated(spr_predict1$CLUID_1), ]
nrow(spr_predict)

#-----------------------------------------------------------
#extract only the columns needed
spring_L8_pred <- spr_predict[ ,c("CLUID_1", "spring_ndvi","spring_date", "PreviousCrop", "PlantingMethod", "WhenPlanted", "CALCACRES")]

#check columns
head(spring_L8_pred, 5)
colnames(spring_L8_pred)

#change column names to match calibration data
names(spring_L8_pred)[names(spring_L8_pred)=="spring_ndvi"] <- "max_ndvi"
names(spring_L8_pred)[names(spring_L8_pred)=="PreviousCrop"] <- "CROP_PLANT"
head(spring_L8_pred, 5)

#--------------------------------------------------------------------------
##Calculate Growing Degree Days from Planting Date
spring_L8_pred$GDDcalc <- as.numeric(difftime((as.Date(spring_L8_pred$spring_date, '%m/%d/%Y')), (as.Date(spring_L8_pred$WhenPlanted, '%m/%d/%Y')), units = c("days")))
#head(spring_L8_pred, 5)     #check calculation

#--------------------------------------------------------------------------
##Reconfigure planting method into 2 groups, for both 
#Direct Seed/Soil:-  BCLD (Broadcast followed by a light disk/tillage), Conventional, NoTill, vertical tillage
#Broadcast:- Aerial, Aerial Air, Aerial Ground, BCSC (Broadcast followed by stalk chopping), Broadcast
#--------------------------------------------------------------------------

#check how many levels are there in satellite data
levels(spring_L8_pred$PlantingMethod)

#make new column
spring_L8_pred$new_plantMethod <- as.character(spring_L8_pred$PlantingMethod)
spring_L8_pred$new_plantMethod[spring_L8_pred$PlantingMethod=="Aerial Air"] <- "BROADCAST"
spring_L8_pred$new_plantMethod[spring_L8_pred$PlantingMethod=="Aerial Ground"] <- "BROADCAST"
spring_L8_pred$new_plantMethod[spring_L8_pred$PlantingMethod=="Broadcast Stalkchop"] <- "BROADCAST"
spring_L8_pred$new_plantMethod[spring_L8_pred$PlantingMethod=="Broadcast"] <- "BROADCAST"
spring_L8_pred$new_plantMethod[spring_L8_pred$PlantingMethod=="Broadcast Light Tillage"] <- "DIRECTSEED"
spring_L8_pred$new_plantMethod[spring_L8_pred$PlantingMethod=="Conventional"] <- "DIRECTSEED"
spring_L8_pred$new_plantMethod[spring_L8_pred$PlantingMethod=="No-till"] <- "DIRECTSEED"
spring_L8_pred$new_plantMethod[spring_L8_pred$PlantingMethod=="Vertical Tillage"] <- "DIRECTSEED"
spring_L8_pred$new_plantMethod[spring_L8_pred$PlantingMethod=="na"] <- NA
table(spring_L8_pred$new_plantMethod)
#count(spring_L8_pred$PlantingMethod)
#head(spring_L8_pred, 5)

#refactorize
spring_L8_pred$new_plantMethod <- as.factor(spring_L8_pred$new_plantMethod)
#levels(spring_L8_pred$new_plantMethod)
table(spring_L8_pred$new_plantMethod)

#------------------------
#You have to remove the extra levels before any calculation:
#If you have planting methods not listed above or in calibration data, 
# remove those using code below
#id <- which(!(spring_L8_pred$new_plantMethod %in% levels(spring_all2$new_plantMethod)))
#spring_L8_pred$new_plantMethod[id] <- NA


#--------------------------------------------------------------------------
#PREDICTIONS FOR SPRING BIOMASS
#--------------------------------------------------------------------------

#predict using NEW DATA, setup prediction file
rm (L8_springNEW)
L8_springNEW <- data.frame(spring_L8_pred$CLUID_1)
L8_springNEW$max_ndvi <- spring_L8_pred$max_ndvi
L8_springNEW$GDDcalc <- spring_L8_pred$GDDcalc
L8_springNEW$PLANTING_METHOD <- spring_L8_pred$new_plantMethod
L8_springNEW$spring_date <- spring_L8_pred$spring_date
head(L8_springNEW, 5)

#This is where the PREDICTION MODEL runs
#predictions for biomass
L8_springNEW$preds <- predict(spr1.lm, newdata = spring_L8_pred)
head(L8_springNEW, 5)
#reverse transform for log(biomass)
L8_springNEW$exp_preds <- exp(L8_springNEW$preds)
head(L8_springNEW, 5)
dim(L8_springNEW)

# Change column names to integrate with MDA database
colnames(L8_springNEW) = c("CLUID", "max_ndvi", "GDD", "PlantingMethod", "spring_date", "biomassPred", "biomassExp")

#-----------------------------------------------------------------------
#predictions for N% using model
#L8_springNEW$predN <- predict(spr2.lm, newdata = spring_L8_pred)

#predictions for N% using 2% of Biomass, use the transformed number
L8_springNEW$predN <- (L8_springNEW$biomassExp*0.02)
#head(L8_springNEW, 5)

# Remove rows with max_ndvi of zero
#L8_springNEW = L8_springNEW[which(L8_springNEW$max_ndvi != 0), ]

# Change column names to integrate with MDA database
colnames(L8_springNEW) = c("CLUID", "max_ndvi", "GDD", "PlantingMethod", "spring_date","biomassPred", "biomassExp", "N%")
head(L8_springNEW, 5)

# Write table
write.csv(L8_springNEW, file = spr_predictionF)

#--------------------------------------------------------------------------------
# THE END
#--------------------------------------------------------------------------------
