CALIBRATION Scripts
Author: Sunita Yadav
Version: Aug 31, 2017 (v 2.0)

PURPOSE: These scripts were developed to quantify winter cover crop productivity from satellite imagery using NDVI, growing degree days (GDD), and planting method. The code in both scripts is the same.

METHODS: The calibration file for each season was developed by extracting the max NDVI value by field for each corresponding year using LANDSAT 5 data; this file was created for the 3 counties for which the USDA had field data. Subsequently, this script develops linear regression models to estimate aboveground biomass and percent ground cover (still in the works) for current year data using the model developed. 

   Log(biomass) ~ NDVI + GDD + Planting_Method
   Nitrogen content is calculated as 2% of biomass
         
USAGE: 
1)	Run the models first and analyze the statistics, decide which model makes sense for your data. The calibration file is the same throughout and covers the period 2006-2012.
2)	Then use the model to predict for the current year's data set

    INPUT FILES:   spring_LT5_all.csv (spring calibration file)
                              current year field level data file (e.g. L8_20142015.csv)

    OUTPUT FILES:   spring_modelResults.txt (summary of the calibration model)
                                  spring_modelPlots.pdf    (plots of the calibration model)
                                  prediction_springNEW_1.csv (main prediction file for current year)

CAUTION: Please replace directory paths and filenames if needed in the global variables section. In this code, the same path is used for all files. For winter files, the prefix would be winter_*


