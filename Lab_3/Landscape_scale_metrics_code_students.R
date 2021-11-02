#####Landscape metrics - landscape level######

#This lab requires the following packages. Be sure to install these packages before attempting to load them.
library (landscapemetrics)
library(raster)
library (psych)
list_lsm() #gives full list of metrics by patch, class and landscape (very useful)
data.frame (list_lsm()) #to see full list (not a tibble)
setwd("~/Documents/Landscape Ecology 2019/Landscape_Metrics") #change file path to where your data are stored

#######Data set-up#######

#This lab uses 10, 10 x 10 km landscapes randomly clipped from the Oregon NLCD (National Land Cover Database) layer. For the purposes of this lab, 
# I have reclassified the NLCD to have only 7 categories: 1 = developed land, 2 = cultivated, 3 = barren land and scrub, 4 = herbaceous vegetation, 5 = water and snow
# 7 = wetlands, 8 = forest. Don't ask why there is no "6"!
#If you want to look at the whole NLCD map: https://spatialdata.oregonexplorer.info/geoportal/download 

#You can download the Oregon NLCD to your working directory and view in R using the following code:
tile = raster("NLCD_OR_2011_.tif") 
plot (tile)
#Legend available here: https://www.mrlc.gov/data/legends/national-land-cover-database-2011-nlcd2011-legend


#Data import

#Import rasters that have been saved to your working directory.
land1 = raster("land1.tif")
land2 = raster("land2.tif")
land3 = raster("land3.tif")
land4 = raster("land4.tif")
land5 = raster("land5.tif")
land6 = raster("land6.tif")
land7 = raster("land7.tif")
land8 = raster("land8.tif")
land9 = raster("land9.tif")
land10 = raster("land10.tif")
#All landscape .tiffs (e.g., "land1.tiff") need to be in your working directory

#Panel figure with all 10 landscapes
par(mfrow=c(5,2)) #sets up panel figure

#Plot results
land_1 = plot (land1)
land_2 = plot (land2)
land_3 = plot (land3)
land_4 = plot (land4)
land_5 = plot (land5)
land_6 = plot (land6)
land_7 = plot (land7)
land_8 = plot (land8)
land_9 = plot (land9)
land_10 = plot (land10)
#######Calculating a suite of landcape metrics for 10 landcapes#####

test1 <- data.frame (calculate_lsm (land1, what=c("lsm_l_ed", "lsm_l_core_mn", "lsm_l_shei", "lsm_l_np", "lsm_l_enn_mn")))
#above makes a dataframe of key landscape-level metrics for landscape #1. You can replace these with any on the list from landscapemetrics (list_lsm())
#"lsm_l_ed" = edge density, "lsm_l_core_mn" = core area, "lsm_l_shei" = Shannon's diversity, "lsm_l_np" = number of patches
#"lsm_l_enn_mn" = mean nearest neighbor

#Now try this for each landscape. 


#some potentially useful code:
library (gdata) #package needed to use command "combine", which appends datasets with unique dataset ID ("test1", "test2" etc.)
final_land_dat <- combine (test1, test2)

library (tidyr) #this package allows you to go from long to wide format
final_land_dat <- spread(final_land_dat, "metric", "value")

#Log transformation of patch size/core area
final_land_dat$log_core_mn <- log(final_land_dat$core_mn) #log transforms core area for an additional metric that is more biologically meaningful
View (final_land_dat) #You can now look at the final table with 6 landscpae metrics

write.table (final_land_dat, file="final_land_dat.csv", sep=",") #Saves file to working directory if you like

#plots pairwise correlation between all variables in the final dataframe (columns 6-11).
pairs(final_land_dat[6:11], pch = 19,  cex = 2,
      col = "red",
      lower.panel=NULL)

#Or you can try below to get Pearson correlation coefficients

install.packages("psych")
library(psych)
pairs.panels(final_land_dat[6:11],  
             lm=TRUE,
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = FALSE # show correlation ellipses
             
             )


