
library (landscapemetrics)
library (raster)
setwd("~/Documents/Landscape Ecology 2019/Landscape_Metrics") #change file path to where your data are stored


#1. Load raster for the NLCD that encompasses Corvallis and Albany

nlcd <- raster("NLCD_corvallis.tif") 
par(mfrow=c(1,1)) #if you had plotting set up previously for 5 rows, 2 columns, this changes back to a single map
plot (nlcd)
#2. Reclassify using the same scheme as in the previous questions
m <- c(10,13,5, 20,25,1, 30,32,3, 50,53, 3, 70,75, 4, 80,83,2, 89,96, 7, 40, 44, 8) #In order of human impact
#NLCD categories: 11-12= water and snow, 21-24=developed land, 31=barren, 41-43=deciduous forest, 51-52=Scrub, 71-74=herbaceous, 81-82=Pasture and cultivated, 90-95=wetlands
rclmat <- matrix( m, ncol=3, byrow=TRUE) #matrix used for reclassification. First two columns are range of data to be reclassified, last column is new class
landscape <- reclassify(nlcd, rclmat) #reclassifies NLCD to be only 7 classes as in the previous excercise

par(mfrow=c(1,1)) #resets plotting from previous excercise so that only one panel is plotted at a time

#3. Plot the new reclassified raster
plot (landscape) #plots reclassified NLCD raster for area surrounding Corvallis and Albany

#4. Select only "habitat" (in this case, forest of any type)
forest <- landscape== 8 #selects only forest category
plot (forest) #plots just the forest, the rest has value of '0'

#5. Randomly sample across the landscape with N=100
pts <- sampleStratified(landscape, size=100, cells=TRUE, sp=TRUE) #randomly selects 1000 points across the study area
points(pts) #plot these points on the map


#6. Calculate patch area associated with each point within forest
pts_extract <- extract_lsm(landscape, y=pts, what ="lsm_p_area") 
#forest_points <- data.frame (subset (pts_extract, id>1)) #subsets to only points found in forest

pts_extract$area <- pts_extract$value # optional: can change variable name from "value" to "area"
# note: In the "lsm_p_area" function, areas are expressed in hectares.

# 7. Calculate neighborhood metrics for all forest pixels (e.g., this is the amount of forest within a 1 km radius)

focal1000m <- focal(forest, w=matrix(1/1000,nrow=31.62,ncol=31.62)) #focal neighborhood of 31.62 x 31.62 cells (1000 m)
plot (focal1000m) #plots neighborhood map. Note that this looks like a blurry map of forest. For each pixel in the map
#we have calculate the number of forest pixels within 1000 m radius that contain forest

#8. Extract values from 'focal1000m' raster and attach to dataframe containing sample points and patch sizes

pts_extract$focal1000m <- raster::extract(focal1000m, pts) #extract values of focal neighborhood from new raster
forest_points <- data.frame (subset (pts_extract, class==8)) #make a new dataframe with only points found in forest

#9. Look at x~y relationship between forest amount (1000 m radius) and log of patch size
plot (forest_points$focal1000m, log (forest_points$area), pch=16, xlab="Forest amount (1000m)", ylab = "log patch size (ha)") 
cor.test (forest_points$focal1000m, log (forest_points$area)) #test for correlation between these two variables

#Or you can plot with ggplot and a loess smoother
forest_points$log_area <- log (forest_points$area)
library (ggplot2)
p <- ggplot(forest_points, aes(focal1000m, log_area)) +
  geom_point() +
  geom_smooth() 
p + xlab ("Forest amount (1000m)") +  ylab ("log patch size (ha)") 


# 10. Design new study

#Identify new study that separates forest amount from fragmentation (patch size)
plot(forest)

# the "click" command (from the raster package) allows you to click on the map where you'd like to 
#establish sample points. Points are stored in dataframe 'x' with xy coordinates.

new_study <- click (forest, xy=TRUE)
#To end adding new points, press "ESC"

# 11. Next extract patch size and forest amount data from rasters above
points <- data.frame (new_study$x, new_study$y) #new dataframe with only xy coordinates for use in extract



points_mat <- as.matrix(points) #turn dataframe into a matrix (for use in 'extract')
new_study <- extract_lsm(forest, y=points_mat, what ="lsm_p_area")  #extract patch sizes (as above)
new_study$patch_size <- new_study$value #renames 'value' to 'patch size'
new_study$focal1000m <- raster::extract(focal1000m, points) #attaches forest amount to data in newstudy

#12. Plot results
plot (new_study$focal1000m, log (new_study$patch_size), pch=16, xlab="Forest amount (1000m)", ylab = "log patch size (ha)") 



