#####Landscape metrics - class level######

#lsm_c_cai_mn #mean of core area index
#lsm_c_ca #mean class area
#lsm_c_ed #edge density for a particular class
setwd("~/Documents/Landscape Ecology 2019/Landscape_Metrics") #change file path to where your data are stored


test1_ca <- data.frame (calculate_lsm (land1, what=c("lsm_c_cai_mn", "lsm_c_ca", "lsm_c_ed")))
#above makes a dataframe of key class-level metrics. You can replace these with any on the list from landscapemetrics (list_lsm())

#Do this for all 10 landscapes

#Other useful code
library (gdata) #package needed to use command "combine", which appends datasets with unique dataset ID ("test1", "test2" etc.)
df4 <- combine (test1_ca, test2_ca, test3_ca, test4_ca, test5_ca, test6_ca, test7_ca, test8_ca, test9_ca, test10_ca)

write.table (df4, file="class_scale_metrics_all.csv", sep=",") #save the table to your working directory if you like
View (df4, "Class-level data") #Notice that for each class (1-8, the metrics are reported for all three class-level variables )

#The code below subsets the full dataset to include only forest (class 8)
forest_data <- subset (df4, class==8)
forest_class_dat <- spread(forest_data, "metric", "value")

pairs.panels(forest_class_dat[6:8],  
             lm=TRUE,
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = FALSE # show correlation ellipses
             )


#To do all classes at the same time

df4 <- combine (test1_ca, test2_ca, test3_ca, test4_ca, test5_ca, test6_ca, test7_ca, test8_ca, test9_ca, test10_ca)
class_dat <- spread(df4, "metric", "value") #summarizes data in wide format with landscape number and class retained

#Plot all pair-wise relationships among all class variables for all classes
pairs.panels(class_dat[6:8],  
             lm=TRUE,
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = FALSE # show correlation ellipses
             
)

#Plot relationship between class area and edge with a loess smoother (interpolates best smoothed fit between points)
library (ggplot2)
ggplot(class_dat, aes(ca, ed)) +
  geom_point() +
  geom_smooth()


