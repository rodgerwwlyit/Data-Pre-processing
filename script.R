#Create a data frame heatloss_1 by reading the csv file traing2.csv
#Set header = TRUE to use the headings from the csv file
#sep = "," indicates that the data is separated by commas
#na.strings = c(" ")) fills all blank cells with NA

heatloss_1 <- read.csv("training2.csv", header = TRUE, sep = ",", na.strings = c(""))

#Forms a new dataframe heatloss_2 consisting of the following variables BuildingID, Type, Year, EnergyRatingCont,
# GroundFloorArea, AvgWallU, AvgRoofU, AvgFloorU, AvgWindowU, AvgDoorU, ExposedWallArea, ExposedRoofArea, TotFloorArea,
# TotWindowArea, TotDoorArea, MainSHFuel, MainWHFuel.

myvars <- c("BuildingID", "Type", "Year", "EnergyRatingCont", "GroundFloorArea", "AvgWallU", "AvgRoofU", "AvgFloorU",
"AvgWindowU", "AvgDoorU", "ExposedWallArea", "ExposedRoofArea", "TotFloorArea", "TotWindowArea", "TotDoorArea", "MainSHFuel", "MainWHFuel") 
heatloss_2 <- heatloss_1[myvars]

#Changes cells which contains the value 0.00 to NA. 

heatloss_2[heatloss_2 == 0.00] <- NA
View(heatloss_2)

#Creates a new variable Total_AvU by adding the following variables together: AvgWallU + AvgRoofU + AvgFloorU + AvgWindowU + AvgDoorU

heatloss_2 <- transform(heatloss_2, Total_AvU = AvgWallU + AvgRoofU + AvgFloorU + AvgWindowU + AvgDoorU)
View(heatloss_2)

#Allows rows which had NA in cells to be included in the new calculation for Total_AvU by using na.rm = True

heatloss_2$Total_AvU <- rowSums(heatloss_2[,c("AvgWallU", "AvgRoofU", "AvgFloorU", "AvgWindowU", "AvgDoorU")], na.rm = TRUE)
View(heatloss_2)

#Creates a new variable called ER_CATEGORY that gives an energy rating category depending on the value of the 
#EnergyRatingCont variable. The categories are A1,A2,A3,B1,B2,B3,C1,C2,C3,D1,D2,E1,E2,F,G.
#The categories change for each increase of 25 in the EnergyRatingCont variable

heatloss_2$ER_CATEGORY[heatloss_2$EnergyRatingCont <= 25] <- "A1"
heatloss_2$ER_CATEGORY[heatloss_2$EnergyRatingCont > 25 & heatloss_2$EnergyRatingCont <= 50] <- "A2"
heatloss_2$ER_CATEGORY[heatloss_2$EnergyRatingCont > 50 & heatloss_2$EnergyRatingCont <= 75] <- "A3"
heatloss_2$ER_CATEGORY[heatloss_2$EnergyRatingCont > 75 & heatloss_2$EnergyRatingCont <= 100] <- "B1"
heatloss_2$ER_CATEGORY[heatloss_2$EnergyRatingCont > 100 & heatloss_2$EnergyRatingCont <= 125] <- "B2"
heatloss_2$ER_CATEGORY[heatloss_2$EnergyRatingCont > 125 & heatloss_2$EnergyRatingCont <= 150] <- "B3"
heatloss_2$ER_CATEGORY[heatloss_2$EnergyRatingCont > 150 & heatloss_2$EnergyRatingCont <= 175] <- "C1"
heatloss_2$ER_CATEGORY[heatloss_2$EnergyRatingCont > 175 & heatloss_2$EnergyRatingCont <= 200] <- "C2"
heatloss_2$ER_CATEGORY[heatloss_2$EnergyRatingCont > 200 & heatloss_2$EnergyRatingCont <= 225] <- "C3"
heatloss_2$ER_CATEGORY[heatloss_2$EnergyRatingCont > 225 & heatloss_2$EnergyRatingCont <= 260] <- "D1"
heatloss_2$ER_CATEGORY[heatloss_2$EnergyRatingCont > 260 & heatloss_2$EnergyRatingCont <= 300] <- "D2"
heatloss_2$ER_CATEGORY[heatloss_2$EnergyRatingCont > 300 & heatloss_2$EnergyRatingCont <= 340] <- "E1"
heatloss_2$ER_CATEGORY[heatloss_2$EnergyRatingCont > 340 & heatloss_2$EnergyRatingCont <= 380] <- "E2"
heatloss_2$ER_CATEGORY[heatloss_2$EnergyRatingCont > 380 & heatloss_2$EnergyRatingCont <= 450] <- "F"
heatloss_2$ER_CATEGORY[heatloss_2$EnergyRatingCont > 450 ] <- "G"

View(heatloss_2)

# Creates a new variable SpaceHeatFuel by converting the method of heating from the MainSHFuel variable
# to a numerical value based on the price to produce 1 kw/h of energy using that energy source

heatloss_2$SpaceHeatFuel[heatloss_2$MainSHFuel == "Heating Oil                   "] <- 6.23
heatloss_2$SpaceHeatFuel[heatloss_2$MainSHFuel == "Mains Gas                     "] <- 6.52
heatloss_2$SpaceHeatFuel[heatloss_2$MainSHFuel == "Solid Multi-Fuel              "] <- 5.50
heatloss_2$SpaceHeatFuel[heatloss_2$MainSHFuel == "Electricity                   "] <- 19.77
heatloss_2$SpaceHeatFuel[heatloss_2$MainSHFuel == "Bulk LPG (propane or butane)  "] <- 9.07
heatloss_2$SpaceHeatFuel[heatloss_2$MainSHFuel == "Wood Pellets (bulk supply for "] <- 6.75
heatloss_2$SpaceHeatFuel[heatloss_2$MainSHFuel == "Wood Logs                     "] <- 9.79
heatloss_2$SpaceHeatFuel[heatloss_2$MainSHFuel == "Manufactured Smokeless Fuel   "] <- 5.47
heatloss_2$SpaceHeatFuel[heatloss_2$MainSHFuel == "House Coal                    "] <- 5.35
heatloss_2$SpaceHeatFuel[heatloss_2$MainSHFuel == "Bottled LPG                   "] <- 16.61

# Creates a new variable WaterHeatFuel by converting the method of heating from the MainWHFuel
# variable to a numerical value based on the price to produce 1 kw/h of energy using that energy source

heatloss_2$WaterHeatFuel[heatloss_2$MainWHFuel == "Heating Oil                   "] <- 6.23
heatloss_2$WaterHeatFuel[heatloss_2$MainWHFuel == "Mains Gas                     "] <- 6.52
heatloss_2$WaterHeatFuel[heatloss_2$MainWHFuel == "Solid Multi-Fuel              "] <- 5.50
heatloss_2$WaterHeatFuel[heatloss_2$MainWHFuel == "Electricity                   "] <- 19.77
heatloss_2$WaterHeatFuel[heatloss_2$MainWHFuel == "Bulk LPG (propane or butane)  "] <- 9.07
heatloss_2$WaterHeatFuel[heatloss_2$MainWHFuel == "Wood Pellets (bulk supply for "] <- 6.75
heatloss_2$WaterHeatFuel[heatloss_2$MainWHFuel == "Wood Logs                     "] <- 9.79
heatloss_2$WaterHeatFuel[heatloss_2$MainWHFuel == "Manufactured Smokeless Fuel   "] <- 5.47
heatloss_2$WaterHeatFuel[heatloss_2$MainWHFuel == "House Coal                    "] <- 5.35
heatloss_2$WaterHeatFuel[heatloss_2$MainWHFuel == "Bottled LPG                   "] <- 16.61

View(heatloss_2)

# Creates a new dataframe heatloss_3 by removing rows that contained NA in either 
# the SpaceHeatFuel or WaterHeatFuel columns

heatloss_3 <- heatloss_2[!is.na(heatloss_2$SpaceHeatFuel & heatloss_2$WaterHeatFuel),]


View(heatloss_3)
heatloss_3
str(heatloss_3)
summary(heatloss_3)