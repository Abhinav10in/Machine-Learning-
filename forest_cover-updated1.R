# Set the path to your data location;
library(psych)  #describe() function
library(ggplot2)
my.path <- 'C:\\Users\\DBA - Spectre\\Desktop\\Forest_Cover_Package\\';
# Read in the RData object using readRDS();
forest.cover <- readRDS(paste(my.path,'forest_cover.RData',sep=''));

# Print the structure;
str(forest.cover)
summary(forest.cover)
describe(forest.cover)

#Name                                     Data Type    Measurement                       Description
# 
# Elevation                               quantitative    meters                       Elevation in meters
# Aspect                                  quantitative    azimuth                      Aspect in degrees azimuth
# Slope                                   quantitative    degrees                      Slope in degrees
# Horizontal_Distance_To_Hydrology        quantitative    meters                       Horz Dist to nearest surface water features
# Vertical_Distance_To_Hydrology          quantitative    meters                       Vert Dist to nearest surface water features
# Horizontal_Distance_To_Roadways         quantitative    meters                       Horz Dist to nearest roadway
# Hillshade_9am                           quantitative    0 to 255 index               Hillshade index at 9am, summer solstice
# Hillshade_Noon                          quantitative    0 to 255 index               Hillshade index at noon, summer soltice
# Hillshade_3pm                           quantitative    0 to 255 index               Hillshade index at 3pm, summer solstice
# Horizontal_Distance_To_Fire_Points      quantitative    meters                       Horz Dist to nearest wildfire ignition points
# Wilderness_Area (4 binary columns)      qualitative     0 (absence) or 1 (presence)  Wilderness area designation
# Soil_Type (40 binary columns)           qualitative     0 (absence) or 1 (presence)  Soil Type designation
# Cover_Type (7 types)                    integer         1 to 7                       Forest Cover Type designation

# Forest Cover Type Classes:
#1 -- Spruce/Fir
# 2 -- Lodgepole Pine
# 3 -- Ponderosa Pine
# 4 -- Cottonwood/Willow
# 5 -- Aspen
# 6 -- Douglas-fir
# 7 -- Krummholz

#Wilderness Areas:  	
#1 -- Rawah
# 2 -- Neota 
# 3 -- Comanche 
# 4 -- Cache  

#Soil Type description taken form Kaggle
#Soil Type designation. Options are:
# 1	   Cathedral family - Rock outcrop complex, extremely stony.
# 2	   Vanet - Ratake families complex, very stony.
# 3	   Haploborolis - Rock outcrop complex, rubbly.
# 4	   Ratake family - Rock outcrop complex, rubbly.
# 5	   Vanet family - Rock outcrop complex complex, rubbly.
# 6	   Vanet - Wetmore families - Rock outcrop complex, stony.
# 7	   Gothic family.
# 8	   Supervisor - Limber families complex.
# 9	   Troutville family, very stony.
# 10	 Bullwark - Catamount families - Rock outcrop complex, rubbly.
# 11	 Bullwark - Catamount families - Rock land complex, rubbly.
# 12	 Legault family - Rock land complex, stony.
# 13	 Catamount family - Rock land - Bullwark family complex, rubbly.
# 14	 Pachic Argiborolis - Aquolis complex.
# 15	 unspecified in the USFS Soil and ELU Survey.
# 16	 Cryaquolis - Cryoborolis complex.
# 17	 Gateview family - Cryaquolis complex.
# 18	 Rogert family, very stony.
# 19	 Typic Cryaquolis - Borohemists complex.
# 20	 Typic Cryaquepts - Typic Cryaquolls complex.
# 21	 Typic Cryaquolls - Leighcan family, till substratum complex.
# 22	 Leighcan family, till substratum, extremely bouldery.
# 23	 Leighcan family, till substratum - Typic Cryaquolls complex.
# 24	 Leighcan family, extremely stony.
# 25	 Leighcan family, warm, extremely stony.
# 26	 Granile - Catamount families complex, very stony.
# 27	 Leighcan family, warm - Rock outcrop complex, extremely stony.
# 28	 Leighcan family - Rock outcrop complex, extremely stony.
# 29	 Como - Legault families complex, extremely stony.
# 30	 Como family - Rock land - Legault family complex, extremely stony.
# 31	 Leighcan - Catamount families complex, extremely stony.
# 32	 Catamount family - Rock outcrop - Leighcan family complex, extremely stony.
# 33	 Leighcan - Catamount families - Rock outcrop complex, extremely stony.
# 34	 Cryorthents - Rock land complex, extremely stony.
# 35	 Cryumbrepts - Rock outcrop - Cryaquepts complex.
# 36	 Bross family - Rock land - Cryumbrepts complex, extremely stony.
# 37	 Rock outcrop - Cryumbrepts - Cryorthents complex, extremely stony.
# 38	 Leighcan - Moran families - Cryaquolls complex, extremely stony.
# 39	 Moran family - Cryorthents - Leighcan family complex, extremely stony.
# 40	 Moran family - Cryorthents - Rock land complex, extremely stony.

#Making new column named Cover_name which contains names of Forrest Cover_type
forest.cover$Cover_Name = 'NA'
forest.cover$Cover_Name[forest.cover$Cover_Type==1] ='Spruce/Fir'
forest.cover$Cover_Name[forest.cover$Cover_Type==2] ='Lodgepole Pine'
forest.cover$Cover_Name[forest.cover$Cover_Type==3] ='Ponderosa Pine'
forest.cover$Cover_Name[forest.cover$Cover_Type==4] ='Cottonwood/Willow'
forest.cover$Cover_Name[forest.cover$Cover_Type==5] ='Aspen'
forest.cover$Cover_Name[forest.cover$Cover_Type==6] ='Douglas-fir'
forest.cover$Cover_Name[forest.cover$Cover_Type==7] ='Krummholz'

#Combining 4 wilderness binary columns into 1 
forest.cover$Wilderness_Area = 0
for (i in 11:14) {
  forest.cover$Wilderness_Area[forest.cover[,i] == 1] = i-10  
}
#Giving names to the Wilderness
forest.cover$Wilderness_Area[forest.cover$Wilderness_Area==1] ='Rawah'
forest.cover$Wilderness_Area[forest.cover$Wilderness_Area==2] ='Neota'
forest.cover$Wilderness_Area[forest.cover$Wilderness_Area==3] ='Comanche'
forest.cover$Wilderness_Area[forest.cover$Wilderness_Area==4] ='Cache'

#Combining 40 Soil_type into single Column named Soil_Type

forest.cover$Soil_Type = 0
for (i in 15:54) {
  forest.cover$Soil_Type[forest.cover[,i]==1] = i-14
}

###Data Exploration for Dependent Variable
ggplot(forest.cover, aes(x=Cover_Name)) + geom_bar(aes(group=Cover_Name, colour=Cover_Name, fill=Cover_Name), alpha=0.3)+geom_text(stat='count', aes(label=..count..), vjust=-1)

# Frequency Distribution of numerical features
par(mfrow=c(3,4))
for(i in 1:10){
  hist(forest.cover[,i], xlab = '', col='steelblue',  main=names(forest.cover[i]))
}

#Exploring the elevation and Cover_type relation
ggplot(forest.cover, aes(x=Elevation)) + geom_histogram(aes(group=Cover_Name, colour=Cover_Name, fill=Cover_Name), alpha=0.3)+ggtitle('Histogram of Elevation')
#we Can see that there is some sort of class segregation as Spruce/Fir grow at elevation higher than 2500m and cottonwood/Willow at elvation lower than 2500m
ggplot(forest.cover, aes(x=Elevation)) + geom_density(aes(group=Cover_Name, colour=Cover_Name, fill=Cover_Name), alpha=0.3)
#From the density graph we see the elevation ranges of various Cover_types 


#ggplot(forest.cover, aes(x=Elevation)) + geom_density(aes(group=Wilderness_Area, colour=Wilderness_Area, fill=Wilderness_Area), alpha=0.3)


# Studying Wilderness Area
ggplot(forest.cover, aes(x=Wilderness_Area)) + geom_bar(aes(group=Cover_Name, colour=Cover_Name, fill=Cover_Name), alpha=1)+ggtitle('T')
#We see that wilderness 1 doesn't support Spruce/Fir 
#Lodgepole pine grows across all wilderness type
# Studying Soil Type
ggplot(forest.cover, aes(x=Soil_Type)) + geom_bar(aes(group=Cover_Name, colour=Cover_Name, fill=Cover_Name), alpha=1)+ggtitle('T')

soil_family = c(1, 2, 3, 4, 2, 2, 5, 6, 7, 8, 8, 9, 10, 11, 12, 13, 14, 15, 13, 14, 15, 16, 16, 16,
                16, 17, 16, 16, 18, 18, 16, 10, 16, 19, 20, 21, 20, 16, 19, 19)
#Where 1 = Cathedral and so on
forest.cover$soil_family = as.numeric(forest.cover$Soil_Type)

for (i in 1:nrow(forest.cover)) {
  forest.cover$soil_family[i]=soil_family[forest.cover$soil_family[i]]
}
forest.cover$soil_family=as.factor(forest.cover$soil_family)
#Division of soil based on the rock type

rock_type=c('stony', 'stony', 'rubbly', 'rubbly', 'rubbly', 'stony', 'neither', 'neither', 'stony', 'rubbly', 'rubbly', 
            'stony', 'rubbly', 'neither', 'neither', 'neither', 'neither', 'stony', 'neither', 'neither', 'neither', 
            'stony', 'neither', 'stony', 'stony', 'stony', 'stony', 'stony', 'stony', 'stony', 'stony', 'stony', 
            'stony', 'stony', 'neither', 'stony', 'stony', 'stony', 'stony', 'stony')

rock_type_factor = c(1,2,3)
rock_type=rock_type_factor[as.factor(rock_type)]

#3 - stony, 1 -neither 2- rubbly 

forest.cover$soil_rock_type=as.numeric(forest.cover$Soil_Type)

for (i in 1:nrow(forest.cover)) {
  forest.cover$soil_rock_type=rock_type[forest.cover$soil_rock_type[i]]
}
forest.cover$soil_rock_type=as.factor(forest.cover$soil_rock_type)

install.packages("tree")
library(tree)
forest.cover$Cover_Type=as.factor(forest.cover$Cover_Type)

model1=tree(Cover_Type~., data=forest.cover[,c(1:55)])
summary(model1)
plot(model1)
text(model1,pretty =0)

install.packages('randomForest')
library(randomForest)
model2 = randomForest(Cover_Type~., data=forest.cover[,c(1:55)],ntree=25)
