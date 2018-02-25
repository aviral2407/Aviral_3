library(tidyr)
library(stringr)
library(car)
library(MASS)

#Loading the carprice dataset
Car_Price <- read.csv("CarPrice_Assignment.csv")

unique(Car_Price$car_ID) #To check for unique car id
sum(is.na(Car_Price)) # To check for any possible NA values

str(Car_Price) #To examine the structure of the dataset

Car_Price <- separate(Car_Price, CarName, c("Car_Company","Car_Name"), sep = " ", remove=TRUE)#Separating Car Company and car name in different columns.

#To replace the misspelt strings and converting them to lower case.
Car_Price$Car_Company <- str_replace_all(Car_Price$Car_Company,"maxda","mazda")
Car_Price$Car_Company <- str_replace_all(Car_Price$Car_Company,"Nissan","nissan")
Car_Price$Car_Company <- str_replace_all(Car_Price$Car_Company,"porcshce","porsche")
Car_Price$Car_Company <- str_replace_all(Car_Price$Car_Company,"toyouta","toyota")
Car_Price$Car_Company <- str_replace_all(Car_Price$Car_Company,"vokswagen","volkswagen")
Car_Price$Car_Company <- str_replace_all(Car_Price$Car_Company,"vw","volkswagen")


#Dummy Variable Creation for variables having 2 levels

levels(Car_Price$fueltype) <- c(1,0)
Car_Price$fueltype<- as.numeric(levels(Car_Price$fueltype))[Car_Price$fueltype]

levels(Car_Price$aspiration) <- c(1,0)
Car_Price$aspiration<- as.numeric(levels(Car_Price$aspiration))[Car_Price$aspiration]


levels(Car_Price$doornumber) <- c(1,0)
Car_Price$doornumber<- as.numeric(levels(Car_Price$doornumber))[Car_Price$doornumber]

levels(Car_Price$enginelocation) <- c(1,0)
Car_Price$enginelocation<- as.numeric(levels(Car_Price$enginelocation))[Car_Price$enginelocation]

# Converting the variables with Integer datatype to numeric.
Car_Price$curbweight <- as.numeric(Car_Price$curbweight)
Car_Price$horsepower <- as.numeric(Car_Price$horsepower)
Car_Price$peakrpm <- as.numeric(Car_Price$peakrpm)
Car_Price$citympg <- as.numeric(Car_Price$citympg)
Car_Price$highwaympg <- as.numeric(Car_Price$highwaympg)

################################################################################

#Dummy Variable Creation for 3 or more than 3 levels.

summary(factor(Car_Price$carbody))

Dummy_1 <- data.frame(model.matrix( ~carbody, data = Car_Price))
Dummy_1 <- Dummy_1[,-1]
Car_Price_1 <- cbind(Car_Price[,-8], Dummy_1)


Dummy_2 <- data.frame(model.matrix(~drivewheel, data = Car_Price_1))
Dummy_2 <- Dummy_2[,-1]
Car_Price_2 <- cbind(Car_Price_1[,-8], Dummy_2)


Dummy_3 <- data.frame(model.matrix(~enginetype, data = Car_Price_2))
Dummy_3 <- Dummy_3[,-1]
Car_Price_3 <- cbind(Car_Price_2[,-14], Dummy_3)



Dummy_4 <- data.frame(model.matrix(~fuelsystem, data = Car_Price_3 ))
Dummy_4 <- Dummy_4[,-1]
Car_Price_4 <- cbind(Car_Price_3[,-16], Dummy_4)

Dummy_5 <- data.frame(model.matrix(~Car_Company,data=Car_Price_4))
Dummy_5 <- Dummy_5[,-1]
Car_Price_5 <- cbind(Car_Price_4[,-3], Dummy_5)


Dummy_6 <- data.frame(model.matrix(~cylindernumber,data=Car_Price_5))
Dummy_6 <- Dummy_6[,-1]
Car_Price_6 <- cbind(Car_Price_5[,-13], Dummy_6)

###############################################################################

#Removing Car Name column from the dataset
Car_Price_6 <- Car_Price_6[,-3]

############################################################################

#Creating new derived column
Car_Price_6$Volume <- Car_Price_5$carlength*Car_Price_5$carwidth*Car_Price_5$carheight

######################################################################


#Checking for outliers

quantile(Car_Price_6$wheelbase,seq(0,1,0.01))
boxplot(Car_Price_6$wheelbase)
Car_Price_6$wheelbase[which(Car_Price_6$wheelbase>114.200)]<- 114.200

quantile(Car_Price_6$carlength,seq(0,1,0.01))
boxplot(Car_Price_6$carlength)
Car_Price_6$carlength[which(Car_Price_6$carlength<170.00)]<- 170.00

quantile(Car_Price_6$carwidth,seq(0,1,0.01))
boxplot(Car_Price_6$carwidth)
Car_Price_6$carwidth[which(Car_Price_6$carwidth>70.852)]<- 70.852

quantile(Car_Price_6$carheight,seq(0,1,0.01))  # No outliers
boxplot(Car_Price_6$carheight)

quantile(Car_Price_6$curbweight,seq(0,1,0.01))  # No outliers
boxplot(Car_Price_6$curbweight)
###############################################################################



#Dividing into training and test databases

set.seed(100)

trainindices= sample(1:nrow(Car_Price_6), 0.7*nrow(Car_Price_6))

train.Car_Price_6 = Car_Price_6[trainindices,]

test.Car_price_6 = Car_Price_6[-trainindices,]



model_1 <- lm(price~.,data=Car_Price_6) #Created a linear model and stored it in variable model_1.

summary(model_1)

corrs = cor(Car_Price_6)
View(corrs)


step <- stepAIC(model_1, direction="both")

step


model_2<- lm(formula = price ~ car_ID + fueltype + aspiration + enginelocation + 
     wheelbase + carlength + carwidth + curbweight + enginesize + 
     boreratio + compressionratio + peakrpm + highwaympg + carbodyhardtop + 
     carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
     enginetypeohcf + enginetyperotor + fuelsystem2bbl + Car_Companybmw + 
     Car_Companychevrolet + Car_Companydodge + Car_Companyhonda + 
     Car_Companyisuzu + Car_Companyjaguar + Car_Companymazda + 
     Car_Companymercury + Car_Companymitsubishi + Car_Companynissan + 
     Car_Companypeugeot + Car_Companyplymouth + Car_Companyrenault + 
     Car_Companysaab + Car_Companytoyota + Car_Companyvolkswagen + 
     Car_Companyvolvo + cylindernumberfive + cylindernumbersix + 
     Volume, data = Car_Price_6)

summary(model_2)

vif(model_2)

#Removed the Volume variable
model_3<-lm(formula = price ~ car_ID + fueltype + aspiration + enginelocation + 
                         wheelbase + carwidth + curbweight + enginesize + 
                         boreratio + compressionratio + peakrpm + highwaympg + carbodyhardtop + 
                         carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                         enginetypeohcf + enginetyperotor + fuelsystem2bbl + Car_Companybmw + 
                         Car_Companychevrolet + Car_Companydodge + Car_Companyhonda + 
                         Car_Companyisuzu + Car_Companyjaguar + Car_Companymazda + 
                         Car_Companymercury + Car_Companymitsubishi + Car_Companynissan + 
                         Car_Companypeugeot + Car_Companyplymouth + Car_Companyrenault + 
                         Car_Companysaab + Car_Companytoyota + Car_Companyvolkswagen + 
                         Car_Companyvolvo + cylindernumberfive + cylindernumbersix, 
                         data = Car_Price_6)

summary(model_3)

vif(model_3)

#Removed the fueltype variable
model_4<-lm(formula = price ~car_ID + aspiration + enginelocation + 
              wheelbase + carwidth + curbweight + enginesize + 
              boreratio + compressionratio + peakrpm + highwaympg + carbodyhardtop + 
              carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
              enginetypeohcf + enginetyperotor + fuelsystem2bbl + Car_Companybmw + 
              Car_Companychevrolet + Car_Companydodge + Car_Companyhonda + 
              Car_Companyisuzu + Car_Companyjaguar + Car_Companymazda + 
              Car_Companymercury + Car_Companymitsubishi + Car_Companynissan + 
              Car_Companypeugeot + Car_Companyplymouth + Car_Companyrenault + 
              Car_Companysaab + Car_Companytoyota + Car_Companyvolkswagen + 
              Car_Companyvolvo + cylindernumberfive + cylindernumbersix, 
            data = Car_Price_6)

summary(model_4)

vif(model_4)

#Removed the carwidth variable

model_5<-lm(formula = price ~car_ID + aspiration + enginelocation + 
              wheelbase + curbweight + enginesize + 
              boreratio + compressionratio + peakrpm + highwaympg + carbodyhardtop + 
              carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
              enginetypeohcf + enginetyperotor + fuelsystem2bbl + Car_Companybmw + 
              Car_Companychevrolet + Car_Companydodge + Car_Companyhonda + 
              Car_Companyisuzu + Car_Companyjaguar + Car_Companymazda + 
              Car_Companymercury + Car_Companymitsubishi + Car_Companynissan + 
              Car_Companypeugeot + Car_Companyplymouth + Car_Companyrenault + 
              Car_Companysaab + Car_Companytoyota + Car_Companyvolkswagen + 
              Car_Companyvolvo + cylindernumberfive + cylindernumbersix, 
            data = Car_Price_6)

summary(model_5)
vif(model_5)

#Remove the cylindernumber5 variable
model_6<- lm(formula = price ~ car_ID + aspiration + enginelocation + wheelbase + 
     curbweight + enginesize + boreratio + compressionratio + 
     peakrpm + highwaympg + carbodyhardtop + carbodyhatchback + 
     carbodysedan + carbodywagon + enginetypel + enginetypeohcf + 
     enginetyperotor + fuelsystem2bbl + Car_Companybmw + Car_Companychevrolet + 
     Car_Companydodge + Car_Companyhonda + Car_Companyisuzu + 
     Car_Companyjaguar + Car_Companymazda + Car_Companymercury + 
     Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + 
     Car_Companyplymouth + Car_Companyrenault + Car_Companysaab + 
     Car_Companytoyota + Car_Companyvolkswagen + Car_Companyvolvo + 
     cylindernumbersix,  data = Car_Price_6)

summary(model_6)
vif(model_6)

#Removing the cylinder6 variable
model_7<- lm(formula = price ~ car_ID + aspiration + enginelocation + wheelbase + 
               curbweight + enginesize + boreratio + compressionratio + 
               peakrpm + highwaympg + carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + enginetypel + enginetypeohcf + 
               enginetyperotor + fuelsystem2bbl + Car_Companybmw + Car_Companychevrolet + 
               Car_Companydodge + Car_Companyhonda + Car_Companyisuzu + 
               Car_Companyjaguar + Car_Companymazda + Car_Companymercury + 
               Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + 
               Car_Companyplymouth + Car_Companyrenault + Car_Companysaab + 
               Car_Companytoyota + Car_Companyvolkswagen + Car_Companyvolvo, data = Car_Price_6)

summary(model_7)
vif(model_7)


#Removing the enginetypel variable

model_8<- lm(formula = price ~ car_ID + aspiration + enginelocation + wheelbase + 
               curbweight + enginesize + boreratio + compressionratio + 
               peakrpm + highwaympg + carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + enginetypeohcf + 
               enginetyperotor + fuelsystem2bbl + Car_Companybmw + Car_Companychevrolet + 
               Car_Companydodge + Car_Companyhonda + Car_Companyisuzu + 
               Car_Companyjaguar + Car_Companymazda + Car_Companymercury + 
               Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + 
               Car_Companyplymouth + Car_Companyrenault + Car_Companysaab + 
               Car_Companytoyota + Car_Companyvolkswagen + Car_Companyvolvo, data = Car_Price_6)


summary(model_8)
vif(model_8)

#Removed the Car_ID variable
model_9<- lm(formula = price ~aspiration + enginelocation + wheelbase + 
               curbweight + enginesize + boreratio + compressionratio + 
               peakrpm + highwaympg + carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + enginetypeohcf + 
               enginetyperotor + fuelsystem2bbl + Car_Companybmw + Car_Companychevrolet + 
               Car_Companydodge + Car_Companyhonda + Car_Companyisuzu + 
               Car_Companyjaguar + Car_Companymazda + Car_Companymercury + 
               Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + 
               Car_Companyplymouth + Car_Companyrenault + Car_Companysaab + 
               Car_Companytoyota + Car_Companyvolkswagen + Car_Companyvolvo, data = Car_Price_6)

summary(model_9)
vif(model_9)

#Removed the Compression ratio
model_10<- lm(formula = price ~aspiration + enginelocation + wheelbase + 
               curbweight + enginesize + boreratio + 
               peakrpm + highwaympg + carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + enginetypeohcf + 
               enginetyperotor + fuelsystem2bbl + Car_Companybmw + Car_Companychevrolet + 
               Car_Companydodge + Car_Companyhonda + Car_Companyisuzu + 
               Car_Companyjaguar + Car_Companymazda + Car_Companymercury + 
               Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + 
               Car_Companyplymouth + Car_Companyrenault + Car_Companysaab + 
               Car_Companytoyota + Car_Companyvolkswagen + Car_Companyvolvo, data = Car_Price_6)

summary(model_10)
vif(model_10)

#Removed the bore ratio
model_11<- lm(formula = price ~aspiration + enginelocation + wheelbase + 
                curbweight + enginesize +  
                peakrpm + highwaympg + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + enginetypeohcf + 
                enginetyperotor + fuelsystem2bbl + Car_Companybmw + Car_Companychevrolet + 
                Car_Companydodge + Car_Companyhonda + Car_Companyisuzu + 
                Car_Companyjaguar + Car_Companymazda + Car_Companymercury + 
                Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + 
                Car_Companyplymouth + Car_Companyrenault + Car_Companysaab + 
                Car_Companytoyota + Car_Companyvolkswagen + Car_Companyvolvo, data = Car_Price_6)

summary(model_11)
vif(model_11)

#Remove the car_Companysaab variable
model_12<- lm(formula = price ~aspiration + enginelocation + wheelbase + 
                curbweight + enginesize +  
                peakrpm + highwaympg + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + enginetypeohcf + 
                enginetyperotor + fuelsystem2bbl + Car_Companybmw + Car_Companychevrolet + 
                Car_Companydodge + Car_Companyhonda + Car_Companyisuzu + 
                Car_Companyjaguar + Car_Companymazda + Car_Companymercury + 
                Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + 
                Car_Companyplymouth + Car_Companyrenault + 
                Car_Companytoyota + Car_Companyvolkswagen + Car_Companyvolvo, data = Car_Price_6)

summary(model_12)
vif(model_12)

#Remove the car_Companymercury variable
model_13<- lm(formula = price ~aspiration + enginelocation + wheelbase + 
                curbweight + enginesize +  
                peakrpm + highwaympg + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + enginetypeohcf + 
                enginetyperotor + fuelsystem2bbl + Car_Companybmw + Car_Companychevrolet + 
                Car_Companydodge + Car_Companyhonda + Car_Companyisuzu + 
                Car_Companyjaguar + Car_Companymazda + 
                Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + 
                Car_Companyplymouth + Car_Companyrenault + 
                Car_Companytoyota + Car_Companyvolkswagen + Car_Companyvolvo, data = Car_Price_6)


summary(model_13)
vif(model_13)

#Remove the carbodyhardtop variable
model_14<- lm(formula = price ~aspiration + enginelocation + wheelbase + 
                curbweight + enginesize +  
                peakrpm + highwaympg +  carbodyhatchback + 
                carbodysedan + carbodywagon + enginetypeohcf + 
                enginetyperotor + fuelsystem2bbl + Car_Companybmw + Car_Companychevrolet + 
                Car_Companydodge + Car_Companyhonda + Car_Companyisuzu + 
                Car_Companyjaguar + Car_Companymazda + 
                Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + 
                Car_Companyplymouth + Car_Companyrenault + 
                Car_Companytoyota + Car_Companyvolkswagen + Car_Companyvolvo, data = Car_Price_6)


summary(model_14)
vif(model_14)


#Remove the car_Companyjaguar variable

model_15<- lm(formula = price ~aspiration + enginelocation + wheelbase + 
                curbweight + enginesize +  
                peakrpm + highwaympg +  carbodyhatchback + 
                carbodysedan + carbodywagon + enginetypeohcf + 
                enginetyperotor + fuelsystem2bbl + Car_Companybmw + Car_Companychevrolet + 
                Car_Companydodge + Car_Companyhonda + Car_Companyisuzu + 
                Car_Companymazda + 
                Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + 
                Car_Companyplymouth + Car_Companyrenault + 
                Car_Companytoyota + Car_Companyvolkswagen + Car_Companyvolvo, data = Car_Price_6)

summary(model_15)
vif(model_15)

#Remove the carbodywagon variable

model_16<- lm(formula = price ~aspiration + enginelocation + wheelbase + 
                curbweight + enginesize +  
                peakrpm + highwaympg +  carbodyhatchback + 
                carbodysedan + enginetypeohcf + 
                enginetyperotor + fuelsystem2bbl + Car_Companybmw + Car_Companychevrolet + 
                Car_Companydodge + Car_Companyhonda + Car_Companyisuzu + 
                Car_Companymazda + 
                Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + 
                Car_Companyplymouth + Car_Companyrenault + 
                Car_Companytoyota + Car_Companyvolkswagen + Car_Companyvolvo, data = Car_Price_6)

summary(model_16)
vif(model_16)

#Remove the carbodyhatchback variable

model_16<- lm(formula = price ~aspiration + enginelocation + wheelbase + 
                curbweight + enginesize +  
                peakrpm + highwaympg +
                carbodysedan + enginetypeohcf + 
                enginetyperotor + fuelsystem2bbl + Car_Companybmw + Car_Companychevrolet + 
                Car_Companydodge + Car_Companyhonda + Car_Companyisuzu + 
                Car_Companymazda + 
                Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + 
                Car_Companyplymouth + Car_Companyrenault + 
                Car_Companytoyota + Car_Companyvolkswagen + Car_Companyvolvo, data = Car_Price_6)


#Remove the carbodysedan variable

model_17<- lm(formula = price ~aspiration + enginelocation + wheelbase + 
                curbweight + enginesize +  
                peakrpm + highwaympg +
                enginetypeohcf + 
                enginetyperotor + fuelsystem2bbl + Car_Companybmw + Car_Companychevrolet + 
                Car_Companydodge + Car_Companyhonda + Car_Companyisuzu + 
                Car_Companymazda + 
                Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + 
                Car_Companyplymouth + Car_Companyrenault + 
                Car_Companytoyota + Car_Companyvolkswagen + Car_Companyvolvo, data = Car_Price_6)

#Remove the fuelsystem2bbl variable

model_18<- lm(formula = price ~aspiration + enginelocation + wheelbase + 
                curbweight + enginesize +  
                peakrpm + highwaympg +
                enginetypeohcf + 
                enginetyperotor + Car_Companybmw + Car_Companychevrolet + 
                Car_Companydodge + Car_Companyhonda + Car_Companyisuzu + 
                Car_Companymazda + 
                Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + 
                Car_Companyplymouth + Car_Companyrenault + 
                Car_Companytoyota + Car_Companyvolkswagen + Car_Companyvolvo, data = Car_Price_6)

summary(model_18)
vif(model_18)

#Removing the wheelbase variable

model_19<- lm(formula = price ~aspiration + enginelocation + 
                curbweight + enginesize +  
                peakrpm + highwaympg +
                enginetypeohcf + 
                enginetyperotor + Car_Companybmw + Car_Companychevrolet + 
                Car_Companydodge + Car_Companyhonda + Car_Companyisuzu + 
                Car_Companymazda + 
                Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + 
                Car_Companyplymouth + Car_Companyrenault + 
                Car_Companytoyota + Car_Companyvolkswagen + Car_Companyvolvo, data = Car_Price_6)

summary(model_19)
vif(model_19)

#Removing the companychevrolet variable

model_20<- lm(formula = price ~aspiration + enginelocation + 
                curbweight + enginesize +  
                peakrpm + highwaympg +
                enginetypeohcf + 
                enginetyperotor + Car_Companybmw +
                Car_Companydodge + Car_Companyhonda + Car_Companyisuzu + 
                Car_Companymazda + 
                Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + 
                Car_Companyplymouth + Car_Companyrenault + 
                Car_Companytoyota + Car_Companyvolkswagen + Car_Companyvolvo, data = Car_Price_6)

summary(model_20)
vif(model_20)

#Removing the highwaympg variable

model_21<- lm(formula = price ~aspiration + enginelocation + 
                curbweight + enginesize +  
                peakrpm + 
                enginetypeohcf + 
                enginetyperotor + Car_Companybmw +
                Car_Companydodge + Car_Companyhonda + Car_Companyisuzu + 
                Car_Companymazda + 
                Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + 
                Car_Companyplymouth + Car_Companyrenault + 
                Car_Companytoyota + Car_Companyvolkswagen + Car_Companyvolvo, data = Car_Price_6)

summary(model_21)
vif(model_21)

#Removing the companyisuzu variable

model_22<- lm(formula = price ~aspiration + enginelocation + 
                curbweight + enginesize +  
                peakrpm + 
                enginetypeohcf + 
                enginetyperotor + Car_Companybmw +
                Car_Companydodge + Car_Companyhonda + 
                Car_Companymazda + 
                Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + 
                Car_Companyplymouth + Car_Companyrenault + 
                Car_Companytoyota + Car_Companyvolkswagen + Car_Companyvolvo, data = Car_Price_6)

summary(model_22)
vif(model_22)


#Removing the peakrpm variable

model_23<- lm(formula = price ~aspiration + enginelocation + 
                curbweight + enginesize +  
                enginetypeohcf + 
                enginetyperotor + Car_Companybmw +
                Car_Companydodge + Car_Companyhonda + 
                Car_Companymazda + 
                Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + 
                Car_Companyplymouth + Car_Companyrenault + 
                Car_Companytoyota + Car_Companyvolkswagen + Car_Companyvolvo, data = Car_Price_6)

summary(model_23)
vif(model_23)

#Removing company volvo variable

model_24<- lm(formula = price ~aspiration + enginelocation + 
                curbweight + enginesize +  
                enginetypeohcf + 
                enginetyperotor + Car_Companybmw +
                Car_Companydodge + Car_Companyhonda + 
                Car_Companymazda + 
                Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + 
                Car_Companyplymouth + Car_Companyrenault + 
                Car_Companytoyota + Car_Companyvolkswagen, data = Car_Price_6)

summary(model_24)
vif(model_24)

#Removing company volkswagen
model_25<- lm(formula = price ~aspiration + enginelocation + 
                curbweight + enginesize +  
                enginetypeohcf + 
                enginetyperotor + Car_Companybmw +
                Car_Companydodge + Car_Companyhonda + 
                Car_Companymazda + 
                Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + 
                Car_Companyplymouth + Car_Companyrenault + 
                Car_Companytoyota, data = Car_Price_6)

summary(model_25)
vif(model_25)

# Removing companyhonda variable
model_26<- lm(formula = price ~aspiration + enginelocation + 
                curbweight + enginesize +  
                enginetypeohcf + 
                enginetyperotor + Car_Companybmw +
                Car_Companydodge +  
                Car_Companymazda + 
                Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + 
                Car_Companyplymouth + Car_Companyrenault + 
                Car_Companytoyota, data = Car_Price_6)

summary(model_26)
vif(model_26)



# Removing company mazda variable

model_26<- lm(formula = price ~aspiration + enginelocation + 
                curbweight + enginesize +  
                enginetypeohcf + 
                enginetyperotor + Car_Companybmw +
                Car_Companydodge +  
                Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + 
                Car_Companyplymouth + Car_Companyrenault + 
                Car_Companytoyota, data = Car_Price_6)

summary(model_26)
vif(model_26)

# Removing company dodge variable

model_27<- lm(formula = price ~aspiration + enginelocation + 
                curbweight + enginesize +  
                enginetypeohcf + 
                enginetyperotor + Car_Companybmw +
                  
                Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + 
                Car_Companyplymouth + Car_Companyrenault + 
                Car_Companytoyota, data = Car_Price_6)

summary(model_27)
vif(model_27)

#Removing aspiration variable
model_27<- lm(formula = price ~enginelocation + 
                curbweight + enginesize +  
                enginetypeohcf + 
                enginetyperotor + Car_Companybmw +
                
                Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + 
                Car_Companyplymouth + Car_Companyrenault + 
                Car_Companytoyota, data = Car_Price_6)

# Removing Companyplymouth variable

model_28<- lm(formula = price ~enginelocation + 
                curbweight + enginesize +  
                enginetypeohcf + 
                enginetyperotor + Car_Companybmw +
                
                Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + 
                Car_Companyrenault + 
                Car_Companytoyota, data = Car_Price_6)

summary(model_28)
vif(model_28)


# Removing company renault variable

model_29<- lm(formula = price ~enginelocation + 
                curbweight + enginesize +  
                enginetypeohcf + 
                enginetyperotor + Car_Companybmw +
                
                Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + 
                 
                Car_Companytoyota, data = Car_Price_6)

summary(model_29)
vif(model_29)

# Removing company mitsubishi variable
model_30<- lm(formula = price ~enginelocation + 
                curbweight + enginesize +  
                 
                enginetyperotor + Car_Companybmw +
                
                Car_Companynissan + Car_Companypeugeot + 
                
                Car_Companytoyota, data = Car_Price_6)

summary(model_30)
vif(model_30)

# Removing curbweight variable
model_31<- lm(formula = price ~enginelocation + 
                enginesize +  
                
                enginetyperotor + Car_Companybmw +
                
                Car_Companynissan + Car_Companypeugeot + 
                
                Car_Companytoyota, data = Car_Price_6)

summary(model_31)
vif(model_31)


# Removing car company peugeot variable
model_32<- lm(formula = price ~enginelocation + 
                enginesize +  
                
                enginetyperotor + Car_Companybmw +
                
                Car_Companynissan +  
                
                Car_Companytoyota, data = Car_Price_6)

summary(model_32)
vif(model_32)


# predicting the results in test dataset

Predict_1 <- predict(model_32,test.Car_price_6[,-20])
test.Car_price_6$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted price. 
r <- cor(test.Car_price_6$price,test.Car_price_6$test_price)
rsquared <- cor(test.Car_price_6$price,test.Car_price_6$test_price)^2
