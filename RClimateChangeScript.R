#Importing the dataset
ClimateChange<-read.csv("climate_change.csv")

#Overview of the dataset

summary(ClimateChange)
str(ClimateChange)

#Splitting the dataset into two sets using subset function
#subset conditional Year less than or equal to 2006
TrainingSet<-subset(ClimateChange,Year<=2006)

#subset conditional Year greater than 2006
TestingSet<-subset(ClimateChange,Year>2006)

#creating first linear regression model and assigning it to the model1 variable
model1<-lm(Temp~MEI+CO2+CH4+N20+CFC.11+CFC.12+TSI+Aerosols,data = trainingset)

#understanding the Model
summary(model1)

#negative coefficient may indicate correlation between the independent variables
#examining correlation between the variables (highly correlated varibales - greater than 0.7)

cor(TrainingSet)

#creating reduced model and assigning it to the model2 variable
model2<-lm(Temp~MEI+N2O+TSI+Aerosols,data = trainingset)

#understanding model2
summary(model2)

#automate the procedure of trying different combinations, using step() function
#applying step() function to model1 and assigning the result to the model3 variable

model3<-step(model1)

#examine model3
summary(model3)

#the step function will not necessarily produce a very interpretable model
#just a model that has balanced quality and simplicity for a particular weighting of quality and simplicity

#testing on unseen data using the predict() function

tempPrediction<-predict(model3, newdata = TestingSet)

# Calculating R-squared 
  
SSE = sum((tempPredict - TestingSet$Temp)^2)

SST = sum( (mean(trainingset$Temp) - TestingSet$Temp)^2)

R2 = 1 - SSE/SST

