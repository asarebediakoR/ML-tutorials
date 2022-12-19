

library('randomForest')
library('ggplot2')
library('dplyr') # for resampling
#library('gmodels') # for confusion matrix (For comparison)
library('caret') # for confusion matrix
library ('lattice')

RFdata = read.table('C:/Users/RSNasarebediako/Desktop/Unit 2/diabetes.txt', header = TRUE)

Preg = RFdata$Pregnancies;  # Pregnancies
Glu = RFdata$Glucose; # Glucose
BP = RFdata$BloodPressure; #Blood Pressure
ST = RFdata$SkinThickness; # Skin Thickness
Ins = RFdata$Insulin; # Insulin
B = RFdata$BMI; # BMI
DPF = RFdata$DiabetesPedigreeFunction; # Diabetes Pedigree Function
A = RFdata$Age; #Age
Out = RFdata$Outcome; # Outcome

RFdataframe = data.frame(Preg,Glu,BP,ST,Ins,B,DPF,A,Out)
subframe = data.frame(Preg,Glu,BP,ST,Ins,B,DPF,A) # Creates a subframe without categorical column-'Output'
print (subframe)
rnd <- sample(1:768, 1)
set.seed(rnd)
RFsample = sample_n(RFdataframe, 100, replace=FALSE) # Resampling without replacements
print(RFsample)
RFspecies = RFsample[3:3] # Selecting column 'BP' from sample. This is my training sample
print(RFspecies)

#random forest classifier
Out = as.factor(Out)
RFtest <-  randomForest(Out~Preg+Glu+BP+Ins+B+DPF+A+ST, RFsample, ntree = 500)
RFpred <- predict(RFtest, subframe, probability = FALSE, decision.values = TRUE)

Plot1 <-ggplot(RFsample, aes(Glu, Ins, colour = Out)) + geom_point()
Plot2 <-ggplot(RFsample, aes(Preg, BP, colour = Out)) + geom_point()                                 
Plot3 <-ggplot(RFsample, aes(B, DPF, colour = Out)) + geom_point()
Plot4 <-ggplot(RFsample, aes(ST, A, colour = Out)) + geom_point() 
Plot5 <-ggplot(subframe, aes(Glu, Ins, colour = Out)) + geom_point()
Plot6 <-ggplot(subframe, aes(Preg, BP, colour = Out)) + geom_point()                                 
Plot7 <-ggplot(subframe, aes(B, DPF, colour = Out)) + geom_point()
Plot8 <-ggplot(subframe, aes(ST, A, colour = Out)) + geom_point()

MyRFsum <- summary(RFtest)
print(MyRFsum)

#ggplots
Plot9 <-ggplot(subframe, aes(Glu, Ins, colour = RFpred)) + geom_point()
Plot10 <-ggplot(subframe, aes(Preg, BP, colour = RFpred)) + geom_point()                                 
Plot11 <-ggplot(subframe, aes(B, DPF, colour = RFpred)) + geom_point()
Plot12 <-ggplot(subframe, aes(ST, A, colour = RFpred)) + geom_point()


library('grid')
pushViewport(viewport(layout = grid.layout(6, 2)))
print(Plot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(Plot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(Plot3, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(Plot4, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
print(Plot5, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(Plot6, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
print(Plot7, vp = viewport(layout.pos.row = 4, layout.pos.col = 1))
print(Plot8, vp = viewport(layout.pos.row = 4, layout.pos.col = 2))
print(Plot9, vp = viewport(layout.pos.row = 5, layout.pos.col = 1))
print(Plot10, vp = viewport(layout.pos.row = 5, layout.pos.col = 2))
print(Plot11, vp = viewport(layout.pos.row = 6, layout.pos.col = 1))
print(Plot12, vp = viewport(layout.pos.row = 6, layout.pos.col = 2)) 

# confusion matrix for model performance
print(as.factor(RFpred))
print(as.factor(Out))
RFmatrix <- confusionMatrix(as.factor(round(RFpred)), as.factor(Out))
print(RFmatrix)


