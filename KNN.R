

library('ggplot2')
library('class')
library('dplyr') # for resampling
#library('gmodels') # for confusion matrix (comparison)
library('caret') # for confusion matrix
library ('lattice')

KNNdata = read.table('C:/Users/RSNasarebediako/Desktop/Unit 2/diabetes.txt', header = TRUE)

Preg = KNNdata$Pregnancies;  # Pregnancies
Glu = KNNdata$Glucose; # Glucose
BP = KNNdata$BloodPressure; #Blood Pressure
ST = KNNdata$SkinThickness; # Skin Thickness
Ins = KNNdata$Insulin; # Insulin
B = KNNdata$BMI; # BMI
DPF = KNNdata$DiabetesPedigreeFunction; # Diabetes Pedigree Function
A = KNNdata$Age; #Age
Out = KNNdata$Outcome; # Outcome

KNNdataframe = data.frame(Preg,Glu,BP,ST,Ins,B,DPF,A,Out)
subframe = data.frame(Preg,Glu,BP,ST,Ins,B,DPF,A) # Creates a sub frame without categorical column-'Output'
print (KNNdataframe)
print(subframe)

Random_data <- sample(1:768, 1) # Randomize my data with 768 sample size
set.seed(Random_data)
KNNsample = sample_n(KNNdataframe, 70, replace=FALSE) # Creates a  70 samples data frame out of the original dataset
print(KNNsample)
print(dim(KNNsample)) # Reports the dimension of  new craeted dataset
print(length(KNNsample)) # Reports the number of clases in new dataset

KNNspecies = KNNsample[,8] # Creates training datasets
print(KNNspecies)
print(dim(KNNspecies))
print(length(KNNspecies)) # Checks the dimensions of my training sets

KNNtest <- knn(KNNsample, KNNdataframe, KNNspecies, k=5, l = 0, prob = FALSE, use.all = TRUE) #Computes predictions considering 5 nearest neighbors, testing training set on  the rest dataset

##ggplots
Plot1 <-ggplot(KNNsample, aes(Glu, Ins, colour = as.factor(Out))) + geom_point()
Plot2 <-ggplot(KNNsample, aes(Preg, BP, colour = as.factor(Out))) + geom_point()                                 
Plot3 <-ggplot(KNNsample, aes(B, DPF, colour = as.factor(Out))) + geom_point()
Plot4 <-ggplot(KNNsample, aes(ST, A, colour = as.factor(Out))) + geom_point() 
Plot5 <-ggplot(subframe, aes(Glu, Ins, colour = as.factor(Out))) + geom_point()
Plot6 <-ggplot(subframe, aes(Preg, BP, colour = as.factor(Out))) + geom_point()                                 
Plot7 <-ggplot(subframe, aes(B, DPF, colour = as.factor(Out))) + geom_point()
Plot8 <-ggplot(subframe, aes(ST, A, colour = as.factor(Out))) + geom_point()
myKNN <- summary(KNNtest)
print(myKNN)

Plot9 <-ggplot(subframe, aes(Glu, Ins, colour = KNNtest)) + geom_point()
Plot10 <-ggplot(subframe, aes(Preg, BP, colour = KNNtest)) + geom_point()                                 
Plot11 <-ggplot(subframe, aes(B, DPF, colour = KNNtest)) + geom_point()
Plot12 <-ggplot(subframe, aes(ST, A, colour = KNNtest)) + geom_point()
                
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

#####  Using a confusion matrix for model performance
print(as.integer(as.factor(KNNtest)))
print(as.factor(Out))
mymatrix <- confusionMatrix(as.factor(as.integer(KNNtest)), as.factor(Out))
print(mymatrix)



