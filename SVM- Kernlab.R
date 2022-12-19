

library('kernlab')
library('ggplot2')
library('dplyr') # for resampling
#library('gmodels') # for confusion matrix (For comparison)
library('caret') # for confusion matrix
#library ('lattice')

Kerndata = read.table('C:/Users/RSNasarebediako/Desktop/Unit 2/diabetes.txt', header = TRUE)

Preg = Kerndata$Pregnancies;  # Pregnancies
Glu = Kerndata$Glucose; # Glucose
BP = Kerndata$BloodPressure; # Blood Pressure
ST = Kerndata$SkinThickness; # Skin Thickness
Ins = Kerndata$Insulin; # Insulin
B = Kerndata$BMI; # BMI
DPF = Kerndata$DiabetesPedigreeFunction; # Diabetes Pedigree Function
A = Kerndata$Age; #Age
Out = Kerndata$Outcome; # Outcome

Kerndataframe = data.frame(Preg,Glu,BP,ST,Ins,B,DPF,A,Out)
subframe = data.frame(Preg,Glu,BP,ST,Ins,B,DPF,A) # Creates a subframe without categorical column-'Output'

print(subframe)

Random_data <- sample(1:768, 1) # Randomise my data sample
set.seed(Random_data)

Kernsample = sample_n(Kerndataframe, 120, replace=FALSE) # Creates a  150 samples data frame out of the 768 data set
print(Kernsample)
Kernspecies = Kernsample[7:7] # Selecting the seventh column from the 9 dataframe
print(Kernspecies)

# Running tuned support vector machine with kernel function
Kerntest1 <-  ksvm(as.matrix(Kernsample), Kernspecies, kernel = 'vanilladot') # tuned linear kernel
#Kerntest2 <-  ksvm(as.matrix(Kernsample), Kernspecies, kernel = 'polydot') # tuned polynomial kernel
#Kerntest3 <-  ksvm(as.matrix(Kernsample), Kernspecies, kernel = 'rbfdot') # tuned radial basis function kernel

Kernpred <- predict(Kerntest1, Kerndataframe, type='response')

Plot1 <-ggplot(Kernsample, aes(Glu, Ins, colour = Out)) + geom_point()
Plot2 <-ggplot(Kernsample, aes(Preg, BP, colour = Out)) + geom_point()                                 
Plot3 <-ggplot(Kernsample, aes(B, DPF, colour = Out)) + geom_point()
Plot4 <-ggplot(Kernsample, aes(ST, A, colour = Out)) + geom_point() 
Plot5 <-ggplot(subframe, aes(Glu, Ins, colour = Out)) + geom_point()
Plot6 <-ggplot(subframe, aes(Preg, BP, colour = Out)) + geom_point()                                 
Plot7 <-ggplot(subframe, aes(B, DPF, colour = Out)) + geom_point()
Plot8 <-ggplot(subframe, aes(ST, A, colour = Out)) + geom_point()

Kern_report <- summary(Kerntest1)
print(Kern_report)

Plot9 <-ggplot(subframe, aes(Glu, Ins, colour = Kernpred)) + geom_point()
Plot10 <-ggplot(subframe, aes(Preg, BP, colour = Kernpred)) + geom_point()                                 
Plot11 <-ggplot(subframe, aes(B, DPF, colour = Kernpred)) + geom_point()
Plot12 <-ggplot(subframe, aes(ST, A, colour = Kernpred)) + geom_point()

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
mypred = round(Kernpred) # round integer to next whole number
print(as.factor(mypred))
print(as.factor(Out))



