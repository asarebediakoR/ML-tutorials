

library('ggplot2')
library('e1071')
library('dplyr') # for resampling
#library('gmodels') # for confusion matrix (For comparison)
library('caret') # for confusion matrix
#library ('lattice')

SVMdata = read.table('D:/Academics/RIT Fall term/BIOL 672 - Comp Stats/Codes/Unit 2/diabetes.txt', header = TRUE)

Preg = SVMdata$Pregnancies;  # Pregnancies
Glu = SVMdata$Glucose; # Glucose
BP = SVMdata$BloodPressure; # Blood Pressure
ST = SVMdata$SkinThickness; # Skin Thickness
Ins = SVMdata$Insulin; # Insulin
B = SVMdata$BMI; # BMI
DPF = SVMdata$DiabetesPedigreeFunction; # Diabetes Pedigree Function
A = SVMdata$Age; # Age
Out = SVMdata$Outcome; # Outcome

SVMdataframe = data.frame(Preg,Glu,BP,ST,Ins,B,DPF,A,Out)
subframe = data.frame(Preg,Glu,BP,ST,Ins,B,DPF,A) # Creates a subframe without categorical column-'Output'

print(subframe)

Random_data <- sample(1:768, 1) # Randomise my data sample
set.seed(Random_data)

SVMsample = sample_n(SVMdataframe, 200, replace=FALSE) # Creates a  200 samples data frame out of the 768 data set without replacements
print(SVMsample)

# Running support vector machine test

SVMtest <-  svm(Out~Preg+Glu+BP+Ins+B+DPF+A+ST, SVMsample, probability = TRUE, type = "C-classification", kernel = "linear") ## Runs SVM with linear kernel function
#SVMtest <-  svm(Out~Preg+Glu+BP+Ins+B+DPF+A+ST, SVMsample, probability =TRUE, type = "C-classification", kernel = "polynomial", degree = 3) ## Runs SVM with polynomial function
#SVMtest <-  svm(Out~Preg+Glu+BP+Ins+B+DPF+A+ST, SVMsample, probability =TRUE, type = "C-classification", kernel = "radial", gamma = 4.5) ## Runs SVM with radial function

# Performs support vector machine prediction
SVMpred <- predict(SVMtest, subframe, probability = FALSE, decision.values = TRUE)

# ggplots
Plot1 <-ggplot(SVMsample, aes(Glu, Ins, colour = Out)) + geom_point()
Plot2 <-ggplot(SVMsample, aes(Preg, BP, colour = Out)) + geom_point()                                 
Plot3 <-ggplot(SVMsample, aes(B, DPF, colour = Out)) + geom_point()
Plot4 <-ggplot(SVMsample, aes(ST, A, colour = Out)) + geom_point() 
Plot5 <-ggplot(subframe, aes(Glu, Ins, colour = Out)) + geom_point()
Plot6 <-ggplot(subframe, aes(Preg, BP, colour = Out)) + geom_point()                                 
Plot7 <-ggplot(subframe, aes(B, DPF, colour = Out)) + geom_point()
Plot8 <-ggplot(subframe, aes(ST, A, colour = Out)) + geom_point()

SVM_Report <- summary(SVMtest)
print(SVM_Report)

Plot9 <-ggplot(subframe, aes(Glu, Ins, colour = SVMpred)) + geom_point()
Plot10 <-ggplot(subframe, aes(Preg, BP, colour = SVMpred)) + geom_point()                                 
Plot11 <-ggplot(subframe, aes(B, DPF, colour = SVMpred)) + geom_point()
Plot12 <-ggplot(subframe, aes(ST, A, colour = SVMpred)) + geom_point()


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

# Running confusion matrix for model performance
print(as.factor(SVMpred))
print(as.factor(Out))
Datamatrix <- confusionMatrix(as.factor(SVMpred), as.factor(Out))
print(Datamatrix)


