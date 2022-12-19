

library('ada')
library('ggplot2')
library('dplyr') # for resampling
library('caret') # for confusion matrix

Adadata = read.table('C:/Users/RSNasarebediako/Desktop/Unit 2/diabetes.txt', header = TRUE)

Preg = Adadata$Pregnancies;  # Pregnancies
Glu = Adadata$Glucose; # Glucose
BP = Adadata$BloodPressure; #Blood Pressure
ST = Adadata$SkinThickness; # Skin Thickness
Ins = Adadata$Insulin; # Insulin
B = Adadata$BMI; # BMI
DPF = Adadata$DiabetesPedigreeFunction; # Diabetes Pedigree Function
A = Adadata$Age; #Age
Out = Adadata$Outcome; # Outcome

ABdataframe = data.frame(Preg,Glu,BP,ST,Ins,B,DPF,A,Out)
subframe = data.frame(Preg,Glu,BP,ST,Ins,B,DPF,A) # Creates a subframe without categorical column-'Output'
print (subframe)
Random_sample<- sample(1:768, 1) # Random sampling
set.seed(Random_sample)
Adasample = sample_n(ABdataframe, 150, replace=FALSE) # Resampling without replacements
print(Adasample)
ABspecies = Adasample[3:3] # Selecting column 'BP' from sample. This is my training sample
print(ABspecies)

#Adaptive Boosting algorithm
ABtest <-  ada(Out~Preg+Glu+BP+Ins+B+DPF+A+ST, Adasample) 
ABpred <- predict(ABtest, subframe, probability = FALSE, decision.values = TRUE)

#ggplots
Plot1 <-ggplot(Adasample, aes(Glu, Ins, colour = Out)) + geom_point()
Plot2 <-ggplot(Adasample, aes(Preg, BP, colour = Out)) + geom_point()                                 
Plot3 <-ggplot(Adasample, aes(B, DPF, colour = Out)) + geom_point()
Plot4 <-ggplot(Adasample, aes(ST, A, colour = Out)) + geom_point() 
Plot5 <-ggplot(subframe, aes(Glu, Ins, colour = Out)) + geom_point()
Plot6 <-ggplot(subframe, aes(Preg, BP, colour = Out)) + geom_point()                                 
Plot7 <-ggplot(subframe, aes(B, DPF, colour = Out)) + geom_point()
Plot8 <-ggplot(subframe, aes(ST, A, colour = Out)) + geom_point()

AB_Summary <- summary(ABtest)
print(AB_Summary)

Plot9 <-ggplot(subframe, aes(Glu, Ins, colour = ABpred)) + geom_point()
Plot10 <-ggplot(subframe, aes(Preg, BP, colour = ABpred)) + geom_point()                                 
Plot11 <-ggplot(subframe, aes(B, DPF, colour = ABpred)) + geom_point()
Plot12 <-ggplot(subframe, aes(ST, A, colour = ABpred)) + geom_point()

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
print(as.factor(ABpred))
print(as.factor(Out))
ABmatrix <- confusionMatrix(as.factor(ABpred), as.factor(Out))
print(ABmatrix)


