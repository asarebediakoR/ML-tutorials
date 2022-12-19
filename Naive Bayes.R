

library('ggplot2')
library ('e1071')
library('class')
library('dplyr') # for resampling
#library('gmodels') # for confusion matrix (For comparison)
library('caret') # for confusion matrix
library ('lattice')

NaiveB_data = read.table('D:/Academics/RIT Fall term/BIOL 672 - Comp Stats/Codes/Unit 2/diabetes.txt', header = TRUE       )

Preg = NaiveB_data$Pregnancies;  # Pregnancies
Glu = NaiveB_data$Glucose; # Glucose
BP = NaiveB_data$BloodPressure; # Blood Pressure
ST = NaiveB_data$SkinThickness; # Skin Thickness
Ins = NaiveB_data$Insulin; # Insulin
B = NaiveB_data$BMI; # BMI
DPF = NaiveB_data$DiabetesPedigreeFunction; # Diabetes Pedigree Function
A = NaiveB_data$Age; # Age
Out = NaiveB_data$Outcome; # Outcome

Naivedataframe = data.frame(Preg,Glu,BP,ST,Ins,B,DPF,A,Out)
subframe = data.frame(Preg,Glu,BP,ST,Ins,B,DPF,A) # Creates a subframe without categorical column-'Output'
print (Naivedataframe)
print(subframe)

Random_data <- sample(1:768, 1) # Randomise my data sample
set.seed(Random_data)
Naivesample = sample_n(Naivedataframe,150, replace=FALSE) # Creates a  150 samples data frame out of the 768 dataset
print(Naivesample)
print(dim(Naivesample)) # Reports the dimension of  new craeted dataset
print(length(Naivesample)) # Reports the number of clases in new dataset

Naivespecies = Naivesample[5:5] # Creates training datasets, pulls out the fifth column in dataset as training sample
print(Naivespecies)
print(dim(Naivespecies))
print(length(Naivespecies))

Naivetest <-  naiveBayes(Out~Preg+Glu+BP+Ins+B+DPF+A+ST, Naivesample, laplace = 0) #  Computes naive Bayes classifier
Naivepred <- predict(Naivetest, subframe, probability = FALSE, decision.values = TRUE)

#ggplots
Plot1 <-ggplot(Naivesample, aes(Glu, Ins, colour = Out)) + geom_point()
Plot2 <-ggplot(Naivesample, aes(Preg, BP, colour = Out)) + geom_point()                                 
Plot3 <-ggplot(Naivesample, aes(B, DPF, colour = Out)) + geom_point()
Plot4 <-ggplot(Naivesample, aes(ST, A, colour = Out)) + geom_point() 
Plot5 <-ggplot(subframe, aes(Glu, Ins, colour = Out)) + geom_point()
Plot6 <-ggplot(subframe, aes(Preg, BP, colour = Out)) + geom_point()                                 
Plot7 <-ggplot(subframe, aes(B, DPF, colour = Out)) + geom_point()
Plot8 <-ggplot(subframe, aes(ST, A, colour = Out)) + geom_point()
Report <- summary(Naivetest)
print(Report)

Plot9 <-ggplot(subframe, aes(Glu, Ins, colour = Naivepred)) + geom_point()
Plot10 <-ggplot(subframe, aes(Preg, BP, colour = Naivepred)) + geom_point()                                 
Plot11 <-ggplot(subframe, aes(B, DPF, colour = Naivepred)) + geom_point()
Plot12 <-ggplot(subframe, aes(ST, A, colour = Naivepred)) + geom_point()

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
print(as.factor(Naivepred))
print(as.factor(Out))
mymatrix <- confusionMatrix(as.factor(Naivepred), as.factor(Out))
print(mymatrix)




