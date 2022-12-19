

library('neuralnet')
library('ggplot2')
#library('class')
library('dplyr') # for resampling
#library('gmodels') # for confusion matrix (For comparison)
library('caret') # for confusion matrix
library ('lattice')

NNetdata = read.table('C:/Users/RSNasarebediako/Desktop/Unit 2/diabetes.txt', header = TRUE)

Preg = NNetdata$Pregnancies;  # Pregnancies
Glu = NNetdata$Glucose; # Glucose
BP = NNetdata$BloodPressure; # Blood Pressure
ST = NNetdata$SkinThickness; # Skin Thickness
Ins = NNetdata$Insulin; # Insulin
B = NNetdata$BMI; # BMI
DPF = NNetdata$DiabetesPedigreeFunction; # Diabetes Pedigree Function
A = NNetdata$Age; #Age
Out = NNetdata$Outcome; # Outcome

NNetdataframe = data.frame(Preg,Glu,BP,ST,Ins,B,DPF,A,Out)
subframe = data.frame(Preg,Glu,BP,ST,Ins,B,DPF,A) # Creates a subframe without categorical column-'Output'

print(subframe)

Random_data <- sample(1:768, 1) # Randomise my data sample
set.seed(Random_data)

NNetsample = sample_n(NNetdataframe, 100, replace=FALSE) # Creates a  100 samples data frame out of the 768 data set
print(NNetsample)
Myspecies = NNetsample[2:2] # Selecting the second column from the 9 dataframe. This is my training data set
print(Myspecies)

# Performs neural network classification
NNettest <- neuralnet(Out~Preg+Glu+BP+Ins+B+DPF+A+ST, NNetsample, hidden = 2, linear.output = TRUE)
print(NNettest)
print(plot(NNettest))

# Two(2) and three(3) vertices(neurons) gives a better view or display of the weighting values on each nodes because of the complexity of the data used

NNetpred <- predict(NNettest, subframe, rep = 1, all.units = FALSE)
print(round(NNetpred))

library(data.table)
print(max.col(NNetpred)) # Gives the column with maximum value

##gglpots
Plot1 <-ggplot(NNetsample, aes(Glu, Ins, colour = Out)) + geom_point()
Plot2 <-ggplot(NNetsample, aes(Preg, BP, colour = Out)) + geom_point()                                 
Plot3 <-ggplot(NNetsample, aes(B, DPF, colour = Out)) + geom_point()
Plot4 <-ggplot(NNetsample, aes(ST, A, colour = Out)) + geom_point() 
Plot5 <-ggplot(subframe, aes(Glu, Ins, colour = Out)) + geom_point()
Plot6 <-ggplot(subframe, aes(Preg, BP, colour = Out)) + geom_point()                                 
Plot7 <-ggplot(subframe, aes(B, DPF, colour = Out)) + geom_point()
Plot8 <-ggplot(subframe, aes(ST, A, colour = Out)) + geom_point()

Plot9 <-ggplot(subframe, aes(Glu, Ins, colour = max.col(NNetpred))) + geom_point()
Plot10 <-ggplot(subframe, aes(Preg, BP, colour =max.col(NNetpred))) + geom_point()                                 
Plot11 <-ggplot(subframe, aes(B, DPF, colour =max.col (NNetpred))) + geom_point()
Plot12 <-ggplot(subframe, aes(ST, A, colour = NNetpred)) + geom_point()


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
factor_Out <- as.factor(Out)
print(max.col(NNetpred))
print( as.integer(factor_Out))

interger_Out <- as.integer(factor_Out)

NNetmatrix <- confusionMatrix(as.factor(max.col(NNetpred)), as.factor(factor_Out))
print(NNetmatrix)
print(plot(NNettest))


