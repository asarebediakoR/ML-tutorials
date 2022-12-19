

library('MASS')
library('ggplot2')
library('dplyr') # for resampling
library('caret') # for confusion matrix
#library('lattice')

LDAdata = read.table('D:/Academics/RIT Fall term/BIOL 672 - Comp Stats/Codes/Unit 2/diabetes.txt', header = TRUE  )

Preg = LDAdata$Pregnancies;  # Pregnancies
Glu = LDAdata$Glucose; # Glucose
BP = LDAdata$BloodPressure; # Blood Pressure
ST = LDAdata$SkinThickness; # Skin Thickness
Ins = LDAdata$Insulin; # Insulin
B = LDAdata$BMI; # BMI
DPF = LDAdata$DiabetesPedigreeFunction; # Diabetes Pedigree Function
A = LDAdata$Age; # Age
Out = LDAdata$Outcome; # Outcome

LDAdataframe = data.frame(Preg,Glu,BP,ST,Ins,B,DPF,A,Out)
subframe = data.frame(Preg,Glu,BP,ST,Ins,B,DPF,A) # Creates a subframe without categorical column-'Output'
print (LDAdataframe)
print(subframe)

Random_data <- sample(1:768, 1) # Randomize my data sample 
set.seed(Random_data)
LDAsample = sample_n(LDAdataframe, 150, replace=FALSE) # Creates a  150 samples data frame out of the 768 data set without replacement
print(LDAsample)

LDAtest <-  lda(Out~Preg+Glu+BP+Ins+B+DPF+A+ST, LDAsample) # Computes linear discriminant analysis
LDApred <- predict(LDAtest, subframe, prior = LDAtest$prior, method = "predictive")
print(LDAtest)
print(LDApred)
print(LDApred$class) # Prints class in prediction
LDApred_class = LDApred$class

 # ggplots
Plot1 <-ggplot(LDAsample, aes(Glu, Ins, colour = Out)) + geom_point()
Plot2 <-ggplot(LDAsample, aes(Preg, BP, colour = Out)) + geom_point()                                 
Plot3 <-ggplot(LDAsample, aes(B, DPF, colour = Out)) + geom_point()
Plot4 <-ggplot(LDAsample, aes(ST, A, colour = Out)) + geom_point() 
Plot5 <-ggplot(subframe, aes(Glu, Ins, colour = Out)) + geom_point()
Plot6 <-ggplot(subframe, aes(Preg, BP, colour = Out)) + geom_point()                                 
Plot7 <-ggplot(subframe, aes(B, DPF, colour = Out)) + geom_point()
Plot8 <-ggplot(subframe, aes(ST, A, colour = Out)) + geom_point()

MyLDA <- summary(LDAtest) # Prints summary of my LDA test
print( head(MyLDA))

Plot9 <-ggplot(subframe, aes(Glu, Ins, colour = as.factor(LDApred))) + geom_point()
Plot10 <-ggplot(subframe, aes(Preg, BP, colour = as.factor(LDApred))) + geom_point()                                 
Plot11 <-ggplot(subframe, aes(B, DPF, colour = as.factor(LDApred))) + geom_point()
Plot12 <-ggplot(subframe, aes(ST, A, colour = as.factor(LDApred))) + geom_point()


library('grid')
pushViewport(viewport(layout = grid.layout(4, 2)))
print(Plot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(Plot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(Plot3, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(Plot4, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
print(Plot5, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(Plot6, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
print(Plot7, vp = viewport(layout.pos.row = 4, layout.pos.col = 1))
print(Plot8, vp = viewport(layout.pos.row = 4, layout.pos.col = 2))
#print(Plot9, vp = viewport(layout.pos.row = 5, layout.pos.col = 1))
#print(Plot10, vp = viewport(layout.pos.row = 5, layout.pos.col = 2))
#print(Plot11, vp = viewport(layout.pos.row = 6, layout.pos.col = 1))
#print(Plot12, vp = viewport(layout.pos.row = 6, layout.pos.col = 2)) 

# Confusion matrix for model performance
LDAmatrix <- confusionMatrix(as.factor(LDApred_class), as.factor(Out))
print(LDAmatrix)

LDAplot <- plot(LDAtest) # Plots Linear Discriminant
print(LDAplot)





