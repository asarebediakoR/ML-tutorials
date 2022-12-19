


library(ggplot2)
library(grid)
library('mclust')

My_data= read.table("C:/Users/RSNasarebediako/Desktop/Unit 2/Theoph.txt",header= TRUE)

sb = My_data$Subject # Subjects
wt = My_data$Wt      # Weight (Wt)
ds = My_data$Dose    #Dose
tm = My_data$Time    #Time
cn = My_data$conc    #Concentration(Conc)

DF <- data.frame(sb,wt,ds,tm,cn) #Creats dataframe
print(DF)
subDF = data.frame(wt,ds,tm,cn) # Creates a sub data frame without categorical data

set.seed(6) # Seems to work better with this seed

BICselect <- mclustBIC(subDF) # This selects  best model  


Best_Model <- Mclust(subDF, G = 12, x = BICselect) # G sets the number of clusters in dataset which is equal to the classes

print(summary(Best_Model, parameters = TRUE))

## ggplts
plot1 <-ggplot(My_data, aes(cn, ds, color = sb)) + geom_point()
plot2 <-ggplot(My_data, aes(wt, tm, color = sb)) + geom_point()  

myEM <- summary(Best_Model)
print(myEM)

Best_Model$cluster <- as.factor(Best_Model$cluster)

plot3 <-ggplot(My_data, aes(cn, ds, color = Best_Model$classification)) + geom_point()                                 
plot4 <-ggplot(My_data, aes(wt, tm, color = Best_Model$classification)) + geom_point()   


library('grid')
pushViewport(viewport(layout = grid.layout(2, 2)))
print(plot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(plot3, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(plot4, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))

# plot BIC fit and model overlay on data
print(plot(BICselect))
print(plot(Best_Model, what = "uncertainty"))
print(plot(Best_Model, what="classification"))
print(plot(Best_Model, what="density"))


