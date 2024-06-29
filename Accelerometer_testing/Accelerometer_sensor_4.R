install.packages("tidyverse")
install.packages("ggplot2")
library(ggplot2)
library(tidyverse) 

data <- read.csv("C:/Users/Minahil Ashfaq/OneDrive/Desktop/Advance stats project/TrainingData.csv")#read the data
data
head(data)
View(data)
summary(data)

# Question 4
# time corelates to road_condition

# Tried chisq but it doesnt work for it
# table(data$Road_Condition, dat$Timer)
# test <- chisq.test(table(data$Species, data$size))
# test

# opted for one way anova test 
q4.model<-lm(Road_Condition ~ Timer, data = data)
q4.model
# we used one way anova test 
res4 = aov(q4.model)
summary(res4)

# p-value < alpha
# high speeds and bad roads corelate

