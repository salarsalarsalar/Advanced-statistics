install.packages("tidyverse")
install.packages("ggplot2")
library(ggplot2)
library(tidyverse) 

data <- read.csv("C:/Users/Minahil Ashfaq/OneDrive/Desktop/Advance stats project/TrainingData.csv")#read the data
data
head(data)
View(data)
summary(data)

# Question 2
# Which accelerometer readings better explain the road conditions.
# Road_Condition
# Acceleromete_Z
# h0 : az is better
# ha : az isnt better
df =data
df$Road_Condition[df$Road_Condition==1 | df$Road_Condition==2]="Good_Road"
df$Road_Condition[df$Road_Condition==3 | df$Road_Condition==4 | df$Road_Condition==5]="Bad_Road"
df$Road_Condition

q2.model <- lm(Acceleromete_Z ~ Road_Condition + Acceleromete_Y +Acceleromete_X, data = df)
q2.model

#  We used anova one way test because there was two independent and one dependent variable which was categorical
res = aov(q2.model)
summary(res)

# p-value < alpha 
# a_z is the better