install.packages("tidyverse")
install.packages("ggplot2")
library(ggplot2)
library(tidyverse) 

data <- read.csv("C:/Users/Minahil Ashfaq/OneDrive/Desktop/Advance stats project/TrainingData.csv")#read the data
data
head(data)
View(data)
summary(data)

#Question 3

# Experimental data is showing mostly bad road conditions at high speed. Verify this
# claim.
# h0 -> high speed = bad road
# ha -> high speed != bad road
# lm(formula = Weight ~ pH + Calluna + pH:Calluna, data = festuca)

df =data
df$Road_Condition[df$Road_Condition==1 | df$Road_Condition==2]="Good_Road"
df$Road_Condition[df$Road_Condition==3 | df$Road_Condition==4 | df$Road_Condition==5]="Bad_Road"
df$Road_Condition

q3.model<-lm(formula = Speed ~ Road_Condition, data = df)
q3.model
# opted for two approaches 

#1 treat the road condition as categorical and perform anova
res3 = aov(q3.model)
summary(res3)

# p-value < alpha
# high speeds and bad roads corelate


# OR
ggplot(data, aes(x = Speed)) + geom_dotplot(binwidth = 100)
ggplot(data, aes(x = Road_Condition)) + geom_dotplot(binwidth = 2)

#2 do not treat road condition as categorical and go for corelation test
cor.test(~ Speed + Road_Condition, method = "pearson", data = data)
# giving the same answer
