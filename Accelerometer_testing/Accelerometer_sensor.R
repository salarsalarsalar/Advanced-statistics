install.packages("tidyverse")
install.packages("ggplot2")
library(ggplot2)
library(tidyverse) 

data <- read.csv("C:/Users/Minahil Ashfaq/OneDrive/Desktop/Advance stats project/TrainingData.csv")#read the data
data
head(data)
View(data)
summary(data)
glimpse(data)
# Question 1

# Accelerometer sensor when reading augmented with Gyro sensor data has a better
# chance of detecting the road condition.
# hypothesis statement
# Ho ---> U(a,g) <= U(!a,!g) a and g do not have better road detection readings when augmented
# Ha ---> U(a,g) >= U(!a,!g) a and g do have better road detection readings when augmented

df =data
df$Road_Condition[df$Road_Condition==1 | df$Road_Condition==2]="Good_Road"
df$Road_Condition[df$Road_Condition==3 | df$Road_Condition==4 | df$Road_Condition==5]="Bad_Road"
df$Road_Condition

# The road anomaly in accelerometer data is defined by peak acceleration in
# Z-axis, while the in gyro data by peak acceleration in Y-axis.

ggplot(data = df, aes(x = Acceleromete_Z, y = Gyroscope_Y, colour = Road_Condition)) + 
  geom_boxplot()

q1.model <- lm(Acceleromete_Z ~ Road_Condition + Gyroscope_Y + Road_Condition : Gyroscope_Y, data = df)

plot(q1.model, which = 2, add.smooth = FALSE)

plot(q1.model, which = 3, add.smooth = FALSE)
anova(q1.model)

df$Acceleromete_X
df$Acceleromete_Y
df$Acceleromete_Z

# p value < alpha
# so we fail to reject null hypothesis
# so they do have a better reading combined


# Question 2
# Which accelerometer readings better explain the road conditions.
# Road_Condition
# Acceleromete_Z
q2.model <- lm(Acceleromete_Z ~ Road_Condition + Acceleromete_Y +Acceleromete_X, data = df)
q2.model
res = aov(q2.model)
summary(res)

# p-value < alpha 
# a_z is the best

#Question 3

# Experimental data is showing mostly bad road conditions at high speed. Verify this
# claim.
# h0 -> high speed = bad road
# ha -> high speed != bad road
# lm(formula = Weight ~ pH + Calluna + pH:Calluna, data = festuca)
q3.model<-lm(formula = Speed ~ Road_Condition, data = df)
q3.model
res3 = aov(q3.model)
summary(res3)

# p-value < alpha
# high speeds and bad roads corelate


# OR
ggplot(data, aes(x = Speed)) + geom_dotplot(binwidth = 100)
ggplot(data, aes(x = Road_Condition)) + geom_dotplot(binwidth = 2)
cor.test(~ Speed + Road_Condition, method = "pearson", data = data)
# giving the same answer


# Question 4
# time corelates to road_condition

q4.model<-lm(Road_Condition ~ Timer, data = data)
q4.model
res4 = aov(q4.model)
summary(res4)

# p-value < alpha
# high speeds and bad roads corelate


# Question 5


