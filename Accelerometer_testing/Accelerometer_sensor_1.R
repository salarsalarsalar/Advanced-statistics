install.packages("tidyverse")
install.packages("ggplot2")
library(ggplot2)
library(tidyverse) 

data <- read.csv("C:/Users/Minahil Ashfaq/OneDrive/Desktop/Advance stats project/TrainingData.csv")#read the data
data
head(data)
View(data)
summary(data)


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

#  We used anova one way test because there was two independent and one dependent variable which was categorical
anova(q1.model)
