data <- read.csv("C:/Users/Minahil Ashfaq/Downloads/heart_failure_clinical_records_dataset .csv")#read the data
head(data)
View(data)



#Question 1

table1=table(diabetes,high_blood_pressure)

#labeling data
colnames(table1)[colnames(table1) == "0"] = "Low BP"
colnames(table1)[colnames(table1) == "1"] = "High BP"
rownames(table1)[rownames(table1) == "1"] = "Diabetes"
rownames(table1)[rownames(table1) == "0"] = "Not Diabetes"



barplot(table1,legend=T,beside=T,main="Diabetes vs High Blood Pressure")

addmargins(table1)


# high_blood_pressure
# diabetes         low BP    High BP Sum
# No Diabetes        112          62 174
# Diabetes            82          43 125
# Sum                194         105 299


chisq.test(diabetes,high_blood_pressure)

# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  diabetes and high_blood_pressure
# X-squared = 0.0094767, df = 1, p-value = 0.9224


#we fail to reject the null hypothesis


#Question 2
names(data)[names(data) == 'anaemia'] <- 'anemia'
table2=table(sex,anemia)


#labeling data
colnames(table2)[colnames(table2) == "0"] = "NO (Anemia)"
colnames(table2)[colnames(table2) == "1"] = "Yes (Anemia)"
rownames(table2)[rownames(table2) == "1"] = "Male"
rownames(table2)[rownames(table2) == "0"] = "Female"
barplot(table2,legend=T,beside=T,main="Anaemia ,Gender ")

addmargins(table2)

# Anemia
# sex      NO (Anemia) Yes (Anemia) Sum
# Female          53           52 105
# Male           117           77 194
# Sum            170          129 299

chisq.test(sex,anemia)#we fail to reject the null hypothesis

# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  sex and anemia
# X-squared = 2.2995, df = 1, p-value = 0.1294



#Question 4
new_data<- subset(data ,smoking == 1)

death = new_data$DEATH_EVENT
diabetes = new_data$diabetes

table4=table(death,diabetes)

#labeling data
colnames(table4)[colnames(table4) == "0"] = "NOT Diabetes"
colnames(table4)[colnames(table4) == "1"] = "Diabetes"
rownames(table4)[rownames(table4) == "1"] = "Death"
rownames(table4)[rownames(table4) == "0"] = "No Death"


barplot(table4,legend=T,beside=T,main="Diabetes vs Surviver")



addmargins(table4)

# NOT Diabetes Diabetes Sum
# Female          50       55 105
# Male           124       70 194
# Sum            174      125 299

chisq.test(death,diabetes)

# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  death and diabetes
# X-squared = 1.0191, df = 1, p-value = 0.3127

#we reject the null hypothesis



#Question 5
gender = data$sex
diabetes = data$diabetes

table5=table(gender,diabetes)


#labeling data
colnames(table5)[colnames(table5) == "0"] = "NOT Diabetes"
colnames(table5)[colnames(table5) == "1"] = "Diabetes"
rownames(table5)[rownames(table5) == "1"] = "Male"
rownames(table5)[rownames(table5) == "0"] = "Female"
barplot(table5,legend=T,beside=T,main="Diabetes vs Gender")



addmargins(table5)

# NOT Diabetes Diabetes Sum
# Female          50       55 105
# Male           124       70 194
# Sum            174      125 299

chisq.test(gender,diabetes)

# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  gender and diabetes
# X-squared = 6.7839, df = 1, p-value = 0.009199

#we reject the null hypothesis




