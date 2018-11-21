library(readxl)
library(car)

Data = Experimentation_Hypothesis_1_Results <- read_excel("C:/Users/Ugur/OneDrive/Studium UCF/18 Fall - IDS6916/Experiments/Experimentation Results_1 UCAV,2 Targets(1AA,1APC).xlsx", 
                                                          sheet = "T.Test Data")


# 1. Comparing Amount of destroyed targets: (No significant difference)
Sample1 = Data$`Amount of destroyed targets - A`
Sample2 = Data$`Amount of destroyed targets - B`

# Mean Values
mean(Sample1)
mean(Sample2)

# SD Values
sd(Sample1)
sd(Sample2)

# Variance
var(Sample1)
var(Sample2)

#Levene's Test: Comparing the variances of Amount of destroyed targets of experiments A and B.
# p-value greater than 0.05, indicating that there is no significant difference in variances between the groups
y1 <- c(Sample1, Sample2)
group <- as.factor(c(rep(1, length(Sample1)), rep(2, length(Sample2))))
leveneTest(y1, group, center = mean) #p-value 0.1383 --> var.equal = TRUE

# t.test (if p> 0.05 we conclude that there is not enough evidence of a difference between the (true) averages of the two samples at a=0.95)
t.test(Sample1, Sample2, var.equal = TRUE) # p-value = 0.2289 (No significant difference)
#############


# 2 Comparing Amount of autonomous Systems at the end of the mission:(No significant difference)

Sample3 = Data$`Amount of UCAV at the end of the mission - A`
Sample4 = Data$`Amount of UCAV at the end of the mission - B`

# Mean Values
mean(Sample3)
mean(Sample4)

# SD Values
sd(Sample3)
sd(Sample4)

# Variance
var(Sample3)
var(Sample4)

#Levene's Test:
# p-value greater than 0.05, indicating that there is no significant difference in variances between the groups
y1 <- c(Sample3, Sample4)
group <- as.factor(c(rep(1, length(Sample3)), rep(2, length(Sample4))))
leveneTest(y1, group, center = mean) # p-value 0.1255 --> var.equal = TRUE

# t.test (if p> 0.05 we conclude that there is not enough evidence of a difference between the (true) averages of the two samples at a=0.95)
t.test(Sample3, Sample4, var.equal = TRUE) # p-value = 0.4254 (No significant difference)
#############


# 3. Comparing elapsed Time since command received: (No significant difference)

Sample5 = Data$`Elapsed Time (sec) since command received - A`
Sample6 = Data$`Elapsed Time (sec) since command received - B`

# Mean Values
mean(Sample5)
mean(Sample6)

# SD Values
sd(Sample5)
sd(Sample6)

# Variance
var(Sample5)
var(Sample6)

#Levene's Test:
# p-value greater than 0.05, indicating that there is no significant difference in variances between the groups
y1 <- c(Sample5, Sample6)
group <- as.factor(c(rep(1, length(Sample5)), rep(2, length(Sample6))))
leveneTest(y1, group, center = mean) #p-value 0.5497 --> var.equal = TRUE

# t.test (if p> 0.05 we conclude that there is not enough evidence of a difference between the (true) averages of the two samples at a=0.95)
t.test(Sample5, Sample6, var.equal = TRUE) # p-value = 0.4552 (No significant difference)


#############


# 4. Comparing Amount of Used Ammunition: (No significant difference)

Sample7 = Data$`Amount of Used Ammunition - A`
Sample8 = Data$`Amount of Used Ammunition - B`

# Mean Values
mean(Sample7)
mean(Sample8)

# SD Values
sd(Sample7)
sd(Sample8)

# Variance
var(Sample7)
var(Sample8)

#Levene's Test:
# p-value greater than 0.05, indicating that there is no significant difference in variances between the groups
y1 <- c(Sample7, Sample8)
group <- as.factor(c(rep(1, length(Sample7)), rep(2, length(Sample8))))
leveneTest(y1, group, center = mean) #p-value 0.09991 --> var.equal = TRUE

# t.test (if p> 0.05 we conclude that there is not enough evidence of a difference between the (true) averages of the two samples at a=0.95)
t.test(Sample7, Sample8, var.equal = TRUE) # p-value = 0.1754 (No significant difference)

#############


# 5. Comparing Only hostile targets engaged: (No significant difference)
# Obviosly no significant difference, since Data is same

#############


# 6. Comparing Operator was able to supervise all used autonomous systems {True = 1, False = 0}: (No significant difference)
# Obviosly no significant difference, since Data is same

#############


# 7. Comparing Operator perception of cognitive workload: (significant difference)

Sample13 = Data$`Operator perception of  cognitive workload {1..10} - A`
Sample14 = Data$`Operator perception of  cognitive workload {1..10} - B`

# Mean Values
mean(Sample13)
mean(Sample14)

# SD Values
sd(Sample13)
sd(Sample14)

# Variance
var(Sample13)
var(Sample14)

#Levene's Test:
# p-value greater than 0.05, indicating that there is no significant difference in variances between the groups
y1 <- c(Sample13, Sample14)
group <- as.factor(c(rep(1, length(Sample13)), rep(2, length(Sample14))))
leveneTest(y1, group, center = mean) # p-value 0.0001597 --> var.equal = FALSE

# t.test (if p> 0.05 we conclude that there is not enough evidence of a difference between the (true) averages of the two samples at a=0.95)
t.test(Sample13, Sample14, var.equal = FALSE) # p-value = 1.913e-15 (significant difference)
#############

