#remove all variables stored
rm(list=ls)
install.packages("Hmisc")
library("Hmisc")

data <- read.csv(file.choose())
head(data)
describe(data)

data$death_dummy <- as.integer(data$death != 0)
unique(data$death_dummy) #0 (no deaths), 1 (death)

#death rate
sum(data$death_dummy) / nrow(data) # 0.05806452 (5.8%)

#AGE
#claim: people who die are older. Can we prove this?
dead = subset(data, death_dummy == 1)
alive = subset(data, death_dummy == 0)
mean(dead$age, na.rm = TRUE) #68.58621
mean(alive$age, na.rm = TRUE) #48.07229
#is this statistically significant?
t.test(alive$age, dead$age, alternative="two.sided", conf.level = 0.95)
#data:  alive$age and dead$age
#t = -10.839, df = 72.234, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -24.28669 -16.74114
#sample estimates:
#  mean of x mean of y 
#48.07229  68.58621 
t.test(alive$age, dead$age, alternative="two.sided", conf.level = 0.99)
#data:  alive$age and dead$age
# t = -10.839, df = 72.234, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 99 percent confidence interval:
#   -25.52122 -15.50661
# sample estimates:
#   mean of x mean of y 
# 48.07229  68.58621 

#YES, it is statististically significant
#normally, if p-value < 0.05, we reject null hypothesis
# here, p-value ~ 0, so we reject the null hypothesis and
#we conclude that people that have died are indeed much older, than the people
#who do not die from the corona virus


#GENDER
#claim: gender has no effect
men = subset(data, gender == "male")
women = subset(data, gender == "female")
mean(men$death_dummy, na.rm = TRUE) #8.5%
mean(women$death_dummy, na.rm = TRUE) #3.7%
#is this statistically significant?
t.test(men$death_dummy, women$death_dummy, alternative="two.sided", conf.level = 0.95)
# data:  men$death_dummy and women$death_dummy
# t = 3.084, df = 894.06, p-value = 0.002105
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.01744083 0.07849151
# sample estimates:
#   mean of x  mean of y 
# 0.08461538 0.03664921 

t.test(men$death_dummy, women$death_dummy, alternative="two.sided", conf.level = 0.99)
# data:  men$death_dummy and women$death_dummy
# t = 3.084, df = 894.06, p-value = 0.002105
# alternative hypothesis: true difference in means is not equal to 0
# 99 percent confidence interval:
#   0.007817675 0.088114665
# sample estimates:
#   mean of x  mean of y 
# 0.08461538 0.03664921 

#99% confidence: men have from 0.8% to 8.8% higher change of dying
# p-value = 0.002 < 0.05, so this is statisticaly significant



