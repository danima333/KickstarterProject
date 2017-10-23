# Kicstarter Project

# Loading info:
rm(list=ls())
ls()
folder <- "/Users/daniella/Downloads/Kickstarter0717/"
file_list <- list.files(path=folder, pattern="*.csv")
kickstarter <- do.call("rbind", 
                       lapply(file_list, 
                              function(x) 
                              read.csv(paste(folder, x, sep=''),
                              stringsAsFactors = FALSE)))
                              
# Converting all goals to USD

kickstarter$goalUSD <- kickstarter$goal*kickstarter$static_usd_rate


# Difference between goal and pledged

kickstarter$diffGoalPledged <- kickstarter$usd_pledged-kickstarter$goalUSD

# Cleaning the data:

# Category:
kickstarter$category <- sub(pattern=".*slug\\\":\\\"", replace="", kickstarter$category)
kickstarter$category <- sub(pattern="\"}.*", replace="", kickstarter$category)
head(kickstarter$category)

# Creator:
kickstarter$creator <- sub(pattern=".*profile/", replace="", kickstarter$creator)
kickstarter$creator <- sub(pattern="\"},\"api.*", replace="", kickstarter$creator)
head(kickstarter$creator)

# Location:

kickstarter$country <- sub(pattern=".*country\\\":\\\"", replace="", kickstarter$location)
kickstarter$country <- sub(pattern="\\\",\\\".*", replace="", kickstarter$country)
head(kickstarter$country)
kickstarter$city <- sub(pattern=".*displayable_name\\\":\\\"", replace="", kickstarter$location)
kickstarter$city <- sub(pattern="\\\",\\\".*", replace="", kickstarter$city)
head(kickstarter$city)

# CONVERT DATES FROM UNIX
library("dplyr")
kickstarter$created_at <- as.Date(as.POSIXct(kickstarter$created_at, origin="1970-01-01"))
kickstarter$state_changed_at <- as.Date(as.POSIXct(kickstarter$state_changed_at, origin="1970-01-01"))
kickstarter$launched_at <- as.Date(as.POSIXct(kickstarter$launched_at, origin="1970-01-01"))
kickstarter$deadline <- as.Date(as.POSIXct(kickstarter$deadline, origin="1970-01-01"))
class(kickstarter$created_at)
#Convert to number of days
kickstarter$total_time <- abs(kickstarter$deadline - kickstarter$launched_at)/60/60/24;
kickstarter$total_time_till_state_change <- abs(kickstarter$state_changed_at - kickstarter$launched_at)/60/60/24;
kickstarter$prep_time <- abs(kickstarter$launched_at - kickstarter$created_at)/60/60/24;
# convert back to days
kickstarter$total_time <- kickstarter$total_time * 60 * 60 * 24
kickstarter$total_time_till_state_change <- kickstarter$total_time_till_state_change * 60 * 60 * 24
kickstarter$prep_time <- kickstarter$prep_time * 60 * 60 * 24


# Percent of kickstarters successful
(table(kickstarter$state)[4])/(nrow(kickstarter)-table(kickstarter$state[3]))


# Creating dummy variables

kickstarter$failed <- NA

for (i in 1:length(kickstarter$state)) {
	if (kickstarter$state[i] == 'failed'){
	kickstarter$failed[i] <- 1
		} else {kickstarter$failed[i] <- 0
			}
}

head(kickstarter$failed, 2000)

kickstarter$successful <- NA
for (i in 1:length(kickstarter$state)) {
	if (kickstarter$state[i] == 'successful'){
		kickstarter$successful[i] <-1
	} else {
		kickstarter$successful[i] <- 0
	}
}
head(kickstarter$successful, 1000)

kickstarter$canceled <- NA
for (i in 1:length(kickstarter$state)) {
	if (kickstarter$state[i] == 'canceled') {
		kickstarter$canceled[i] <- 1
	} else {
		kickstarter$canceled[i] <- 0
	}
}

kickstarter$staffpicked <- rep(NA, length(kickstarter$staff_pick))
for (i in 1:length(kickstarter$staff_pick)) {
	if (kickstarter$staff_pick[i] == 'true') {
		kickstarter$staffpicked[i] <- 1
	} else if (kickstarter$staff_pick[i]== 'false'){
		kickstarter$staffpicked[i] <- 0
	} else {
		kickstarter$staffpicked[i] <- NA
	}
}

head(kickstarter$total_time)
names(kickstarter)


# LOGISTIC REGRESSION
set.seed(1337)
sample_kickstarter <- sample(1:nrow(kickstarter), 20000)
SVkickstarter <- kickstarter[sample_kickstarter,]
HSkickstarter <- kickstarter[-sample_kickstarter,]
names(SVkickstarter)

NewSVkickstarter <- subset(SVkickstarter, country != 'PY' & country != 'QA')
# Mod.1 Logistic Regression Model
mod.1 <- glm(successful~category+goalUSD+total_time+country, family=binomial(link="logit"), data=HSkickstarter)
NewSVkickstarter$total_time <- as.numeric(NewSVkickstarter$total_time)
fittest1<- data.frame(category=NewSVkickstarter$category, 
                      goalUSD=NewSVkickstarter$goalUSD, 
                      total_time = NewSVkickstarter$total_time, 
                      country = NewSVkickstarter$country)
fitted.resultsa <- predict(mod.1, newdata=subset(fittest1, country != 'PY' & country != 'QA'), type="response")
fitted.results1 <- ifelse(fitted.resultsa > 0.6, 1, 0)
misClasificError1 <- mean(fitted.results1 != NewSVkickstarter$successful)
# How accurate is this model?
print(paste('Accuracy',1-misClasificError1))

null.mod <- glm(successful~1, family=binomial(link=logit, data=HSkickstarter)







# ROC plot for Mod.1
p <- fitted.resultsa
pr <- prediction(p, NewSVkickstarter$successful)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

# AUC is best when closer to 1 than to 0.5
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
aucconfusionMatrix(fitted.results1, cutoff = 0.5)





mod.2 <- glm(successful~category+goalUSD+country, family=binomial(link="logit"), data=HSkickstarter)
fittest2<- data.frame(category=NewSVkickstarter$category, goalUSD=NewSVkickstarter$goalUSD, country=NewSVkickstarter$country)
fitted.results2<- predict(mod.2, fittest2, type="response")
fitted.results2 <- ifelse(fitted.results2 > 0.5, 1, 0)
misClasificError2 <- mean(fitted.results2 != NewSVkickstarter$successful)
print(paste('Accuracy',1-misClasificError2))


mod.3 <- glm(successful~category+goalUSD, family=binomial(link="logit"), data=HSkickstarter)

fittest3<- data.frame(category=SVkickstarter$category, goalUSD=SVkickstarter$goalUSD)
fitted.results3<- predict(mod.3, fittest3, type="response")
fitted.results3 <- ifelse(fitted.results3 > 0.5, 1, 0)
misClasificError3 <- mean(fitted.results3 != SVkickstarter$successful)
print(paste('Accuracy',1-misClasificError3))

mod.4 <- glm(successful~category+country, family=binomial(link="logit"), data=HSkickstarter)
fittest4<- data.frame(category=NewSVkickstarter$category, country=NewSVkickstarter$country)
fitted.results4<- predict(mod.4, fittest4, type="response")
fitted.results4 <- ifelse(fitted.results4 > 0.5, 1, 0)
misClasificError4 <- mean(fitted.results4 != NewSVkickstarter$successful)
print(paste('Accuracy',1-misClasificError4))


# Graphics:

success_subset <- subset(kickstarter, successful = 1)
country_success <- aggregate(usd_pledged~country, FUN = median, success_subset, na.rm = TRUE)
head(country_success)
country_success_order <- country_success[order(-country_success$usd_pledged),]
country_success_order

success_subset <- subset(kickstarter, successful = 1)
country_success <- aggregate(usd_pledged~country, FUN = sum, success_subset, na.rm = TRUE)
head(country_success)
country_success_order <- country_success[order(-country_success$usd_pledged),]
country_success_order

# New clean csv
#NOT WORKING: write.csv(kickstarter, file="kickstarterclean.csv")
