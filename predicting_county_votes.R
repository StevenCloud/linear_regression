library(dplyr)

election_data <- read.csv("C:/Users/Acer/Documents/Fall 2021/Stat410/410_project/2016_election/county_facts.csv")
election_dictionary <- read.csv("C:/Users/Acer/Documents/Fall 2021/Stat410/410_project/2016_election/county_facts_dictionary.csv")
primary_results <- read.csv("C:/Users/Acer/Documents/Fall 2021/Stat410/410_project/2016_election/primary_results.csv")




#################Initial EDA


pov <- select(election_data, c('fips', 'PVY020213'))


republicans_poverty <- left_join(republicans, pov, by = "fips")
republicans_poverty <- republicans_poverty[complete.cases(republicans_poverty), ]
#Plot of % of republican votes & % of people below poverty line
plot(x = republicans_poverty$PVY020213, y = republicans_poverty$percentage)

#Correlation between poverty level and % of republican votes
cor(republicans_poverty$percentage, republicans_poverty$PVY020213)
#Correlation is -.2902, weak relationship


#Linear regression model of % below poverty line vs. % republican votes
poverty_repub_model <- lm(percentage ~ PVY020213, data = republicans_poverty)
print(poverty_repub_model)
summary(poverty_repub_model)
#B0 = 80.2408, B1 = -0.09146 with p-values <2.2e-16 but an R-squared value of 0.08422

#Plot of fitted values vs. observed values
plot(predict(poverty_repub_model),
     republicans_poverty$percentage,
     xlab = "Predicted Values",
     ylab = "Observed Values")
abline(a = 80.2408,
       b = -0.09146,
       col = "red",
       lwd = 2)
#Line describes data terribly





##MLR with population density + poverty
temp_pov <- select(election_data, c('fips', 'POP060210'))
temp_pov <- temp_pov[(temp_pov$fips %in% republicans_poverty$fips),]


republicans_poverty <- left_join(republicans_poverty, temp_pov, by = "fips")
model_pop_pov <- lm(percentage ~ PVY020213 + POP060210, data = republicans_poverty)
summary(model_pop_pov)
#B0 = 80.96, B1 = -0.923, B2 = -0.0021 with pvalues <2.2e-16 but an R-squred value of 0.1195


##MLR with population density + poverty + age
#election_data$AGE775214
temp_pov <- select(election_data, c("fips", "AGE775214"))
temp_pov <- temp_pov[(temp_pov$fips %in% republicans_poverty$fips),]

republicans_poverty <- left_join(republicans_poverty, temp_pov, by = "fips")

model_pop_pov_age <- lm(percentage ~ PVY020213 + POP060210 + AGE775214, data = republicans_poverty)
summary(model_pop_pov_age)
#B0 = 67.36, B1 = -0.878, B2 = -0.002, B3 = 0.727, pvalue < 2.2e-16 with R-squared 0.1427
plot(predict(model_pop_pov_age),
     republicans_poverty$percentage,
     xlab = "Predicted Values",
     ylab = "Observed Values")
abline(a = 67.36,
       b = -0.878, #B1
       col = "red",
       lwd = 2)
#Plot got weirder, it seems like it would work a lot better if the B0 value changed quite a bit


#MLR with population density + poverty + age + percent with a bachelors degree or higher
election_data$EDU685213

temp_pov <- select(election_data, c("fips", "EDU685213"))
temp_pov <- temp_pov[(temp_pov$fips %in% republicans_poverty$fips),]

republicans_poverty <- left_join(republicans_poverty, temp_pov, by = "fips")

model_pop_pov_age_edu <- lm(percentage ~ PVY020213 + POP060210 + AGE775214 + EDU685213, data = republicans_poverty)
summary(model_pop_pov_age_edu)
#B0 = 88.66, B1 = -1.21, B2 = -0.0013, B3 = 0.45, B4 = -0.57, pvalue < 2.2e-16 with R-squared 0.1837


## MLR with population density + poverty + age + percent with a bachelors+ + % of Black people
election_data$RHI225214

temp_pov <- select(election_data, c("fips", "RHI225214"))
temp_pov <- temp_pov[(temp_pov$fips %in% republicans_poverty$fips),]

republicans_poverty <- left_join(republicans_poverty, temp_pov, by = "fips")

model_pop_pov_age_edu <- lm(percentage ~ PVY020213 + POP060210 + AGE775214 + EDU685213 + RHI225214, data = republicans_poverty)
summary(model_pop_pov_age_edu)
#B0 = 88.274, B1 = -0.91, B2 = -0.001, B3 = 0.3, B4 = -0.54, B5 = -0.27, pvalue very low except age is 0.000459 with R-squared 0.2149


#MLR with population density + poverty + age + percent with a bachelors+ + % of Black people
election_data$VET605213

temp_pov <- select(election_data, c("fips", "VET605213"))
temp_pov <- temp_pov[(temp_pov$fips %in% republicans_poverty$fips),]

republicans_poverty <- left_join(republicans_poverty, temp_pov, by = "fips")

model_pop_pov_age_edu <- lm(percentage ~ PVY020213 + POP060210 + AGE775214 + EDU685213 + RHI225214 + VET605213, data = republicans_poverty)
summary(model_pop_pov_age_edu)
#B0 = 88.63, B1 = -9.184, B2 = -1.07, B3 = 2.5, B4 = -4.66, B5 = -2.649, B6 = -1.143 with insignificant pvalues and R-squared of 0.2219

#####################End of Initial EDA




total_votes <- primary_results %>% 
  group_by(fips) %>% 
  summarise(votes = sum(votes))


republicans <- primary_results %>% 
  group_by(fips, party) %>% 
  summarise(votes = sum(votes))

republicans <- republicans[!republicans$party == "Democrat", ]

republicans <- left_join(total_votes, republicans, by = "fips")

republicans <- republicans[complete.cases(republicans), ]

republicans$percentage <- (republicans$votes.y/republicans$votes.x)*100

#Appending republican percentage to demographic dataset
temp_pov <- select(republicans, c("fips","percentage"))

republicans_demographics <- left_join(election_data, temp_pov, by = "fips")
republicans_demographics <- republicans_demographics[complete.cases(republicans_demographics), ]

#Removing state name & abbreviation
republicans_demographics <- subset(republicans_demographics, select = -c(area_name,state_abbreviation))

#Subsetting for variables of interest to make pairs possible
subsetted_republicans <- select(republicans_demographics, c("AGE775214","SEX255214", "RHI125214", "RHI225214", "EDU635213", "EDU685213", "VET605213", "INC110213", "PVY020213", "POP060210", "percentage"))


#Creating scatterplot matrix of republican_demographic information
pairs(subsetted_republicans)

#Using psych library
library(psych)
pairs.panels(subsetted_republicans, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             lm = TRUE # show regression line
)

#Using Psych Library part 2
subsetted_republicans2 <- select(republicans_demographics, c("RHI725214", "POP645213", "POP815213", "LFE305213", "HSG445213", "HSD310213", "RTN131207", "RHI125214", "VET605213", "PVY020213", "POP060210", "AGE775214","percentage"))
pairs.panels(subsetted_republicans2, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             lm = TRUE # show regression line
)

#Creation of initial MLR Model
model_pop_pov <- lm(percentage ~ AGE775214 + RHI125214 + VET605213 + PVY020213 + POP060210 + POP815213 + HSG445213, data = republicans_demographics)
summary(model_pop_pov)

#MLR Model with removed insignificant predictors
model_upd <- lm(percentage ~ RHI125214 + VET605213 + PVY020213 + POP060210 + POP815213, data = republicans_demographics)
summary(model_upd)

#Model Assessment
par(mfrow = c(2, 2))
plot(model_upd)

#Removal of outlier
republicans_demographics <- republicans_demographics[!republicans_demographics$fips == "36061", ]

#MLR model with outlier removed
model_upd_2 <- lm(percentage ~ RHI125214 + VET605213 + PVY020213 + POP060210 + POP815213, data = republicans_demographics)
summary(model_upd_2)

plot(model_upd_2)

#Testing for multicollinearity
library(mctest)

omcdiag(model_upd_2)

imcdiag(model_upd_2)


#Standardizing coefficients
install.packages('QuantPsyc')
library(QuantPsyc)

modelformula <- percentage ~ RHI125214 + VET605213 + PVY020213 + POP060210 + POP815213

republicans_demographics_stand <- lapply(republicans_demographics[, all.vars(modelformula)], scale)

model_upd_2 <- lm(percentage ~ RHI125214 + VET605213 + PVY020213 + POP060210 + POP815213, data = republicans_demographics_stand)
summary(model_upd_2)


summary(republicans_demographics_stand)

lm(scale(percentage) ~ scale(RHI125214 + VET605213 + PVY020213 + POP060210 + POP815213), data=republicans_demographics)



install.packages("psych")
library(psych)
pairs.panels(republicans_demographics[,3], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)






