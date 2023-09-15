library(foreign)
library(dplyr)
library(ggplot2)
library(glmnet)
library(car)
library(ggfortify)
library(lmtest)

mydata <- read.spss("Downloads/ZA7505_v4-0-0.sav", to.data.frame = TRUE)

survey_data <- mydata[c("cntry_y", "reg_iso", "size_5c", 
                        "X001", "X003", "X025A_01", "X028", "X052",
                         "X047_WVS7" )]

View(survey_data) #still with NA 

survey_mexico <- subset(survey_data, cntry_y == "Mexico (2018)")
#Only Mexico observations
survey_mexico_clean<- na.omit(survey_mexico, cols = "X052")

# Check the levels and labels of variables of interest
str(survey_mexico_clean$X025A_01)
levels(survey_mexico_clean$reg_iso)
levels(survey_mexico_clean$size_5c)
levels(survey_mexico_clean$X001)
levels(survey_mexico_clean$X003)
levels(survey_mexico_clean$X025A_01)
levels(survey_mexico_clean$X028)
levels(survey_mexico_clean$X052)
levels(survey_mexico_clean$X047_WVS7)

# We need to re code the variables in such a way we are able to perform
# quantitative analysis on them
class(survey_mexico_clean$X025A_01)
# Create a new data set
survey_mexico_recoded <- survey_mexico_clean

# Rearrange the levels of employment status, starting from the category with the 
# lowest value recorded for the mean of income level.

survey_mexico_recoded %>%
  group_by(X028) %>%
  summarise(mean_income_level = mean(X047_WVS7, na.rm = TRUE))

survey_mexico_recoded$X028 <- factor(survey_mexico_recoded$X028, 
                                     levels = c("Unemployed", "Housewife (not otherwise employed)",
                                                "Retired/pensioned", "Full time (30h a week or more)",
                                                "Part time (less then 30 hours a week)",
                                                "Self employed", "Student"))
# Check the order of the levels
levels(survey_mexico_recoded$X025A_01)

ggplot(survey_mexico_recoded, aes(x = X025A_01, y = X047_WVS7, fill = X025A_01)) +
  geom_boxplot() +
  labs(x = "Educational Level", y = "Income Level", fill = "Educational Level") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.title.x = element_text(size = 14, face = "bold"))

ggplot(survey_mexico_recoded, aes(x = X025A_01, y = X047_WVS7)) +
  stat_summary(fun = mean, geom = "bar", fill = "blue") +
  labs(x = "Educational Level", y = "Mean Income Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#Recode income level
survey_mexico_recoded$X047_WVS7 <- ifelse(survey_mexico_recoded$X047_WVS7 == "Lower step", 1,
 ifelse(survey_mexico_recoded$X047_WVS7 == "second step", 2,
 ifelse(survey_mexico_recoded$X047_WVS7 == "Third step", 3,
 ifelse(survey_mexico_recoded$X047_WVS7 == "Fourth step", 4,
 ifelse(survey_mexico_recoded$X047_WVS7 == "Fifth step", 5,
 ifelse(survey_mexico_recoded$X047_WVS7 == "Sixth step", 6,
 ifelse(survey_mexico_recoded$X047_WVS7 == "Seventh step", 7,
 ifelse(survey_mexico_recoded$X047_WVS7 == "Eight step", 8,
 ifelse(survey_mexico_recoded$X047_WVS7 == "Nineth step", 9, 10)))))))))
as.numeric(survey_mexico_recoded$X047_WVS7)


# Recode cntry_y
survey_mexico_recoded$cntry_y <- ifelse(survey_mexico_recoded$cntry_y == "Mexico (2018)", 1, NA)

# Recode reg_iso
survey_mexico_recoded$reg_iso <- as.numeric(as.factor(survey_mexico_recoded$reg_iso))

# Recode size_5c
survey_mexico_recoded$size_5c <- as.numeric(as.factor(survey_mexico_recoded$size_5c))

# Recode X001
survey_mexico_recoded$X001 <- ifelse(survey_mexico_recoded$X001 == "Female", 1, 2)

# Recode X003 -  The participants over 82 were deleted 
# because some complications during the coding 
survey_mexico_recoded <- subset(survey_mexico_recoded, !(X003 == "82 and older" | X003 == "99"))
survey_mexico_recoded$X003 <- as.numeric(survey_mexico_recoded$X003)

# Recode X025A_01
survey_mexico_recoded$X025A_01 <- as.numeric(as.factor(survey_mexico_recoded$X025A_01))

# Recode X028
survey_mexico_recoded$X028 <- as.numeric(as.factor(survey_mexico_recoded$X028))

# Recode X052
survey_mexico_recoded$X052 <- as.numeric(as.factor(survey_mexico_recoded$X052))

#Change names of variables 
names(survey_mexico_recoded)[names(survey_mexico_recoded) == "cntry_y"] <- "Country"
names(survey_mexico_recoded)[names(survey_mexico_recoded) == "size_5c"] <- "Size"
names(survey_mexico_recoded)[names(survey_mexico_recoded) == "X001"] <- "Gender"
names(survey_mexico_recoded)[names(survey_mexico_recoded) == "X003"] <- "Age"
names(survey_mexico_recoded)[names(survey_mexico_recoded) == "X025A_01"] <- "Education_level"
names(survey_mexico_recoded)[names(survey_mexico_recoded) == "X028"] <- "Employment_status"
names(survey_mexico_recoded)[names(survey_mexico_recoded) == "X047_WVS7"] <- "Income_level"
names(survey_mexico_recoded)[names(survey_mexico_recoded) == "reg_iso"] <-"State"
names(survey_mexico_recoded)[names(survey_mexico_recoded) == "X052"] <-"Employment_Sector"

# Apply the Linear Regression Model 
model <- lm(Income_level ~ Education_level + Size +Age + Gender + Employment_status + State + Employment_Sector, data = survey_mexico_recoded)

# Perform an ANOVA test to assess overall model significance
anova(model)

# Check the coefficients of the predictor variables
summary(model)

# NEW MODELS 
# MODEL 2: Example code to add interaction terms and log
# Example code to add interaction terms:
model2 <- lm(Income_level ~ Education_level * Age + Size + Gender + Employment_status + State + Employment_Sector, data = survey_mexico_recoded)
summary(model2)
# MODEL 3: Interaction effect between education level and employment status:
model3 <- lm(Income_level ~ Education_level + Size + Age + Gender + Employment_status + State + Employment_Sector + Education_level:Employment_status, data = survey_mexico_recoded)
summary(model3)
# MODEL 4: Interaction effect between education level and state:
model4 <- lm(Income_level ~ Education_level + Size + Age + Gender + Employment_status + State + Employment_Sector + Education_level:State, data = survey_mexico_recoded)
summary(model4)
# MODEL 5: Logarithmic transformation of the dependent variable:
survey_mexico_recoded$log_income <- log(survey_mexico_recoded$Income_level)
model5 <- lm(log_income ~ Education_level + Size + Age + Gender + Employment_status + State + Employment_Sector, data = survey_mexico_recoded)
summary(model5)

#BEST MODEL SO FAR: MODEL 1

# Assess the normality of residuals
shapiro.test(resid(model))

# Graphic representation
ggplot(data.frame(resid = resid(model)), aes(sample = resid)) +
  geom_qq() +
  geom_abline(color = "red") +
  ggtitle("Q-Q Plot of Residuals") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  theme_minimal()

# Assess the homoscedasticity of residuals
# run the Breusch-Pagan test and store the output
bptest(model)

# Assess the multicollinearity of the predictor variables
vif(model)

# create a data frame with actual and predicted values of income level and education level
predicted <- fitted(model)
actual <- survey_mexico_recoded$Income_level
education <- survey_mexico_recoded$Education_level
results <- data.frame(actual, predicted, education)

# create a scatter plot with color-coded points for different education levels

ggplot(results, aes(x = actual, y = predicted, color = factor(education))) +
  geom_point() +
  xlab("Actual Income Level") +
  ylab("Predicted Income Level") +
  scale_color_discrete(name = "Education Level") +
  ggtitle("Predicted vs Actual Income Levels by Education Level") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(1, 10), breaks = seq(1, 10, 1))+
  scale_x_continuous(name="Income level", breaks=1:10, labels=1:10)+
  theme_minimal()

# Augmented plot 
ggplot(results, aes(x = actual, y = predicted, color = factor(education))) +
  geom_point() +
  xlab("Actual Income") +
  ylab("Predicted Income Level") +
  scale_color_discrete(name = "Education Level") +
  ggtitle("Predicted vs Actual Income Levels by Education Level") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(name="Income level", breaks=1:10, labels=1:10)+
  theme_minimal()

# Regression line added
slope <- summary(model)$coefficients[2]

ggplot(survey_mexico_recoded, aes(x = Education_level, y = Income_level)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_point(alpha = 0) +
  xlab("Education level") +
  ylab("Income level") +
  ggtitle("Association between education level and income level")+
  theme_minimal() +
  labs(subtitle = "Slope = 0.372")

 #Hypothesis 2 

#1. Create a binary variable indicating rural or urban area
survey_mexico_recoded$Area <- ifelse(survey_mexico_recoded$State %in% c(587, 591, 599, 603, 606, 609, 611), "Rural", "Urban")

# Fit a linear regression model with the interaction between Area and other predictors
modelh2 <- lm(Income_level ~ Education_level + Age + Gender + Employment_status + Area, data = survey_mexico_recoded)

# Obtain ANOVA table
anova(modelh2)

# Obtain coefficient summary
summary(modelh2)

# Shapiro-Wilk test for normality of residuals
shapiro.test(residuals(modelh2))

# Graphic representation
ggplot(data.frame(resid = resid(modelh2)), aes(sample = resid)) +
  geom_qq() +
  geom_abline(color = "red") +
  ggtitle("Q-Q Plot of Residuals") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  theme_minimal()

# Perform studentized Breusch-Pagan test for heteroscedasticity
bptest(modelh2)

# Calculate variance inflation factors
vif(modelh2)


  # create a data frame with actual and predicted values of income level and education level
  predictedh2 <- fitted(modelh2)
actual <- survey_mexico_recoded$Income_level
area <- survey_mexico_recoded$Area
results <- data.frame(actual, predicted, area)

# create a scatter plot with color-coded points for different education levels

ggplot(results, aes(x = actual, y = predicted, color = factor(area))) +
  geom_point() +
  xlab("Actual Income Level") +
  ylab("Predicted Income Level") +
  scale_color_discrete(name = "Area") +
  ggtitle("Predicted vs Actual Income Levels by Area") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(1, 10), breaks = seq(1, 10, 1))+
  scale_x_continuous(name="Income level", breaks=1:10, labels=1:10)+
  theme_minimal()

# Augmented plot 

ggplot(results, aes(x = actual, y = predicted, color = factor(area))) +
  geom_point(alpha = 0.3) +
  xlab("Actual Income Level") +
  ylab("Predicted Income Level") +
  scale_color_discrete(name = "Area") +
  ggtitle("Predicted vs Actual Income Levels by Area") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(1, 7), breaks = seq(1, 10, 1))+
  scale_x_continuous(name="Income level", breaks=1:10, labels=1:10)+
  theme_minimal()

# Hypothesis 3: 

# “Individuals employed in private institutions have higher incomes than
# those employed in public institutions, controlling for other factors such as
# age, education level, gender, and employment status.”

# Recode the levels 2 and 3 as private institutions, and level 1 as the public sector
survey_mexico_recoded$Employment_Sector <- 
  ifelse(survey_mexico_recoded$Employment_Sector %in% c(2, 3), "Private Sector",
         "Public Sector")

# Fit a linear regression model with Employment_Sector as a factor variable
modelh3 <- lm(Income_level ~ Age + Education_level + Gender + Employment_status + Employment_Sector, data = survey_mexico_recoded)

# Obtain ANOVA table
anova(modelh3)

# Obtain coefficient summary
summary(modelh3)

# Shapiro-Wilk test for normality of residuals
shapiro.test(residuals(modelh3))

# Graphic representation
ggplot(data.frame(resid = resid(modelh3)), aes(sample = resid)) +
  geom_qq() +
  geom_abline(color = "red") +
  ggtitle("Q-Q Plot of Residuals") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  theme_minimal()

# Perform studentized Breusch-Pagan test for heteroscedasticity
bptest(modelh3)

# Calculate variance inflation factors
vif(modelh3)

# create a data frame with actual and predicted values of income level and education level
predictedh3 <- fitted(modelh3)
actual <- survey_mexico_recoded$Income_level
sector <- survey_mexico_recoded$Employment_Sector
results <- data.frame(actual, predicted, sector)

# create a scatter plot with color-coded points for different education levels

ggplot(results, aes(x = actual, y = predicted, color = factor(sector))) +
  geom_point(alpha = 0.2) +
  xlab("Actual Income Level") +
  ylab("Predicted Income Level") +
  scale_color_discrete(name = "sector") +
  ggtitle("Predicted vs Actual Income Levels by Employment Sector") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(1, 10), breaks = seq(1, 10, 1))+
  scale_x_continuous(name="Income level", breaks=1:10, labels=1:10)+
  theme_minimal()

# Section 6: boxplot for Education Level and Income Level

# Boxplot for Education Level
education_level_labels <- c("Less than primary", "Primary", "Lower Secondary",
                            "Upper Secondary", "post-secondary non tertiary", 
                            "Short-cycle tertiary", "bachelor or equivalent",
                            "Master or equivalent", "Doctoral or equivalent") 

ggplot(survey_mexico_recoded, aes(x = factor(Education_level), y = Income_level, fill = factor(Education_level))) +
  geom_boxplot() +
  labs(x = "Educational Level", y = "Income Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_x_discrete(labels = education_level_labels)

# Boxplot for Employment Status 
employment_status_labels <- c(
  "Full time", "Part time", "Self employed", "Retired/pensioned",
  "Housewife", "Student", "Unemployed", "Other")

ggplot(survey_mexico_recoded, aes(x = factor(Employment_status), y = Income_level, fill = factor(Employment_status))) +
  geom_boxplot() +
  labs(x = "Employment Status", y = "Income Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = employment_status_labels)
