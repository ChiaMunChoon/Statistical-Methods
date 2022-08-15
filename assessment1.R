library(car) # Perform logistic regression and other tests
library(dplyr) # data manipulation
library(tidyr) # create data frame
library(ggplot2) # create visualization
library(ggpubr) # formatting plots and graphs
library(DataExplorer) # Descriptive statistic functions (ex. introduce(), plot_correlation())
library(dlookr) # Data Exploration (ex. QQplots)
library(stats) # chi-square test
library(nortest) # Normality test 
library(rstatix) # t-test, Wilcox Test, summary statistics
library(caTools) # For logistic Regression 
library(caret) # Confusion Matrix
library(InformationValue) # Optimal Cutoff
library(ROCR) # ROC curve

# Import dataset
default <- read.csv("D:\\Desktop\\Master\\Sem 4 - Statistical Methods for Data Science\\dataset\\UCI_Credit_Card.csv")


# Create another data frame to avoid making permanent changes to the original data frame
df <- default

# list all variables in the dataset
ls(df)

# To check the dimension of the data
dim(df)

# Check data type of each variable
str(df)

# To find the basic information of the data frame and missing values 
introduce(df)

# Basic Data Cleaning
# Convert column name of the response variable to "default_payment" for the ease of coding 
# and convert it to categorical type

colnames(df)[colnames(df)=="default.payment.next.month"] <- "default_payment"
head(df)

# Change PAY_0 into PAY_1 to avoid confusion 
colnames(df)[colnames(df)=="PAY_0"] <- "PAY_1"
head(df)

# Drop the ID variable as it is an unique identifier for each cardholder and holds no meaningful information
df <- subset(df, select = -c(ID))

# Convert all demographic information and response variable from continuous to categorical 
# as it is now in an incorrect data type.

# All categorical variables will be converted to factor data type 

# Based on the data dictionary from the UCI 
# GENDER 1 = Male, 2 = Female 
df$SEX<-ifelse(df$SEX==1,"Male","Female")

# Education level 1 = Graduate School, 2 = University, 3 = High school, 4 = others, 0,5,6 = Unknown
# since we do not have information on 0,5,6, we can include all those as "Others" to reduce the group

count(df, vars = EDUCATION)
df$EDUCATION<-ifelse(df$EDUCATION==1,"Graduate School",ifelse(df$EDUCATION==2,"University",
                                                              (ifelse(df$EDUCATION==3,"High School","Others"))))                                                       

# Marital Status 1 = Married, 2 = Single, 3 = Others
df$MARRIAGE<-ifelse(df$MARRIAGE==1,"Married",ifelse(df$MARRIAGE==2,"Single","Others"))

# Changing the default_payment (response variable) into factor data type with custom labels.
df$default_payment<- factor(df$default_payment,labels = c("Non-defaulter", "Defaulter"))

# Changing all categorical into factor data type
Cvariables<-c("SEX","EDUCATION","MARRIAGE")
df[Cvariables]<-lapply(df[Cvariables],as.factor)

# Rechecking data type
str(df)
head(df)

# Exploratory Data Analysis / Descriptive Summary
# The total count of observation for groups in EDUCATION
count(df, vars = EDUCATION)
count(df, vars = SEX)
count(df, vars = MARRIAGE)
count(df, vars = default_payment)

# Bar graph for each demographic variables
educationBar <- table(df$EDUCATION)
barplot(educationBar)

sexBar <- table(df$SEX)
barplot(sexBar)

marriageBar <- table(df$MARRIAGE)
barplot(marriageBar)


# The distribution plot of cardholders who defaulted vs non-default

# Default vs Non-default
defaulter <- df%>%
  group_by(default_payment)%>%
  summarise(n = n())%>%
  mutate(percentage = n/sum(n)*100)%>%
  ggplot(aes(default_payment, n, fill=default_payment))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=n), vjust=-0.3, hjust = 1, size=4)+
  geom_text(aes(label=paste0("( ",round(percentage,2),"% )")),
            vjust=-0.3, hjust = -0.1, size=4)+
  ylab("Total Observations")+
  xlab("")+ 
  scale_fill_manual(name = "Default payment",
                    values = c("Non-defaulter" = "#00BFC4", "Defaulter" = "#F8766D"))+
  theme(legend.background = element_rect(fill="white",
                                         size=0.5,
                                         linetype="solid", 
                                         colour ="black"))

# The distribution of EDUCATION LEVEL vs Defaulter
Education <- df%>%
  group_by(EDUCATION,default_payment)%>%
  summarise(n = n(), .groups = 'drop')%>%
  mutate(percentage = n/sum(n)*100)%>%
  ggplot(aes(fill=default_payment, y=n,
             x=factor(EDUCATION, levels = c("Others", "High School", "University", "Graduate School", "Unknown")))) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n), position = position_dodge(0.9), vjust = 1)+
  scale_fill_manual(name = "Default payment",
                    values = c("Non-defaulter" = "#00BFC4", "Defaulter" = "#F8766D"))+
  xlab("Education Level")+
  ylab("Total Observations")+
  theme(legend.background = element_rect(fill="white",
                                         size=0.5,
                                         linetype="solid", 
                                         colour ="black"))

# The distribution of Marital Status vs Defaulter
Marriage <- df%>%
  group_by(MARRIAGE,default_payment)%>%
  summarise(n = n(), .groups = 'drop')%>%
  mutate(percentage = n/sum(n)*100)%>%
  ggplot(aes(fill=default_payment, y=n,
             x=factor(MARRIAGE, levels = c("Married","Single","Others")))) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n), position = position_dodge(0.9), vjust = 1)+
  scale_fill_manual(name = "Default payment",
                    values = c("Non-defaulter" = "#00BFC4", "Defaulter" = "#F8766D"))+
  xlab("Marital Status")+
  ylab("Total Observations")+
  theme(legend.background = element_rect(fill="white",
                                         size=0.5,
                                         linetype="solid", 
                                         colour ="black"))

# The distribution of gENDER vs Defaulter
Gender <- df%>%
  group_by(SEX,default_payment)%>%
  summarise(n = n(), .groups = 'drop')%>%
  mutate(percentage = n/sum(n)*100)%>%
  ggplot(aes(fill=default_payment, y=n,
             x=factor(SEX, levels = c("Male","Female")))) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=n), position = position_dodge(0.9), vjust = 1)+
  scale_fill_manual(name = "Default payment",
                    values = c("Non-defaulter" = "#00BFC4", "Defaulter" = "#F8766D"))+
  xlab("Gender")+
  ylab("Total Observations")+
  theme(legend.background = element_rect(fill="white",
                                         size=0.5,
                                         linetype="solid", 
                                         colour ="black"))

# Combine all plots in 1 matrix/diagram
ggarrange(defaulter, Gender, Education, Marriage, ncol = 2, nrow = 2)

# Histogram for all numeric variables
df %>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +   # In separate panels
  geom_density()  

# QQ plot for all numeric variables
numeric_fields<-c("LIMIT_BAL","AGE","BILL_AMT1","BILL_AMT2","BILL_AMT3",
                  "BILL_AMT4","BILL_AMT5","BILL_AMT6",
                  "PAY_AMT1","PAY_AMT2","PAY_AMT3","PAY_AMT4","PAY_AMT5","PAY_AMT6")

plot_qq_numeric(
  df_numeric,
  col_point = "steelblue",
  col_line = "black",
  title = "Q-Q plot by numerical variables",
  each = FALSE,
  typographic = TRUE,
  base_family = NULL
)

# Correlation Heatmap
df_numeric<-subset(df, select=numeric_fields)
plot_correlation(df_numeric)

#-------------Statistical Inference----------------#

# Chi-Square test of Independence

# Test significant relationship between all demographic information and default payment using Chi-squared test of Independence 
CatVariables <- subset(df,select = c(default_payment,SEX, EDUCATION, MARRIAGE))

# lapply function was used to apply the chi.sq.test() function to all categorical variables
CHIS <- lapply(CatVariables[,-1], function(x) chisq.test(CatVariables[,1], x))

CHIS

do.call(rbind, CHIS)[,c(1,3)]

# Assumptions of Chi-square test

# Assumption 1: Both variables must be categorical

# This assumption is met as the response and predictor variables were changed into factor data type during the data cleaning phase.

# Assumption 2: All observations are independent

# Assumption 3: Cells in the contingency table are mutually exclusive

# Assumption 2 & 3 are met as the data was obtained from a real bank and each observation belonged to each individual cardholder

# Assumption 4: Expected value of cells should be 5 or greater in at least 80% of the cells.
# Expected value can be checked by getting the expected table from each variable tested in chi-square test
print(CHIS$SEX$expected)
print(CHIS$EDUCATION$expected)
print(CHIS$MARRIAGE$expected)


# Two sample t-test 

# lapply function was used to apply the t.test() function to all continuous variables
TwoSampleTest <- lapply(df[,c("AGE","PAY_AMT1","PAY_AMT2","PAY_AMT3","PAY_AMT4","PAY_AMT5","PAY_AMT6")], function(x) t.test(x ~ df$default_payment, var.equal = TRUE))
TwoSampleTest
do.call(rbind,TwoSampleTest)[,c(1,3,7,9)]

# Subset all continuous variable 
NumericVar <- subset(df,select = -c(SEX,EDUCATION,MARRIAGE,PAY_1,PAY_2,PAY_3,PAY_4,PAY_5,PAY_6))

# To get the mean and standard deviation for all continuous variables for different groups
NumericVar %>%
  group_by(default_payment) %>%
  get_summary_stats(, type = "mean_sd") %>%
  print(n=40)

# To get the mean and standard deviation for all continuous variables
get_summary_stats(NumericVar, type = "mean_sd")


# Assumptions of Two sample t-test

# Assumption 1: The data must be continuous.
# Assumption 2: All observations are independent.
# Assumption 3: Data in each group must be obtained via a random sample from the population.

# Assumption 4: Data in each group is normally distributed
# Anderson-Darling Test was used to test whether the data is normally distributed or not.

ad.test(df$PAY_AMT1)

# ad.test() output: p-value = 2.2e-16, hence, the data is not normally distributed

# Assumption 5: The variances for the two independent groups are equal.

# Comparing the variances of two groups (Non-defaulter vs Defaulter) using VarTest() function
VarTest <- lapply(df[,c("AGE","PAY_AMT1","PAY_AMT2","PAY_AMT3","PAY_AMT4","PAY_AMT5","PAY_AMT6")], function(x) var.test(x ~ df$default_payment))

VarTest

do.call(rbind, VarTest)[,c(1,3,5)]

# VarTest() Output: all p-values were less than 0.05; hence, the variances of both groups were not equal

# If the variances is not equal then use Welch's test

# Wilcoxon Test was used as normality assumption was violated

wilcox <- lapply(df[,c("AGE","PAY_AMT1","PAY_AMT2","PAY_AMT3","PAY_AMT4","PAY_AMT5","PAY_AMT6")], function(x) wilcox.test(x ~ df$default_payment)) 

do.call(rbind,wilcox)[,c(1,3)]


# Welch t-test was used as Homogeneity of variance was violated

welch <- lapply(df[,c("AGE","PAY_AMT1","PAY_AMT2","PAY_AMT3","PAY_AMT4","PAY_AMT5","PAY_AMT6")], function(x) t.test(x ~ df$default_payment, var.equal = FALSE))

do.call(rbind,welch)[,c(1,3)]


# Multiple Logistic Regression

# Splitting the data set into 8:2 ratio for training and test data set

split <- sample.split(df, SplitRatio = 0.8)

train_reg <- subset(df, split =="TRUE")
test_reg <- subset(df, split =="FALSE")

# Training the logistic regression model
model <- glm(formula = default_payment~.-(BILL_AMT1+BILL_AMT2+BILL_AMT3+
                                              BILL_AMT4+BILL_AMT5), train_reg, family = "binomial")

# Summary of the trained model 
summary(model)

# Predict test data based on the model
predict_reg <- predict(model, test_reg, type = "response")
predict_reg  

# Changing the probabilities of the predicted results into 1 or 0
predict_reg <- ifelse(predict_reg > 0.5, 1,0)
predict_reg


# Convert defaults from "Defaulter" and "Non-defaulter" into 1 and 0
test_reg$default_payment <-ifelse(test_reg$default_payment=="Defaulter", 1, 0)

# find the optimal cutoff probability to use the maximize accuracy
optimal <- optimalCutoff(test_reg$default_payment, predict_reg)

# Evaluating model accuracy
# Create confusion matrix
caret::confusionMatrix(as.factor(predict_reg), as.factor(test_reg$default_payment))
predict_reg


# calculate sensitivity
sensitivity(test_reg$default_payment, predict_reg)

# calculate misclassification error rate
misClassError(test_reg$default_payment, predict_reg, threshold=optimal)

# Calculate the accuracy of the model
missing_classerr <- misClassError(test_reg$default_payment, predict_reg, threshold=optimal)
print(paste('Accuracy =', 1 - missing_classerr))

# ROC-AUC curve
ROCPred <- prediction(predict_reg, test_reg$default_payment) 
ROCPer <- performance(ROCPred, measure = "tpr", 
                      x.measure = "fpr")

auc <- performance(ROCPred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Plotting ROC Curve
plot(ROCPer)
plot(ROCPer, colorize = TRUE, 
     print.cutoffs.at = seq(0.1, by = 0.1), 
     main = "ROC CURVE")
abline(a = 0, b = 1)

auc <- round(auc, 4)
legend(.6, .4, auc, title = "AUC", cex = 1)

# Odds Ratio

coef(model)
exp(coef(model))
exp(confint(model))

cbind(coef(model), odds_ratio=exp(coef(model)),exp(confint(model)))

# Assumption of Multiple Logistic Regression

# Assumption 1: 1The response variable is binary
# Assumption 2: The observations are independent
# Assumption 3: The sample size is sufficiently large
# The sample size of this dataset is 30000 observations so the assumption is met.

# Assumption 4: There is no multicollinearity among explanatory variables

# vif() function is used to find the multicollinearity in the model. VIF score between 5 to 10 is considered high amount collinearity
vif(model)

# Assumption 5:There are no extreme outliers

# Cook's Distance was used to find whether outliers are presented
plot(model, which = 4, id.n = 10)
                
# Based on the Cook's Distance plot, multiple outliers were found so the assumption was violated.

# Assumption 6: There is a linear relationship between explanatory variables and the logit of the response variable

probabilities <- predict(model, type = "response")

logit = log(probabilities/(1-probabilities))

ggplot(df, aes(logit, AGE))+
  geom_point(size = 0.5, alpha = 0.5)+
  geom_smooth(method = "loess")+
  theme_bw()

