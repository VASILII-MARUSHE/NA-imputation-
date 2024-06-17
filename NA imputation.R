# ------------------------------- ex 1
install.packages("mice")
data(iris)
?iris
head(iris)
iris=iris[,1:4]
for(i in 1:100){
  set.seed(i)
  r=sample(1:nrow(iris), 1)
  set.seed(i)
  c=sample(1:ncol(iris), 1)
  iris[r,c ] = NA
}
summary(iris)

# pairwise deletion
pairwise_data = na.omit(iris$Sepal.Length)
summary(pairwise_data)
length(pairwise_data)
sd(pairwise_data)

# listwise deletion
listwise_data = na.omit(iris)
summary(listwise_data$Sepal.Length)
nrow(listwise_data)  
sd(listwise_data$Sepal.Length)

#  mean/median imputation
mean_data = iris
mean_data$Sepal.Length[is.na(mean_data$Sepal.Length)] =
  mean(mean_data$Sepal.Length, na.rm = TRUE)
summary(mean_data$Sepal.Length)

# regression imputation
library(mice)
?mice
lm(Sepal.Length~., data=na.omit(iris)) #you can impute missing values it "manually"
mice(iris, m = 1,  maxit=1, method = "norm.predict")
reg_data = complete(mice(iris, m = 1, maxit=1, method = "norm.predict"))
head(reg_data)
summary(reg_data$Sepal.Length)

# stochastic imputation
library(mice)
stoch_data = complete(mice(iris, m = 1, maxit=1, method = "norm.nob"))
summary(stoch_data$Sepal.Length)

# multiple imputation
library(mice)
mu_data = complete(mice(iris, m = 5, method = "pmm"))
summary(mu_data$Sepal.Length)


#1st phase - imputation
IMP = mice(iris, m = 5, method = "pmm")
iris_imputed = list()
for (i in 1:5){
  iris_imputed[[i]]=complete(IMP, action = i)
}
mi1 = iris_imputed[[1]]
mi2 = iris_imputed[[2]]
mi3 = iris_imputed[[3]]
mi4 = iris_imputed[[4]]
mi5 = iris_imputed[[5]]

#2nd phase analysis
summary(mi1$Sepal.Length)
summary(mi2$Sepal.Length)
summary(mi3$Sepal.Length)
summary(mi4$Sepal.Length)
summary(mi5$Sepal.Length)

#3rd phase pooling
mi_summary=apply(
  cbind(
      summary(mi1$Sepal.Length),
      summary(mi2$Sepal.Length),
      summary(mi3$Sepal.Length),
      summary(mi4$Sepal.Length),
      summary(mi5$Sepal.Length)
      ),
  1,
  mean
)
mi_summary


data(iris)
results=cbind(
summary(iris$Sepal.Length),
summary(pairwise_data),
summary(listwise_data$Sepal.Length),
summary(reg_data$Sepal.Length),
summary(stoch_data$Sepal.Length),
mi_summary
)
colnames(results)=c("original","pairwise","listwise","reg","stoch reg","mi")
results

# ex 2,
#-----------------------------------------------------------------------

library(car)
data(SLID)
summary(SLID)
#describe wages
summary(SLID$wages)

mean(SLID$wages, na.rm = TRUE)

hist(SLID$wages)
boxplot(SLID$wages)

# lets impute missing values with mean

SLID$wages_impMean <- SLID$wages

is.na(SLID$wages_impMean)

SLID$wages_impMean[is.na(SLID$wages_impMean)] = mean(SLID$wages, na.rm = T)

SLID$wages_impMedian <- SLID$wages

SLID$wages_impMedian[is.na(SLID$wages_impMedian)] = median(SLID$wages, na.rm = T)

summary(SLID[, c("wages", "wages_impMean", "wages_impMedian")])
var(SLID$wages, na.rm = T)
var(SLID$wages_impMean)
var(SLID$wages_impMedian)

par(mfrow = c(3,1))
hist(SLID$wages, main = "Original values")
hist(SLID$wages_impMean, main = "Imputed mean")
hist(SLID$wages_impMedian, main = "Imputed median")

par(mfrow = c(1,1))
boxplot(SLID[, c("wages", "wages_impMean", "wages_impMedian")])

# correlation AGE AND WAGE 

cor(SLID$wages, SLID$age, use = "pairwise.complete.obs") # pairwise

SLID.com = na.omit(SLID)
cor(SLID.com$wages, SLID.com$age)

cor(SLID$wages_impMean, SLID$age, use = "pairwise.complete.obs")

cor(SLID$wages_impMedian, SLID$age, use = "pairwise.complete.obs")

plot(x = SLID$age, y = SLID$wages)
plot(x = SLID$age, y = SLID$wages, type = "p") # point 
plot(x = SLID$age, y = SLID$wages, type = "l") # lines
head(SLID[, c("age", "wages")])
plot(x = SLID$age, y = SLID$wages, type = "b") # both line and point
plot(x = SLID$age, y = SLID$wages, type = "n") # none 

plot(x = SLID$age, y = SLID$wages, main = "wage vs age", sub = "dataset SLID", 
     xlab = "age in years", ylab = "wage in canadian dollars", pch = 16, xlim = c(0,70), 
     ylim = c(0,50), col = "blue")

points(x = 10, y = 20, pch = 17, col = "red", cex = 1.7)

# lets visualize age vs wages with sex

plot(x = SLID$age, y = SLID$wages, main = "wage vs age", sub = "dataset SLID", 
     xlab = "age in years", ylab = "wage in canadian dollars", pch = 16, xlim = c(0,70), 
     ylim = c(0,50), col = "blue", type = "n")

m = SLID[SLID$sex == "Male", ]

points(x = m$age, y = m$wages, pch = 15, col = "light blue")

f = SLID[SLID$sex == "Female", ]

points(x = f$age, y = f$wages, pch = 17, col = "pink")
legend("left", legend = c("Male", "Female"), pch = c(15,17), col = c("light blue", "pink"))


#-----------------------------------------------------------------------------

# ex 3

#----------------------------------------------------------------------------
install.packages("datasets")
library(datasets)


df <- airquality

# pairwise method
pairwise_df <- df
pairwise_df <- pairwise_df[!(is.na(pairwise_df$Ozone)),]

NROW(pairwise_df)


# listwise method 

pairlist_df <- na.omit(df)

NROW(pairlist_df)

# median imputation 

median_imp_df <- df

median_imp_df$Ozone[is.na(median_imp_df$Ozone)] = median(median_imp_df$Ozone, na.rm = T)


# mean imputation 

mean_imp_df <- df

mean_imp_df$Ozone[is.na(mean_imp_df$Ozone)] = mean(mean_imp_df$Ozone, na.rm = T)


# regression imputation
reg_df <- airquality
library(mice)
?mice
lm(Ozone~., data=na.omit(reg_df)) #you can impute missing values it "manually"
mice(reg_df, m = 1,  maxit=1, method = "norm.predict")
reg_df = complete(mice(reg_df, m = 1, maxit=1, method = "norm.predict"))

library(dplyr)
library(tidyverse)

analyze <- function(data) {
  
data %>% 
  group_by(Month) %>%
  summarise(Count = n(), 
            mean = mean(Ozone),
            median = median(Ozone),
            sd = sd(Ozone), 
            IQR = IQR(Ozone))
  }

analyze(pairwise_df)
analyze(pairlist_df)
analyze(median_imp_df)
analyze(mean_imp_df)
analyze(reg_df)

library(ggplot2)



ggplot(pairwise_df, aes(x = as.character(Month), y = Ozone)) +
  geom_boxplot(aes(fill = Month))+
  xlab("Month") +
  ggtitle("Pairwise method changes Ozone level in time")


ggplot(pairlist_df, aes(x = as.character(Month), y = Ozone)) +
  geom_boxplot(aes(fill = Month))+
  xlab("Month") +
  ggtitle("Pairlist method changes Ozone level in time")


ggplot(mean_imp_df, aes(x = as.character(Month), y = Ozone)) +
  geom_boxplot(aes(fill = Month))+
  xlab("Month") +
  ggtitle("Mean imputation method changes Ozone level in time")


ggplot(median_imp_df, aes(x = as.character(Month), y = Ozone)) +
  geom_boxplot(aes(fill = Month))+
  xlab("Month") +
  ggtitle("Median imputation method changes Ozone level in time")


ggplot(reg_df, aes(x = as.character(Month), y = Ozone)) +
  geom_boxplot(aes(fill = Month))+
  xlab("Month") +
  ggtitle("Regresion imputation method changes Ozone level in time")
