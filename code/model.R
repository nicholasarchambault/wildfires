###### Modeling - Script 5
###### Nicholas Archambault
###### Final Project, S&DS 425, 7 Dec. 2020

# This script runs six different models in an effort to understand the factors
# most heavily influencing wildfire size in terms of predictive power.

# Read in data
library(MASS)
library(car)

x <- read.csv("total_all.csv", as.is = TRUE, stringsAsFactors = FALSE)
x_num <- x[, c(9, 17:21, 23:43, 45, 47:49, 51:59, 61:66)]

# Create training and testing sets
set.seed(55)
rows <- sample(nrow(x_num))
shuffled <- x_num[rows, ]
sp <- round(nrow(x_num) * 0.8)
xtrain <- shuffled[1:sp, ]
xtest <- shuffled[sp:nrow(shuffled), ]


# Run initial model - poor performance indicative of fires' randomness and the
# non-normality of distribution
l <- lm(size ~ ., xtrain)
summary(l)
boxcox(l)

# Create weights and run weighted regression with Box-Cox transform applied
weights <- 1 / lm(abs(l$residuals) ~ l$fitted.values)$fitted.values^2
l2 <- lm(size**-.2 ~ ., data = xtrain, weights = weights)
summary(l2)
View(summary(l2)$coefficients)


# "Both" selection
step_both <- stepAIC(l2, direction = "both", trace = FALSE)
s_both <- summary(step_both)
s_both

# "Backward" selection
step_back <- stepAIC(l2, direction = "backward", trace = FALSE)
s_back <- summary(step_back)
s_back

# "Forward" selection
step_fwd <- stepAIC(l2, direction = "forward")
s_fwd <- summary(step_fwd)
s_fwd 

# Predict values
p <- predict(lm(size ~., xtest))

preds <- as.data.frame(matrix(NA, nrow = nrow(xtest), ncol = 2))
preds[, 1] <- xtest$size
preds[, 2] <- round(p, 1)

# Define rmse function
rmse <- function(pred, obs){
  sqrt(mean(unlist((pred - obs)^2)))
}

# High rmse
rmse(preds[, 2], preds[, 1])



### Limit and transform predictors
rm(list = ls())
library(MASS)
library(car)

# Repeat process for limited pool of predictors
x <- read.csv("total_all.csv", as.is = TRUE, stringsAsFactors = FALSE)
x_num <- x[, c(9, 17:21, 23:43, 45, 47:49, 51:59, 61:66)]
predictors <- c("size", "air_tmp_avg", "dew_pnt", "eto", "rel_hum_avg", 
                "soil_tmp_avg", "sol_rad_net", "vp_max", "wind_run", 
                "remoteness")
x_num <- x[, predictors]
x_num <- x_num[x_num$size < 1, ]

# Create training and testing datasets
set.seed(55)
rows <- sample(nrow(x_num))
shuffled <- x_num[rows, ]
sp <- round(nrow(x_num) * 0.8)
xtrain <- shuffled[1:sp, ]
xtest <- shuffled[sp:nrow(shuffled), ]

# Model still performs poorly
l <- lm(size ~ ., xtrain)
summary(l)
boxcox(l)

# Weighted model performs better
weights <- 1 / lm(abs(l$residuals) ~ l$fitted.values)$fitted.values^2
l2 <- lm(size**-.2 ~ ., data = xtrain, weights = weights)
summary(l2)


# "Both" selection
step_both <- stepAIC(l2, direction = "both", trace = FALSE)
s_both <- summary(step_both)
s_both

# "Backward" selection
step_back <- stepAIC(l2, direction = "backward", trace = FALSE)
s_back <- summary(step_back)
s_back

# "Forward" selection
step_fwd <- stepAIC(l2, direction = "forward")
s_fwd <- summary(step_fwd)
s_fwd 


p <- predict(lm(size ~., xtest))

preds <- as.data.frame(matrix(NA, nrow = nrow(xtest), ncol = 2))
preds[, 1] <- xtest$size
preds[, 2] <- round(p, 1)
rmse <- function(pred, obs){
  sqrt(mean(unlist((pred - obs)^2)))
}

rmse(preds[, 2], preds[, 1])

### All predictors, restricted outliers
# Read in data
library(MASS)
library(car)

x <- read.csv("total_all.csv", as.is = TRUE, stringsAsFactors = FALSE)
x_num <- x[, c(9, 17:21, 23:43, 45, 47:49, 51:59, 61:66)]


for (i in 1:ncol(x_num)){
  q3 <- summary(x_num[, i])[5]
  q1 <- summary(x_num[, i])[2]
  r <- IQR(x_num[, i])
  x_num <- x_num[(x_num[, i] >= (q1 - 1.5*r)) & (x_num[, i] <= (q3 + 1.5*r)), ]
}


# Create training and testing sets
set.seed(55)
rows <- sample(nrow(x_num))
shuffled <- x_num[rows, ]
sp <- round(nrow(x_num) * 0.8)
xtrain <- shuffled[1:sp, ]
xtest <- shuffled[sp:nrow(shuffled), ]

# Run initial model - poor performance indicative of fires' randomness and the
# non-normality of distribution
l <- lm(size ~ ., xtrain)
summary(l)
boxcox(l)

# Create weights and run weighted regression with Box-Cox transform applied
weights <- 1 / lm(abs(l$residuals) ~ l$fitted.values)$fitted.values^2
l2 <- lm(size**-.02 ~ ., data = xtrain, weights = weights)
summary(l2)


# Predict values
p <- predict(lm(size ~., xtest))

preds <- as.data.frame(matrix(NA, nrow = nrow(xtest), ncol = 2))
preds[, 1] <- xtest$size
preds[, 2] <- round(p, 1)

# Define rmse function
rmse <- function(pred, obs){
  sqrt(mean(unlist((pred - obs)^2)))
}

# High rmse
rmse(preds[, 2], preds[, 1])


#### Remove outliers with limited predictors
# Read in data
library(MASS)
library(car)

x <- read.csv("total_all.csv", as.is = TRUE, stringsAsFactors = FALSE)
x_num <- x[, c(9, 17:21, 23:43, 45, 47:49, 51:59, 61:66)]


for (i in 1:ncol(x_num)){
  q3 <- summary(x_num[, i])[5]
  q1 <- summary(x_num[, i])[2]
  r <- IQR(x_num[, i])
  x_num <- x_num[(x_num[, i] >= (q1 - 1.5*r)) & (x_num[, i] <= (q3 + 1.5*r)), ]
}
predictors <- c("size", "air_tmp_avg", "dew_pnt", "eto", "rel_hum_avg", 
                "soil_tmp_avg", "sol_rad_net", "vp_max", "wind_run", 
                "remoteness")
x_num <- x_num[, predictors]

# Create training and testing sets
set.seed(55)
rows <- sample(nrow(x_num))
shuffled <- x_num[rows, ]
sp <- round(nrow(x_num) * 0.8)
xtrain <- shuffled[1:sp, ]
xtest <- shuffled[sp:nrow(shuffled), ]


# Run initial model - poor performance indicative of fires' randomness and the
# non-normality of distribution
l <- lm(size ~ ., xtrain)
summary(l)
boxcox(l)

# Create weights and run weighted regression with Box-Cox transform applied
weights <- 1 / lm(abs(l$residuals) ~ l$fitted.values)$fitted.values^2
l2 <- lm(size**-.02 ~ ., data = xtrain, weights = weights)
summary(l2)


# Predict values
p <- predict(lm(size ~., xtest))

preds <- as.data.frame(matrix(NA, nrow = nrow(xtest), ncol = 2))
preds[, 1] <- xtest$size
preds[, 2] <- round(p, 1)

# Define rmse function
rmse <- function(pred, obs){
  sqrt(mean(unlist((pred - obs)^2)))
}

# High rmse
rmse(preds[, 2], preds[, 1])



### By county
rm(list = ls())
library(MASS)
library(car)

z <- read.csv("by_county.csv", as.is = TRUE, stringsAsFactors = FALSE)

# Impute a zero value with column mean
z <- z[, -c(1, 4, 53, 55:57)]
z[z$prcp == 0, "prcp"] <- mean(z$prcp)

# Create testing and training datasets
set.seed(45)
rows <- sample(nrow(z))
shuffled <- z[rows, ]
sp <- round(nrow(z) * 0.8)
ztrain <- shuffled[1:sp, ]
ztest <- shuffled[sp:nrow(shuffled), ]

# Initial model performs decently
full_model <- lm(size ~., z[, -1])
summary(full_model)
boxcox(full_model)

# Weighted model performs better
weights <- 
  1 / lm(abs(full_model$residuals) ~ full_model$fitted.values)$fitted.values^2
full_lmw <- lm(size ~ ., data = z[, -1], weights = weights)
summary(full_lmw)


p <- predict(full_lmw, ztest)

preds <- as.data.frame(matrix(NA, nrow = nrow(ztest), ncol = 2))
preds[, 1] <- ztest$size
preds[, 2] <- round(p, 1)
rmse <- function(pred, obs){
  sqrt(mean(unlist((pred - obs)^2)))
}
View(preds)

rmse(preds[, 2], preds[, 1])

colnames(preds) <- c("obs", "pred")


### By county log
rm(list = ls())
library(MASS)
library(car)


# Repeat process with log-transformed variables
z <- read.csv("by_county.csv", as.is = TRUE, stringsAsFactors = FALSE)

z <- z[, -c(1, 4, 53, 55:57)]
z[z$prcp == 0, "prcp"] <- mean(z$prcp)



for (i in 2:ncol(z)) {z[, i] <- log(z[, i])}

qqPlot(z$size)


set.seed(55)
rows <- sample(nrow(z))
shuffled <- z[rows, ]
sp <- round(nrow(z) * 0.8)
ztrain <- shuffled[1:sp, ]
ztest <- shuffled[sp:nrow(shuffled), ]


full_model <- lm(size ~., z[, -1])
summary(full_model)



weights <- 
  1 / lm(abs(full_model$residuals) ~ full_model$fitted.values)$fitted.values^2
full_lmw <- lm(size ~ ., data = z[, -1], weights = weights)
summary(full_lmw)


p <- predict(full_lmw, ztest)

preds <- as.data.frame(matrix(NA, nrow = nrow(ztest), ncol = 2))
preds[, 1] <- ztest$size
preds[, 2] <- round(p, 1)
rmse <- function(pred, obs){
  sqrt(mean(unlist((pred - obs)^2)))
}

rmse(preds[, 2], preds[, 1])
colnames(preds) <- c("obs", "pred")

preds <- exp(preds)

plot(seq(1, 12), preds$obs, type = "l", 
     main = "Predictions vs. Observations,\nAverage Fire Size by County",
     xlab = "",
     ylab = "Size (acres)", lwd = 2)
lines(seq(1, 12), preds$pred, type = "l", lwd = 2, lty = 2, col = "red")
legend("topright", legend = c("Observations", "Predictions"), 
       col = c("black", "red"), lty = 1:2)


