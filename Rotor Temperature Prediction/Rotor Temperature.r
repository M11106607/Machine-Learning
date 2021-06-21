# Load the libraries used in the notebook
library(dplyr)
library(randomForest)
library(glmnet)
library(psych)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(car)
library(e1071)

# Reading the data
mydata <- read.csv("pmsm_temperature_data_A1_2021.csv")

# dropping 3 columns "stator_yoke", "stator_tooth", and "stator_winding" as they are not required for the analysis or prediction
rotor_temp <- subset(mydata, select = -c(stator_yoke, stator_tooth, stator_winding))

# Splitting the data into training and testing, we need data for testing where profile_id is 72 and 81

# training data
training_data <- filter(rotor_temp, profile_id != 72 & profile_id != 81)

# dropping profile_id column as it's not required for the analysis
training_data <- subset(training_data, select = -c(profile_id))

# testing data
testing_data <- filter(rotor_temp, profile_id == 72 | profile_id == 81)

# dropping profile_id column as it's not required for the analysis
testing_data <- subset(testing_data, select = -c(profile_id))

rotor_temp <- subset(rotor_temp, select = -c(profile_id))

# Dimensions of data
cat("The pmsp temperature dataset has", dim(rotor_temp)[1], "records, each with", dim(rotor_temp)[2],
    "attributes. The structure is:\n\n")

# Structure of data
str(rotor_temp)

cat("\nThe head and tail records of dataset are:")
# first few records
head(rotor_temp)
# last few records
tail(rotor_temp)

cat("\nBasic statistics for each attribute are:")
# Statistical summary 
summary(rotor_temp)

cat("The numbers of unique values for each attribute are:")
apply(rotor_temp, 2, function(x) length(unique(x)))

# Check for any null values in data set column wise.
for (cols in colnames(rotor_temp)) {
    print(paste("Null values in attribute", cols,"are",sum(is.na(rotor_temp$cols))))  
}

# moving beyong basic statistics
round(describe(rotor_temp),3)

# Boxplot of dataset
boxplot(rotor_temp, las=2, cex.axis = 1)

data <- melt(as.data.frame(rotor_temp))
ggplot(data,aes(x = variable,y = value)) + facet_wrap(~variable, scales="free") + geom_boxplot() +
scale_y_continuous(labels=function (n) {format(n, scientific=FALSE)})

# Histogram plots
options(repr.plot.width = 15, repr.plot.height = 15, repr.plot.res = 100)

ambient <- ggplot(rotor_temp, aes(x=ambient)) + geom_histogram(colour = "blue", fill = "sky blue", bins=15) + theme_classic()
ambient <- ambient + ggtitle("Ambient Distribution")

coolant <- ggplot(rotor_temp, aes(x=coolant)) + geom_histogram(colour = "blue", fill = "sky blue", bins=15) + theme_classic()
coolant <- coolant + ggtitle("coolant Distribution")

u_d <- ggplot(rotor_temp, aes(x=u_d)) + geom_histogram(colour = "blue", fill = "sky blue", bins=15) + theme_classic()
u_d <- u_d + ggtitle("u_d Distribution")

u_q <- ggplot(rotor_temp, aes(x=u_q)) + geom_histogram(colour = "blue", fill = "sky blue", bins=15) + theme_classic()
u_q <- u_q + ggtitle("u_q Distribution")

motor_speed <- ggplot(rotor_temp, aes(x=motor_speed)) + geom_histogram(colour = "blue", fill = "sky blue", bins=15) + theme_classic()
motor_speed <- motor_speed + ggtitle("motor_speed Distribution")

torque <- ggplot(rotor_temp, aes(x=torque)) + geom_histogram(colour = "blue", fill = "sky blue", bins=15) + theme_classic()
torque <- torque + ggtitle("torque Distribution")

i_d <- ggplot(rotor_temp, aes(x=i_d)) + geom_histogram(colour = "blue", fill = "sky blue", bins=15) + theme_classic()
i_d <- i_d + ggtitle("i_d Distribution")

i_q <- ggplot(rotor_temp, aes(x=i_q)) + geom_histogram(colour = "blue", fill = "sky blue", bins=15) + theme_classic()
i_q <- i_q + ggtitle("i_q Distribution")

pm <- ggplot(rotor_temp, aes(x=pm)) + geom_histogram(colour = "blue", fill = "sky blue", bins=15) + theme_classic()
pm <- pm + ggtitle("pm Distribution")

grid.arrange(ambient,coolant,u_d,u_q,motor_speed,torque,i_d,i_q,pm, ncol=2)

# Density plots
ambient<-ggplot(rotor_temp, aes(x=ambient)) + geom_density(colour = "blue", fill = "sky blue")
coolant<-ggplot(rotor_temp, aes(x=coolant)) + geom_density(colour = "blue", fill = "sky blue")
u_d<-ggplot(rotor_temp, aes(x=u_d)) + geom_density(colour = "blue", fill = "sky blue")
u_q<-ggplot(rotor_temp, aes(x=u_q)) + geom_density(colour = "blue", fill = "sky blue")
motor_speed<-ggplot(rotor_temp, aes(x=motor_speed)) + geom_density(colour = "blue", fill = "sky blue")
torque<-ggplot(rotor_temp, aes(x=torque)) + geom_density(colour = "blue", fill = "sky blue")
i_d<-ggplot(rotor_temp, aes(x=i_d)) + geom_density(colour = "blue", fill = "sky blue")
i_q<-ggplot(rotor_temp, aes(x=i_q)) + geom_density(colour = "blue", fill = "sky blue")
pm<-ggplot(rotor_temp, aes(x=pm)) + geom_density(colour = "blue", fill = "sky blue")

grid.arrange(ambient,coolant,u_d,u_q,motor_speed,torque,i_d,i_q,pm, ncol=2)

options(repr.plot.width = 15, repr.plot.height = 10, repr.plot.res = 100)

coolant <- ggplot(rotor_temp, aes(x=coolant)) + geom_histogram(colour = "blue", fill = "sky blue", bins=15) + theme_classic()
coolant <- coolant + ggtitle("coolant Distribution")

coolant.log <- ggplot(rotor_temp, aes(x=coolant+1-min(coolant))) + geom_histogram(colour = "blue", fill = "sky blue", bins=15) + theme_classic() + scale_x_log10()
coolant.log <- coolant.log + ggtitle("coolant log Distribution")

coolant.sqrt <- ggplot(rotor_temp, aes(x=coolant+1-min(coolant))) + geom_histogram(colour = "blue", fill = "sky blue", bins=15) + theme_classic() + scale_x_sqrt()
coolant.sqrt <- coolant.sqrt + ggtitle("coolant sqrt Distribution")

motor_speed <- ggplot(rotor_temp, aes(x=motor_speed)) + geom_histogram(colour = "blue", fill = "sky blue", bins=15) + theme_classic()
motor_speed <- motor_speed + ggtitle("motor_speed Distribution")

motor_speed.log <- ggplot(rotor_temp, aes(x=motor_speed+1-min(motor_speed))) + geom_histogram(colour = "blue", fill = "sky blue", bins=15) + theme_classic() + scale_x_log10()
motor_speed.log <- motor_speed.log + ggtitle("motor_speed log Distribution")

motor_speed.sqrt <- ggplot(rotor_temp, aes(x=motor_speed+1-min(motor_speed))) + geom_histogram(colour = "blue", fill = "sky blue", bins=15) + theme_classic() + scale_x_sqrt()
motor_speed.sqrt <- motor_speed.sqrt + ggtitle("motor_speed sqrt Distribution")

grid.arrange(coolant, coolant.log, coolant.sqrt, motor_speed, motor_speed.log, motor_speed.sqrt, ncol=3)

options(digits=4)
cor(rotor_temp)

# DIY correlation plot
# http://stackoverflow.com/questions/31709982/how-to-plot-in-r-a-correlogram-on-top-of-a-correlation-matrix
# there's some truth to the quote that modern programming is often stitching together pieces from SO 

colorRange <- c('#69091e', '#e37f65', 'white', '#aed2e6', '#042f60')
## colorRamp() returns a function which takes as an argument a number
## on [0,1] and returns a color in the gradient in colorRange
myColorRampFunc <- colorRamp(colorRange)

panel.cor <- function(w, z, ...) {
    correlation <- cor(w, z)

    ## because the func needs [0,1] and cor gives [-1,1], we need to shift and scale it
    col <- rgb(myColorRampFunc((1 + correlation) / 2 ) / 255 )

    ## square it to avoid visual bias due to "area vs diameter"
    radius <- sqrt(abs(correlation))
    radians <- seq(0, 2*pi, len = 50) # 50 is arbitrary
    x <- radius * cos(radians)
    y <- radius * sin(radians)
    ## make them full loops
    x <- c(x, tail(x,n=1))
    y <- c(y, tail(y,n=1))

    ## trick: "don't create a new plot" thing by following the
    ## advice here: http://www.r-bloggers.com/multiple-y-axis-in-a-r-plot/
    ## This allows
    par(new=TRUE)
    plot(0, type='n', xlim=c(-1,1), ylim=c(-1,1), axes=FALSE, asp=1)
    polygon(x, y, border=col, col=col)
}


pairs(rotor_temp[sample.int(nrow(rotor_temp),1000),], lower.panel=panel.cor)

scatterplotMatrix(~pm+ambient+coolant+motor_speed,data=rotor_temp)

# fitting linear model
lm.fit1 <- lm(pm ~., data = training_data)
summary(lm.fit1)

par(mfrow=c(2,2))
plot(lm.fit1)

# log transformations on coolant
lm.fit2 <- lm(pm ~ ambient+log10(coolant+1-min(coolant))+u_d+u_q+motor_speed+torque+i_d+i_q, data = training_data)
summary(lm.fit2)

# sqrt transformations on coolant
lm.fit2 <- lm(pm ~ ambient+sqrt(coolant+1-min(coolant))+u_d+u_q+motor_speed+torque+i_d+i_q, data = training_data)
summary(lm.fit2)

cat("Using LOG TRANSFORMATION on target variable")
lm.fit3 <- lm(log10(pm+1-min(pm)) ~ ambient+coolant+u_d+u_q+motor_speed+torque+i_d+i_q, data = training_data)
summary(lm.fit3)
cat("\n---------------------------------------------------------------------------------------------------\n")
cat("Using SQRT TRANSFORMATION on target variable")
lm.fit3 <- lm(sqrt(pm+1-min(pm)) ~ ambient+coolant+u_d+u_q+motor_speed+torque+i_d+i_q, data = training_data)
summary(lm.fit3)

lm.fit4 <- lm(log(pm+1-min(pm)) ~ ambient+coolant+u_d+u_q+motor_speed+torque+i_d+i_q+
              motor_speed:u_q+torque:i_q+torque:u_d+motor_speed:i_d, data = training_data)
summary(lm.fit4)

lm1 <- step(lm.fit4, direction= "forward") # forward step selection
summary(lm1)

lm2 <- step(lm.fit4, direction= "backward") # backward step selection
summary(lm2)

mse <- mean((training_data$pm - exp(lm1$fitted.values))^2) # Mean Sqaured Error
cat("The RMSE for Training Data for Linear Regression is:",sqrt(mse))

pred  <- predict(lm.fit4, testing_data) # Predicting the rotor temperature for test data
avg.mse  <- mean((pred-testing_data$pm)^2) # Calculating MSE for Test data
cat("The MSE for Testing Data for Linear Regression is:",avg.mse)

rf.fit <- randomForest(pm~., data = training_data)
rf.fit

(VI_F=importance(rf.fit)) # node purity for each variable
varImpPlot(rf.fit, scale =T, main = "Importance of Variable")

set.seed(40)
# find minimum number of trees to get lowest MSE
x <- which.min(rf.fit$mse)

cat("We got", x,"as the optimal number of trees, now going forward we will use this many trees and analyze if it improves the model performance any further.")

rf.fit2 <- randomForest(pm ~ ambient+coolant+u_d+u_q+motor_speed+torque+i_d+i_q+
                        motor_speed:u_q+torque:i_q+torque:u_d+motor_speed:i_d, data = training_data, ntree = x)
rf.fit2

# Calculating RMSE for training data
mse = mean((training_data$pm - rf.fit2$predicted)^2)
cat("RMSE of RF Model on Training data is", sqrt(mse))

# Making Prediction on testing data and calculating it'd MSE
rf.pred <- predict(rf.fit2, testing_data)
mse = mean((rf.pred - testing_data$pm)^2)
cat("MSE of RF Model on Testing data is", mse)

# MSE and Adjusted R Square of Random Forest Model
cat("MSE of Random Forest Model", mean(rf.fit2$mse))
cat("\nAdjusted R Square of Random Forest Model", mean(rf.fit2$rsq))


