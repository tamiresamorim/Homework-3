---
output: github_document
editor_options: 
  chunk_output_type: inline
---
# Homework-3
Tamires Amorim, Yamei Li and Meirou Guan - Lab 3


load("workspace.RData")

attach(dat_NYC)
borough_f <- factor((in_Bronx + 2*in_Manhattan + 3*in_StatenI + 4*in_Brooklyn + 5*in_Queens), levels=c(1,2,3,4,5),labels = c("Bronx","Manhattan","Staten Island","Brooklyn","Queens"))

#### Normalize data

norm_varb <- function(X_in) {
  (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE) )
}

#### clear data

annual_household_cost <-COSTELEC+COSTFUEL+COSTGAS+COSTWATR
norm_inc_tot <-norm_varb(INCTOT)
norm_annual_household_cost <-norm_varb(annual_household_cost)

#### Create data frame

data_use_prelim <-data.frame(norm_inc_tot,norm_annual_household_cost)
good_obs_data_use <-complete.cases(data_use_prelim,borough_f)
dat_use <-subset(data_use_prelim,good_obs_data_use)
y_use <-subset(borough_f,good_obs_data_use)


#### Create train data set and test data set. here we try to use 70%of the data selected as our train data and the rest of 30% as the test data set.

set.seed(12345)
NN_obs <- sum(good_obs_data_use ==1)
select1 <-runif(NN_obs)<0.7
train_data <- subset(dat_use,select1)
test_data <-subset(dat_use,!select1)
cl_data <-y_use[select1]  ##matrix of classes of the K
true_data <-y_use[!select1]


#### run the data and analyze

summary(cl_data)
##         Bronx     Manhattan Staten Island      Brooklyn        Queens 
##          4283          4610          1633         10846          9582

prop.table(summary(cl_data))
##         Bronx     Manhattan Staten Island      Brooklyn        Queens 
##     0.1383666     0.1489307     0.0527557     0.3503909     0.3095561

summary(train_data)
##   norm_inc_tot     norm_annual_household_cost
##  Min.   :0.00000   Min.   :0.0000            
##  1st Qu.:0.01184   1st Qu.:0.4079            
##  Median :0.02693   Median :0.5509            
##  Mean   :0.04275   Mean   :0.5860            
##  3rd Qu.:0.05219   3rd Qu.:0.7767            
##  Max.   :1.00000   Max.   :1.0000

library(class)
 for (indx in seq(1, 9, by= 2)) {
  pred_borough <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
  num_correct_labels <- sum(pred_borough == true_data)
  correct_rate <- num_correct_labels/length(true_data)
  print(c(indx,correct_rate))
}

## [1] 1.0000000 0.3814826
## [1] 3.0000000 0.3858298
## [1] 5.0000000 0.4032184
## [1] 7.0000000 0.4090146
## [1] 9.0000000 0.4096248

save.image("workspace.RData")