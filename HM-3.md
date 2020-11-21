HM\#3
================
Tamires Amorim and Carol
11/20/2020

`{r setup, include=FALSE} knitr::opts_chunk$set(echo = TRUE)`

``` {r}
load("~/R/acs2017_ny_data.RData")
```

``` {r}
dat_NYC <- subset(acs2017_ny, (acs2017_ny$in_NYC == 1)&(acs2017_ny$AGE > 20) & (acs2017_ny$AGE < 66))
attach(dat_NYC)
borough_f <- factor((in_Bronx + 2*in_Manhattan + 3*in_StatenI + 4*in_Brooklyn + 5*in_Queens), levels=c(1,2,3,4,5),labels = c("Bronx","Manhattan","Staten Island","Brooklyn","Queens"))
```

#### Normalize data

``` {r}
norm_varb <- function(X_in) {
  (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE) )
}
```

``` {r}
is.na(OWNCOST) <- which(OWNCOST == 9999999)
housing_cost <- OWNCOST + RENT
norm_inc_tot <- norm_varb(INCTOT)
norm_housing_cost <- norm_varb(housing_cost)
```

#### clear data

``` {r}
annual_household_cost <-COSTELEC+COSTFUEL+COSTGAS+COSTWATR
norm_inc_tot <-norm_varb(INCTOT)
norm_annual_household_cost <-norm_varb(annual_household_cost)
```

#### Create dataframe

``` {r}
data_use_prelim <-data.frame(norm_inc_tot,norm_annual_household_cost)
good_obs_data_use <-complete.cases(data_use_prelim,borough_f)
dat_use <-subset(data_use_prelim,good_obs_data_use)
y_use <-subset(borough_f,good_obs_data_use)
```

#### Create train data set and test data set. here we try to use 70% of the data selected as our train data and the rest of 30% as the test data set.

``` {r}
set.seed(12345)
NN_obs <- sum(good_obs_data_use ==1)
select1 <-runif(NN_obs)<0.7
train_data <- subset(dat_use,select1)
test_data <-subset(dat_use,!select1)
cl_data <-y_use[select1]  ##matrix of classes of the K
true_data <-y_use[!select1]
```

#### run the data and analyze

``` {r}
summary(cl_data)
```

``` {r}
prop.table(summary(cl_data))
```

``` {r}
summary(train_data)
```

``` {r}
 for (indx in seq(1, 9, by= 2)) {
  pred_borough <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
  num_correct_labels <- sum(pred_borough == true_data)
  correct_rate <- num_correct_labels/length(true_data)
  print(c(indx,correct_rate))
}
```
