#Installing Packages
install.packages("ISLR")
install.packages("rattle")
install.packages("clValid")
library(ISLR) 
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(class)
library(cluster)
library(clValid)
library(stats)

#Loading the AUTO dataset
df <- Auto

#Creating a binary to classify origin as American/Non-American
df$origin_flag <- ifelse(df$origin==1,1,0)

#Dropping unnecessary variables like origin and name
df_new <- df[,-c(8,9)]

# Set random seed.
set.seed(1)

# Shuffle the dataset, call the result shuffled
n <- nrow(df_new)
shuffled <- df_new[sample(n),]



###################Classification Problem Using Decision Tree##########################
######################################################################################
# Initialize the accuracy test vector
accs_test_final <- rep(0,6)

# Initialize the accuracy train vector
accs_train_final <- rep(0,6)

for (j in c(4,6)) {
  
  # Initialize the accs test and train vector
  accs_test <- rep(0,j)
  accs_train <- rep(0,j)

  for (i in 1:j) {
    # These indices indicate the interval of the test set
    indices <- (((i-1) * round((1/j)*nrow(shuffled))) + 1):((i*round((1/j) * nrow(shuffled))))
    
    # Exclude them from the train set
    train <- shuffled[-indices,]
    
    # Include them in the test set
    test <- shuffled[indices,]
    
    # A model is learned using each training set
    tree <- rpart(origin_flag ~ ., train, method = "class")
    
    # Make a prediction on the test set using tree
    pred_test <- predict(tree, test, type = "class")
    
    # Make a prediction on the train set using tree
    pred_train <- predict(tree, train, type = "class")
    
    # Assign the confusion matrix to conf_test
    conf_test <- table(test$origin_flag, pred_test)
    
    # Assign the confusion matrix to conf_train
    conf_train <- table(train$origin_flag, pred_train)
    
    # Assign the accuracy of this test model to the ith index in accs
    accs_test[i] <- sum(diag(conf_test))/sum(conf_test)
    
    # Assign the accuracy of this train model to the ith index in accs
    accs_train[i] <- sum(diag(conf_train))/sum(conf_train)
  }
  
  accs_test_final[j] <- mean(accs_test)
  accs_train_final[j] <- mean(accs_train)
}

# Print out the mean of accs of the test dataset
mean(c(accs_test_final[4],accs_test_final[6]))

# Print out the mean of accs of the train dataset
mean(c(accs_train_final[4],accs_train_final[6]))




###################Classification Problem Using K- Nearest Method##########################
###########################################################################################

##K=1
ki=1

# Initialize the accuracy test vector
accs_test_final <- rep(0,6)

# Initialize the accuracy train vector
accs_train_final <- rep(0,6)

for (j in c(4,6)) {
  
  # Initialize the accs test and train vector
  accs_test <- rep(0,j)
  accs_train <- rep(0,j)
  
  for (i in 1:j) {
    # These indices indicate the interval of the test set
    indices <- (((i-1) * round((1/j)*nrow(shuffled))) + 1):((i*round((1/j) * nrow(shuffled))))
    
    # Exclude them from the train set
    train <- shuffled[-indices,]
    
    # Include them in the test set
    test <- shuffled[indices,]
    
    #Train Dataset without label
    knn_train <- train[,-8]
    
    #Test Dataset without label
    knn_test <- test[,-8]
    
    #Train Labels
    train_labels <- train[,8]
    
    #Train Labels
    test_labels <- test[,8]
    
    #Normalized Data
    knn_train$mpg = (knn_train$mpg-min(knn_train$mpg))/(max(knn_train$mpg)-min(knn_train$mpg))
    knn_train$cylinders = (knn_train$cylinders-min(knn_train$cylinders))/(max(knn_train$cylinders)-min(knn_train$cylinders))
    knn_train$displacement = (knn_train$displacement-min(knn_train$displacement))/(max(knn_train$displacement)-min(knn_train$displacement))
    knn_train$horsepower = (knn_train$horsepower-min(knn_train$horsepower))/(max(knn_train$horsepower)-min(knn_train$horsepower))
    knn_train$weight = (knn_train$weight-min(knn_train$weight))/(max(knn_train$weight)-min(knn_train$weight))
    knn_train$acceleration = (knn_train$acceleration-min(knn_train$acceleration))/(max(knn_train$acceleration)-min(knn_train$acceleration))
    knn_train$year = (knn_train$year-min(knn_train$year))/(max(knn_train$year)-min(knn_train$year))
    
    
    knn_test$mpg = (knn_test$mpg-min(knn_train$mpg))/(max(knn_train$mpg)-min(knn_train$mpg))
    knn_test$cylinders = (knn_test$cylinders-min(knn_train$cylinders))/(max(knn_train$cylinders)-min(knn_train$cylinders))
    knn_test$displacement = (knn_test$displacement-min(knn_train$displacement))/(max(knn_train$displacement)-min(knn_train$displacement))
    knn_test$horsepower = (knn_test$horsepower-min(knn_train$horsepower))/(max(knn_train$horsepower)-min(knn_train$horsepower))
    knn_test$weight = (knn_test$weight-min(knn_train$weight))/(max(knn_train$weight)-min(knn_train$weight))
    knn_test$acceleration = (knn_test$acceleration-min(knn_train$acceleration))/(max(knn_train$acceleration)-min(knn_train$acceleration))
    knn_test$year = (knn_test$year-min(knn_train$year))/(max(knn_train$year)-min(knn_train$year))
    
    
    
    # Make a prediction on the test set using tree
    pred_test <- knn(train = knn_train, test = knn_test, cl = train_labels, k = ki)
    
    
    # Assign the confusion matrix to conf_test
    conf_test <- table(test_labels, pred_test)
    
    # Assign the accuracy of this test model to the ith index in accs
    accs_test[i] <- sum(diag(conf_test))/sum(conf_test)
    
  }
  
  accs_test_final[j] <- mean(accs_test)
}

# Print out the mean of accs of the test dataset
mean(c(accs_test_final[4],accs_test_final[6]))





##K=2
ki=2

# Initialize the accuracy test vector
accs_test_final <- rep(0,6)

# Initialize the accuracy train vector
accs_train_final <- rep(0,6)

for (j in c(4,6)) {
  
  # Initialize the accs test and train vector
  accs_test <- rep(0,j)
  accs_train <- rep(0,j)
  
  for (i in 1:j) {
    # These indices indicate the interval of the test set
    indices <- (((i-1) * round((1/j)*nrow(shuffled))) + 1):((i*round((1/j) * nrow(shuffled))))
    
    # Exclude them from the train set
    train <- shuffled[-indices,]
    
    # Include them in the test set
    test <- shuffled[indices,]
    
    #Train Dataset without label
    knn_train <- train[,-8]
    
    #Test Dataset without label
    knn_test <- test[,-8]
    
    #Train Labels
    train_labels <- train[,8]
    
    #Train Labels
    test_labels <- test[,8]
    
    #Normalized Data
    knn_train$mpg = (knn_train$mpg-min(knn_train$mpg))/(max(knn_train$mpg)-min(knn_train$mpg))
    knn_train$cylinders = (knn_train$cylinders-min(knn_train$cylinders))/(max(knn_train$cylinders)-min(knn_train$cylinders))
    knn_train$displacement = (knn_train$displacement-min(knn_train$displacement))/(max(knn_train$displacement)-min(knn_train$displacement))
    knn_train$horsepower = (knn_train$horsepower-min(knn_train$horsepower))/(max(knn_train$horsepower)-min(knn_train$horsepower))
    knn_train$weight = (knn_train$weight-min(knn_train$weight))/(max(knn_train$weight)-min(knn_train$weight))
    knn_train$acceleration = (knn_train$acceleration-min(knn_train$acceleration))/(max(knn_train$acceleration)-min(knn_train$acceleration))
    knn_train$year = (knn_train$year-min(knn_train$year))/(max(knn_train$year)-min(knn_train$year))
    
    
    knn_test$mpg = (knn_test$mpg-min(knn_train$mpg))/(max(knn_train$mpg)-min(knn_train$mpg))
    knn_test$cylinders = (knn_test$cylinders-min(knn_train$cylinders))/(max(knn_train$cylinders)-min(knn_train$cylinders))
    knn_test$displacement = (knn_test$displacement-min(knn_train$displacement))/(max(knn_train$displacement)-min(knn_train$displacement))
    knn_test$horsepower = (knn_test$horsepower-min(knn_train$horsepower))/(max(knn_train$horsepower)-min(knn_train$horsepower))
    knn_test$weight = (knn_test$weight-min(knn_train$weight))/(max(knn_train$weight)-min(knn_train$weight))
    knn_test$acceleration = (knn_test$acceleration-min(knn_train$acceleration))/(max(knn_train$acceleration)-min(knn_train$acceleration))
    knn_test$year = (knn_test$year-min(knn_train$year))/(max(knn_train$year)-min(knn_train$year))
    
    
    
    # Make a prediction on the test set using tree
    pred_test <- knn(train = knn_train, test = knn_test, cl = train_labels, k = ki)
    
    
    # Assign the confusion matrix to conf_test
    conf_test <- table(test_labels, pred_test)
    
    # Assign the accuracy of this test model to the ith index in accs
    accs_test[i] <- sum(diag(conf_test))/sum(conf_test)
    
  }
  
  accs_test_final[j] <- mean(accs_test)
}

# Print out the mean of accs of the test dataset
mean(c(accs_test_final[4],accs_test_final[6]))





##K=4
ki=4

# Initialize the accuracy test vector
accs_test_final <- rep(0,6)

# Initialize the accuracy train vector
accs_train_final <- rep(0,6)

for (j in c(4,6)) {
  
  # Initialize the accs test and train vector
  accs_test <- rep(0,j)
  accs_train <- rep(0,j)
  
  for (i in 1:j) {
    # These indices indicate the interval of the test set
    indices <- (((i-1) * round((1/j)*nrow(shuffled))) + 1):((i*round((1/j) * nrow(shuffled))))
    
    # Exclude them from the train set
    train <- shuffled[-indices,]
    
    # Include them in the test set
    test <- shuffled[indices,]
    
    #Train Dataset without label
    knn_train <- train[,-8]
    
    #Test Dataset without label
    knn_test <- test[,-8]
    
    #Train Labels
    train_labels <- train[,8]
    
    #Train Labels
    test_labels <- test[,8]
    
    #Normalized Data
    knn_train$mpg = (knn_train$mpg-min(knn_train$mpg))/(max(knn_train$mpg)-min(knn_train$mpg))
    knn_train$cylinders = (knn_train$cylinders-min(knn_train$cylinders))/(max(knn_train$cylinders)-min(knn_train$cylinders))
    knn_train$displacement = (knn_train$displacement-min(knn_train$displacement))/(max(knn_train$displacement)-min(knn_train$displacement))
    knn_train$horsepower = (knn_train$horsepower-min(knn_train$horsepower))/(max(knn_train$horsepower)-min(knn_train$horsepower))
    knn_train$weight = (knn_train$weight-min(knn_train$weight))/(max(knn_train$weight)-min(knn_train$weight))
    knn_train$acceleration = (knn_train$acceleration-min(knn_train$acceleration))/(max(knn_train$acceleration)-min(knn_train$acceleration))
    knn_train$year = (knn_train$year-min(knn_train$year))/(max(knn_train$year)-min(knn_train$year))
    
    
    knn_test$mpg = (knn_test$mpg-min(knn_train$mpg))/(max(knn_train$mpg)-min(knn_train$mpg))
    knn_test$cylinders = (knn_test$cylinders-min(knn_train$cylinders))/(max(knn_train$cylinders)-min(knn_train$cylinders))
    knn_test$displacement = (knn_test$displacement-min(knn_train$displacement))/(max(knn_train$displacement)-min(knn_train$displacement))
    knn_test$horsepower = (knn_test$horsepower-min(knn_train$horsepower))/(max(knn_train$horsepower)-min(knn_train$horsepower))
    knn_test$weight = (knn_test$weight-min(knn_train$weight))/(max(knn_train$weight)-min(knn_train$weight))
    knn_test$acceleration = (knn_test$acceleration-min(knn_train$acceleration))/(max(knn_train$acceleration)-min(knn_train$acceleration))
    knn_test$year = (knn_test$year-min(knn_train$year))/(max(knn_train$year)-min(knn_train$year))
    
    
    
    # Make a prediction on the test set using tree
    pred_test <- knn(train = knn_train, test = knn_test, cl = train_labels, k = ki)
    
    
    # Assign the confusion matrix to conf_test
    conf_test <- table(test_labels, pred_test)
    
    # Assign the accuracy of this test model to the ith index in accs
    accs_test[i] <- sum(diag(conf_test))/sum(conf_test)
    
  }
  
  accs_test_final[j] <- mean(accs_test)
}

# Print out the mean of accs of the test dataset
mean(c(accs_test_final[4],accs_test_final[6]))




###################Numerical Prediction Using Linear Regression##########################
###########################################################################################


# Initialize the rmse test vector
rmse_test_final <- rep(0,6)

# Initialize the rmse train vector
rmse_train_final <- rep(0,6)

# Initialize the  R Square test vector
r_square_test_final <- rep(0,6)

# Initialize the  R Square train vector
r_square_train_final <- rep(0,6)



for (j in c(4,6)) {
  
  # Initialize the accs test and train vector
  rmse_test <- rep(0,j)
  rmse_train <- rep(0,j)
  r_square_test <- rep(0,j)
  r_square_train <- rep(0,j)
  
  for (i in 1:j) {
    # These indices indicate the interval of the test set
    indices <- (((i-1) * round((1/j)*nrow(shuffled))) + 1):((i*round((1/j) * nrow(shuffled))))
    
    # Exclude them from the train set
    train <- shuffled[-indices,]
    
    # Include them in the test set
    test <- shuffled[indices,]
    
    # A model is learned using each training set
    lm_linear <- lm(mpg ~ ., train)
    
    # Make a prediction on the test set using tree
    pred_test <- predict(lm_linear, test)
    
    # Make a prediction on the train set using tree
    pred_train <- predict(tree, train)
    
    # rmse test
    rmse_test[i] <- sqrt(mean((pred_test - test$mpg) ^ 2))
    
    # rmse train
    rmse_train[i] <- sqrt(mean( lm_linear$residuals^ 2))
    
    # Adjusted R Square test
    r_square_test[i] <- 1 - (sum((pred_test - test$mpg) ^ 2)/sum(( test$mpg - mean(test$mpg)) ^ 2))
    
    # Adjusted R Square train
    r_square_train[i] <- summary(lm_linear)$r.square
  }
  rmse_test_final[j] <- mean(rmse_test) 
  rmse_train_final[j] <- mean(rmse_train) 
  r_square_test_final[j] <- mean(r_square_test)
  r_square_train_final[j] <- mean(r_square_train)
}

# Print out the mean of rmse of the test dataset
mean(c(rmse_test_final[4],rmse_test_final[6]))

# Print out the mean of rmse of the train dataset
mean(c(rmse_train_final[4],rmse_train_final[6]))


# Print out the mean of R square of the test dataset
mean(c(r_square_test_final[4],r_square_test_final[6]))

# Print out the mean of R square of the train dataset
mean(c(r_square_train_final[4],r_square_train_final[6]))


###################Numerical Prediction Using Non-Linear Regression Technique##########################
######################################################################################################

#Bivariate plots with the dependant variables
plot(df_new$cylinders,df_new$mpg, xlab = "Cylinders", ylab = "mpg")
plot(df_new$displacement,df_new$mpg, xlab = "Displacement", ylab = "mpg")
plot(df_new$horsepower,df_new$mpg, xlab = "Horsepower", ylab = "mpg")
plot(df_new$weight,df_new$mpg, xlab = "weight", ylab = "mpg")
plot(df_new$acceleration,df_new$mpg, xlab = "accelearation", ylab = "mpg")
plot(df_new$year,df_new$mpg, xlab = "year", ylab = "mpg")
plot(df_new$origin_flag,df_new$mpg, xlab = "origin", ylab = "mpg")

#Logarithmic plot for the displacement variable
plot(log(df_new$displacement),df_new$mpg, xlab = "log(Displacement)", ylab = "mpg")
plot(log(df_new$horsepower),df_new$mpg, xlab = "log(horsepower)", ylab = "mpg")
plot(log(df_new$weight),df_new$mpg, xlab = "log(weight)", ylab = "mpg")
plot(log(df_new$acceleration),df_new$mpg, xlab = "log(acceleration)", ylab = "mpg")


# Initialize the rmse test vector
rmse_test_final <- rep(0,6)

# Initialize the rmse train vector
rmse_train_final <- rep(0,6)

# Initialize the  R Square test vector
r_square_test_final <- rep(0,6)

# Initialize the  R Square train vector
r_square_train_final <- rep(0,6)



for (j in c(4,6)) {
  
  # Initialize the accs test and train vector
  rmse_test <- rep(0,j)
  rmse_train <- rep(0,j)
  r_square_test <- rep(0,j)
  r_square_train <- rep(0,j)
  
  for (i in 1:j) {
    # These indices indicate the interval of the test set
    indices <- (((i-1) * round((1/j)*nrow(shuffled))) + 1):((i*round((1/j) * nrow(shuffled))))
    
    # Exclude them from the train set
    train <- shuffled[-indices,]
    
    # Include them in the test set
    test <- shuffled[indices,]
    
    # A model is learned using each training set
    lm_linear <- lm(mpg ~ cylinders + log(displacement) + log(horsepower) + log(weight) + acceleration + year + origin_flag, train)
    
    # Make a prediction on the test set using tree
    pred_test <- predict(lm_linear, test)
    
    # Make a prediction on the train set using tree
    pred_train <- predict(tree, train)
    
    # rmse test
    rmse_test[i] <- sqrt(mean((pred_test - test$mpg) ^ 2))
    
    # rmse train
    rmse_train[i] <- sqrt(mean( lm_linear$residuals^ 2))
    
    # Adjusted R Square test
    r_square_test[i] <- 1 - (sum((pred_test - test$mpg) ^ 2)/sum(( test$mpg - mean(test$mpg)) ^ 2))
    
    # Adjusted R Square train
    r_square_train[i] <- summary(lm_linear)$r.square
  }
  rmse_test_final[j] <- mean(rmse_test) 
  rmse_train_final[j] <- mean(rmse_train) 
  r_square_test_final[j] <- mean(r_square_test)
  r_square_train_final[j] <- mean(r_square_train)
}

# Print out the mean of rmse of the test dataset
mean(c(rmse_test_final[4],rmse_test_final[6]))

# Print out the mean of rmse of the train dataset
mean(c(rmse_train_final[4],rmse_train_final[6]))


# Print out the mean of R square of the test dataset
mean(c(r_square_test_final[4],r_square_test_final[6]))

# Print out the mean of R square of the train dataset
mean(c(r_square_train_final[4],r_square_train_final[6]))





###################Clustering problem using k-means algorithm####################################
######################################################################################################


#Scaling the dataset
df_new_sc <- as.data.frame(scale(df_new))

### K = 2

#Running the K-means algorith
km_2 <- kmeans(df_new_sc, 2, nstart =20)

#Calculating Dunn index
dunn_index_2 <- dunn(clusters = km_2$cluster , Data = df_new_sc)
dunn_index_2

#Ratio of Within sum of squares / Total sum of Sqaures
ratio_2 <- km_2$tot.withinss/km_2$totss
ratio_2

### K = 3

#Running the K-means algorith
km_3 <- kmeans(df_new_sc, 3, nstart =20)

#Calculating Dunn index
dunn_index_3 <- dunn(clusters = km_3$cluster , Data = df_new_sc)
dunn_index_3

#Ratio of Within sum of squares / Total sum of Sqaures
ratio_3 <- km_3$tot.withinss/km_3$totss
ratio_3


### K = 5

#Running the K-means algorith
km_5 <- kmeans(df_new_sc, 5, nstart =20)

#Calculating Dunn index
dunn_index_5 <- dunn(clusters = km_5$cluster , Data = df_new_sc)
dunn_index_5

#Ratio of Within sum of squares / Total sum of Sqaures
ratio_5 <- km_5$tot.withinss/km_5$totss
ratio_5


###################Clustering problem using Hierarchical clusters###################################
######################################################################################################

## Calculate the distance matrix: dist_matrix
dist_matrix <- dist(df_new_sc)

#Calculate the clusters using hclust
Hc <- hclust(dist_matrix,method = "single")

### K = 2

## Cut the clusters using cutree into 2 clusters
memb_2 <- cutree(Hc,2)

#Calculating Dunn index
dunn_2 <- dunn(clusters = memb_2, Data = df_new_sc)
dunn_2


### K = 3

## Cut the clusters using cutree into 3 clusters
memb_3 <- cutree(Hc,3)

#Calculating Dunn index
dunn_3 <- dunn(clusters = memb_3, Data = df_new_sc)
dunn_3

### K = 5
## Cut the clusters using cutree into 5 clusters
memb_5 <- cutree(Hc,5)

#Calculating Dunn index
dunn_5 <- dunn(clusters = memb_5, Data = df_new_sc)
dunn_5


