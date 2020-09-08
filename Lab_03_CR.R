###########################################
##### BIG DATA FOR BUSINESS DECISIONS #####
############ cod. 20564 ###################
########## BOCCONI UNIVERSITY #############
###########################################

###################################
########### LAB 3 #################
## CLUSTERING AND CLASSIFICATION ##
######## CROSS-VALIDATION #########
###################################


library( caret )
library( ggplot2 )
library( GGally )
library( data.table )
library( stringr )
library( grid )
library( gridExtra )
library( lemon )
library( factoextra )
library( FactoMineR )
library( cluster )



# UNSUPERVISED LEARNING ---------------------------------------------------


# We will implement customer segmentation in R. 
# Customer segmentation is a good approach to find the most
# valuable customers
# Customer Segmentation is the process of division of customer base 
# into several groups of individuals that share a similarity in different 
# ways that are relevant to marketing such as gender, age, interests, and 
# miscellaneous spending habits.

# we will apply some descriptive analysis of our data and then implement 
# several versions of the K-means algorithm

customers = fread("Lab_03/Mall_Customers.csv")
# we don't like spaces in column names...let's kille them! 
setnames( customers, names( customers ), 
          str_replace_all( names( customers ), "\\s", "_" ) )

summary( customers[ , 3:5 ] )

ggpairs( customers[ , 2:5 ], aes( color = Gender, alpha = .7 ) )

# it seems we can identify some structures there
# let's apply the k-means method
# we start with just two clusters and then we increase the number

# we want to use just numeric variables
customers_num = customers[ , 3:5 ]

k = 2
set.seed( 1970 )
k2 = kmeans( customers_num, centers = k, iter.max = 100 )

fviz_cluster( k2, data = customers_num, 
              ellipse.type = "t", ellipse.level = .95,
              labelsize = 0 ) +
  theme( legend.position = "bottom" ) +
  ggtitle( paste0( "k-means with k = ", k ) )

# let's run it with k = 10 now and see the differences
k = 10
set.seed( 1970 )
k10 = kmeans( customers_num, centers = k, iter.max = 100 )

fviz_cluster( k10, data = customers_num, 
              ellipse.type = "t", ellipse.level = .95,
              labelsize = 0 ) +
  theme( legend.position = "bottom" ) +
  ggtitle( paste0( "k-means with k = ", k ) )



# let's run a set of kmeans now
# we put the little code above into a loop

kvec = seq( 2, 10, by = 2 )
for ( i_k in seq_along( kvec ) ) {
  set.seed( 1970 )
  k.res = kmeans( customers_num, centers = kvec[ i_k ], iter.max = 100 )
  assign( paste0( "pkmean_", formatC( kvec[ i_k ], width = 2, flag = "0" ) ),  
          fviz_cluster( k.res, data = customers_num, 
                        ellipse.type = "t", ellipse.level = .95,
                        labelsize = 0 ) +
            theme( legend.position = "bottom" ) +
            ggtitle( paste0( "k-means with k = ", kvec[ i_k ] ) ) 
  )
}

marrangeGrob( mget( objects( pattern = "pkmean_\\d+" ) ),
              nrow = 2, ncol = 3, top = "" )


# let's build some statistics in an automated way
p_wss = fviz_nbclust( x = customers_num, FUNcluster = kmeans, 
                      k.max = 10, method = "wss" )
p_sil = fviz_nbclust( x = customers_num, FUNcluster = kmeans, 
                      k.max = 10, method = "silhouette" )
p_gap = fviz_nbclust( x = customers_num, FUNcluster = kmeans, 
                      k.max = 10, method = "gap" )

marrangeGrob( list( p_wss, p_sil, p_gap ), ncol = 3, nrow = 1, top = "" )

# TOTAL WITHIN SUM OF SQUARES (WSS)
# With the measurement of the total intra-cluster variation, 
# one can evaluate the compactness of the clustering boundary. 
# We can then proceed to define the optimal clusters as follows:
# 1. we calculate the clustering algorithm for several values of k. 
# This can be done by creating a variation within k from 1 to 10 clusters. 
# 2. We then calculate the total intra-cluster sum of square (iss). 
# 3. Then, we proceed to plot iss based on the number of k clusters. 
# This plot denotes the appropriate number of clusters required in our model. 
# In the plot, the location of a bend or a knee is the indication of the 
# optimum number of clusters.

# SILHOUETTE
# With the help of the average silhouette method, we can measure the quality 
# of our clustering operation. With this, we can determine how well within 
# the cluster is the data object. If we obtain a high average silhouette width, 
# it means that we have good clustering. The average silhouette method 
# calculates the mean of silhouette observations for different k values. 
# With the optimal number of k clusters, one can maximize the average 
# silhouette over significant values for k clusters.
# 
# GAP STATISTIC
# In 2001, researchers at Stanford University – R. Tibshirani, G.Walther and 
# T. Hastie published the Gap Statistic Method. We can use this method to any 
# of the clustering method like K-means, hierarchical clustering etc. 
# Using the gap statistic, one can compare the total intracluster variation 
# for different values of k along with their expected values under the null 
# reference distribution of data. With the help of Monte Carlo simulations, 
# one can produce the sample dataset. For each variable in the dataset, 
# we can calculate the range between min(xi) and max (xj) through which we 
# can produce values uniformly from interval lower bound to upper bound.


# let's consider k = 7 as the optimal number of clusters
set.seed( 1970 )
k7 = kmeans( customers_num, centers = 7, iter.max = 100 )

p_final = ggplot( customers, aes_string( x = "Annual_Income", 
                               y = "Spending_Score" ) ) +
  geom_point( size = 2.5 )
p_final

p_final + geom_point( aes( color = as.factor( k7$cluster ) ) ) +
  ggtitle( "Customer Segmentation", "k-means with 7 clusters" ) +
  theme( legend.position = "bottom" )

fviz_cluster( k7, customers_num, ellipse.type = "t", geom = "point",
               pointsize = 3, shape = 16 ) +
  ggtitle( "Customer Segmentation", "k-means with 7 clusters" ) +
  theme( legend.position = "bottom" )



# SUPERVISED LEARNING -----------------------------------------------------


# load the data
data("iris")
setDT(iris)


# PREPROCESSING -----------------------------------------------------------

set.seed( 1970 )
validation_index <- createDataPartition(iris$Species, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- iris[ -validation_index ]
# use the remaining 80% of data to training and testing the models
training <- iris[ validation_index ]
num_cols = names( which( unlist( lapply(training, is.numeric ) ) ) )

# build boxplots for each numeric variable in the data
for ( var in seq_along(num_cols) ) {
  assign(paste0( "p_", var ), 
         ggplot( training, aes_string( x = "Species", y = num_cols[ var ] ) ) +
           geom_boxplot() + geom_jitter(width = 0.1) +
           stat_boxplot(geom="errorbar") )
}

# plot 
marrangeGrob( mget( objects(pattern = "p_\\d+" ) ), 
              nrow = 2, ncol = 2, top = "")

# density plots
for ( var in seq_along(num_cols) ) {
  assign(paste0( "p_", var ), 
         ggplot( training, aes_string( num_cols[ var ], color = "Species" ) ) +
           geom_density() + geom_rug() )
}

# plot 
grid_arrange_shared_legend( p_1, p_2, p_3, p_4, nrow = 2, ncol = 2 )




# ALGORITHMS --------------------------------------------------------------

# Let's create some models of the data and estimate their accuracy 
# on unseen data.
# 1. We implement a 10-fold cross validation
# 2. We build 5 different models to predict species from flower measurements
# 3. We need to select the best model

# the algorithms are:
# 1. LDA
# 2. kNN
# 3. SVM
# 4. CART
# 5. Random Forest

# Run algorithms using 10-fold cross validation
seed = 1970
k = 10
control <- trainControl( method = "cv", number = k )
metric <- "Accuracy"

# LDA
set.seed( seed )
fit.lda <- train( Species ~ ., data = training, 
                  method = "lda", metric = metric, trControl = control )

# LDA^2
set.seed( seed )
fit.lda2 <- train( Species ~ ., data = training, 
                   method = "qda", metric = metric, trControl = control )

# kNN
set.seed( seed )
fit.knn <- train( Species ~ ., data = training, 
                  method = "knn", metric = metric, trControl = control )

# SVM
set.seed( seed )
fit.svm <- train( Species ~ ., data = training, 
                  method = "svmRadial", metric = metric, trControl = control )

# Trees
set.seed( seed )
fit.cart <- train( Species ~ ., data = training, 
                   method = "rpart", metric = metric, trControl = control )

# Random Forest
set.seed( seed )
fit.rf <- train( Species ~ ., data = training, 
                 method = "rf", metric = metric, trControl = control )

# Naive Bayes
set.seed( seed )
fit.nb <- train( Species ~ ., data = training, 
                 method = "naive_bayes", metric = metric, trControl = control )


# visualize the results in terms of out metric
# in this case the accuracy
obj_fit = objects( pattern = "fit." )
dt_res = lapply( obj_fit, function( x ) setDT( get( x )$resample ) )
for ( i in seq_along( dt_res ) ) {
  model = str_extract( obj_fit[ i ], "\\w+$" )
  dt_res[[ i ]][ , model := model ]
}

results = rbindlist( dt_res )


ggplot( results, aes( x = Resample, y = Accuracy, 
                      color = model, group = model ) ) +
  geom_line() + geom_point()

# summarize the results by computing mean and sd of Accuracy
# and 95% confidence intervals
viz_res = results[ , .( mean = mean( Accuracy ),
                        sd = sd( Accuracy ) ), by = model ]
viz_res[ , `:=` ( lwr = mean - qt( 0.95, df = 1 ) / k * sd, 
                  upr = mean + qt( 0.95, df = 1 ) / k * sd ) ]



ggplot( viz_res, aes( x = model, y = mean ) ) +
  geom_point() + 
  geom_errorbar( aes(ymin = lwr, ymax = upr)) + 
  scale_y_continuous( breaks = seq( 0.85, 1.05, by = 0.025 ) ) +
  ylab( "Mean Accuracy" ) +
  coord_flip() 

res_caret <- caret::resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, 
                                   svm=fit.svm, rf=fit.rf))
summary(res_caret)

dotplot(res_caret)


# FORECASTS ---------------------------------------------------------------

# we can clearly see how LDA outperforms the other methods
# let's use it to make predictions! 
# now we use the validation set to test the performance of the model
# on new data

predictions <- predict( fit.lda, validation )
confusionMatrix( predictions, validation$Species )



# REPEATED CV -------------------------------------------------------------

seed = 1970
k = 10
repetitions = 10
control <- trainControl( method = "repeatedcv", number = k, 
                         repeats = repetitions, verboseIter = TRUE )
metric <- "Accuracy"


# LDA
set.seed( seed )
fit.lda <- train( Species ~ ., data = training, 
                  method = "lda", metric = metric, trControl = control )

# LDA^2
set.seed( seed )
fit.lda2 <- train( Species ~ ., data = training, 
                   method = "qda", metric = metric, trControl = control )

# kNN
set.seed( seed )
fit.knn <- train( Species ~ ., data = training, 
                  method = "knn", metric = metric, trControl = control )

# SVM
set.seed( seed )
fit.svm <- train( Species ~ ., data = training, 
                  method = "svmRadial", metric = metric, trControl = control )

# Trees
set.seed( seed )
fit.cart <- train( Species ~ ., data = training, 
                   method = "rpart", metric = metric, trControl = control )

# Random Forest
set.seed( seed )
fit.rf <- train( Species ~ ., data = training, 
                 method = "rf", metric = metric, trControl = control )

# Naive Bayes
set.seed( seed )
fit.nb <- train( Species ~ ., data = training, 
                 method = "naive_bayes", metric = metric, trControl = control )


# visualize the results in terms of out metric
# in this case the accuracy
obj_fit = objects( pattern = "fit." )
dt_res = lapply( obj_fit, function( x ) setDT( get( x )$resample ) )
for ( i in seq_along( dt_res ) ) {
  model = str_extract( obj_fit[ i ], "\\w+$" )
  dt_res[[ i ]][ , model := model ]
}

results = rbindlist( dt_res )


pres = ggplot( results, aes( x = Resample, y = Accuracy, 
                             color = model, group = model ) ) +
  geom_line() + geom_point() + 
  theme( axis.text.x = element_text( angle = 90, hjust = 0.9 ) )
pres

pres + geom_smooth(se = FALSE)

# summarize the results by computing mean and sd of Accuracy
# and 95% confidence intervals
viz_res = results[ , .( mean = mean( Accuracy ),
                        sd = sd( Accuracy ) ), by = model ]
viz_res[ , `:=` ( lwr = mean - qt( 0.95, df = 1 ) / k * sd, 
                  upr = mean + qt( 0.95, df = 1 ) / k * sd ) ]

ggplot( viz_res, aes( x = model, y = mean ) ) +
  geom_point() + 
  geom_errorbar( aes(ymin = lwr, ymax = upr)) + 
  scale_y_continuous( breaks = seq( 0.85, 1.05, by = 0.025 ) ) +
  ylab( "Mean Accuracy" ) +
  coord_flip() 




