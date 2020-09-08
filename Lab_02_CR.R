###########################################
##### BIG DATA FOR BUSINESS DECISIONS #####
############ cod. 20564 ###################
########## BOCCONI UNIVERSITY #############
###########################################

###################
##### LAB 2 #######
## LINEAR MODELS ##
###################


# load the necessary packages
library( data.table )
library( ggplot2 )
library( ggridges )
library( grid )
library( gridExtra )
library( GGally )

# A SIMPLE EXAMPLE --------------------------------------------------------

# You can fit a linear regression with the function lm()
# let's generate some data, plot them and build a linear model

set.seed( 1970 )
x = seq( 0, 1, len = 50 )
y = 2 * x + 1 + rnorm( 50, 0, 0.1 )

dt = data.table( x = x, y = y )
p = ggplot( dt, aes( x, y ) ) +
  geom_point()
p

# we can clearly see that there is a linear relationship 
# let's try with a correlation
cor( dt$x, dt$y )

# fit a linear model
reg = lm( y ~ x, data = dt )
class(reg)

# this prints the estimated coefficients
reg

# there is much more information in the object reg
names( reg )

# in general, we are interested in the coefficients, the residuals and 
# the fitted values

# let's print some further information then with the function summary()
summary( reg )

# as you can see we now know the significance and the goodness of fit too
# let's now try to superimpose the linear function we just 
# estimated
p + geom_abline( slope = 2.03967, intercept = 0.97245, 
                 color = "red", size = 1 )

# or, more easily, we can just use ggplot
p + geom_smooth( method = "lm", color = "red", size = 1 )

# the red line represents the fitted values estimated by the model. 
# we can access this information with
dt[ , yhat := reg$fitted.values ]

# if you don't believe these are the same as the line...check it out! 
p + geom_smooth( method = "lm", color = "red", size = 1 ) +
  geom_point( aes( x, yhat ), color = "royalblue", size = 4, shape = 18 )

# CHECKING ASSUMPTIONS ----------------------------------------------------

# how do we check model's assumptions?
# we want to be sure that the residuals are normally distributed
# 
# we basically have two ways:
# 1. visualize them using plots
# 2. run a proper statistical test

# let's start with visualization
# first of all, we will need the residuals so we need to extract them
# and put them in a data structure.
dt[ , res := reg$residuals ]

# check there are no particular patterns
# you really wanna see just a boring cloud of points
p_res_scatter = ggplot( dt, aes( yhat, res ) ) +
  geom_point()
p_res_scatter

# make a histogram of the residuals
p_res_hist = ggplot( dt, aes( x = res, y = ..density.. ) ) +
  geom_histogram( binwidth = 0.04 )
p_res_hist

# make a normal probability plot
p_res_qq = ggplot( dt, aes( sample = res ) ) + 
  geom_qq() + geom_qq_line()
p_res_qq


marrangeGrob( list( p_res_scatter, p_res_hist, p_res_qq ), 
              ncol = 3, nrow = 1, top = "Model Evaluation" )


# the problem with this is that residuals have not been standardized
# it is strongly recommended you do the standardization so that you think
# of deviation in terms of standard deviations
# for instance, a 2-sigma deviation in a normal distribution occurs at
# 95% of its density
# what this means is that once we standardize the residuals, we can simply look
# at the number of points which lie above or below +2 and -2 respectively.
# If this number is higher than 5%, it means that the distribution is not 
# really a gaussian
# let's try it! 

reg_sigma = summary( reg )$sigma

dt[ , res_std := res / reg_sigma ] 

p_res_scatter_std = ggplot( dt, aes( yhat, res_std ) ) +
  geom_point() + 
  geom_hline( yintercept = 2, linetype = 4 ) +
  geom_hline( yintercept = -2, linetype = 4 )

# make a histogram of the residuals
p_res_hist_std = ggplot( dt, aes( x = res_std, y = ..density.. ) ) +
  geom_histogram( binwidth = 0.3 )

# make a normal probability plot
p_res_qq_std = ggplot( dt, aes( sample = res_std ) ) + 
  geom_qq() + geom_qq_line()

marrangeGrob( list( p_res_scatter_std, p_res_hist_std, p_res_qq_std ), 
              ncol = 3, nrow = 1, top = "Model Evaluation" )

outliers = nrow(dt[ res_std > 2 | res_std < -2 ])
N = nrow(dt)

outliers
outliers / N


# if you are annoyed by the visualization, which you shouldn't...
# you can run a test called Shapiro-Wilk

shapiro.test(dt$res_std)


# LEVERAGE POINTS ---------------------------------------------------------

# let's use an available dataset in R
data( anscombe )
anscombe

# fit four different  models
m1 = lm( y1 ~ x1, data = anscombe )
m2 = lm( y2 ~ x2, data = anscombe )
m3 = lm( y3 ~ x3, data = anscombe )
m4 = lm( y4 ~ x4, data = anscombe )

# print the coefficients

coeff = data.table( 
  model = paste0( "m", 1:4 ),
  intercept = c( coefficients(m1)[1],
                 coefficients(m2)[1],
                 coefficients(m3)[1],
                 coefficients(m4)[1]),
  slope = c( coefficients(m1)[2],
             coefficients(m2)[2],
             coefficients(m3)[2],
             coefficients(m4)[2]) 
)

# let's visualize the data points
p1 = ggplot( anscombe, aes( x = x1, y = y1 ) ) +
  geom_point() +
  geom_smooth( method = "lm", color = "red" )
p2 = ggplot( anscombe, aes( x = x2, y = y2 ) ) +
  geom_point() +
  geom_smooth( method = "lm", color = "red" )
p3 = ggplot( anscombe, aes( x = x3, y = y3 ) ) +
  geom_point() +
  geom_smooth( method = "lm", color = "red" )
p4 = ggplot( anscombe, aes( x = x4, y = y4 ) ) +
  geom_point() +
  geom_smooth( method = "lm", color = "red" )

marrangeGrob( list( p1, p2, p3, p4 ), ncol = 2, nrow = 2, top = "")

# well, these are completely different situations! 
# let's analyze the standardized residuals for each and every model
find_models = objects( pattern = "m\\d" )

# print a summary for each model and compute the residuals
dt_res = matrix( NA_real_, nrow = nrow( anscombe ), 
                 ncol = length( find_models ) )
colnames( dt_res ) = paste0( "res", 1:4 )
dt_res = as.data.table( dt_res )

for ( i_mod in seq_along( find_models ) ) {
  print(summary( get( find_models[ i_mod ] ) ))
  mod_res = residuals( get( find_models[ i_mod ] ) )
  mod_sigma = summary( get( find_models[ i_mod ] ) )$sigma
  dt_res[ , ( i_mod ) :=  mod_res / mod_sigma ]
  dt_res[]
}

# run Shapiro-Wilk over each model's residuals
apply( dt_res, 2, shapiro.test )

dt_res = cbind( anscombe, dt_res )
setDT( dt_res )

for ( i in seq_along( find_models ) ) {
  assign( paste0("p_res", i ),
          ggplot( dt_res, aes_string( x = paste0( "x", i ), 
                                      y = paste0( "res", i ) ) ) +
            geom_point() +
            geom_hline( yintercept = 2, linetype = 4 ) +
            geom_hline( yintercept = -2, linetype = 4 )
  ) 
}

marrangeGrob( list(p_res1, p_res2, p_res3, p_res4 ), 
              ncol = 2, nrow = 2, top = "" )

# well, model 2 clearly has a quadratic component...


# MULTIVARIATE LINEAR REGRESSION ------------------------------------------

# let's import an external file
cement = fread( "Lab_02/cement.csv" )
names( cement )

# The data refers to 10 different types of cement.
# The aim of this exercise is to run a multivariate linear model
# the dependent variable is the hardness of cement (cement_hardness)
# which we want to explain through the other variables which act
# as independent variables or covariates.
# We need to find the best model! 

# Let's take a look at some relationships in the data
ggpairs(cement)

# let's implement the linear regression
mod = lm( cement_hardness ~ aluminum + silicate + ferrite + silicate_bi,
          data = cement )
summary(mod)

# as you can see, we now have multiple parameters which have been estimated
# Also, it seems that not all the covariates are significant (p < 0.10)
# Please note that now the degrees of freedom are 8, given by
# n - p - 1, where n is the number os data points and p the number of 
# covariates
# 
# We can see how the model is globally significant and explains
# around 97% of the variance (i.e. Adjusted R-squared)
# 
# But we can probably improve the performance. Let's drop the least
# significant covariate. That is, the one with the highest p-value so,
# the silicate
mod2 = lm( cement_hardness ~ aluminum + ferrite + silicate_bi,
           data = cement )
summary(mod2)

# Looking good here! The Adjusted R-squared has improved together with the 
# global F-test...but still, the ferrite seems to be borderline
# let's drop it too! 
mod3 = lm( cement_hardness ~ aluminum + silicate_bi,
           data = cement )
summary(mod3)

# So the R-squared went down, but all the covariates are now significant! 
# This is a tricky situation in which is not trivial what to do.
# This could depend for instance if collecting the data for the ferrite
# is easy or not. 
# 
# Do we have always to perform variable selection manually?
# Of course not! We can use a method called "stepwise regression"
# Now, this method is not based on the concept of significance, but more
# on the likelihood of the model. In particular, it aims at minimizing 
# a metric called Akaike Information Criterion or AIC
# 
# Applying it is very easy! 
# Just pass it the model and provide the direction you want to perform
# the selection

# in this case, we start with the full model, the object "mod" and we 
# require the algorithm to select the best reduced model
step( mod, direction = "backward" )

# as you can see, it just deletes the silicate covariate
# so we might think that the best model would be "mod2". 
# one important thing is to always check the consistency of residuals
res2_std = mod2$residuals / summary( mod2 )$sigma
cement[ , `:=` ( yhat2 = mod2$fitted.values,
                 res2_std = res2_std ) ]

ggplot( cement, aes( yhat2, res2_std ) ) +
  geom_point() + 
  geom_hline( yintercept = -2, linetype = 4 ) +
  geom_hline( yintercept = 2, linetype = 4 )

ggplot( cement, aes( res2_std, ..density.. ) ) +
  geom_histogram( binwidth = 0.4 )

shapiro.test(res2_std)



# CATEGORICAL PREDICTOR ---------------------------------------------------

# Goal: understand if it is possible to use the variable which indicates
# the employee's gender ("Sex"), together with the years of service 
# ("Years_Service") to predict the average score ("Average_Score") that
# the candicate obtained in a questionnaire. Use a linear model 

job = fread( "Lab_02/job.csv" )

# let's plot the data to understand if there is any linear relationship

p = ggplot( job, aes( x = Years_Service, y = Average_Score ) ) +
  geom_point()
p

p + geom_smooth( method = "lm" )

# let's now add the gender in the plot...how?
# well, we can color the points that belong to the different genders
p_sex = p + geom_point( aes( color = Sex ) ) +
  scale_color_manual( values = c("deeppink", "royalblue" ) ) +
  theme( legend.position = "bottom" )
p_sex

# let's plot the regression line
p_sex + geom_smooth( method = "lm" )
# well, doesn't look great though...

# now we clearly see that there are two different relationships
# one that belongs to the males population and the other one to the females
# population. Let's then draw to different regression lines here
p_regs = p_sex + geom_smooth( aes( group = Sex, color = Sex ), method = "lm" )
p_regs


reg = lm( Average_Score ~ Years_Service, data = job )
summary( reg )



# QUANTILE REGRESSION -----------------------------------------------------

library( quantreg )

# Engel food expenditure data used in Koenker and Bassett(1982). 
# This is a regression data set consisting of 235 observations on income and 
# expenditure on food for Belgian working class households.
data("engel")
setDT( engel )

p_q = ggplot( engel, aes( x = income, y = foodexp ) ) +
  geom_point() +
  xlab("Household Income") + ylab("Food Expenditure")
p_q

# run a linear model
mod = lm( foodexp ~ income, data = engel )
summary(mod)

p_qlm = p_q + geom_smooth( method = "lm", se = FALSE, size = 1.2, color = "red" )
p_qlm

shapiro.test(mod$residuals)
# residuals seem to be strongly non-gaussian

results = data.table( yhat = mod$fitted.values, 
                      res_std = mod$residuals / summary( mod )$sigma )
ggplot( results, aes( yhat, res_std ) ) +
  geom_point() + 
  geom_hline(yintercept = -2, linetype = 4 ) +
  geom_hline(yintercept = 2, linetype = 4 )

ggplot( results, aes( sample = res_std ) ) + 
  geom_qq() + geom_qq_line()

# let's run a quantile regression
mod_q = rq( foodexp ~ income, data = engel )
summary( mod_q )

# as you can see, there no t-stat and p-values here
# this is because, by default the function computes confidence intervals
# let's now compute p-values...
summary( mod_q, se = "nid" )

p_qlm + geom_quantile( quantiles = 0.5 )

vec_q = c( seq( .1, .4, by = .1 ),
           seq( .6, .9, by = .1 ) )
p_qlm + 
  geom_quantile( quantiles = 0.5, size = 1.2 ) +
  geom_quantile( quantiles = vec_q, color = "gray63" , size = 0.8 )



# PANEL REGRESSION --------------------------------------------------------

library( plm )

# The data were originally analyzed by Cornwell and Rupert (1988) and 
# employed for assessing various instrumental-variable estimators for 
# panel models (including the Hausman-Taylor model). 
# Baltagi and Khanti-Akom (1990) reanalyzed the data, made corrections to 
# the data and also suggest modeling with a different set of instruments.
# PSID7682 is the version of the data as provided by Baltagi (2005), 
# or Greene (2008).

# A data frame containing 7 annual observations on 12 
# variables for 595 individuals.
# 
# id: integer indicating individual subject ID.
# # year: integer indicating year.
# experience: Years of full-time work experience.
# weeks: Weeks worked.
# occupation: factor. Is the individual a white-collar ("white") 
# or blue-collar ("blue") worker?
# industry factor. Does the individual work in a manufacturing industry?
# south: factor. Does the individual reside in the South?
# smsa: factor. Does the individual reside in a SMSA 
# (standard metropolitan statistical area)?
# married: factor. Is the individual married?
# gender: factor indicating gender.
# union: factor. Is the individual's wage set by a union contract?
# education: Years of education.
# ethnicity: factor indicating ethnicity. 
# Is the individual African-American ("afam") or not ("other")?
# wage: Wage.
# lwage: logarithm of wage

wages = fread( "Lab_02/wages.csv" )

# handy way to carry out summary statistics on just numeric columns
wages[ , lapply( .SD, summary ), 
       .SDcols = names(wages[, lapply(wages, is.numeric) == TRUE, with = FALSE]) ]


# viz 
ggplot( wages, aes( experience, lwage, color = gender, shape = ethnicity ) ) +
  geom_point() + 
  facet_wrap( . ~ year )

ggplot( wages, aes( year, lwage, color = ethnicity ) ) +
  geom_point() + 
  geom_smooth(method = "lm" ) +
  facet_wrap( . ~ gender )

p_rigde1 = ggplot( wages, aes( x = lwage, y = as.factor(year), fill = gender ) ) +
  geom_density_ridges( scale = 2, size = 0.6, 
                       alpha = 0.5, bandwidth = 0.08 ) + 
  xlab( "Log(Wage)" ) + ylab( "Year" ) +
  theme( legend.position = "bottom" ) 
p_rigde2 = ggplot( wages, aes( x = lwage, y = as.factor(year), fill = ethnicity ) ) +
  geom_density_ridges( scale = 2, size = 0.6, 
                       alpha = 0.5, bandwidth = 0.08 ) +
  xlab( "Log(Wage)" ) + ylab( "Year" ) +
  theme( legend.position = "bottom" ) 
p_rigde3 = ggplot( wages, aes( x = lwage, y = as.factor(year), fill = married ) ) +
  geom_density_ridges( scale = 2, size = 0.6, 
                       alpha = 0.5, bandwidth = 0.08 ) +
  xlab( "Log(Wage)" ) + ylab( "Year" ) +
  theme( legend.position = "bottom" ) 

marrangeGrob( list( p_rigde1, p_rigde2, p_rigde3 ), nrow = 1, ncol = 3, top = "")


psid_ht1 <- plm(lwage ~ weeks + south + smsa + married +
                  experience + I(experience^2) + occupation + 
                  industry + union,
                data = wages, model ="within" )

psid_ht2 <- plm(lwage ~ weeks + south + smsa + married +
                  experience + I(experience^2) + occupation + 
                  industry + union,
                data = wages, model ="random" )

summary(psid_ht1)
summary(psid_ht2)

# H0: random effects model
# H1: fixed effects model
phtest(psid_ht1, psid_ht2)





# END OF SCRIPT