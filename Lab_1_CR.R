###########################################
##### BIG DATA FOR BUSINESS DECISIONS #####
############ cod. 20564 ###################
########## BOCCONI UNIVERSITY #############
###########################################

#################
##### LAB1 1 ####
## INTRO TO R ##
################

# Topics discussed in this lab 
# 1. Base commands and functions: scalars, vectors, matrices and related 
# mathematical operations
# 2. Import/Export of dataframes
# 3. Plots

# R is an interpreted language. It is possible to:
# 1. write code directly in the console
# 2. write code in a script (like this one) and then execute it 
# in the console line by line or by selection. This method is strongly
# recommended. 
# 3. To execute the code, position the cursor at a given line and press:
# Windows/Linux: ctrl + r
# MacOS: cmd + enter


# VERY IMPORTANT OBSERVATIONS

# 1. R has a proper working directory in which the data and the scripts
# are stored. 
# 2. To select the working directory, you can go to 
# 'Session -> Set Working Directory -> Choose Directory'
# OR
# just type the following command:

setwd('~/')

# If you get lost and want to know the working directory, type:
getwd()

#	To list what's inside the current directory, type:
list.files()

# 1. To save a R script, the file must have the extension '.R'
# 2. To create a new script, go to 'File -> New File -> R Script' 
# OR
# just press:
# Windows/Linux: ctrl + shift + N
# MacOS: cmd + shift + N
# 3. To save a script, just do the usual saving procedure: 
# Windows/Linux: ctlr + s
# MacOS: cmd + s

# As you can see, there is a lot writing here all precede by #
# You want to use '#' to initiate a comment of your code
# Comments are important to keep track of what you are doing...
# do that cause you will forget what those specific lines of code were doing


# R AS A CALCULATOR -------------------------------------------------------

2+3

exp(-2)

log(-1)

# if you find yourself in trouble, you can always ask for help! 
# R has great inline help guides
?log

# oppure

help.search('logarithm')


# R AS A PROGRAMMING LANGUAGE ---------------------------------------------

# Let's define variables, or R objects
x <- 3

x

# <- is called assignment operator. You can use the usual = if you want.
y = 4

y

x + y

# you can dynamically store the result of a given operation by assigning it
# to a new variable as follows:

z = x + y

z

# to list all the variables defined in the current global environment 
# (i.e. what R has in the current memory), type:
ls()

# To remove specific variables use rm():

rm(x,z)

ls()

# to remove all the variables do this:
rm(list=ls())

ls()



# VECTORS -----------------------------------------------------------------

v = c(2, 3, 7, 10)

# c() is the function to concatenate objects, in this case some numbers

v

# we can concatenate strings as well

gibberish = c('a', 'b', 'c', 'd')
gibberish

# There are few shortcuts to build vectors

# build a sequence

x = 1:100
x

# build a sequence of length 5
y = seq(1, 10, len = 5)
y

# build a sequence between 0 and 10 with a step of .5
z = seq(1, 10, by = 0.5)

z

# we can go the other way around

x2 = 10:1
x2

# please note the sign 
z2 = seq(10, 1, by = -0.5)

z2

# we can also build sequences out of repetitions

w = rep(1, 10)
w

# as you can see, rep() has two arguments:
# 1. the element you want to repeat
# 2. the number of times you want to repeat the element

w2  =  rep(1:4 , times = 3)
w2

# there are other parameters too! 
w2bis = rep(1:4, each = 3)
w2bis

# you can do this with strings too
char = rep('b', 4)
char

# length of a vector

length(w2)

length(parola)

# and you can combine all these functions at will
l = rep( seq( 1, 10, len = length(w2) ), 2 )
l


# VECTOR SLICING AND INDEXING ---------------------------------------------

# this extracts the fourth element out of vector l
# remember: R starts counting from 1 not from 0 like Python
l[4]

a = l[7]

a

l[2:10]

l[c(1,3)] # the first and the third elements 

l[-1] # all elements but the first one

l[-length(l)] # all elements but the last one


# MATRICES ----------------------------------------------------------------

# a matrix is basically a 2D vector
# you can build a matrix by passing some data, the number of rows and columns
# also, you need to specify the way you want the elements to be inserted

W  =  matrix(data = 1:12, nrow = 4, ncol = 3, byrow = FALSE)
W
Wc  =  matrix(data = 1:12, nrow = 4, ncol = 3, byrow = TRUE)
Wc

# or you can just stack up rows

W  =  rbind(c(1,5,9),c(2,6,10),c(3,7,11),c(4,8,12))
W

# or columns

W  =  cbind(1:4,5:8,9:12)
W

# You can extract elements in pretty much the same way you did with vectors
# only we need to tell which axis we want to work on

W[ 2, 3 ]

W[ 2:4, 1]

W[ 4, c(1,3) ]

# give me the entire third row
W[ 3, ]

# give me the entire second column
W[ , 2 ]

# give me a sub-matrix

W[ c(1,3,4), 2:3]



# MATHEMATICAL OPERATIONS IN R --------------------------------------------

# IMPORTANT: by default, R carries out operations element-by-element

a = 1
b = 2
c = c(2, 3, 4)
d = c(10, 10, 10)
e = c(1, 2, 3, 4)
f = 1:6

# this is 4x3
W 

Z = rbind( rep(0, 3), 1:3, rep(10, 3), c(4, 7, 1))

Z
# let's check Z's dimensions
dim(Z) == dim(W)

# Operations on scalars and vectors

a+b # scalar + scalar
c+d # vector + vector
a*b # scalar * scalar
c*d # vector * vector (element wise)
c+a # vector + scalar
c^2 # PLEASE NOTE: always element wise operation
exp(c) # same here

# the function c() concatenates any object! Including vectors
c(c, d, e) 


# THE RECYCLING -----------------------------------------------------------

# PLEASE NOTE: THIS IS SUPER IMPORTANT AND DANGEROUS!
# if we ask R to carry out an operation with a vector whose dimension is 
# "wrong", R won't complain too much and will force the operation by 
# re-using the elements...
# Let's see an example

c
e

c + e
 
# As you can see, R warns us (warning message) though the operation 
# is carried out

c 
f

c + f

if ( length(c) != length(f) ) {
  stop("Arghhh")
}

# Now R doesn't warn us at all!!!
# This is because it used the vector "c" twice. This is even more weird and
# dangerous. Beware of vectors which have a dimension that are multiple

# Another handy feature is to pre-allocate in memory a given object for
# later use
# In this case a 4x3 matrix
W = matrix(NA_integer_, nrow = 4, ncol = 3 )
W

W = matrix(0, nrow = 4, ncol = 3)
W


W2 = matrix( c(0, 1), 4, 3 )
W2

W3 = matrix( c(0, 1, 2), 4, 3)
W3


# OPERATIONS WITH MATRICES ------------------------------------------------

# matrix + matrix (element wise)
Z + W 

# matrix * matrix (element wise)
Z * W 

# sum of all the components of vector c
sum(c) 

# sum of all the components of matrix Z
sum(Z)

# same thing for the product
prod(c)
prod(Z)

# transposing a matrix
V = t(W2) 
V
dim(W2)
dim(V)

# element wise product...not working! 
V * W 

# matrix multiplication
V %*% W2
W %*% V 

# matrix + scalar (watch out for recycling)
W + a

# matrix + vector
W
c
W + c

# adding a column
V2 = cbind(V, c)
V2

# add a row
W2 = rbind(W, c)
W2

# matrix dimensions
dim(W2)
nrow(W2)
ncol(W2)

# compute the determinant, eigenvectors and eigenvalues
# inverse, and solution for linear systems
P = matrix(c(1,3,2,8,5,6,7,9,6),3,3)

det(P)

AU = eigen(P)
AU

invP = solve(P)
invP

b = c( 1, 1, 1) 
x = solve(P, b)
x



# DATAFRAMES --------------------------------------------------------------

# this is the most important object you can find in R. 
# Long story short, it's like an Excel spreadsheet in R
# Let's see an example

diagnosis = c("healthy","healthy","ill","healthy","ill","ill","ill","healthy")
concentration = c(0.43,0.35,0.45,0.37,0.46,0.43,0.5,0.42)
concentration2 = c(0.43,0.35,0.45,0.37,0.46,0.43,0.5)

D = data.frame(diagnosis, concentration)
D
D2 = data.frame(diagnosis, concentration2)
D2
class(D)
dim(D)

mean( D$concentration )

# whenever we import a dataset, like a txt or a csv file, generally this gets
# imported as a data.frame
geyser = read.table('Lab_01/geyser.txt', header = TRUE)
class(geyser)

head(geyser)
names(geyser)

# let's compute some general descriptives
summary( geyser )

# let's extract only the observations for which the waiting time
# is greater than 90
geyser[ which( geyser$waiting > 90 ),  ]


# WORKING WITH OTHER PACKAGES ---------------------------------------------

# PLEASE NOTE: there are several ways to work with tabular data like 
# the geyser dataset. Some might have been already exposed to the so called
# tidyverse which has a very specific workflow in R. 
# Personally, I find the the world of data.table is much easier, much faster
# to work with...and much fun too! 
# data.table is a R package which extends R's functionalities.
# to load a package or library do the following: 

library( data.table )
# if you didn't see any errors...you are good to go! 
# if you did have errors, you should probably install the package by doing:

# install.packages("data.table")

# let's try to load geyser.txt with data.table
geyser = fread( "Lab_01/geyser.txt" )
names(geyser)
class(geyser)

summary(geyser)

geyser[ which( geyser$waiting > 90 ),  ]

geyser[ waiting > 90, mean(duration) ]


D
setDT(D)

D[ diagnosis == "healthy", mean( concentration ) ]
D[ diagnosis == "ill", mean( concentration ) ]

D[ , .( bla = mean( concentration ) ), by = diagnosis]
 
# PLOTS AND GRAPHICS ------------------------------------------------------

# generate some data
x = seq(1,10,len=100)

# plot 
plot(x, sin(x))

# add a line to the old plot
plot(x, sin(x), type='l') 
points(x,sin(2*x),col=4,type='l') 

quartz()
plot(x, exp(x))
abline(h=0,col="green",lty=3,lwd=2)



# GGPLOT2 -----------------------------------------------------------------

# really, the capacity of the default plot() function is really poor! 
# there is a library which has been specifically designed to create 
# professional plots
# this is the one I will use througout the course
# it's called ggplot2 and you can find the manual here
# https://ggplot2.tidyverse.org
# 
# it's a big library which contains lot of stuff
# and it sounds complicated at the beginning so...keep practicing! 

# let's do the plot through ggplot2
# first, you will need a data.frame/data.table to do anything

to_plot = data.table( x = x, y = sin( x ) )
to_plot[ , y2 := sin(2*x) ]

library(ggplot2)

p_basic = ggplot( to_plot, aes( x = x, y = y ) ) + 
  geom_line() 

p_basic

# adding more fancy stuff
p_basic + geom_point()

p_all = p_basic + geom_point()

p_all = p_all + 
  geom_line( aes( x = x, y = y2 ), color = "royalblue" ) + 
  geom_point( aes( x = x, y = y2 ), color = "royalblue" )
p_all

p_final = p_all + theme_bw() +
  xlab( "Time" ) + ylab( "Intensity" ) +
  ggtitle( "Intensity over time", "Simulation study" )
p_final



# make some visualizations of the geyser dataset
ggplot( geyser ) + 
  geom_histogram( aes( x = waiting, y = ..density.. ), binwidth = 2 ) + 
  geom_density( aes( waiting ), color = "red" ) 

ggplot( geyser, aes( x = waiting, y = duration ) ) +
  geom_point()

geyser[ , groups := ifelse( duration < 3, "short", "long" ) ]
p = ggplot( geyser, aes( x = waiting, y = duration, color = groups ) ) +
  geom_point() + 
  scale_x_continuous( breaks = seq(40, 120, by = 10 ) ) + 
                        theme( legend.position = "bottom" )
p

# smoothed lines
p + geom_smooth( data = geyser, 
                 formula = y ~ x, method = "loess")

# linear models
p + geom_smooth( data = geyser, 
                 formula = y ~ x, method = "lm")


# SAVING OBJECTS ----------------------------------------------------------

# saving data.frames in a csv file
write.table(geyser, file = "Lab_01/mynewgeyser.csv" )

# with data.table
fwrite( geyser, file = "Lab_01/mynewgeyser.csv" )

# saving R object in RData file 
save(geyser, file = 'Lab_01/geyser.RData' )

save.image('Lab_01/geyser.RData')

rm(geyser)

load('Lab_01/geyser.RData')

geyser

# END OF SCRIPT