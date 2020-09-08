library( data.table )
library( ggplot2 )
library( plm )
library( grid )
library( gridExtra )
library( lemon )



# EXPLORING RELATIONSHIPS -------------------------------------------------

# ad hoc linear regression
set.seed( 2017 )
x = 1:100
y = x + rnorm( 100, 1, 3 )
set.seed( 2017 )
y2 = cumsum( sort( rexp( 100, 1 ) ) )

quartz()
p = qplot( x, y ) + ylim( -5, 110 )
p
p + geom_smooth( method = "lm", se = FALSE )
p = qplot( x, y2 ) + ylim( -10, 100 )
p 
p + geom_smooth( method = "lm", se = FALSE )


data( "AirPassengers" )
p = autoplot( as.zoo( AirPassengers), geom = "line" ) +  
  ylab( "Passengers" ) + xlab( "Year" ) + 
  scale_x_continuous( breaks = seq( 1949, 1961, 2 ) ) + ylim( 0, 700 )
p
p + geom_smooth( method = "lm", se = FALSE )


# LOGISTIC CURVE ----------------------------------------------------------

x = seq( -1, 1, by = 0.01 )
d = data.table( x = x )
b0 = 0
b1 = c( 100, 50, 20, 10, 5, 3, 2, 1 ) # slope

for ( i in seq_along( b1 ) ) {
  y = exp( ( b0 + b1[i]*x ) ) / ( 1 + exp( ( b0 + b1[i]*x ) ) )
  d[ , paste0( "y", i ) := y ]
  d[]
}

d_long = melt( d, id.vars = "x" )
p = ggplot( data = d_long, aes( x = x, y = value,
                                color = variable ) ) + geom_line()
p + ylab( "P(x)") + ggtitle( "S-shaped curve" ) + 
  scale_colour_manual(name = "Slope Values",
                      labels = c( "100", "50", "20", "10", "5", "3", "2", "1" ),
                      values = unique( ggplot_build( p )$data[[ 1 ]]$colour ) )


# LONGITUDINAL / PANEL  ---------------------------------------------------

data( "Gasoline" )
setDT( Gasoline )

p0 = ggplot( data = Gasoline ) +
  geom_point( aes( x = lrpmg, y = lgaspcar ) )

p0 + geom_smooth( aes( x = lrpmg, y = lgaspcar ), 
                 method = "lm", color = "red", se = FALSE, size = 2 )
p1 = ggplot( data = Gasoline ) +
  geom_point( aes( x = lrpmg, y = lgaspcar, color = country ) ) 
p1 + 
  geom_smooth( aes( x = lrpmg, y = lgaspcar ), 
               method = "lm", color = "red", se = FALSE, size = 2 ) + 
  geom_smooth( aes( x = lrpmg, y = lgaspcar, color = country ), 
                  method = "lm", se = FALSE )


p2 = ggplot( data = Gasoline ) + 
  geom_line( aes( x = year, y = lrpmg, color = country ) ) 
p3 = ggplot( data = Gasoline ) + 
  geom_line( aes( x = year, y = lgaspcar, color = country ) ) 

grid_arrange_shared_legend( p2, p3, ncol = 1, nrow = 2, position = "right" )

