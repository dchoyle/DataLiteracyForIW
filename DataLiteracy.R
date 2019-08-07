

set.seed( 1729 )

#### CTR Example
n1 <- 42
n2 <- 34

ctr_population1 <- 0.1
ctr_population2 <- 0.075

x1 <- runif( n1 ) < ctr_population1
x2 <- runif( n2 ) < ctr_population2


f1 <- sum( x1 )/length( x1 )
f2 <- sum( x2 )/length( x2 )
print( paste( f1, f2 ) )


#### Random scatter example
populationMean <- 1.0
plot( populationMean + (3.0*rnorm( n=1000 )), ylab="y" )
abline( h=populationMean, col="red", lwd=2 )

xtmp <- 3.0 * rnorm( 1000)
MAPE <- abs( xtmp / ( 1.0 + xtmp ) )


#### Sample mean example

## Define properties of the sample of data
nSample <- 10 # number of data points to use
populationMean <- 5.6 # ground truth value
minMeasurement <- 3.0 
maxMeasurement <- (2.0*populationMean) - minMeasurement

## sample the data
measurements <- runif( n=nSample, min=minMeasurement, max=maxMeasurement )

## Look at the mean of the data sample
sampleMean <- mean(measurements)
sampleMean

### Let's repeat that little experiment a number of times
nExperiments <- 1000
lotsOfExperiments <- matrix( runif(n=nSample*nExperiments, 
                                   min=minMeasurement, 
                                   max=maxMeasurement ),
                             nrow = nExperiments,
                             ncol = nSample )

lotsOfSampleMeans <- apply( lotsOfExperiments, 1, mean )

## Look at spread of sample means
hist( lotsOfSampleMeans, br=30, xlab="Sample Mean" )

## What is the average (across all the experiments) of those sample means
meanOfSampleMeans <- mean( lotsOfSampleMeans )
meanOfSampleMeans

### Let's repeat the experiment lots of times, but with a range of 
### sample sizes

nSample.seq <- seq( 10, 200, 10 )
nExperiments <- 1000
experiment <- c( 1:nExperiments )

lotsOfExperiments2 <- expand.grid( experiment, nSample.seq )
colnames( lotsOfExperiments2 ) <- c( "experimentNumber", "sampleSize" )
lotsOfExperiments2$sampleMean <- rep( NA, nExperiments * length( nSample.seq ) )

for( i in 1:length( nSample.seq ) ){
  data_tmp <- matrix( runif(n=nSample.seq[i]*nExperiments, 
                            min=minMeasurement, 
                            max=maxMeasurement ), 
                      nrow = nExperiments, 
                      ncol = nSample.seq[i] )
  lotsOfExperiments2$sampleMean[(1+((i-1)*nExperiments)):(i*nExperiments)] <- apply( data_tmp, 1, mean )
}

boxplot( sampleMean ~ sampleSize, data = lotsOfExperiments2,
         xlab = "Sample size",
         ylab = "Sample mean",
         main = "Distribution of sample mean vs sample size")
abline( h=populationMean, col="red" )

## plot standard error of sample mean against sample size
lotsOfExperiments2$sampleMean_Delta <- lotsOfExperiments2$sampleMean - populationMean
standardError <- aggregate( sampleMean_Delta ~ sampleSize, data=lotsOfExperiments2, FUN=sd)
colnames( standardError ) <- c("sampleSize", "standardError")
plot( x=standardError$sampleSize, y=standardError$standardError,
      xlab="Sample Size", 
      ylab="Standard error of sample mean")

# add theory line
sd_uniform <- (maxMeasurement - minMeasurement)/sqrt(12)
lines( x=nSample.seq, y=sd_uniform/sqrt(nSample.seq), col="red")


#### Traffic monitoring

## Generate data

# setup basic variables
nTimepoint <- 3000
df <- data.frame( "timepoint" = seq(1, 1+nTimepoint, 1) )
df$hourOfDay <- df$timepoint %% 24
df$hourOfDay_factor <- factor( df$hourOfDay )
df$dayOfWeek <- round( df$timepoint / 24 ) %% 7
df$dayOfWeek_factor <- factor( df$dayOfWeek )
df$anomaly <- (runif( n=nrow( df ) ) < 0.005)*1 # anomalies occur 5 in 1000 times

# setup size of effects of the various variables
effect_baseline <- 0.0
effect_anomaly <- -3
effects_hourOfDay <- sin( 2.0*pi*seq(1,24,1)/24 )
effects_dayOfWeek <- 0.2*cos( 2.0 * pi * seq(1,7,1)/7 )[1:6]
effects_all <- c( effects_hourOfDay, effects_dayOfWeek, effect_anomaly )

# create dummy variables (using design matrix utilities in R) to aid
# calculation of linear predictors
f <- as.formula( ~ 0 + hourOfDay_factor + dayOfWeek_factor + anomaly )
mm <- model.matrix( f, data=df )

# calculate linear predictor values and convert from log-odds to 
# probability of a session visiting the page in question
linearPredictor <- effect_baseline + ( mm %*% effects_all )
probabilityVisit <- exp( linearPredictor ) / ( 1.0 + exp( linearPredictor ) )

# sample number of page visits given probability of sessions visiting the page
# We will need to randomly generate the number of sessions. Ideally we would 
# put some auto-correlation in the number of sessions, but for simplicity we 
# will assume i.i.d uniform.
df$nVisits <- rep( NA, nrow( df ) )
nSession <- sample( seq( 100000, 120000, 10), size=nrow(df), replace=TRUE )
for( i in 1:nrow( df ) )
{
  df$nVisits[i] <- rbinom( n=1, size=nSession[i], prob=probabilityVisit[i] )
}

# plot page visit numbers against timepoint along with 
# locations of the anomalies
plot( probabilityVisit, xlab="Timepoint", ylab="Probability of page visit" )
plot( df$timepoint, df$nVisits, xlab = "Timepoint", ylab="Page visits" )
points( df$timepoint[df$anomaly==1], df$nVisits[df$anomaly==1], pch=19, col="red" )

# analyse data to see if we can detect the anomalies from the data
# First calculate hour-on-hour changes. This will remove effects of 
# slow-moving seasonality, e.g. seasonality that varies over timescales of
# weeks and months.
df$hr_on_hr <- c( NA, (df$nVisits[-1] - df$nVisits[1:(length(df$nVisits)-1)]) )
df$hr_on_hr <- df$hr_on_hr / df$nVisits

# construct estimates of typical hour-on-hour change along with its scale.
# We will use median and IQR to estimate the location and scale of the hour-on-hour
# changes, as median and IQR are robust estimates and so less affected by 
# outliers
hr_on_hr_medians <- aggregate( hr_on_hr ~ hourOfDay, data=df, FUN=median)
hr_on_hr_iqrs <- aggregate( hr_on_hr ~ hourOfDay, data=df, FUN=IQR)
colnames( hr_on_hr_medians ) <- c( "hourOfDay", "hr_on_hr_median" )
colnames( hr_on_hr_iqrs ) <- c( "hourOfDay", "hr_on_hr_iqr" )

df <- merge( df, hr_on_hr_medians, by=c("hourOfDay"), all.x=TRUE )
df <- merge( df, hr_on_hr_iqrs, by=c("hourOfDay"), all.x=TRUE )
df$scaledDeviation <- (1.349 * ( df$hr_on_hr - df$hr_on_hr_median ) / df$hr_on_hr_iqr)

# we alert if the hour-on-hour change in more than 3 standard deviations below the median 
# for the particular hour of the day.
df$alert <- df$scaledDeviation < -3 


# find timepoints where we would have flagged an alert
idx <- (df$alert==TRUE) & (!is.na(df$alert))
df[idx, ]

# plot where we have flagged an alert
plot( df$timepoint, df$nVisits, xlab = "Timepoint", ylab="Page visits" )
points( df$timepoint[idx], df$nVisits[idx], pch=19, col="red" )


### 'Real data'

# Boston housing data
install.packages('MASS')
library( MASS )
boston_raw <- Boston

hist( boston_raw$medv, br=30, xlab="Median value" )


# Loan default data
install.packages('ISLR')
library( ISLR )

defaultData_raw <- ISLR::Default
head( defaultData_raw )

hist( defaultData_raw$balance, br=30, xlab="Balance" )





