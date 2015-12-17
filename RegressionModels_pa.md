# Regression Models - Course Project
Pier Lorenzo Paracchini  

#Executive Summary

#Data Analysis
##Exploratory Data Analysis
The dataset include 32 observations of 11 features. Each onservations comprises information for a specific automobiles (1973 - 1974 model). More information about the features can be found running `help(mtcars)`.  

Focus is on two specific features __mpg__ (miles per US gallon) and __am__ (type of transmission - __0: automatic__, __1: manual__). The available datase contains 19 automatic car models and 13 manual car models. The observations of `mpg` by type of transmission `am` can be seen in the hystograms below (__note!__ black line represents the sample mean for the group).

![](RegressionModels_pa_files/figure-html/unnamed-chunk-1-1.png) 

The `am` feature (predictor) is visibly related to the `mpg` feature (outcome) as we can see from the sample mean of each group. The "automatic" group has a lower sample mean (17.147 Miles/ Gallon) than the "manual" group (24.392 Miles/ Gallon).  

##Is an automatic or manual transmission better for MPG?  
Let's fit a __linear model__ using `mpg` (as outcome) with `am` as predictor and let's start to look at the coefficients   

```
##              Estimate Std. Error   t value     Pr(>|t|)
## (Intercept) 17.147368   1.124603 15.247492 1.133983e-15
## am1          7.244939   1.764422  4.106127 2.850207e-04
```

* __automatic__ models (reference group) have an estimated mpg of 17.147 (miles per gallon) with a standard error of 1.125 (miles per gallon).

* __manual__ models have an increased estimated mpg of 7.245 (miles per gallon) (over the reference group) with a standard error of 1.764 (miles per gallon). The P-value of 2.8502074\times 10^{-4} (statistically significant) confirms that the __null hypothersis__ (having a coefficient null) can be rejected.

According to this very simple linear model __automatic__ transmission is better for __mpg__ than __manual__ transmission. 

#Description __(ToBeDeleted)__  
__Interested in exploring the relationship between a set of variables and miles per gallon (MPG) (outcome)__. They are particularly interested in the following two questions:

* <U+201C>Is an automatic or manual transmission better for MPG<U+201D>  
* "Quantify the MPG difference between automatic and manual transmissions"

__Constraints__ Report must be:

* Written as a PDF printout of a compiled (using knitr) R markdown document.  
* Brief. Roughly the equivalent of 2 pages or less for the main text. Supporting figures in an appendix can be included up to 5 total pages including the 2 for the main report. The appendix can only include figures.  
* Include a first paragraph executive summary.


#Appendix
Code chunks and plots used in the previous sections.  
##Dataset Structure    

```r
str(mtcars)
```

```
## 'data.frame':	32 obs. of  11 variables:
##  $ mpg : num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
##  $ cyl : num  6 6 4 6 8 6 8 4 4 6 ...
##  $ disp: num  160 160 108 258 360 ...
##  $ hp  : num  110 110 93 110 175 105 245 62 95 123 ...
##  $ drat: num  3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
##  $ wt  : num  2.62 2.88 2.32 3.21 3.44 ...
##  $ qsec: num  16.5 17 18.6 19.4 17 ...
##  $ vs  : num  0 0 1 1 0 1 0 1 1 1 ...
##  $ am  : num  1 1 1 0 0 0 0 0 0 0 ...
##  $ gear: num  4 4 4 3 3 3 3 4 4 4 ...
##  $ carb: num  4 4 1 1 2 1 4 2 2 4 ...
```
### Some more details about `mpg` and `am` features  
Some `mpg` relevat statistics.  

```r
summary(mtcars$mpg)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   10.40   15.42   19.20   20.09   22.80   33.90
```
Number of car models by type of transmission.  

```r
table(mtcars$am)
## 
##  0  1 
## 19 13
```
Plot showing the number of car models by type of transmission and mpg.  

```r
require(dplyr)
require(ggplot2)
freqData <- as.data.frame(table(mtcars$mpg, mtcars$am))
names(freqData) <- c("mpg", "am", "freq")

g <- ggplot(filter(freqData, freq > 0), aes(x = am, y = mpg))
g <- g  + scale_size(guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = freq+2, show_guide = TRUE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")
g <- g + ggtitle("Mile Per (US) Gallon vs. Transmission Type")
g <- g + xlab("Transmission Type - 0: automatic, 1: manual")
g <- g + ylab(" Miles/(US) Gallon")
g
```

![](RegressionModels_pa_files/figure-html/amDetailsExtra-1.png) 
Histograms for `mpg` by `am` (transmission type)  

```r
require(ggplot2)
require(dplyr)

mpg_avg_0 <- mean(filter(mtcars, am == 0)$mpg)
mpg_avg_1 <- mean(filter(mtcars, am == 1)$mpg)


#Other possibility
data_0 <- data.frame(mpg = mtcars$mpg, am = as.factor(mtcars$am))
data_1 <- data.frame(am = c("0", "1"), mean = c(mpg_avg_0, mpg_avg_1))

g <- ggplot(data_0, aes(x = mpg, fill = am))
g <- g + geom_histogram(colour = "black", binwidth=1)
g <- g + facet_grid(am ~ .)
g <- g + geom_vline(data = data_1, aes(xintercept = mean), lwd = 1)
g
```
