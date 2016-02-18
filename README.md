# ggkm
Kaplan-Meier plots with at risk table below

##About
This is a slightly modified version of the script created by Abhijit Dasgupta from statbandit [statbandit](http://statbandit.wordpress.com/2011/03/08/an-enhanced-kaplan-meier-plot/) with contributions by Gil Tomas and option to remove the legend and also draw marks at censoring locations by Nadieh Bremer.  I have included some further modifications regarding the positioning of legends etc...

##Dependencies
Several dependencies:
* ggplot2
* survival
* gridExtra
* reshape
* plyr
* grid


##Install

To easily install the ggkm package, install and run the devtools package and run the below in an R window:
```
install.packages("devtools")
library(devtools)
install_github("michaelway/ggkm")
library(ggkm)
```
