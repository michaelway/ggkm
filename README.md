# ggkm
Kaplan-Meier plots with at risk table below

![](https://github.com/michaelway/ggkm/Images/ggkm.gif)

##Install

To install the ggkm package, install and run the devtools package and then run the code below in a R console:

```
install.packages("devtools")
library(devtools)
install_github("michaelway/ggkm")
library(ggkm)
```


### Examples

#### Basic Kaplan-Meier plot

Loading the "colon" dataset from the survival package allows us to create a 

```
#Load dataset

library(survival)
data(colon)
fit <- survfit(Surv(time,status)~rx, data=colon)

#Plot the data
ggkm(fit)

#Save a PNG image using R plotting device
png("KM1.png", units='in', height=5, width=7, res = 220)
ggkm(fit)
dev.off()

```
#### Basic Kaplan-Meier plot with at-risk table

An at risk table can easily be added.

```
ggkm(fit, table = TRUE)
```


#### Customise it!

More advanced plots can be created by modifying a number of parameters.  For example the title, y-strata name and lables, x-strata name can all be modified; a 95% confidence interval can be added; the dashtype can be removed or the symbol changed; and, a P-value from the survival object may also be included.

```
ggkm(fit, table = T, 
     main="Survival  by treatment group", 
     ystratalabs = c("Placebo", "No-drug", "Drug"),
     ystrataname = "Group",
     xlabs = "Time-to-death (days)",
     dashed=TRUE,
     pval=TRUE,
     ci=TRUE,
     shape = 4,
     linecols = "Set2")

```

### Points

The marker points which can be modified based on the the numbering of the point symbols given in the image below.

![Available points](http://1.1.1.2/bmi/www.sthda.com/sthda/RDoc/images/points-symbols.png)

### Colours

The colours of the lines are based on colour brewer palletes.  To set a pallete use a palette name shown in the image below.

![Available line colours](http://www.datavis.ca/sasmac/brewer.all.jpg)

