Basic Descriptive Statistics ---- SAS
========================================================
 - proc univariate
 - proc means
 
proc univariate ---- SAS
========================================================

```r
proc univariate <options> ; 
  <options>
run;

proc univariate data=dcn;
  var ITSN1_N;
run;
```

proc means ---- SAS
============================

```r
proc meansa <options> ; 
  <options>
run;

proc means data=dcn;
  var ITSN1_N;
run;
```

Basic Descriptive Statistics ---- R
========================================================

```r
data_cortex <- read.csv("~/Desktop/data_cortex.csv")
## kind of ugly output, but just as useful
summary(data_cortex[2], na.rm=TRUE)
```

```
    DYRK1A_N     
 Min.   :0.1453  
 1st Qu.:0.2881  
 Median :0.3664  
 Mean   :0.4258  
 3rd Qu.:0.4877  
 Max.   :2.5164  
 NA's   :3       
```

Basic Descriptive Statistics ---- R
========================================================

```r
var(data_cortex[2], na.rm=TRUE)
```

```
           DYRK1A_N
DYRK1A_N 0.06218139
```


Basic Descriptive Statistics ---- R
========================================================

```r
colMeans(data_cortex[c(2,5)], na.rm=TRUE)
```

```
 DYRK1A_N     NR1_N 
0.4258102 2.2972691 
```

 Basic Descriptive Statistics ---- R
========================================================

```r
sapply(data_cortex[2:5], mean, na.rm=TRUE)
```

```
 DYRK1A_N   ITSN1_N    BDNF_N     NR1_N 
0.4258102 0.6171020 0.3190884 2.2972691 
```

Basic Descriptive Statistics ---- R
========================================================
sapply is extremely useful for working with data in R.
In order to understand sapply better we first need
to understand the some basic data structures.


