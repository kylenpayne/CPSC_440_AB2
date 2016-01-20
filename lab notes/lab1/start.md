Software
========================================================

- Software is a huge component of statistical analysis
- We will support SAS and R usage for this course
- SAS is a suite of tools for statistical analysis
- R is a programming language primarily for statistical analysis

SAS
========================================================
SAS inc. is the worlds largest privately held software company.

* Used by most fortune 500 companies
http://archive.fortune.com/2010/01/21/technology/sas_best_companies.fortune/index.htm
* SAS Software has over 200 components
  - Can perform many statistical analyses with limited effort
  - Presents a robust suite of industry standard tools.
-https://en.wikipedia.org/wiki/SAS_(software)#cite_note-encycl-2

R 
========================================================
- "Much closer to the metal" than SAS
- Can be used to create your own functions, libraries, etc...
- Requires more effort
- But this results in arguably better skill development

Accessing SAS
========================================================
- N120
- Many campus computer labs
- Remotely through ACES
- SAS OnDemand

Accessing R
========================================================
- Rstudio : https://www.rstudio.com/
- On many workstation on campus
- Can be downloaded on most computers
- Very lightweight software

Reading Data SAS --- Inline
========================================================

```r
/*
data read inline from a within a SAS document
*/
data survey; 
   INPUT id sex $ age inc r1 r2 r3 ; 
   DATALINES; 
 1  F  35 17  7 2 2 
17  M  50 14  5 5 3 
33  F  45  6  7 2 7 
49  M  24 14  7 5 7 
65  F  52  9  4 7 7 
81  M  44 11  7 7 7 
2   F  34 17  6 5 3 
18  M  40 14  7 5 2 
34  F  47  6  6 5 6 
50  M  35 17  5 7 5 
; 
```
Reading Data SAS --- Inline
========================================================

```r
proc print data= survey;
run;
proc univariate data=survey;
run;
```

Reading Data SAS --- Inline
========================================================

```r
/*
data read exactly from where in the document 
it occurs
*/
data thisone; 
  input name $1-20  age city $29-37 state $ ; 
  datalines; 
Oliver Schabenberger    33  Lansing    MI 
John T. Smith        37     New York   NY 
; 
proc print data=thisone;
run;
```
Reading Data SAS --- Inline
========================================================

```r
options nocenter; 
DATA survey; 
   INPUT id sex $ age inc r1 r2 r3 ; 
   DATALINES; 
 1  F  35 17  7 2 2 
17  M  50 14  5 5 3 
33  F  45  6  7 2 7 
49  M  24 14  7 5 7
;
PROC PRINT; RUN;
```



Reading Data R ---- Inline
========================================================

```r
## FROM THE FIRST HW :)
pr_3_data <- scan(
text='26.6  27.8  26.5  26.7	30.6
28.9	30.4	29.2	22.6	27.0
28.2	24.7	27.6	27.6	27.2
...')
```


Reading Data R ---- Inline
========================================================

```r
## FROM THE FIRST HW :)
pr_3_data <- c(26.6,27.8,26.5,26.7,30.6,
28.9,30.4,29.2,22.6,27.0,
28.2,24.7,27.6,27.6,27.2)
```


Reading Data SAS ---- CSV
=======================================================

```r
proc import datafile="/home/knpayne2/sas_examples/
    Data_Cortex_Nuclear_2.csv"
     out=dcn
     dbms=csv
     replace;
     getnames=yes;
run;

proc univariate data=dcn;
run;
```

Reading Data SAS ---- CSV
===================
Let's try this example!

- But first note,
- $ in SAS means a character variable
- By default, SAS assumes that a variable is numeric
- If SAS encodes a variable as numeric, but encounters a character 
it will code is as ., which means missing. 

Reading Data R ---- CSV 
========================================================

```r
data_cortex <- read.csv("~/Desktop/data_cortex.csv")
## kind of ugly output, but just as useful
```
Or just use the "import dataset" button in Rstudio

Reading Data R ---- CSV 
========================================================
Let's try this example!
