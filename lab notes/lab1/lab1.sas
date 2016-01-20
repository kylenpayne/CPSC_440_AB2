
/*
data read inline from a within a SAS document
note that the sex variable is labeled as a 
character variable
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


proc print data= survey;
run;

/* runs basic univariate statistics */
proc univariate data=survey;
run;


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

/* prints out the data */
proc print data=thisone;
run;

/*
This code imports the csv datafile Data_Cortex_Nuclear_2.csv
and outputs a SAS dataset called dcn.

It also looks for names in the dataset, and will assign the 
variables the names in the dataset with the getnames=yes tag.
*/
proc import datafile="/home/knpayne2/sas_examples/Data_Cortex_Nuclear_2.csv"
     out=dcn
     dbms=csv
     replace;
     getnames=yes;
run;
/*
Perform basic univariate analysis of data.
*/
proc univariate data=dcn;
	var ITSN1_N;
run;

/*
Look at the means.
*/
proc means data=dcn;
	var ITSN1_N;
run;

/*
subsetting the data
 */
data dcn_sub;
	set dcn;
	keep MouseID DYRK1A_N ITSN1_N;
run;

/*
There are some missing data 
so we can kick these out
*/ 

data dcn_sub_no_miss;
	set dcn;
	keep MouseID DYRK1A_N ITSN1_N;
	if MouseID ^= . OR DYRK1A_N ^=. OR ITSN1_N ^=.;
run;


/*
Subset based on value
*/
data dcn_sub_gt_50;
	set dcn_sub_no_miss;
	if MouseID = '309_1';
run;


/*
ACTIVITY CODE:
*/

data dcn_sub_act;
	set dcn;
	keep H3MeK4_N TIAM1_N Ubiquitin_N Genotype MouseID;
	if cmiss(of _all_) then delete;
run;

proc means data=dcn_sub_act;
run;

/*
Basic Statistical Graphics
*/
proc univariate data=dcn_sub_act;
	var H3MeK4_N;
	histogram;
run;
/*
Boxplots
*/
proc univariate data=dcn_sub_act plot;
	var H3MeK4_N;
run;

proc sgplot data=dcn_sub_act;
	vbox H3MeK4_N;
run;


