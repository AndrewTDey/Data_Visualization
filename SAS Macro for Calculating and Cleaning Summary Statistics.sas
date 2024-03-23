/*************************************Program Name: SAS Macro for Calculating and Cleaning Summary Statistics.sas*****************************************************************************************************************
Project Name: Any
Purpose: A macro that can be used to calculate summary statistics for selecting the valid values and calculating summary statistics for numeric data, and cleaning the output
Programmer: Andy Dey
Location: filepath
Notes: The word Lab can be replaced with whatever variable type of interest
*******************************************************************************************************************************************************************/


/********************MACRO For Getting SumStats and Cleaning************************************/
%Macro LabStats (labname);
/*Formatting to make sure units are all the same etc*/
data &labname.fmted;
set &labname;
	units=upcase(UNITS);
	labchemtestname=upcase(labchemtestname);
run;

/*Sorting*/
proc sort data=work.&labname.fmted;
by labchemtestsid sta3n TopographySID units;
run;


/*Summary Stats*/
proc means data=&labname.fmted n min max p1 p5 p10 p25 p50 p75 p90 p95 p99 max;
class labchemtestsid sta3n TopographySID units VISN;
var labchemresultnumericvalue;
id labchemtestname;
output out=work.AD_&labname.SumStats n=n mean=mean min=min_val max=max_val p1=p1 p5=p5 p10=p10 p25=p25 p50=p50 p75=p75 p90=p90 p95=p95 p99=p99;
run;

/*Cleaning the sum stat dataset*/
data &labname.sumstat_clean;
set work.AD_&labname.SumStats;
where labchemtestsid ne . and sta3n ne . and topographysid ne . and units ne '' and VISN ne . and N>0;
drop _type_ _freq_; 
run;

%mend LabStats;


/***Lab1***/
/*Importing the source data from SQL*/
proc sql;
create table Lab1 as select * from sqllibname._Lab1;
quit;

%labstats(Lab1);

proc print data=work.Lab1sumstat_clean; run;

/*outputting to SQL*/
data sqllibname.AD_Lab1_Stats;
set work.Lab1SumStat_clean;
run;
