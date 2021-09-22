/* Importing file */
proc import out = cardano
datafile= "/home/u58769025/Lilian SAS/Project/ADA-2021.csv"
dbms=csv replace; getnames=yes;
run;

proc print data = cardano;
title "Price Predictions of Cryptocurrency Cardano";

*/6.1 Time Series Regression - No Trend Model*/;
data cardano2;
 set cardano;
 one = 1;
run;
symbol value=none interpol=sm width=2;
proc gplot data = cardano;
	plot Low*Date;
	
proc reg data = work.cardano2;
	model Low = one/noint clm cli;
	output out = results predicted = yhat
		residual = resid;
symbol value=none interpol=sm width=2;		
proc gplot data = work.results;
	plot resid*Date;
run;

*/6.2 Time Series Regression - Linear Trend Model*/;
proc gplot data = work.cardano;
 plot Low*Date;
proc reg data = work.cardano;
 model Low = Date/clm cli dw;
 output out = results predicted = yhat
 			  residual = resid;
proc plot data = work.results;
 plot resid*Date;
run;

*/6.3 Time Series Regression - Quadtratic Trend Model*/;
data cardano2;
 set cardano;
 Datesq = Date**2;
proc gplot data = work.cardano2;
 plot Low*Date;
proc reg data = work.cardano2;
 model Low = Date Datesq/clm cli dw;
 output out = results predicted = yhat
 	residual = resid;
proc gplot data = work.results;
 plot resid*Date;
run;

*/6.4 Time Series Regression - Transformations*/;
data cardano2;
 set cardano;
 Date = _n_;
 Lny =Log(Low);
 Sqrty=Low**.5;
 Qtrooty=Low**.25;
run;
proc gplot data =cardano linesize = 2dx;
 plot Low*Date;
 plot Sqrty*Date;
 plot Qtrooty*Date;
 plot Lny*Date;
run;

*/6.4 Time Series Regression - Seasonal Models*/;
data cardano2;
 Lny = Log(Low);
 Date = _n_;
run;
data future;
 input Low Lny Date;
datalines;
. . 14-JUN-20121	
. . 15-JUN-2021
. . 16-JUN-2021
. . 17-JUN-2021
. . 18-JUN-2021
. . 19-JUN-2021
. . 20-JUN-2021
run;
data cardano2;
 update cardano future
 by Date;
run;

*/Durbin-Watson Test*/;
symbol interpol=l;
proc gplot data = work.cardano; 
plot Low*Date; 
proc reg data = work.cardano; 
model Low = Date/clm cli dw; 
output out = results predicted = yhat residual = resid; 
proc gplot data = work.results; 
plot resid*Date; 
run;

*Growth Curve*;

data cardano1; 
title "Growth Curve";
	set work.cardano; 
	Lny = Log(Low); 
run; 
symbol value=none interpol=sm width=2;
Proc gplot data = work.cardano1; 
	plot Low*Date; 
	plot Lny*Date;
run;
Proc reg data = work.cardano1; 
	model Lny = Date/clm cli dw; 
run;

*Chapter 7 Decomposition;
proc import out = cardanoMonthly
datafile= "/home/u58769025/Lilian SAS/Project/ADA-Monthly.csv"
dbms=csv replace; getnames=yes;
run;
proc print data = cardanoMonthly;
title "Montly Price Predictions of Cryptocurrency Cardano";



Data cardanoMonthly_MovingAverage;
title "Decomposition Model for Low Price Cardano";
Set cardanoMonthly;
Array LowLag {12} LoLag0-LoLag11;
LowLag{1} = Low;
do i = 2 to 12;
  LowLag{i} = Lag(LowLag{i-1});/*note Lag is a SAS function*/
end;
MovingAverage = 0;
do i = 1 to 12;
  MovingAverage = MovingAverage + LowLag{i};
end;
MovingAverage = MovingAverage/12;
CenteredMV = (MovingAverage + Lag(MovingAverage))/2;
Drop i; 
proc print data = cardanoMonthly_MovingAverage;
title "Low data = LowData_MovingAverage";
run;

/* STEP 3 */

Data cardanoMonthly_MovingAverage;
Set cardanoMonthly_MovingAverage;
Keep CenteredMV;
If _N_ <=2 then delete;

Data cardanoMonthly_SeasonalIndex;
Set cardanoMonthly_MovingAverage; Set cardanoMonthly;
If CenteredMV = "." then SeasonalIndexInitial = 0;
Else SeasonalIndexInitial = Low/CenteredMV;
proc print data = cardanoMonthly_SeasonalIndex;
run;
title "Low data = LowData_SeasonalIndex";

/* STEP 4 */

Data cardanoMonthly_SeasonalIndex;
set cardanoMonthly_SeasonalIndex end=myEOF;
Array SeasonalIndex{12} SeasIndex1-SeasIndex12;
Retain SeasIndex1-SeasIndex12 0;
Time = _N_;
Do i = 1 to 12;
   If Mod(Time, 12)= i then SeasonalIndex{i} = SeasonalIndex{i} + SeasonalIndexInitial;   
end;
If Mod(Time, 12)= 0 then SeasIndex12 = SeasIndex12 + SeasonalIndexInitial;
/*  Get average on next set of lines */ 
If myEOF then do;
  sum_of_indices =0;
  Do i = 1 to 12;
     SeasonalIndex{i} = SeasonalIndex{i}/ 11; 
     sum_of_indices = sum_of_indices + SeasonalIndex{i}; 
  End;
End;
/**Only keep last line**/
If ~myEOF then delete;
Keep sum_of_indices SeasIndex1-SeasIndex12;
run;

proc print data = cardanoMonthly_SeasonalIndex;
var sum_of_indices SeasIndex1-SeasIndex12;
title "Low Price Seasonal Indexes";
run;

/* STEP 5 */

Data DeseasonalizedData;
If _N_ =1 then Set cardanoMonthly_SeasonalIndex;  Set cardanoMonthly;
Array SeasonalIndex {12} SeasIndex1-SeasIndex12;
Time = _N_; 
Do i = 1 to 12;
   If Mod(Time, 12)= i then SeasonalEffect  = SeasonalIndex{i};  
end;
If Mod(Time, 12)= 0 then SeasonalEffect  = SeasonalIndex{12};  
DeseasonalizedLow = Low/SeasonalEffect;
Keep  Time DeseasonalizedLow Low SeasonalEffect;
proc print data = DeseasonalizedData;
run;
title "Low Price Deseasonalized Data";

/* STEP 6 */

Proc Reg data=DeseasonalizedData;
model DeseasonalizedLow  = Time ;
output out=tempfile p=Trend;
title "Low Price DeseasonalizedLow regressed on Time";
run;
proc print data = tempfile;
run;
title "Low Price Predicted DeseasonalizedLow - Trend ";

/* STEP 7 */

Data Cyclical;
Set tempfile;
CyclicalInitial = DeseasonalizedLow/Trend; *shows multiplicative model*;

Data Cyclical;
Set Cyclical;
Array CyclicalLag {11} CyclicalLag1-CyclicalLag11;
CyclicalLag{1} = CyclicalInitial;
do i = 2 to 11;
  CyclicalLag{i} = Lag(CyclicalLag{i-1});/*note Lag is a SAS function*/
end;
CycMovingAverage = 0;
do i = 1 to 11;
  CycMovingAverage = CycMovingAverage + CyclicalLag{i};
end;
CycMovingAverage = CycMovingAverage/3;
Keep CycMovingAverage;
If _N_ = 1 then delete;
Drop i; 

proc print data = Cyclical;
title "Low data = Cyclical";
run;

/* STEP 8 */

Data Decomposition;
Set tempfile; Set Cyclical;
Irreg = Low/(SeasonalEffect*Trend*CycMovingAverage);

proc print data = Decomposition;
Title "Low Price Decomposition";
 Run;


/* STEP 9 */

Proc Reg data=DeseasonalizedData;
	model DeseasonalizedLow  = Time / cli clm;
	title 'New Low Price Predicted Values';
run;

proc forecast data = Decomposition lead=12 out=prediction;
var Low;
title 'Low Price Forecast';
run;

proc print data=prediction;
run;

*Generate forecast unobserved components model*;
proc ucm data=cardanoMonthly;
 	where year(Date) >2017;
 	id Date interval=month;
 	model Low;
 	
 	level var=0 noest;
 	slope var=0 noest;
 	season length=12 var=0 noest;
 	irregular sp=3;
 	
 	estimate back=10;
 	forecast back=10 lead=10 plot=decomp print=decomp;
 run;


/* Chapter 8 Using Proc Reg - Exponential Smoothing Methods */;
title 'Winters Multiplicative Method ';
proc esm data = cardano outfor = out2 back = 10 lead = 25
plot = (modelforecasts forecastonly)
print = (estimates forecasts statistics performance);
id Date interval = day;
forecast Low / model=winters;
run;

title 'Linear Holts Exponential Smoothing Method';
proc esm data = cardano outfor = out2 back = 10 lead = 25
plot = (modelforecasts forecastsonly)
print = (estimates forecasts statistics performance);
id Date interval = day;
forecast Low / model = linear;
run;

title 'Simple Exponential Smoothing Method';
proc esm data = cardano outfor = out2 back = 10 lead = 25
plot = (modelforecasts forecastsonly)
print = (estimates forecasts statistics performance);
id Date interval = day;
forecast Low / model = simple;
run;

*/Chapter 9 *programming commands for procedures to plot data and find SAC and SPAC*/;
Time = _n_;*creates variable for time t;
z=dif1(Low);*creates a variable for 1st differences;

symbol interpol=1;
proc print data=work.cardano;
run;

proc gplot data=work.cardano;
plot Low*Date;
plot z*Date;
run;

proc gplot data=work.cardano;
Time =_n_;
z=dif1(Low); /*z=Low_t-Low_{t-1}*/
plot Low*Date;
plot z*Date; 
run;

proc arima data=work.cardano; /*PROC ARIMA*/;
identify var=Low; *Generate SAC and SPAC for y_t *;
run;

identify var=Low(1,1); /*Generate SAC and SPAC for z_t=y_t-2y_{t-1}+y_{t-2}*/
run;
/*How to fit a model*/
*MA Model*/;
proc arima data=cardano;
 identify var = Low(1);*/Specifies first differencing;
run;


proc arima data=cardano;
 identify var = Low(1);
 estimate q=(1) noconstant printall plot;*/Requests estimation & diagnostics for model zt=at-deltaat-1*;
 estimate q=(1) printall plot;
 forecast lead=12;
run;
proc arima data=cardano;
identify var = Low(1);
 estimate q=(3) noconstant printall plot;*/Requests estimation & diagnostics for model zt=at-deltaat-1*;
 estimate q=(3) printall plot;
 forecast lead=12;
run;

*AR Model*/;
proc gplot data=work.cardano;
 identify var=Low;
 run;
proc arima data=cardano;
 identify var=Low;
 estimate p=(2) noconstant printall plot;
 estimate p=(2) printall plot;
run;

proc arima data=cardano;
 identify var=Low;
 estimate p=(1,2,3);
 estimate p=(1,2,3) noconstant printall plot;
 estimate q=(1,3) printall plot;
 forecast lead=12;
run;
*ARMA Model*;
proc arima data=cardano;
 identify var=Low;
 estimate p=(1,2) printall plot;
 estimate p=(1,2) noconstant printall plot;*lag 30?;
run;

proc arima data=cardano;
 identify var=Low;
 estimate q=(1,3) noconstant printall plot;
 estimate q=(1,3) printall plot;
run;

proc arima data=cardano;
 identify var=Low;
 estimate q=(2,3)noconstant printall plot;
 estimate q=(1,3) p=(1,4)printall plot;*Lag 30?;
run; 
*MA, AR & ARMA Model*;
proc arima data=cardano;
 identify var=Low;
 estimate q=(0,1) p=(2,1)noconstant printall plot;*at lag 30;
run;

 estimate p=(1,2) printall plot;
 estimate q=(1,0) printall plot;
run;
*SCAN function SAS gives you the best ARIMA model*;
proc arima data=cardano;
 identify var=Low scan;
run;

*ADF Test*;
proc autoreg data = cardano;
 model Low = /stationarity=(adf=1);
run;

*Chapter 10 - Invertibility, t-values & p-values*;
*MA(1)*;
proc arima data=cardano;
 identify var = Low nlag=10;
 estimate q=(1) noconstant printall plot;*Spike at lag1;
 forecast lead = 10;
run;
*MA(1)*;
proc arima data=cardano;
 identify var = Low(1) nlag=10;*1st differencing;
 estimate q=(3) noconstant printall plot;*Spike at lag1;
 forecast lead = 10;
run;
*AR(2)*;
proc arima data=cardano;
 identify var = Low nlag=10;*original time series;
 estimate p=(3,5) noconstant printall plot;*lag 1&2 has spike;
 forecast lead = 10;
run;
*AR(1)*;
proc arima data=cardano;
 identify var = Low nlag=10;*original time series;
 estimate p=(1,1) noconstant printall plot;*lag 1&1 has spike, Ljung >.05 at lag 30;
 forecast lead = 10;
run;
*AR(5)*;
proc arima data=cardano;
 identify var = Low nlag=10;*original time series;
 estimate p=(1,2,3,4,5) noconstant printall plot;*lag 1&1 has spike, Ljung >.05 at lag 30;
 forecast lead = 10;
run;
proc arima data=cardano;
 identify var = Low nlag=10;*original time series;
 estimate p=(5,6) noconstant printall plot;
 estimate p=(5,6) printall plot;
 forecast lead = 10;
run;

*Chapter 11 - *Model#2: ARMA(1) Seasonal differencing;;
data ada;
set cardano;
y=low;
time=_n_;
z=dif1(y);
Lny=log(y);
Sqrty=y**.5;
QtRooty=y**.25;
run;

proc print data=work.ada;
run;

symbol interpol=l;
proc gplot data=ada;
symbol interpol=l;
proc gplot data=work.ada;
plot y*time;
plot z*time; 
run;

symbol value=none interpol=sm width=2;
proc gplot data=work.ada;
plot y*time;
plot Lny*time;
plot Sqrty*time;
plot QtRooty*time;
run;
symbol value=none interpol=sm width=2;
proc gplot data=work.ada;
plot y*time;
plot Lny*time;
plot Sqrty*time;
plot QtRooty*time;
run;
proc arima data=work.ada;
identify var=QtRooty;
run;
proc arima data=ada;
identify var=QtRooty;
identify var=QtRooty(1);
identify var=QtRooty(12);
identify var=QtRooty(1,12,24);
forecast lead = 10;
run;


*Best Performing Models:
*Model#1: Zt = delta + at with logarithmic transformation;
data ada;
set cardano;
y=low;
time=_n_;
z=dif1(y);
Lny=log(y);
Sqrty=y**.5;
QtRooty=y**.25;
run;
symbol interpol=l;
proc gplot data=ada;
symbol interpol=l;
proc gplot data=work.ada;
plot y*time;
plot z*time; 
run;

symbol value=none interpol=sm width=2;
proc gplot data=work.ada;
plot y*time;
plot Lny*time;
plot Sqrty*time;
plot QtRooty*time;
run;
symbol value=none interpol=sm width=2;
proc gplot data=work.ada;
plot y*time;
plot Lny*time;
plot Sqrty*time;
plot QtRooty*time;
run;
proc arima data=work.ada;
Lny=Log(Low);
identify var=Lny(1);
estimate printall plot;
forecast lead=10;
run;

*Model#2: ARMA(1) Seasonal differencing;
symbol interpol=l;
proc gplot data=ada;
symbol interpol=l;
proc gplot data=work.ada;
plot y*time;
plot z*time; 
run;

symbol value=none interpol=sm width=2;
proc gplot data=work.ada;
plot y*time;
plot Lny*time;
plot Sqrty*time;
plot QtRooty*time;
run;
symbol value=none interpol=sm width=2;
proc gplot data=work.ada;
plot y*time;
plot Lny*time;
plot Sqrty*time;
plot QtRooty*time;
run;

proc arima data=ada;
identify var=QtRooty;
identify var=QtRooty(1);
estimate printall plot;
forecast lead = 10;
run;

*Model#3: Growth Curve;
data cardano1; 
title "Growth Curve";
	set work.cardano; 
	Lny = Log(Low); 
run; 
symbol value=none interpol=sm width=2;
Proc gplot data = work.cardano1; 
	plot Low*Date; 
	plot Lny*Date;
run;
Proc reg data = work.cardano1; 
	model Lny = Date/cli clm dw clb sbc aic;
run;
