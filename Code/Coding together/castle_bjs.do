** Imputation estimator by Borusyak, Jaravel and Spiess (2021)
* Data comes from Cheng and Hoekstra (2015-ish) estimating the effect
* of a gun reform on log homicide (l_homicide). Effective treatment dates
* are in the variable effyear.  Data runs from 2000 to 2010. Treatment dates
* are 2005, 2006, 2007, 2008, and 2009.  There should be therefore 9 leads
* and 5 lags. 

use https://github.com/scunning1975/mixtape/raw/master/castle.dta, clear
set scheme cleanplots

tsset sid year

did_imputation l_homicide sid year effyear, autosample horizons(0/5) pretrend(1/5) minn(0)	
