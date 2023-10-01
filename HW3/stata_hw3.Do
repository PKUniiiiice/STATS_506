//load data
cd "K:\STATS_506\STATA\"

//Part a --------------------------------------------------------------------------------------------

//import first
//import sasxport5 VIX_D.XPT, clear
//save as dta
//save "K:\STATS_506\STATA\VIX_D.dta"
//import second
import sasxport5 DEMO_D.XPT, clear
// Merge the second dataset using the SEQN variable
merge 1:1 seqn using "K:\STATS_506\STATA\VIX_D.dta"

// Keep only the matched records
keep if _merge == 3

//total sample size
di _N

//End of Part a --------------------------------------------------------------------------------------------

//Part b ---------------------------------------------------------------------------------------------------
//We use the variable 'VIX220 - Glasses/contact lenses worn for distance' (viq220)
//and  'RIDAGEYR - Age at Screening Adjudicated - Recode' (ridageyr)

egen age_interval = cut(ridageyr), at(0(10)90) label
table age_interval viq220, missing statistic(percent, across(viq220)) statistic(frequency)
quietly collect layout (age_interval) (viq220#result[percent] viq220#result[frequency])
//If only want to see first column
//https://grodri.github.io/stata/tables https://www.stata.com/manuals/tables.pdf

//percent
collect layout (age_interval) (viq220[1]#result[percent] viq220[.m]#result[frequency])

//End of Part b --------------------------------------------------------------------------------------------


//Part c ---------------------------------------------------------------------------------------------------
//For age, we use ridageyr
//For race, we use ridreth1
//For gender, we use riagendr
//For Poverty Income ratio, we use indfmpir
//We first check how many missing values are in these variables
misstable summarize ridageyr ridreth1 riagendr indfmpir viq220

//It seems that the proportion of missing value is not large, about 10%, so we choose to directly delete them
drop if missing(indfmpir) | missing(viq220)
misstable summarize ridageyr ridreth1 riagendr indfmpir viq220

///We treat viq220==1 as "Yes, wear", and all other values as "No, don't wear"
//recode viq220
recode viq220 (1=1) (else=0), generate(viq220_bin)

//ref https://www.stata.com/manuals/rlogistic.pdf
//Note that race and gender shoule be categorical variables and age and PIR are continuous variables


// Fit the first logistic regression model (age only)
logistic viq220_bin ridageyr

// Store the results
eststo model1 

// Fit the second logistic regression model (age, race, gender)
logistic viq220_bin ridageyr i.ridreth1 i.riagendr

eststo model2

// Fit the third logistic regression model (age, race, gender, Poverty Income ratio)
logistic viq220_bin ridageyr i.ridreth1 i.riagendr indfmpir

eststo model3

// Create a table to display results using esttab
// https://repec.org/bocode/e/estout/hlp_esttab.html
esttab model1 model2 model3, ///
	con ///
	not ///
    stats(N r2_p aic) ///
    eform ///
    varwidth(15) ///
    title("Logistic Regression Results") ///
    label

//End of Part c --------------------------------------------------------------------------------------------

//Part d ---------------------------------------------------------------------------------------------------

//Note that in the output table, the odds ratio of Gender=2 is significant, therefore, 
//the odds of men and women being wears of glasses/contact lenses for distance vision differs.

//We use chi-square test (Pearson's Chi-Squared Test of Independence)
tabulate riagendr viq220_bin, chi2

//From the result, p-value is 0.000, therefore we conclude that gender and wearing or not
//are not independent, in other words, the proportion of wearers of glasses/contact lenses for distance vision differs between men and women

//End of Part d --------------------------------------------------------------------------------------------





