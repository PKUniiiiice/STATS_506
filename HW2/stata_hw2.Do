//load data
cd "K:\STATS_506\STATA\"
import delimited "cars.csv", clear
describe

// Part a ------------------------------------------------------------------------------------------------------------------------
local oldvars "dimensionsheight dimensionslength dimensionswidth engineinformationdriveline engineinformationenginetype engineinformationhybrid engineinformationnumberofforward engineinformationtransmission fuelinformationcitympg fuelinformationfueltype fuelinformationhighwaympg identificationclassification identificationid identificationmake identificationmodelyear identificationyear engineinformationenginestatistic v18"

local newvars "Height Length Width Driveline EngineType Hybrid NofForwardGears Transmission Citympg FuelType Highwaympg Classification ID Make ModelYear Year Horsepower Torque"

//loop over the table, and change column names
local items : word count `oldvars'
forval i = 1/`items' {
    local old : word `i' of `oldvars'
	local new : word `i' of `newvars'
    rename `old' `new'
}
// End of Part a------------------------------------------------------------------------------------------------------------------


// Part b ------------------------------------------------------------------------------------------------------------------------
keep if FuelType == "Gasoline"
// End of Part b------------------------------------------------------------------------------------------------------------------


// Part c ------------------------------------------------------------------------------------------------------------------------
regress Highwaympg Horsepower Torque Height Length Width i.Year
// End of Part c------------------------------------------------------------------------------------------------------------------

// Part d ------------------------------------------------------------------------------------------------------------------------
regress Highwaympg c.Horsepower##c.Torque Height Length Width i.Year

_pctile Torque, p(25,50,75)
local q1 = r(r1)
local q2 = r(r2)
local q3 = r(r3) 

margins, at(Horsepower=(100(100)700) Torque=(`q1' `q2' `q3`'') Year=(2009(1)2012))
marginsplot
// End of Part d------------------------------------------------------------------------------------------------------------------

// Part e ------------------------------------------------------------------------------------------------------------------------
tabulate Year, generate(Year_dummies)
gen Inter = Horsepower * Torque
gen ones = 1

//y
mkmat Highwaympg, matrix(y)

//part c
mkmat ones Horsepower Torque Height Length Width Year_dummies2 Year_dummies3 Year_dummies4, matrix(designofc)

//part d
mkmat ones Horsepower Torque Height Length Width Year_dummies2 Year_dummies3 Year_dummies4 Inter, matrix(designofd)

mata:

real matrix cal_beta(real matrix X, real matrix y)
{
    real matrix XTX_inv, b
    XTX_inv = invsym(X' * X)
    b = XTX_inv * X' * y
    return(b)
}

designofc = st_matrix("designofc")
designofd = st_matrix("designofd")
y = st_matrix("y")

// part c
betac = cal_beta(designofc, y)
st_matrix("betac", betac)

//part d
betad = cal_beta(designofd, y)
st_matrix("betad", betad)

mata clear
end

matrix list betac
matrix list betad
// End of Part e------------------------------------------------------------------------------------------------------------------