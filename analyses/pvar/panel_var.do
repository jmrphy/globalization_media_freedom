cd "/Users/justin/Dropbox/gh_projects/globalization_media_freedom/analyses/pvar"

insheet using "/Users/justin/Dropbox/gh_projects/globalization_media_freedom/data/out_df.csv", comma

encode openk, generate(Trade)
encode fhscore, generate(PressScore)
encode polity2, generate(Democracy)
encode fdiinward, generate(FDI)
encode fpistock, generate(FPI)
encode rgdpch, generate(GDPCap)

generate id = scode
xtset id year
tsset id year


% In paper

helm Democracy Trade FDI FPI PressScore GDPCap

pvar2 Trade Democracy GDPCap PressScore, lag(5) gmm monte 300

 decomp 8 8


% Works also
pvar2 Democracy Trade FDI FPI PressScore, lag(9) gmm monte 300

% Very nearly the same but trade is not quite significantly negative in long run
pvar2 Democracy Trade FDI FPI PressScore, lag(7) gmm monte 300

% Works also
pvar2 PressScore Democracy FDI FPI Trade, lag(8) gmm monte 300



% All models below are consistent with the predicted effect of trade on press freedom

% pvar2 PressScore Democracy GDPCap FDI FPI Trade, lag(8) gmm monte 300% pvar2 PressScore Democracy GDPCap FDI FPI Trade, lag(7) gmm monte 300
% pvar2 PressScore Democracy GDPCap FDI FPI Trade, lag(9) gmm monte 300



%%%%%%%%%%%%%%% Additional

% All show unit roots
xtunitroot fisher Trade, dfuller trend demean lags(8)
xtunitroot fisher Democracy, dfuller trend demean lags(8)
xtunitroot fisher FDI, dfuller trend demean lags(8)
xtunitroot fisher FPI, dfuller trend demean lags(8)

gen dTrade = d.Trade
gen dDemocracy = d.Democracy
gen dFDI = d.FDI
gen dFPI = d.FPI
gen dPressScore = d.PressScore



% No unit root
xtunitroot fisher PressScore, dfuller lags(8)
xtunitroot fisher dPressScore, dfuller lags(8)
xtunitroot fisher dTrade, dfuller lags(8)
xtunitroot fisher dDemocracy, dfuller lags(8)
xtunitroot fisher dFDI, dfuller lags(8)
xtunitroot fisher dFPI, dfuller lags(8)

helm Trade FDI FPI Democracy PressScore

% So pvar2 will draw the first-difference variables instead of doing the Helmert transform

gen h_dTrade = d.Trade
gen h_dDemocracy = d.Democracy
gen h_dFDI = d.FDI
gen h_dFPI = d.FPI
gen h_dPressScore = d.PressScore

gen l1_dTrade = l1.d.Trade
gen l1_dDemocracy = l1.d.Democracy
gen l1_dFDI = l1.d.FDI
gen l1_dFPI = l1.d.FPI
gen l1_PressScore = l1.PressScore


gen l2_dTrade = l2.d.Trade
gen l2_dDemocracy = l2.d.Democracy
gen l2_dFDI = l2.d.FDI
gen l2_dFPI = l2.d.FPI
gen l2_PressScore = l2.PressScore


gen l3_dTrade = l3.d.Trade
gen l3_dDemocracy = l3.d.Democracy
gen l3_dFDI = l3.d.FDI
gen l3_dFPI = l3.d.FPI
gen l3_PressScore = l3.PressScore

gen l4_PressScore = l4.PressScore
gen l5_PressScore = l5.PressScore
gen l6_PressScore = l6.PressScore
gen l7_PressScore = l7.PressScore

xtserial PressScore l1_PressScore l2_PressScore l3_PressScore l4_PressScore l5_PressScore l6_PressScore l7_PressScore

xtserial PressScore Democracy Trade FDI FPI l1_PressScore, output
xtserial dPressScore l1_dDemocracy l1_dTrade l1_dFDI l1_dFPI l1_PressScore l2_dDemocracy l2_dTrade l2_dFDI l2_dFPI l2_PressScore
xtserial dPressScore l1_dDemocracy l1_dTrade l1_dFDI l1_dFPI l1_PressScore l2_dDemocracy l2_dTrade l2_dFDI l2_dFPI l2_PressScore l3_dDemocracy l3_dTrade l3_dFDI l3_dFPI l3_PressScore

xtserial PressScore Democracy Trade FDI FPI

pvar2 dDemocracy dTrade PressScore, lag(6) gmm monte 300 decomp 10 5

pvar2 dPressScore dDemocracy dTrade dFDI dFPI, lag(5) gmm monte 300 decomp 10 5

bysort id: drop if _N==11
bysort id: drop if _N<=24
xtwest PressScore Trade FDI FPI Democracy, lags(3)





