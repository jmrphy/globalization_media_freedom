cd "/Users/justin/Dropbox/gh_projects/globalization_media_freedom/analyses/pvar"

insheet using "/Users/justin/Dropbox/gh_projects/globalization_media_freedom/data/out_df.csv", comma

encode openk, generate(Trade)
encode fhscore, generate(PressScore)
encode polity2, generate(Democracy)
encode fdiinward, generate(FDI)
encode fpistock, generate(FPI)
encode rgdpch, generate(GDPCap)
encode scode, generate(id)
xtset id yearhelm Trade FDI FPI GDPCap Democracy PressScore

# In paper
pvar2 Democracy Trade FDI FPI PressScore, lag(8) gmm monte 300

# Works also
pvar2 Democracy Trade FDI FPI PressScore, lag(9) gmm monte 300

# Very nearly the same but trade is not quite significantly negative in long run
pvar2 Democracy Trade FDI FPI PressScore, lag(7) gmm monte 300

# Works also
pvar2 PressScore Democracy FDI FPI Trade, lag(8) gmm monte 300



# All models below are consistent with the predicted effect of trade on press freedom

# pvar2 PressScore Democracy GDPCap FDI FPI Trade, lag(8) gmm monte 300# pvar2 PressScore Democracy GDPCap FDI FPI Trade, lag(7) gmm monte 300
# pvar2 PressScore Democracy GDPCap FDI FPI Trade, lag(9) gmm monte 300





