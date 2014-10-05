require(psych)
require(stargazer)



# Copy to clipboard:

# Copy and pasted from Stata output, 
# pvar2 Democracy Trade FDI FPI PressScore, lag(8) gmm monte 300 decomp 8 8

s,Democracy,Trade,FDI,FPI,PressScore,
Democracy,8,.95235921,.01251201,.01385165,.00124343,.02003369,
Trade,8,.33559893,.61159264,.01521511,.00762207,.02997125,
FDI,8,.19567627,.0397477,.74293439,.0138908,.00775085,
FPI,8,.26865217,.05924319,.14617952,.52265124,.00327387,
PressScore,8,.24223829,.02537721,.0121614,.00907452,.71114858,


vardecomp<-read.clipboard(header=TRUE, sep=',')


vardecomp<-vardecomp[1:6]
vardecomp$s<-row.names(vardecomp)
names(vardecomp)<-c("Variable", paste(names(vardecomp)[2:6]))

vard<-stargazer(vardecomp, summary=FALSE, title=c("Variance Decomposition for Panel VAR"), style="apsr", notes=c("Proportion of row variable explained by 8 lags of column variable"))

write(vard, file="vardecomp.tex")


