newdf <- read.csv("/Users/ams939/Box Sync/DEPENd/Projects/PD_Interaction_Dynamics/output/fulldata_VAR_mod09112018.csv")
newdf <- dplyr::rename(newdf, ECRavo_1 = ECR_avoidance_1, ECRavo_0 = ECR_avoidance_0, ECRanx_0 = ECR_anxiety_0, ECRanx_1 = ECR_anxiety_1)
newdf <- dplyr::select(newdf, PTNUM, scpt, ccpt, scpr, ccpr, prnapt, prnapr, pnapt, pnapr, prafpt, prafpr, pafpt, pafpr, ECRanx_1, ECRanx_0, ECRavo_1, ECRavo_0, cmpt, cmpr, elpt, elpr)
h1_newdf <- "
scpt ~ elpt
scpr ~ elpr
ccpt ~ u*elpt
ccpr ~ elpr
prnapt ~ w*elpt
prnapr ~ j*elpr

ECRanx_1 ~h*pnapt
ECRanx_0 ~ k*pnapr
pnapt~~ pnapr
pnapt ~ prnapt +scpt + c*ccpt
pnapr ~ prnapr +scpr + d*ccpr
scpt ~ a1*prnapt
scpr~ a1*prnapr
ccpt ~b1*prnapt 
ccpr ~b1*prnapr
scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr
b1c := b1*c
b1d := b1*d
hb1c:=h*b1*c
whb1c := w*h*b1*c
uhc:=u*h*c
"


h1_newdf_out <- runBSEM(modsyntax(dat = newdf,model = h1_newdf))

h2_newdf <- "
scpt ~ cmpt
scpr ~ cmpr
ccpt ~ w*cmpt
ccpr ~ cmpr
prnapt ~ cmpt
prnapr ~ j*cmpr

ECRanx_1 ~h*pnapt
ECRanx_0 ~ k*pnapr
pnapt~~ pnapr
pnapt ~ prnapt +scpt + c*ccpt
pnapr ~ prnapr +scpr + d*ccpr
scpt ~ a1*prnapt
scpr~ a1*prnapr
ccpt ~b1*prnapt 
ccpr ~b1*prnapr
scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr
b1c := b1*c
b1d := b1*d
hb1c:=h*b1*c
whb1c := w*h*b1*c
whc := w*h*c
"
h2_newdf_out <- runBSEM(modsyntax(newdf, h2_newdf))


h3_newdf <- "
scpt ~ cmpt
ccpt ~ z*cmpt
scpr ~ cmpr
ccpr ~ cmpr
prafpt ~ cmpt
prafpr ~ cmpr


ECRanx_0 ~ h*pafpt
ECRanx_1 ~ h*pafpr
ECRanx_1 ~ k*pafpt
ECRanx_0 ~ k*pafpr
pafpt~~ pafpr

pafpt ~ prafpt + a1*scpt + a3*ccpt + a5*scpr + a7*ccpr
pafpr ~ prafpr + a2*scpr + a4*ccpr + a5*scpt + a7*ccpt
scpr ~ b5*prafpt
scpt~ b5*prafpr
ccpt ~ b7*prafpr
ccpr ~ b7*prafpt 
scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr
ha3:=h*a3

ka3:=k*a3
a3b7:=a3*b7
a7b7:=a7*b7
a4b7:=a4*b7
zka3 := z*k*a3
"
h3_newdf_out <- runBSEM(modsyntax(newdf, h3_newdf))


h4_newdf <- "
ECRavo_1 ~ h*pafpt
ECRavo_0 ~ k*pafpr
pafpt~~ pafpr
scpt ~ cmpt
ccpt ~ z*cmpt
scpr ~ cmpr
ccpr ~ cmpr
prafpt ~ cmpt
prafpr ~ cmpr
prafpt ~~ prafpr
pafpt ~ prafpt + a1*scpt + a3*ccpt + a5*scpr + a7*ccpr
pafpr ~ prafpr + a2*scpr + a4*ccpr + a5*scpt + a7*ccpt
scpr ~ b5*prafpt
scpt~ b5*prafpr
ccpt ~ b7*prafpr
ccpr ~ b7*prafpt 
scpt ~~ ccpt
scpt ~~ scpr
scpt ~~ ccpr
ccpt ~~ scpr
ccpt ~~ ccpr
scpr ~~ ccpr
ECRavo_0~cmpr
ha3:=h*a3
a3b7:=a3*b7
a7b7:=a7*b7
a4b7:=a4*b7
zha3 := z*h*a3
"
h4_newdf_out <- runBSEM(modsyntax(newdf, h4_newdf))


