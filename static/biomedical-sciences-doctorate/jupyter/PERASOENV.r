TheData = DATA[,c(
  "PERASOENV_questionario_preguntas_1",
  "PERASOENV_questionario_preguntas_2",
  "PERASOENV_questionario_preguntas_3",
  "PERASOENV_questionario_preguntas_4",
  "PERASOENV_questionario_preguntas_5",
  "PERASOENV_questionario_preguntas_6",
  "PERASOENV_questionario_preguntas_7",
  "PERASOENV_questionario_preguntas_8",
  "PERASOENV_questionario_preguntas_9",
  "PERASOENV_questionario_preguntas_10",
  "PERASOENV_questionario_preguntas_11",
  "PERASOENV_questionario_preguntas_12",
  "PERASOENV_questionario_preguntas_13",
  "PERASOENV_questionario_preguntas_14",
  "PERASOENV_questionario_preguntas_15",
  "PERASOENV_questionario_preguntas_16",
  "PERASOENV_questionario_preguntas_17",
  "PERASOENV_questionario_preguntas_18",
  "PERASOENV_questionario_preguntas_19",
  "PERASOENV_questionario_preguntas_20",
  "PERASOENV_questionario_preguntas_21",
  "PERASOENV_questionario_preguntas_22"
)];
TheData = TheData[complete.cases(TheData),];
colnames(TheData)=c(
  "p1",
  "p2",
  "p3",
  "p4",
  "p5",
  "p6",
  "p7",
  "p8",
  "p9",
  "p10",
  "p11",
  "p12",
  "p13",
  "p14",
  "p15",
  "p16",
  "p17",
  "p18",
  "p19",
  "p20",
  "p21",
  "p22"
);
View(TheData);

MOD01= '
F =~ p1 + p7 + p13 + p20 + p22
R =~ p2 + p8 + p14 + p19
M =~ p3 + p9 + p15
S =~ p4 + p10 + p16
V =~ p5 + p11 + p17
C =~ p6 + p12 + p18 + p21
';

CFA01 <- cfa(MOD01, data=TheData, ordered = colnames(TheData));
Alpha=psych::alpha(TheData);
Alpha$total$raw_alpha;
Alpha$total$std.alpha;
summary(CFA01, fit.measures=TRUE);
View(subset(standardizedSolution(CFA01), op=="=~"));
View(subset(modindices(CFA01), op=="=~"));

TheData02 = TheData[,c(
  "p1",
  "p2",
  "p3",
  "p4",
  "p5",
  "p6",
  "p7",
  "p9",
  "p10",
  "p11",
  "p12",
  "p13",
  "p14",
  "p15",
  "p16",
  "p17",
  "p18",
  "p19",
  "p20",
  "p21",
  "p22"
)];
View(TheData02);
MOD02= '
F =~ p1 + p7 + p13 + p20 + p22
R =~ p2 + p14 + p19
M =~ p3 + p9 + p15
S =~ p4 + p10 + p16
V =~ p5 + p11 + p17
C =~ p6 + p12 + p18 + p21
';

CFA02 <- cfa(MOD02, data=TheData02, ordered = colnames(TheData02));
Alpha=psych::alpha(TheData02);
Alpha$total$raw_alpha;
Alpha$total$std.alpha;
summary(CFA02, fit.measures=TRUE);
View(subset(standardizedSolution(CFA02), op=="=~"));
View(subset(modindices(CFA02), op=="=~"));

MOD03= '
F =~ p1 + p7 + p20 + p22
R =~ p2 + p13 + p14 + p19
M =~ p3 + p9 + p15
S =~ p4 + p10 + p16
V =~ p5 + p11 + p17
C =~ p6 + p12 + p18 + p21
';

CFA03 <- cfa(MOD03, data=TheData02, ordered = colnames(TheData));
Alpha=psych::alpha(TheData02);
Alpha$total$raw_alpha;
Alpha$total$std.alpha;
summary(CFA03, fit.measures=TRUE);
View(subset(standardizedSolution(CFA03), op=="=~"));
View(subset(modindices(CFA03), op=="=~"));


TheData05 = TheData[,c(
  "p1",
  "p2",
  "p3",
  "p4",
  "p5",
  "p6",
  "p7",
  "p9",
  "p10",
  "p11",
  "p12",
  # "p13",
  "p14",
  "p15",
  "p16",
  "p17",
  "p18",
  "p19",
  "p20",
  "p21",
  "p22"
)];
MOD05= '
F =~ p1 + p7 + p20 + p22
R =~ p2 + p14 + p19
M =~ p3 + p9 + p15
S =~ p4 + p10 + p16
V =~ p5 + p11 + p17
C =~ p6 + p12 + p18 + p21
';

CFA05 <- cfa(MOD05, data=TheData05, ordered = colnames(TheData));
Alpha=psych::alpha(TheData05);
Alpha$total$raw_alpha;
Alpha$total$std.alpha;
summary(CFA05, fit.measures=TRUE);
View(subset(standardizedSolution(CFA05), op=="=~"));
View(subset(modindices(CFA05), op=="=~"));

TheData06 = TheData[,c(
  "p1",
  "p2",
  "p3",
  "p4",
  "p5",
  "p6",
  "p7",
  "p9",
  "p10",
  "p11",
  "p12",
  # "p13",
  "p14",
  "p15",
  "p16",
  "p17",
  "p18",
  "p19",
  "p20",
  "p21"
  # "p22"
)];
MOD06= '
F =~ p1 + p7 + p20
R =~ p2 + p14 + p19
M =~ p3 + p9 + p15
S =~ p4 + p10 + p16
V =~ p5 + p11 + p17
C =~ p6 + p12 + p18 + p21
';

CFA06 <- cfa(MOD06, data=TheData06, ordered = colnames(TheData));
Alpha=psych::alpha(TheData06);
Alpha$total$raw_alpha;
Alpha$total$std.alpha;
summary(CFA06, fit.measures=TRUE);
View(subset(standardizedSolution(CFA06), op=="=~"));
View(subset(modindices(CFA06), op=="=~"));


TheData07 = TheData[,c(
  "p1",
  "p2",
  "p3",
  "p4",
  "p5",
  "p6",
  "p7",
  "p9",
  "p10",
  "p11",
  "p12",
  # "p13",
  "p14",
  "p15",
  "p16",
  "p17",
  # "p18",
  "p19",
  "p20",
  "p21"
  # "p22"
)];
MOD07= '
F =~ p1 + p7 + p20
R =~ p2 + p14 + p19
M =~ p3 + p9 + p15
S =~ p4 + p10 + p16
V =~ p5 + p11 + p17
C =~ p6 + p12 + p21
';

CFA07 <- cfa(MOD07, data=TheData07, ordered = colnames(TheData));
Alpha=psych::alpha(TheData07);
Alpha$total$raw_alpha;
Alpha$total$std.alpha;
summary(CFA07, fit.measures=TRUE);
View(subset(standardizedSolution(CFA07), op=="=~"));
View(subset(modindices(CFA07), op=="=~"));
## Fin CFA

# 
# FAP=psych::fa.parallel(x = TheData, cor="poly")
# # PC=polycor::hetcor(TheData,ML=TRUE)
# # FA=psych::fa(r=PC$correlations,nfactors = 6,n.obs = 183, rotate = "varimax")
# # FAD=psych::fa.poly(TheData,nfactors = 10,rotate = "varimax")
# FA=psych::fa(r=TheData,nfactors = 10,n.obs = 183, rotate = "varimax")
# psych::fa.diagram(FA)