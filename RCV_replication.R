##################################################
# Replication packet for McCarthy and Santucci on public opinion toward RCV
# Tests the hypothesis that age is the most important factor
# Loads the data; recodes the data; runs a regression for each set; generates tables of coefficients; and generates predicted support in each model for ages 25, 35, 45, 55, 65, and 75. THIS ISN'T TRUE. THE AGE CATEGORIES DIFFER ACROSS THE LOCAL-LEVEL SURVEYS. SO DO THE EDUCATION LEVELS.
# All are linear probability models with appropriate weights and RCV support rescaled to run [0,1]
# Control variables are race, gender, education, party ID, and ideology (trust in government?)
# This version: 2020-9-04
##################################################

##################################################
# LOAD PACKAGES, CREATE OUTPUT DIRECTORY
##################################################

library(readstata13)
library(stringr)
library(foreign)
library(scales) # for rescaling RCV variable [0, 1]
library(texreg) # for regression tables
library(ggplot2)
library(gridExtra)

dir.create("output")

##################################################
# DATA: MAINE
##################################################

mainedata <- read.dta13("MainePollData.dta")
# age is categories, PID is registration, race does not exist

### rcv support

mainedata$rcv <- as.character(mainedata$rankedchoicevoting)
mainedata$rcv[as.character(mainedata$rankedchoicevoting)=="Excellent"] <- 4
mainedata$rcv[as.character(mainedata$rankedchoicevoting)=="Good"] <- 3
mainedata$rcv[as.character(mainedata$rankedchoicevoting)=="Just Fair"] <- 2
mainedata$rcv[as.character(mainedata$rankedchoicevoting)=="Poor"] <- 1
mainedata$rcv[as.character(mainedata$rankedchoicevoting)=="Not Sure"] <- NA
mainedata$rcv <- as.numeric(mainedata$rcv)
mainedata$rcv <- rescale(mainedata$rcv, from=c(1, 4), to=c(0,1))

### agecat -- four bins: 18-34, 35-49, 50-64, 65+

mainedata$agecat <- as.numeric(mainedata$age)

### race -- not in data

### gender -- to female

mainedata$female <- ifelse(mainedata$gender=="Woman", 1, 0)

### education -- HS, some college, 4-year degree
# table(mainedata$education)

mainedata$edu <- as.numeric(mainedata$education)

### pid3
table(mainedata$registeredas)
mainedata$pid3 <- as.character(mainedata$registeredas)
mainedata$pid3[mainedata$pid3=="Democrat"] <- 1
mainedata$pid3[mainedata$pid3=="Republican"] <- 3
mainedata$pid3[mainedata$pid3 %in% c("Libertarian", "Green Party", "Unerolled")] <- 2
mainedata$pid3[mainedata$pid3=="Not Sure"] <- NA
mainedata$pid3 <- as.numeric(mainedata$pid3)

##################################################
# DATA: VSG
##################################################

VSGdata <- read.csv("VOTER_Survey_April18_Release1.csv")
head(VSGdata)

### rcv support

VSGdata$rcv <- VSGdata$electway_2018
VSGdata$rcv[VSGdata$rcv==8] <- NA
VSGdata$rcv <- VSGdata$rcv-1


VSGdata$pr <- VSGdata$electway4_2018
VSGdata$pr[VSGdata$pr==8] <- NA
VSGdata$pr <- 2 - VSGdata$pr

VSGdata$stv <- VSGdata$electway2_2018
VSGdata$stv[VSGdata$stv==8] <- NA
VSGdata$stv <- VSGdata$stv - 1


### agecat -- four bins: 18-34, 35-49, 50-64, 65+

VSGdata$age <- 2018-VSGdata$birthyr_2018
VSGdata$agecat <- cut(VSGdata$age, breaks=c(18, 34, 49, 64, max(VSGdata$age, na.rm=T)))
VSGdata$ageordinv <- 5 - as.numeric(VSGdata$agecat)

### wnh (white, non-hispanic)

VSGdata$wnh <- ifelse(VSGdata$race_2018 == 1, 0, 1)

### black

VSGdata$black <- ifelse(VSGdata$race_2018==2, 1, 0)
### gender -- to female

VSGdata$female <- ifelse(VSGdata$gender_2018 == 2, 1, 0)

### edu -- HS, some college, 4-year degree. Zero here notes no HS diploma. 2-year degree is "some college."

VSGdata$edu <- VSGdata$educ_2018
VSGdata$edu[VSGdata$edu==6] <- 5
VSGdata$edu[VSGdata$edu==3] <- 4
VSGdata$edu[VSGdata$edu==2] <- 3
VSGdata$edu <- VSGdata$edu-2
VSGdata$edu[VSGdata$edu==-1] <- 0

### pid3
VSGdata$pid3 <- VSGdata$pid3_2018
VSGdata$pid3[VSGdata$pid3==1] <- "Dem"
VSGdata$pid3[VSGdata$pid3==2] <- "Rep"
VSGdata$pid3[VSGdata$pid3==3] <- "Ind"
VSGdata$pid3[VSGdata$pid3==4] <- "Ind" # other
VSGdata$pid3[VSGdata$pid3==5] <- NA # Not sure
VSGdata$pid3 <- factor(VSGdata$pid3)
VSGdata$pid3 <- as.numeric(VSGdata$pid3)

VSGdata$pid7 <- VSGdata$pid7_2018
VSGdata$leaner <- ifelse(VSGdata$pid7 == 3 | VSGdata$pid7 == 5 | VSGdata$pid7 == 4, 1, 0)
VSGdata$pid7factor <- as.factor(VSGdata$pid7)

lm.vsg <- lm(rcv ~ as.numeric(agecat) + female + edu + pid7factor + wnh + black, weight=weight_overall, data=VSGdata)
summary(lm.vsg)

# satisfaction with democracy

VSGdata$satisf_dem_2018 <- 5 - VSGdata$satisf_dem_2018
VSGdata$trustgovt_2018 <- 4 - VSGdata$trustgovt_2018

##################################################
# DATA: CCES 2016
##################################################

###### NOTE: need factor analysis with index

Grcv <- read.dta13("GronkeCCES.dta")

### rcv support -- an additive index here

Grcv$rcv.reduce.corrupt <- Grcv$RCO388
Grcv$rcv.reduce.corrupt[Grcv$rcv.reduce.corrupt==3] <- NA
Grcv$rcv.reduce.corrupt <- (-1*Grcv$rcv.reduce.corrupt)+2

Grcv$rcv.more.compet <- Grcv$RCO389
Grcv$rcv.more.compet[Grcv$rcv.more.compet ==3] <- NA
Grcv$rcv.more.compet <- (-1*Grcv$rcv.more.compet)+2

Grcv$no.change <- Grcv$RCO390
Grcv$no.change[Grcv$no.change ==3] <- NA
Grcv$no.change <- Grcv$no.change-1 #as "would change something"

Grcv$bad.idea <- Grcv$RCO391
Grcv$bad.idea[Grcv$bad.idea ==3] <- NA
Grcv$bad.idea <- Grcv$bad.idea-1 # as "good idea"


# create index of four RCV variables
Grcv$rcv <- (Grcv$rcv.reduce.corrupt + Grcv$rcv.more.compet + Grcv$no.change + Grcv$bad.idea)/4
#

### agecat -- four bins: 18-34, 35-49, 50-64, 65+

Grcv$age <- 2016-Grcv$birthyr
Grcv$agecat <- cut(Grcv$age, breaks=c(18, 34, 49, 64, max(Grcv$age, na.rm=T)))

### wnh (white, non-hispanic)

Grcv$wnh <- ifelse(Grcv$race == 1, 1, 0)
Grcv$wnh[Grcv$race==3] <- 1

### black

Grcv$black <- ifelse(Grcv$race == 2, 1, 0)

### gender -- to female

Grcv$female <- ifelse(Grcv$gender == 2, 1, 0)

### edu -- HS, some college, 4-year degree. Zero here notes no HS diploma. 2-year degree is "some college."

Grcv$edu <- Grcv$educ
Grcv$edu[Grcv$edu==6] <- 5
Grcv$edu[Grcv$edu==3] <- 4
Grcv$edu[Grcv$edu==2] <- 3
Grcv$edu <- Grcv$edu-2
Grcv$edu[Grcv$edu==-1] <- 0

### pid3

Grcv$pid3[Grcv$pid3==4] <- 3 # Other to ind
Grcv$pid3[Grcv$pid3==2] <- "Rep"
Grcv$pid3[Grcv$pid3==3] <- 2
Grcv$pid3[Grcv$pid3=="Rep"] <- 3
Grcv$pid3[Grcv$pid3==5] <- NA # Not sure
Grcv$pid3 <- as.numeric(Grcv$pid3)

Grcv$pid7factor <- as.factor(Grcv$pid7)

##################################################
# DATA: SANTA FE
##################################################

sfdata <- read.dta13("santafe.dta")

### rcv

sfdata$best_method <- ifelse(sfdata$best_method == "I prefer that the winning candidate gets a majority of votes through Ranked Choice Voting ",
                     1, ifelse(sfdata$best_method == "I prefer that the winning candidate gets the most votes, whether or not it is a majority",0, NA))
sfdata$rcv_future <- ifelse(sfdata$rcv_future == "Yes", 1, 0)
sfdata$use_rcv_state <- ifelse(sfdata$use_rcv_state == "Ranked Choice Voting should be used to select the winner", 1, 0)

#create and RCV index

sfdata$rcv <- (sfdata$best_method + sfdata$rcv_future + sfdata$use_rcv_state)/3

### agecat

sfdata$agecat <- as.numeric(sfdata$age)
sfdata$agecat[sfdata$agecat==2] <- 1


### wnh

sfdata$wnh <-ifelse(sfdata$race == "White", 0, 1)

### black

sfdata$black <- ifelse(sfdata$race == "African-American", 1, 0)

### gender

sfdata$female <- ifelse(sfdata$gender == "Female" | sfdata$gender == "Transgender female", 1, 0)

### edu

# This isn't right... category mismatch
sfdata$edu <- as.numeric(sfdata$educ)

### pid3

sfdata$pid3 <- as.numeric(sfdata$party_affil)
sfdata$pid3[sfdata$pid3>2] <- "I"
sfdata$pid3[sfdata$pid3==2] <- 3
sfdata$pid3[sfdata$pid3=="I"] <- 2
sfdata$pid3 <- as.numeric(sfdata$pid3)

##################################################
# DATA: BEYER
##################################################

Beyerdata <- read.dta13("Beyer.dta")

### rcv -- Q40 "should your member support the bill"

# I think this scale is flipped
Beyerdata$rcv <- Beyerdata$Q40
Beyerdata$rcv[Beyerdata$rcv==99] <- NA
Beyerdata$rcv <- rescale(Beyerdata$rcv, to=c(0,1), from=c(1, 2))
Beyerdata$rcv <- -1*(Beyerdata$rcv-1)

Beyerdata$stv <- Beyerdata$Q52
Beyerdata$stv[Beyerdata$stv==99] <- NA
Beyerdata$stv <- rescale(Beyerdata$stv, to=c(0,1), from=c(1, 2))
Beyerdata$stv <- -1*(Beyerdata$stv-1)


### agecat

Beyerdata$agecat <- as.factor(Beyerdata$age_cat5)

### wnh

Beyerdata$wnh <- Beyerdata$PDEMRACEWHITE
Beyerdata$wnh[Beyerdata$PDEMSPAN==1] <- 0

### black

Beyerdata$black <- Beyerdata$PDEMRACEBLACK

### gender

Beyerdata$gender <- as.factor(Beyerdata$gender)
levels(Beyerdata$gender) <- c("Male", "Female")
Beyerdata$female <- ifelse(Beyerdata$gender == "Female", 1, 0)

### edu

Beyerdata$edu <- Beyerdata$educat_wt

### pid3

# Is the scale right?
Beyerdata$pid3 <- Beyerdata$partyid
Beyerdata$pid3[Beyerdata$pid3==2] <- "D"
Beyerdata$pid3[Beyerdata$pid3==3] <- "I"
Beyerdata$pid3[Beyerdata$pid3==1] <- "R"
Beyerdata$pid3 <- as.factor(Beyerdata$pid3)
Beyerdata$pid3 <- as.numeric(Beyerdata$pid3)

##################################################
# REGRESSIONS: SIMPLE
##################################################

lm.maine <- lm(rcv ~ as.numeric(agecat) + female + edu + pid3, weight=weight, data=mainedata)

lm.vsg <- lm(rcv ~ as.numeric(agecat) + female + edu + pid3 + wnh + black, weight=weight_overall, data=VSGdata)
lm.vsg.stv <- lm(stv ~ as.numeric(agecat) + female + edu + pid3 + wnh + black, weight=weight_overall, data=VSGdata)
lm.vsg.pr <- lm(pr ~ as.numeric(agecat) + female + edu + pid3 + wnh + black, weight=weight_overall, data=VSGdata)


lm.cces16 <- lm(rcv ~ as.numeric(agecat) + female + edu + pid3 + wnh + black, weight=weight, data=Grcv)

lm.beyer <- lm(rcv ~ as.numeric(agecat) + female + edu + pid3 + wnh + black, weight=weight, data=Beyerdata)

lm.santafe <- lm(rcv ~ as.numeric(agecat) + female + edu + pid3 + wnh + black, data=sfdata)

##################################################
# CROSSTABS: SIMPLE
##################################################

prop.table(table(VSGdata$rcv, VSGdata$black), 2)
prop.table(table(Grcv$bad.idea, Grcv$black), 2)
prop.table(table(Beyerdata$rcv, Beyerdata$black), 2)
prop.table(table(sfdata$rcv, sfdata$black), 2)

prop.table(table(mainedata$rcv, mainedata$pid3), 2)
prop.table(table(VSGdata$rcv, VSGdata$pid3), 2)
prop.table(table(Grcv$bad.idea, Grcv$pid3), 2)
prop.table(table(Beyerdata$rcv, Beyerdata$pid3), 2)
prop.table(table(sfdata$rcv, sfdata$pid3), 2)

prop.table(table(mainedata$rcv, mainedata$agecat), 2)
prop.table(table(VSGdata$rcv, VSGdata$agecat), 2)
prop.table(table(Grcv$bad.idea, Grcv$agecat), 2)
prop.table(table(Beyerdata$rcv, Beyerdata$agecat), 2)
prop.table(table(sfdata$rcv, sfdata$agecat), 2)


##################################################
# TABLE: SIMPLE
##################################################

simple.table <- htmlreg(l=list(lm.vsg, lm.cces16, lm.beyer, lm.santafe, lm.maine), custom.coef.names=c("Intercept", "Age (cat.)", "Female", "Education (cat.)", "Party ID (3-point)", "White non-Hispanic", "Black"), custom.model.names=c("VSG 2018", "CCES 2016", "Beyer", "Santa Fe", "Maine"), caption="Linear models of RCV support. Age and education category levels are slightly different in the Beyer, Maine, and Santa Fe data. Party ID in Maine is self-reported registration.")

writeLines(simple.table, con="output/simple_coefs.html")

##################################################
# REGRESSIONS: PID3 INTERACTIONS
##################################################

lm.maine.pid <- lm(rcv ~ as.numeric(agecat) + female + edu + pid3 + as.numeric(agecat)*pid3, weight=weight, data=mainedata)

lm.vsg.pid <- lm(rcv ~ as.numeric(agecat) + female + edu + pid3 + wnh + black + as.numeric(agecat)*pid3, weight=weight_overall, data=VSGdata)

lm.cces16.pid <- lm(rcv ~ as.numeric(agecat) + female + edu + pid7 + wnh + black + as.numeric(agecat)*pid3, weight=weight, data=Grcv)

lm.beyer.pid <- lm(rcv ~ as.numeric(agecat) + female + edu + pid3 + wnh + black + as.numeric(agecat)*pid3, weight=weight, data=Beyerdata)

lm.santafe.pid <- lm(rcv ~ as.numeric(agecat) + female + edu + pid3 + wnh + black + as.numeric(agecat)*pid3, data=sfdata)

##################################################
# TABLE: PID3 INTERACTIONS
##################################################

pid.table <- htmlreg(l=list(lm.vsg.pid, lm.cces16.pid, lm.beyer.pid, lm.santafe.pid, lm.maine.pid), custom.coef.names=c("Intercept", "Age (cat.)", "Female", "Education (cat.)", "Party ID (3-point)", "White non-Hispanic", "Black", "Age/PID3 interaction"), custom.model.names=c("VSG 2018", "CCES 2016", "Beyer", "Santa Fe", "Maine"), caption="Linear models of RCV support. Age and education category levels are slightly different in the Beyer, Maine, and Santa Fe data. Party ID in Maine is self-reported registration.")

writeLines(pid.table, con="output/pid_coefs.html")

##################################################
# REGRESSIONS: BLACK INTERACTIONS
##################################################

# lm.maine.black <- lm(rcv ~ as.numeric(agecat) + female + edu + pid3 + as.numeric(agecat)*black, weight=weight, data=mainedata) # Can't run it. No race.

lm.vsg.black <- lm(rcv ~ as.numeric(agecat) + female + edu + pid3 + wnh + black + as.numeric(agecat)*black, weight=weight_overall, data=VSGdata)

lm.cces16.black <- lm(rcv ~ as.numeric(agecat) + female + edu + pid3 + wnh + black + as.numeric(agecat)*black, weight=weight, data=Grcv)

lm.beyer.black <- lm(rcv ~ as.numeric(agecat) + female + edu + pid3 + wnh + black + as.numeric(agecat)*black, weight=weight, data=Beyerdata)

lm.santafe.black <- lm(rcv ~ as.numeric(agecat) + female + edu + pid3 + wnh + black + as.numeric(agecat)*black, data=sfdata)

##################################################
# TABLE: BLACK INTERACTIONS
##################################################

black.table <- htmlreg(l=list(lm.vsg.black, lm.cces16.black, lm.beyer.black, lm.santafe.black), custom.coef.names=c("Intercept", "Age (cat.)", "Female", "Education (cat.)", "Party ID (3-point)", "White non-Hispanic", "Black", "Age/Black interaction"), custom.model.names=c("VSG 2018", "CCES 2016", "Beyer", "Santa Fe"), caption="Linear models of RCV support. Age and education category levels are slightly different in the Beyer and Santa Fe data. The Maine survey did not include race.")

writeLines(black.table, con="output/black_coefs.html")

##################################################
# REGRESSIONS: SUBGROUP SIMPLE
##################################################

VSGdatablack <- subset(VSGdata, black == 1)
VSGdatawnh <- subset(VSGdata, wnh == 1)
VSGdataDem <- subset(VSGdata, pid3 == 1)
VSGdataGOP <- subset(VSGdata, pid3 == 2)

lm.vsg.blacksub <- lm(rcv ~ as.numeric(agecat) + female + edu + pid3, weight=weight_overall, data=VSGdatablack)
lm.vsg.wnhsub <- lm(rcv ~ as.numeric(agecat) + female + edu + pid3, weight=weight_overall, data=VSGdatawnh)
lm.vsg.demsub <- lm(rcv ~ as.numeric(agecat) + female + edu + wnh + black, weight=weight_overall, data=VSGdataDem)
lm.vsg.gopsub <- lm(rcv ~ as.numeric(agecat) + female + edu + wnh + black, weight=weight_overall, data=VSGdataGOP)


##################################################
# TABLE: SUBGROUP SIMPLE
##################################################

subgroup.table <- htmlreg(l=list(lm.vsg.blacksub, lm.vsg.wnhsub, lm.vsg.demsub, lm.vsg.gopsub), custom.coef.names=c("Intercept", "Age (cat.)", "Female", "Education (cat.)", "Party ID (3-point)", "White non-Hispanic", "Black"),
                          custom.model.names=c("Blacks", "Non-Hispanic Whites", "Democrats", "Republicans"), caption="Linear models of RCV support across racial and partisan subgroups in the Voter Study Group data")

writeLines(subgroup.table, con="output/subgroup_coefs.html")


##################################################
# REGRESSIONS: ANTI-SYSTEM
##################################################

lm.vsg.satisf <- lm(rcv ~ as.numeric(agecat) + female + edu + pid3 + wnh + black + satisf_dem_2018, weight=weight_overall, data=VSGdata)
summary(lm.vsg.satisf)

lm.vsg.satisf.int <- lm(rcv ~ as.numeric(agecat)*satisf_dem_2018 + female + edu + pid3 + wnh + black, weight=weight_overall, data=VSGdata)
summary(lm.vsg.satisf.int)

#VSGdata <- VSGdata[!is.na(VSGdata$rcv),]

lm.vsg.satisf.dv <- lm(satisf_dem_2018 ~ as.numeric(agecat) + female + edu + pid3 + wnh + black, weight=weight_overall, data=VSGdata)
summary(lm.vsg.satisf.dv)


library(mediation)
#med <- mediate(lm.vsg.satisf.dv, lm.vsg.satisf, treat = "ageordinv", mediator = "satisf_dem_2018", boot = TRUE, sims = 500)

lm.vsg.satisf.black <- lm(rcv ~ as.numeric(agecat) + satisf_dem_2018 + female + edu + pid3, weight=weight_overall, data=VSGdatablack)
summary(lm.vsg.satisf.black)




##################################################
# TABLE: ANTI-SYSTEM
##################################################

antisys.table <- htmlreg(l=list(lm.vsg.satisf.dv, lm.vsg.satisf), custom.coef.names=c(
  "Intercept", "Age (cat.)", "Female", "Education (cat.)", "Party ID (3-point)", "White non-Hispanic", "Black", "Satisfaction with Democracy"), 
  custom.model.names=c("Satisfaction with Democracy", "RCV Support"), 
caption="Mediation analysis of age, satisfaction with the way democracy works, and RCV support using the Voter Study Group data. The first model shows the relationship between age and satisfaction with democracy.
The second model shows the relationship between satisfaction with democracy and RCV support, controlling for age,")

writeLines(antisys.table, con="output/antisys_coefs.html")

interplot(lm.vsg.satisf.int, var1 = "ageordinv", var2 = "satisf_dem_2018", point = TRUE) + xlab("Satisfaction with democracy") + ylab("Effect of age on support for RCV")

plot + xlab("Satisfaction with democracy")

##################################################
# REGRESSIONS: AGE APPENDIX
##################################################

lm.vsg.ord <- lm(rcv ~  female + edu + pid3 + wnh + black + as.numeric(agecat), weight=weight_overall, data=VSGdata)

lm.vsg.cat <- lm(rcv ~ female + edu + pid3 + wnh + black + agecat, weight=weight_overall, data=VSGdata)
summary(lm.vsg.cat)

lm.vsg.num <- lm(rcv ~ female + edu + pid3 + wnh + black + age, weight=weight_overall, data=VSGdata)


##################################################
# TABLE: AGE APPENDIX
##################################################

antisys.table <- htmlreg(l=list(lm.vsg.ord, lm.vsg.num, lm.vsg.cat), custom.coef.names=c(
  "Intercept", "Female", "Education (cat.)", "Party ID (3-point)", "White non-Hispanic", "Black", "Age (cat.)", "Age (num.)", "35-49", "50-64", "65+"), 
  custom.model.names=c("Ordinal Age", "Numerical Age", "Categorical Age"), caption="Linear models of RCV support in Voter Study Group data,
  using three operationalizations of age. Ordinal and categorical age each include four age categories: 18-34, 35-49, 50-64, and 65+.")

writeLines(antisys.table, con="output/ageapp_coefs.html")


##################################################
# PLOT: EFFECTS IN VSG
##################################################

level.names <- levels(VSGdata$agecat)

level.nos <- seq_along(level.names)

##### PREDICTIONS FROM SIMPLE MODEL

newdat.simple <- cbind.data.frame("agecat"=level.nos, "female"=rep(mean(VSGdata$female, na.rm=T), 4), "edu"=rep(mean(VSGdata$edu, na.rm=T), 4), "wnh"=rep(mean(VSGdata$wnh, na.rm=T), 4), "pid3"=rep(mean(VSGdata$pid3, na.rm=T), 4), "black"=rep(mean(VSGdata$black, na.rm=T), 4))

pred.simple <- predict.lm(lm.vsg, newdata=newdat.simple, interval="confidence")

forplot.simple <- cbind(newdat.simple, pred.simple)

##### PREDICTIONS FROM PID3 MODEL

newdat.pid3 <- cbind.data.frame("agecat"=rep(level.nos, 3), "female"=rep(mean(VSGdata$female, na.rm=T), 12), "edu"=rep(mean(VSGdata$edu, na.rm=T), 12), "wnh"=rep(mean(VSGdata$wnh, na.rm=T), 12), "pid3"=c(1,1,1,1,2,2,2,2,3,3,3,3), "black"=rep(mean(VSGdata$black, na.rm=T), 4))

pred.pid3 <- predict.lm(lm.vsg.pid, newdata=newdat.pid3, interval="confidence")

forplot.pid3 <- cbind(newdat.pid3, pred.pid3)

##### PREDICTIONS FROM BLACK MODEL

newdat.black <- cbind.data.frame("agecat"=rep(level.nos, 2), "female"=rep(mean(VSGdata$female, na.rm=T), 8), "edu"=rep(mean(VSGdata$edu, na.rm=T), 8), "wnh"=rep(mean(VSGdata$wnh, na.rm=T), 8), "pid3"=rep(mean(VSGdata$pid3, na.rm=T), 8), "black"=c(0,0,0,0,1,1,1,1))

pred.black <- predict.lm(lm.vsg.pid, newdata=newdat.black, interval="confidence")

forplot.black <- cbind(newdat.black, pred.black)


#### PREDICTIONS FROM SATISFACTION MODEL

VSGdata18 <- subset(VSGdata, agecat == "(18,34]")
VSGdata35 <- subset(VSGdata, agecat == "(34,49]")
VSGdata50 <- subset(VSGdata, agecat == "(49,64]")
VSGdata65 <- subset(VSGdata, agecat == "(64,93]")


#18-34
newdat.satisf.age1 <- with(VSGdata18, data.frame(satisf_dem_2018 = c(1, 2, 3, 4), agecat = mean(as.numeric(agecat), na.rm = TRUE), female = mean(female), edu = mean(edu), wnh = mean(wnh), 
                                                 pid3 = mean(pid3, na.rm = TRUE), black = mean(black)))

pred.satisf <- predict(lm.vsg.satisf, newdata=newdat.satisf.age1, type='response', se = TRUE)
probs1 <- cbind(pred.satisf, newdat.satisf.age1)

#35-49

newdat.satisf.age2 <- with(VSGdata35, data.frame(satisf_dem_2018 = c(1, 2, 3, 4), agecat = mean(as.numeric(agecat), na.rm = TRUE), female = mean(female), edu = mean(edu), wnh = mean(wnh), 
                                                 pid3 = mean(pid3, na.rm = TRUE), black = mean(black)))

pred.satisf <- predict(lm.vsg.satisf, newdata=newdat.satisf.age2, type='response', se = TRUE)
probs2 <- cbind(pred.satisf, newdat.satisf.age2)

#50-64

newdat.satisf.age3 <- with(VSGdata50, data.frame(satisf_dem_2018 = c(1, 2, 3, 4), agecat = mean(as.numeric(agecat), na.rm = TRUE), female = mean(female), edu = mean(edu), wnh = mean(wnh), 
                                                 pid3 = mean(pid3, na.rm = TRUE), black = mean(black)))

pred.satisf <- predict(lm.vsg.satisf, newdata=newdat.satisf.age3, type='response', se = TRUE)
probs3 <- cbind(pred.satisf, newdat.satisf.age3)

#65+

newdat.satisf.age4 <- with(VSGdata65, data.frame(satisf_dem_2018 = c(1, 2, 3, 4), agecat = mean(as.numeric(agecat), na.rm = TRUE), female = mean(female), edu = mean(edu), wnh = mean(wnh), 
                                                 pid3 = mean(pid3, na.rm = TRUE), black = mean(black)))

pred.satisf <- predict(lm.vsg.satisf, newdata=newdat.satisf.age4, type='response', se = TRUE)
probs4 <- cbind(pred.satisf, newdat.satisf.age4)


##### BUILD THE PLOT

# None of this needed... simple model is simple. Doesn't need a graphic.
# attach(forplot.simple)
# plot(agecat, fit, ylim=c(-0.1, 0.6), axes=F, xlab="", ylab="")
# segments(x0=agecat, y0=lwr, y1=upr)
# lines(agecat, fit)
# axis(1, tick=F, labels=level.names, at=level.nos)
# axis(2, tick=F, las=2)
# detach(forplot.simple)

pdf('output/interaction_effects.pdf')
par(mfrow=c(2,2))

### Democrats

attach(forplot.pid3)
plot(agecat[forplot.pid3$pid3==1], fit[forplot.pid3$pid3==1], ylim=c(-0.1, 0.6), axes=F, xlab="", ylab="", main="Democrats")
abline(h=0, lty=3)
segments(x0=agecat[forplot.pid3$pid3==1], y0=lwr[forplot.pid3$pid3==1], y1=upr[forplot.pid3$pid3==1])
lines(agecat[forplot.pid3$pid3==1], fit[forplot.pid3$pid3==1])
axis(1, tick=F, labels=level.names, at=level.nos)
axis(2, tick=F, las=2)
detach(forplot.pid3)

### Republicans

attach(forplot.pid3)
plot(agecat[forplot.pid3$pid3==3], fit[forplot.pid3$pid3==3], ylim=c(-0.1, 0.6), axes=F, xlab="", ylab="", main="Republicans")
abline(h=0, lty=3)
segments(x0=agecat[forplot.pid3$pid3==3], y0=lwr[forplot.pid3$pid3==3], y1=upr[forplot.pid3$pid3==3])
lines(agecat[forplot.pid3$pid3==3], fit[forplot.pid3$pid3==3])
axis(1, tick=F, labels=level.names, at=level.nos)
axis(2, tick=F, las=2)
detach(forplot.pid3)

### Non-Blacks

attach(forplot.black)
plot(agecat[forplot.black$black==0], fit[forplot.black$black==0], ylim=c(-0.1, 0.6), axes=F, xlab="", ylab="", main="Non-Blacks")
abline(h=0, lty=3)
segments(x0=agecat[forplot.black$black==0], y0=lwr[forplot.black$black==0], y1=upr[forplot.black$black==0])
lines(agecat[forplot.black$black==0], fit[forplot.black$black==0])
axis(1, tick=F, labels=level.names, at=level.nos)
axis(2, tick=F, las=2)
detach(forplot.black)

### Blacks

attach(forplot.black)
plot(agecat[forplot.black$black==1], fit[forplot.black$black==1], ylim=c(-0.1, 0.6), axes=F, xlab="", ylab="", main="Blacks")
abline(h=0, lty=3)
segments(x0=agecat[forplot.black$black==1], y0=lwr[forplot.black$black==1], y1=upr[forplot.black$black==1])
lines(agecat[forplot.black$black==1], fit[forplot.black$black==1])
axis(1, tick=F, labels=level.names, at=level.nos)
axis(2, tick=F, las=2)
detach(forplot.black)

dev.off()

###
p1 <- ggplot(data = probs1, aes(x = satisf_dem_2018, y = fit)) + 
  geom_errorbar(aes(ymin=fit-se.fit, ymax=fit+se.fit), colour="black", width=.1) +
  geom_line() +  geom_point(size=2, shape=21, fill="white") +
  ylab("Predicted RCV Support") + xlab("Satisfaction with democracy") + theme_classic() + scale_y_continuous(limits=c(0, 0.6)) + ggtitle("18-34") + 
  theme(text = element_text(size=15))

p2 <- ggplot(data = probs2, aes(x = satisf_dem_2018, y = fit)) + 
  geom_errorbar(aes(ymin=fit-se.fit, ymax=fit+se.fit), colour="black", width=.1) +
  geom_line() +  geom_point(size=2, shape=21, fill="white") +
  ylab("Predicted RCV Support") + xlab("Satisfaction with democracy") + theme_classic() + scale_y_continuous(limits=c(0, 0.6)) + ggtitle("35-49") + 
  theme(text = element_text(size=15))

p3 <- ggplot(data = probs3, aes(x = satisf_dem_2018, y = fit)) + 
  geom_errorbar(aes(ymin=fit-se.fit, ymax=fit+se.fit), colour="black", width=.1) +
  geom_line() +  geom_point(size=2, shape=21, fill="white") +
  ylab("Predicted RCV Support") + xlab("Satisfaction with democracy") + theme_classic() + scale_y_continuous(limits=c(0, 0.6)) + ggtitle("50-64") + 
  theme(text = element_text(size=15))

p4 <- ggplot(data = probs4, aes(x = satisf_dem_2018, y = fit)) + 
  geom_errorbar(aes(ymin=fit-se.fit, ymax=fit+se.fit), colour="black", width=.1) +
  geom_line() +  geom_point(size=2, shape=21, fill="white") +
  ylab("Predicted RCV Support") + xlab("Satisfaction with democracy") + theme_classic() + scale_y_continuous(limits=c(0, 0.6)) + ggtitle("65+") + 
  theme(text = element_text(size=15))

grid.arrange(p1, p2, p3, p4, ncol = 2)
