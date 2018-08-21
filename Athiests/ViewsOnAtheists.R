

'# PROJECT: REGRESSION IN R EXERCISES #'   
'# PURPOSE: MULTIPLE REGRESSION: ATHEISTS FT #'   
'# DIR:     Dropbox/R/RegressionInR #'   
'# DATA:    ATP W24.sav #'   
'# AUTHOR:  Todd COMBS #'   
'# CREATED: JUL 27, 2018 #'   
'# LATEST:  JUL 27, 2018 #'   
'# NOTES:   Pew religion survey #'   
'#          Data from ACS 5-yr 2012-16 Social Explorer #'
'# PROLOG   ################################################'  


#libraries
library(tidyverse)
library(magrittr)
library(lmtest)
library(stargazer)
library(haven)
library(labelled)
library(survey)
library(car)
library(sandwich)

# ADJ R SQUARED FUNCTION 

#adjusted r-squared function
ar2 <- function (srm) # where srm is the survey regression model object
{
  r2 = (srm$null.deviance - srm$deviance)/srm$null.deviance
  adjust = srm$df.null/(srm$df.null - length(coef(srm)))
  value = 1 - ((1 - r2) * adjust)
  return(value)
}  
# data
r <- read_spss("ATP W24.sav")
# make variable dictionary
rlabs <- tibble(var = names(r), labels =as.character(var_label(r)))
rlabs2 <- rlabs %>%
  mutate(labels = str_sub(labels,start=95))
# rename vars
r <- r %>%
  rename(atheistFT = REL3F_W24, 
         weight = WEIGHT_W24,
         relig = F_RELIG_FINAL,
         ideo = F_IDEO_FINAL,
         income = F_INCOME_FINAL,
         raceth = F_RACETHN_RECRUITMENT,
         ed = F_EDUCCAT_FINAL,
         age = F_AGECAT_FINAL,
         reg = F_CREGION_FINAL,
         sex = F_SEX_FINAL,
         knowNonRelig = REL5_W24,
         kind = KIND1_W24)

r <- r %>%
  select(atheistFT,weight, relig, ideo, income, raceth,
         ed, age, reg, sex, knowNonRelig, kind)

# recodes

r <- r %>%
  mutate(relig = case_when(relig==1~"Protestant",
                           relig==2~"Roman Catholic",
                           relig %in% 9:10~"Atheist/Agnostic",
                           relig==12~"None",
                           relig %in% c(3:8,11,13:14)~"Other"),
         ideo = case_when(ideo %in% 1:2~"Conservative",
                          ideo==3~"Moderate",
                          ideo %in% 4:5~"Liberal"),
         income = case_when(income %in% 1:3~"<$30k",
                            income %in% 4:6~"$30-75k",
                            income %in% 7:9~">$75k"),
         raceth = case_when(raceth==1~"White",
                            raceth==2~"Black",
                            raceth==3~"Hispanic",
                            raceth==4~"Other"),
         ed = case_when(ed==1~"College grad",
                        ed %in% 2:3~"No degree"),
         age = case_when(age==1~"18-29",
                         age==2~"30-49",
                         age==3~"50-64",
                         age==4~"65+"),
         reg = case_when(reg==1~"Northeast",
                         reg==2~"Midwest",
                         reg==3~"South",
                         reg==4~"West"),
         sex = case_when(sex==1~"Male",
                         sex==2~"Female"),
         knowNonRelig = case_when(knowNonRelig==1~"Yes",
                                  knowNonRelig==2~"No"),
         kind = case_when(kind==1~"People are kind",
                          kind==2~"People are unkind"))
r <- r %>%
  mutate_at(vars(3:12),factor)
  
 
 
rs <- svydesign(ids=~1, data=na.omit(r), weights=~weight)

m1 <- svyglm(atheistFT~relig+ideo+income+raceth+ed+age+
               reg+sex+knowNonRelig+kind,
             design=rs)  
  
m2 <- svyglm(atheistFT~relig+income+ed+age+knowNonRelig+ideo+kind,
             design=rs)  



ar2(m2)
  
# SVYGLM computes robust standard errors so these should suffice for
# potential heteroskedasticity

# outliers/influential obs
g <- ggplot(m2, aes(seq_along(.cooksd), .cooksd))
r2 <- as_tibble(g$data)
r3 <- rs$variables %>%
  mutate(cooksd = r2 %$% .cooksd)

r3 <- r3 %>%
  mutate(high = case_when(cooksd>mean(cooksd)*3~"Over 3*mean(cooks'd)",
                          cooksd<=mean(cooksd)*3~"Acceptable        "),
         obs = row_number()) 

g <- r3 %>%
  ggplot(aes(x=obs, y=cooksd, color=high))

g <- g + geom_point()


g <- g+labs(x="Obs. Number", y="Cook's distance", color="",title="Cook's distance")

g <- g  + theme_minimal() + theme(legend.position = 'top')

g

rs2 <- svydesign(ids=~1, data=r3, weights=~weight)

rs3 <- subset(rs2, cooksd<0.005)


m3 <- svyglm(atheistFT~relig+income+ed+age+knowNonRelig+ideo+kind,
             design=rs3)  

# coefs aren't that different, so we'll use all obs (m2)




# coefplot

myco <- summary(m2)$coef[-1,1:2]

myco <- as_tibble(myco) %>%
  mutate(lab=c("Religion: None v. atheists",
        "Religion: Other v. atheists",
        "Religion: Protestant v. atheists",
        "Religion: Catholic v. atheists",
        "Income: <$30k v. $30-75k",
        "Income: >$75k v. $30-75k",
        "Education: College degree v. none",
        "Age: 30-49 v. 18-30",
        "Age: 50-64 v. 18-30",
        "Age: 65+ v. 18-30",
        "Know someone who is not religious: Yes",
        "Liberal v. Conservative",
        "Moderate v. Conservative",
        "People are generally kind: No"))

names(myco) <-  c("est","se","lab")

myco <- myco %>%
  mutate(lb=est-se*1.96,
         ub=est+se*1.96)
  
# plot

g <- myco %>%
  ggplot(aes(x=est,xmin=lb,xmax=ub,y=reorder(lab,est)))

g <- g + geom_errorbarh(height=0.25) + geom_point()

g <- g + labs(x="Estimate with 95% confidence intervals",
              y="",subtitle="N: 4048, Adj. R-sq: 0.232")
g <- g + geom_vline(xintercept=0,color="red",linetype=2) +
  theme_minimal()

g
  


# predictions

# liberal v. conservative

r4 <- r3 %>%
  select(1:12)

map(r4 %>% select(-weight,-atheistFT), table)

lib <- as_tibble(r4) %>%
  mutate(relig = as_factor("Protestant", levels=levels(r %$% relig)),
         ideo = as_factor("Liberal", levels=levels(r %$% ideo)),
         income=as_factor("$30-75k", levels=levels(r %$% income)),
         raceth=as_factor("White", levels=levels(r %$% raceth)),
         ed=as_factor("College grad", levels=levels(r %$% ed)),
         age=as_factor("50-64", levels=levels(r %$% age)),
         knowNonRelig= as_factor("Yes", levels(r %$% knowNonRelig)),
         kind = as_factor("People are kind", levels(r %$% kind)))

cons <- as_tibble(r4) %>%
  mutate(relig = as_factor("Protestant", levels=levels(r %$% relig)),
         ideo = as_factor("Conservative", levels=levels(r %$% ideo)),
         income=as_factor("$30-75k", levels=levels(r %$% income)),
         raceth=as_factor("White", levels=levels(r %$% raceth)),
         ed=as_factor("College grad", levels=levels(r %$% ed)),
         age=as_factor("50-64", levels=levels(r %$% age)),
         knowNonRelig= as_factor("Yes", levels(r %$% knowNonRelig)),
         kind = as_factor("People are kind", levels(r %$% kind)))

# Protestant v. non-religious
prot <- as_tibble(r4) %>%
  mutate(relig = as_factor("Protestant", levels=levels(r %$% relig)),
         ideo = as_factor("Moderate", levels=levels(r %$% ideo)),
         income=as_factor("$30-75k", levels=levels(r %$% income)),
         raceth=as_factor("White", levels=levels(r %$% raceth)),
         ed=as_factor("College grad", levels=levels(r %$% ed)),
         age=as_factor("50-64", levels=levels(r %$% age)),
         knowNonRelig= as_factor("Yes", levels(r %$% knowNonRelig)),
         kind = as_factor("People are kind", levels(r %$% kind)))

norel <- as_tibble(r4) %>%
  mutate(relig = as_factor("None", levels=levels(r %$% relig)),
         ideo = as_factor("Moderate", levels=levels(r %$% ideo)),
         income=as_factor("$30-75k", levels=levels(r %$% income)),
         raceth=as_factor("White", levels=levels(r %$% raceth)),
         ed=as_factor("College grad", levels=levels(r %$% ed)),
         age=as_factor("50-64", levels=levels(r %$% age)),
         knowNonRelig= as_factor("Yes", levels(r %$% knowNonRelig)),
         kind = as_factor("People are kind", levels(r %$% kind)))

lp <- predict(m2, newdata = lib, se.fit=T)
lp <- tibble(est=lp[1], se=sqrt(attr(lp,"var")[1]),varn="Liberal Protestant")

cp <- predict(m2, newdata = cons, se.fit=T)
cp <- tibble(est=cp[1], se=sqrt(attr(cp,"var")[1]),varn="Conservative Protestant")

pp <- predict(m2, newdata = prot, se.fit=T)
pp <- tibble(est=pp[1], se=sqrt(attr(pp,"var")[1]),varn="Protestant-Moderate")

np <- predict(m2, newdata = norel, se.fit=T)
np <- tibble(est=np[1], se=sqrt(attr(np,"var")[1]),varn="Not religious-Moderate")


mypred <- bind_rows(lp,cp,pp,np)
mypred <- mypred %>%
  mutate(lb=est-se*1.96,
         ub=est+se*1.96)

g <- mypred %>%
  ggplot(aes(x=varn, y=est, ymin=lb, ymax=ub))

g <- g + geom_errorbar(width=0.25) + geom_point()

g <- g + labs(y="Predicted Atheist feeling thermometer (95% CIs)",
              x="") + theme_minimal()

g
