# Toxoplasmosis-of-cattle-herds
It is a project about seroepidemiology and on farm risk factors of toxoplasmosis on cattle herds of Nepal.

getwd()
setwd("/Users/Tulsigompo/Documents/Presentaion_Dr_Gompo/Rupandehi_study_data/ToxoPlasma")
library(readr)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(car)
cattle_toxo_herd<-read_csv("/Users/Tulsigompo/Documents/Presentaion_Dr_Gompo/Rupandehi_study_data/ToxoPlasma/farm_data_herd_level.csv")
head(cattle_toxo_herd)
str(cattle_toxo_herd)
head(cattle_toxo_herd)
dim(cattle_toxo_herd)
view(cattle_toxo_herd)

variable.names(cattle_toxo_herd)
## Prevalence
table(cattle_toxo_herd$toxoplasmosis)
# Herd characteristics
# Herd_size_cat

table(cattle_toxo_herd$herd_size_cat,cattle_toxo_herd$toxoplasmosis)     # Boderline sig
summary(cattle_toxo_herd$herd_size)

 #cattle_origin_cat
table(cattle_toxo_herd$cattle_origin_cat,cattle_toxo_herd$toxoplasmosis)  # Conntrovercial Not sig

## Herd structure

table(cattle_toxo_herd$ herd_structure,cattle_toxo_herd$toxoplasmosis) 

# locations
table(cattle_toxo_herd$municipality_rural_municipality)
chisq.test(cattle_toxo_herd$municipality_rural_municipality,cattle_toxo_herd$toxoplasmosis)



## Important risk and useful variables
table(cattle_toxo_herd$ presence_of_rat, cattle_toxo_herd$toxoplasmosis)  ## Highly Sig and important
table(cattle_toxo_herd$ dog_and_cat_on_farm,cattle_toxo_herd$toxoplasmosis) # Important and significant
table(cattle_toxo_herd$contact_with_other_domestic_animals_codes, cattle_toxo_herd$toxoplasmosis) ## ## IMportant boderline signifcant
table(cattle_toxo_herd$stray_cat_visit_farms, cattle_toxo_herd$toxoplasmosis) ## IMportant Boderline sig

## Owner knowledge
table (cattle_toxo_herd$do_owner_know_about_zoonosis, cattle_toxo_herd$toxoplasmosis) # Contovercial and not sig
table (cattle_toxo_herd$any_history_of_fever_myalgia, cattle_toxo_herd$ toxoplasmosis)
table(cattle_toxo_herd$take_cattle_in_rice_field, cattle_toxo_herd$toxoplasmosis) #  Not sig


# Univariate or bivaraiate

# Cattle Origin  # Not significant (p=0.4954)
table(cattle_toxo_herd$cattle_origin_codes,cattle_toxo_herd$toxoplasmosis) 
cattle_toxo_herd$cattle_origin_cat<-as.factor(cattle_toxo_herd$cattle_origin_cat)
cattle_toxo_herd$toxoplasmosis<-as.factor(cattle_toxo_herd$toxoplasmosis)
cattle_origin_cat_herd<- glm (toxoplasmosis ~ cattle_origin_cat,  data= cattle_toxo_herd, family= binomial(link="logit"))
summary(cattle_origin_cat_herd)
Anova(cattle_origin_cat_herd)
exp(coef(cattle_origin_cat_herd))
exp(confint(cattle_origin_cat_herd))

# Herd size (Not sig byt p =0.1071 and consider in model)
table(cattle_toxo_herd$herd_size_cat, cattle_toxo_herd$toxoplasmosis)
cattle_toxo_herd$herd_size_cat<-as.factor(cattle_toxo_herd$herd_size_cat)
cattle_toxo_herd$toxoplasmosis<-as.factor(cattle_toxo_herd$toxoplasmosis)
cattle_toxo_herd$herd_size_cat<-relevel(cattle_toxo_herd$herd_size_cat,ref="equal_or_below_10")
Herd_size_1<- glm (toxoplasmosis ~ herd_size_cat,  data= cattle_toxo_herd, family= binomial(link="logit"))
summary(Herd_size_1)
Anova(Herd_size_1)
exp(coef(Herd_size_1))
exp(confint(Herd_size_1))



# contact with other ruminants
table(cattle_toxo_herd$contact_with_other_domestic_animals,cattle_toxo_herd$toxoplasmosis)
cattle_toxo_herd$contact_with_other_domestic_animals<-as.factor(cattle_toxo_herd$contact_with_other_domestic_animals)  
cattle_toxo_herd$toxoplasmosis<-as.factor(cattle_toxo_herd$toxoplasmosis)
cattle_toxo_herd$contact_with_other_domestic_animals<-relevel(cattle_toxo_herd$contact_with_other_domestic_animals,ref="no")
contact_with_other_ruminants<- glm (toxoplasmosis ~ contact_with_other_domestic_animals,  data= cattle_toxo_herd, family= binomial(link="logit"))
summary(contact_with_other_ruminants)
exp(coef(contact_with_other_ruminants))
exp(confint(contact_with_other_ruminants))



# Water purification an source  # Not significant (p=0.3774)

cattle_toxo_herd$water_source<-as.factor(cattle_toxo_herd$water_source)
cattle_toxo_herd$water_source<-relevel(cattle_toxo_herd$water_source, ref="tap")
water_source_1<- glm (toxoplasmosis ~ water_source,  data= cattle_toxo_herd, family= binomial(link="logit"))
summary(water_source_1)
Anova(water_source_1)
exp(coef(water_source_1))
confint(water_source_1)



## Presenace of rat significant ## Highly Sig and important (p=0.007773)
table(cattle_toxo_herd$presence_of_rat, presence_of_rat$toxoplasmosis)
cattle_toxo_herd$presence_of_rat<-as.factor(cattle_toxo_herd$presence_of_rat)
cattle_toxo_herd$presence_of_rat_codes<-as.factor(cattle_toxo_herd$presence_of_rat_codes)
cattle_toxo_herd$presence_of_rat<-relevel(cattle_toxo_herd$presence_of_rat, ref="no")
rat_at_farms<- glm (toxoplasmosis ~ presence_of_rat,  data= cattle_toxo_herd, family= binomial(link="logit"))
summary(rat_at_farms)
Anova(rat_at_farms)
exp(coef(rat_at_farms))
exp(confint(rat_at_farms))


## domestic and stray_cat_visit_farms  are Important and significant

table(cattle_toxo_herd$ stray_cat_visit_farms,cattle_toxo_herd$toxoplasmosis) # Important abut not significant
table(cattle_toxo_herd$stray_cat_visit_farms, cattle_toxo_herd$toxoplasmosis)
cattle_toxo_herd$stray_cat_visit_farms<-as.factor(cattle_toxo_herd$stray_cat_visit_farms)
cattle_toxo_herd$stray_cat_visit_farms<-relevel(cattle_toxo_herd$stray_cat_visit_farms, ref="no")
cat_visit_cattle_farm<- glm (toxoplasmosis ~ stray_cat_visit_farms,  data= cattle_toxo_herd, family= binomial(link="logit"))
summary(cat_visit_cattle_farm)
exp(coef(cat_visit_cattle_farm))
exp(confint(cat_visit_cattle_farm))

# chisquare or fisher exact for checking any associations between the variables (correlations is only for the continueous variables)
# Y=0, N=1
# Running chi-square test to check correlations/associations

table(cattle_toxo_herd$stray_cat_visit_farms,cattle_toxo_herd$dog_and_cat_on_farm)# not associated
chisq.test (cattle_toxo_herd$stray_cat_visit_farms,cattle_toxo_herd$dog_and_cat_on_farm)

table(cattle_toxo_herd$stray_cat_visit_farms,cattle_toxo_herd$herd_size_cat) # not associated
chisq.test (cattle_toxo_herd$stray_cat_visit_farms,cattle_toxo_herd$herd_size_cat)

table(cattle_toxo_herd$stray_cat_visit_farms,cattle_toxo_herd$ presence_of_rat)# associated
chisq.test (cattle_toxo_herd$stray_cat_visit_farms,cattle_toxo_herd$ presence_of_rat)


table(cattle_toxo_herd$dog_and_cat_on_farm,cattle_toxo_herd$herd_size_cat) # not associated
chisq.test (cattle_toxo_herd$dog_and_cat_on_farm,cattle_toxo_herd$herd_size_cat)

table(cattle_toxo_herd$dog_and_cat_on_farm,cattle_toxo_herd$presence_of_rat) # not associated
chisq.test (cattle_toxo_herd$herd_size_cat,cattle_toxo_herd$presence_of_rat)

table(cattle_toxo_herd$herd_size_cat,cattle_toxo_herd$presence_of_rat) # not associated
chisq.test (cattle_toxo_herd$herd_size_cat,cattle_toxo_herd$presence_of_rat)

# Since the p value is not less than 0.05 (not-significant), they are not associated/correlated and can be included in the model. But sometime you need to check if they are biologically related and can be removed and run the 
#seperate models if even not significant.


# Multivariables automated process
library(car)
library(stats)
cattle_toxo_herd$toxoplasmosis<-as.factor(cattle_toxo_herd$toxoplasmosis)
FullModel_toxo<-glm (toxoplasmosis~cattle_origin_cat+herd_size_cat+feeding_type+common_grazing_ground +presence_of_rat +dog_and_cat_on_farm +contact_with_other_domestic_animals +stray_cat_visit_farms,
                       data = cattle_toxo_herd, family = binomial(link = "logit"))
summary(FullModel_toxo)
anova(FullModel_toxo)
Model_toxo_1<-step(FullModel_toxo,direction="backward")
summary(Model_toxo_1)
anova(Model_toxo_1)
exp(coef(Model_toxo_1))
exp(confint(Model_toxo_1))

# By step wise AIC subset selection
options(na.action = "na.fail")
library(MuMIn)
AllSubsets<-dredge(Model_toxo_1,rank="AIC")
head(AllSubsets)

## Running final Model  
cattle_toxo_herd$contact_with_other_domestic_animals<- as.factor (cattle_toxo_herd $ contact_with_other_domestic_animals)
cattle_toxo_herd$presence_of_rat<- as.factor (cattle_toxo_herd $ presence_of_rat)
cattle_toxo_herd$dog_and_cat_on_farm<- as.factor (cattle_toxo_herd $ dog_and_cat_on_farm)

# step wise manual

##Multivaribale Model 1
Model_toxo_2<-glm(toxoplasmosis ~ contact_with_other_domestic_animals + presence_of_rat+ stray_cat_visit_farms +  herd_size_cat, data = cattle_toxo_herd, na.action=na.exclude,family = binomial(link = logit))
summary(Model_toxo_2)
exp(coef(Model_toxo_2))
exp(confint(Model_toxo_2))
library(ResourceSelection)
hoslem.test(Model_toxo_2$y,fitted(Model_toxo_2),g=10)

# Since stray cat visit and  presence of rats were highly correlated, we run different models seperately.
# Multivaribale Model 1 (with stray cat visit)
Model_toxo_3<-glm(toxoplasmosis ~ contact_with_other_domestic_animals + stray_cat_visit_farms + herd_size_cat, data = cattle_toxo_herd, na.action=na.exclude,family = binomial(link = logit))
summary(Model_toxo_3)
exp(coef(Model_toxo_3))
exp(confint(Model_toxo_3))
library(ResourceSelection)
hoslem.test(Model_toxo_3$y,fitted(Model_toxo_3),g=10)

Model_toxo_4 <-glm(toxoplasmosis ~ contact_with_other_domestic_animals + herd_size_cat, data = cattle_toxo_herd, na.action=na.exclude,family = binomial(link = logit))
summary(Model_toxo_4) 


Model_toxo_5 <-glm(toxoplasmosis ~ herd_size_cat, data = cattle_toxo_herd, na.action=na.exclude,family = binomial(link = logit))
summary(Model_toxo_5)

##Multivaribale Model 2 (with presence of rats)
Model_toxo_3<-glm(toxoplasmosis ~ contact_with_other_domestic_animals + presence_of_rat + herd_size_cat, data = cattle_toxo_herd, na.action=na.exclude,family = binomial(link = logit))
summary(Model_toxo_3)
exp(coef(Model_toxo_3))
exp(confint(Model_toxo_3))
library(ResourceSelection)
hoslem.test(Model_toxo_3$y,fitted(Model_toxo_3),g=10)


#Since p value is greater than 0.1 (criteria for retaining) in Multivaribale Model 2, we should remove herd size from the model 
#and re run model again.
##Multivaribale Model 3
Model_toxo_6<-glm (toxoplasmosis ~ contact_with_other_domestic_animals + presence_of_rat, data = cattle_toxo_herd, na.action=na.exclude,family = binomial(link = logit))
summary(Model_toxo_6)
exp(coef(Model_toxo_6))
exp(confint(Model_toxo_6))

Model_toxo_7<-glm (toxoplasmosis ~ herd_size_cat+ presence_of_rat, data = cattle_toxo_herd, na.action=na.exclude,family = binomial(link = logit))
summary(Model_toxo_7)
# there were 

## VIF is preferred as it can show the correlation of a variable with a group of other variables.
par(mfrow=c(2,2))
plot(Model_toxo_1)
library(faraway) # for the VIF functions in R
vif(Model_toxo_1) # Resulsts shows correlation of two variables.

# Accessing model fit
library(ResourceSelection)
hoslem.test(Model_toxo_1$y,fitted(Model_toxo_1),g=10
