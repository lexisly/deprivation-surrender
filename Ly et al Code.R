library(ggplot2)
library(shapefiles)
library(readxl)
library(dplyr)
library(nnet)

#READ CIMD FILE

owner_all_cimd<-read.csv("C:/Users/lexis/OneDrive/Documents/Masters/Density maps project/Data/Ly Protopopova Data.csv", stringsAsFactors = FALSE)

# SURRENDER REASON CATEGORIES
unique(owner_all_cimd$Surrender.Reason)

owner_all_cimd$surrender.category[grepl('Abandoned', owner_all_cimd$Surrender.Reason)]<- "Abandoned"

owner_all_cimd$surrender.category[owner_all_cimd$Surrender.Reason == "Pregnant - Animal Is Pregnant"|
                                    owner_all_cimd$Surrender.Reason == "Upper Respiratory Infection (uri)"|
                                    owner_all_cimd$Surrender.Reason == "Urinary Tract Infection (UTI)"|
                                    owner_all_cimd$Surrender.Reason == "Animal Had Debilitating Injury"|
                                   owner_all_cimd$Surrender.Reason == "Dog got too Big"]<- "Animal Health"

owner_all_cimd$surrender.category[grepl('Behaviour', owner_all_cimd$Surrender.Reason)|
                                   grepl('Aggression', owner_all_cimd$Surrender.Reason)|
                                   grepl('Aggressive', owner_all_cimd$Surrender.Reason)|
                                   grepl('Bit', owner_all_cimd$Surrender.Reason)|
                                   owner_all_cimd$Surrender.Reason == "Attacking Livestock"|
                                   owner_all_cimd$Surrender.Reason == "Food Protective"|
                                   owner_all_cimd$Surrender.Reason == "Barking"|
                                   owner_all_cimd$Surrender.Reason == "Chewing"|
                                   grepl('Digging', owner_all_cimd$Surrender.Reason)|
                                   grepl('Doesnt Like', owner_all_cimd$Surrender.Reason)|
                                   owner_all_cimd$Surrender.Reason == "Home - Pet Is Showing Signs Of Stress In The Home"|
                                   owner_all_cimd$Surrender.Reason == "Jumping The Fence"|
                                   owner_all_cimd$Surrender.Reason == "Marking / Spraying"|
                                   owner_all_cimd$Surrender.Reason == "Not Housebroken"|
                                   owner_all_cimd$Surrender.Reason == "Not Good With Cats"|
                                   owner_all_cimd$Surrender.Reason == "Problems With Other Pets"|
                                   owner_all_cimd$Surrender.Reason == "Reactive House Soiling (situational/stress)"|
                                   owner_all_cimd$Surrender.Reason == "Separation Anxiety"|
                                   owner_all_cimd$Surrender.Reason == "Snapping At Children"]<-"Behaviour"

owner_all_cimd$surrender.category[grepl('Cant Afford', owner_all_cimd$Surrender.Reason)|
                                   owner_all_cimd$Surrender.Reason == "Vet Costs"|
                                   owner_all_cimd$Surrender.Reason == "Wont Pay Impound Fees"]<-"Cant Afford"

owner_all_cimd$surrender.category[grepl('Feral', owner_all_cimd$Surrender.Reason)]<-"Feral"

owner_all_cimd$surrender.category[grepl('Allergies', owner_all_cimd$Surrender.Reason)|
                                   owner_all_cimd$Surrender.Reason == "Death Of Owner"|
                                   owner_all_cimd$Surrender.Reason == "Life Change-Human Illness/Injury/CareHome/Hospital"|
                                   owner_all_cimd$Surrender.Reason == "Owner Had Debilitating Injury"|
                                   owner_all_cimd$Surrender.Reason == "Owner Sick"|
                                   owner_all_cimd$Surrender.Reason == "Treatment Centre"]<-"Guardian Health"

owner_all_cimd$surrender.category[grepl('Moving/Evicted', owner_all_cimd$Surrender.Reason)|
                                   owner_all_cimd$Surrender.Reason == "Eviction"|
                                   owner_all_cimd$Surrender.Reason == "Moving House" | 
                                   owner_all_cimd$Surrender.Reason == "Abandoned By Tenant During Move"| 
                                   owner_all_cimd$Surrender.Reason == "Landlord Does Not Allow Pets"|
                                   owner_all_cimd$Surrender.Reason == "Unable To Find Home"|
                                   owner_all_cimd$Surrender.Reason == "Yard Too Small"|
                                   owner_all_cimd$Surrender.Reason == "Complaints From Neighbour"|
                                   owner_all_cimd$Surrender.Reason == "Military Transfer"|
                                   owner_all_cimd$Surrender.Reason == "Holidays/Travel"|
                                   owner_all_cimd$Surrender.Reason == "Holidays/travel"]<-"Housing Issues"

owner_all_cimd$surrender.category[owner_all_cimd$Surrender.Reason == "Gift"|
                                   owner_all_cimd$Surrender.Reason == "No Longer Wanted"]<-"No Longer Wanted"


owner_all_cimd$surrender.category[grepl('Divorce', owner_all_cimd$Surrender.Reason)|
                                   grepl('New Baby', owner_all_cimd$Surrender.Reason)|
                                   grepl('Relation Split', owner_all_cimd$Surrender.Reason)|
                                   grepl('Jail', owner_all_cimd$Surrender.Reason)|
                                   grepl('Owner Pregnant', owner_all_cimd$Surrender.Reason)|
                                   grepl('Abusive Relationship', owner_all_cimd$Surrender.Reason)|
                                   grepl('Constable Visit', owner_all_cimd$Surrender.Reason)|
                                   grepl('Children Not Ready', owner_all_cimd$Surrender.Reason)|
                                   grepl('No Time', owner_all_cimd$Surrender.Reason)|
                                   grepl('Too Much Responsibility', owner_all_cimd$Surrender.Reason)|
                                   grepl('Children Not Ready', owner_all_cimd$Surrender.Reason)|
                                   grepl('Not Due To Cost', owner_all_cimd$Surrender.Reason)|
                                   grepl('Constable Visit', owner_all_cimd$Surrender.Reason)]<-"Personal Issues"

owner_all_cimd$surrender.category[grepl('Too Many', owner_all_cimd$Surrender.Reason)|
                                   owner_all_cimd$Surrender.Reason == "Unwanted Litter"]<-"Too Many"

owner_all_cimd$surrender.category[owner_all_cimd$Surrender.Reason == "Other"]<-"Other"

#SPECIES
owner_all_cimd$species[owner_all_cimd$Type == "Dog"]<- "Dog"
owner_all_cimd$species[owner_all_cimd$Type == "Puppy"]<- "Puppy"
owner_all_cimd$species[owner_all_cimd$Type == "Cat"]<- "Cat"
owner_all_cimd$species[owner_all_cimd$Type == "Kitten"]<- "Kitten"
owner_all_cimd$species[owner_all_cimd$Type == "Rabbit" ]<- "Rabbit"
owner_all_cimd$species[grepl('Small Animal', owner_all_cimd$Type)]<- "Small Animal"
owner_all_cimd$species[grepl('Farm Animal', owner_all_cimd$Type)]<- "Farm Animal"
owner_all_cimd$species[grepl('Exotic', owner_all_cimd$Type)]<- "Exotic"
owner_all_cimd$species <- as.factor(owner_all_cimd$species)
is.factor(owner_all_cimd$species)

# SPAY NEUTER
owner_all_cimd$Incoming.Spayed...Neutered.Status[owner_all_cimd$Incoming.Spayed...Neutered.Status == "Unknown"|
                                                owner_all_cimd$Incoming.Spayed...Neutered.Status == "No"]<- "0" #make all unknown spay neuter into NO 
owner_all_cimd$Incoming.Spayed...Neutered.Status[owner_all_cimd$Incoming.Spayed...Neutered.Status == "Yes"]<- "1" #make all yes into 1
owner_all_cimd$Incoming.Spayed...Neutered.Status <- factor(owner_all_cimd$Incoming.Spayed...Neutered.Status)#binomial data as factor

##########################################################################                Data for ALL OF BRITISH COLUMBIA

#Surrender Categories
owner_all_cimd$surrender.category <-as.factor(owner_all_cimd$surrender.category)
owner_all_cimd$surrender.category <- relevel(owner_all_cimd$surrender.category, ref = "Too Many") 
multi_surrender <- multinom(surrender.category ~ EC_score + SC_score + ED_score + RI_score, data = owner_all_cimd)
summary(multi_surrender)
z_surrender <-summary(multi_surrender)$coefficients/summary(multi_surrender)$standard.errors
p_surrender<- (1 - pnorm(abs(z_surrender), 0, 1))*2
coef_multi_surrender<- coef(multi_surrender)
bc_surrender<-exp(coef_multi_surrender) 
bc_surrender_ci<-exp(confint(multi_surrender))

#Species
owner_all_cimd$species <- relevel(owner_all_cimd$species, ref = "Cat")
multi_species <- multinom(species ~ EC_score + SC_score + ED_score + RI_score, data = owner_all_cimd)
summary(multi_species)
coef_multi_species<- coef(multi_species)
bc_species<-exp(coef_multi_species)
z_species <- summary(multi_species)$coefficients/summary(multi_species)$standard.errors
z_species
p_species <- (1 - pnorm(abs(z_species), 0, 1))*2
p_species
bc_species_ci <-exp(confint(multi_species))

#Spay/Neuter Status upon Intake
owner_all_dogcat <- subset(owner_all_cimd, Type== 'Dog'|Type== 'Puppy'|Type== 'Cat'|Type== 'Kitten')
summary(owner_all_dogcat$Incoming.Spayed...Neutered.Status)
owner_all_spay <- glm(Incoming.Spayed...Neutered.Status ~ EC_score + SC_score + ED_score + RI_score, data = owner_all_dogcat, family = "binomial")
summary(owner_all_spay)
exp(cbind(OR = coef(owner_all_spay), confint(owner_all_spay)))

#Pit bull/ Not Pit bull
owner_all_dogs <- subset(owner_all_cimd, Type== 'Dog'|Type== 'Puppy')
owner_all_dogs$pit <- ifelse(grepl("Pit Bull", owner_all_dogs$Breed), "1", "0")
owner_all_dogs$pit <- factor(owner_all_dogs$pit)
summary(owner_all_dogs$pit)
owner_all_pit <- glm(pit ~ EC_score + SC_score + ED_score + RI_score, data = owner_all_dogs, family = "binomial")
summary(owner_all_pit)
exp(cbind(OR = coef(owner_all_pit), confint(owner_all_pit)))

#Health (Asilomar Accords)
owner_all_cimd$eval <-as.factor(owner_all_cimd$Intake.Eval.Category)
owner_all_cimd$eval <- relevel(owner_all_cimd$eval, ref = "Healthy")
summary(owner_all_cimd$eval)
multi_eval_all <- multinom(eval ~ EC_score + SC_score + ED_score + RI_score, data = owner_all_cimd)
summary(multi_eval_all)
coef_multi_eval_all<- coef(multi_eval_all)
bc_eval<-exp(coef_multi_eval_all)
(exp(coef_multi_eval_all)-1)*100
z_eval_all <- summary(multi_eval_all)$coefficients/summary(multi_eval_all)$standard.errors
z_eval_all
p_eval_all <- (1 - pnorm(abs(z_eval_all), 0, 1))*2
p_eval_all
bc_eval_ci <-exp(confint(multi_eval_all))


##########################################################################        Data for VANCOUVER AREA

vancouver_all_cimd <- subset(owner_all_cimd, lat >=49.27 & lat <= 49.28 & long >= -123.3 & long <=-122.8, 
                          select=c(1:55))

metrovan_all_cimd <- subset(owner_all_cimd, lat >=49.00 & lat <= 49.30 & long >= -123.31 & long <= -122.46,
                             select=c(1:55))

#Surrender Reasons
metrovan_all_cimd$surrender.category <-as.factor(metrovan_all_cimd$surrender.category)
metrovan_all_cimd$surrender.category <- relevel(metrovan_all_cimd$surrender.category, ref = "Too Many") 
summary(metrovan_all_cimd$surrender.category)
multi_surrender_all <- multinom(surrender.category ~ EC_score + SC_score + ED_score + RI_score, data = metrovan_all_cimd)
summary(multi_surrender_all) 
z_surrender_all <-summary(multi_surrender_all)$coefficients/summary(multi_surrender_all)$standard.errors
z_surrender_all 
p_surrender_all <- (1 - pnorm(abs(z_surrender_all), 0, 1))*2
p_surrender_all
coef_multi_surrender_all <- coef(multi_surrender_all)
van_surrender<-exp(coef_multi_surrender_all)
van_surrender_ci<-exp(confint(multi_surrender_all))

#Species
metrovan_all_cimd$species <- relevel(metrovan_all_cimd$species, ref = "Cat")
metrovan_species_all <- multinom(species ~ EC_score + SC_score + ED_score + RI_score, data = metrovan_all_cimd)
summary(metrovan_species_all)
coef_species_all<- coef(metrovan_species_all)
van_species<-exp(coef_species_all)
(exp(coef_species_all)-1)*100
z_species_all <- summary(metrovan_species_all)$coefficients/summary(metrovan_species_all)$standard.errors
z_species_all
p_species_all <- (1 - pnorm(abs(z_species_all), 0, 1))*2
p_species_all
ci_species_all <-confint(species_all)
ci_species_all
exp(cbind(OR = coef(species_all), confint(species_all)))

#Spay/Neuter Status upon Intake
metrovan_all_dogcat <- subset(metrovan_all_cimd, Type== 'Dog'|Type== 'Puppy'|Type== 'Cat'|Type== 'Kitten')
summary(metrovan_all_dogcat$Incoming.Spayed...Neutered.Status)
metrovan_all_spay <- glm(Incoming.Spayed...Neutered.Status ~ EC_score + SC_score + ED_score + RI_score, data = metrovan_all_dogcat, family = "binomial")
summary(metrovan_all_spay)
metrovan_spay<-exp(cbind(OR = coef(metrovan_all_spay), confint(metrovan_all_spay)))


#Pit bull/Not Pit bull
metrovan_all_dogs <- subset(metrovan_all_cimd, Type== 'Dog'|Type== 'Puppy')
metrovan_all_dogs$pit <- ifelse(grepl("Pit Bull", metrovan_all_dogs$Breed), "1", "0")
metrovan_all_dogs$pit <- factor(metrovan_all_dogs$pit)
summary(metrovan_all_dogs$pit)
metrovan_all_pit <- glm(pit ~ EC_score + SC_score + ED_score + RI_score, data = metrovan_all_dogs, family = "binomial")
summary(metrovan_all_pit)
exp(cbind(OR = coef(metrovan_all_pit), confint(metrovan_all_pit)))

#Health (Asilomar Accords)
metrovan_all_cimd$eval <-as.factor(metrovan_all_cimd$Intake.Eval.Category)
metrovan_all_cimd$eval <- relevel(metrovan_all_cimd$eval, ref = "Healthy")
summary(metrovan_all_cimd$eval)
multi_eval_all <- multinom(eval ~ EC_score + SC_score + ED_score + RI_score, data = metrovan_all_cimd)
summary(multi_eval_all)
coef_multi_eval_all<- coef(multi_eval_all)
exp(coef_multi_eval_all)
z_eval_all <- summary(multi_eval_all)$coefficients/summary(multi_eval_all)$standard.errors
z_eval_all
p_eval_all <- (1 - pnorm(abs(z_eval_all), 0, 1))*2
p_eval_all

#########################################################################       Data for KAMLOOPS AREA

kamloops_all_cimd <- subset(owner_all_cimd, lat >= 50.20 & lat <= 51.00 & long >= -121.50 & long <= -119.00,
                           select=c(1:55)) # 2665 obs

#Surrender Reasons
kamloops_all_cimd$surrender.category <-as.factor(kamloops_all_cimd$surrender.category)
kamloops_all_cimd$surrender.category <- relevel(kamloops_all_cimd$surrender.category, ref = "Too Many") 
summary(kamloops_all_cimd$surrender.category)
multi_surrender_all <- multinom(surrender.category ~ EC_score + SC_score + ED_score + RI_score, data = kamloops_all_cimd)
summary(multi_surrender_all) 
z_surrender_all <-summary(multi_surrender_all)$coefficients/summary(multi_surrender_all)$standard.errors
z_surrender_all 
p_surrender_all <- (1 - pnorm(abs(z_surrender_all), 0, 1))*2
p_surrender_all
coef_multi_surrender_all <- coef(multi_surrender_all)
kamloops_surrender<-exp(coef_multi_surrender_all)
kamloops_surrender_ci<-exp(confint(multi_surrender_all))

#Species
summary(kamloops_all_cimd$species)
kamloops_all_cimd$species <- relevel(kamloops_all_cimd$species, ref = "Cat")
kamloops_species_all <- multinom(species ~ EC_score + SC_score + ED_score + RI_score, data = kamloops_all_cimd)
summary(kamloops_species_all)
coef_species_all<- coef(kamloops_species_all)
kamloops_species<-exp(coef_species_all)
(exp(coef_species_all)-1)*100
z_species_all <- summary(kamloops_species_all)$coefficients/summary(kamloops_species_all)$standard.errors
z_species_all
p_species_all <- (1 - pnorm(abs(z_species_all), 0, 1))*2
p_species_all
ci_species_all <-confint(species_all)
ci_species_all
kamloops_species_ci<-exp(confint(kamloops_species_all))

#Spay/Neuter Status upon Intake
kamloops_all_dogcat <- subset(kamloops_all_cimd, Type== 'Dog'|Type== 'Puppy'|Type== 'Cat'|Type== 'Kitten')
summary(kamloops_all_dogcat$Incoming.Spayed...Neutered.Status)
kamloops_all_spay <- glm(Incoming.Spayed...Neutered.Status ~ EC_score + SC_score + ED_score + RI_score, data = kamloops_all_dogcat, family = "binomial")
summary(kamloops_all_spay)
exp(cbind(OR = coef(kamloops_all_spay), confint(kamloops_all_spay)))

#Pit bull/ Not Pit bull
kamloops_all_dogs <- subset(kamloops_all_cimd, Type== 'Dog'|Type== 'Puppy')
kamloops_all_dogs$pit <- ifelse(grepl("Pit Bull", kamloops_all_dogs$Breed), "1", "0")
kamloops_all_dogs$pit <- factor(kamloops_all_dogs$pit)
summary(kamloops_all_dogs$pit)
kamloops_all_pit <- glm(pit ~ EC_score + SC_score + ED_score + RI_score, data = kamloops_all_dogs, family = "binomial")
summary(kamloops_all_pit)
exp(cbind(OR = coef(kamloops_all_pit), confint(kamloops_all_pit)))

#Health (Asilomar Accords)
kamloops_all_cimd$eval <-as.factor(kamloops_all_cimd$Intake.Eval.Category)
kamloops_all_cimd$eval <- relevel(kamloops_all_cimd$eval, ref = "Healthy")
summary(kamloops_all_cimd$eval)
multi_eval_all <- multinom(eval ~ EC_score + SC_score + ED_score + RI_score, data = kamloops_all_cimd)
summary(multi_eval_all)
coef_multi_eval_all<- coef(multi_eval_all)
exp(coef_multi_eval_all)
z_eval_all <- summary(multi_eval_all)$coefficients/summary(multi_eval_all)$standard.errors
z_eval_all
p_eval_all <- (1 - pnorm(abs(z_eval_all), 0, 1))*2
p_eval_all
kamloops_eval<-exp(coef_multi_eval_all) #relative risk ratio
kamloops_eval_ci<-exp(confint(multi_eval_all))