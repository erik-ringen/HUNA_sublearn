library(tidyverse)

#############################################
## Main dataset
age <- read.csv("data-raw/age.csv", header=T)

#Removing the free-list skills
age <- subset( age, !(skill %in% c('X3.bow', 'X9.gun', 'X10.spear', 'X13.honey', 'X19.traps') ))

#Remvoing rank mean
age <- age %>% select(-meanz)

## Rank datasets
bfp <- read.csv("data-raw/BaYaka_forcedpair.csv", header=T)
hfp <- read.csv("data-raw/Hadza_forcedpair.csv", header=T)

## Explicit knowledge data ##################
eB <- read.csv("data-raw/BaYaka_explicit.csv")
names(eB)[1] <- "ID"

# removing summary variables
eB <- eB %>% select(-animal_sum, -plant_sum)

# Converting to long-form
eB_long <- eB %>% gather(key="species", value="endorse", -ID)

# Labelling type of explcit knowledge
eB_long$skill <- ifelse(substr(eB_long$species, 1, 2) == "an", "B_animal", NA)
eB_long$skill <- ifelse(substr(eB_long$species, 1, 2) == "pl", "B_plant", eB_long$skill)
eB_long$skill <- ifelse(substr(eB_long$species, 1, 3) == "X18", "B_climbvine", eB_long$skill)
eB_long$skill <- ifelse(substr(eB_long$species, 1, 2) == "X4", "B_basketvine", eB_long$skill)
eB_long$skill <- ifelse(substr(eB_long$species, 1, 2) == "X9", "B_gunsum", eB_long$skill)
eB_long$skill <- ifelse(substr(eB_long$species, 1, 3) == "X10", "B_spearsum", eB_long$skill)
eB_long$skill <- ifelse(substr(eB_long$species, 1, 3) == "X19", "B_trapsum", eB_long$skill)
eB_long$skill <- ifelse(substr(eB_long$species, 1, 3) == "X24", "B_honeysum", eB_long$skill)

table(eB_long$skill)

# Now give species labels
eB_long$animal <- ifelse(eB_long$skill == "B_animal", substr(eB_long$species, 10, nchar(eB_long$species)), NA)
eB_long$plant <- ifelse(eB_long$skill == "B_plant", substr(eB_long$species, 9, nchar(eB_long$species)), NA)

# Now for Hadza
eH <- read.csv("data-raw/Hadza_explicit.csv")
names(eH)[1] <- "ID"

# removing summary variables
eH <- eH %>% select(-animal_sum, -plant_sum)

# Modifying Hadza ids to be consistent with other datasets
eH$ID <- as.integer(paste0(1, eH$ID))

# Converting to long-form
eH_long <- eH %>% gather(key="species", value="endorse", -ID)

eH_long$skill <- ifelse(substr(eH_long$species, 1, 2) == "an", "H_animal", NA)
eH_long$skill <- ifelse(substr(eH_long$species, 1, 2) == "pl", "H_plant", eH_long$skill)
eH_long$skill <- ifelse(substr(eH_long$species, 1, 2) == "X3", "H_bowsum", eH_long$skill)
eH_long$skill <- ifelse(substr(eH_long$species, 1, 2) == "ho", "H_honeysum", eH_long$skill)

# Species labels
eH_long$animal <- ifelse(eH_long$skill == "H_animal", substr(eH_long$species, 10, nchar(eH_long$species)), NA)
eH_long$plant <- ifelse(eH_long$skill == "H_plant", substr(eH_long$species, 9, nchar(eH_long$species)), NA)

### Adding sex, age, and culture variables to explicit dfs
eH_long$culture <- rep("Hadza", nrow(eH_long))
eB_long$culture <- rep("BaYaka", nrow(eB_long))

age_df <- age %>% group_by(ID) %>% summarise(age=mean(age))
sex_df <- age %>% group_by(ID) %>% summarise(sex=mean(sex))

eH_long <- merge(eH_long, age_df, by="ID")
eH_long <- merge(eH_long, sex_df, by="ID")

eB_long <- merge(eB_long, age_df, by="ID")
eB_long <- merge(eB_long, sex_df, by="ID")

##And then the datasets for the mechanism (by) pathway (from) and same-sex teaching (sex)
by<-read.csv("data-raw/by.csv", header=T)
from<-read.csv("data-raw/from.csv", header=T)
sex<-read.csv("data-raw/sex.csv", header=T)

# Bring them all together. We need to make sure that skill has the same names across dfs
by$skill <- as.character(by$skill)
by$skill <- substr(by$skill, 1, nchar(by$skill)-3)
sex$skill <- as.character(sex$skill)
sex$skill <- substr(sex$skill, 1, nchar(sex$skill)-5)
from$skill <- as.character(from$skill)
from$skill <- substr(from$skill, 1, nchar(from$skill)-5)

# Matching transmission method, path and transmitter sex to endorsement
endorse_learn <- age

endorse_learn$method <- by$method[match( interaction(age$skill,age$ID), interaction(by$skill,by$ID) )]
endorse_learn$from <- from$Pathway[match( interaction(age$skill, age$ID), interaction(from$skill, from$ID))]
endorse_learn$sex_pathway <- sex$sex_pathway[match( interaction(age$skill,age$ID), interaction(sex$skill,sex$ID) )]

endorse_learn <- bind_rows(endorse_learn, eB_long)
endorse_learn <- bind_rows(endorse_learn, eH_long)

# Renaming outcome
endorse_learn <- endorse_learn %>% rename(y = endorse)

# Removing extraneous species var
endorse_learn <- endorse_learn %>% select(-species)

####### Wrangling rank data ######
bfp_t <- t(bfp[,-ncol(bfp)])
colnames(bfp_t) <- bfp_t[1,]
bfp_t <- bfp_t[-1,]

bfp_t <- apply(bfp_t, 2, as.numeric)

hfp_t <- t(hfp[,-ncol(hfp)])
colnames(hfp_t) <- hfp_t[1,]
hfp_t <- hfp_t[-1,]

hfp_t <- apply(hfp_t, 2, as.numeric)

### Removing the plant questions that were not the same for BaYaka
endorse_learn <- subset(endorse_learn, !is.na(endorse_learn$y))

####### Exporting data for analysis ###
write_csv(endorse_learn, "skill_data.csv")
write_csv(as.data.frame(bfp_t), "BaYaka_rank_data.csv")
write_csv(as.data.frame(hfp_t), "Hadza_rank_data.csv")
