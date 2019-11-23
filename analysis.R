library(tidyverse)
library(rethinking)
library(ggridges)

# Read in subsistence skill data (tacit and explicit knoweldge) for both pops
d <- read.csv("skill_data.csv", stringsAsFactors = F)

#### Data dictionary ##############################################
## skill: subsistence domain, e.g., digging tuber
## ID: Participant identifier
## age: ethnographer estimated age
## sex: female = 0, male = 1
## culture: Hadza/BaYaka
## y: outcome, either binary endorsement or free-list count
## method: if endorsed, how did they learn (tacit only)
## sex_pathway: if observation or teaching, which sex learned from
## from: what pathway of learning (horizontal, oblique, vertical)
## animal: species identifier for animal identification 
## plant: species identifier for plant identification 
##################################################################

# Read in rank data for each pop, each row is an indvidual's forced-choice rankings
BaYaka_rank <- read.csv("BaYaka_rank_data.csv", stringsAsFactors = F)
Hadza_rank <- read.csv("Hadza_rank_data.csv", stringsAsFactors = F)

# Now, indicate whether the subsistence outcome is tacit or explicit
d$expl <- ifelse(substr(d$skill, 1, 1) %in% c("B", "H"), 1, 0)

# Indicate whether it subsistence outcome was a free-list
d$freelist <- ifelse(d$skill %in% c("B_honeysum", "B_basketvine", "B_climbvine", "B_trapsum", "B_gunsum", "B_spearsum", "H_honeysum", "H_bowsum"), 1, 0)

# Unique identifier for each freelist activity
d$freelist_id <- match(d$skill, unique(d$skill[d$freelist == 1]))

# Now, we need to create indices for individuals, skills, and species. The first step is to seperate the data by society, because we'll estimate different variances for Hadza and BaYaka

##### BaYaka ##########################
d_B <- subset(d, d$culture == "BaYaka")
d_B$id <- match(d_B$ID, unique(d_B$ID))
d_B$skill_id <- match(d_B$skill, unique(d_B$skill))
d_B$animal_id <- ifelse( is.na(d_B$animal), NA, match(d_B$animal, unique(d_B$animal)) ) - 1
d_B$plant_id <- ifelse( is.na(d_B$plant), NA, match(d_B$plant, unique(d_B$plant)) ) - 1

N_B <- max(d_B$id, na.rm=T)
N_skillB <- max(d_B$skill_id, na.rm=T)
N_animalB <- max(d_B$animal_id, na.rm=T)
N_plantB <- max(d_B$plant_id, na.rm=T)

# Putting rank data in same order as skill index
Bskills <- d_B %>% group_by(skill) %>% summarise(ind = mean(skill_id))
Bskills <- Bskills[Bskills$ind < 20,]
BaYaka_rank <- BaYaka_rank[,Bskills$skill]

N_BR <- nrow(BaYaka_rank)

BaYaka_rank <- BaYaka_rank / max(BaYaka_rank) # scale by max rank

# Error for age estimates, standardizing by 80 years
age_idB <- d_B %>% group_by(id) %>% summarise(age = mean(age))

age_muB <- ifelse( age_idB$age <= 20, age_idB$age + 0.5 , age_idB$age ) / 80
age_seB <- ifelse( age_idB$age <= 20, 0.5 , 5 ) / 80

##### Hadza ##########################
d_H <- subset(d, d$culture == "Hadza")
d_H$id <- match(d_H$ID, unique(d_H$ID))
d_H$skill_id <- match(d_H$skill, unique(d_H$skill))
d_H$animal_id <- ifelse( is.na(d_H$animal), NA, match(d_H$animal, unique(d_H$animal)) ) - 1 
d_H$plant_id <- ifelse( is.na(d_H$plant), NA, match(d_H$plant, unique(d_H$plant)) ) - 1

N_H <- max(d_H$id, na.rm=T)
N_skillH <- max(d_H$skill_id, na.rm=T)
N_animalH <- max(d_H$animal_id, na.rm=T)
N_plantH <- max(d_H$plant_id, na.rm=T)

# Putting rank data in same order as skill index
Hskills <- d_H %>% group_by(skill) %>% summarise(ind = mean(skill_id))
Hskills <- Hskills[Hskills$ind < 13,]
Hadza_rank <- Hadza_rank[,Hskills$skill]

Hadza_rank <- Hadza_rank / max(Hadza_rank) # scale by max rank
N_HR <- nrow(Hadza_rank)

# Error for age estimates, standardizing by 80 years
age_idH <- d_H %>% group_by(id) %>% summarise(age = mean(age))

age_muH <- ifelse( age_idH$age <= 20, age_idH$age + 0.5 , age_idH$age ) / 80
age_seH <- ifelse( age_idH$age <= 20, 0.5 , 5 ) / 80

####### Putting dataframes backtogether ####
d <- rbind(d_H, d_B)

## Creating index for transmission method
d$transmission <- ifelse(d$method == "Individual", 3, d$method)
d$transmission <- ifelse(d$method == "Observation", 1, d$transmission)
d$transmission <- ifelse(d$method == "Teaching", 2, d$transmission)
d$transmission <- ifelse(is.na(d$method), -99, d$transmission)

## Creating index for transmission pathway
d$path <- ifelse(d$from == "Vertical", 3, d$from)
d$path <- ifelse(d$from == "Horizontal", 1, d$path)
d$path <- ifelse(d$from == "Oblique", 2, d$path)
d$path <- ifelse(is.na(d$from), -99, d$path)

##### Creating index for sex
d$sex_learn <- ifelse(d$sex_pathway == "Female", 1, d$sex_pathway)
d$sex_learn <- ifelse(d$sex_pathway == "Male", 2, d$sex_learn)
d$sex_learn <- ifelse(d$sex_pathway == "Both Sexes", 3, d$sex_learn)
d$sex_learn <- ifelse(is.na(d$sex_learn), -99, d$sex_learn)

# Sex * culture interaction
sexcult <- ifelse(d$sex == 0 & d$culture == "Hadza", 1, NA)
sexcult <- ifelse(d$sex == 1 & d$culture == "Hadza", 2, sexcult)
sexcult <- ifelse(d$sex == 0 & d$culture == "BaYaka", 3, sexcult)
sexcult <- ifelse(d$sex == 1 & d$culture == "BaYaka", 4, sexcult)

# We'll substitute NAs for -99, because Stan will not accept NAs in data.
d[is.na(d)] <- -99

# Putting all the model data in a list
data_list <- list(
  N_obs = nrow(d),
  N_H = N_H,
  N_B = N_B,
  N_HR = N_HR,
  N_BR = N_BR,
  N_skillH = N_skillH,
  N_skillB = N_skillB,
  N_animalB = N_animalB,
  N_animalH = N_animalH,
  N_plantB = N_plantB,
  N_plantH = N_plantH,
  id = d$id,
  skill = d$skill_id,
  animal_id = d$animal_id,
  plant_id = d$plant_id,
  sex = d$sex,
  y = d$y,
  expl = d$expl,
  freelist = d$freelist,
  freelist_id = d$freelist_id,
  path = as.integer(d$path),
  transmission = as.integer(d$transmission),
  K_methods = max(as.integer(d$transmission)[as.integer(d$transmission) < 99]),
  K_paths = max(as.integer(d$path)[as.integer(d$path) < 99]),
  sex_learn = as.integer(d$sex_learn),
  sexcult = sexcult,
  age_muB = age_muB,
  age_seB = age_seB,
  age_muH = age_muH,
  age_seH = age_seH,
  Hadza_rank = Hadza_rank,
  BaYaka_rank = BaYaka_rank,
  Hadza = ifelse(d$culture == "Hadza", 1, 0)
)
######################################
##### Fitting stan model #############
m_cor <- stan_model( file="subsistence_model_expl_cor.stan" )
m_sub <- stan_model( file="subsistence_model_lh.stan" )
# fit_cor <- sampling( m_cor, data=data_list, init="0", chains=4, cores=4, iter=2000, control=list(adapt_delta=0.9 ) )
# fit_m <- sampling( m_sub, data=data_list, init="0", chains=4, cores=4, iter=2000, control=list(adapt_delta=0.9 ) )

# fit_cor <- readRDS("fit_cor.rds")
# fit_m <- readRDS("fit_m.rds")

# Checking mcmc diagnostics
write.csv(precis(fit_m, depth=3), "model_precis.csv")
prec <- read_csv("model_precis.csv")

prec %>% filter(Rhat > 1.05)

# Extract posterior samples
post <- extract.samples(fit_m)

#### Plot the life-course of knowledge across diff skills #####
library(viridis)
library(SDMTools)

b_seq <- seq(from=0.01, to=3, length.out=100)
plot_cols <- viridis(100, option="A")

pdf( "lifecourse_example.pdf", height=6, width=6, pointsize=12)
curve( (1 - exp(-2*x))^1, from=0, to=50/80 , ylim=c(0,1), col="white" , axes=F, xlab="Age", ylab="Variation Due to Age")

axis(1, at=c(0,0.2,0.4,0.6,0.8,1), labels=80*c(0,0.2,0.4,0.6,0.8,1))
axis(2, at=c(0,1), labels=c("Min", "Max"))
for (i in 1:100) {
  curve( (1 - exp(-2*x))^b_seq[i], from=0, to=50/80 , ylim=c(0,1), add=T, col=plot_cols[i])
}
legend.gradient(pnts=cbind( x=c(0.53,0.55,0.53,0.55), y=c(0, 0.2, 0, 0.2)), cols = plot_cols, c("0.01","3"),
                title = "", cex=0.75)
dev.off()

# Knowledge plots for each skill
pdf( "lifecourse_skill.pdf", height=6, width=6, pointsize=12 )
{
par(cex=1)
age_seq <- seq(from=0, to=50/80, length.out = 75)
plot(NULL, xlim=c(0,50/80), ylim=c(0,1), axes=F, xlab="Age", ylab="Variation Due to Age")
axis(1, at=c(0,0.2,0.4,0.6,0.8,1), labels=80*c(0,0.2,0.4,0.6,0.8,1))
axis(2, at=c(0,1), labels=c("Min", "Max"))
legend(x=0.4, y=0.3, legend=c("BaYaka Skill", "Hadza Skill"), lty="solid", col=c("seagreen", "cornflowerblue"), lwd=3, bty='n')

for (s in 1:N_skillH) {
  k <- exp(post$ak + post$skillH_v[,s,1])
  b <- exp(post$ab + post$skillH_v[,s,1])
  
  preds <- matrix(NA, nrow=length(post$lp__), ncol=length(age_seq))
  
  for (i in 1:nrow(preds)) preds[i,] <- (1 - exp(-k[i]*age_seq)^b[i])
  lines(x=age_seq, y=apply(preds,2,median), col=col.alpha("cornflowerblue", 0.8))
}

for (s in 1:N_skillB) {
  k <- exp(post$ak + post$skillB_v[,s,1])
  b <- exp(post$ab + post$skillB_v[,s,1])
  
  preds <- matrix(NA, nrow=length(post$lp__), ncol=length(age_seq))
  
  for (i in 1:nrow(preds)) preds[i,] <- (1 - exp(-k[i]*age_seq)^b[i])
  lines(x=age_seq, y=apply(preds,2,median), col=col.alpha("seagreen", 0.8))
}
dev.off()
}

###### Variation acrss skills for different parameters ###
sd_skillB <- post$sigma_skillB
sd_skillH <- post$sigma_skillH
colnames(sd_skillB) <- c("k", "b", "eta", "p", "p_male", "")

###### Correlations between learning parameters ####
cor_rankB <- post$Rho_skillB[,-11,11]
cor_rankH <- post$Rho_skillH[,-11,11]

colnames(cor_rankB) <- c("k", "b", "eta", "p", "p_male", "horiz_trans", "oblique_trans", "obs_learn", "teach_learn", "male_trans")
colnames(cor_rankH) <- colnames(cor_rankB)

cor_rankBlong <- as.data.frame(cor_rankB) %>% gather(key="var", value="cor")
cor_rankHlong <- as.data.frame(cor_rankH) %>% gather(key="var", value="cor")

cor_ranks_long <- rbind(cor_rankBlong, cor_rankHlong)
cor_ranks_long$culture <- rep(c("BaYaka", "Hadza"), each=length(post$lp__)*10)

ggplot(cor_ranks_long, aes(x=cor, y=var)) + geom_density_ridges2(aes( color=culture, fill=culture), alpha=0.5, scale=0.9, rel_min_height=0.01) + theme_bw(base_size = 16) + scale_y_discrete(expand = c(0, 0)) +
  scale_fill_manual(values=c("seagreen", "cornflowerblue")) +
  scale_color_manual(values=c(NA, NA)) +
  labs(fill="Culture", color="Culture") + ylab("") + xlab("Correlation with Skill Difficulty Ranking") + scale_x_continuous(limits=c(-1,1)) + geom_vline(xintercept = 0, lty="dashed", color="darkred", lwd=1)

###### Sex-biased transmission (obs or teaching) ####
pr_fB <- 1 - logistic(post$as + post$sexcult_v[,3,6])
pr_mB <- 1 - logistic(post$as + post$sexcult_v[,4,6])
pr_fH <- 1 - logistic(post$as + post$sexcult_v[,1,6])
pr_mH <- 1 - logistic(post$as + post$sexcult_v[,2,6])
bothB <- (pr_mB + pr_fB) / 2 
bothH <- (pr_mH + pr_fH) / 2 

trans_df <- data.frame(
  prob = c(pr_fB, pr_mB, pr_fH, pr_mH, bothB, bothH),
  learner_sex = c( rep("Female", length(pr_fB)), rep("Male", length(pr_fB)), rep("Female", length(pr_fB)), rep("Male", length(pr_fB)), rep("Both", length(pr_fB)*2) ),
  culture = c( rep("BaYaka", length(pr_fB)*2), rep("Hadza", length(pr_fB)*2), rep("BaYaka", length(pr_fB)), rep("Hadza", length(pr_fB)) )
)

pdf( "sexbias_trans1.pdf", height=5, width=8.5, pointsize=12 )
ggplot(trans_df, aes(x=prob)) +
  facet_wrap(~culture) + geom_density(aes(color=learner_sex, fill=learner_sex, alpha=learner_sex, linetype=learner_sex)) +
  theme_bw(base_size = 16) +
  scale_y_discrete(expand = c(0, 0)) +
  ylab("") + xlab("Probability of Female Transmission") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(fill="white", color="black")) +
  scale_fill_manual(values=c("darkred", "slategray", "orange")) +
  scale_color_manual(values=c("darkred", NA, NA)) +
  scale_alpha_manual(values=c(0, 0.6, 0.6)) +
  scale_linetype_manual(values=c("dashed", "solid", "solid")) + 
  labs(fill="Learner Sex", color="Learner Sex", alpha="Learner Sex", linetype="Learner Sex")
dev.off()
################################################
########### Explicit and tacit knowledge #######
post_cor <- extract.samples(fit_cor)

id_cor <- data.frame(
  cor = c(post_cor$Rho_idB[,1,2],post$Rho_idB[,1,2],post_cor$Rho_idH[,1,2],post$Rho_idH[,1,2] ),
  culture = c( rep("BaYaka", length(post_cor$lp__)), rep("BaYaka", length(post$lp__)), rep("Hadza", length(post_cor$lp__)), rep("Hadza", length(post$lp__)) ),
  model = c( rep("Correlation", length(post_cor$lp__)), rep("Residual Correlation", length(post$lp__)), rep("Correlation", length(post_cor$lp__)), rep("Residual Correlation", length(post$lp__)) )
)

pdf( "id_cor.pdf", height=5, width=8.5, pointsize=12 )
ggplot(id_cor, aes(x=cor)) +
  facet_wrap(~model) + geom_density(aes(color=culture, fill=culture), alpha=0.5) +
  theme_bw(base_size = 16) +
  scale_y_discrete(expand = c(0, 0)) +
  ylab("") + xlab("Correlation Among Individuals Tacit and Explicit") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(fill="white", color="black")) +
  scale_x_continuous(limits=c(-1,1)) +
  scale_fill_manual(values=c("seagreen", "cornflowerblue")) +
  scale_color_manual(values=c("seagreen", "cornflowerblue")) + 
  labs(fill="Culture", color="Culture") + geom_vline(xintercept = 0, linetype="dashed", lwd=1, color="darkred")
dev.off()

#################################################
########## Pace-of-learning results #############
# We'll make predictions based on skill, using the names in these relational dataframes
Hskills <- d_H %>% group_by(skill) %>% summarise(ind = mean(skill_id))
Bskills <- d_B %>% group_by(skill) %>% summarise(ind = mean(skill_id))

# Age sequence to predict along, 1 = 80 years
age_seq <- seq(from=0, to=1, length.out = 30)

############## Subsistence knowledge plot predict function ###########################
skill_plot <- function( skill, culture, sex, plot.data=T, PI=0.9, quantiles=T, add=F, color="darkorange") {
  
  preds <- matrix(NA, nrow=length(post$lp__), ncol=length(age_seq))
  expl <- ifelse( substr(skill, 1, 1) != "X", 1, 0)
  sex_d <- ifelse(sex == "Male", 1, 0)
  
  # Assiging sex*culture parameter
  if (sex == "Female" & culture == "Hadza") sexcult <- 1
  if (sex == "Male" & culture == "Hadza") sexcult <- 2
  if (sex == "Female" & culture == "BaYaka") sexcult <- 3
  if (sex == "Male" & culture == "BaYaka") sexcult <- 4
  
  if (culture == "Hadza") {
  skill_id <- Hskills[ Hskills$skill == skill , ]$ind
  freelist <- ifelse( skill %in% c("H_honeysum", "H_bowsum"), 1, 0)
    
  k <- exp(post$ak + post$skillH_v[,skill_id,1])
  b <- exp(post$ab + post$skillH_v[,skill_id,2])
  eta <- exp(post$ae + post$skillH_v[,skill_id,3] + post$e_expl*expl )
  
  stem <- post$ap + post$a_freelist*freelist + post$p_expl*expl + post$sexcult_v[,sexcult,1] + post$skillH_v[,skill_id,4] + post$skillH_v[,skill_id,5]*sex_d
  }
  
  if (culture == "BaYaka") {
    skill_id <- Bskills[ Bskills$skill == skill , ]$ind
    freelist <- ifelse( skill %in% c("B_basketvine", "B_climbvine", "B_gunsum", "B_honeysum", "B_spearsum", "B_trapsum"), 1, 0)
    
    k <- exp(post$ak + post$skillB_v[,skill_id,1])
    b <- exp(post$ab + post$skillB_v[,skill_id,2])
    eta <- exp(post$ae + post$skillB_v[,skill_id,3] + post$e_expl*expl)
    
    stem <- post$ap + post$a_freelist*freelist + post$p_expl*expl + post$sexcult_v[,sexcult,1] + post$skillB_v[,skill_id,4] + post$skillB_v[,skill_id,5]*sex_d
  }
  
  for (j in 1:ncol(preds)) {
    if (freelist == 0) preds[,j] <- 2 * ( logistic(exp( (1 - exp(-k*age_seq[j])^b )*eta + stem ) ) - 0.5 )
    if (freelist == 1) preds[,j] <- exp( (1 - exp(-k*age_seq[j])^b )*eta + stem )
  }
  
  quants <- PI/4
  
  PI_1 <- apply(preds, 2, PI, prob=quants)
  PI_2 <- apply(preds, 2, PI, prob=quants*2)
  PI_3 <- apply(preds, 2, PI, prob=quants*3)
  PI_4 <- apply(preds, 2, PI, prob=quants*4)
  med <- apply(preds, 2, median)
  
  # Initializing new plot if needed
  if (add == F) {
  if (freelist==0 & !(substr(skill,1,3) %in% c("B_a", "H_a", "B_p", "H_p"))) plot(NULL, xlim=c(0,1), ylim=c(-0.05,1.05), xlab="Age", ylab="Pr(Endorse Skill)", axes=F)
  if (freelist==0 & substr(skill,1,3) %in% c("B_a", "H_a", "B_p", "H_p")) plot(NULL, xlim=c(-0.05,1.05), ylim=c(-0.05,1.05), xlab="Age", ylab="Pr (Identify Species)",axes=F)
    
  if (freelist==1) {
    plot(NULL, xlim=c(0,1), ylim=c(0,PI(preds, 0.97)[2]), xlab="Age", ylab="Freelist Sum", axes=F)
    axis(2, at=round(seq(from=0, to=PI(preds, 0.97)[2], length.out = 4)))
  }
    else axis(2, at=c(0, 0.5, 1))
    
  axis(1, at=c(0,0.2,0.4,0.6,0.8,1), labels=80*c(0,0.2,0.4,0.6,0.8,1))
  mtext(skill, line=0.5, cex=1.1)
  }
  
  if (quantiles == T) {
  shade(PI_4, age_seq, col=col.alpha(color, 0.1))
  shade(PI_3, age_seq, col=col.alpha(color, 0.1))
  shade(PI_2, age_seq, col=col.alpha(color, 0.1))
  shade(PI_1, age_seq, col=col.alpha(color, 0.1))
  }
  else shade(PI_4, age_seq, col=col.alpha(color, 0.2))
  
  lines(x=age_seq, y=med, col=color, lwd=2)
  
  ## Adding raw data
  skill_name <- skill
  raw_d <- d %>% filter(skill==skill_name, sex==sex_d)
  
  if (freelist == 0) {
  raw_d$y <- ifelse(raw_d$y == 1, 1.05, raw_d$y)
  raw_d$y <- ifelse(raw_d$y == 0, -0.05, raw_d$y)
  }
  
  if (plot.data == T) {
    if (substr(skill,1,3) %in% c("B_a", "H_a", "B_p", "H_p")) points(x=jitter(raw_d$age/80, amount=2/80), y=raw_d$y, col=col.alpha(color, 0.05), pch=16)
    
    else points(x=jitter(raw_d$age/80, amount=2/80), y=raw_d$y, col=col.alpha(color, 0.4), pch=16)
  }
}
############# End plot predict function #####################
#############################################################

skill_plot(skill="B_animal", culture="BaYaka", sex="Male")

####### Plotting all BaYaka Skills ######
pdf("BaYaka_skills_1.pdf", 
    width=8.5, 
    height=11, 
    pointsize=12)

par(mfrow=c(3,3), cex=0.9)
for (s in 1:9) {
  skill_plot(skill=Bskills$skill[s], culture="BaYaka", sex="Male", color="orange", quantiles = T)
  skill_plot(skill=Bskills$skill[s], culture="BaYaka", sex="Female", color="slategray", add=T, quantiles = T)
}
dev.off()

pdf("BaYaka_skills_2.pdf", 
    width=8.5, 
    height=11, 
    pointsize=12)

par(mfrow=c(3,3), cex=0.9)
for (s in 10:18) {
  skill_plot(skill=Bskills$skill[s], culture="BaYaka", sex="Male", color="orange", quantiles = T)
  skill_plot(skill=Bskills$skill[s], culture="BaYaka", sex="Female", color="slategray", add=T, quantiles = T)
}
dev.off()

pdf("BaYaka_skills_3.pdf", 
    width=8.5, 
    height=11, 
    pointsize=12)

par(mfrow=c(3,3), cex=1)
for (s in 19:27) {
  skill_plot(skill=Bskills$skill[s], culture="BaYaka", sex="Male", color="orange", quantiles = T)
  skill_plot(skill=Bskills$skill[s], culture="BaYaka", sex="Female", color="slategray", add=T, quantiles = T)
}
dev.off()

####### Plotting all Hadza Skills ######
pdf("Hadza_skills_1.pdf", 
    width=8.5, 
    height=11, 
    pointsize=12)

par(mfrow=c(3,3), cex=1)

for (s in 1:9) {
  skill_plot(skill=Hskills$skill[s], culture="Hadza", sex="Male", color="orange", quantiles = T)
  skill_plot(skill=Hskills$skill[s], culture="Hadza", sex="Female", color="slategray", add=T, quantiles = T)
}
dev.off()

pdf("Hadza_skills_2.pdf", 
    width=8.5, 
    height=11, 
    pointsize=12)

par(mfrow=c(3,3), cex=1)

for (s in 10:nrow(Hskills)) {
  skill_plot(skill=Hskills$skill[s], culture="Hadza", sex="Male", color="orange", quantiles = T)
  skill_plot(skill=Hskills$skill[s], culture="Hadza", sex="Female", color="slategray", add=T, quantiles = T)
}
dev.off()
############################################################
