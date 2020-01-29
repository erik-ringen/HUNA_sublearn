library(tidyverse)
library(scales)
library(rethinking)
library(ggridges)
library(ggtern)

# Read in subsistence skill data (tacit and explicit knoweldge) for both pops
d <- read.csv("skill_data.csv", stringsAsFactors = F)

#### Data dictionary ####
## skill: subsistence domain, e.g., digging tuber
## skill2: alt identifier for the explicit tasks
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

# Read in rank data for each pop, each row is an indvidual's forced-choice rankings
BaYaka_rank <- read.csv("BaYaka_rank_data.csv", stringsAsFactors = F)
Hadza_rank <- read.csv("Hadza_rank_data.csv", stringsAsFactors = F)

# Now, indicate whether the subsistence outcome is tacit or explicit
d$expl <- ifelse(substr(d$skill, 1, 1) %in% c("B", "H") | d$skill %in% c("X4.basketvine", "X18.climbingvine"), 1, 0)

# Indicate whether it subsistence outcome was a free-list
d$freelist <- ifelse(d$skill %in% c("X4.basketvine", "X18.climbingvine", "X19.traps", "X9.gun", "X10.spear", "X24.honey", "X3.bow", "X13.honey"), 1, 0)

# Unique identifier for each freelist activity
d$freelist_id <- match(d$skill, unique(d$skill[d$freelist == 1]))

# Now, we need to create indices for individuals, skills, and species. The first step is to seperate the data by society, because we'll estimate different variances for Hadza and BaYaka

#### BaYaka indices #####
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
Bskills <- Bskills[Bskills$skill %in% colnames(BaYaka_rank),]
BaYaka_rank <- BaYaka_rank[,Bskills$skill[order(Bskills$ind)]]

N_BR <- nrow(BaYaka_rank)

BaYaka_rank <- BaYaka_rank / max(BaYaka_rank) # scale by max rank

# Error for age estimates, standardizing by 80 years
age_idB <- d_B %>% group_by(id) %>% summarise(age = mean(age))

age_muB <- ifelse( age_idB$age <= 20, age_idB$age + 0.5 , age_idB$age ) / 80
age_seB <- ifelse( age_idB$age <= 20, 0.5 , 5 ) / 80

#### Hadza indices #####
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
Hskills <- Hskills[Hskills$skill %in% colnames(Hadza_rank),]
Hadza_rank <- Hadza_rank[,Hskills$skill[order(Hskills$ind)]]

Hadza_rank <- Hadza_rank / max(Hadza_rank) # scale by max rank
N_HR <- nrow(Hadza_rank)

# Error for age estimates, standardizing by 80 years
age_idH <- d_H %>% group_by(id) %>% summarise(age = mean(age))

age_muH <- ifelse( age_idH$age <= 20, age_idH$age + 0.5 , age_idH$age ) / 80
age_seH <- ifelse( age_idH$age <= 20, 0.5 , 5 ) / 80

# Putting dataframes backtogether
d <- rbind(d_H, d_B)

#### index for transmission method ####
d$transmission <- ifelse(d$method == "Individual", 3, d$method)
d$transmission <- ifelse(d$method == "Observation", 1, d$transmission)
d$transmission <- ifelse(d$method == "Teaching", 2, d$transmission)
d$transmission <- ifelse(is.na(d$method), -99, d$transmission)

#### index for transmission pathway ####
d$path <- ifelse(d$from == "Vertical", 3, d$from)
d$path <- ifelse(d$from == "Horizontal", 1, d$path)
d$path <- ifelse(d$from == "Oblique", 2, d$path)
d$path <- ifelse(is.na(d$from), -99, d$path)

#### index for sex ####
d$sex_learn <- ifelse(d$sex_pathway == "Female", 1, d$sex_pathway)
d$sex_learn <- ifelse(d$sex_pathway == "Male", 2, d$sex_learn)
d$sex_learn <- ifelse(d$sex_pathway == "Both Sexes", 3, d$sex_learn)
d$sex_learn <- ifelse(is.na(d$sex_learn), -99, d$sex_learn)

#### Sex * culture interaction index ####
sexcult <- ifelse(d$sex == 0 & d$culture == "Hadza", 1, NA)
sexcult <- ifelse(d$sex == 1 & d$culture == "Hadza", 2, sexcult)
sexcult <- ifelse(d$sex == 0 & d$culture == "BaYaka", 3, sexcult)
sexcult <- ifelse(d$sex == 1 & d$culture == "BaYaka", 4, sexcult)

# We'll substitute NAs for -99, because Stan will not accept NAs in data.
d[is.na(d)] <- -99

#### Organzing data into list ####
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

#### Fitting stan model ####
m_cor <- stan_model( file="subsistence_model_expl_cor.stan" )
m_sub <- stan_model( file="subsistence_model_lh.stan" )

fit_cor <- sampling( m_cor, data=data_list, init="0", chains=4, cores=4, iter=2000, control=list(adapt_delta=0.9 ) )
fit_m <- sampling( m_sub, data=data_list, init="0", chains=4, cores=4, iter=2000, control=list(adapt_delta=0.9 ) )

# If they've been fit once, I suggest saving to-reload later
# saveRDS(fit_cor, "fit_cor.rds")
# saveRDS(fit_m, "fit_m.rds")

#fit_cor <- readRDS("fit_cor.rds")
#fit_m <- readRDS("fit_m.rds")

# Checking mcmc diagnostics
write.csv(precis(fit_m, depth=3), "model_precis.csv")
prec <- read_csv("model_precis.csv")

prec %>% filter(Rhat > 1.01)

# Extract posterior samples
post <- extract.samples(fit_m)
post_cor <- extract.samples(fit_cor)

#### Plot life-course of knowledge ####
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
    b <- exp(post$ab + post$skillH_v[,s,2])
    
    preds <- matrix(NA, nrow=length(post$lp__), ncol=length(age_seq))
    
    for (i in 1:nrow(preds)) preds[i,] <- (1 - exp(-k[i]*age_seq))^b[i]
    lines(x=age_seq, y=apply(preds,2,median), col=col.alpha("cornflowerblue", 0.8))
  }
  
  for (s in 1:N_skillB) {
    k <- exp(post$ak + post$skillB_v[,s,1])
    b <- exp(post$ab + post$skillB_v[,s,2])
    
    preds <- matrix(NA, nrow=length(post$lp__), ncol=length(age_seq))
    
    for (i in 1:nrow(preds)) preds[i,] <- (1 - exp(-k[i]*age_seq))^b[i]
    lines(x=age_seq, y=apply(preds,2,median), col=col.alpha("seagreen", 0.8))
  }
  dev.off()
}

#### Knowledge at age 14 ####
# Organize skill data
Hskills <- d_H %>% group_by(skill) %>% summarise(ind = mean(skill_id))
Bskills <- d_B %>% group_by(skill) %>% summarise(ind = mean(skill_id))

# Add publication-friendly labels
pub_labels <- read_csv("figure_labels.csv")
Hskills$skill2 <- pub_labels$`Change to`[match(Hskills$skill, pub_labels$`In figure`)]
Bskills$skill2 <- pub_labels$`Change to`[match(Bskills$skill, pub_labels$`In figure`)]

# Creating predictions at age 14 for each skill
pred14 <- matrix(NA, nrow=length(post$lp__), ncol=(N_skillB + N_skillH))
for ( j in 1:(N_skillB + N_skillH) ) {
  if (j > N_skillB) {
    k <- exp(post$ak + post$skillH_v[,(j - N_skillB),1])
    b <- exp(post$ab + post$skillH_v[,(j - N_skillB),2])
  }
  else {
    k <- exp(post$ak + post$skillB_v[,j,1])
    b <- exp(post$ab + post$skillB_v[,j,2])
  }
  pred14[,j] <- (1 - exp(-k*(14/80)))^b
}
pred14 <- as.data.frame(pred14)
colnames(pred14) <- c(Bskills$skill2[order(Bskills$ind)],Hskills$skill2[order(Hskills$ind)])
# Converting wide to long
pred14_l <- pred14 %>% gather(key="skill", value="est")
pred14_l$culture <- ifelse(pred14_l$skill %in% Bskills$skill2, "BaYaka", "Hadza")

# Summarizing posterior predictions and plotting summaries
pred14_l %>% group_by(skill) %>% summarise(med=mean(est), lower=HPDI(est, prob=0.9)[1], upper=HPDI(est, prob=0.9)[2]) %>% mutate(culture=ifelse(skill %in% Bskills$skill2, "BaYaka", "Hadza")) %>% ggplot(aes(x=med,y=fct_reorder(skill, med))) + facet_wrap(~culture, scales="free_y") + geom_point(aes(color=culture), lwd=2) + geom_errorbarh(aes(xmin=lower, xmax=upper, color=culture), height=0, lwd=1) + scale_x_continuous(limits=c(0,1), labels=percent) + theme_bw(base_size=14) + xlab("Percent Knowledge at Age 14") + ylab("") + scale_color_manual(values=c("seagreen", "cornflowerblue")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title = element_blank(), strip.background = element_rect(fill="white", color="black"), legend.position = "none")

#### Transmission method results ####
logsumexp <- function (x) {
  y = max(x)
  y + log(sum(exp(x - y)))
}

softmax2 <- function (x) {
  exp(x - logsumexp(x))
}

# Teaching freq
method_BF <- cbind( post$am[,1] + post$sexcult_v[,3,4], post$am[,2] + post$sexcult_v[,3,5], 0 )
method_BM <- cbind( post$am[,1] + post$sexcult_v[,4,4], post$am[,2] + post$sexcult_v[,4,5], 0 )

method_HF <- cbind( post$am[,1] + post$sexcult_v[,1,4], post$am[,2] + post$sexcult_v[,1,5], 0 )
method_HM <- cbind( post$am[,1] + post$sexcult_v[,2,4], post$am[,2] + post$sexcult_v[,2,5], 0 )

# Converting to prob scale
for (i in 1:nrow(method_BF)) {
  method_BF[i,] <- softmax2(method_BF[i,])
  method_BM[i,] <- softmax2(method_BM[i,])
  method_HF[i,] <- softmax2(method_HF[i,])
  method_HM[i,] <- softmax2(method_HM[i,])
}

round( apply(method_HM, 2, median) , 4 )
round( apply(method_HM, 2, HPDI, prob=0.9) , 4 )

B_ID <- subset(d, d$culture == "BaYaka") %>% group_by(id) %>% summarise(sex = mean(sex))
H_ID <- subset(d, d$culture == "Hadza") %>% group_by(id) %>% summarise(sex = mean(sex))

# Hadza
H_prob <- array(NA, dim=c(length(post$lp__), nrow(H_ID), 3))
for (i in 1:length(post$lp__)) {
  for (n in 1:nrow(H_ID)) {
    
    if (H_ID$sex[n] == 0) H_prob[i,n,1] <- post$am[i,1] + post$sexcult_v[i,1,4] + post$idH_v[i,n,5] + post$mH_age[i,1]*post$age_estH[i,n] + post$m2H_age[i,1]*post$age_estH[i,n]^2
    
    else H_prob[i,n,1] <- post$am[i,1] + post$sexcult_v[i,2,4] + post$idH_v[i,n,5] + post$mH_age[i,1]*post$age_estH[i,n] + post$m2H_age[i,1]*post$age_estH[i,n]^2
    
    if (H_ID$sex[n] == 0) H_prob[i,n,2] <- post$am[i,2] + post$sexcult_v[i,1,5] + post$idH_v[i,n,6] + post$mH_age[i,2]*post$age_estH[i,n] + post$m2H_age[i,2]*post$age_estH[i,n]^2
    
    else H_prob[i,n,2] <- post$am[i,2] + post$sexcult_v[i,2,5] + post$idH_v[i,n,6] + post$mH_age[i,2]*post$age_estH[i,n] + post$m2H_age[i,2]*post$age_estH[i,n]^2
  }
}
H_prob[,,3] <- 0
# Converting to probs
for (i in 1:length(post$lp__))
  for (n in 1:nrow(H_ID)) {
    H_prob[i,n,] <- softmax2(H_prob[i,n,])
  }
# Median profile for each ID
H_profiles <- matrix(NA, nrow=nrow(H_ID), ncol=3)
for (n in 1:nrow(H_ID)) H_profiles[n,] <- apply(H_prob[,n,], 2, median)
H_profiles <- as.data.frame(H_profiles)
colnames(H_profiles) <- c("x", "y", "z")
H_profiles$Sex <- ifelse(H_ID$sex == 1, "Male", "Female")

# BaYaka
B_prob <- array(NA, dim=c(length(post$lp__), nrow(B_ID), 3))
for (i in 1:length(post$lp__)) {
  for (n in 1:nrow(B_ID)) {
    if (B_ID$sex[n] == 0) B_prob[i,n,1] <- post$am[i,1] + post$sexcult_v[i,3,4] + post$idB_v[i,n,5] + post$mB_age[i,1]*post$age_estB[i,n] + post$m2B_age[i,1]*post$age_estB[i,n]^2
    
    else B_prob[i,n,1] <- post$am[i,1] + post$sexcult_v[i,4,4] + post$idB_v[i,n,5] + post$mB_age[i,1]*post$age_estB[i,n] + post$m2B_age[i,1]*post$age_estB[i,n]^2
    
    if (B_ID$sex[n] == 0) B_prob[i,n,2] <- post$am[i,2] + post$sexcult_v[i,3,5] + post$idB_v[i,n,6] + post$mB_age[i,2]*post$age_estB[i,n] + post$m2B_age[i,2]*post$age_estB[i,n]^2
    
    else B_prob[i,n,2] <- post$am[i,2] + post$sexcult_v[i,4,5] + post$idB_v[i,n,6] + post$mB_age[i,2]*post$age_estB[i,n] + post$m2B_age[i,2]*post$age_estB[i,n]^2
  }
}
B_prob[,,3] <- 0
# Converting to probs
for (i in 1:length(post$lp__))
  for (n in 1:nrow(B_ID)) {
    B_prob[i,n,] <- softmax2(B_prob[i,n,])
  }
# Median profile for each ID
B_profiles <- matrix(NA, nrow=nrow(B_ID), ncol=3)
for (n in 1:nrow(B_ID)) B_profiles[n,] <- apply(B_prob[,n,], 2, median)

B_profiles <- as.data.frame(B_profiles)
colnames(B_profiles) <- c("x", "y", "z")
B_profiles$Sex <- ifelse(B_ID$sex == 1, "Male", "Female")

# combining
both_profiles <- rbind(B_profiles, H_profiles)
both_profiles$Culture <- c( rep("BaYaka", times=nrow(B_ID)),rep("Hadza", times=nrow(H_ID)) )

ggtern(both_profiles, mapping = aes(x = x, y = y, z = z)) + facet_wrap(~Culture) + geom_point(alpha=0.5, size=3, aes(color=Sex)) + tern_limits(T=1.05, L=1.05, R=1.05) + scale_color_manual(values=c("slategray", "orange")) + theme_bw(base_size=13) + xlab("Obs") + ylab("Teach") + zlab("Ind") + theme(strip.background = element_rect(fill="white", color="black"), tern.plot.background = element_rect(fill="white", color="black"))

#### Transmision path results ####
logsumexp <- function (x) {
  y = max(x)
  y + log(sum(exp(x - y)))
}

softmax2 <- function (x) {
  exp(x - logsumexp(x))
}

# Pathway freq
path_BF <- cbind( post$at[,1] + post$sexcult_v[,3,2], post$at[,2] + post$sexcult_v[,3,3], 0 )
path_BM <- cbind( post$at[,1] + post$sexcult_v[,4,2], post$at[,2] + post$sexcult_v[,4,3], 0 )

path_HF <- cbind( post$at[,1] + post$sexcult_v[,1,2], post$at[,2] + post$sexcult_v[,1,3], 0 )
path_HM <- cbind( post$at[,1] + post$sexcult_v[,2,2], post$at[,2] + post$sexcult_v[,2,3], 0 )

# Converting to prob scale
for (i in 1:nrow(path_BF)) {
  path_BF[i,] <- softmax2(path_BF[i,])
  path_BM[i,] <- softmax2(path_BM[i,])
  path_HF[i,] <- softmax2(path_HF[i,])
  path_HM[i,] <- softmax2(path_HM[i,])
}

round( apply(path_HM, 2, median) , 4 )
round( apply(path_HM, 2, HPDI, prob=0.9) , 4 )

# Difference in oblique across cultures
H_obl <- (path_HM[,2] + path_HF[,2])/2
B_obl <- (path_BM[,2] + path_BF[,2])/2
round(median(H_obl - B_obl),4)
round(HPDI(H_obl - B_obl, prob=0.9),4)

B_ID <- subset(d, d$culture == "BaYaka") %>% group_by(id) %>% summarise(sex = mean(sex))
H_ID <- subset(d, d$culture == "Hadza") %>% group_by(id) %>% summarise(sex = mean(sex))

# Hadza
H_prob <- array(NA, dim=c(length(post$lp__), nrow(H_ID), 3))
for (i in 1:length(post$lp__)) {
for (n in 1:nrow(H_ID)) {

if (H_ID$sex[n] == 0) H_prob[i,n,1] <- post$at[i,1] + post$sexcult_v[i,1,2] + post$idH_v[i,n,3] + post$tH_age[i,1]*post$age_estH[i,n] + post$t2H_age[i,1]*post$age_estH[i,n]^2

else H_prob[i,n,1] <- post$at[i,1] + post$sexcult_v[i,2,2] + post$idH_v[i,n,3] + post$tH_age[i,1]*post$age_estH[i,n] + post$t2H_age[i,1]*post$age_estH[i,n]^2

if (H_ID$sex[n] == 0) H_prob[i,n,2] <- post$at[i,2] + post$sexcult_v[i,1,3] + post$idH_v[i,n,4] + post$tH_age[i,2]*post$age_estH[i,n] + post$t2H_age[i,2]*post$age_estH[i,n]^2

else H_prob[i,n,2] <- post$at[i,2] + post$sexcult_v[i,2,3] + post$idH_v[i,n,4] + post$tH_age[i,2]*post$age_estH[i,n] + post$t2H_age[i,2]*post$age_estH[i,n]^2
}
}
H_prob[,,3] <- 0
# Converting to probs
for (i in 1:length(post$lp__))
for (n in 1:nrow(H_ID)) {
H_prob[i,n,] <- softmax2(H_prob[i,n,])
}
# Median profile for each ID
H_profiles <- matrix(NA, nrow=nrow(H_ID), ncol=3)
for (n in 1:nrow(H_ID)) H_profiles[n,] <- apply(H_prob[,n,], 2, median)
H_profiles <- as.data.frame(H_profiles)
colnames(H_profiles) <- c("x", "y", "z")
H_profiles$Sex <- ifelse(H_ID$sex == 1, "Male", "Female")

# BaYaka
B_prob <- array(NA, dim=c(length(post$lp__), nrow(B_ID), 3))
for (i in 1:length(post$lp__)) {
for (n in 1:nrow(B_ID)) {
if (B_ID$sex[n] == 0) B_prob[i,n,1] <- post$at[i,1] + post$sexcult_v[i,3,2] + post$idB_v[i,n,3] + post$tB_age[i,1]*post$age_estB[i,n] + post$t2B_age[i,1]*post$age_estB[i,n]^2
else B_prob[i,n,1] <- post$at[i,1] + post$sexcult_v[i,4,2] + post$idB_v[i,n,3] + post$tB_age[i,1]*post$age_estB[i,n] + post$t2B_age[i,1]*post$age_estB[i,n]^2
if (B_ID$sex[n] == 0) B_prob[i,n,2] <- post$at[i,2] + post$sexcult_v[i,3,3] + post$idB_v[i,n,4] + post$tB_age[i,2]*post$age_estB[i,n] + post$t2B_age[i,2]*post$age_estB[i,n]^2
else B_prob[i,n,2] <- post$at[i,2] + post$sexcult_v[i,4,3] + post$idB_v[i,n,4] + post$tB_age[i,2]*post$age_estB[i,n] + post$t2B_age[i,2]*post$age_estB[i,n]^2
}
}
B_prob[,,3] <- 0
# Converting to probs
for (i in 1:length(post$lp__))
for (n in 1:nrow(B_ID)) {
B_prob[i,n,] <- softmax2(B_prob[i,n,])
}
# Median profile for each ID
B_profiles <- matrix(NA, nrow=nrow(B_ID), ncol=3)
for (n in 1:nrow(B_ID)) B_profiles[n,] <- apply(B_prob[,n,], 2, median)
round(B_profiles, 4)
B_profiles <- as.data.frame(B_profiles)
colnames(B_profiles) <- c("x", "y", "z")
B_profiles$Sex <- ifelse(B_ID$sex == 1, "Male", "Female")

# combining
both_profiles <- rbind(B_profiles, H_profiles)
both_profiles$Culture <- c( rep("BaYaka", times=nrow(B_ID)),rep("Hadza", times=nrow(H_ID)) )

ggtern(both_profiles, mapping = aes(x = x, y = y, z = z)) + facet_wrap(~Culture) + geom_point(alpha=0.5, size=3, aes(color=Sex)) + tern_limits(T=1.05, L=1.05, R=1.05) + scale_color_manual(values=c("slategray", "orange")) + theme_bw(base_size=13) + xlab("Hor") + ylab("Obl") + zlab("Ver") + theme(strip.background = element_rect(fill="white", color="black"), tern.plot.background = element_rect(fill="white", color="black"))

#### Correlations between learning parameters ####
cor_rankB <- post$Rho_skillB[,-11,11]
cor_rankH <- post$Rho_skillH[,-11,11]

colnames(cor_rankB) <- c("k", "b", "eta", "p", "p_male", "horiz_trans", "oblique_trans", "obs_learn", "teach_learn", "male_trans")
colnames(cor_rankH) <- colnames(cor_rankB)

cor_rankBlong <- as.data.frame(cor_rankB) %>% gather(key="var", value="cor")
cor_rankHlong <- as.data.frame(cor_rankH) %>% gather(key="var", value="cor")

cor_ranks_long <- rbind(cor_rankBlong, cor_rankHlong)
cor_ranks_long$culture <- rep(c("BaYaka", "Hadza"), each=length(post$lp__)*10)

round( median(cor_ranks_long$cor[cor_ranks_long$var == "p_male" & cor_ranks_long$culture == "Hadza"]) , 2)
round( HPDI(cor_ranks_long$cor[cor_ranks_long$var == "p_male" & cor_ranks_long$culture == "Hadza"], prob=0.9) , 2)

ggplot(cor_ranks_long, aes(x=cor, y=var)) + geom_density_ridges2(aes( color=culture, fill=culture), alpha=0.5, scale=0.9, rel_min_height=0.01) + theme_bw(base_size = 16) + scale_y_discrete(expand = c(0, 0)) +
  scale_fill_manual(values=c("seagreen", "cornflowerblue")) +
  scale_color_manual(values=c(NA, NA)) +
  labs(fill="Culture", color="Culture") + ylab("") + xlab("Correlation with Skill Difficulty Ranking") + scale_x_continuous(limits=c(-1,1)) + geom_vline(xintercept = 0, lty="dashed", color="darkred", lwd=1)

#### Sex-biased transmission results ####
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

#### Explicit and tacit knowledge cor results #### 
# Extract posterior samples
post <- extract.samples(fit_m)
post_cor <- extract.samples(fit_cor)

id_cor <- data.frame(
  cor = c(post_cor$Rho_idB[,1,2],post$Rho_idB[,1,2],post_cor$Rho_idH[,1,2],post$Rho_idH[,1,2] ),
  culture = c( rep("BaYaka", length(post_cor$lp__)), rep("BaYaka", length(post$lp__)), rep("Hadza", length(post_cor$lp__)), rep("Hadza", length(post$lp__)) ),
  model = c( rep("Correlation", length(post_cor$lp__)), rep("Residual Correlation", length(post$lp__)), rep("Correlation", length(post_cor$lp__)), rep("Residual Correlation", length(post$lp__)) )
)

# Point estimates and intervals
round(median(id_cor$cor[id_cor$model == "Correlation" & id_cor$culture == "BaYaka"]),2)
round(HPDI(id_cor$cor[id_cor$model == "Correlation" & id_cor$culture == "BaYaka"], prob=0.9),2)

round(median(id_cor$cor[id_cor$model == "Correlation" & id_cor$culture == "Hadza"]),2)
round(HPDI(id_cor$cor[id_cor$model == "Correlation" & id_cor$culture == "Hadza"], prob=0.9),2)

round(median(id_cor$cor[id_cor$model == "Residual Correlation" & id_cor$culture == "BaYaka"]),2)
round(HPDI(id_cor$cor[id_cor$model == "Residual Correlation" & id_cor$culture == "BaYaka"], prob=0.9),2)

round(median(id_cor$cor[id_cor$model == "Residual Correlation" & id_cor$culture == "Hadza"]),2)
round(HPDI(id_cor$cor[id_cor$model == "Residual Correlation" & id_cor$culture == "Hadza"], prob=0.9),2)


# Plotting densities
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

#### Pace-of-learning plots #### 
post <- extract.samples(fit_m)
# We'll make predictions based on skill, using the names in these relational dataframes
Hskills <- d_H %>% group_by(skill) %>% summarise(ind = mean(skill_id))
Bskills <- d_B %>% group_by(skill) %>% summarise(ind = mean(skill_id))

# Add publication-friendly labels
pub_labels <- read_csv("figure_labels.csv")
Hskills$skill2 <- pub_labels$`Change to`[match(Hskills$skill, pub_labels$`In figure`)]
Bskills$skill2 <- pub_labels$`Change to`[match(Bskills$skill, pub_labels$`In figure`)]

both_skills <- bind_rows(Hskills, Bskills)

# Age sequence to predict along, 1 = 80 years
age_seq <- seq(from=0, to=1, length.out = 30)

##### Subsistence knowledge plot predict function
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
    freelist <- ifelse( skill %in% c("X13.honey", "X3.bow"), 1, 0)
    
    k <- exp(post$ak + post$skillH_v[,skill_id,1])
    b <- exp(post$ab + post$skillH_v[,skill_id,2])
    eta <- exp(post$ae + post$skillH_v[,skill_id,3] + post$e_expl*expl )
    
    stem <- post$ap + post$a_freelist*freelist + post$p_expl*expl + post$sexcult_v[,sexcult,1] + post$skillH_v[,skill_id,4] + post$skillH_v[,skill_id,5]*sex_d
  }
  
  if (culture == "BaYaka") {
    skill_id <- Bskills[ Bskills$skill == skill , ]$ind
    freelist <- ifelse( skill %in% c("B_basketvine", "B_climbvine", "X9.gun", "B_honeysum", "X10.spear", "X19.traps"), 1, 0)
    
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
    mtext(both_skills$skill2[both_skills$skill == skill], line=0.5, cex=1.1)
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

skill_plot(skill="B_animal", culture="BaYaka", sex="Male")

#### Plotting all BaYaka Skills #### 
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

#### Plotting all Hadza Skills #### 
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


## Plotting correlated random effects ####
library(cowplot)
post <- extract.samples(fit_m)

## ranks
rank_reB <- as.data.frame(post$skillB_v[,,11])
rank_reH <- as.data.frame(post$skillH_v[,,11])
colnames(rank_reB) <- paste0("B",1:ncol(rank_reB))
colnames(rank_reH) <- paste0("H",1:ncol(rank_reH))
# wide to long
rank_reB <- rank_reB %>% gather(key="task", value="est")
rank_reH <- rank_reH %>% gather(key="task", value="est")
# combining
rank_both <- bind_rows(rank_reB, rank_reH)
rank_both$culture <- c(rep("BaYaka", nrow(rank_reB)), rep("Hadza", nrow(rank_reH)))


# a[skill]
a_reB <- as.data.frame(post$skillB_v[,,4])
a_reH <- as.data.frame(post$skillH_v[,,4])
colnames(a_reB) <- paste0("B",1:ncol(a_reB))
colnames(a_reH) <- paste0("H",1:ncol(a_reH))
# wide to long
a_reB <- a_reB %>% gather(key="task", value="est")
a_reH <- a_reH %>% gather(key="task", value="est")
# combining
a_both <- bind_rows(a_reB, a_reH)
a_both$culture <- c(rep("BaYaka", nrow(a_reB)), rep("Hadza", nrow(a_reH)))

# mega merge
re_every <- bind_rows(rank_both, a_both)
re_every$par <- c(rep("rank", nrow(rank_both)), rep("a", nrow(a_both)))

re_sum <- re_every %>% group_by(task, par, culture) %>% summarise(lower=HPDI(est, prob=0.9)[1], upper=HPDI(est, prob=0.9)[2], median=median(est) ) %>% pivot_wider(names_from=par, values_from=c(lower, upper, median))

# raw data
Hadza_long <- Hadza_rank %>% gather(key="task", value="rank")
BaYaka_long <- BaYaka_rank %>% gather(key="task", value="rank")

both_rank <- bind_rows(Hadza_long, BaYaka_long)
both_rank$culture <- c( rep("Hadza", nrow(Hadza_long)), rep("BaYaka", nrow(BaYaka_long)) )

rank_plot <- both_rank %>% ggplot(aes(x=rank, y=task, color=culture, fill=culture)) + facet_wrap(~culture, scales="free_y") + geom_density_ridges(stat="binline", bins=10, scale=0.9) + theme_bw(base_size=15) + theme(legend.position = "none", axis.ticks.y=element_blank(), axis.text.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title = element_blank(), strip.background = element_rect(fill="white", color="black"), axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.spacing = unit(2, "lines")) + scale_fill_manual(values=c("seagreen", "cornflowerblue")) + scale_color_manual(values=c("seagreen", "cornflowerblue")) + ylab("Subsistence Task") + xlab("Difficulty Ranking")

endorse_plot <- d %>% filter(freelist == 0) %>% select(y, skill, culture) %>% group_by(skill, culture) %>% summarise(prop=sum(y)/n()) %>% ggplot(aes(x=prop, y=skill, color=culture)) + facet_wrap(~culture, scales="free") + geom_point() + xlab("Proportion of Endorsements") + ylab("Subsistence Task") + theme_bw(base_size=15) + theme(legend.position = "none", axis.ticks.y=element_blank(), axis.text.y=element_blank(),strip.background = element_rect(fill="white", color="black"), axis.ticks.x=element_blank(), panel.spacing = unit(2, "lines")) + scale_color_manual(values=c("seagreen", "cornflowerblue")) + scale_x_continuous(limits=c(0,1), breaks=c(0,0.5,1))

re_plot <- ggplot(re_sum, aes(x=median_a, y=median_rank, color=culture)) + geom_point() + geom_errorbarh(aes(xmin=lower_a, xmax=upper_a), alpha=0.3) + geom_errorbar(aes(ymin=lower_rank, ymax=upper_rank), alpha=0.3) + theme_bw(base_size=15) + ylab(expression(alpha["skill,rank"])) + xlab(expression(alpha["skill,knowledge"])) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") + stat_ellipse(type = "norm", linetype = 2) + scale_color_manual(values=c("seagreen", "cornflowerblue"))

# correlations between rank and a
rho_plot <- data.frame(est=c(post$Rho_skillB[,4,11], post$Rho_skillH[,4,11]), Culture = c(rep("BaYaka", length(post$lp__)), rep("Hadza", length(post$lp__)))) %>% ggplot(aes(x=est,color=Culture, fill=Culture)) + geom_density(alpha=0.5) + scale_y_discrete(expand = c(0, 0)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values=c("seagreen", "cornflowerblue")) + scale_fill_manual(values=c("seagreen", "cornflowerblue")) + theme_bw(base_size=15) + xlab(expression(paste("Correlation (",rho, ")"))) + ylab("")


plot_grid(
  rank_plot, re_plot, endorse_plot, rho_plot,
  labels = "", ncol = 2, scale=0.8
)
