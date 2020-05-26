library(tidyverse)
library(scales)
library(rethinking) # https://github.com/rmcelreath/rethinking/
library(ggridges)
library(viridis)
library(patchwork) # https://github.com/thomasp85/patchwork

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
m_sub <- stan_model( file="subsistence_model_lh_rescor.stan" )

fit_cor <- sampling( m_cor, data=data_list, init="0", chains=4, cores=4, iter=2000, control=list(adapt_delta=0.9 ) )
fit_m <- sampling( m_sub, data=data_list, init="0", chains=4, cores=4, iter=2000, control=list(adapt_delta=0.9 ) )

# If they've been fit once, I suggest saving to-reload later
# saveRDS(fit_cor, "fit_cor.rds")
# saveRDS(fit_m, "fit_m.rds")

#### Checking mcmc diagnostics ########################
# write.csv(precis(fit_m, depth=3), "model_precis.csv")
# prec <- read_csv("model_precis.csv")

# prec %>% filter(Rhat4 > 1.05)

#### Extract posterior samples #######################
#fit_cor <- readRDS("fit_cor.rds")
#fit_m <- readRDS("fit_m.rds")

post <- extract.samples(fit_m)
post_cor <- extract.samples(fit_cor)

n_samps <- length(post$lp__)

# Organize skill data
Hskills <- d_H %>% group_by(skill) %>% summarise(ind = mean(skill_id))
Bskills <- d_B %>% group_by(skill) %>% summarise(ind = mean(skill_id))

# Add publication-friendly labels
pub_labels <- read_csv("figure_labels.csv")
Hskills$skill2 <- pub_labels$`Change to`[match(Hskills$skill, pub_labels$`In figure`)]
Bskills$skill2 <- pub_labels$`Change to`[match(Bskills$skill, pub_labels$`In figure`)]
###############################################
######## Figure 1 #############################
## Plotting correlated random effects ####
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

rank_plot <- both_rank %>% ggplot(aes(x=rank, y=task, color=culture, fill=culture)) + facet_wrap(~culture, scales="free_y") + geom_density_ridges(stat="binline", bins=10, scale=0.9) + theme_bw(base_size=15) + theme(legend.position = "none", axis.ticks.y=element_blank(), axis.text.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title = element_blank(), strip.background = element_rect(fill="white", color="black"), axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.spacing = unit(2, "lines")) + scale_fill_manual(values=c("seagreen", "cornflowerblue")) + scale_color_manual(values=c("seagreen", "cornflowerblue")) + ylab("Subsistence Task") + xlab("Difficulty Ranking") + ggtitle("Study Data")

endorse_plot <- d %>% filter(freelist == 0) %>% select(y, skill, culture) %>% group_by(skill, culture) %>% summarise(prop=sum(y)/n()) %>% ggplot(aes(x=prop, y=skill, color=culture)) + facet_wrap(~culture, scales="free") + geom_point() + xlab("Proportion of Endorsements") + ylab("Subsistence Task") + theme_bw(base_size=15) + theme(legend.position = "none", axis.ticks.y=element_blank(), axis.text.y=element_blank(),strip.background = element_rect(fill="white", color="black"), axis.ticks.x=element_blank(), panel.spacing = unit(2, "lines")) + scale_color_manual(values=c("seagreen", "cornflowerblue")) + scale_x_continuous(limits=c(0,1), breaks=c(0,0.5,1))

re_plot <- ggplot(re_sum, aes(x=median_a, y=median_rank, color=culture)) + geom_point() + geom_errorbarh(aes(xmin=lower_a, xmax=upper_a), alpha=0.3) + geom_errorbar(aes(ymin=lower_rank, ymax=upper_rank), alpha=0.3) + theme_bw(base_size=15) + ylab(expression(alpha["task,rank"])) + xlab(expression(alpha["task,knowledge"])) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") + stat_ellipse(type = "norm", linetype = 2) + scale_color_manual(values=c("seagreen", "cornflowerblue")) + ggtitle("Model Parameters")

# correlations between rank and a
rho_plot <- data.frame(est=c(post$Rho_skillB[,4,11], post$Rho_skillH[,4,11]), Culture = c(rep("BaYaka", length(post$lp__)), rep("Hadza", length(post$lp__)))) %>% ggplot(aes(x=est,color=Culture, fill=Culture)) + geom_density(alpha=0.5) + scale_y_discrete(expand = c(0, 0)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values=c("seagreen", "cornflowerblue")) + scale_fill_manual(values=c("seagreen", "cornflowerblue")) + theme_bw(base_size=15) + xlab(expression(paste("Correlation (",rho, ")"))) + ylab("") + ggtitle("Correlations Between Parameters")

svg("Figure_1.svg",  height=6, width=7.5, pointsize=12)

(rank_plot + re_plot) / (endorse_plot + rho_plot)

dev.off()

#### Figure 2 #################################
#### Plot prior K functions ###################
b_seq <- rep( seq(from=0.01, to=3, length.out=100), 2 )
k_seq <- rep( c(2,4), each = length(b_seq)/2 )
age_seq <- seq(from=0, to=50, length.out = 100)

K_pred <- data.frame( matrix(NA, nrow=length(b_seq), ncol=length(age_seq)) )
colnames(K_pred) <- age_seq

for (i in 1:nrow(K_pred)) {
  K_pred[i,] <- (1 - exp(-k_seq[i]*age_seq/80))^b_seq[i]
}

K_pred$k <- k_seq
K_pred$b <- as.character(b_seq)

K_long <- K_pred %>% pivot_longer(-c(b,k), names_to="Age")
K_long$k <- ifelse(K_long$k == 2, "k = 2", "k = 4")
K_long$b <- as.numeric(K_long$b)

svg( "Figure_2.svg" , height=5, width=8.5, pointsize=12 )

ggplot(K_long, aes(x=as.numeric(Age), y=value)) + facet_wrap(~k) + geom_line(aes(color=b,  group=as.character(b))) + scale_color_viridis(option="A") + scale_x_continuous(breaks=c(0,16,32,48)) + xlab("Age") + ylab("Age-Structured Knowledge") + theme_bw(base_size=18) + scale_y_continuous(breaks=c(0,1), labels=c("Min", "Max")) + theme(strip.background = element_rect(fill="white"))

dev.off()
##################################################
###### Figure 3 ##################################
#### Knowledge at age 14 ####

### Creating predictions at age 14 for each skill
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
pred14_summary <- pred14_l %>% group_by(skill) %>% summarise(med=median(est), lower=HPDI(est, prob=0.9)[1], upper=HPDI(est, prob=0.9)[2]) %>% mutate(culture=ifelse(skill %in% Bskills$skill2, "BaYaka", "Hadza"))

### Match up to rank data
B_rank_median <- data.frame(task=colnames(BaYaka_rank), med_rank=apply(BaYaka_rank, 2, median))
H_rank_median <- data.frame(task=colnames(Hadza_rank), med_rank=apply(Hadza_rank, 2, median))

B_rank_median$skill <- pub_labels$`Change to`[match(B_rank_median$task, pub_labels$`In figure`)]
H_rank_median$skill <- pub_labels$`Change to`[match(H_rank_median$task, pub_labels$`In figure`)]

both_rank_median <- bind_rows(B_rank_median, H_rank_median) %>% mutate(culture = c( rep("BaYaka", nrow(B_rank_median)), rep("Hadza", nrow(H_rank_median)))) %>% select(skill, med_rank, culture)

pred14_summary <- left_join(pred14_summary, both_rank_median)

svg( "figure_3.svg", height=6, width=7.5, pointsize=12 )

# Need to re-factor within-culture
pred14_summary %>% 
  mutate(term = reorder(skill, med_rank)) %>%
  group_by(culture, term) %>% 
  arrange(desc(med_rank)) %>% 
  ungroup() %>% 
  mutate(term = factor(paste(term, culture, sep = "__"), 
                       levels = rev(paste(term, culture, sep = "__")))) %>%
  ggplot(aes(x=med,y=term)) + facet_wrap(~culture, scales="free_y") + 
  geom_point(aes(color=culture), lwd=2) + geom_errorbarh(aes(xmin=lower, xmax=upper, color=culture), height=0, lwd=1) + 
  scale_x_continuous(limits=c(0,1), labels=percent, breaks=c(0, 0.5, 1)) + 
  theme_bw(base_size=14) + xlab("Percent Knowledge at Age 14") + ylab("") + 
  scale_color_manual(values=c("seagreen", "cornflowerblue")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title = element_blank(), strip.background = element_rect(fill="white", color="black"), legend.position = "none") + 
  scale_y_discrete(labels = function(x) gsub("__.+$", "", x))

dev.off()
#####################################################
##### Figure 4 ######################################
n_samps <- length(post$lp__) # number of posterior samples
age_seq <- seq(from=0, to=50/80, length.out = 75)

preds_B <- array( NA , dim=c(n_samps, N_skillB, length(age_seq)), dimnames=list( samps=1:n_samps, task=paste0("B",1:N_skillB), Age=age_seq*80  ) )
preds_H <- array( NA , dim=c(n_samps, N_skillH, length(age_seq)), dimnames=list( samps=1:n_samps, task=paste0("H",1:N_skillH), Age=age_seq*80  ) )

# BaYaka predictions
for (s in 1:N_skillB) {
  k_B <- exp(post$ak + post$skillB_v[,s,1])
  b_B <- exp(post$ab + post$skillB_v[,s,2])
  
for (i in 1:n_samps) {
  preds_B[i,s,] <- (1 - exp(-k_B[i]*age_seq))^b_B[i]
}
}

# Hadza predictions
for (s in 1:N_skillH) {
  k_H <- exp(post$ak + post$skillH_v[,s,1])
  b_H <- exp(post$ab + post$skillH_v[,s,2])
  
  for (i in 1:n_samps) {
    preds_H[i,s,] <- (1 - exp(-k_H[i]*age_seq))^b_H[i]
  }
}

# Convert arrays into long form
preds_B_long <- preds_B %>%  as.tbl_cube(met_name = "est") %>% as_tibble
preds_H_long <- preds_H %>% as.tbl_cube(met_name = "est") %>% as_tibble

preds_both <- bind_rows(preds_B_long, preds_H_long)
preds_both$culture <- c( rep("BaYaka", nrow(preds_B_long)), rep("Hadza", nrow(preds_H_long)) )

preds_med <- preds_both %>% group_by(task, Age, culture) %>% summarise(med = median(est))


med_S_plot <- ggplot(preds_med, aes(x=Age, y=med)) + geom_line(aes(color=culture, group=task)) + scale_color_manual(values=c("seagreen", "cornflowerblue")) + theme_bw(base_size=18) + ylab("Age-Structured Knowledge") + scale_x_continuous(breaks=c(0,16,32,48)) + scale_y_continuous(breaks=c(0,1), labels=c("Min", "Max")) + theme(legend.position = "top", legend.title = element_blank()) + guides(color = guide_legend(override.aes = list(size = 2, title=NA)))

#### Now, get correlations between rank and task acquisition parameters
B_cor <- as.data.frame(post$Rho_skillB[,1:4,11])
names(B_cor) <- c("rho(rank,k)","rho(rank,b)","rho(rank,eta)","rho(rank,alpha)")

H_cor <- as.data.frame(post$Rho_skillH[,1:4,11])
names(H_cor) <- c("rho(rank,k)","rho(rank,b)","rho(rank,eta)","rho(rank,alpha)")

B_cor_long <- B_cor %>% mutate(samp=1:n_samps) %>% pivot_longer(-samp)
H_cor_long <- H_cor %>% mutate(samp=1:n_samps) %>% pivot_longer(-samp)

cor_both <- bind_rows(B_cor_long, H_cor_long)
cor_both$culture <- c( rep("BaYaka", nrow(B_cor_long)), rep("Hadza", nrow(H_cor_long)) )

cor_both$name <- factor(cor_both$name, levels=c("rho(rank,k)","rho(rank,b)","rho(rank,eta)","rho(rank,alpha)"))

rank_cor_plot <- ggplot(cor_both, aes(x=value)) + facet_wrap(~name, labeller = label_parsed) + geom_density(aes(color=culture, fill=culture), alpha=0.5) + geom_vline(xintercept = 0, linetype="dashed") + scale_color_manual(values=c("seagreen", "cornflowerblue")) + scale_fill_manual(values=c("seagreen", "cornflowerblue")) + scale_y_continuous(expand = c(0, 0)) + theme_bw(base_size = 18) + theme(legend.position = "none",axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(), strip.background = element_rect(fill="white")) + xlab("Correlation") + scale_x_continuous(breaks=c(-1,-0.5, 0, 0.5, 1), labels=c("-1", "-0.5", "0", "0.5", "1"))

svg( "Figure_4.svg" , height=6, width=8.5, pointsize=12 )

med_S_plot + rank_cor_plot

dev.off()
#####################################
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

library(ggtern)

tern_path <- ggtern(both_profiles, mapping = aes(x = x, y = y, z = z)) + facet_wrap(~Culture) + geom_point(alpha=0.5, size=3, aes(color=Sex)) + tern_limits(T=1.05, L=1.05, R=1.05) + scale_color_manual(values=c("slategray", "orange")) + theme_bw(base_size=13) + xlab("Obs") + ylab("Teach") + zlab("Ind") + theme(strip.background = element_rect(fill="white", color="black"), tern.plot.background = element_rect(fill="white", color="black")) + ggtitle("a Transmission Pathway")

detach("package:ggtern", unload=TRUE)

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

library(ggtern)

method_tern <- ggtern(both_profiles, mapping = aes(x = x, y = y, z = z)) + facet_wrap(~Culture) + geom_point(alpha=0.5, size=3, aes(color=Sex)) + tern_limits(T=1.05, L=1.05, R=1.05) + scale_color_manual(values=c("slategray", "orange")) + theme_bw(base_size=13) + xlab("Hor") + ylab("Obl") + zlab("Ver") + theme(strip.background = element_rect(fill="white", color="black"), tern.plot.background = element_rect(fill="white", color="black")) + ggtitle("b Learning Method")

detach("package:ggtern", unload=TRUE)

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

round(median(cor_ranks_long$cor[cor_ranks_long$var == "teach_learn" & cor_ranks_long$culture == "BaYaka"], prob=0.9),2)
round(HPDI(cor_ranks_long$cor[cor_ranks_long$var == "teach_learn" & cor_ranks_long$culture == "BaYaka"], prob=0.9),2)


ggplot(cor_ranks_long, aes(x=cor, y=var)) + geom_density_ridges2(aes( color=culture, fill=culture), alpha=0.5, scale=0.9, rel_min_height=0.01) + theme_bw(base_size = 16) + scale_y_discrete(expand = c(0, 0)) +
  scale_fill_manual(values=c("seagreen", "cornflowerblue")) +
  scale_color_manual(values=c(NA, NA)) +
  labs(fill="Culture", color="Culture") + ylab("") + xlab("Correlation with Skill Difficulty Ranking") + scale_x_continuous(limits=c(-1,1)) + geom_vline(xintercept = 0, lty="dashed", color="darkred", lwd=1)


###### Figure 5 ####################################
#### Sex-biased transmission results ###############
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

sexbias_1 <- ggplot(trans_df, aes(x=prob)) +
  facet_wrap(~culture) + geom_density(aes(color=learner_sex, fill=learner_sex, alpha=learner_sex, linetype=learner_sex)) +
  theme_bw(base_size = 15) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(breaks=c(0,0.5,1), labels=c("0", "0.5", "1")) +
  ylab("") + xlab("Probability of Female Transmission") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(fill="white", color="black")) +
  scale_fill_manual(values=c("darkred", "slategray", "orange")) +
  scale_color_manual(values=c("darkred", NA, NA)) +
  scale_alpha_manual(values=c(0, 0.6, 0.6)) +
  scale_linetype_manual(values=c("dashed", "solid", "solid")) + 
  labs(fill="Learner Sex", color="Learner Sex", alpha="Learner Sex", linetype="Learner Sex") +
  ggtitle("a")

## Now, break down by task #####

# Prob female transmission
pr_fB <- matrix( NA, nrow=n_samps, ncol=N_skillB )
pr_fH <- matrix( NA, nrow=n_samps, ncol=N_skillH )

# Task difficulty and task male-ness
rank_B <- matrix( NA, nrow=n_samps, ncol=N_skillB )
alphaM_B <- rank_B

rank_H <- matrix( NA, nrow=n_samps, ncol=N_skillH )
alphaM_H <- rank_H

for (s in 1:N_skillB) {
  pr_fB[,s] <- 1 - logistic( post$as + (post$sexcult_v[,3,6] + post$sexcult_v[,4,6])/2 +  post$skillB_v[,s,10]  )
  rank_B[,s] <- post$skillB_v[,s,11]
  alphaM_B[,s] <- post$skillB_v[,s,5]
}

for (s in 1:N_skillH) {
pr_fH[,s] <- 1 - logistic( post$as + (post$sexcult_v[,1,6] + post$sexcult_v[,2,6])/2 +  post$skillH_v[,s,10]  )
rank_H[,s] <- post$skillH_v[,s,11]
alphaM_H[,s] <- post$skillH_v[,s,5]
}

## Summarize using median and 90% HPDI
ft_df_B <- data.frame(
  pr_f_med = apply(pr_fB, 2, median),
  pr_f_lower = apply(pr_fB, 2, HPDI, prob=0.9)[1,],
  pr_f_upper = apply(pr_fB, 2, HPDI, prob=0.9)[2,],
  alphaM_med = apply(alphaM_B, 2, median),
  alphaM_lower = apply(alphaM_B, 2, HPDI, prob=0.9)[1,],
  alphaM_upper = apply(alphaM_B, 2, HPDI, prob=0.9)[2,],
  rank_med = apply(rank_B, 2, median)
)

ft_df_H <- data.frame(
  pr_f_med = apply(pr_fH, 2, median),
  pr_f_lower = apply(pr_fH, 2, HPDI, prob=0.9)[1,],
  pr_f_upper = apply(pr_fH, 2, HPDI, prob=0.9)[2,],
  alphaM_med = apply(alphaM_H, 2, median),
  alphaM_lower = apply(alphaM_H, 2, HPDI, prob=0.9)[1,],
  alphaM_upper = apply(alphaM_H, 2, HPDI, prob=0.9)[2,],
  rank_med = apply(rank_H, 2, median)
)

ft_df_B$rank_med <- (ft_df_B$rank_med / max(ft_df_B$rank_med))
ft_df_H$rank_med <-  (ft_df_H$rank_med / max(ft_df_H$rank_med)) 

both_ft <- bind_rows(ft_df_B, ft_df_H) %>% mutate(culture = c( rep("BaYaka", nrow(ft_df_B)), rep("Hadza", nrow(ft_df_H))) )

skill_sex <- ggplot(data=both_ft, aes(color=culture, size=rank_med - min(rank_med))) +
  facet_wrap(~culture) + 
  geom_point(aes(y=pr_f_med, x=alphaM_med), alpha=0.5) + 
  theme_bw(base_size=15) + 
  ylab("Probability of Female Transmission") + 
  xlab("") + 
  geom_vline(xintercept = 0, linetype="dashed") + 
  geom_hline(yintercept = 0.5, linetype="dashed") + 
  scale_color_manual(values=c("seagreen", "cornflowerblue"), guide=F) + 
  scale_x_continuous(breaks=c(-3, 3), labels=c("Female-Biased \nTasks", "Male-Biased \nTasks")) + 
  scale_y_continuous(breaks=c(0,0.5,1), labels=c("0", "0.5", "1")) +
  scale_size_area(name = "Task Ranking", breaks=c(1,2), labels=c("Less Difficult", "More Difficult")) + 
  theme(legend.title = element_text(),strip.background = element_rect(fill="white", color="black")) + 
  ggtitle("b")

svg("Figure_5.svg", height=8, width=8, pointsize = 12)

sexbias_1 / skill_sex

dev.off()
########################################################
###### Figure 6 ################################
B_cor <- as.data.frame(post$Rho_skillB[,1:4,11])
names(B_cor) <- c("rho(rank,k)","rho(rank,b)","rho(rank,eta)","rho(rank,alpha)")

H_cor <- as.data.frame(post$Rho_skillH[,1:4,11])
names(H_cor) <- c("rho(rank,k)","rho(rank,b)","rho(rank,eta)","rho(rank,alpha)")

B_cor_long <- B_cor %>% mutate(samp=1:n_samps) %>% pivot_longer(-samp)
H_cor_long <- H_cor %>% mutate(samp=1:n_samps) %>% pivot_longer(-samp)

cor_both <- bind_rows(B_cor_long, H_cor_long)
cor_both$culture <- c( rep("BaYaka", nrow(B_cor_long)), rep("Hadza", nrow(H_cor_long)) )

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
svg( "Figure_6.svg", height=5, width=8.5, pointsize=12 )
ggplot(id_cor, aes(x=cor)) +
  facet_wrap(~model) + geom_density(aes(color=culture, fill=culture), alpha=0.5) +
  theme_bw(base_size = 16) +
  scale_y_discrete(expand = c(0, 0)) +
  ylab("") + xlab("Correlation Within Individuals, Tacit and Explicit Task Performance") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(fill="white", color="black")) +
  scale_x_continuous(limits=c(-1,1)) +
  scale_fill_manual(values=c("seagreen", "cornflowerblue")) +
  scale_color_manual(values=c("seagreen", "cornflowerblue")) + 
  labs(fill="Culture", color="Culture") + geom_vline(xintercept = 0, linetype="dashed", lwd=1, color="darkred")
dev.off()
############################################
##### Supplementary Plots ##################

#### Pace-of-learning plots #### 
post <- extract.samples(fit_m)
# We'll make predictions based on skill, using the names in these relational dataframes
Hskills <- d_H %>% group_by(skill) %>% summarise(ind = mean(skill_id))
Bskills <- d_B %>% group_by(skill) %>% summarise(ind = mean(skill_id))

# Add publication-friendly labels
pub_labels <- read_csv("figure_labels.csv")
Hskills$skill2 <- pub_labels$`Change to`[match(Hskills$skill, pub_labels$`In figure`)]
Bskills$skill2 <- pub_labels$`Change to`[match(Bskills$skill, pub_labels$`In figure`)]

# Plot in order of their median task ranking
Brank_med <- data.frame( order = 1:ncol(BaYaka_rank),
                         skill = names(sort(apply(BaYaka_rank, 2, median))),
                         med_rank = paste0( sort(apply(BaYaka_rank, 2, median))*ncol(BaYaka_rank), "/", ncol(BaYaka_rank)  )
)
                         
Hrank_med <- data.frame( order = 1:ncol(Hadza_rank), 
                         skill = names(sort(apply(Hadza_rank, 2, median))), 
                         med_rank = paste0( sort(apply(Hadza_rank, 2, median))*ncol(Hadza_rank), "/", ncol(Hadza_rank) )
)

Bskills <- left_join(Bskills, Brank_med)
Hskills <- left_join(Hskills, Hrank_med) 

# Fill in plot order for skills that weren't ranked
for (s in 1:nrow(Bskills)) {
  if(is.na(Bskills$order[s])) Bskills$order[s] <- max(Bskills$order,na.rm = T) + 1
}

for (s in 1:nrow(Hskills)) {
  if(is.na(Hskills$order[s])) Hskills$order[s] <- max(Hskills$order,na.rm = T) + 1
}

# Sort the dataframe by order of ranks
Bskills <- arrange(Bskills, order)
Hskills <- arrange(Hskills, order)

both_skills <- bind_rows(Bskills, Hskills)

# old order for plotting
#Hskills$order <- as.numeric( substr(sub('\\..*', '', Hskills$skill2), 2, nchar(sub('\\..*', '', Hskills$skill2))) )
#Bskills$order <- as.numeric( substr(sub('\\..*', '', Bskills$skill2), 2, nchar(sub('\\..*', '', Bskills$skill2))) )

# Dispersion parameter for the freelist responses
freelist_key <- d %>% group_by(skill) %>% summarise(freelist_id = mean(freelist_id[freelist_id > 0]))

# Age sequence to predict along, 1 = 80 years
age_seq <- seq(from=0, to=1, length.out = 30)

##### Subsistence knowledge plot predict function
skill_plot <- function( skill, culture, sex, plot.data=T, PI=0.9, quantiles=T, add=F, color="darkorange") {
  
  preds <- matrix(NA, nrow=length(post$lp__), ncol=length(age_seq)) # expected values
  preds_resp <- preds # rng predictions
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
    freelist <- ifelse( skill %in% c("X4.basketvine", "X18.climbingvine", "X9.gun", "X24.honey", "X10.spear", "X19.traps"), 1, 0)
    
    k <- exp(post$ak + post$skillB_v[,skill_id,1])
    b <- exp(post$ab + post$skillB_v[,skill_id,2])
    eta <- exp(post$ae + post$skillB_v[,skill_id,3] + post$e_expl*expl)
    
    stem <- post$ap + post$a_freelist*freelist + post$p_expl*expl + post$sexcult_v[,sexcult,1] + post$skillB_v[,skill_id,4] + post$skillB_v[,skill_id,5]*sex_d
  }
  
  for (j in 1:ncol(preds)) {
    if (freelist == 0) preds[,j] <- 2 * ( logistic( ((1 - exp(-k*age_seq[j]))^b)^eta * exp(stem)) - 0.5 )
    if (freelist == 1) preds[,j] <- ((1 - exp(-k*age_seq[j]))^b)^eta * exp(stem)
    
    if (freelist == 1) {
      preds_resp[,j] <- rnbinom( n_samps, mu=((1 - exp(-k*age_seq[j]))^b)^eta * exp(stem), size=post$phi_free[,freelist_key$freelist_id[freelist_key$skill == skill]] )
    }
    
  }
  
  quants <- PI/4
  
  PI_1 <- apply(preds, 2, PI, prob=quants)
  PI_2 <- apply(preds, 2, PI, prob=quants*2)
  PI_3 <- apply(preds, 2, PI, prob=quants*3)
  PI_4 <- apply(preds, 2, PI, prob=quants*4)
  med <- apply(preds, 2, median)
  
  if (freelist == 1) var_90 <- apply(preds_resp, 2, PI, prob=0.9)
  
  # Initializing new plot if needed
  if (add == F) {
    if (freelist==0 & !(substr(skill,1,3) %in% c("B_a", "H_a", "B_p", "H_p"))) plot(NULL, xlim=c(0,1), ylim=c(-0.05,1.05), xlab="Age", ylab="Pr(Endorse Skill)", axes=F)
    if (freelist==0 & substr(skill,1,3) %in% c("B_a", "H_a", "B_p", "H_p")) plot(NULL, xlim=c(0,1), ylim=c(-0.05,1.05), xlab="Age", ylab="Pr(Identify Species)",axes=F)
    
    if (freelist==1) {
      plot(NULL, xlim=c(0,1), ylim=c(0,PI(preds_resp, 0.97)[2]), xlab="Age", ylab="Freelist Sum", axes=F)
      axis(2, at=round(seq(from=0, to=PI(preds_resp, 0.97)[2], length.out = 4)))
    }
    else axis(2, at=c(0, 0.5, 1))
    
    axis(1, at=c(0,0.2,0.4,0.6,0.8,1), labels=80*c(0,0.2,0.4,0.6,0.8,1))
    
    # Title of task + median rank
    if (is.na(both_skills$med_rank[both_skills$skill == skill])) mtext( paste(both_skills$skill2[both_skills$skill == skill], "Not ranked", sep="\n"),line=0.5, cex=1.1)

    else mtext( paste(both_skills$skill2[both_skills$skill == skill], paste0("Med. Difficulty Rank = ", both_skills$med_rank[both_skills$skill == skill]), sep="\n"),line=0.5, cex=1.1)
    
  }
  
  if (quantiles == T) {
    shade(PI_4, age_seq, col=col.alpha(color, 0.1))
    shade(PI_3, age_seq, col=col.alpha(color, 0.1))
    shade(PI_2, age_seq, col=col.alpha(color, 0.1))
    shade(PI_1, age_seq, col=col.alpha(color, 0.1))
  }
  else shade(PI_4, age_seq, col=col.alpha(color, 0.2))
  
  lines(x=age_seq, y=med, col=color, lwd=2)
  if (freelist==1)  {
    lines(x=age_seq, y=var_90[1,], lty="dashed", col=color)
    lines(x=age_seq, y=var_90[2,], lty="dashed", col=color)
  }
  
  ## Adding raw data
  skill_name <- skill
  raw_d <- d %>% filter(skill==skill_name, sex==sex_d)
  
  if (freelist == 0) {
    raw_d$y <- ifelse(raw_d$y == 1, 1.05, raw_d$y)
    raw_d$y <- ifelse(raw_d$y == 0, -0.05, raw_d$y)
  }
  
  if (plot.data == T) {
    if (substr(skill,1,3) %in% c("B_a", "H_a", "B_p", "H_p")) points(x=jitter(raw_d$age/80, amount=1/80), y=raw_d$y, col=col.alpha(color, 0.05), pch=16)
    
    else points(x=jitter(raw_d$age/80, amount=1/80), y=raw_d$y, col=col.alpha(color, 0.4), pch=16)
  }
}

skill_plot(skill="B_animal", culture="BaYaka", sex="Male")

#### Plotting all BaYaka Skills #### 
dev.off()
svg("BaYaka_skills_1.svg", 
    width=8.5, 
    height=11, 
    pointsize=12)

par(mfrow=c(3,3), cex=0.9)
for (s in 1:9) {
  skill_plot(skill=Bskills$skill[s], culture="BaYaka", sex="Male", color="orange", quantiles = T)
  skill_plot(skill=Bskills$skill[s], culture="BaYaka", sex="Female", color="slategray", add=T, quantiles = T)
  
  if (s == 1) legend(0.25, 0.5, legend=c("Males", "Females"),
                     col=c("orange", "slategray"), bg="transparent", lty=1, lwd=4)
  
}
dev.off()

svg("BaYaka_skills_2.svg", 
    width=8.5, 
    height=11, 
    pointsize=12)

par(mfrow=c(3,3), cex=0.9)
for (s in 10:18) {
  skill_plot(skill=Bskills$skill[s], culture="BaYaka", sex="Male", color="orange", quantiles = T)
  skill_plot(skill=Bskills$skill[s], culture="BaYaka", sex="Female", color="slategray", add=T, quantiles = T)
  
  if (s == 10) legend(0.25, 0.5, legend=c("Males", "Females"),
                     col=c("orange", "slategray"), bg="transparent", lty=1, lwd=4)
}
dev.off()

svg("BaYaka_skills_3.svg", 
    width=8.5, 
    height=11, 
    pointsize=12)

par(mfrow=c(3,3), cex=0.9)
for (s in 19:27) {
  skill_plot(skill=Bskills$skill[s], culture="BaYaka", sex="Male", color="orange", quantiles = T)
  skill_plot(skill=Bskills$skill[s], culture="BaYaka", sex="Female", color="slategray", add=T, quantiles = T)
  
  if (s == 19) legend(0.25, 22, legend=c("Males", "Females"),
                     col=c("orange", "slategray"), bg="transparent", lty=1, lwd=4)
}
dev.off()

#### Plotting all Hadza Skills #### 
svg("Hadza_skills_1.svg", 
    width=8.5, 
    height=11, 
    pointsize=12)

par(mfrow=c(3,3), cex=0.9)

for (s in 1:9) {
  skill_plot(skill=Hskills$skill[s], culture="Hadza", sex="Male", color="orange", quantiles = T)
  skill_plot(skill=Hskills$skill[s], culture="Hadza", sex="Female", color="slategray", add=T, quantiles = T)
  
  if (s == 1) legend(0.25, 0.5, legend=c("Males", "Females"),
                      col=c("orange", "slategray"), bg="transparent", lty=1, lwd=4)
}
dev.off()

svg("Hadza_skills_2.svg", 
    width=8.5, 
    height=11, 
    pointsize=12)

par(mfrow=c(3,3), cex=0.9)

for (s in 10:nrow(Hskills)) {
  skill_plot(skill=Hskills$skill[s], culture="Hadza", sex="Male", color="orange", quantiles = T)
  skill_plot(skill=Hskills$skill[s], culture="Hadza", sex="Female", color="slategray", add=T, quantiles = T)
  
  if (s == 10) legend(0.25, 0.5, legend=c("Males", "Females"),
                      col=c("orange", "slategray"), bg="transparent", lty=1, lwd=4)
}
dev.off()
#####################################################

#####################################################
#### Sex differences in task difficulty ranking #####
# bring in sex-specific ranking data
B_sex <- readxl::read_xlsx("data-raw/forced pair.xlsx", sheet = 1)
H_sex <- readxl::read_xlsx("data-raw/forced pair.xlsx", sheet = 2)

# Wrangle into long
B <- as.data.frame(t(B_sex[-1,]), stringsAsFactors = F)[-1,]
colnames(B) <- unlist(B_sex[-1,1])
B$sex <- as.character(B_sex[1,-1])
B$rater <- paste0("B",1:nrow(B))

B_long <- B %>% pivot_longer(-c(rater, sex))
B_long$culture <- "BaYaka"

H <- as.data.frame(t(H_sex[-1,]), stringsAsFactors = F)[-1,]
colnames(H) <- unlist(H_sex[-1,1])
H$sex <- as.character(H_sex[1,-1])
H$rater <- paste0("H",1:nrow(H))

H_long <- H %>% pivot_longer(-c(rater, sex))
H_long$culture <- "Hadza"

# Bring them together, give tasks better labels
rating_long <- bind_rows(B_long, H_long)
pub_labels <- read_csv("figure_labels.csv")
rating_long$task <- pub_labels$`Change to`[match(rating_long$name, pub_labels$`In figure`)]
rating_long$value <- as.numeric(rating_long$value)

# med by culture and sex
rating_summary <- rating_long %>% group_by(culture, sex, task) %>% summarise(value = median((value)))

svg( file="Supp_sex_rank.svg", width=8.5, height=8, pointsize=12 )

ggplot(rating_long, aes(x=fct_reorder(task,value), y=value, color=sex, group=1)) + facet_wrap(~culture, scales="free") + geom_jitter(width=0, alpha=0.35) + stat_summary(aes(y=value, group=sex), fun="mean", geom="line", lwd=1) + coord_flip() + ylab("Difficulty Ranking") + xlab("") + theme_bw(base_size=15) + scale_color_manual(values=c("slategray", "orange")) + theme(strip.background = element_rect(fill="white"))

dev.off()
########################################################
##### Additional parameter estimates ###################
## rate of learning (k) by task for each population ####
# BaYaka
B_k <- matrix( NA, nrow=n_samps, ncol=N_skillB )
for (j in 1:ncol(B_k)) {
  B_k[,j] <- exp( post$ak + post$skillB_v[,j,1] )
}

B_k <- as.data.frame(B_k)
names(B_k) <- Bskills$skill2[order(Bskills$ind)]
B_k <- B_k %>% mutate(samp=1:n_samps, culture=rep("BaYaka", n_samps), par=rep("k", n_samps)) %>% pivot_longer(-c(samp, culture, par))

# Hadza
H_k <- matrix( NA, nrow=n_samps, ncol=N_skillH )
for (j in 1:ncol(H_k)) {
  H_k[,j] <- exp( post$ak + post$skillH_v[,j,1] )
}

H_k <- as.data.frame(H_k)
names(H_k) <- Hskills$skill2[order(Hskills$ind)]
H_k <- H_k %>% mutate(samp=1:n_samps, culture=rep("Hadza", n_samps), par=rep("k", n_samps)) %>% pivot_longer(-c(samp, culture, par))

## elasticity of learning (b) by task for each population ####
# BaYaka
B_b <- matrix( NA, nrow=n_samps, ncol=N_skillB )
for (j in 1:ncol(B_b)) {
  B_b[,j] <- exp( post$ab + post$skillB_v[,j,2] )
}

B_b <- as.data.frame(B_b)
names(B_b) <- Bskills$skill2[order(Bskills$ind)]
B_b <- B_b %>% mutate(samp=1:n_samps, culture=rep("BaYaka", n_samps), par=rep("b", n_samps)) %>% pivot_longer(-c(samp, culture, par))

# Hadza
H_b <- matrix( NA, nrow=n_samps, ncol=N_skillH )
for (j in 1:ncol(H_b)) {
  H_b[,j] <- exp( post$ab + post$skillH_v[,j,2] )
}

H_b <- as.data.frame(H_b)
names(H_b) <- Hskills$skill2[order(Hskills$ind)]
H_b <- H_b %>% mutate(samp=1:n_samps, culture=rep("Hadza", n_samps), par=rep("b", n_samps)) %>% pivot_longer(-c(samp, culture, par))

## elasticity of knowledge (eta) by task for each population ####
# BaYaka
B_eta <- matrix( NA, nrow=n_samps, ncol=N_skillB )
for (j in 1:ncol(B_eta)) {
  B_eta[,j] <- exp( post$ae + post$skillB_v[,j,3] )
}

B_eta <- as.data.frame(B_eta)
names(B_eta) <- Bskills$skill2[order(Bskills$ind)]
B_eta <- B_eta %>% mutate(samp=1:n_samps, culture=rep("BaYaka", n_samps), par=rep("eta", n_samps)) %>% pivot_longer(-c(samp, culture, par))

# Hadza
H_eta <- matrix( NA, nrow=n_samps, ncol=N_skillH )
for (j in 1:ncol(H_eta)) {
  H_eta[,j] <- exp( post$ae + post$skillH_v[,j,3] )
}

H_eta <- as.data.frame(H_eta)
names(H_eta) <- Hskills$skill2[order(Hskills$ind)]
H_eta <- H_eta %>% mutate(samp=1:n_samps, culture=rep("Hadza", n_samps), par=rep("eta", n_samps)) %>% pivot_longer(-c(samp, culture, par))

### Put all the age-structured parameters together ####
age_pars <- bind_rows(B_k, H_k, B_b, H_b, B_eta, H_eta)

age_par_summary <- age_pars %>% group_by(culture, name, par) %>% summarise(med = median(value), lower=HPDI(value, prob=0.9)[1], upper=HPDI(value, prob=0.9)[2])

svg("learning_pars.svg", height=9, width=8, pointsize=12)
ggplot(age_par_summary, aes(x=med, y=name, color=culture)) + facet_grid(culture ~ fct_rev(par), scales="free") + geom_point() + geom_errorbarh(aes(y=name, xmin=lower, xmax=upper), height=0, lwd=1) + theme_bw(base_size = 14) + theme(legend.title = element_blank(), strip.background = element_rect(fill="white", color="black"), legend.position = "none") + xlab("") + ylab("") + scale_color_manual(values=c("seagreen", "cornflowerblue")) + xlab("")
dev.off()

##########################################
#### Pathway of Learning #################
logsumexp <- function (x) {
  y = max(x)
  y + log(sum(exp(x - y)))
}

softmax2 <- function (x) {
  exp(x - logsumexp(x))
}

B_hor <- matrix( NA, nrow=n_samps, ncol=N_skillB )
B_obl <- matrix( NA, nrow=n_samps, ncol=N_skillB )
B_vert <- matrix( NA, nrow=n_samps, ncol=N_skillB )

H_hor <- matrix( NA, nrow=n_samps, ncol=N_skillH )
H_obl <- matrix( NA, nrow=n_samps, ncol=N_skillH )
H_vert <- matrix( NA, nrow=n_samps, ncol=N_skillH )

for ( j in 1:N_skillB ) {
  pathway <- cbind( post$at[,1] + (post$sexcult_v[,3,2] + post$sexcult_v[,4,2])/2 + post$skillB_v[,j,6], post$at[,2] + (post$sexcult_v[,3,3] + post$sexcult_v[,4,3])/2 + post$skillB_v[,j,7], 0 )
  
  for (i in 1:nrow(pathway)) pathway[i,] <- softmax2(pathway[i,])
  
  B_hor[,j] = pathway[,1]
  B_obl[,j] = pathway[,2]
  B_vert[,j] = pathway[,3]
}

for ( j in 1:N_skillH ) {
  pathway <- cbind( post$at[,1] + (post$sexcult_v[,1,2] + post$sexcult_v[,2,2])/2 + post$skillH_v[,j,6], post$at[,2] + (post$sexcult_v[,1,2] + post$sexcult_v[,2,2])/2 + post$skillH_v[,j,7], 0 )
  
  for (i in 1:nrow(pathway)) pathway[i,] <- softmax2(pathway[i,])
  
  H_hor[,j] = pathway[,1]
  H_obl[,j] = pathway[,2]
  H_vert[,j] = pathway[,3]
}

# Add skill names
H_hor <- as.data.frame(H_hor);names(H_hor) <- Hskills$skill2[order(Hskills$ind)]
H_obl <- as.data.frame(H_obl);names(H_obl) <- Hskills$skill2[order(Hskills$ind)]
H_vert <- as.data.frame(H_vert);names(H_vert) <- Hskills$skill2[order(Hskills$ind)]

H_hor <- H_hor %>% mutate(samp=1:n_samps, culture=rep("Hadza", n_samps), par=rep("Horizontal", n_samps)) %>% pivot_longer(-c(samp, culture, par))
H_obl <- H_obl %>% mutate(samp=1:n_samps, culture=rep("Hadza", n_samps), par=rep("Oblique", n_samps)) %>% pivot_longer(-c(samp, culture, par))
H_vert <- H_vert %>% mutate(samp=1:n_samps, culture=rep("Hadza", n_samps), par=rep("Vertical", n_samps)) %>% pivot_longer(-c(samp, culture, par))

B_hor <- as.data.frame(B_hor);names(B_hor) <- Bskills$skill2[order(Bskills$ind)]
B_obl <- as.data.frame(B_obl);names(B_obl) <- Bskills$skill2[order(Bskills$ind)]
B_vert <- as.data.frame(B_vert);names(B_vert) <- Bskills$skill2[order(Bskills$ind)]

B_hor <- B_hor %>% mutate(samp=1:n_samps, culture=rep("BaYaka", n_samps), par=rep("Horizontal", n_samps)) %>% pivot_longer(-c(samp, culture, par))
B_obl <- B_obl %>% mutate(samp=1:n_samps, culture=rep("BaYaka", n_samps), par=rep("Oblique", n_samps)) %>% pivot_longer(-c(samp, culture, par))
B_vert <- B_vert %>% mutate(samp=1:n_samps, culture=rep("BaYaka", n_samps), par=rep("Vertical", n_samps)) %>% pivot_longer(-c(samp, culture, par))

path_long <- bind_rows(H_hor, H_obl, H_vert, B_hor, B_obl, B_vert)
path_summary <- path_long %>% group_by(culture, par, name) %>% summarise(med = median(value), lower=HPDI(value, prob=0.9)[1], upper=HPDI(value, prob=0.9)[2])

svg("path_pars.svg", height=9, width=8, pointsize=12)
ggplot(path_summary, aes(x=med, y=name, color=culture)) + facet_grid(culture ~ fct_rev(par), scales="free_y") + geom_point() + geom_errorbarh(aes(y=name, xmin=lower, xmax=upper), height=0, lwd=1) + theme_bw(base_size = 14) + theme(legend.title = element_blank(), strip.background = element_rect(fill="white", color="black"), legend.position = "none") + xlab("") + ylab("") + scale_color_manual(values=c("seagreen", "cornflowerblue")) + xlab("Probability of Transmission Pathway")  + scale_x_continuous(breaks=c(0,0.25,0.5,0.75,1), labels=c(0,0.25,0.5,0.75,1))
dev.off()


##########################################
#### Method of Learning ##############
logsumexp <- function (x) {
  y = max(x)
  y + log(sum(exp(x - y)))
}

softmax2 <- function (x) {
  exp(x - logsumexp(x))
}

B_obs <- matrix( NA, nrow=n_samps, ncol=N_skillB )
B_teach <- matrix( NA, nrow=n_samps, ncol=N_skillB )
B_ind <- matrix( NA, nrow=n_samps, ncol=N_skillB )

H_obs <- matrix( NA, nrow=n_samps, ncol=N_skillH )
H_teach <- matrix( NA, nrow=n_samps, ncol=N_skillH )
H_ind <- matrix( NA, nrow=n_samps, ncol=N_skillH )

for ( j in 1:N_skillB ) {
  method <- cbind( post$am[,1] + (post$sexcult_v[,3,4] + post$sexcult_v[,4,4])/2 + post$skillB_v[,j,8], post$am[,2] + (post$sexcult_v[,3,5] + post$sexcult_v[,4,5])/2 + post$skillB_v[,j,9], 0 )
  
  for (i in 1:nrow(method)) method[i,] <- softmax2(method[i,])
  
  B_obs[,j] = method[,1]
  B_teach[,j] = method[,2]
  B_ind[,j] = method[,3]
}

for ( j in 1:N_skillH ) {
  method <- cbind( post$am[,1] + (post$sexcult_v[,1,4] + post$sexcult_v[,2,4])/2 + post$skillH_v[,j,8], post$am[,2] + (post$sexcult_v[,1,5] + post$sexcult_v[,2,5])/2 + post$skillH_v[,j,9], 0 )
  
  for (i in 1:nrow(method)) method[i,] <- softmax2(method[i,])
  
  H_obs[,j] = method[,1]
  H_teach[,j] = method[,2]
  H_ind[,j] = method[,3]
}

# Add skill names
H_obs <- as.data.frame(H_obs);names(H_obs) <- Hskills$skill2[order(Hskills$ind)]
H_teach <- as.data.frame(H_teach);names(H_teach) <- Hskills$skill2[order(Hskills$ind)]
H_ind <- as.data.frame(H_ind);names(H_ind) <- Hskills$skill2[order(Hskills$ind)]

H_obs <- H_obs %>% mutate(samp=1:n_samps, culture=rep("Hadza", n_samps), par=rep("Observation", n_samps)) %>% pivot_longer(-c(samp, culture, par))
H_teach <- H_teach %>% mutate(samp=1:n_samps, culture=rep("Hadza", n_samps), par=rep("Teaching", n_samps)) %>% pivot_longer(-c(samp, culture, par))
H_ind <- H_ind %>% mutate(samp=1:n_samps, culture=rep("Hadza", n_samps), par=rep("Individual", n_samps)) %>% pivot_longer(-c(samp, culture, par))

B_obs <- as.data.frame(B_obs);names(B_obs) <- Bskills$skill2[order(Bskills$ind)]
B_teach <- as.data.frame(B_teach);names(B_teach) <- Bskills$skill2[order(Bskills$ind)]
B_ind <- as.data.frame(B_ind);names(B_ind) <- Bskills$skill2[order(Bskills$ind)]

B_obs <- B_obs %>% mutate(samp=1:n_samps, culture=rep("BaYaka", n_samps), par=rep("Observation", n_samps)) %>% pivot_longer(-c(samp, culture, par))
B_teach <- B_teach %>% mutate(samp=1:n_samps, culture=rep("BaYaka", n_samps), par=rep("Teaching", n_samps)) %>% pivot_longer(-c(samp, culture, par))
B_ind <- B_ind %>% mutate(samp=1:n_samps, culture=rep("BaYaka", n_samps), par=rep("Individual", n_samps)) %>% pivot_longer(-c(samp, culture, par))

method_long <- bind_rows(H_obs, H_teach, H_ind, B_obs, B_teach, B_ind)
method_summary <- method_long %>% group_by(culture, par, name) %>% summarise(med = median(value), lower=HPDI(value, prob=0.9)[1], upper=HPDI(value, prob=0.9)[2])

svg("method_pars.svg", height=9, width=8, pointsize=12)
ggplot(method_summary, aes(x=med, y=name, color=culture)) + facet_grid(culture ~ fct_rev(par), scales="free_y") + geom_point() + geom_errorbarh(aes(y=name, xmin=lower, xmax=upper), height=0, lwd=1) + theme_bw(base_size = 14) + theme(legend.title = element_blank(), strip.background = element_rect(fill="white", color="black"), legend.position = "none") + xlab("") + ylab("") + scale_color_manual(values=c("seagreen", "cornflowerblue")) + xlab("Probability of Learning Method")
dev.off()

##########################################
#### Reliability supplement ##############

# We want to calc the reliability of the rankings among rankers`
# N_r = number of rankers
# c_hat = average covariance between rankers
# v_hat = average variance within rankers

# alpha = (N_r*c_hat) / (v_hat + (N_r-1)*c_hat)

##################################
## Set graphical pars ##
{
svg("reliability_supp.svg", height=9, width=8, pointsize=12)
par(mfcol=c(2,2))

##### Check BaYaka data first ####
N_r <- nrow(BaYaka_rank)

c <- cov( t(BaYaka_rank) ) # transposed matrix to get between-rater covariance
c_hat <- mean(c[lower.tri(c)])

var <- apply(t(BaYaka_rank), 2, var) # variance of items
v_hat <- mean(var)

alpha <- (N_r*c_hat) / (v_hat + (N_r-1)*c_hat)
round(alpha, 2)

#### How much variance vs expected variance in ranks due to chance?
N_ranks <- ncol(BaYaka_rank)

# Individual rankings
sorted_tasks <- BaYaka_rank[,names(sort(apply(BaYaka_rank, 2, median)))] * N_ranks

plot(apply((sorted_tasks), 2, mean), type="l", lwd=2, ylim=c(0,N_ranks), ylab="Rank", xlab="Ranked Task (sorted low to high)", col="seagreen")
mtext( expression("(BaYaka) Cronbach's" ~ alpha ~ "= 0.91" ))
for (i in 1:nrow(sorted_tasks)) lines(x=1:N_ranks, y=sorted_tasks[i,], col=col.alpha("seagreen", 0.5))

# First, calc the expected variance under null model
n_sims <- 5000
null_var <- c()
for (i in 1:n_sims) {
  rank <- matrix(NA, nrow=N_ranks, ncol=N_r)
  for (j in 1:N_r) rank[,j] <- sample( 1:N_ranks, size=N_ranks, replace=F )
  
  null_var[i] <- mean(apply(t(rank), 2, var))
}

# Compare to actual variance
B_var <- apply(BaYaka_rank*N_ranks, 2, var)

hist( B_var / mean(null_var) , main = "Observed Rank Variance / Null Rank Variance", xlab="")
mtext("BaYaka")
abline(v=1, lty="dashed", col="red")

##### Then Hadza ####
N_r <- nrow(Hadza_rank)

c <- cov( t(Hadza_rank) ) # transposed matrix to get between-rater covariance
c_hat <- mean(c[lower.tri(c)])

var <- apply(t(Hadza_rank), 2, var) # variance of items
v_hat <- mean(var)

alpha <- (N_r*c_hat) / (v_hat + (N_r-1)*c_hat)
round(alpha, 2)

#### How much variance vs expected variance in ranks due to chance?
N_ranks <- ncol(Hadza_rank)

# Individual ranks
sorted_tasks <- Hadza_rank[,names(sort(apply(Hadza_rank, 2, median)))] * N_ranks

plot(apply((sorted_tasks), 2, mean), type="l", lwd=2, ylim=c(0,N_ranks), ylab="Rank", xlab="Ranked Task (sorted low to high)", col="cornflowerblue")
mtext( expression("(Hadza) Cronbach's" ~ alpha ~ "= 0.88" ))
for (i in 1:nrow(sorted_tasks)) lines(x=1:N_ranks, y=sorted_tasks[i,], col=col.alpha("cornflowerblue", 0.5))

# First, calc the expected variance under null model
n_sims <- 5000
null_var <- c()
for (i in 1:n_sims) {
  rank <- matrix(NA, nrow=N_ranks, ncol=N_r)
  for (j in 1:N_r) rank[,j] <- sample( 1:N_ranks, size=N_ranks, replace=F )
  
  null_var[i] <- mean(apply(t(rank), 2, var))
}

# Compare to actual variance
H_var <- apply(Hadza_rank*N_ranks, 2, var)

hist( H_var / mean(null_var) , main = "Observed Rank Variance / Null Rank Variance", xlab="")
abline(v=1, lty="dashed", col="red")
mtext("Hadza")

dev.off()
} # end reliability plot

##########################################
#### Social learning individual ternary plots
svg("tranmission_pathway.svg", width=7, height=5, pointsize=12)
tern_path
dev.off()

svg("method_transmission.svg", width=7, height=5, pointsize=12)
method_tern
dev.off()