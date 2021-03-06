data{
int N_obs; // total obs
int N_H; // num Hadza
int N_B; // num BaYaka
int N_HR; // num Hadza rankers
int N_BR; 
int N_skillH; // num Hadza skills
int N_skillB; 
int N_animalH; // num Hadza animals (explicit)
int N_animalB;
int N_plantH; // num Hadza plants
int N_plantB;

int y[N_obs]; // outcome
int expl[N_obs]; // indicator of explicit knowledge
int freelist[N_obs]; // indicator of freelist
int freelist_id[N_obs]; // which free list
int Hadza[N_obs]; // indicator

int id[N_obs];
int skill[N_obs];
int animal_id[N_obs];
int plant_id[N_obs];
int sex[N_obs];
int path[N_obs];
int transmission[N_obs];
int sex_learn[N_obs];
int sexcult[N_obs];

real Hadza_rank[N_HR, N_skillH - 2]; // all skills minus unranked skills
real BaYaka_rank[N_BR, N_skillB - 5];

real age_muH[N_H]; // mean Hadza age estimate
real age_seH[N_H]; // sd Hadza age estimate
real age_muB[N_B]; // mean BaYaka age estimate
real age_seB[N_B]; // sd BaYaka age estimate

int K_methods; // number of transmision methods
int K_paths; // number of transmision pathways
}

parameters{
///// Biological age parameters ///////////////////////  
vector<lower=0>[N_H] age_estH; // scaled age-at-interview
vector<lower=0>[N_B] age_estB;

///// Main effects //////////////////////  
real ap; // baseline probability of skill availability
real ak; // mean rate of learning
real ab; // mean elasticity of skill on learning
real ae; // mean elasticity of mu on skill
vector[K_paths-1] at; // transmission path probabilities
vector[K_paths-1] am; // transmission method probabilities
real as; // prob male transmission

// Implicit vs tacit knowledge pars
real p_expl;
real e_expl;

// age effects on transmission path
vector[K_paths-1] tH_age;
vector[K_paths-1] tB_age;
vector[K_paths-1] t2H_age;
vector[K_paths-1] t2B_age;

// age effects on transmission method
vector[K_methods-1] mH_age;
vector[K_methods-1] mB_age;
vector[K_methods-1] m2H_age;
vector[K_methods-1] m2B_age;

// age effects on sex-biased transmission
real sH_age;
real sB_age;
real s2H_age;
real s2B_age;

// random effects, unscaled
matrix[K_paths + 4,N_H] idH_z; // individual variation
matrix[K_paths + 4,N_B] idB_z;

matrix[(8 + K_paths),N_skillH] skillH_z;  // skill-specific variation
matrix[(8 + K_paths),N_skillB] skillB_z;

matrix[(3 + K_paths),max(sexcult)] sexcult_z; // random effects specific to sex*culture

matrix[1,N_plantH] plantH_z; // plant-specific variation in explicit knowledge
matrix[1,N_plantB] plantB_z;
matrix[1,N_animalH] animalH_z; // animal-specific variation in explicit knowledge
matrix[1,N_animalB] animalB_z;

real a_freelist; // intercept for freelist tasks
vector<lower=0>[max(freelist_id)] phi_free; // inverse dispersion parameter

matrix[N_skillH,N_H] resH_z;  // residual random effects, unscaled and uncorrelated
matrix[N_skillB,N_B] resB_z;  // residual random effects, unscaled and uncorrelated

// variance components
vector<lower=0>[K_paths + 4] sigma_idH;
vector<lower=0>[K_paths + 4] sigma_idB;

vector<lower=0>[(8 + K_paths)] sigma_skillH;
vector<lower=0>[(8 + K_paths)] sigma_skillB;

vector<lower=0>[(3 + K_paths)] sigma_sexcult;

vector<lower=0>[1] sigma_plantH;
vector<lower=0>[1] sigma_plantB;
vector<lower=0>[1] sigma_animalH;
vector<lower=0>[1] sigma_animalB;

vector<lower=0>[N_skillH] sigma_resH; // scaling of residual correlations
vector<lower=0>[N_skillB] sigma_resB; // scaling of residual correlations

// mean rank
real a_HR;
real a_BR;
// individual variance in ranking order
real<lower=0> sigma_HR;
real<lower=0> sigma_BR;

// Correlations between parameters, cholesky decomposition
cholesky_factor_corr[(8 + K_paths)] L_skillH;
cholesky_factor_corr[(8 + K_paths)] L_skillB;
cholesky_factor_corr[(4 + K_paths)] L_idH;
cholesky_factor_corr[(4 + K_paths)] L_idB;

cholesky_factor_corr[N_skillH] L_resH;  // residual correlation matrix
cholesky_factor_corr[N_skillB] L_resB;  // residual correlation matrix

// age measurement error parameters
real a_age_H;
real a_age_B;
real<lower=0> sigma_age_H;
real<lower=0> sigma_age_B;
}

transformed parameters{
matrix[N_H,K_paths + 4] idH_v; // note the transposed dimensions
matrix[N_B,K_paths + 4] idB_v;

matrix[N_H, N_skillH] resH_v; 
matrix[N_B, N_skillB] resB_v; 

matrix[N_skillH,(8 + K_paths)] skillH_v;
matrix[N_skillB,(8 + K_paths)] skillB_v;

matrix[N_plantH,1] plantH_v;
matrix[N_plantB,1] plantB_v;
matrix[N_animalH,1] animalH_v;
matrix[N_animalB,1] animalB_v;

matrix[max(sexcult),(3 + K_paths)] sexcult_v;

///////////////////////////////////////////////////////////////////
///// Scaling and correlating random effects
idH_v = (diag_pre_multiply(sigma_idH, L_idH) * idH_z)';
idB_v = (diag_pre_multiply(sigma_idB, L_idB) * idB_z)';

skillH_v = (diag_pre_multiply(sigma_skillH, L_skillH) * skillH_z)';
skillB_v = (diag_pre_multiply(sigma_skillB, L_skillB) * skillB_z)';

resH_v = (diag_pre_multiply(sigma_resH, L_resH) * resH_z)';
resB_v = (diag_pre_multiply(sigma_resB, L_resB) * resB_z)';

// The other random effects are not correlated across responses
sexcult_v = (diag_matrix(sigma_sexcult) * sexcult_z)';
plantH_v = (diag_matrix(sigma_plantH) * plantH_z)';
plantB_v = (diag_matrix(sigma_plantB) * plantB_z)';
animalH_v = (diag_matrix(sigma_animalH) * animalH_z)';
animalB_v = (diag_matrix(sigma_animalB) * animalB_z)';

} // end transformed parameters block

model{
vector[N_obs] k; // learning rate
vector[N_obs] b; // elasticity of knowledge on skill
vector[N_obs] K; // subsistence knowledge
vector[N_obs] eta; // elasticity for given domain on skill  
vector[N_obs] mu; // linear model

//////////////////////////////////////////////////////////
// Biological age with gaussian measurement error, scaled by 80 years olds
age_muH ~ normal(age_estH, age_seH);
age_estH ~ normal(a_age_H, sigma_age_H);

age_muB ~ normal(age_estB, age_seB);
age_estB ~ normal(a_age_B, sigma_age_B);

///// Priors //////////////////////////////////////////////
ap ~ std_normal(); // baseline probability of pursuing skill
ab ~ std_normal();
ak ~ std_normal();
ae ~ std_normal();
am ~ std_normal();
at ~ std_normal();
as ~ std_normal();

// effect of explicit vs tacit knowledge
p_expl ~ std_normal();
e_expl ~ std_normal();

// linear effects of age on transmission
tH_age ~ std_normal();
sH_age ~ std_normal();
tB_age ~ std_normal();
sB_age ~ std_normal();
mH_age ~ std_normal();
mB_age ~ std_normal();
t2H_age ~ std_normal();
s2H_age ~ std_normal();
t2B_age ~ std_normal();
s2B_age ~ std_normal();
m2H_age ~ std_normal();
m2B_age ~ std_normal();

a_HR ~ std_normal();
a_BR ~ std_normal();
sigma_HR ~ exponential(1);
sigma_BR ~ exponential(1);

// Age measurement error parameters
a_age_H ~ std_normal();
a_age_B ~ std_normal();
sigma_age_H ~ exponential(1);
sigma_age_B ~ exponential(1);

/// random effects ///////////
to_vector(idH_z) ~ std_normal();  // individual diff random effects, unscaled and uncorelated
to_vector(skillH_z) ~ std_normal(); // skill diff random effects, unscaled and uncorrelated
to_vector(idB_z) ~ std_normal();
to_vector(skillB_z) ~ std_normal();
to_vector(sexcult_z) ~ std_normal(); // sex*culture differences
to_vector(plantB_z) ~ std_normal();
to_vector(plantH_z) ~ std_normal();
to_vector(animalB_z) ~ std_normal();
to_vector(animalH_z) ~ std_normal();
to_vector(resH_z) ~ std_normal();
to_vector(resB_z) ~ std_normal();

// scale parameters for random effects
sigma_idH ~ exponential(1); 
sigma_idB ~ exponential(1);
sigma_skillH ~ exponential(1);
sigma_skillB ~ exponential(1);
sigma_sexcult ~ exponential(1);
sigma_plantH ~ exponential(1);
sigma_plantB ~ exponential(1);
sigma_animalH ~ exponential(1);
sigma_animalB ~ exponential(1);
sigma_resH ~ exponential(1);
sigma_resB ~ exponential(1);

L_idH ~ lkj_corr_cholesky(4);
L_idB ~ lkj_corr_cholesky(4);
L_skillH ~ lkj_corr_cholesky(4);
L_skillB ~ lkj_corr_cholesky(4);
L_resH ~ lkj_corr_cholesky(4);
L_resB ~ lkj_corr_cholesky(4);

// free-list parameters
a_freelist ~ std_normal();
phi_free ~ exponential(1);

////////// Subsistence knowledge model /////////////////////////////
for (i in 1:N_obs) {

// Rate of learning model
if (Hadza[i] == 1) k[i] = exp( ak + skillH_v[skill[i],1] );
if (Hadza[i] == 0) k[i] = exp( ak + skillB_v[skill[i],1] );

// knowledge Elasticity model
if (Hadza[i] == 1) b[i] = exp( ab + skillH_v[skill[i],2] );
if (Hadza[i] == 0) b[i] = exp( ab + skillB_v[skill[i],2] );

// Subsistence knowledge model
if (Hadza[i] == 1) K[i] = pow(1 - exp(-k[i]*age_estH[id[i]]),b[i]);
if (Hadza[i] == 0) K[i] = pow(1 - exp(-k[i]*age_estB[id[i]]),b[i]);

// Elasticity
if (Hadza[i] == 1) eta[i] = exp( ae + e_expl*expl[i] + skillH_v[skill[i],3] );
if (Hadza[i] == 0) eta[i] = exp( ae + e_expl*expl[i] + skillB_v[skill[i],3] );

// Making linear model stems, culture and outcome specific /////
{
real l_stem = ap + p_expl*expl[i] + sexcult_v[sexcult[i],1];

if (Hadza[i] == 1) {
  l_stem = l_stem + resH_v[id[i],skill[i]] + idH_v[id[i],1]*(1-expl[i]) + idH_v[id[i],2]*expl[i] + skillH_v[skill[i],4] + skillH_v[skill[i],5]*sex[i];
  
  if (plant_id[i] > 0) l_stem = l_stem + plantH_v[plant_id[i],1];
  if (animal_id[i] > 0) l_stem = l_stem + animalH_v[animal_id[i],1];
}

if (Hadza[i] == 0) {
  l_stem = l_stem + resB_v[id[i],skill[i]] + idB_v[id[i],1]*(1-expl[i]) + idB_v[id[i],2]*expl[i] + skillB_v[skill[i],4] + skillB_v[skill[i],5]*sex[i];
  
  if (plant_id[i] > 0) l_stem = l_stem + plantB_v[plant_id[i],1];
  if (animal_id[i] > 0) l_stem = l_stem + animalB_v[animal_id[i],1];
}

if (freelist[i] == 1) l_stem = l_stem + a_freelist;

mu[i] = K[i]^eta[i] * exp(l_stem);
}
  ///// Likelihood functions /////////////////////////////////////
  if (freelist[i] == 0) y[i] ~ bernoulli( 2*(inv_logit( mu[i] ) - 0.5) );
  if (freelist[i] == 1) y[i] ~ neg_binomial_2( mu[i], phi_free[freelist_id[i]]);
}

/////////////////////////////////////////////////////////////////
//// Transmission path model //////////////////////////////////
for (i in 1:N_obs) {
    
  if (path[i] > 0) {
  vector[K_paths] pk; // prob of each transmission pathway
  
  if (Hadza[i] == 1) {
    
  for (P in 1:(K_paths-1)) pk[P] = at[P] + idH_v[id[i],(2+P)] + skillH_v[skill[i],(5+P)] + sexcult_v[sexcult[i],1+P] + tH_age[P]*age_estH[id[i]] + t2H_age[P]*square(age_estH[id[i]]);
  }
  
  if (Hadza[i] == 0) {
    
  for (P in 1:(K_paths-1)) pk[P] = at[P] + idB_v[id[i],(2+P)] + skillB_v[skill[i],(5+P)] + sexcult_v[sexcult[i],1+P] + tB_age[P]*age_estB[id[i]] + t2B_age[P]*square(age_estB[id[i]]);
  }
  
  pk[K_paths] = 0; // vertical learning is the ref. category
  
  path[i] ~ categorical_logit( pk );
  }
}

/////////////////////////////////////////////////////////////////
//// Transmission method model //////////////////////////////////
for (i in 1:N_obs) {
    
  if (transmission[i] > 0) {
  vector[K_methods] pk; // prob of each transmission method
  
  if (Hadza[i] == 1) {
    
  for (P in 1:(K_methods-1)) pk[P] = am[P] + idH_v[id[i],(4+P)] + skillH_v[skill[i],(7+P)] + sexcult_v[sexcult[i],3+P] + mH_age[P]*age_estH[id[i]] + m2H_age[P]*square(age_estH[id[i]]);
  }
  
  if (Hadza[i] == 0) {
    
  for (P in 1:(K_methods-1)) pk[P] = am[P] + idB_v[id[i],(4+P)] + skillB_v[skill[i],(7+P)] + sexcult_v[sexcult[i],3+P] + mB_age[P]*age_estB[id[i]] + m2B_age[P]*square(age_estB[id[i]]);
  }
  
  pk[K_methods] = 0; // individual learning is the ref. category
  
  transmission[i] ~ categorical_logit( pk );
  }
}
//////////////////////////////////////////////////////////////
//// Sex-biased learning model ///////////////////////////////
for (i in 1:N_obs) {
  if (sex_learn[i] > 0) {
  
    real pS;
  
  if (Hadza[i] == 1) {
    pS = as + idH_v[id[i],(4+K_paths)] + skillH_v[skill[i],(7+K_paths)] + sexcult_v[sexcult[i],3+K_paths] + sH_age*age_estH[id[i]] + s2H_age*square(age_estH[id[i]]);
  }
  
    if (Hadza[i] == 0) {
    pS = as + idB_v[id[i],(4+K_paths)] + skillB_v[skill[i],(7+K_paths)] + sexcult_v[sexcult[i],3+K_paths] + sB_age*age_estB[id[i]] + s2B_age*square(age_estB[id[i]]);
    }
    
  // If response is either male or female
  if (sex_learn[i] == 1 || sex_learn[i] == 2) (sex_learn[i]-1) ~ bernoulli_logit( pS );
  
  // If response is both sexes, mix over with equal probability
  if (sex_learn[i] == 3) target += log_mix(0.5, bernoulli_logit_lpmf(1 | pS), bernoulli_logit_lpmf(0 | pS));
}
}
//////////////////////////////////////////////////////////////
/////////// Rank model ///////////////////////////
for (n in 1:N_HR)
for (s in 1:(N_skillH - 2)) {
Hadza_rank[n,s] ~ normal(a_HR + skillH_v[s,(8+K_paths)], sigma_HR );
}

for (n in 1:N_HR)
for (s in 1:(N_skillB - 5)) {
BaYaka_rank[n,s] ~ normal(a_BR + skillB_v[s,(8+K_paths)], sigma_BR );
}
//////////////////////////////////////////////////////////////
} // end model block

generated quantities{
  matrix[(8 + K_paths),(8 + K_paths)] Rho_skillH;
  matrix[(4 + K_paths),(4 + K_paths)] Rho_idH;
  matrix[(8 + K_paths),(8 + K_paths)] Rho_skillB;
  matrix[(4 + K_paths),(4 + K_paths)] Rho_idB;
  matrix[N_skillH,N_skillH] Rho_resH;
  matrix[N_skillB,N_skillB] Rho_resB;
  
  // Recovering full correlation matrices
  Rho_skillH = L_skillH * L_skillH';
  Rho_idH = L_idH * L_idH';
  Rho_skillB = L_skillB * L_skillB';
  Rho_idB = L_idB * L_idB';
  Rho_resH = L_resH * L_resH';
  Rho_resB = L_resB * L_resB';
}
