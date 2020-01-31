data{
int N_obs; // total obs
int N_H; // num Hadza
int N_B; // num BaYaka
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

real age_muH[N_H]; // mean Hadza age estimate
real age_seH[N_H]; // sd Hadza age estimate
real age_muB[N_B]; // mean BaYaka age estimate
real age_seB[N_B]; // sd BaYaka age estimate
}

parameters{
///// Biological age parameters ///////////////////////  
vector<lower=0>[N_H] age_estH; // scaled age-at-interview
vector<lower=0>[N_B] age_estB;

///// Main effects //////////////////////  
real ap; // baseline probability of skill availability

// Implicit vs tacit knowledge pars
real p_expl;

// random effects, unscaled
matrix[2,N_H] idH_z; // individual variation
matrix[2,N_B] idB_z;

matrix[1,N_skillH] skillH_z;  // skill-specific variation
matrix[1,N_skillB] skillB_z;

matrix[1,N_plantH] plantH_z; // plant-specific variation in explicit knowledge
matrix[1,N_plantB] plantB_z;
matrix[1,N_animalH] animalH_z; // animal-specific variation in explicit knowledge
matrix[1,N_animalB] animalB_z;

real a_freelist; // intercept for freelist tasks
vector<lower=0>[max(freelist_id)] phi_free; // inverse dispersion parameter

// variance components
vector<lower=0>[2] sigma_idH;
vector<lower=0>[2] sigma_idB;

vector<lower=0>[1] sigma_skillH;
vector<lower=0>[1] sigma_skillB;

vector<lower=0>[1] sigma_plantH;
vector<lower=0>[1] sigma_plantB;
vector<lower=0>[1] sigma_animalH;
vector<lower=0>[1] sigma_animalB;

// Correlations between parameters, cholesky decomposition
cholesky_factor_corr[2] L_idH;
cholesky_factor_corr[2] L_idB;
}

transformed parameters{
matrix[N_H,2] idH_v; // note the transposed dimensions
matrix[N_B,2] idB_v;

matrix[N_skillH,1] skillH_v;
matrix[N_skillB,1] skillB_v;

matrix[N_plantH,1] plantH_v;
matrix[N_plantB,1] plantB_v;
matrix[N_animalH,1] animalH_v;
matrix[N_animalB,1] animalB_v;

///////////////////////////////////////////////////////////////////
///// Scaling and correlating random effects
idH_v = (diag_pre_multiply(sigma_idH, L_idH) * idH_z)';
idB_v = (diag_pre_multiply(sigma_idB, L_idB) * idB_z)';

// The other random effects are not correlated across responses
skillH_v = (diag_matrix(sigma_skillH) * skillH_z)';
skillB_v = (diag_matrix(sigma_skillB) * skillB_z)';
plantH_v = (diag_matrix(sigma_plantH) * plantH_z)';
plantB_v = (diag_matrix(sigma_plantB) * plantB_z)';
animalH_v = (diag_matrix(sigma_animalH) * animalH_z)';
animalB_v = (diag_matrix(sigma_animalB) * animalB_z)';

} // end transformed parameters block

model{
vector[N_obs] mu; // linear model

//////////////////////////////////////////////////////////
// Biological age with gaussian measurement error, scaled by 80 years old
age_estH ~ normal(age_muH, age_seH);
age_estB ~ normal(age_muB, age_seB);  

///// Priors //////////////////////////////////////////////
ap ~ normal(0,1); // baseline probability of skill

// effect of explicit vs tacit knowledge
p_expl ~ normal(0,1);

/// random effects ///////////
to_vector(idH_z) ~ normal(0,1);  // individual diff random effects, unscaled and uncorelated
to_vector(idB_z) ~ normal(0,1);
to_vector(skillH_z) ~ normal(0,1); // skill diff random effects, unscaled and uncorrelated
to_vector(skillB_z) ~ normal(0,1);
to_vector(plantB_z) ~ normal(0,1);
to_vector(plantH_z) ~ normal(0,1);
to_vector(animalB_z) ~ normal(0,1);
to_vector(animalH_z) ~ normal(0,1);

// scale parameters for random effects
sigma_idH ~ exponential(1); 
sigma_idB ~ exponential(1);
sigma_skillH ~ exponential(1);
sigma_skillB ~ exponential(1);
sigma_plantH ~ exponential(1);
sigma_plantB ~ exponential(1);
sigma_animalH ~ exponential(1);
sigma_animalB ~ exponential(1);

L_idH ~ lkj_corr_cholesky(4);
L_idB ~ lkj_corr_cholesky(4);

// free-list parameters
a_freelist ~ normal(0,1);
phi_free ~ exponential(1);

////////// Subsistence knowledge model /////////////////////////////
for (i in 1:N_obs) {
// Making linear model stems, culture and outcome specific /////
{
real l_stem = ap + p_expl*expl[i];

if (Hadza[i] == 1) {
  l_stem = l_stem + idH_v[id[i],1]*(1-expl[i]) + idH_v[id[i],2]*expl[i] + skillH_v[skill[i],1];
  
  if (plant_id[i] > 0) l_stem = l_stem + plantH_v[plant_id[i],1];
  if (animal_id[i] > 0) l_stem = l_stem + animalH_v[animal_id[i],1];
}

if (Hadza[i] == 0) {
  l_stem = l_stem + idB_v[id[i],1]*(1-expl[i]) + idB_v[id[i],2]*expl[i] + skillB_v[skill[i],1];
  
  if (plant_id[i] > 0) l_stem = l_stem + plantB_v[plant_id[i],1];
  if (animal_id[i] > 0) l_stem = l_stem + animalB_v[animal_id[i],1];
}

if (freelist[i] == 1) l_stem = l_stem + a_freelist;
}
  ///// Likelihood functions /////////////////////////////////////
  if (freelist[i] == 0) y[i] ~ bernoulli( 2*(inv_logit( exp(mu[i]) ) - 0.5) );
  if (freelist[i] == 1) y[i] ~ neg_binomial_2( exp(mu[i] ), phi_free[freelist_id[i]]);
}
}
 // end model block

generated quantities{
  matrix[2,2] Rho_idH;
  matrix[2,2] Rho_idB;
  
  // Recovering full correlation matrices
  Rho_idH = L_idH * L_idH';
  Rho_idB = L_idB * L_idB';
}

