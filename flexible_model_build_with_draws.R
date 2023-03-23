# US State-level COVID Resilience
# Model build with draws

rm(list=ls())
set.seed(3001)

## SET UP --------------------------------
# Load Libraries
library(data.table)
library(MASS)
source("FILEPATH/get_location_metadata.R")
source("FILEPATH/get_population.R")
source("FILEPATH/age_standardize_funcs.R")

# Specify arguments
data_version <- '2023_01_12.01' 
seir_version <- '2022_12_15.05'
dependent_var <- 'infections' #valid options: infections, deaths, employment, gdp, edu

#for edu: two additional flags required to specify the grade and subject
if(dependent_var=="edu"){
  subject <- "math" # valid options: "math" OR "reading"
  grade <- 8 # valid options: 4 OR 8 
}

# Define results version id
today <- gsub('-','_',Sys.Date())
files <- list.files(paste0("FILEPATH/outputs/",dependent_var), 
                    pattern = glob2rx(paste0(dependent_var, '_results_',today,'.**.csv')))
if(length(files)==0){
  version_id <- paste0(today,'.01')
} else{ 
  start <- nchar(files[1])-5; stop <- start + 1
  max_iter <- max(as.numeric(sapply(files, substr, start=start, stop=stop)))
  iter <- max_iter + 1
  version_id <- ifelse(iter<10, paste0(today,'.0',iter), paste0(today,'.',iter))
}

# Specify directories
input_root <- 'FILEPATH/data/'
covid_dir <- file.path('FILEPATH',seir_version,'/reference/output_draws/')
out_root <- 'FILEPATH/outputs'
out_dir <- file.path(out_root, dependent_var)


# Location metadata
hierarchy <- get_location_metadata(location_set_id = 35, release_id = 9)
usa_hierarchy <- hierarchy[parent_id==102]
usa_locs <- usa_hierarchy$location_id
name_dict <- hierarchy[parent_id==102, .(location_id, state = substr(local_id,4,5), location_name)]

# Define our cross-sections
cross_section_dt <- data.table(name = c('main', 'main_late_start', 'full', 'full_late_start',
                                        'mandate', 'vaccine', 'vaccine_omicron', 'gdp_vaccine_omicron',
                                        'gdp_full','gdp_full_late_start', 'NCES', 'NCES_late_start',
                                        'mandate_early_start', 'late'),
                               
                               start_date = as.Date(c('2020-01-01', '2020-04-01', '2020-01-01', '2020-04-01',
                                                      '2020-04-01', '2021-03-15', '2021-03-15', '2021-03-15',
                                                      '2020-01-01', '2020-04-01', '2020-01-01', '2020-04-01',
                                                      '2020-01-01', '2021-12-15')),
                               
                               end_date  =  as.Date(c('2021-12-15', '2021-12-15', '2022-07-31', '2022-07-31',
                                                      '2021-06-01', '2021-12-15', '2022-07-31', '2022-06-30',
                                                      '2022-06-30', '2022-06-30', '2022-02-15', '2022-02-15',
                                                      '2021-06-01', '2022-07-31')))


# Load model specs file
# This file contains information about the regressions to be fit. One regression per row.
# Column names: 
# dep_var - dependent variable
# factor - factor of interest
# category - broad category of the factor (pre-COVID, COVID policy, behavior, or COVID outcomes)
# dep_var_transf - mathematical transformation of the dependent variable
# time period - name of the time period
# controls - list of control variables, + delimited
model_specs_dt <- fread(paste0(input_root, dependent_var,'_model_specs_by_factor.csv'))

# Controls
controls <- model_specs_dt[1, controls]
if(!is.na(controls)){
  control_list <- gsub(" ", "", unlist(strsplit(controls, "[+]")))
}else{
  control_list <- ""
}

# All factors
factor_list <- c(model_specs_dt$factor)

# List of COVID policies
covid_policy_response <- model_specs_dt[category=='COVID policy', factor]

## DATA PREP -----------------------------

# Load data
dt <- fread(paste0(input_root,'data_',data_version,'.csv'))

# Fix governor party (DC)
dt[GovernorParty=="", GovernorParty:=NA]

# Rename trust vars
setnames(dt, c('interpersonal_trust_high_quality_surveys_mean', 'science_trust_cumulative_mean', 'govt_trust_all_mean'),
         c('interpersonal_trust', 'science_trust', 'government_trust'))

# Define paid leave variable
dt <- dt[, paid_leave := paid_family_leave + paid_sick_leave]

# Redefine school vax mandates
dt[, vax_mandate_schools := ifelse(vax_mandate_schools=='No', 0, 1)]


# LOAD COVID DRAWS
if(dependent_var=='infections'){
  draws_dt <- fread(file.path(covid_dir, "daily_infections.csv"))[location_id %in% usa_locs]
} else if(dependent_var=='deaths'){
  # read in age-standardized daily deaths
  draws_dt <- fread(paste0(out_dir, "/intermediate/daily_deaths_draws_age_std.csv"))[location_id %in% usa_locs]
} else{ #econ, edu models
  inf_draws <- fread(file.path(covid_dir, "daily_infections.csv"))[location_id %in% usa_locs]
  death_draws <- fread(file.path(out_root, "deaths/intermediate/daily_deaths.csv"))[location_id %in% usa_locs]
} 

# Age-standardize vaccines if running the deaths model
if(dependent_var=='deaths'){
  vax_dt <- age_standardize_vaccines(dt)
  dt$vax_fully <- NULL
  dt <- merge(dt, vax_dt[,.(location_id, date, vax_fully)], by=c('location_id','date'))
}

# Read in employment/GDP/edu data
if(dependent_var=="employment"){
  econ_flat_file <- fread('FILEPATH/data/employment_flat_file.csv')[location_id %in% usa_locs]
}else if(dependent_var=="gdp") {
  econ_flat_file <- fread('FILEPATH/data/gdp_flat_file.csv')[location_id %in% usa_locs]
}else if(dependent_var=="edu"){
  eduDT <- fread(paste0("FILEPATH/data/edu_data/NAEP/", subject,"_", grade, "_prepped.csv"))
  # Calculate absolute change in score 
  eduDT[, absolute_score_change:=score_2022 - score_2019]
  setnames(eduDT,"state","location_name")
  eduDT <- merge(eduDT, name_dict, by="location_name")
}

if((dependent_var=="employment") | (dependent_var=="gdp") ){
  fed_benefits <- fread("FILEPATH/data/economic_data/federal_benefits_adjusted.csv")[location_id %in% usa_locs]
  setnames(fed_benefits,c("PEUC_percent_covered","federal_adders_adj"),c("federal_UI_time_coverage","federal_UI_amount_rpp"))
  fed_benefits <- data.table(fed_benefits)
  fed_benefits[, location_name:=NULL]
  fed_benefits <- fed_benefits[, .(location_id, federal_UI_time_coverage,federal_UI_amount_rpp)]
}


## Model Fitting  -----------------------

# Define function for flexible regression fitting
fit_factor <- function(factor_name){
  print(factor_name)
  factor_specs <- model_specs_dt[factor==factor_name]
  
  # TIME PERIOD
  time_period <- factor_specs$time_period
  start_date <- cross_section_dt[name==time_period]$start_date
  analysis_date <- cross_section_dt[name==time_period]$end_date
  
  
  # DEPENDENT VARIABLE
  if(dependent_var=="employment"){ 
    dep_var <- "diff_emp" 
    dep_dt <- econ_flat_file[date >= start_date & date <= analysis_date,]
    if(factor_specs$dep_var_transf=='normal'){
      dep_dt <- unique(dep_dt[, eval(dependent_var) := log(mean(get(dep_var))), by='location_id'][, .SD, .SDcols = c('location_id', dependent_var)])
    } else{ 
      dep_dt <- unique(dep_dt[, eval(dependent_var) := mean(log(get(dep_var))), by='location_id'][, .SD,.SDcols = c('location_id', dependent_var)])
    }
  }else if(dependent_var=="gdp") { 
    dep_var <- "diff_gdp_adj"  
    dep_dt <- econ_flat_file[date >= start_date & date <= analysis_date,]
    if(factor_specs$dep_var_transf=='normal'){
      dep_dt <- unique(dep_dt[, eval(dependent_var) := log(mean(get(dep_var))), by='location_id'][, .SD, .SDcols = c('location_id', dependent_var)])
    } else{ 
      dep_dt <- unique(dep_dt[, eval(dependent_var) := mean(log(get(dep_var))), by='location_id'][, .SD,.SDcols = c('location_id', dependent_var)])
    }
  }else if(dependent_var=="edu") { 
    dep_var <- "absolute_score_change"
    dep_dt <- eduDT
    dep_dt <- dep_dt[, date := analysis_date]
    dep_dt <- unique(dep_dt[, eval(dependent_var) := get(dep_var), by='location_id'][, .SD, .SDcols = c('location_id', dependent_var)])
  }else{ #infections or deaths (draws)
    
    col_list <- paste0("draw_",0:99)
    dep_draws_dt <- draws_dt[date >= start_date & date <= analysis_date]
    dep_draws_dt <- merge(dep_draws_dt, unique(dt[,.(location_id, population)]), by='location_id')
    
    # Transform
    if(factor_specs$dep_var_transf=='normal'){
      dep_draws_dt[, (col_list) := lapply(.SD, function(x) log(sum(x)/population)), by='location_id', .SDcols = col_list]
      dep_draws_dt <- dep_draws_dt[date==start_date, .SD, .SDcols = c('location_id', col_list)]
    } else{ 
      dep_draws_dt[, (col_list) := lapply(.SD, function(x) mean(log(x/population))), by='location_id', .SDcols = col_list]
      dep_draws_dt <- dep_draws_dt[date==start_date, .SD, .SDcols = c('location_id', col_list)]
    }
    
  }
  
  # INDEPENDENT VARIABLES
  if(dependent_var=='deaths'){
    main_dt <- dt[date==analysis_date, .SD, .SDcols = c('location_id', 'location_name',
                                                        control_list[control_list != 'comorbid_pc1'])]
    
    comorbid_dt <- dt[date==analysis_date, .(location_id,
                                             bmi, smoking_prevalence,
                                             asthma_pc, cancer_pc, copd_pc, 
                                             cvd_pc, diabetes_pc)]
    
    risk_list <- c('asthma_pc', 'cancer_pc', 'copd_pc', 'cvd_pc', 'diabetes_pc', 'bmi', 'smoking_prevalence') 
    pc <- prcomp(comorbid_dt[, .SD, .SDcols=risk_list], center=F, scale=F) 
    main_dt$comorbid_pc1 <- (pc$x)[,1]*(-1)
  }else if((dependent_var=="employment") | (dependent_var=="gdp") | (dependent_var=="edu")){
    if(!is.na(controls)){
      if(tail(control_list,1)=="pca"){
        control_list <- control_list[-length(control_list)]
      }
      main_dt <- dt[date==analysis_date, .SD, .SDcols = c('location_id','location_name','ifr_age_stnd','population',
                                                          control_list)]
    }else{
      main_dt <- dt[date==analysis_date, .SD, .SDcols = c('location_id', 'location_name', 'ifr_age_stnd','population')]
    }
    
    # Cumulative hospitalizations during the time period
    if (factor_name == 'cumul_hosp_pc'){
      temp <- dt[date %in% c(start_date, analysis_date), .(location_id, date, cumulative_hospitalizations)]
      temp_wide <- dcast(temp, location_id ~ date, value.var = 'cumulative_hospitalizations')
      setnames(temp_wide, as.character(c(start_date, analysis_date)), c('start_date', 'end_date'))
      temp_wide[, cumulative_hospitalizations := end_date - start_date]
      main_dt <- merge(main_dt, temp_wide[,.(location_id, cumulative_hospitalizations)], by='location_id')
      main_dt[, cumul_hosp_pc := cumulative_hospitalizations/population]
    }
    
    # Cumulative deaths during the time period (DRAWS)
    if(factor_name == 'cumul_death_pc'){
      col_list <- paste0("draw_",0:99)
      death_pc_col_list <- paste0('death_pc_',col_list)
      temp_death <- death_draws[date >= start_date & date <= analysis_date]
      temp_death <- temp_death[, (death_pc_col_list) := lapply(.SD, sum), by=location_id, .SDcols = col_list][date==analysis_date]
      temp_death$date <- NULL
      main_dt <- merge(main_dt, temp_death[,.SD,.SDcols = c('location_id', death_pc_col_list)], by='location_id')
      main_dt[, (death_pc_col_list) := lapply(.SD, function(x) log(x/population)), .SDcols = death_pc_col_list]
    }
    
    # Cumulative infections during the time period (DRAWS)
    if(factor_name == 'infections'){
      col_list <- paste0("draw_",0:99)
      inf_pc_col_list <- paste0('inf_pc_',col_list)
      temp_inf <- inf_draws[date >= start_date & date <= analysis_date]
      temp_inf <- temp_inf[, (inf_pc_col_list) := lapply(.SD, sum), by=location_id, .SDcols = col_list][date==analysis_date]
      temp_inf$date <- NULL
      main_dt <- merge(main_dt, temp_inf[,.SD,.SDcols = c('location_id', inf_pc_col_list)], by='location_id')
      main_dt[, (inf_pc_col_list) := lapply(.SD, function(x) log(x/population)), .SDcols = inf_pc_col_list]
    }
    
  } else { #dependent_var=='infections'
    if(! is.na(controls)){
      main_dt <- dt[date==analysis_date, .SD, .SDcols = c('location_id', 'location_name', control_list)]
    } else{
      main_dt <- dt[date==analysis_date, .SD, .SDcols = c('location_id', 'location_name')]
    }
  }
  
  if(factor_specs$category=='pre-COVID'){ #covid outcomes too
    
    pre_covid_dt <- dt[date==analysis_date, .SD, .SDcols=c('location_id', factor_name)]
    main_dt <- merge(main_dt, pre_covid_dt, by='location_id')
    
  } else if(factor_name %like% 'federal_UI'){
    main_dt <- merge(main_dt, fed_benefits[, .SD, .SDcols = c('location_id', factor_name)], by='location_id')
  } else if(factor_specs$category=='COVID policy'){
    
    mandate_dt <- dt[date>=start_date & date<=analysis_date,
                     .(mask_mandate = mean(mask_mandate),
                       dining_close = mean(dining_close),
                       bar_close = mean(bar_close),
                       primary_edu = mean(primary_edu),
                       higher_edu = mean(higher_edu),
                       gatherings50i100o = mean(gatherings50i100o),
                       gym_pool_leisure_close = mean(gym_pool_leisure_close),
                       stay_at_home = mean(stay_at_home),
                       vax_mandate_schools = mean(vax_mandate_schools),
                       vax_mandate_state = mean(vax_mandate_state)
                     ), by='location_id']
    
    if(factor_name!='mandate_propensity'){
      main_dt <- merge(main_dt, mandate_dt[, .SD, .SDcols = c('location_id', factor_name)], by='location_id')
    }
    
    if(grepl('mandate', time_period) | (dependent_var=="edu")){
      policy_pca_list <- covid_policy_response[! covid_policy_response %in% c("vax_mandate_schools", "vax_mandate_state", "mandate_propensity",
                                                                              "federal_UI_time_coverage", "federal_UI_amount_rpp",
                                                                              factor_name)]
      pc <- prcomp(mandate_dt[, .SD, .SDcols=policy_pca_list], center=F, scale=F)
      
      if(factor_name!='mandate_propensity'){
        main_dt$pca <- (pc$x)[,1]*(-1)
      } else {
        main_dt$mandate_propensity <- (pc$x)[,1]*(-1)
      }
    }
    
    # For the vax mandates, include a mandate PCA for the mandate period
    if(grepl('vaccine', time_period)){
      mandate_start <- cross_section_dt[name=='mandate']$start_date
      mandate_end <- cross_section_dt[name=='mandate']$end_date
      
      mandate_dt <- dt[date>='2020-01-01' & date<=mandate_end,
                       .(mask_mandate = mean(mask_mandate),
                         dining_close = mean(dining_close),
                         bar_close = mean(bar_close),
                         primary_edu = mean(primary_edu),
                         higher_edu = mean(higher_edu),
                         gatherings50i100o = mean(gatherings50i100o),
                         gym_pool_leisure_close = mean(gym_pool_leisure_close),
                         stay_at_home = mean(stay_at_home)
                       ), by='location_id']
      
      pc <- prcomp(mandate_dt[, -1], center=F, scale=F)
      main_dt$pca <- (pc$x)[,1]*(-1)
    }
    
    
  } else if(factor_specs$category=='behavior') { 
    
    # Mobility and mask use (mean value during the time period)
    behavior_dt <- dt[date>=start_date & date<=analysis_date, .(location_id, date, mask_use, mobility)]
    behavior_dt[, mob_norm := (mobility - mean(mobility))/(sd(mobility))]
    behavior_dt <- behavior_dt[, .(mask_use = mean(mask_use, na.rm = T),
                                   mobility = mean(mob_norm)),
                               by='location_id']
    
    # Vaccine coverage
    # fully vaxed person days per total person days
    vax_dt <- dt[date>=start_date & date<=analysis_date, .(location_id, date, vax_fully, population)]
    vax_dt[, fully_vax_count := vax_fully * population] #convert from per capita to count space
    vax_dt[, fully_vax_count_daily := fully_vax_count - shift(fully_vax_count, fill = 0), by=location_id] #convert from cumulative to daily space
    vax_dt[, day := seq_len(.N), by=location_id]
    vax_dt[, num_days_vaxed := max(vax_dt$day) - day + 1]
    vax_dt[, vaxed_person_days := fully_vax_count_daily * num_days_vaxed]
    vax_dt <- vax_dt[, .(vax_fully = sum(vaxed_person_days)/(unique(population)*max(vax_dt$day))), by=location_id]
    
    # Combine
    main_dt <- merge(main_dt, behavior_dt, by='location_id')
    main_dt <- merge(main_dt, vax_dt, by='location_id')
    
  }
  
  # Normalize
  # Control variables 
  if(!is.na(controls)){
    main_dt[, (control_list) := lapply(.SD, function(x){(x - mean(x, na.rm=T))/sd(x, na.rm=T)}), .SDcols = control_list]
  }
  # Pre-COVID factors (Gov party and paid leave are factor vars so they aren't normalized)
  if(factor_specs$category=='pre-COVID' & ! factor_name %in% c('GovernorParty', 'paid_leave')){
    main_dt[, eval(factor_name) := (get(factor_name) - mean(get(factor_name),na.rm=T)) / sd(get(factor_name),na.rm=T)]
  }
  # COVID outcomes
  if(factor_specs$category=='COVID outcomes'){
    if(! factor_name %in% c('infections', 'cumul_death_pc')){
      main_dt[, eval(factor_name) := (get(factor_name) - mean(get(factor_name),na.rm=T)) / sd(get(factor_name),na.rm=T)]
    } else if(factor_name == 'infections'){
      main_dt[, (inf_pc_col_list) := lapply(.SD, function(x){(x - mean(x, na.rm=T))/sd(x, na.rm=T)}), .SDcols = inf_pc_col_list]
    } else if(factor_name == 'cumul_death_pc'){
      main_dt[, (death_pc_col_list) := lapply(.SD, function(x){(x - mean(x, na.rm=T))/sd(x, na.rm=T)}), .SDcols = death_pc_col_list]
    }
  }
  
  # Add the dependent variable to the data table
  if(dependent_var %in% c('infections','deaths')){
    main_dt <- merge(main_dt, dep_draws_dt, by='location_id')
  } else {
    main_dt <- merge(main_dt, dep_dt, by='location_id')
  }
  
  # FIT THE MODEL AND SAVE RESULTS
  if(dependent_var %in% c('infections','deaths')){
    # Fit 100 models with the 100 draws of the dependent variable
    if(is.na(controls)){
      print(paste('draw_n ~', factor_name))
      mod <- lapply(col_list, function(y) lm(as.formula(paste(y, '~', factor_name)), data=main_dt))
    } else{
      print(paste('draw_n ~', factor_specs$controls, '+', factor_name))
      mod <- lapply(col_list, function(y) lm(as.formula(paste(y, '~', factor_specs$controls, '+', factor_name)), data=main_dt))
    }
    
    # Pull 100 draws of beta per model
    beta_list <- list()
    beta_pca_list <- list()
    rsq_list <- list()
    for(i in 1:100){
      vcov_mat <- vcov(mod[[i]])
      beta_means <- coefficients(summary(mod[[i]]))[, 1]
      beta_draws <- mvrnorm(100, beta_means, vcov_mat)
      beta_temp <- beta_draws[, ncol(beta_draws)] #last col is factor of interest
      
      beta_list <- c(beta_list,beta_temp)
      rsq_list <- c(rsq_list, summary(mod[[i]])$r.squared)
      
      if(grepl('pca', factor_specs$controls)){
        beta_pca_temp <- beta_draws[, ncol(beta_draws)-1] #second-to-last col is pca
        beta_pca_list <- c(beta_pca_list, beta_pca_temp)
      }
    }
    
    # Summarize draws
    betas <- unlist(beta_list)
    
    coef_dt <- data.table(factor = factor_name,
                          beta = mean(betas),
                          lower = quantile(betas, probs = c(0.025)),
                          upper = quantile(betas, probs = c(0.975)),
                          rsq = mean(unlist(rsq_list)))
    
    # Exponentiate for easy plotting
    coef_dt[, `:=` (exp_beta = exp(beta),
                    exp_lower = exp(lower),
                    exp_upper = exp(upper))]
    
    # sig. at 5% level is +/-2, sig at 10% level is +/-1
    coef_dt[, sig := ifelse(lower<0 & upper<0, '-1',
                            ifelse(lower>0 & upper>0, '1', '0'))]
    
    if(grepl('pca', factor_specs$controls)){
      
      # Summarize draws of pca betas
      betas_pca <- unlist(beta_pca_list)
      
      coef_dt$beta_pca <- mean(betas_pca)
      coef_dt$lower_pca <- quantile(betas_pca, probs = c(0.025))
      coef_dt$upper_pca <- quantile(betas_pca, probs = c(0.975))
      coef_dt[, sig_pca := ifelse(lower_pca<0 & upper_pca<0, '-1',
                                  ifelse(lower_pca>0 & upper_pca>0, '1', '0'))]
      
      # Exponentiate for easy plotting
      coef_dt[, `:=` (exp_beta_pca = exp(beta_pca),
                      exp_lower_pca = exp(lower_pca),
                      exp_upper_pca = exp(upper_pca))]
      
    } else{
      coef_dt$beta_pca <- NA
      coef_dt$lower_pca <- NA
      coef_dt$upper_pca <- NA
      coef_dt$sig_pca <- NA
      coef_dt$exp_beta_pca <- NA
      coef_dt$exp_lower_pca <- NA
      coef_dt$exp_upper_pca <- NA
    }
    
  } else if(dependent_var %in% c('gdp', 'employment', 'edu') & factor_name %in% c('cumul_death_pc', 'infections')){
    # Fit 100 models with the 100 draws of deaths or infections
    if(!is.na(controls)){
      if(factor_name == 'cumul_death_pc'){
        mod <- lapply(death_pc_col_list, function(x) lm(as.formula(paste(dependent_var, '~', factor_specs$controls, '+', x)), data=main_dt))
      }
      if(factor_name == 'infections'){
        mod <- lapply(inf_pc_col_list, function(x) lm(as.formula(paste(dependent_var, '~', factor_specs$controls, '+', x)), data=main_dt))
      }
    }else{
      if(factor_name == 'cumul_death_pc'){
        mod <- lapply(death_pc_col_list, function(x) lm(as.formula(paste(dependent_var, '~', x)), data=main_dt))
      }
      if(factor_name == 'infections'){
        mod <- lapply(inf_pc_col_list, function(x) lm(as.formula(paste(dependent_var, '~', x)), data=main_dt))
      }
    }
    
    # Pull 100 draws of beta per model
    beta_list <- list()
    rsq_list <- list()
    for(i in 1:100){
      vcov_mat <- vcov(mod[[i]])
      beta_means <- coefficients(summary(mod[[i]]))[, 1]
      beta_draws <- mvrnorm(100, beta_means, vcov_mat) 
      beta_temp <- beta_draws[, ncol(beta_draws)] #last col is factor of interest
      
      beta_list <- c(beta_list,beta_temp)
      rsq_list <- c(rsq_list, summary(mod[[i]])$r.squared)
    }
    
    # Summarize draws
    betas <- unlist(beta_list)
    
    coef_dt <- data.table(factor = factor_name,
                          beta = mean(betas),
                          rsq = mean(unlist(rsq_list)),
                          lower = quantile(betas, probs = c(0.025)),
                          upper = quantile(betas, probs = c(0.975)))
    
    # Exponentiate for easy plotting
    coef_dt[, `:=` (exp_beta = exp(beta),
                    exp_lower = exp(lower),
                    exp_upper = exp(upper))]
    
    # sig. at 5% level is +/-2, sig at 10% level is +/-1
    coef_dt[, sig := ifelse(lower<0 & upper<0, '-1',
                            ifelse(lower>0 & upper>0, '1', '0'))]
    
    # No PCA for behaviors
    coef_dt$beta_pca <- NA
    coef_dt$lower_pca <- NA
    coef_dt$upper_pca <- NA
    coef_dt$sig_pca <- NA
    coef_dt$exp_beta_pca <- NA
    coef_dt$exp_lower_pca <- NA
    coef_dt$exp_upper_pca <- NA
    
    
  } else {# ECON & EPI models that dont use draws
    
    if(!is.na(controls)){
      formula <- as.formula(paste(dependent_var, '~', factor_specs$controls, '+', factor_name))
    }else{
      formula <- as.formula(paste(dependent_var, '~', factor_name))
    }
    
    # Fit the model
    mod <- lm(formula, data=main_dt)
    
    # Save results
    covar_index <- nrow(coefficients(summary(mod)))
    coef_dt <- data.table(factor = factor_name,
                          beta = coefficients(summary(mod))[covar_index, 1],
                          se = coefficients(summary(mod))[covar_index, 2],
                          rsq = round(summary(mod)$r.squared, 3))  
    coef_dt[, `:=` (lower = beta - 1.96*se,
                    upper = beta + 1.96*se)]
    
    coef_dt[, `:=` (exp_beta = exp(beta),
                    exp_lower = exp(lower),
                    exp_upper = exp(upper))]
    
    coef_dt[, sig := ifelse(lower<0 & upper<0, '-1',
                            ifelse(lower>0 & upper>0, '1', '0'))]
    
    coef_dt$se <- NULL
    
    if(grepl('pca', factor_specs$controls)){
      covar_index <- covar_index - 1
      coef_dt$beta_pca <- coefficients(summary(mod))[covar_index, 1]
      coef_dt$se_pca <- coefficients(summary(mod))[covar_index, 2]
      
      coef_dt[, `:=` (lower_pca = beta_pca - 1.96*se_pca,
                      upper_pca = beta_pca + 1.96*se_pca)]
      
      coef_dt[, sig_pca := ifelse((beta_pca - 1.96*se_pca)<0 & (beta_pca + 1.96*se_pca)<0, '-1',
                                  ifelse((beta_pca - 1.96*se_pca)>0 & (beta_pca + 1.96*se_pca)>0, '1', '0'))]
      
      coef_dt[, `:=` (exp_beta_pca = exp(beta_pca),
                      exp_lower_pca = exp(lower),
                      exp_upper_pca = exp(upper_pca))]
      
      coef_dt$se_pca <- NULL
      
    } else{
      coef_dt$beta_pca <- NA
      coef_dt$lower_pca <- NA
      coef_dt$upper_pca <- NA
      coef_dt$sig_pca <- NA
      coef_dt$exp_beta_pca <- NA
      coef_dt$exp_lower_pca <- NA
      coef_dt$exp_upper_pca <- NA
    }
    
  } #end econ, edu models
  return(coef_dt)
}

# Fit all the regressions and combine results into a single data table
coefs_dt <- rbindlist(lapply(factor_list, fit_factor))

# Combine results with information from model specs file
results_dt <- merge(model_specs_dt, coefs_dt, by='factor', all.x=T)
results_dt <- results_dt[order(-category)]

# Save
if(dependent_var=="edu"){
  fwrite(results_dt, paste0(out_dir, '/', dependent_var,'_',subject,'_',grade,'_results_',version_id,'.csv'))
}else{
  fwrite(results_dt, paste0(out_dir, '/', dependent_var,'_results_',version_id,'.csv'))
}


