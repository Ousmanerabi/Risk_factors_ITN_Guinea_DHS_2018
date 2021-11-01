##  Take account survey design
data_use = read.csv('C:/Users/ode3599/Box/NU-malaria-team/data/guinea_dhs/data_analysis/master/data/data_PR/data_use_among_those_with_access_cleaning.csv')

data_use$wt = data_use$hv005/1000000

design_sample = svydesign(ids = ~hv021, data = data_use, strata = ~hv022, weights = ~wt, num_p=1, nest = T)

## 1. Bivariate analysis and Rao-scott chisquare test for ITN use among those with access

explanatory_vars_ind <- c("wealth", "sex", "urb", "u5_hh", "rooms", "hh_size", "Edu", "head_age", "Marital", "age", "preg", "region")

for(i in 1:length(explanatory_vars_ind)){
  col = explanatory_vars[i]
  tbl <- svytable(~net_use + col, design_sample)
  t = summary(tbl, statistic="Chisq")
  t = plyr::ldply(t)
}


## 2.1 Univariate analysis of different risk factors for ITN use at the country level 

models_ind <- explanatory_vars_ind %>%       # begin with variables of interest
  str_c("net_use ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~svyglm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      family = "binomial",           # specify type of glm (logistic)
      design = design_sample)) %>%          # dataset
  
  # tidy up each of the glm regression outputs from above
  map(
    .f = ~tidy(
      .x, 
      exponentiate = TRUE,           # exponentiate 
      conf.int = TRUE)) %>%          # return confidence intervals
  
  # collapse the list of regression outputs in to one data frame
  bind_rows() %>% 
  
  # round all numeric columns
  mutate(across(where(is.numeric), round, digits = 2))

models_ind

## 2.2 Univariate analysis of different risk factors for ITN use at the regional level

explanatory_vars_ind <- c("wealth", "sex", "urb", "u5_hh", "rooms", "hh_size", "Edu", "head_age", "Marital", "age", "preg") ## remove region in the explanatory variables
region_vars = c("Boke", "Kankan", "Kindia", "Mamou", "Labe", "Faranah", "Conakry", "Nzerekore")
models_region_ind = function(survey.design, region_vars){
  region = region_vars[i]
  design_sample_v = subset(survey.design, region %in% region_vars)
  explanatory_vars_ind %>%       # begin with variables of interest
    str_c("net_use ~ ", .) %>%  
    
    map(                               
      .f = ~svyglm(                       # pass the formulas one-by-one to glm()
        formula = as.formula(.x),      # within glm(), the string formula is .x
        family = "binomial",           # specify type of glm (logistic)
        design = design_sample_v))
  # tidy up each of the glm regression outputs from above
  map(
    .f = ~tidy(
      .x, 
      exponentiate = TRUE,           # exponentiate 
      conf.int = TRUE)) %>%          # return confidence intervals
    
    # collapse the list of regression outputs in to one data frame
    bind_rows() %>% 
    
    # round all numeric columns
    mutate(across(where(is.numeric), round, digits = 2))
  
}

##3 Risk factors estimations using multivariate analysis, Akaike Information Criterion use to select the best models (stepAIC)
# 3.1 Whole country
###
reg_ind = svyglm(net_use~ urb + rooms + hh_size + u5_hh + Marital + sex + Edu + head_age + wealth + age + preg + region,
                 design = design_sample, family = binomial())

stepAIC(reg_ind)

# Final model
reg_ind_fina = svyglm(formula = net_use ~ hh_size + rooms + Edu + age + head_age + Marital + preg + wealth
                      + urb + region, design = design_sample,family = binomial())

models_ind = odds.ratio(reg_ind_fina)

stargazer(reg_ind_fina, type = "text")

# 3.2 For each region
##3.2.1 Labé region
design_sample_labe = subset(design_sample, region=="Labe")
reg_in_labe = svyglm(net_use ~ urb + rooms + hh_size + u5_hh + Marital + sex + Edu + head_age + wealth + age, 
                     design = design_sample_labe, family = binomial())



stepAIC(reg_in_labe)

reg_in_labe = svyglm(net_use ~ urb + rooms + u5_hh + Marital + Edu + 
                       head_age, design = design_sample_labe, family = binomial())



odds.ratio(reg_in_labe)
tab_model(reg_in_labe)


## 3.2.2 Mamou region
design_sample_mamou = subset(design_sample, region=="Mamou")
reg_in_mam = svyglm(net_use ~ urb + rooms + hh_size + u5_hh + Marital + sex + Edu + head_age + wealth + age, 
                    design = design_sample_mamou, family = binomial())



stepAIC(reg_in_mam)

reg_in_mam = svyglm(net_use ~ urb + rooms + hh_size + Marital + 
                      head_age + wealth, design = design_sample_mamou, family = binomial())



odds.ratio(reg_in_mam)
tab_model(reg_in_labe)

## 3.2.3 Boké region
design_sample_bok = subset(design_sample, region=="Boke")
reg_in_bok = svyglm(net_use ~ urb + rooms + hh_size + u5_hh + Marital + sex + Edu + head_age + wealth + age, 
                    design = design_sample_bok, family = binomial())



stepAIC(reg_in_bok)

reg_in_bok = svyglm(net_use ~ urb + rooms + hh_size + Marital + 
                      sex + wealth + age, design = design_sample_bok, family = binomial())



odds.ratio(reg_in_bok)
tab_model(reg_in_bok)

## 3.2.4 Kindia region
design_sample_kin = subset(design_sample, region=="Kindia")
reg_in_kin = svyglm(net_use ~ urb + rooms + hh_size + u5_hh + Marital + sex + Edu + head_age + wealth + age, 
                    design = design_sample_kin, family = binomial())



stepAIC(reg_in_kin)

reg_in_kin = svyglm(net_use ~ urb + hh_size + Marital + head_age + 
                      wealth, design = design_sample_kin, family = binomial())

tab_model(reg_in_kin)

## 3.2.5 Faranah region
design_sample_fara = subset(design_sample, region=="Faranah")
reg_in_fara = svyglm(net_use ~ urb + rooms + hh_size + u5_hh + Marital + sex + Edu + head_age + wealth + age, 
                     design = design_sample_fara, family = binomial())



stepAIC(reg_in_fara)

reg_in_fara = svyglm(net_use ~ urb + rooms + Marital + sex + wealth + age, 
                     design = design_sample_fara, family = binomial())




odds.ratio(reg_in_fara)

## 3.2.6 Kankan region
design_sample_kan = subset(design_sample, region=="Kankan")
reg_in_kan = svyglm(net_use ~ urb + rooms + hh_size + u5_hh + Marital + sex + Edu + head_age + wealth + age, 
                    design = design_sample_kan, family = binomial())



stepAIC(reg_in_kan)

reg_in_kan = svyglm(net_use ~ urb + rooms + hh_size + u5_hh + Marital + 
                      head_age + age, design = design_sample_kan, family = binomial())


odds.ratio(reg_in_kan)
tab_model(reg_in_kan)


## 3.2.7 N'zérékoré region
design_sample_zere = subset(design_sample, region=="Nzerekore")
reg_in_zere = svyglm(net_use ~ urb + rooms + hh_size + u5_hh + Marital + sex + Edu + head_age + wealth + age, 
                     design = design_sample_zere, family = binomial())



stepAIC(reg_in_zere)

reg_in_zere = svyglm(net_use ~ u5_hh + Marital + Edu + head_age + 
                       wealth + age, design = design_sample_zere, family = binomial())





odds.ratio(reg_in_zere)

## 3.2.8 Conakry region (remove place of residence in Conakry)
design_sample_cona = subset(design_sample, region=="Conakry")
reg_in_cona = svyglm(net_use ~rooms + hh_size + Marital + sex + Edu + head_age + age, 
                     design = design_sample_cona, family = binomial())



stepAIC(reg_in_cona)

reg_in_cona = svyglm(net_use ~ rooms + hh_size + Marital + Edu + 
                       head_age, design = design_sample_cona, family = binomial())





odds.ratio(reg_in_cona)

tab_model(reg_in_cona)
