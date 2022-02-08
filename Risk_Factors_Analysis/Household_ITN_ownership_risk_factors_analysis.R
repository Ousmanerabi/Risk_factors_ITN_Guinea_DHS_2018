#1. Read the ITN ownserhip datasets
hh_ex = read.csv('C:/Users/ode3599/Box/NU-malaria-team/data/guinea_dhs/data_analysis/master/data/data_PR/data_HR.csv')
#2. Take account survey design for the analysis

hh_ex$wt = hh_ex$hv005/1000000
design_sample_hh= svydesign(ids = ~hv021, data = hh_ex, strata = ~hv022, weights = ~wt, num_p=1, nest = T)


## 3. Bivariate analysis and Rao-scott chisquare test for Household ITN ownership

explanatory_vars <- c("wealth", "sex", "urb", "Num_childre", "region", "rooms", "hh_size", "Edu", "head_age", "Marital")

for(i in 1:length(explanatory_vars)){
  col = explanatory_vars[i]
  tbl <- svytable(~HH_at_least_one + col, design_sample_hh)
  t = summary(tbl, statistic="Chisq")
  t = plyr::ldply(t)
}


## 4.1 Univariate analysis of different risk factors for ITN ownership at the country level 

models <- explanatory_vars %>%       # begin with variables of interest
  str_c("HH_at_least_one ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~svyglm(                      
      formula = as.formula(.x),      
      family = "binomial",           
      design = design_sample_hh)) %>%       
  

  map(
    .f = ~tidy(
      .x, 
      exponentiate = TRUE,           
      conf.int = TRUE)) %>%         
  
 
  bind_rows() %>% 
  
  mutate(across(where(is.numeric), round, digits = 2))

models

## 4.2 Univariate analysis of different risk factors for ITN ownership at the regional level 

region_vars = c("Boké", "Kankan", "Kindia", "Mamou", "Labé", "Faranah", "Conakry", "Nzerekore")
models_region = function(survey.design, region_vars){
  region = region_vars[i]
  design_sample_v = subset(survey.design, region %in% region_vars)
  explanatory_vars %>%       # begin with variables of interest
    str_c("HH_at_least_one ~ ", .) %>%  
    
    map(                               
      .f = ~svyglm(                       
        formula = as.formula(.x),      
        family = "binomial",           
        design = design_sample_v))
  
  map(
    .f = ~tidy(
      .x, 
      exponentiate = TRUE,            
      conf.int = TRUE)) %>%          
    
    
    bind_rows() %>% 
    
 
    mutate(across(where(is.numeric), round, digits = 2))
  
}

#5. Risk factors estimations using multivariate analysis, Akaike Information Criterion use to select the best models (stepAIC)
# 5.1 Whole country
reg_hh = svyglm(HH_at_least_one ~ urb + rooms + hh_size + Num_childre + Marital + sex + Edu + head_age + wealth + region,
                design = design_sample_hh, family = binomial())

StepAIC(reg_hh)

reg_hh_final = svyglm(HH_at_least_one ~ urb + hh_size +  Marital +  Edu + head_age + wealth + region,
                      design = design_sample_hh, family = binomial())
odds.ratio(reg_hh_final)
# 5.2 For each region
# 5.2.1 Boké region
hh_design_boke = subset(design_sample_hh, region=="Boke")

reg_boke = svyglm(HH_at_least_one~ urb + rooms + hh_size + Num_childre + Marital + sex + Edu + head_age + wealth,
                  design = hh_design_boke, family = binomial())

stepAIC(reg_boke)

reg_boke_fina = svyglm(HH_at_least_one ~ urb + rooms + hh_size + Num_childre + Marital + 
                         sex + Edu + wealth, design = hh_design_boke, family = binomial())


odds.ratio(reg_boke_fina)


# 5.2.2 Kindia region
hh_design_kin = subset(design_sample_hh, region=="Kindia")

reg_kin = svyglm(HH_at_least_one~ urb + rooms + hh_size + Num_childre + Marital + sex + Edu + head_age + wealth,
                 design = hh_design_kin, family = binomial())

stepAIC(reg_kin)

reg_kind_fina = svyglm(HH_at_least_one ~ urb + Num_childre + Marital + sex + wealth, design = hh_design_ki, family = binomial())



odds.ratio(reg_kind_fina)


# 5.2.3 Faranah region
hh_design_fara = subset(design_sample_hh, region=="Faranah")

reg_fara = svyglm(HH_at_least_one~ urb + rooms + hh_size + Num_childre + Marital + sex + Edu + head_age + wealth,
                  design = hh_design_fara, family = binomial())

stepAIC(reg_fara)

reg_fara_fina = svyglm(HH_at_least_one ~ urb + rooms + hh_size + Marital + 
                         Edu + wealth, design = hh_design_fara, family = binomial())



odds.ratio(reg_fara_fina)

# 5.2.4 Kankan region

hh_design_kan = subset(design_sample_hh, region=="Kankan")

reg_kan = svyglm(HH_at_least_one~ urb + rooms + hh_size + Num_childre + sex + Edu + head_age + wealth,
                 design = hh_design_kan, family = binomial())

stepAIC(reg_kan)

reg_kan_fina = svyglm(HH_at_least_one ~ hh_size + Num_childre, design = hh_design_kan, family = binomial())



odds.ratio(reg_kan_fina)

# 5.2.5 Labé region
hh_design_labe = subset(design_sample_hh, region=="Labe")

reg_labe = svyglm(HH_at_least_one~ urb + rooms + hh_size + Num_childre + sex + Edu + head_age + wealth,
                  design = hh_design_labe, family = binomial())

stepAIC(reg_labe)

reg_labe_fina = svyglm(HH_at_least_one ~ head_age + wealth, design = hh_design_kan, family = binomial())



odds.ratio(reg_labe_fina)


# 5.2.6 Mamou region
hh_design_mamou = subset(design_sample_hh, region=="Mamou")

reg_mam = svyglm(HH_at_least_one~ urb + rooms + hh_size + Num_childre + sex + Edu + head_age + wealth,
                 design = hh_design_mamou, family = binomial())

stepAIC(reg_mam)

reg_mam_fina = svyglm(HH_at_least_one ~ urb + hh_size + sex + wealth, design = hh_design_mamou, family = binomial())



odds.ratio(reg_mam_fina)



# 5.2.7 N'zérékoré region
hh_design_zere = subset(design_sample_hh, region=="Nzerekore")

reg_zere = svyglm(HH_at_least_one~ urb + rooms + hh_size + Num_childre + sex + Edu + head_age + wealth,
                  design = hh_design_zere, family = binomial())

stepAIC(reg_zere)

reg_zere_fina = svyglm(HH_at_least_one ~ Edu + hh_size + urb + wealth, design = hh_design_zere, family = binomial())



odds.ratio(reg_zere_fina)


# 5.2.8 Conakry region
hh_design_cona = subset(design_sample_hh, region=="Conakry")

reg_cona = svyglm(HH_at_least_one~rooms + hh_size + Num_childre + sex + Edu + head_age + wealth,
                  design = hh_design_cona, family = binomial())

stepAIC(reg_cona)

reg_cona_fina = svyglm(HH_at_least_one ~ Num_childre + head_age + hh_size, 
                       design = hh_design_cona, family = binomial())



odds.ratio(reg_cona_fina)
