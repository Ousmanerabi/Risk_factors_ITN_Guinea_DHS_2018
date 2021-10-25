###
library(lme4)
library(ggplot2)
library(sp)
library(rgdal) 
library(mapproj) 
library(maptools) 
library(reshape2)
library(RColorBrewer)
library(classInt)
library(scales)
library(questionr)
library(foreign)
library(dummies)
library(plyr)
library(survey)
library(car)
library(RcmdrMisc)
library(corrplot)
library(readxl)
library(wesanderson)
library(gridExtra)
library(gcookbook)
### Directories
user <- Sys.getenv("USERNAME")
if ("mambrose" %in% user) {
  user_path <- file.path("C:/Users", user)
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  NuDir <- file.path(Drive, "NU-malaria-team")
  NGDir <-file.path(NuDir, "data", "nigeria_dhs",  "data_analysis")
  DataDir <-file.path(NGDir, "data")
  ResultDir <-file.path(NGDir, "results")
  SrcDir <- file.path(NGDir, "src", "DHS")
  BinDir <- file.path(NGDir, "bin")
} else {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  NuDir <- file.path(Drive, "Box", "NU-malaria-team")
  CliDir <- file.path(NuDir, "data", "guinea_climate")
  GNDir <-file.path(NuDir, "data", "guinea_dhs",  "data_analysis", "master")
  DataDir <-file.path(GNDir, "data")
  ResultDir <-file.path(GNDir, "results")
  BinDir <- file.path(GNDir, "bin")
  SrcDir <- file.path(GNDir, "src", "DHS")
  VarDir <- file.path(SrcDir, "1_variables_scripts")
  ProjectDir <- file.path(NuDir, "projects", "hbhi_guinea")
  SimDir <- file.path(ProjectDir, 'simulation_input')
}


## -----------------------------------------
### Required functions and settings
## -----------------------------------------
source(file.path(VarDir, "generic_functions", "DHS_fun.R"))
source(file.path(SrcDir, "Fonctions_nettoyage.R"))


###### Import dhs, climate, entological and routine data
dhs <-read.files( ".*GNPR.*\\.DTA", DataDir, read_dta)
dhs_hhs = read.files(".*GNHR.*\\.DTA", DataDir, read_dta)

dhs_wom = read.files(".*GNIR.*\\.DTA", DataDir, read_dta)

dhs_child = read.files(".*GNKR.*\\.DTA", DataDir, read_dta)

HDshp <- readOGR(file.path(DataDir,"guinea_shapefiles/Guinea_Health_District"), layer ="GIN_adm2", use_iconv=TRUE, encoding= "UTF-8")

HD_sf <- st_as_sf(HDshp)

Gn_names <- read.csv(file.path(BinDir,"names/GN_shp_pop_names.csv")) %>% dplyr::select(-X)


GN_clean_names <- left_join(HD_sf, Gn_names, by=c("NAME_2" = "DS_shape")) 
head(GN_clean_names)


# rep DS file
rep_DS <- read.csv(file.path(BinDir, "rep_DS/representative_DS_orig60clusters.csv")) %>% dplyr::select(-X)

library(nngeo)
# cluster locations 
GNshplist<-read.files("*FL.*\\.shp$", DataDir, shapefile)
GNshplist <- lapply(GNshplist, st_as_sf)
key_list <- lapply(GNshplist, function(x) x %>% st_join(GN_clean_names, join = st_nn, maxdist = 10000))

dhs = list(dhs[[3]], dhs[[4]])
dhs <- lapply(dhs, subset, hv103 == 1)

GNshplist<- list(GNshplist[[3]],GNshplist[[4]])
GN_ID <- lapply(GNshplist, "[[", "DHSCLUST")#QA this
key_list <- list(key_list[[3]], key_list[[4]])
key_list <- Map(cbind, key_list, hv001 = GN_ID)
dhs <- map2(dhs, key_list, left_join)



### Import climate and Adjusted Incidence
climate_data = read.csv("C:/Users/ode3599/Box/NU-malaria-team/data/guinea_climate/Guinea_long_format.csv")

#routine_data = read.csv2("/Users/ousmanediallo/Box/NU-malaria-team/data/guinea_cases/cases_from_DHSI2/disdata_mon.csv")

#pfpr_direc = setwd("C:/Users/ode3599/Box/NU-malaria-team/data/guinea_who/GUI_MAP_estimates/MAP_estimates/")

#pfpr_data = read_xlsx("district_summaries_GIN.xlsx", 3)

#pfpr_data_2018 = pfpr_data %>%dplyr::select(Region, District, map4_pfpr_2018, map4_pfpr_LCI_2018, map4_pfpr_UCI_2018)

Incidence_strat = read_xlsx("C:/Users/ode3599/Box/NU-malaria-team/data/guinea_cases/Endemicity_WHO_1.xlsx")

### Cleaning climate data 

## Recodage de climate_data$district
climate_data$district <- fct_recode(climate_data$district,
                                    "Boke" = "Bok\xe9",
                                    "Dubreka" = "Dubr\xe9ka",
                                    "Forecariah" = "For\xe9cariah",
                                    "Gueckedou" = "Gu\xe9ck\xe9dou",
                                    "Kerouane" = "K\xe9rouan\xe9",
                                    "Lelouma" = "L\xe9louma",
                                    "Labe" = "Lab\xe9",
                                    "Nzerekore" = "Nz\xe9r\xe9kor\xe9",
                                    "Telimele" = "T\xe9lim\xe9l\xe9",
                                    "Tougue" = "Tougu\xe9",
                                    "Yomou" = "Yamou"
)

## Recodage de climate_data$region
climate_data$region <- fct_recode(climate_data$region,
                                  "Boke" = "Bok\xe9",
                                  "Labe" = "Lab\xe9",
                                  "Nzerekore" = "Nz\xe9r\xe9kor\xe9"
)

## select climate for 2018
climate_data_2018 = climate_data %>%dplyr::filter(year == "2018")
### summarise era and precipitation by year and districts
data_climate_2018_groupe <- climate_data_2018 %>%
  group_by (district) %>%
  dplyr::summarize(avg_precip_era5 = mean(precip_era5, na.rm =TRUE), avg_air_temp_era5 = mean(air_temp_era5))%>%
  dplyr::select(district, avg_precip_era5, avg_air_temp_era5)

data_climate_2018_groupe = dplyr::rename(data_climate_2018_groupe, NAME_2 = district, precipitation = avg_precip_era5, tempera = avg_air_temp_era5)

## cleannig pfpr data (we have 5 districts in conakry, We just need to have whole conakry)
pfpr_conakry = pfpr_data_2018%>%dplyr::filter(Region == "Conakry")

pfpr_conakry = pfpr_conakry %>%
  group_by(Region) %>%
  dplyr::summarize(map4_pfpr_2018 = mean(map4_pfpr_2018, na.rm = T), map4_pfpr_LCI_2018 = mean(map4_pfpr_LCI_2018, na.rm = T),
                   map4_pfpr_UCI_2018 = mean(map4_pfpr_UCI_2018, na.rm = T)) %>%
  dplyr::select(Region, map4_pfpr_2018, map4_pfpr_LCI_2018, map4_pfpr_UCI_2018)

pfpr_conakry$District = rep("Conakry", 1)

#pfpr_data_2018_with_conakry = sqldf("select * from pfpr_data_2018 where Region %in% (Boke, Faranah, Kindia, Kankan, Mamou, Nzerekore)")

pfpr_data_2018_with_conakry = pfpr_data_2018 %>% dplyr::filter(Region %in% c("Boke", "Faranah", "Kankan", "Kindia", "Labe", "Mamou", "Nzerekore"))

pfpr_data_2018_with_conakry = rbind(pfpr_data_2018_with_conakry, pfpr_conakry)

pfpr_data_2018_with_conakry = dplyr::rename(pfpr_data_2018_with_conakry, NAME_2 = District)


#subsetting for ITN (persons slept there last night) 2018
dhs_2018 <- dhs[2]

dhs_2018 = as.data.frame(dhs_2018)

dhs_hh_2018 = dhs_hhs[[4]]
## Recodage de dhs_2018$NAME_2
dhs_2018$NAME_2 <- fct_recode(dhs_2018$NAME_2,
                              "Boke" = "Boké",
                              "Dubreka" = "Dubréka",
                              "Forecariah" = "Forécariah",
                              "Gueckedou" = "Guéckédou",
                              "Kerouane" = "Kérouané",
                              "Labe" = "Labé",
                              "Lelouma" = "Lélouma",
                              "Nzerekore" = "Nzérékoré",
                              "Telimele" = "Télimélé",
                              "Tougue" = "Tougué",
                              "Yomou" = "Yamou"
)

## Routine data 2018
## Recodage de pfpr_data_2018_with_conakry$NAME_2
pfpr_data_2018_with_conakry$NAME_2 <- fct_recode(pfpr_data_2018_with_conakry$NAME_2,
                                                 "Telimele" = "Telemele"
)


routine_data_2018 = routine_data %>%dplyr::filter(year == "2018")

routine_data_2018_without_conakry = routine_data_2018 %>% dplyr::filter(adm1 %in% c("Boke", "Faranah", "Kankan", "Kindia", "Labe", "Mamou", "Nzerekore"))

routine_data_conakry = routine_data_2018 %>%dplyr::filter(adm1 == "Conakry")

routine_data_conakry = routine_data_conakry %>%
  group_by(adm1) %>%
  dplyr::summarize(conf = sum(conf, na.rm = T), pop = sum(pop, na.rm = T),
                   susp = sum(susp, na.rm = T)) %>%
  dplyr::select(adm1, conf, susp, pop)



#pfpr_conakry$District = rep("Conakry", 1)
## Fusion all tables (Routine data, dhs, climate and Pfpr)
#routine_data = rename(routine_data, NAME_2 = adm2)
#all_data = merge(dhs_2018, Incidence_strat, by = "NAME_2")
#all_data = merge(all_data, pfpr_data_2018_with_conakry, by = "NAME_2")
all_data = merge(dhs_2018, data_climate_2018_groupe, by = "NAME_2")


#number of persons per household 
clusters_2018 <- all_data_fin %>% dplyr::group_by(hv001,hv002) %>% dplyr::summarise(num_persons = n()) 

hist_18= ggplot(clusters_2018, aes(x=num_persons))+
  geom_histogram(color="green", fill ='lightgreen')+
  theme_minimal()+
  labs(x = "Number of persons per Household", y = "Frequence")


hist_18
### Selectionner toutes les variables
## hv001 : Cluster number is the number identifying the sample point as used during the fieldwork
## hv002 : Household number is the number identifying the hh within the cluster or sample point
## hv009 : Total number of household members indicates the number of entries to be found
## hv024 : region
## hv025 : Place of residence (urban = 1 (0)& rural = 2 (1))
## hv015 : Number of children 5 and under
## hv104 : sex of the household member (1 = Male (0) and 2 = female (1))
## hv105 : Age of the household member
## hv106 : Highest level of education the household member attended
## hv115 : Marital status of the household member
## hv220 : Age of the head of household
## hv227 : Household has at least one mosquito net
## hml1  : Number of mosquito nets household owns
## hml12 : type of bednets person slept under last night
## hml16 : Corrected age from individual 
## hc27  : sex of child
## hml18  : pregnant status
## ha66  : woman's highest educational level
## Temperature : from CHIRPS data
## Precipitation : from CHIRPS data
## PfPR : from routine data
## Incidence : from routine data


#all_data_2018_analysis = all_data_fin %>% dplyr::select(hhid_PR, hv001, hv002, hv005_PR, hv021_PR, hv022_PR, hv024_PR, hv025_PR, hv014_PR, hv013_PR, hml12, hv220_PR, hv270_PR, hml16, hv104, hv105, hv115, hc27, ha54,
#                                                    ha66,hv106, hml18, hv227_PR, hv009_PR, hml1_PR, hv103, PfPR, Incidence, precipitation, tempera)


#all_data_2018_analysis = data_PR_HR %>% dplyr::select(hhid, hv001, hv002, hv005, hv021, hv022, hv024, hv025, hv014,hv220, hv270, hml16, hv104, hv105, hv115, hc27, ha54,
#                                                      ha66,hv106, hml18, hv227, hv009, hv013, hv216, hml12, hml1, nommiimenage, potuse, hh_has_itn,access, access2,
#                                                      hv103, Adjusted_inc, precipitation, tempera, region)

all_data_2018_analysis = data_PR_HR %>% dplyr::select(hhid, hv001, hv002, hvidx, hv003, hv005, hv021, hv022, hv024, hv025, hv014,hv220, hv270, hml16, hv104, hv105, hv115, 
                                                      hc27, ha54,ha66,hv106, hml18, hv227, hv009, hv013, hv216, hml1, nommiimenage, potuse,potuse_ajusted, 
                                                      HH_at_least_one,ratio_HH_2,saturation,access,indi,access2,access3,net_use, net_use_among_access, defacto_pop)
## -----------------------------------------

## Create variable net use 
all_data_2018_analysis = all_data_2018_analysis %>% dplyr::mutate(net_use =ifelse(hml12 %in% c(1,2),"1", "0" )) ### hml10

all_data_2018_analysis$net_use = as.factor(as.character(all_data_2018_analysis$net_use))


## Create variable rurale and urban
all_data_2018_analysis = all_data_2018_analysis %>% dplyr::mutate(urb = ifelse(hv025 == 1, "0", "1"))

table(all_data_2018_analysis$urb)

all_data_2018_analysis$urb = as.factor(as.character(all_data_2018_analysis$urb))
#all_data_2018_analysis$urb = revalue(all_data_2018_analysis$hv025, c("1" = "1", "2" = "0"))

all_data_2018_analysis$urb = relevel(all_data_2018_analysis$urb, ref = "0")


## Create regional variable
all_data_2018_analysis = all_data_2018_analysis %>%dplyr::mutate(region = case_when(hv024 == 1 ~ "Boke", hv024 == 2 ~ "Conakry",
                                                                                    hv024 == 3 ~ "Faranah", hv024 == 4 ~ "Kankan",
                                                                                    hv024 == 5 ~ "Kindia", hv024 == 6 ~ "Labe",
                                                                                    hv024 == 7 ~ "Mamou", hv024 == 8 ~ "Nzerekore"))

all_data_2018_analysis$region = as.factor(as.character(all_data_2018_analysis$region))

all_data_2018_analysis$region = relevel(all_data_2018_analysis$region, ref = "Nzerekore")

## create sex variable
all_data_2018_analysis= all_data_2018_analysis %>% dplyr::mutate(sex=ifelse(hv104 == 1, "0", "1"))

table(all_data_2018_analysis$sex)
all_data_2018_analysis$sex = as.factor(as.character(all_data_2018_analysis$sex))

all_data_2018_analysis$sex = relevel(all_data_2018_analysis$sex, ref = "0")

## create wealth index

all_data_2018_analysis = all_data_2018_analysis %>%dplyr::mutate(wealth = case_when(hv270 == 1 ~ "lowest",
                                                                                    hv270 == 2 ~ "second", hv270 == 3 ~ "Middle", hv270 == 4 ~ "Fourth",
                                                                                    hv270 == 5 ~ "Highest"))

all_data_2018_analysis$wealth = mapvalues(all_data_2018_analysis$wealth, from = c("lowest", "second", "Middle", "Fourth", "Highest"),
                                          to = c("1", "2", "3", "4", "5"))
all_data_2018_analysis$wealth = as.factor(as.character(all_data_2018_analysis$wealth))

all_data_2018_analysis$wealth = relevel(all_data_2018_analysis$wealth, ref = "1")


## Create education variable


all_data_2018_analysis = all_data_2018_analysis %>% dplyr::mutate(Edu =case_when(hv106 == 0 ~ "None", hv106 == 1 ~ "primary",
                                                                                 hv106 == 2 | hv106 == 3 ~ "High", hv106 == 8 ~ "dont know"))



all_data_2018_analysis$Edu = mapvalues(all_data_2018_analysis$Edu, from = c("None", "primary", "High", "dont know"),
                                       to = c("0", "1", "2", "3"))

all_data_2018_analysis$Edu = as.factor(as.character(all_data_2018_analysis$Edu))

all_data_2018_analysis$Edu = relevel(all_data_2018_analysis$Edu, ref = "2")


## create household size

all_data_2018_analysis = all_data_2018_analysis %>%dplyr::mutate(hh_size=ifelse(hv013 <= 4, '1-4', ifelse(hv013 >4 & hv013<=7, '5-7', ifelse(hv013 >=8, '>8', ''))))

all_data_2018_analysis$hh_size = as.factor(as.character(all_data_2018_analysis$hh_size))

all_data_2018_analysis$hh_size = relevel(all_data_2018_analysis$hh_size, ref = ">8")
table(all_data_2018_analysis$hh_size)


## Create woman education
#all_data_2018_analysis = all_data_2018_analysis %>%dplyr::mutate(Wo_Edu =case_when(ha66 == 0 ~ "None", ha66 == 1 ~ "primary",
#                                                             ha66 == 2 | ha66 == 3 ~ "High"))

#all_data_2018_analysis$Wo_Edu = mapvalues(all_data_2018_analysis$Wo_Edu, from = c("None", "primary", "High"),
#                                       to = c("0", "1", "2"))


#all_data_2018_analysis$Wo_Edu = as.factor(as.character(all_data_2018_analysis$Wo_Edu))
#all_data_2018_analysis$Wo_Edu = relevel(all_data_2018_analysis$Wo_Edu, ref = "2")

#table(all_data_2018_analysis$Wo_Edu)


## status marital

all_data_2018_analysis=all_data_2018_analysis %>% dplyr::mutate(M = replace_na(hv115, 5))
all_data_2018_analysis = all_data_2018_analysis %>%dplyr::mutate(Marital =case_when(M == 0 ~ "Never Married", M == 1 ~ "Married",
                                                                                    M == 3 ~ "Wind", M == 4 ~ "Divorced", M == 5~ "not eligible"))

all_data_2018_analysis$Marital = as.factor(as.character(all_data_2018_analysis$Marital))


### Married == 0, NM==1, Divorced == 2, wind == 3
all_data_2018_analysis$Marital = mapvalues(all_data_2018_analysis$Marital, from = c("Never Married","Married", "Wind" ,"Divorced", "not eligible"),
                                           to = c("0", "1", "2", "3", "4"))

all_data_2018_analysis$Marital = relevel(all_data_2018_analysis$Marital, ref = "0")
table(all_data_2018_analysis$Marital)

## Age of household member
all_data_2018_analysis$age = cut(all_data_2018_analysis$hv105, c(0, 5, 10, 20, 40, 50, 60, 98), include.lowest = T, right = T)

all_data_2018_analysis$age = relevel(all_data_2018_analysis$age, ref = "[0,5]")
table(all_data_2018_analysis$age)

## Age of head of household (hv220)
table(all_data_2018_analysis$hv220)

all_data_2018_analysis$head_age = cut(all_data_2018_analysis$hv220, c(15, 30, 40, 50, 60, 98), include.lowest = T, righ = T)


all_data_2018_analysis$head_age = relevel(all_data_2018_analysis$head_age, ref = "(40,50]")
table(all_data_2018_analysis$head_age)

## create the Number of mosquito bed nets

#table(all_data_2018_analysis$hml1)

all_data_2018_analysis = all_data_2018_analysis %>% dplyr::mutate(ratio_HH_2 = ifelse(ratio_HH_2 == 1, "Sufficient_net", "Not_sufficient"))

#all_data_2018_analysis = all_data_2018_analysis %>% dplyr::mutate(nets_num = ifelse(nommiimenage <= 2, "At most 2", " More than 2"))

#all_data_2018_analysis = rename(all_data_2018_analysis, "nets_num" = hml1)
#all_data_2018_analysis = rename.variable(all_data_2018_analysis, "hml1", "nets_num")

table(all_data_2018_analysis$ratio_HH_2)

piechart(all_data_2018_analysis$nets_num)

all_data_2018_analysis$nets_num = as.factor(as.character(all_data_2018_analysis$nets_num))
all_data_2018_analysis$ratio_HH_2 = as.factor(as.character(all_data_2018_analysis$ratio_HH_2))

all_data_2018_analysis$nets_num = relevel(all_data_2018_analysis$ratio_HH_2, ref = "Not_sufficient")


## ownership by hh

all_data_2018_analysis = all_data_2018_analysis %>% dplyr::mutate(access = ifelse(hv227_PR == 1, "1", "0"))

all_data_2018_analysis$HH_at_least_one = as.factor(as.numeric(all_data_2018_analysis$HH_at_least_one))

all_data_2018_analysis$HH_at_least_one = relevel(all_data_2018_analysis$HH_at_least_one, ref = "1")


### access
all_data_2018_analysis$access2 = as.factor(as.numeric(all_data_2018_analysis$access2))

all_data_2018_analysis$access2 = relevel(all_data_2018_analysis$access2, ref = "1")

all_data_2018_analysis$access3 = as.factor(as.numeric(all_data_2018_analysis$access3))

all_data_2018_analysis$access3 = relevel(all_data_2018_analysis$access3, ref = "1")
## Incidence (routine data)
table(all_data_2018_analysis$Adjusted_inc)

all_data_2018_analysis$Incidence = as.factor(as.character(all_data_2018_analysis$Adjusted_inc))

all_data_2018_analysis$Incidence = relevel(all_data_2018_analysis$Incidence, ref = "600<800")


### Pregnant status

table(all_data_2018_analysis$hml18)

val_labels(all_data$hml18)

## Modalite pour les enfants et les hommes (NA)
all_data_2018_analysis=all_data_2018_analysis %>% dplyr::mutate(preg = replace_na(hml18, 2))

all_data_2018_analysis = all_data_2018_analysis %>%dplyr::mutate(preg = ifelse(hml18 == 1, "1", "0"))

all_data_2018_analysis$preg = as.factor(as.character(all_data_2018_analysis$preg))

all_data_2018_analysis$preg = relevel(all_data_2018_analysis$preg, ref = "1")

## Presence under 5 children

val_labels(all_data$hml16)
table(all_data_2018_analysis$hv014)

all_data_2018_analysis <- all_data_2018_analysis %>%  mutate(u5_hh = ifelse(hv014 > 0, "1", "0"))

all_data_2018_analysis$u5_hh = as.factor(as.character(all_data_2018_analysis$u5_hh))

### Number of rooms in the hh

all_data_2018_analysis = all_data_2018_analysis %>% dplyr::mutate(rooms = ifelse(hv216 <=3, "1-3", ifelse(hv216 >=4 & hv216<=6, "4-6", ifelse(hv216>=7, "More than 7", ''))))
class(all_data_2018_analysis$rooms)
all_data_2018_analysis$rooms = as.factor(as.character(all_data_2018_analysis$rooms))

all_data_2018_analysis$rooms = relevel(all_data_2018_analysis$rooms, ref = "1-3")


### Only Use among those have access
use_access_data = all_data_2018_analysis %>% dplyr::filter(access3 == 1)

data_use_access = use_access_data %>% dplyr::select(hv005, hv021, hv022, net_use, sex, age, head_age, wealth, hh_size, urb, region, u5_hh, preg, Edu,
                                                    Marital, rooms)