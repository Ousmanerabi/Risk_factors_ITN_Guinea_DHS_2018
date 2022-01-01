### Install all these packages and load them
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
library(tidyr)

##1. Read the datasets and select the variables to include in the ITN use among those with access 
all_data_2018_analysis = read.csv('C:/Users/ode3599/Box/NU-malaria-team/data/guinea_dhs/data_analysis/master/data/data_PR/data_PR_HR.csv')

all_data_2018_analysis = data_PR_HR %>% dplyr::select(hhid, hv001, hv002, hvidx, hv003, hv005, hv021, hv022, hv024, hv025, hv014,hv220, hv270, hml16, hv104, hv105, hv115, 
                                                      hc27, ha54,ha66,hv106, hml18, hv227, hv009, hv013, hv216, hml1, nommiimenage, potuse,potuse_ajusted, 
                                                      HH_at_least_one,ratio_HH_2,saturation,access,indi,access2,access3, net_use, net_use_among_access, defacto_pop)



all_data_2018_analysis = all_data_2018_analysis %>% 
  dplyr::mutate(urb = ifelse(hv025 == 1, "0", "1"),
                region = case_when(hv024 == 1 ~ "Boke", hv024 == 2 ~ "Conakry",
                                   hv024 == 3 ~ "Faranah", hv024 == 4 ~ "Kankan",
                                   hv024 == 5 ~ "Kindia", hv024 == 6 ~ "Labe",
                                   hv024 == 7 ~ "Mamou", hv024 == 8 ~ "Nzerekore"),
                sex=ifelse(hv104 == 1, "0", "1"),
                wealth = case_when(hv270 == 1 ~ "lowest",
                                   hv270 == 2 ~ "second", hv270 == 3 ~ "Middle", hv270 == 4 ~ "Fourth",
                                   hv270 == 5 ~ "Highest"),
                Edu =case_when(hv106 == 0 ~ "None", hv106 == 1 ~ "primary",
                               hv106 == 2 | hv106 == 3 ~ "High", hv106 == 8 ~ "dont know"),
                hh_size=ifelse(hv013 <= 4, '1-4', ifelse(hv013 >4 & hv013<=7, '5-7', ifelse(hv013 >=8, '>8', ''))),
                M = replace_na(hv115, 5),
                Marital =case_when(M == 0 ~ "Never Married", M == 1 ~ "Married",
                                   M == 3 ~ "Wind", M == 4 ~ "Divorced", M == 5~ "not eligible"),
                age = cut(hv105, c(0, 5, 10, 20, 40, 50, 60, 98), include.lowest = T, right = T),
                head_age = cut(hv220, c(15, 30, 40, 50, 60, 98), include.lowest = T, righ = T),
                ratio_HH_2 = ifelse(ratio_HH_2 == 1, "Sufficient_net", "Not_sufficient"),
                preg = replace_na(hml18, 2),
                preg = case_when(preg == 1 ~  "1", preg == 0 ~ "O",
                                 preg == 2 ~  '2'),
                u5_hh = ifelse(hv014 > 0, "1", "0"),
                rooms = ifelse(hv216 <=3, "1-3", ifelse(hv216 >=4 & hv216<=6, "4-6", ifelse(hv216>=7, "More than 7", ''))))

#2. Reorder the level for the variables
cols = c('net_use', 'urb', 'region', 'sex', 'wealth', 'Edu', 'hh_size', 'Marital', 'age', 'head_age',
         'ratio_HH_2', 'preg', 'u5_hh', 'rooms')

all_data_2018_analysis[cols] = lapply(all_data_2018_analysis[cols], factor)


all_data_2018_analysis$urb = relevel(all_data_2018_analysis$urb, ref = "0")

all_data_2018_analysis$region = relevel(all_data_2018_analysis$region, ref = "Nzerekore")

all_data_2018_analysis$sex = relevel(all_data_2018_analysis$sex, ref = "0")

all_data_2018_analysis$wealth = mapvalues(all_data_2018_analysis$wealth, from = c("lowest", "second", "Middle", "Fourth", "Highest"),
                                          to = c("1", "2", "3", "4", "5"))
#all_data_2018_analysis$wealth = as.factor(as.character(all_data_2018_analysis$wealth))

all_data_2018_analysis$wealth = relevel(all_data_2018_analysis$wealth, ref = "1")


## Create education variable


all_data_2018_analysis$Edu = mapvalues(all_data_2018_analysis$Edu, from = c("None", "primary", "High", "dont know"),
                                       to = c("0", "1", "2", "3"))


all_data_2018_analysis$Edu = relevel(all_data_2018_analysis$Edu, ref = "2")

all_data_2018_analysis$hh_size = relevel(all_data_2018_analysis$hh_size, ref = ">8")

### Married == 0, NM==1, Divorced == 2, wind == 3
all_data_2018_analysis$Marital = mapvalues(all_data_2018_analysis$Marital, from = c("Never Married","Married", "Wind" ,"Divorced", "not eligible"),
                                           to = c("0", "1", "2", "3", "4"))

all_data_2018_analysis$Marital = relevel(all_data_2018_analysis$Marital, ref = "0")

all_data_2018_analysis$age = relevel(all_data_2018_analysis$age, ref = "[0,5]")

all_data_2018_analysis$head_age = relevel(all_data_2018_analysis$head_age, ref = "(40,50]")

all_data_2018_analysis$nets_num = relevel(all_data_2018_analysis$ratio_HH_2, ref = "Not_sufficient")

all_data_2018_analysis$preg = relevel(all_data_2018_analysis$preg, ref = "1")

all_data_2018_analysis$rooms = relevel(all_data_2018_analysis$rooms, ref = "1-3")

write.csv(all_data_2018_analysis, '/Users/ousmanediallo/Box/NU-malaria-team/data/guinea_dhs/data_analysis/master/data/data_PR/PR_data_cleaning.csv')

#3. select only use among those with access
use_access_data = all_data_2018_analysis %>% dplyr::filter(access3 == 1)

data_use_access = use_access_data %>% dplyr::select(hv005, hv021, hv022, net_use, sex, age, head_age, wealth, hh_size, urb, region, u5_hh, preg, Edu,
                                                    Marital, rooms)

write.csv(data_use_access, '/Users/ousmanediallo/Box/NU-malaria-team/data/guinea_dhs/data_analysis/master/data/data_PR/data_use_among_those_with_access_cleaning.csv')
