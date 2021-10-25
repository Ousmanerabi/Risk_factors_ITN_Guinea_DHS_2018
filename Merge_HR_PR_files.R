### Select data HR to merge to PR 

dhs_hh_2018 = dhs_hhs[[4]]

hh_ex = dhs_hh_2018 %>%
  dplyr::select(hhid, hv001, hv002, hv003, hv013, hv012, hv024, hv025, hv005, hv021, hv022, contains("hml10_"), hv227, hml1, hv216, hv014)

## Number of ITN in household
hh_ex$nommiimenage = rowSums(hh_ex[c("hml10_1", "hml10_2", "hml10_3", "hml10_4", "hml10_5", "hml10_6",
                                     "hml10_7")], na.rm = TRUE)


### hv013>0

hh_ex = subset(hh_ex, hv013 > 0)
### First way to calculate access et ownership in the HR file
## hv013 > 0 & (sum of hml10_1 - hml10_7 = 1)/hv013 >= 0.5)
hh_ex = hh_ex %>%
  dplyr::mutate(access = nommiimenage/hv013) %>%
  dplyr::mutate(access2 = ifelse(access >=0.5, 1, 0)) %>%
  dplyr::mutate(anyITN = ifelse(nommiimenage > 0, 1, 0)) %>%
  dplyr::mutate(numnet = hml1)
## hh has any ITN

hh_ex = hh_ex %>% dplyr::mutate(hh_has_itn = ifelse(nommiimenage > 0, 1, 0))

## calculate variable for hh with at least 1 ITN for 2 people (hv012 and hv013)
## itnpers = ITN per person in household
hh_ex = hh_ex %>%
  dplyr::mutate(itnpers = nommiimenage/hv013)

### Second solution to calculate access et ownership in the HR file

hh_ex = hh_ex %>% dplyr::mutate(person = hv013)%>%
  dplyr::mutate(itnper = nommiimenage/person) %>%
  dplyr::mutate(potuse = nommiimenage*2)%>%
  #dplyr::mutate(n_with_access = pmin(nommiimenage*2, stay))%>%
  dplyr::mutate(access=potuse/person) %>%
  dplyr::mutate(access2=ifelse(potuse/person > 1, 1, 0))%>%
  dplyr::mutate(numnet = hml1)


hh_ex = hh_ex %>%dplyr::mutate(hh_size=ifelse(hv013 >= 1 & hv, '1-4', ifelse(hv013 >4 & hv013<=7, '5-7', ifelse(hv013 >=8, '>8', ''))))

hh_ex$hh_size = as.factor(as.character(hh_ex$hh_size))


hh_ex = hh_ex %>%
  dplyr::mutate(potusenet = numnet*2) %>%
  ## proportion in hh with access to net of any kind
  dplyr::mutate(accessnet = potusenet/hv013)%>%
  dplyr::mutate(accessnet1 = ifelse(accessnet >1, 1, 0))

### Calcul du nombre de chambre par menage
hh_ex = hh_ex %>% dplyr::mutate(rooms = ifelse(hv216 <=3, "1-3", ifelse(hv216 >=4 & hv216<=6, "4-6", ifelse(hv216>=7, "More than 7", ''))))

## Number of children resident in the household and aged 5 and under
hh_ex = hh_ex %>% dplyr::mutate(Num_childre = ifelse())

## Select small dataset
hh_2018 = hh_ex %>%
  #dplyr::mutate(numnet = hml1) %>%
  #dplyr::filter(hh_has_itn == 1) %>%
  dplyr::select(hhid, nommiimenage, hh_has_itn, numnet, rooms)
## Merge PR file and HR (hh_2018)

data_PR_HR = merge(all_data, hh_2018, by = "hhid")

data_PR_HR = data_PR_HR %>% dplyr::mutate(region = case_when(hv024 == 1 ~ "Boke", hv024 == 2 ~ "Conakry",
                                                            hv024 == 3 ~ "Faranah", hv024 == 4 ~ "Kankan",
                                                            hv024 == 5 ~ "Kindia", hv024 == 6 ~ "Labe",
                                                            hv024 == 7 ~ "Mamou", hv024 == 8 ~ "Nzerekore"))
#data_PR_HR = merge(data_PR_HR, Incidence_who_2018_fin, by = "region")

## create stayed in the household last night hv103=1
#data_PR_HR = subset(data_PR_HR, hv103 = 1)

## calculate population access using the MERG method (potential ITN users in hh) and de facto population
## potuse = potential ITN users
##
## access2 = proportion in persons with access

data_PR_HR = data_PR_HR %>%
  ## Potential ITN users in hh potuse
  dplyr::mutate(potuse = nommiimenage*2)%>%
  group_by(hhid) %>%
  dplyr::mutate(defacto_pop = hv013) %>%
  dplyr::arrange(hhid) %>%
  #dplyr::mutate(n_with_access = pmin(nommiimenage*2, stay))%>%
  dplyr::mutate(access=potuse/defacto_pop) %>%
  dplyr::mutate(access2=ifelse(potuse/defacto_pop > 1, 1, 0)) %>%
  ## potusenet===Potential net users in hh
  dplyr::mutate(potusenet = numnet*2) %>%
  ## proportion in hh with access to net of any kind
  dplyr::mutate(accessnet = potusenet/hv013)%>%
  dplyr::mutate(accessnet1 = ifelse(accessnet >1, 1, 0))

### Merge
data_PR_HR = merge(data_PR_HR, Incidence_who_2018_finale, by ="region")


###





