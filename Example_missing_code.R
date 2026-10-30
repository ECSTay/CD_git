######example code to check missing data

temp <- data_elig[which(is.na(data_elig$BPD_any)),] #36
temp2 <- temp[which(temp$withdraw_yn == 1),]  ##34 ### 1 = NA CD-03018,  1 = 0 CD-01267
temp3 <- temp[which(temp$withdraw_yn != 1),]


##TTE summary stats

data_S7_Control <- data_S7[which(data_S7$rand_gp == 0),]
summary(data_S7_Control$discharge_pma) #38.3
data_S7_Intervention <- data_S7[which(data_S7$rand_gp == 1),]
summary(data_S7_Intervention$discharge_pma) #38.3

stratified_results7 <- data_S7 %>%
  group_by(ga_strata.f, rand_gp) %>%
  summarise(
    numerator = sum(discharge_pma >= 40, na.rm = TRUE),
    denominator = sum(!is.na(discharge_pma)),
    proportion = mean(discharge_pma >= 40, na.rm = TRUE),
    .groups = "drop"
  )

print(stratified_results7)
# ga_strata.f rand_gp numerator denominator proportion
# <fct>         <dbl>     <int>       <int>      <dbl>
# 1 0                 0        60         105      0.571
# 2 0                 1        50          99      0.505
# 3 1                 0        49         248      0.198
# 4 1                 1        41         237      0.173

temp <-data_S6[which(is.na(data_S6$dcgdtm.dfor)),] #42
temp3 <- data_S6[which(is.na(data_S6$dcgdtm.dfor)),] #43


data_S6_Control <- data_S6[which(data_S6$rand_gp == 0),]
summary(data_S6_Control$discharge_pma) #37.4

stratified_results6 <- data_S6 %>%
  group_by(ga_strata.f, rand_gp) %>%
  summarise(
    numerator = sum(discharge_pma >= 40, na.rm = TRUE),
    denominator = sum(!is.na(discharge_pma)),
    proportion = mean(discharge_pma >= 40, na.rm = TRUE),
    .groups = "drop"
  )

print(stratified_results6)
# 
# ga_strata.f rand_gp numerator denominator proportion
# <fct>         <dbl>     <int>       <int>      <dbl>
# 1 0                 0        49         109      0.450
# 2 0                 1        37         103      0.359
# 3 1                 0        30         251      0.120
# 4 1                 1        27         238      0.113

temp <-data_S6[which(is.na(data_S6$dcgdtm.dfor)),] #42
temp3 <- data_S6[which(is.na(data_S6$dcgdtm.dfor)),] #43

