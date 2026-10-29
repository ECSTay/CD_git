######example code to check missing data

temp <- data_elig[which(is.na(data_elig$BPD_any)),] #36
temp2 <- temp[which(temp$withdraw_yn == 1),]  ##34 ### 1 = NA CD-03018,  1 = 0 CD-01267
temp3 <- temp[which(temp$withdraw_yn != 1),]
