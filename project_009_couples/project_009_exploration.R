# https://data.stanford.edu/hcmst2017
load("~/HCMST 2017 to 2022 small public version 2.2.rdata")
d <- `HCMST small public version 2.2`
# View(d)
colnames(d) <- tolower(colnames(d))
write.csv(d, "project_009_hcmst_everything.csv", row.names=FALSE)

colnames(d)[1:5]
length(d$caseid_new)==length(unique(d$caseid_new)) # unique

d$w3_weight # why does it have NAs? 
# only contains qualified respondents from 2017 genpop sample. n=1539
# View(d[!is.na(d$w3_weight),])
nrow(d[!is.na(d$w3_weight),]) # confirmed to only be 1539

nrow(d[!is.na(d$w3_weight_lgb),]) #only qualified LGB respondents across all samples # n=249

#LGB is oversample: Self-identified Lesbian, Gay, and Bisexual respondents were oversampled.

nrow(d[!is.na(d$w3_combo_weight),]) # n=1=1722
nrow(d[!is.na(d$w3_attrition_adj_weight),]) # n=1722

### return and figure out.
# View(d[!is.na(d$w3_combo_weight),]) # n=1722
# View(d[!is.na(d$w3_weight) & !is.na(d$w3_weight_lgb),]) # these are LGB genpop and oversample? n=66
# View(d[is.na(d$w3_weight) & is.na(d$w3_weight_lgb),]) # these are LGB genpop and oversample? n=66

colnames(d)[6:11]
# same as above but for w2 & w1

colnames(d)[12:19]
apply(d[12:17],2,table)
d$w3_xpartner_type # 1= married; 2 = unmarried partners; 3= unpartnered; 4= never had a partner; partnership status in 2020
d$w3_duration # duration spent on survey

d$w3_xcohab # 1 = yes, 2 = no

apply(d[19],2,table)

colnames(d)[20:30]
apply(d[20:22],2,table)
apply(d[23:30],2,table)

imp_cols <- colnames(d)[1:5]
imp_cols <- c(imp_cols,colnames(d)[6:16],colnames(d)[19:20])
imp_cols <- c(imp_cols,colnames(d)[23],colnames(d)[25:29])

colnames(d)[31:40]
table(d$w3_real_inc)
table(d$w3_ppinc)
apply(d[33:40],2,table)
table(d$w3_real_inc)

imp_cols <- c(imp_cols,colnames(d)[31])
imp_cols <- c(imp_cols,colnames(d)[33:35])
imp_cols <- c(imp_cols,colnames(d)[37:40])

colnames(d)[41:50]
apply(d[41:50],2,table)
imp_cols <- c(imp_cols,colnames(d)[42:50])

colnames(d)[51:60]
apply(d[52:60],2,table)
imp_cols <- c(imp_cols,colnames(d)[52:54])
imp_cols <- c(imp_cols,colnames(d)[58:60])


colnames(d)[61:70]
apply(d[52:60],2,table)
table(d$w3_sex_frequency, d$w3_weekly_sex_frequency)
imp_cols <- c(imp_cols,colnames(d)[62:70])

colnames(d)[71:80]
apply(d[71:80],2,table)
imp_cols <- c(imp_cols,colnames(d)[71:80])

colnames(d)[81:90]
apply(d[81:90],2,table)
imp_cols <- c(imp_cols,colnames(d)[81:82])
imp_cols <- c(imp_cols,colnames(d)[85:90])

colnames(d)[91:100]
apply(d[91:100],2,table)
imp_cols <- c(imp_cols,colnames(d)[c(91:95,97:100)])

colnames(d)[101:110]
apply(d[101:110],2,table)
imp_cols <- c(imp_cols,colnames(d)[c(101:103,105:106,108:110)])

colnames(d)[111:120]
apply(d[111:120],2,table)
imp_cols <- c(imp_cols,colnames(d)[c(111:120)])


colnames(d)[121:130]
apply(d[121:130],2,table)

colnames(d)[131:140]
apply(d[131:140],2,table)

colnames(d)[141:150]
apply(d[141:150],2,table)
imp_cols <- c(imp_cols,colnames(d)[c(121:141)])

colnames(d)[151:160]
apply(d[151:160],2,table)

colnames(d)[161:170]
apply(d[161:170],2,table)

colnames(d)[171:180]
apply(d[171:180],2,table)
imp_cols <- c(imp_cols,colnames(d)[c(158:177,179:180)])

colnames(d)[181:190]
apply(d[181:190],2,table)

colnames(d)[191:200]
apply(d[191:195],2,table)
apply(d[196:198],2,table)
imp_cols <- c(imp_cols,colnames(d)[c(181:198)])

sum(unique(imp_cols)!=imp_cols)



# grep(colnames(d),"w3_")

replaced_w2 <- gsub("w3_","w2_",imp_cols)
replaced_w1 <- gsub("w3_","w1_",imp_cols)


imp_cols <- c(imp_cols,replaced_w2, replaced_w1)
imp_cols <- unique(imp_cols)

d_sub <- d[colnames(d) %in% imp_cols]

write.csv(d_sub, "project_009_hcmst.csv", row.names=FALSE)
