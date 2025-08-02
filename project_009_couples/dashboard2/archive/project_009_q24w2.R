load("~/HCMST 2017 to 2022 small public version 2.2.rdata")
full_d <- `HCMST small public version 2.2`
colnames(full_d) <- tolower(colnames(full_d))

# Packages and libraries

# Package names
packages <- c("tidyverse","plotly","data.table")
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# necessary columns
q24_cols <- colnames(full_d)[grep("w1_q24_",colnames(full_d))][c(17:37, 40:44)]
full_d$All <- 1
full_d$never_dated <- ifelse(full_d$w1_partnership_status==4,1,0)

full_d$year_met <- coalesce(full_d$w1_q21a_year, full_d$w2_q21a_year, full_d$w3_q21a_year)
necessary_columns <- c(q24_cols,
                       "w2_surveyed",
                       "w1_partnership_status",
                       "w2_section",
                       "w2_relationship_end",
                       "w2_partner_type",
                       "w3_surveyed",
                       "w3_partner_type",
                       "w3_relationship_end_combo",
                       "never_dated")

q24_cols_never <- c(q24_cols,"never_dated")

# # ### TESTING FILTERING FOR Q24 SOURCE 
# user <- c("School","College", "Military","Church",
#           "Volunteer Organization",
#           "Customer", "Bar or Restaurant", "Party", "Internet (Other)", 
#           "Internet Dating or Phone Apps",
#           "Internet (Social Network)","Online Gaming","Internet (Chat)",
#           "Internet Site Not Mainly Dedicated to Dating","Public","Blind Date",
#           "Vacation","Non Internet Single Service","Business Trip",
#           "Work Neighbors", "Online",
#           "Family","Friend","Neighbors",
#           "Coworkers","Online Excluding Phone Apps","Never Dated")
# 
# variable <- c("w1_q24_school","w1_q24_college", "w1_q24_mil","w1_q24_church",
#               "w1_q24_vol_org", "w1_q24_customer", "w1_q24_bar_restaurant", "w1_q24_party", 
#               "w1_q24_internet_other",
#               "w1_q24_internet_dating",
#               "w1_q24_internet_soc_network", "w1_q24_internet_game", "w1_q24_internet_chat",
#               "w1_q24_internet_org","w1_q24_public","w1_q24_blind_date",
#               "w1_q24_vacation", "w1_q24_singles_serve_nonint", "w1_q24_business_trip",
#               "w1_q24_work_neighbors", "w1_q24_met_online",
#               "w1_q24_met_through_family", "w1_q24_met_through_friend", "w1_q24_met_through_as_nghbrs",
#               "w1_q24_met_as_through_cowork", "w1_q24_metonline_no_phone_apps", "never_dated"
#               )
# filt <- "Work Neighbors"
# ind <- match(filt,user)
# var <- variable[ind]
# 
# if (filt=="All"){
#   full_d <- full_d
# } else {
#   full_d <- eval(parse(text= paste0('subset(full_d,' , var,'==1)' ))) #358
# }



# ### END TESTING

### TESTING FILTERING  FOR lgbtq+ COUPLES
# filt <- "Straight"
# if (filt=="LGB"){
#   full_d <- base::subset(full_d, w1_identity_all_modified %in% 2:5 | #582
#                            p17_pppa_lgb %in% c(1,3,4) | #613
#                            p18_pppa_lgb %in% c(1,3,4) | #629
#                            p19_pppa_lgb %in% c(1,3,4) | # 640
#                            w1_prior_identity_lgb==1 | #641
#                            w2_xpppa_lgb_num %in% c(1,3,4) | #655
#                            w3_gen_pop_sample==0 #655
#   )
# }
# 
# if (filt=="Straight"){
#   full_d <- base::subset(full_d, 
#                          w1_identity_all_modified %in% 1 | #2917
#                            p17_pppa_lgb==2 | #2947
#                            p18_pppa_lgb==2 | #2959
#                            p19_pppa_lgb==2 | # 2969
#                            w1_prior_identity_lgb==0 | #2982
#                            w2_xpppa_lgb_num==2 #3286
#   )
# }
# 
# ### END TESTING

### TESTING  FILTERING FOR UNHAPPY COUPLES
# w3_who_breakup
# w3_women_wanted_breakup
# w2_who_breakup_combo
# w2_women_wanted_breakup

# Respondent is Male
# w3_ppgender==1
# w2_ppgender==1 
# w1_ppgender==1  

#Female
# w3_ppgender==2 
# w2_ppgender==2 
# w1_ppgender==2 

# Partner is Male
# w1_q4==1

# Partner is female
# w1_q4==2

# "All Couples", "One party wanted to end it", "A woman ended it","A man ended it", "Both Wanted to End it""
# filt="All Couples"
# if (filt=="One party wanted to end it") {
#   full_d <- base::subset(full_d,
#                          w3_who_breakup %in% 1:2 |
#                            w2_who_breakup_combo %in% 1:2
#   )
# } else if (filt=="A woman ended it"){
#   full_d <- base::subset(full_d,
#                          (w3_who_breakup==1 & 
#                             (w3_ppgender==2 |
#                                w2_ppgender==2 |
#                                w1_ppgender==2)
#                          ) |
#                            (w3_who_breakup==2 &
#                               (w1_q4==2 |
#                                  w2_q4==2)) |
#                            (w2_who_breakup_combo==1 & 
#                               (w3_ppgender==2 |
#                                  w2_ppgender==2 |
#                                  w1_ppgender==2)
#                            ) |
#                            (w2_who_breakup_combo==2 &
#                               (w1_q4==2 |
#                                  w2_q4==2))
#   )
# } else if (filt=="A man ended it"){
#   full_d <- base::subset(full_d,
#                          (w3_who_breakup==1 & 
#                            (w3_ppgender==1 |
#                               w2_ppgender==1 |
#                               w1_ppgender==1)
#                           ) |
#                            (w3_who_breakup==2 &
#                               (w1_q4==1 |
#                                  w2_q4==1)) |
#                            (w2_who_breakup_combo==1 & 
#                               (w3_ppgender==1 |
#                                  w2_ppgender==1 |
#                                  w1_ppgender==1)
#                            ) |
#                            (w2_who_breakup_combo==2 &
#                               (w1_q4==1 |
#                                  w2_q4==1))
#   )
# } else if (filt=="Both wanted to end it"){
#   full_d <- base::subset(full_d,
#                          w3_who_breakup==3 |
#                            w2_who_breakup_combo==3
#   )
# } else {
#   full_d <- full_d
# }

### END TESTING

### Line graph
q24_cols <- colnames(full_d)[grep("w1_q24_",colnames(full_d))][c(17:37, 40:44)]
cols <- c("year_met", q24_cols)

## Remember to add in W2 and W3 meetings
colnames(full_d)[grep("w2_q24_",colnames(full_d))]
colnames(full_d)[grep("w3_q24_",colnames(full_d))]

d_q24 <- full_d[,cols]

year_q24 <- pivot_longer(
  data = d_q24,
  cols = c(q24_cols)
) %>% group_by(year_met,name) %>%  
  summarise(total = sum(value, na.rm = TRUE), .groups = "keep")

year_q24 <- year_q24 %>%
  #### renamed columns
  mutate(name = recode(name,
                       w1_q24_bar_restaurant = 'Bar or Restaurant',
                       w1_q24_college = 'College',
                       w1_q24_internet_dating =  'Internet Dating',
                       w1_q24_internet_other = 'Other Internet',
                       w1_q24_met_online = 'Met Online',
                       w1_q24_party = 'Party',
                       w1_q24_school = 'School',
                       w1_q24_blind_date = "Blind Date",
                       w1_q24_business_trip = "Business Trip",
                       w1_q24_church = "Place of Worship",
                       w1_q24_customer = "Customer",
                       w1_q24_internet_chat = "Internet Chat",
                       w1_q24_internet_game = 'Internet Game',
                       w1_q24_internet_org = 'Internet Organization',
                       w1_q24_internet_soc_network = 'Internet Social Network',
                       w1_q24_mil = 'Military',
                       w1_q24_public = 'Public',
                       w1_q24_singles_serve_nonint = 'Single Service (Not Internet)',
                       w1_q24_vacation = 'Vacation',
                       w1_q24_vol_org = 'Volunteer Organization',
                       w1_q24_work_neighbors = 'Work Neighbors',
                       w1_q24_met_through_family = 'Met Through Family',
                       w1_q24_met_through_friend = 'Met Through Friend',
                       w1_q24_met_through_as_nghbrs = 'Met As Neighbors',
                       w1_q24_met_as_through_cowork = 'Met As Coworkers',
                       w1_q24_metonline_no_phone_apps = 'Met Online (no phone apps)')
  )

ggplot(data = year_q24, aes(x=year_met, y=total)) + 
  geom_line(aes(colour=name)) +
  theme_bw(base_size = 16) +
  labs(x = "Year They Met", y = "Number of Couples", colour = "How They Met") +
  scale_y_continuous(limits = c(0,40),
                     breaks = seq(0, 40, by=10)) +
  scale_x_continuous(limits = c(1935,2020),
                     breaks = seq(1935, 2020, by=10)) 

### end line graph

q24_sums <- colSums(full_d[q24_cols_never], na.rm=T)

if (sum(q24_sums>0)==length(q24_cols_never)){
  q24_cols <- q24_cols_never
} else {
  q24_nonzeros <- q24_cols_never[q24_sums>0]
  q24_cols <- q24_nonzeros
}

d <- full_d[,necessary_columns]
n <- nrow(d)

### TESTING Filter for year met partner
full_d$w1_q21a_year
full_d$w2_q21a_year

d <- full_d
d$w2_status <- "Missed" # Just a catch-all in case anyone gets missed in the case-when statement

d$w2_status <- dplyr::case_when(
  d$w2_surveyed==0 ~ "Not Surveyed",
  # Married
  d$w2_section==1 ~ "Still Married",
  d$w1_partnership_status=="Married" & d$w2_relationship_end==1  ~ "Married -> Divorced",
  d$w1_partnership_status=="Married" & d$w2_relationship_end==2  ~ "Married -> Separation or Other Breakup",
  d$w1_partnership_status=="Married" & d$w2_relationship_end==3 ~ "Married -> Partner Died",
  # Unmarried Partners
  d$w1_partnership_status=="Unmarried Partners" & d$w2_section==3 & d$w2_partner_type==1 ~ "Unmarried Partners -> Married", # unmarried, still with unmarried partner but reported marriage.
  d$w1_partnership_status=="Unmarried Partners" & d$w2_section==3 & d$w2_partner_type==2 ~ "Unmarried Partners -> Unmarried Partners", # unmarried, still with unmarried partner but reported unmarried partnership.
  d$w1_partnership_status=="Unmarried Partners" & d$w2_relationship_end==1 ~ "Unmarried Partners -> Divorced",
  d$w1_partnership_status=="Unmarried Partners" & d$w2_relationship_end==2 ~ "Unmarried Partners -> Separation or Other Breakup",
  d$w1_partnership_status=="Unmarried Partners" & d$w2_relationship_end==3 ~ "Unmarried Partners -> Partner Died",
  # Single but had partner in past
  d$w1_partnership_status=="Single (past)" & d$w2_relationship_end==1 ~ "Single (past) -> Divorced",
  d$w1_partnership_status=="Single (past)" & d$w2_relationship_end==2 ~ "Single (past) -> Separation or Other Breakup",
  d$w1_partnership_status=="Single (past)" & d$w2_relationship_end==3 ~ "Single (past) -> Partner Died",
  d$w1_partnership_status=="Single (past)" & d$w2_partner_type==1 ~ "Single (past) -> Married",
  d$w1_partnership_status=="Single (past)" & d$w2_partner_type==2 ~ "Single (past) -> Unmarried Partners",
  d$w1_partnership_status=="Single (past)" & d$w2_partner_type==3 ~ "Single (past) -> Single",
  # Never had a partner,
  d$w1_partnership_status=="Single" & d$w2_partner_type==1 ~ "Never in a relationship -> Married",
  d$w1_partnership_status=="Single" & d$w2_partner_type==2 ~ "Never in a relationship -> Unmarried Partners",
  d$w1_partnership_status=="Single" & d$w2_partner_type==4 ~ "Never in a relationship",
  # Mis-coded data: w1 had partner, w2_section said no longer married/partner  w/ w1
  # but w2_relationship end says no report of breakup.
  (d$w1_partnership_status=="Married" & d$w2_section==2 & d$w2_relationship_end==0) |
    (d$w1_partnership_status=="Unmarried Partners" & d$w2_section==4 & d$w2_relationship_end==0) ~ "Miscoded data",
  TRUE ~ d$w2_status
)

d$w2_status <- factor(d$w2_status, levels=c(
  "Still Married",
  "Married -> Divorced",
  "Married -> Separation or Other Breakup",
  "Married -> Partner Died",
  "Unmarried Partners -> Married",
  "Unmarried Partners -> Unmarried Partners",
  "Unmarried Partners -> Divorced",
  "Unmarried Partners -> Separation or Other Breakup",
  "Unmarried Partners -> Partner Died",
  "Single (past) -> Divorced",
  "Single (past) -> Separation or Other Breakup",
  "Single (past) -> Partner Died",
  "Single (past) -> Married",
  "Single (past) -> Unmarried Partners",
  "Single (past) -> Single",
  "Never in a relationship -> Married",
  "Never in a relationship -> Unmarried Partners",
  "Never in a relationship",
  "Not Surveyed",
  "Miscoded data",
  "Missed"
))

divorced <- levels(d$w2_status)[grep("-> divorced",tolower(levels(d$w2_status)))]
breakup <- levels(d$w2_status)[grep("-> separation",tolower(levels(d$w2_status)))]
married1 <- levels(d$w2_status)[grep("-> married",tolower(levels(d$w2_status)))]
married2 <- levels(d$w2_status)[grep("still married",tolower(levels(d$w2_status)))]
married <- c(married1,married2)
died <- levels(d$w2_status)[grep("died",tolower(levels(d$w2_status)))]
unmarriedpartners <- levels(d$w2_status)[grep("-> unmarried",tolower(levels(d$w2_status)))]
singlepast <- levels(d$w2_status)[grep("-> single",tolower(levels(d$w2_status)))]
singlenever <- "Never in a relationship"
single <- c(singlepast, singlenever)
notsurveyed <- "Not Surveyed"
miscoded <- "Miscoded data"

d$w2_status_cat <- dplyr::case_when(
  d$w2_status %in% divorced ~ "Divorced",
  d$w2_status %in% breakup ~ "Breakup",
  d$w2_status %in% married ~ "Married",
  d$w2_status %in% died ~ "Died",
  d$w2_status %in% unmarriedpartners ~ "Unmarried Partners",
  d$w2_status %in% single ~ "Single",
  d$w2_status %in% notsurveyed ~ "Not Surveyed",
  d$w2_status %in% miscoded ~ "Miscoded",
  TRUE~"Missed"
)

d$w3_status_cat <- "Missed"
d$w3_status_cat <- dplyr::case_when(
  d$w3_surveyed==0 ~ "Not Surveyed",
  d$w3_partner_type==1 ~ "Married",
  d$w3_relationship_end_combo==1 ~ "Divorced",
  d$w3_relationship_end_combo==2 ~ "Breakup",
  d$w3_relationship_end_combo==3 ~ "Died",
  d$w3_partner_type==2 & 
    (d$w3_relationship_end_combo==1 | is.na(d$w3_relationship_end_combo)) ~ "Unmarried Partners",
  d$w3_partner_type==3 ~ "Single",
  TRUE ~ d$w3_status_cat
)


View(d[is.na(d$w1_q21a_year) & !is.na(d$w2_q21a_year),
       c("w1_partnership_status","w2_status_cat","w3_status_cat","w1_q21a_year","w2_q21a_year")])

d$year_met <- coalesce(d$w1_q21a_year, d$w2_q21a_year, d$w3_q21a_year)
d$year_met %in% seq(1939,2021)
seq(1939,2021)
### END TESTING

d$w1_partnership_status <- dplyr::case_when(
  d$w1_partnership_status==1 ~ "Married",
  d$w1_partnership_status==2 ~ "Unmarried Partners",
  d$w1_partnership_status==3 ~ "Single (Past)",
  d$w1_partnership_status==4 ~ "Single"
)

# How Met -> W1 status
listed <- list()
listindex <- 1
q24_methods <- c()

for (i in q24_cols){
  print(i)
  temp <- d[c(i,"w1_partnership_status")]
  temp2 <- temp %>%
    count(eval(as.name(i)),w1_partnership_status)
  colnames(temp2) <- c(i,"w1_partnership_status","n")
  temp3 <- subset(temp2,eval(as.name(i))==1)
  temp3[1] <- colnames(temp3)[1]
  colnames(temp3) <- c("source","target","n")
  temp3$source <- gsub("w1_q24_","",temp3$source)
  q24_methods <- c(q24_methods,temp3$source[1])
  temp3$sourceID <- listindex
  listed[[listindex]] <- temp3
  listindex <- listindex+1
}
w1_list <- rbindlist(listed)

max_q24 <- max(w1_list$sourceID)
w1_target_unique <- unique(w1_list$target)

w1_unique_values <- data.frame(
  target = w1_target_unique,
  targetID = seq(length(w1_target_unique))+max_q24
)
w1_list <- left_join(
  w1_list,
  w1_unique_values,
  by = "target"
)


# W1 Status -> W2 Status

# Categorizing their statuses during w2
d$w2_status <- "Missed" # Just a catch-all in case anyone gets missed in the case-when statement

d$w2_status <- dplyr::case_when(
  d$w2_surveyed==0 ~ "Not Surveyed",
  # Married
  d$w2_section==1 ~ "Still Married",
  d$w1_partnership_status=="Married" & d$w2_relationship_end==1  ~ "Married -> Divorced",
  d$w1_partnership_status=="Married" & d$w2_relationship_end==2  ~ "Married -> Separation or Other Breakup",
  d$w1_partnership_status=="Married" & d$w2_relationship_end==3 ~ "Married -> Partner Died",
  # Unmarried Partners
  d$w1_partnership_status=="Unmarried Partners" & d$w2_section==3 & d$w2_partner_type==1 ~ "Unmarried Partners -> Married", # unmarried, still with unmarried partner but reported marriage.
  d$w1_partnership_status=="Unmarried Partners" & d$w2_section==3 & d$w2_partner_type==2 ~ "Unmarried Partners -> Unmarried Partners", # unmarried, still with unmarried partner but reported unmarried partnership.
  d$w1_partnership_status=="Unmarried Partners" & d$w2_relationship_end==1 ~ "Unmarried Partners -> Divorced",
  d$w1_partnership_status=="Unmarried Partners" & d$w2_relationship_end==2 ~ "Unmarried Partners -> Separation or Other Breakup",
  d$w1_partnership_status=="Unmarried Partners" & d$w2_relationship_end==3 ~ "Unmarried Partners -> Partner Died",
  # Single but had partner in past
  d$w1_partnership_status=="Single (past)" & d$w2_relationship_end==1 ~ "Single (past) -> Divorced",
  d$w1_partnership_status=="Single (past)" & d$w2_relationship_end==2 ~ "Single (past) -> Separation or Other Breakup",
  d$w1_partnership_status=="Single (past)" & d$w2_relationship_end==3 ~ "Single (past) -> Partner Died",
  d$w1_partnership_status=="Single (past)" & d$w2_partner_type==1 ~ "Single (past) -> Married",
  d$w1_partnership_status=="Single (past)" & d$w2_partner_type==2 ~ "Single (past) -> Unmarried Partners",
  d$w1_partnership_status=="Single (past)" & d$w2_partner_type==3 ~ "Single (past) -> Single",
  # Never had a partner,
  d$w1_partnership_status=="Single" & d$w2_partner_type==1 ~ "Never in a relationship -> Married",
  d$w1_partnership_status=="Single" & d$w2_partner_type==2 ~ "Never in a relationship -> Unmarried Partners",
  d$w1_partnership_status=="Single" & d$w2_partner_type==4 ~ "Never in a relationship",
  # Mis-coded data: w1 had partner, w2_section said no longer married/partner  w/ w1
  # but w2_relationship end says no report of breakup.
  (d$w1_partnership_status=="Married" & d$w2_section==2 & d$w2_relationship_end==0) |
    (d$w1_partnership_status=="Unmarried Partners" & d$w2_section==4 & d$w2_relationship_end==0) ~ "Miscoded data",
  TRUE ~ d$w2_status
)

d$w2_status <- factor(d$w2_status, levels=c(
  "Still Married",
  "Married -> Divorced",
  "Married -> Separation or Other Breakup",
  "Married -> Partner Died",
  "Unmarried Partners -> Married",
  "Unmarried Partners -> Unmarried Partners",
  "Unmarried Partners -> Divorced",
  "Unmarried Partners -> Separation or Other Breakup",
  "Unmarried Partners -> Partner Died",
  "Single (past) -> Divorced",
  "Single (past) -> Separation or Other Breakup",
  "Single (past) -> Partner Died",
  "Single (past) -> Married",
  "Single (past) -> Unmarried Partners",
  "Single (past) -> Single",
  "Never in a relationship -> Married",
  "Never in a relationship -> Unmarried Partners",
  "Never in a relationship",
  "Not Surveyed",
  "Miscoded data",
  "Missed"
))

divorced <- levels(d$w2_status)[grep("-> divorced",tolower(levels(d$w2_status)))]
breakup <- levels(d$w2_status)[grep("-> separation",tolower(levels(d$w2_status)))]
married1 <- levels(d$w2_status)[grep("-> married",tolower(levels(d$w2_status)))]
married2 <- levels(d$w2_status)[grep("still married",tolower(levels(d$w2_status)))]
married <- c(married1,married2)
died <- levels(d$w2_status)[grep("died",tolower(levels(d$w2_status)))]
unmarriedpartners <- levels(d$w2_status)[grep("-> unmarried",tolower(levels(d$w2_status)))]
singlepast <- levels(d$w2_status)[grep("-> single",tolower(levels(d$w2_status)))]
singlenever <- "Never in a relationship"
single <- c(singlepast, singlenever)
notsurveyed <- "Not Surveyed"
miscoded <- "Miscoded data"

d$w2_status_cat <- dplyr::case_when(
  d$w2_status %in% divorced ~ "Divorced",
  d$w2_status %in% breakup ~ "Breakup",
  d$w2_status %in% married ~ "Married",
  d$w2_status %in% died ~ "Died",
  d$w2_status %in% unmarriedpartners ~ "Unmarried Partners",
  d$w2_status %in% single ~ "Single",
  d$w2_status %in% notsurveyed ~ "Not Surveyed",
  d$w2_status %in% miscoded ~ "Miscoded",
  TRUE~"Missed"
)

w2_stayed_together <- d[c("w1_partnership_status","w2_status_cat")]
w2_stayed_together_n <- w2_stayed_together %>% 
  count(w1_partnership_status,w2_status_cat)
colnames(w2_stayed_together_n) <- c("source","target","n")

unique_ordered_source <- unique(w1_list[order(w1_list$sourceID),c("source","sourceID")])
colnames(unique_ordered_source) <- c("name","number")
unique_ordered_target <- unique(w1_list[order(w1_list$targetID),c("target","targetID")])
colnames(unique_ordered_target) <- c("name","number")
names <- rbind(unique_ordered_source,unique_ordered_target)


w2_list <- dplyr::left_join(w2_stayed_together_n,
                            names,
                            by=join_by(source==name),
                            keep = FALSE)
colnames(w2_list)[grep("number",colnames(w2_list))] <- "sourceID"

maxid <- max(w2_list$sourceID)
w2_targets_unique <- unique(w2_list$target)
w2_unique_values <- data.frame(
  target = w2_targets_unique,
  targetID = seq(length(w2_targets_unique))+maxid
)
w2_list <- left_join(
  w2_list,
  w2_unique_values,
  by = "target"
)

# W2 status -> W3 status
d$w3_status_cat <- "Missed"
d$w3_status_cat <- dplyr::case_when(
  d$w3_surveyed==0 ~ "Not Surveyed",
  d$w3_partner_type==1 ~ "Married",
  d$w3_relationship_end_combo==1 ~ "Divorced",
  d$w3_relationship_end_combo==2 ~ "Breakup",
  d$w3_relationship_end_combo==3 ~ "Died",
  d$w3_partner_type==2 & 
    (d$w3_relationship_end_combo==1 | is.na(d$w3_relationship_end_combo)) ~ "Unmarried Partners",
  d$w3_partner_type==3 ~ "Single",
  TRUE ~ d$w3_status_cat
)

w3_stayed_together <- d[c("w2_status_cat","w3_status_cat")]
w3_stayed_together_n <- w3_stayed_together %>% 
  count(w2_status_cat,w3_status_cat)
colnames(w3_stayed_together_n) <- c("source","target","n")


unique_ordered_target_w2 <- unique(w2_list[order(w2_list$targetID),c("target","targetID")])
colnames(unique_ordered_target_w2) <- c("name","number")

w3_list <- dplyr::left_join(w3_stayed_together_n,
                            unique_ordered_target_w2,
                            by=join_by(source==name),
                            keep = FALSE)
colnames(w3_list)[grep("number",colnames(w3_list))] <- "sourceID"

maxid_2 <- max(w3_list$sourceID)
w3_targets_unique <- unique(w3_list$target)
w3_target_num_unique <- length(w3_targets_unique)

w3_unique_values <- data.frame(
  target = w3_targets_unique,
  targetID = seq(w3_target_num_unique)+maxid_2
)
w3_list <- left_join(
  w3_list,
  w3_unique_values,
  by = "target"
)

# setup for final Sankey
combined <- rbind(w1_list,w2_list,w3_list)

unique_ordered_source <- unique(combined[order(combined$sourceID),c("source","sourceID")])
colnames(unique_ordered_source) <- c("name","number")
unique_ordered_target <- unique(combined[order(combined$targetID),c("target","targetID")])
colnames(unique_ordered_target) <- c("name","number")
names <- rbind(unique_ordered_source,unique_ordered_target) %>% unique()

names$name <- dplyr::case_when(
  names$name=="mil"~"Military",
  names$name=="vol_org"~"Volunteer Organization",
  names$name=="bar_restaurant"~"Bar or Restaurant",
  names$name=="internet_other"~"Internet (Other)",
  names$name=="internet_dating"~"Internet Dating or Phone App",
  names$name=="internet_soc_network"~"Internet (Social Network)",
  names$name=="internet_game"~"Online Gaming",
  names$name=="internet_chat"~"Internet (Chat)",
  names$name=="internet_org"~"Internet Site Not Mainly Dedicated to Dating",
  names$name=="blind_date"~"Blind Date",
  names$name=="singles_serve_nonint"~"Non Internet Single Service",
  names$name=="business_trip"~"Business Trip",
  names$name=="work_neighbors"~"Work Neighbors",
  names$name=="met_online"~"Online",
  names$name=="met_through_family"~"Family",
  names$name=="met_through_friend"~"Friend",
  names$name=="met_through_as_nghbrs"~"Neighbors",
  names$name=="met_as_through_cowork"~"Coworkers",
  names$name=="metonline_no_phone_apps"~"Online Excluding Phone Apps",
  names$name=="church"~"Place of Worship",
  names$name=="never_dated"~"Never Dated",
  TRUE~str_to_title(names$name)
)

# Final Plot
plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(pad = 15,
              thickness = 20,
              line = list(color = "black", width = 0.5),
              label = names$name),
  link = list(source = combined$sourceID-1,
              target = combined$targetID-1,
              value = combined$n),
  textfont = list(size = 10),
  width = 720,
  height = 480
) %>%
  layout(title = paste0("How US Couples Meet and Whether They Stay Together \n n=",n),
         font = list(size = 20),
         margin = list(t = 50, l = 10, r = 10, b = -10),
         annotations = list(
           list(x = 0, y = -.1, text = "How Met", showarrow=FALSE, font = list(size = 16)),
           list(x = .35, y = -.1, text = "2017", showarrow=FALSE, font = list(size = 16)),
           list(x = .7, y = -.1, text = "2020", showarrow=FALSE, font = list(size = 16)),
           list(x = 1, y = -.1, text = "2022", showarrow=FALSE, font = list(size = 16))
         )
  )
