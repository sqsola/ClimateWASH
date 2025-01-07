
# FOR LOOP ----------------------------------------------------------------
# For looping through all 184 countries in my project.
countries <- read_excel("Country_Index_HPC.xlsx")

for(entry in 1:nrow(countries)) {
  
  # Specify the files to work on
  country <- countries$country[[entry]]
  name_year <- countries$name_year[[entry]]
  
}


# PIPED WATER -------------------------------------------------------------

# Water Locations (HV201 for DHS7; coding changes throughout years)
data <- data %>% mutate(hv201_class = 
                          case_when(
                            dhs_phase >= 2 & dhs_phase <= 8 & hv201 == 10 ~ "Piped Water",
                            dhs_phase == 2 & dhs_phase >= 5 & dhs_phase <= 8 & hv201 == 11 ~ "Piped into residence",
                            dhs_phase == 2 & hv201 == 12 ~ "Public tap",
                            dhs_phase >= 5 & dhs_phase <= 8 & hv201 == 12 ~ "Piped to yard/plot",
                            dhs_phase >= 5 & dhs_phase <= 6 & hv201 == 13 ~ "Public tap/standpipe",
                            dhs_phase == 7 & hv201 == 13 ~ "Piped to neighbor",
                            dhs_phase == 7 & hv201 == 14 ~ "Public tap/standpipe",
                            dhs_phase >= 3 & dhs_phase <= 4 & hv201 >= 12 & hv201 <= 14 ~ "Other piped water",
                            dhs_phase >= 5 & dhs_phase <= 6 & hv201 >= 14 & hv201 <= 15 ~ "Other piped water",
                            dhs_phase == 2 | dhs_phase == 8 & hv201 == 14 ~ "Other piped water",
                            dhs_phase >= 2 & dhs_phase <= 8 & hv201 >= 15 & hv201 <= 19 ~ "Other piped water",
                            dhs_phase >= 2 & dhs_phase <= 4 & hv201 == 20 ~ "Well water",
                            dhs_phase >= 5 & dhs_phase <= 8 & hv201 == 20 ~ "Tube well water", 
                            dhs_phase == 2 & hv201 == 21 ~ "Well in residence",
                            dhs_phase >= 4 & dhs_phase <= 8 & hv201 == 21 ~ "Tube well or borehole", 
                            dhs_phase == 2 & hv201 == 22 ~ "Public well",
                            dhs_phase >= 2 & dhs_phase <= 3 & hv201 == 30 ~ "Surface water",
                            dhs_phase == 4 & hv201 == 30 ~ "Covered well/borehole",
                            dhs_phase >= 5 & dhs_phase <= 8 & hv201 == 30 ~ "Dug well (open/protected)",
                            dhs_phase == 2 & hv201 == 31 ~ "Spring",
                            dhs_phase >= 5 & dhs_phase <= 8 & hv201 == 31 ~ "Protected well",   
                            dhs_phase == 2 & hv201 == 32 ~ "River, stream",
                            dhs_phase >= 5 & dhs_phase <= 8 & hv201 == 32 ~ "Unprotected well", 
                            dhs_phase == 2 & hv201 == 33 ~ "Pond, lake",
                            dhs_phase == 2 & hv201 == 34 ~ "Dam",
                            dhs_phase >= 4 & dhs_phase <= 8 & hv201 == 40 ~ "Surface water",
                            dhs_phase >= 2 & dhs_phase <= 3 & hv201 == 41 ~ "Rainwater",
                            dhs_phase >= 4 & dhs_phase <= 8 & hv201 == 41 ~ "Protected spring", 
                            dhs_phase >= 4 & dhs_phase <= 8 & hv201 == 42 ~ "Unprotected spring",
                            dhs_phase >= 5 & dhs_phase <= 8 & hv201 == 43 ~ "River/dam/lake/pond/stream", 
                            dhs_phase == 4 & hv201 >= 43 & hv201 <= 49 ~ "Other surface water",
                            dhs_phase >= 4 & dhs_phase <= 8 & hv201 == 51 ~ "Rainwater",
                            dhs_phase >= 2 & dhs_phase <= 3 & hv201 == 51 ~ "Tanker truck",
                            dhs_phase >= 4 & dhs_phase <= 8 & hv201 == 61 ~ "Tanker truck",
                            dhs_phase >= 2 & dhs_phase <= 3 & hv201 == 61 ~ "Bottled water",
                            dhs_phase >= 5 & dhs_phase <= 8 & hv201 == 62 ~ "Cart with small tank",
                            dhs_phase >= 4 & dhs_phase <= 8 & hv201 == 71 ~ "Bottled water",
                            dhs_phase == 2 & hv201 == 71 ~ "Other",
                            dhs_phase >= 5 & dhs_phase <= 8 & hv201 >= 72 ~ "Other",  
                            dhs_phase == 4 & hv201 == 81 ~ "River/dam/lake/pond/stream",
                            dhs_phase >= 2 & dhs_phase <= 8 & hv201 == 99 ~ NA,
                            .default = "CHECK THIS OUT"
                          ))  











plot_rural <- ggplot(rural, aes(x=hv204)) + 
  geom_line(aes(y = tp_totalminus30), color = "steelblue") + 
  geom_line(aes(y = e_totalminus30), color="darkred") +
  xlab("Time to Collect Water") + ylab("Inches in the Previous Month") +
  scale_x_continuous(expand = c(0,0), limits = c(0,300)) +
  scale_y_continuous(expand = c(0,0), limits = c(-0.3,3)) +
  theme(axis.text = element_text (size = 14, face = "bold")) +
  theme(axis.title = element_text(size = 14, face = "bold")) 

plot_rural


rural$e_avgminus30




plot_dry <- ggplot(dry, aes(x=hv204)) + 
  geom_line(aes(y = precip_minus1mototal_inchday), color = "steelblue") + 
  geom_line(aes(y = evap_minus1mototal_inchday), color="darkred") +
  xlab("Time to Collect Water") + ylab("Inches in the Previous Month") +
  scale_x_continuous(expand = c(0,0), limits = c(0,300)) +
  scale_y_continuous(expand = c(0,0), limits = c(-0.1,18)) +
  theme(axis.text = element_text (size = 14, face = "bold")) +
  theme(axis.title = element_text(size = 14, face = "bold")) 

plot_dry







# See if the amount of piped water changes by the year of the survey
test <- rural_pipe_nona %>% 
  group_by(hv007) %>%
  dplyr::summarise(
    "num_households" = n(),
    across(piped, list(Yes = ~ sum(. == "Piped"),
                       No  = ~ sum(. == "Not Piped"))))
test$Percent_Piped <- (test$piped_Yes / test$num_households) * 100
test


# Count the number of PSUs
rural_final %>% group_by(hv001) %>% dplyr::summarise(num_psu = n()) %>% dplyr::summarise(sum_psu = max(hv001))



# Person fetching water
table(rural_final$hv236)

rural_final %>% group_by(hv236) %>% 
  dplyr::summarise(mean_evap_total = mean(evap_overalltotal_inchday),
                   mean_precip_total = mean(precip_overalltotal_inchday),
                   mean_precip_minus1 = mean(precip_minus1mototal_inchday),
                   mean_walk = mean(hv204),
                   num_households = n())

rural_final %>% filter(hv236 > 4 | is.na(hv236)) %>% 
  dplyr::summarise(mean_walk = mean(hv204),
                   num_households = n())




## FOR MAPPING

mapping <- rural_final %>% select(ID, LONGNUM, LATNUM)

mapping <- unique(mapping[c("LATNUM", "LONGNUM")])

mapping$ID <- NA

mapping$ID <- seq(1:nrow(mapping))

write.csv(mapping, "mappingforposter.csv")



# Attempting to fix CM_91 for Aim 2....
if (name_year == "CM_91") {
  
  # Trim the whitespace of the HHID and Case ID variables
  hh$hhid_clean <- trimws(hh$hhid)
  offspring$caseid_clean <- trimws(offspring$caseid)
  
  # Manually change a merged caseid number
  offspring <- offspring %>% 
    mutate(caseid_clean = ifelse(caseid_clean == "99132 1  4", "99 132 1 4", caseid_clean)) %>% 
    mutate(caseid_clean = ifelse(caseid_clean == "99132 1  2", "99 132 1 2", caseid_clean)) %>% 
    mutate(caseid_clean = ifelse(caseid_clean == "104 8727  2", "104 872 7  2", caseid_clean))%>% 
    mutate(caseid_clean = ifelse(caseid_clean == "104 8811  2", "104 881 1  2", caseid_clean))

  # Remove the numbers after the last space (twice)
  offspring$caseid_clean <- gsub(" [^ ]*$", "", offspring$caseid_clean) %>% trimws()
  # offspring$caseid_clean <- gsub(" [^ ]*$", "", offspring$caseid_clean) %>% trimws()
  
  # Separate out 44 and 73 households that were merged
  hh <- hh %>% 
    mutate(hhid_clean = ifelse(hhid_clean == "4440010", "44 400 1", hhid_clean)) %>% 
    mutate(hhid_clean = ifelse(hhid_clean == "7312610", "73 126 1", hhid_clean)) %>% 
    mutate(hhid_clean = ifelse(hhid_clean == "104 8613", "104 86 1", hhid_clean)) %>% 
    mutate(hhid_clean = ifelse(hhid_clean == "104 8639", "104 86 3", hhid_clean)) %>% 
    mutate(hhid_clean = ifelse(hhid_clean == "104 8715", "104 87 1", hhid_clean)) %>% 
    mutate(hhid_clean = ifelse(hhid_clean == "104 8727", "104 87 2", hhid_clean)) %>% 
    mutate(hhid_clean = ifelse(hhid_clean == "104 8811", "104 88 1", hhid_clean)) %>% 
    mutate(hhid_clean = ifelse(hhid_clean == "46 1812", "46 18 1", hhid_clean)) %>% 
    mutate(hhid_clean = ifelse(hhid_clean == "47  113", "47 11 3", hhid_clean)) %>% 
    mutate(hhid_clean = ifelse(hhid_clean == "80 4010", "80 40 1", hhid_clean)) %>% 
    mutate(hhid_clean = ifelse(hhid_clean == "88 5611", "88 561 1", hhid_clean)) %>% 
    mutate(hhid_clean = ifelse(hhid_clean == "113 3030", "113 30 3", hhid_clean)) %>% 
    mutate(hhid_clean = ifelse(hhid_clean == "122 6711", "122 67 1", hhid_clean)) %>% 
    mutate(hhid_clean = ifelse(hhid_clean == "142 4412", "142 44 1", hhid_clean)) 
  
  # Remove the numbers after the last space
  # hh$hhid_clean <- gsub(" [^ ]*$", "", hh$hhid_clean) %>% trimws()
  
  # Detect the number of digits in a row,
  # If 4 digits in a row, add a space after the first digit
  # If 5 digits in a row, add a space after the second digit
  # If 6 digits in a row, add a space after the third digit
  # If doesn't meet any conditions, original HHID
  
  hh <- hh %>% 
    mutate(
      hhid_clean = case_when(
        str_detect(hhid_clean, "^\\d{4,4}$") ~ gsub(hhid_clean, pattern = "(.{1})(.*)", replacement = "\\1 \\2"),
        str_detect(hhid_clean, "^\\d{5,5}$") ~ gsub(hhid_clean, pattern = "(.{2})(.*)", replacement = "\\1 \\2"),
        str_detect(hhid_clean, "^\\d{6,6}$") ~ gsub(hhid_clean, pattern = "(.{3})(.*)", replacement = "\\1 \\2"),
        TRUE ~ hhid_clean
      )
    )
  
  hh <- hh %>% 
    separate(hhid_clean, into = c("hhid_clean", "hv002"), sep = "\\s+") %>% 
    relocate(hv002, .after = hv001) %>%
    select(-hhid_clean)
  
  hh$hv002 <- as.numeric(hh$hv002)
  
  class(hh$hv002)
}





# Full Bar Chart with All and Rural Households Line Chart
full_graph + geom_line_hh + geom_point_hh_outline + geom_point_hh + 
  geom_line_rural + geom_point_rural_outline + geom_point_rural +
  scale_color_manual(name = "Household Type",
                     values = c("All Households" = "#2c7bb6", "Rural Households" = "#d7191c"),
                     labels = c("All Households" = "All Households", "Rural Households" = "Rural Households")) +
  scale_y_continuous(sec.axis = sec_axis(~./2000, name = "Average Walk Time"), expand = c(0, 0))

# Full Bar Chart with All, Rural, and Urban Households Line Chart
full_graph + geom_line_hh + geom_point_hh_outline + geom_point_hh + 
  geom_line_rural + geom_point_rural_outline + geom_point_rural + 
  geom_line_urban + geom_point_urban_outline + geom_point_urban +
  scale_color_manual(name = "Household Type",
                     values = c("All Households" = "#2c7bb6", "Rural Households" = "#d7191c", "Urban Households" = "#fdae61"),
                     labels = c("All Households" = "All Households", "Rural Households" = "Rural Households", "Urban Households" = "Urban Households"))+
  scale_y_continuous(sec.axis = sec_axis(~./2000, name = "Average Walk Time"), expand = c(0, 0))

# Full Bar Chart with All Households Line Chart
full_graph + geom_line_hh + geom_point_hh_outline + geom_point_hh +
  scale_color_manual(name = "Household Type",
                     values = c("All Households" = "#2c7bb6"),
                     labels = c("All Households" = "All Households")) +
  scale_y_continuous(sec.axis = sec_axis(~./2000, name = "Average Walk Time"), expand = c(0, 0))







# Rural dataset, HV270 (filtered to match HV 270a)
rural %>% filter(hv007 %in% c(2015:2022)) %>%
  ggplot(mapping = aes(x = factor(hv007), y = log(hv204), color = factor(hv270))) +
  geom_boxplot() +  
  scale_y_continuous(expand = c(0, 0))+
  xlab("Year") + ylab("Log (Water Walk Time)") + 
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, face = "bold")) +
  ggtitle("Rural dataset and HV270 (Combined Wealth)")+
  theme(plot.title = element_text(hjust = 0.5))

# Urban dataset, HV270
urban %>% ggplot(mapping = aes(x = factor(hv007), y = log(hv204), color = factor(hv270))) +
  geom_boxplot() +  
  scale_y_continuous(expand = c(0, 0))+
  xlab("Year") + ylab("Log (Water Walk Time)") + 
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, face = "bold")) +
  ggtitle("Urban dataset and HV270 (Combined Wealth)")+
  theme(plot.title = element_text(hjust = 0.5))

# Urban dataset, HV270 (filtered to match HV 270a)
urban %>% filter(hv007 %in% c(2015:2022)) %>%
  ggplot(mapping = aes(x = factor(hv007), y = log(hv204), color = factor(hv270))) +
  geom_boxplot() +  
  scale_y_continuous(expand = c(0, 0))+
  xlab("Year") + ylab("Log (Water Walk Time)") + 
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, face = "bold")) +
  ggtitle("Urban dataset and HV270 (Combined Wealth)")+
  theme(plot.title = element_text(hjust = 0.5))

# Rural dataset, HV270a
rural %>% filter(hv007 %in% c(2015:2022)) %>%
  ggplot(mapping = aes(x = factor(hv007), y = log(hv204), color = factor(hv270a))) +
  geom_boxplot() +  
  scale_y_continuous(expand = c(0, 0))+
  xlab("Year") + ylab("Log (Water Walk Time)") + 
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, face = "bold"))+
  ggtitle("Rural dataset and HV270a (Separate Wealth)")+
  theme(plot.title = element_text(hjust = 0.5))

# Urban dataset, HV270a
urban %>% filter(hv007 %in% c(2015:2022)) %>%
  ggplot(mapping = aes(x = factor(hv007), y = log(hv204), color = factor(hv270a))) +
  geom_boxplot() +  
  scale_y_continuous(expand = c(0, 0))+
  xlab("Year") + ylab("Log (Water Walk Time)") + 
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, face = "bold"))+
  ggtitle("Urban dataset and HV270a (Separate Wealth)")+
  theme(plot.title = element_text(hjust = 0.5))



rural_final %>%
  group_by(hv007) %>% 
  summarise(num_children = n(),
            exposed = sum(animal_cattle_present),
            percent_exp = round((exposed/num_children)*100, digits = 1), 
            dia_yes = sum(diarrhea_cattle == 1, na.rm = TRUE),
            dia_no = sum(diarrhea_cattle == 0, na.rm = TRUE),
            percent_dia = round((dia_yes/dia_no)*100, digits = 1)) %>% 
  qflextable() %>% 
  set_header_labels(hv007 = "Year",
                    num_children = "Number of Children",
                    exposed = "Number Exposed",
                    percent_exp = "Exposed %",
                    dia_yes = "Diarrhea",
                    dia_no = "No Diarrhea",
                    percent_dia = "Diarrhea %") %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)%>%
  colformat_num(j = "hv007", big.mark = "")






under5_animal <- under5_animal %>%
  mutate(diarrhea_cattle = case_when(bull_cow_cattle_present == 0 ~ NA_integer_,
                                     bull_cow_cattle_present == 1 & diarrhea_dichot == 1 ~ 1, 
                                     TRUE ~ 0)) %>% 
  mutate(diarrhea_chicken = case_when(chicken_poultry_duck_present == 0 ~ NA_integer_,
                                      chicken_poultry_duck_present == 1 & diarrhea_dichot == 1 ~ 1, 
                                      TRUE ~ 0)) %>% 
  mutate(diarrhea_goat = case_when(goat_sheep_present == 0 ~ NA_integer_,
                                   goat_sheep_present == 1 & diarrhea_dichot == 1 ~ 1, 
                                   TRUE ~ 0)) %>% 
  mutate(diarrhea_horse = case_when(horse_donkey_camel_present == 0 ~ NA_integer_,
                                    horse_donkey_camel_present == 1 & diarrhea_dichot == 1 ~ 1, 
                                    TRUE ~ 0)) %>% 
  mutate(diarrhea_pig = case_when(pig_present == 0 ~ NA_integer_,
                                  pig_present == 1 & diarrhea_dichot == 1 ~ 1, 
                                  TRUE ~ 0)) %>% 
  mutate(diarrhea_other = case_when(other_present == 0 ~ NA_integer_,
                                    other_present == 1 & diarrhea_dichot == 1 ~ 1, 
                                    TRUE ~ 0))



logit_cattle <- glm(diarrhea_dichot ~ bull_cow_cattle_present, data = under5_animal, family = "binomial")
exp(cbind(OR = coef(logit_cattle), confint(logit_cattle)))



rural %>%
  summarise(num_children = n(),
            num_cattle = sum(bull_cow_cattle_present),
            dia_yes = sum(diarrhea_cattle == 1, na.rm = TRUE),
            dia_no = sum(diarrhea_cattle == 0, na.rm = TRUE),
            percent_dia = round((dia_yes/dia_no)*100, digits = 1),
            or = paste0("0.92 (0.91, 0.94)"))%>% 
  qflextable() %>% 
  set_header_labels(num_children = "Number of Children",
                    num_cattle = "Exposed to Cattle",
                    dia_yes = "Diarrhea",
                    dia_no = "No Diarrhea",
                    percent_dia = "% Diarrhea",
                    or = "OR (95% CI)") %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

logit_cow <- glm(diarrhea_dichot ~ animal_cow_present, data = rural_final, family = "binomial")
exp(cbind(OR = coef(logit_cow), confint(logit_cow)))

rural_final %>%
  summarise(num_children = n(),
            num_cattle = sum(animal_cow_present),
            dia_yes = sum(diarrhea_cow == 1, na.rm = TRUE),
            dia_no = sum(diarrhea_cow == 0, na.rm = TRUE),
            percent_dia = round((dia_yes/dia_no)*100, digits = 1),
            or = paste0("0.93 (0.91, 0.95)"))%>% 
  qflextable() %>% 
  set_header_labels(num_children = "Number of Children",
                    num_cattle = "Exposed to Cows",
                    dia_yes = "Diarrhea",
                    dia_no = "No Diarrhea",
                    percent_dia = "% Diarrhea",
                    or = "OR (95% CI)") %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

logit_horse <- glm(diarrhea_dichot ~ animal_horse_present, data = rural_final, family = "binomial")
exp(cbind(OR = coef(logit_horse), confint(logit_horse)))

rural_final %>%
  summarise(num_children = n(),
            num_cattle = sum(animal_horse_present),
            dia_yes = sum(diarrhea_horse == 1, na.rm = TRUE),
            dia_no = sum(diarrhea_horse == 0, na.rm = TRUE),
            percent_dia = round((dia_yes/dia_no)*100, digits = 1),
            or = paste0("0.97 (0.95, 0.99)"))%>% 
  qflextable() %>% 
  set_header_labels(num_children = "Number of Children",
                    num_cattle = "Exposed to Horse",
                    dia_yes = "Diarrhea",
                    dia_no = "No Diarrhea",
                    percent_dia = "% Diarrhea",
                    or = "OR (95% CI)") %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

logit_goat <- glm(diarrhea_dichot ~ animal_goat_present, data = rural_final, family = "binomial")
exp(cbind(OR = coef(logit_goat), confint(logit_goat)))

rural_final %>%
  summarise(num_children = n(),
            num_cattle = sum(animal_goat_present),
            dia_yes = sum(diarrhea_goat == 1, na.rm = TRUE),
            dia_no = sum(diarrhea_goat == 0, na.rm = TRUE),
            percent_dia = round((dia_yes/dia_no)*100, digits = 1),
            or = paste0("1.02 (1.00, 1.04)"))%>% 
  qflextable() %>% 
  set_header_labels(num_children = "Number of Children",
                    num_cattle = "Exposed to Goat",
                    dia_yes = "Diarrhea",
                    dia_no = "No Diarrhea",
                    percent_dia = "% Diarrhea",
                    or = "OR (95% CI)") %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

logit_sheep <- glm(diarrhea_dichot ~ animal_sheep_present, data = rural_final, family = "binomial")
exp(cbind(OR = coef(logit_sheep), confint(logit_sheep)))

rural_final %>%
  summarise(num_children = n(),
            num_cattle = sum(animal_sheep_present),
            dia_yes = sum(diarrhea_sheep == 1, na.rm = TRUE),
            dia_no = sum(diarrhea_sheep == 0, na.rm = TRUE),
            percent_dia = round((dia_yes/dia_no)*100, digits = 1),
            or = paste0("0.96 (0.94, 0.98)"))%>% 
  qflextable() %>% 
  set_header_labels(num_children = "Number of Children",
                    num_cattle = "Exposed to Sheep",
                    dia_yes = "Diarrhea",
                    dia_no = "No Diarrhea",
                    percent_dia = "% Diarrhea",
                    or = "OR (95% CI)") %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

logit_chicken <- glm(diarrhea_dichot ~ animal_chicken_present, data = rural_final, family = "binomial")
exp(cbind(OR = coef(logit_chicken), confint(logit_chicken)))

rural_final %>%
  summarise(num_children = n(),
            num_cattle = sum(animal_chicken_present),
            dia_yes = sum(diarrhea_chicken == 1, na.rm = TRUE),
            dia_no = sum(diarrhea_chicken == 0, na.rm = TRUE),
            percent_dia = round((dia_yes/dia_no)*100, digits = 1),
            or = paste0("0.95 (0.93, 0.97)"))%>% 
  qflextable() %>% 
  set_header_labels(num_children = "Number of Children",
                    num_cattle = "Exposed to Chicken",
                    dia_yes = "Diarrhea",
                    dia_no = "No Diarrhea",
                    percent_dia = "% Diarrhea",
                    or = "OR (95% CI)") %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)




tbl_stack_ex1 <- tbl_stack(list(cattle, cow, horse, goat, sheep, chicken))







# EPE_7
under5_animal %>% 
  group_by(chicken_poultry_duck_present, diarrhea_dichot) %>% 
  summarise(n = n(),
            epe_7_95_0 = sum(epe_7_95 == 0, na.rm = TRUE),
            percent_0 = round((epe_7_95_0/n)*100, digits = 2),
            epe_7_95_1 = sum(epe_7_95 == 1, na.rm = TRUE),
            percent_1 = round((epe_7_95_1/n)*100, digits = 2),
            epe_7_95_2 = sum(epe_7_95 == 2, na.rm = TRUE),
            percent_2 = round((epe_7_95_2/n)*100, digits = 2),
            epe_7_95_3 = sum(epe_7_95 == 3, na.rm = TRUE),
            percent_3 = round((epe_7_95_3/n)*100, digits = 2)) %>%
  mutate(diarrhea_dichot = recode(diarrhea_dichot, "0" = "No",
                                  "1" = "Yes",
                                  .missing = "Unknown"),
         chicken_poultry_duck_present = recode(chicken_poultry_duck_present,
                                               "0" = "No",
                                               "1" = "Yes")) %>% 
  arrange(desc(diarrhea_dichot)) %>% 
  adorn_totals("row",,,,c(n, epe_7_95_0, epe_7_95_1, epe_7_95_2, epe_7_95_3)) %>% 
  qflextable() %>% 
  set_header_labels(chicken_poultry_duck_present = "Chicken/Duck/Poultry?",
                    n = "Number Under 5",  diarrhea_dichot = "Diarrhea?",
                    epe_7_95_0 = "0 EPEs", percent_0 = "%",
                    percent_1 = "%", percent_2 = "%",
                    percent_3 = "%",
                    epe_7_95_1 = "1 EPEs",epe_7_95_2 = "2 EPEs",epe_7_95_3 = "3 EPEs")%>% 
  add_header_lines(values = c("Total Precipitation, Past 7 Days")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)




# Vaccinations ------------------------------------------------------------

# Rotavirus
under5_animal %>% 
  group_by(diarrhea_dichot) %>% 
  summarise(n = n(),
            zero = sum(h57 == 0, na.rm = TRUE),
            percent_0 = round((zero/n)*100, digits = 2),
            one = sum(h57 == 1, na.rm = TRUE),
            percent_1 = round((one/n)*100, digits = 2),
            two = sum(h57 == 2, na.rm = TRUE),
            percent_2 = round((two/n)*100, digits = 2),
            three = sum(h57 == 3, na.rm = TRUE),
            percent_3 = round((three/n)*100, digits = 2),
            eight = sum(h57 == 8, na.rm = TRUE),
            percent_8 = round((eight/n)*100, digits = 2)) %>%
  mutate(diarrhea_dichot = recode(diarrhea_dichot, "0" = "No",
                                  "1" = "Yes",
                                  .missing = "Unknown")) %>% 
  arrange(desc(diarrhea_dichot)) %>% 
  adorn_totals("row",,,,c(n, zero, one, two, three, eight)) %>% 
  qflextable() %>% 
  set_header_labels(n = "Number Under 5",  diarrhea_dichot = "Diarrhea?",
                    zero = "None", percent_0 = "%",
                    one = "Vax Date", percent_1 = "%",
                    two = "Mom Report", percent_2 = "%",
                    three = "Vax Card", percent_3 = "%",
                    eight = "Don't Know", percent_8 = "%")%>% 
  add_header_lines(values = c("Rotavirus Vaccination")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

under5_animal %>% 
  group_by(diarrhea_dichot) %>% 
  summarise(n = n(),
            zero = sum(h61 == 0, na.rm = TRUE),
            percent_0 = round((zero/n)*100, digits = 2),
            one = sum(h61 == 1, na.rm = TRUE),
            percent_1 = round((one/n)*100, digits = 2),
            two = sum(h61 == 2, na.rm = TRUE),
            percent_2 = round((two/n)*100, digits = 2),
            three = sum(h61 == 3, na.rm = TRUE),
            percent_3 = round((three/n)*100, digits = 2),
            eight = sum(h61 == 8, na.rm = TRUE),
            percent_8 = round((eight/n)*100, digits = 2)) %>%
  mutate(diarrhea_dichot = recode(diarrhea_dichot, "0" = "No",
                                  "1" = "Yes",
                                  .missing = "Unknown")) %>% 
  arrange(desc(diarrhea_dichot)) %>% 
  adorn_totals("row",,,,c(n, zero, one, two, three, eight)) %>% 
  qflextable() %>% 
  set_header_labels(n = "Number Under 5",  diarrhea_dichot = "Diarrhea?",
                    zero = "None", percent_0 = "%",
                    one = "Vax Date", percent_1 = "%",
                    two = "Mom Report", percent_2 = "%",
                    three = "Vax Card", percent_3 = "%",
                    eight = "Don't Know", percent_8 = "%")%>% 
  add_header_lines(values = c("Hep B Vaccination")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)


under5_animal %>% 
  group_by(diarrhea_dichot) %>% 
  summarise(n = n(),
            zero = sum(h51 == 0, na.rm = TRUE),
            percent_0 = round((zero/n)*100, digits = 2),
            one = sum(h51 == 1, na.rm = TRUE),
            percent_1 = round((one/n)*100, digits = 2),
            two = sum(h51 == 2, na.rm = TRUE),
            percent_2 = round((two/n)*100, digits = 2),
            three = sum(h51 == 3, na.rm = TRUE),
            percent_3 = round((three/n)*100, digits = 2),
            eight = sum(h51 == 8, na.rm = TRUE),
            percent_8 = round((eight/n)*100, digits = 2)) %>%
  mutate(diarrhea_dichot = recode(diarrhea_dichot, "0" = "No",
                                  "1" = "Yes",
                                  .missing = "Unknown")) %>% 
  arrange(desc(diarrhea_dichot)) %>% 
  adorn_totals("row",,,,c(n, zero, one, two, three, eight)) %>% 
  qflextable() %>% 
  set_header_labels(n = "Number Under 5",  diarrhea_dichot = "Diarrhea?",
                    zero = "None", percent_0 = "%",
                    one = "Vax Date", percent_1 = "%",
                    two = "Mom Report", percent_2 = "%",
                    three = "Vax Card", percent_3 = "%",
                    eight = "Don't Know", percent_8 = "%")%>% 
  add_header_lines(values = c("DPT-HepB-Hib Pentavalent Vaccination")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)



# Quartiles ---------------------------------------------------------------


person_level_filter <- rural %>% select(hv246_chicken_poultry_duck_total_cat,
                                        hv246_bull_cow_cattle_total_cat,
                                        hv246_goat_sheep_total_cat,
                                        hv246_horse_donkey_camel_total_cat, b8) %>% 
  filter(b8 <= 4)

# Calculate quantiles for hv246_chicken_poultry_duck_total_cat
chicken_quantiles <- person_level_filter %>% 
  filter(hv246_chicken_poultry_duck_total_cat >= 1) %>% 
  summarise(
    q25 = quantile(hv246_chicken_poultry_duck_total_cat, probs = 0.25),
    q50 = quantile(hv246_chicken_poultry_duck_total_cat, probs = 0.5),
    q75 = quantile(hv246_chicken_poultry_duck_total_cat, probs = 0.75),
    q90 = quantile(hv246_chicken_poultry_duck_total_cat, probs = 0.90),
    q95 = quantile(hv246_chicken_poultry_duck_total_cat, probs = 0.95)
  ) %>%
  mutate(category = "Chicken/Poultry/Duck")

# Calculate quantiles for hv246_bull_cow_cattle_total_cat
cattle_quantiles <- person_level_filter %>% 
  filter(hv246_bull_cow_cattle_total_cat >= 1) %>% 
  summarise(
    q25 = quantile(hv246_bull_cow_cattle_total_cat, probs = 0.25),
    q50 = quantile(hv246_bull_cow_cattle_total_cat, probs = 0.5),
    q75 = quantile(hv246_bull_cow_cattle_total_cat, probs = 0.75),
    q90 = quantile(hv246_bull_cow_cattle_total_cat, probs = 0.90),
    q95 = quantile(hv246_bull_cow_cattle_total_cat, probs = 0.95)
  ) %>%
  mutate(category = "Bull/Cow/Cattle")

# Calculate quantiles for hv246_bull_cow_cattle_total_cat
goat_sheep_quantiles <- person_level_filter %>% 
  filter(hv246_goat_sheep_total_cat >= 1) %>% 
  summarise(
    q25 = quantile(hv246_goat_sheep_total_cat, probs = 0.25),
    q50 = quantile(hv246_goat_sheep_total_cat, probs = 0.5),
    q75 = quantile(hv246_goat_sheep_total_cat, probs = 0.75),
    q90 = quantile(hv246_goat_sheep_total_cat, probs = 0.90),
    q95 = quantile(hv246_goat_sheep_total_cat, probs = 0.95)
  ) %>%
  mutate(category = "Goat/Sheep")


# Calculate quantiles for hv246_bull_cow_cattle_total_cat
horse_donkey_quantiles <- person_level_filter %>% 
  filter(hv246_horse_donkey_camel_total_cat >= 1) %>% 
  summarise(
    q25 = quantile(hv246_horse_donkey_camel_total_cat, probs = 0.25),
    q50 = quantile(hv246_horse_donkey_camel_total_cat, probs = 0.5),
    q75 = quantile(hv246_horse_donkey_camel_total_cat, probs = 0.75),
    q90 = quantile(hv246_horse_donkey_camel_total_cat, probs = 0.90),
    q95 = quantile(hv246_horse_donkey_camel_total_cat, probs = 0.95)
  ) %>%
  mutate(category = "Horse/Donkey/Camel")

# Combine the tables
bind_rows(chicken_quantiles, cattle_quantiles, goat_sheep_quantiles, horse_donkey_quantiles) %>% 
  select(category, everything()) %>% 
  qflextable() %>% 
  set_header_labels(category = "Animal Category")%>% 
  add_header_lines(values = c("Percentiles of Animal Ownership (Person Level)
                              n = 904,958*")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE) %>% 
  add_footer_lines("*Excludes people older than 5 and children that aren't exposed to each category")


hh_level_filter <- rural_hh %>% ungroup() %>% select(hv246_chicken_poultry_duck_total_cat,
                                                     hv246_bull_cow_cattle_total_cat,
                                                     hv246_goat_sheep_total_cat,
                                                     hv246_horse_donkey_camel_total_cat, hh_under5) %>% 
  filter(hh_under5 == 1)


# Calculate quantiles for hv246_chicken_poultry_duck_total_cat
chicken_quantiles <- hh_level_filter %>% 
  filter(hv246_chicken_poultry_duck_total_cat >= 1) %>% 
  summarise(
    q25 = quantile(hv246_chicken_poultry_duck_total_cat, probs = 0.25),
    q50 = quantile(hv246_chicken_poultry_duck_total_cat, probs = 0.5),
    q75 = quantile(hv246_chicken_poultry_duck_total_cat, probs = 0.75),
    q90 = quantile(hv246_chicken_poultry_duck_total_cat, probs = 0.90),
    q95 = quantile(hv246_chicken_poultry_duck_total_cat, probs = 0.95)
  ) %>%
  mutate(category = "Chicken/Poultry/Duck")

# Calculate quantiles for hv246_bull_cow_cattle_total_cat
cattle_quantiles <- hh_level_filter %>% 
  filter(hv246_bull_cow_cattle_total_cat >= 1) %>% 
  summarise(
    q25 = quantile(hv246_bull_cow_cattle_total_cat, probs = 0.25),
    q50 = quantile(hv246_bull_cow_cattle_total_cat, probs = 0.5),
    q75 = quantile(hv246_bull_cow_cattle_total_cat, probs = 0.75),
    q90 = quantile(hv246_bull_cow_cattle_total_cat, probs = 0.90),
    q95 = quantile(hv246_bull_cow_cattle_total_cat, probs = 0.95)
  ) %>%
  mutate(category = "Bull/Cow/Cattle")

# Calculate quantiles for hv246_bull_cow_cattle_total_cat
goat_sheep_quantiles <- hh_level_filter %>% 
  filter(hv246_goat_sheep_total_cat >= 1) %>% 
  summarise(
    q25 = quantile(hv246_goat_sheep_total_cat, probs = 0.25),
    q50 = quantile(hv246_goat_sheep_total_cat, probs = 0.5),
    q75 = quantile(hv246_goat_sheep_total_cat, probs = 0.75),
    q90 = quantile(hv246_goat_sheep_total_cat, probs = 0.90),
    q95 = quantile(hv246_goat_sheep_total_cat, probs = 0.95)
  ) %>%
  mutate(category = "Goat/Sheep")


# Calculate quantiles for hv246_bull_cow_cattle_total_cat
horse_donkey_quantiles <- hh_level_filter %>% 
  filter(hv246_horse_donkey_camel_total_cat >= 1) %>% 
  summarise(
    q25 = quantile(hv246_horse_donkey_camel_total_cat, probs = 0.25),
    q50 = quantile(hv246_horse_donkey_camel_total_cat, probs = 0.5),
    q75 = quantile(hv246_horse_donkey_camel_total_cat, probs = 0.75),
    q90 = quantile(hv246_horse_donkey_camel_total_cat, probs = 0.90),
    q95 = quantile(hv246_horse_donkey_camel_total_cat, probs = 0.95)
  ) %>%
  mutate(category = "Horse/Donkey/Camel")

# Combine the tables
bind_rows(chicken_quantiles, cattle_quantiles, goat_sheep_quantiles, horse_donkey_quantiles) %>% 
  select(category, everything()) %>% 
  qflextable() %>% 
  set_header_labels(category = "Animal Category")%>% 
  add_header_lines(values = c("Percentiles of Animal Ownership (Household Level)
                              n = 504,036*")) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE) %>% 
  add_footer_lines("*Excludes households that don't have a child under 5 and each category excludes households that don't own an animal in the category")

