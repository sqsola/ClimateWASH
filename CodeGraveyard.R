
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