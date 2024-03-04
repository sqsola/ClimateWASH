
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