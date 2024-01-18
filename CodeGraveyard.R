
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