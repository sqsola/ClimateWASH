# Header ------------------------------------------------------------------

# Load Libraries
library(tidyverse)
library(readr)
library(readxl)
library(janitor)
library(flextable)
library(skimr)

# Set the location
#location <- "personal"
location <- "HPC"

if (location == "personal") {
  # Set Working Directory (Personal)
  setwd("C:/Users/Steven/OneDrive - Johns Hopkins/Dissertation/Dissertation/Data")
}

if (location == "HPC") { 
  # Set Working Directory (HPC)
  setwd("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH")
}

# Read in the full and rural datasets
data_aim1 <- readRDS("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 1/data_aim1.Rdata")
rural <- readRDS("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 1/rural.Rdata")
urban <- readRDS("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 1/urban.Rdata")

# Households Summary ------------------------------------------------------

# number of households by year
num_hh_year <- data_aim1 %>%
                 group_by(hv007) %>%
                 summarise(num_households = n()) %>%
                 mutate(Percentage = round((num_households / sum(num_households)) * 100, digits = 2)) %>%
                 adorn_totals("row") %>%
                 qflextable() %>% 
                 set_header_labels(hv007 = "Year", num_households = "Number of HHs")

# Improved vs. Unimproved -------------------------------------------------

# counting the improved/unimproved across the three populations
data_aim1 %>% tabyl(hv201_improved) %>% adorn_pct_formatting() %>% qflextable() %>% 
               set_header_labels(hv201_improved = "Type of Source", n = "Number of HHs") %>% 
               width(width = 1)
               
rural %>% tabyl(hv201_improved) %>% adorn_pct_formatting() %>% qflextable() %>% 
               set_header_labels(hv201_improved = "Type of Source", n = "Number of HHs") %>% 
               width(width = 1)

urban %>% tabyl(hv201_improved) %>% adorn_pct_formatting() %>% qflextable() %>% 
               set_header_labels(hv201_improved = "Type of Source", n = "Number of HHs") %>% 
               width(width = 1)
               
# Water Walk Times --------------------------------------------------------

# Datasets for the for loop
datasets <- list(full_data=data_aim1, rural=rural, urban=urban)

# 0 Water Collection Times
for (ii in names(datasets)) {
cat("There are", nrow(datasets[[ii]] %>% filter(hv204 == 0)),"Households with 0 collection times in", ii, "\n")
}

# Missing Water Collection Times
for (ii in names(datasets)) {
cat("There are", nrow(datasets[[ii]] %>% filter(is.na(hv204))),"Households with missing collection times in", ii, "\n")
}

# Filter out missings and those with 0 walk times,
# As well as 995, which indicates more than 1 day to water
data_aim1 <- data_aim1 %>% filter(!is.na(hv204), hv204 != 0, hv204 != 995)
rural <- rural %>% filter(!is.na(hv204), hv204 != 0, hv204 != 995)
urban <- urban %>% filter(!is.na(hv204), hv204 != 0, hv204 != 995)

# Check to make sure all 996, 0, and missings have been filtered
data_aim1 %>% skim(hv204) %>% qflextable()
skim(rural$hv204)
skim(urban$hv204)














tabyl(data_aim1$kgc_fine) %>% adorn_pct_formatting() %>% flextable::qflextable()
tabyl(data_aim1$kgc_course)











# Filter out those with piped water
rural_nopipe <- rural %>% filter(hv201_sourcecat != "Piped")
urban_nopipe <- urban %>% filter(hv201_sourcecat != "Piped")
data_nopipe <- data %>% filter(hv201_sourcecat != "Piped")

cor(rural_nopipe$hv204, rural_nopipe$tp_totalminus14, use = "pairwise.complete.obs")
cor(rural_nopipe$hv204, rural_nopipe$tp_totalminus30, use = "pairwise.complete.obs")
cor(rural_nopipe$hv204, rural_nopipe$tp_totalminus60, use = "pairwise.complete.obs")

cor(rural_nopipe$hv204, rural_nopipe$sro_totalminus14, use = "pairwise.complete.obs")
cor(rural_nopipe$hv204, rural_nopipe$sro_totalminus30, use = "pairwise.complete.obs")
cor(rural_nopipe$hv204, rural_nopipe$sro_totalminus60, use = "pairwise.complete.obs")

cor(urban_nopipe$hv204, urban_nopipe$tp_totalminus14, use = "pairwise.complete.obs")
cor(urban_nopipe$hv204, urban_nopipe$tp_totalminus30, use = "pairwise.complete.obs")
cor(urban_nopipe$hv204, urban_nopipe$tp_totalminus60, use = "pairwise.complete.obs")

cor(urban_nopipe$hv204, urban_nopipe$sro_totalminus14, use = "pairwise.complete.obs")
cor(urban_nopipe$hv204, urban_nopipe$sro_totalminus30, use = "pairwise.complete.obs")
cor(urban_nopipe$hv204, urban_nopipe$sro_totalminus60, use = "pairwise.complete.obs")

cor(data_nopipe$hv204, data_nopipe$tp_totalminus14, use = "pairwise.complete.obs")
cor(data_nopipe$hv204, data_nopipe$tp_totalminus30, use = "pairwise.complete.obs")
cor(data_nopipe$hv204, data_nopipe$tp_totalminus60, use = "pairwise.complete.obs")

cor(data_nopipe$hv204, data_nopipe$sro_totalminus14, use = "pairwise.complete.obs")
cor(data_nopipe$hv204, data_nopipe$sro_totalminus30, use = "pairwise.complete.obs")
cor(data_nopipe$hv204, data_nopipe$sro_totalminus60, use = "pairwise.complete.obs")











library(ggplot2)

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




library(ggplot2)

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

# Filter out people who have immediate access to water
rural_pipe <- rural %>% filter(!between(hv201, 11, 14)) %>% 
  filter(!hv201 >= 61)

piped_water <- rural %>% filter(between(hv201, 11, 14))
tanker <- rural %>% filter(hv201 >= 61)

table(rural_pipe$hv201)




# Count the number of PSUs
rural_final %>% group_by(hv001) %>% dplyr::summarise(num_psu = n()) %>% dplyr::summarise(sum_psu = max(hv001))




# Categorize the Climate Zones
rural_final <- rural_final %>% mutate(ClimateZ_2 = ifelse(ClimateZ == "As" | ClimateZ == "Aw", "Tropical", 
                                                          ifelse(ClimateZ == "BSh" | ClimateZ == "BWh", "Dry", "000")))


# number of breakdown for people fetching water.
table(rural_final$hv236)

# describe walk time
psych::describe(rural$hv204)
psych::describeBy(rural_final$hv204, rural_final$hv236)

# find number of households per hv001
test <- rural_final %>% group_by(hv001, hv000) %>% 
  dplyr::summarise(num_households = mean(n()))

mean(test$num_households)

# Table of Main results
rural_final %>% group_by(hv007) %>% 
  dplyr::summarise(mean_evap_60day_inch = mean(evap_overalltotal_inchday, na.rm = TRUE),
                   mean_precip_60day_inch = mean(precip_overalltotal_inchday),
                   mean_evap_30day_inch = mean(evap_minus1mototal_inchday, na.rm = TRUE),
                   mean_precip_30day_inch = mean(precip_minus1mototal_inchday),
                   mean_walk_minutes = mean(hv204),
                   num_households = n())



rural_final %>% group_by(hv007) %>% 
  dplyr::summarise(mean_evap_60day_inch = mean(evap_overalltotal_inchday, na.rm = TRUE),
                   mean_precip_60day_inch = mean(precip_overalltotal_inchday),
                   mean_evap_30day_inch = mean(evap_minus1mototal_inchday, na.rm = TRUE),
                   mean_precip_30day_inch = mean(precip_minus1mototal_inchday),
                   mean_walk_minutes = mean(hv204),
                   num_households = n())






test <- rural_final %>% group_by(hv007, monthinterview) %>% 
  dplyr::summarise(mean_evap_60day_inch = mean(evap_overalltotal_inchday, na.rm = TRUE),
                   mean_precip_60day_inch = mean(precip_overalltotal_inchday),
                   mean_evap_30day_inch = mean(evap_minus1mototal_inchday, na.rm = TRUE),
                   mean_precip_30day_inch = mean(precip_minus1mototal_inchday),
                   mean_walk_minutes = mean(hv204),
                   num_households = n())


write.csv(test, "test.csv")

# Categorize the Climate Zones and do chi sq on walk times in climate zones
climate_walk <- rural_final %>% select(hv204, ClimateZ, hhid)

climate_walk <- climate_walk %>% 
  mutate(ClimateZ_2 = ifelse(ClimateZ == "As" | ClimateZ == "Aw", "Tropical", 
                             ifelse(ClimateZ == "BSh" | ClimateZ == "BWh", "Dry", "000")))

table(climate_walk$ClimateZ, climate_walk$ClimateZ_2)

psych::describeBy(climate_walk$hv204, climate_walk$ClimateZ_2)

chisq <- climate_walk %>% filter(ClimateZ_2 != "000")

chisq.test(chisq$ClimateZ_2, chisq$hv204, simulate.p.value = TRUE)

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

# Special Dataset ||||||| CLEAN LATER
dry <- rural_final %>% filter(ClimateZ_2 == "Dry")

tropical <- rural_final %>% filter(ClimateZ_2 == "Tropical")

## correlation between walk and precipitation in preceding 1 month
cor(dry$hv204, dry$precip_minus1mototal_inchday, method = "kendall")
cor(tropical$hv204, tropical$precip_minus1mototal_inchday, method = "kendall")

## correlation between walk and evaporation in preceding 1 month
cor(dry$hv204, dry$evap_minus1mototal_inchday, method = "kendall", use = "complete.obs")
cor(tropical$hv204, tropical$evap_minus1mototal_inchday, method = "kendall", use = "complete.obs")

library(ggplot2)

plot_dry <- ggplot(dry, aes(x=hv204)) + 
  geom_line(aes(y = precip_minus1mototal_inchday), color = "steelblue") + 
  geom_line(aes(y = evap_minus1mototal_inchday), color="darkred") +
  xlab("Time to Collect Water") + ylab("Inches in the Previous Month") +
  scale_x_continuous(expand = c(0,0), limits = c(0,300)) +
  scale_y_continuous(expand = c(0,0), limits = c(-0.1,18)) +
  theme(axis.text = element_text (size = 14, face = "bold")) +
  theme(axis.title = element_text(size = 14, face = "bold")) 

plot_dry

plot_tropical <- ggplot(tropical, aes(x=hv204)) + 
  geom_line(aes(y = precip_minus1mototal_inchday), color = "steelblue") + 
  geom_line(aes(y = evap_minus1mototal_inchday), color="darkred") +
  xlab("Time to Collect Water") + ylab("Inches in the Previous Month") +
  scale_x_continuous(expand = c(0,0), limits = c(0,275)) +
  scale_y_continuous(expand = c(0,0), limits = c(-0.1,29)) +
  theme(axis.text = element_text (size = 14, face = "bold")) +
  theme(axis.title = element_text(size = 14, face = "bold")) 

plot_tropical



#### MODELING

model <- lmer(hv204 ~ precip_minus1mototal_inchday + (1 | ClimateZ), data=rural_final)
model
summary(model)

