

# number of households by year
num_hh_year <- data %>% 
  group_by(hv007) %>%
  dplyr::summarise(
    "num_households" = n())

# Read in the rural and urban datasets
rural <- readRDS("~/data-mdavis65/steven_sola/4_HHlevel/rural.Rdata")
urban <- readRDS("~/data-mdavis65/steven_sola/4_HHlevel/urban.Rdata")

# counting the improved/unimproved across the three populations
table(rural$hv201_improved, useNA = "always")
table(urban$hv201_improved, useNA = "always")
table(data$hv201_improved, useNA = "always")


table(data$hv204)




# Set all numbers above 996 as Missings
data$hv204 <- if_else(data$hv204 >= 996, NA, data$hv204)
rural$hv204 <- if_else(rural$hv204 >= 996, NA, rural$hv204)
urban$hv204 <- if_else(urban$hv204 >= 996, NA, urban$hv204)

# 995 = one day or longer
# 996 = on premises

# Filter out all those missing, have on premise plumbing, or with 0 collection times
cat("There are", nrow(data) - nrow(data %>% filter(!is.na(hv204), hv204 != 0)), 
    "missing values, have on premise plumbing, or with 0 collection times")

cat("There are", nrow(rural) - nrow(rural %>% filter(!is.na(hv204), hv204 != 0)), 
    "missing values, have on premise plumbing, or with 0 collection times")

cat("There are", nrow(urban) - nrow(urban %>% filter(!is.na(hv204), hv204 != 0)), 
    "missing values, have on premise plumbing, or with 0 collection times")

data <-  data %>% filter(!is.na(hv204), hv204 != 0)
rural <-  rural %>% filter(!is.na(hv204), hv204 != 0)
urban <-  urban %>% filter(!is.na(hv204), hv204 != 0)

# Check to make sure all 996, 0, and missings have been filtered
skim(data$hv204)
skim(rural$hv204)
skim(urban$hv204)

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

