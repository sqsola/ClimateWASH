# Header ------------------------------------------------------------------

# Load Libraries
library(readr)
library(readxl)
library(janitor)
library(flextable)
library(skimr)
library(corrr)
library(corrplot)
library(lmerTest)
library(multilevelTools)
library(tidyverse)

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
data_aim1 %>%
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

# Summary tables for Water walk times
data_aim1 %>% skim(hv204) %>% qflextable()%>% 
  width(width = 0.8)
rural %>% skim(hv204) %>% qflextable() %>% 
  width(width = 0.8)
urban %>% skim(hv204) %>% qflextable() %>% 
  width(width = 0.8)


# HH with GPS -------------------------------------------------------------
for (ii in names(datasets)) {
  cat("There are", nrow(datasets[[ii]] %>% filter(!is.na(LATNUM))),"Households with GPS Coords", ii, "\n")
}

# Koppen-Geiger Zones -----------------------------------------------------

data_aim1 %>% tabyl(kgc_fine) %>% adorn_pct_formatting() %>%
  adorn_totals("row") %>%
  qflextable() %>% 
  set_header_labels(kgc_fine = "KG Zone", n = "Number of HHs",
                    percent = "Total Percent", valid_percent = "Percent among Non-Missing")

data_aim1 %>% tabyl(kgc_course) %>% adorn_pct_formatting() %>%
  adorn_totals("row") %>%
  qflextable() %>% 
  set_header_labels(kgc_course = "KG Zone", n = "Number of HHs",
                    percent = "Total Percent", valid_percent = "Percent among Non-Missing")

rural %>% tabyl(kgc_fine) %>% adorn_pct_formatting() %>%
  adorn_totals("row") %>%
  qflextable() %>% 
  set_header_labels(kgc_fine = "KG Zone", n = "Number of HHs",
                    percent = "Total Percent", valid_percent = "Percent among Non-Missing")

rural %>% tabyl(kgc_course) %>% adorn_pct_formatting() %>%
  adorn_totals("row") %>%
  qflextable() %>% 
  set_header_labels(kgc_course = "KG Zone", n = "Number of HHs",
                    percent = "Total Percent", valid_percent = "Percent among Non-Missing")

urban %>% tabyl(kgc_fine) %>% adorn_pct_formatting() %>%
  adorn_totals("row") %>%
  qflextable() %>% 
  set_header_labels(kgc_fine = "KG Zone", n = "Number of HHs",
                    percent = "Total Percent", valid_percent = "Percent among Non-Missing")

urban %>% tabyl(kgc_course) %>% adorn_pct_formatting() %>%
  adorn_totals("row") %>%
  qflextable() %>% 
  set_header_labels(kgc_course = "KG Zone", n = "Number of HHs",
                    percent = "Total Percent", valid_percent = "Percent among Non-Missing")

                    

# Correlation Graphs ----------------------------------------------------------------

# Filter out those with piped water
dataaim1_nopipe <- data_aim1 %>% filter(hv201_sourcecat != "Piped")
rural_nopipe <- rural %>% filter(hv201_sourcecat != "Piped")
urban_nopipe <- urban %>% filter(hv201_sourcecat != "Piped")

# Correlation for full data
cor_dataaim1 <- dataaim1_nopipe %>% select(hv204,
                                      e_totalminus7, e_totalminus14, e_totalminus30, e_totalminus60,
                                      tp_totalminus7, tp_totalminus14, tp_totalminus30, tp_totalminus60,
                                      sro_totalminus7, sro_totalminus14, sro_totalminus30, sro_totalminus60,
                                      ssro_totalminus7, ssro_totalminus14, ssro_totalminus30, ssro_totalminus60,
                                      t2m_avgminus7, t2m_avgminus14, t2m_avgminus30, t2m_avgminus60,
                                      skt_avgminus7, skt_avgminus14, skt_avgminus30, skt_avgminus60,
                                      lai_lv_avgminus7, lai_lv_avgminus14, lai_lv_avgminus30, lai_lv_avgminus60,
                                      lai_hv_avgminus7, lai_hv_avgminus14, lai_hv_avgminus30, lai_hv_avgminus60,
                                      d2m_avgminus7, d2m_avgminus14, d2m_avgminus30, d2m_avgminus60)
          
x <- cor_dataaim1 %>% 
        correlate() %>% 
        focus(hv204)

x %>% mutate(term = factor(term, levels = term[order(hv204)])) %>% 
      mutate(color = ifelse(hv204 < 0, "negative", "positive")) %>% 
      ggplot(aes(x = term, y = hv204)) + 
      geom_bar(stat = 'identity', position = 'identity', aes(fill = color)) +
      ylab("Correlation with Water Walk Time (HV204)") + 
      xlab("Variable") +
      scale_x_discrete(guide = guide_axis(angle = 30)) +
      scale_fill_manual(values = c(positive = "royalblue3", negative = "firebrick1"), guide = "none")
      

# Correlation for rural data
cor_rural <- rural_nopipe %>% select(hv204,
                                e_totalminus7, e_totalminus14, e_totalminus30, e_totalminus60,
                                tp_totalminus7, tp_totalminus14, tp_totalminus30, tp_totalminus60,
                                sro_totalminus7, sro_totalminus14, sro_totalminus30, sro_totalminus60,
                                ssro_totalminus7, ssro_totalminus14, ssro_totalminus30, ssro_totalminus60,
                                t2m_avgminus7, t2m_avgminus14, t2m_avgminus30, t2m_avgminus60,
                                skt_avgminus7, skt_avgminus14, skt_avgminus30, skt_avgminus60,
                                lai_lv_avgminus7, lai_lv_avgminus14, lai_lv_avgminus30, lai_lv_avgminus60,
                                lai_hv_avgminus7, lai_hv_avgminus14, lai_hv_avgminus30, lai_hv_avgminus60,
                                d2m_avgminus7, d2m_avgminus14, d2m_avgminus30, d2m_avgminus60)

x <- cor_rural %>% 
       correlate() %>% 
       focus(hv204)

x %>% mutate(term = factor(term, levels = term[order(hv204)])) %>% 
      mutate(color = ifelse(hv204 < 0, "negative", "positive")) %>% 
      ggplot(aes(x = term, y = hv204)) + 
      geom_bar(stat = 'identity', position = 'identity', aes(fill = color)) +
      ylab("Correlation with Water Walk Time (HV204)") + 
      xlab("Variable") +
      scale_x_discrete(guide = guide_axis(angle = 30)) +
      scale_fill_manual(values = c(positive = "royalblue3", negative = "firebrick1"), guide = "none")


# Correlation for urban data
cor_urban <- urban_nopipe %>% select(hv204,
                                e_totalminus7, e_totalminus14, e_totalminus30, e_totalminus60,
                                tp_totalminus7, tp_totalminus14, tp_totalminus30, tp_totalminus60,
                                sro_totalminus7, sro_totalminus14, sro_totalminus30, sro_totalminus60,
                                ssro_totalminus7, ssro_totalminus14, ssro_totalminus30, ssro_totalminus60,
                                t2m_avgminus7, t2m_avgminus14, t2m_avgminus30, t2m_avgminus60,
                                skt_avgminus7, skt_avgminus14, skt_avgminus30, skt_avgminus60,
                                lai_lv_avgminus7, lai_lv_avgminus14, lai_lv_avgminus30, lai_lv_avgminus60,
                                lai_hv_avgminus7, lai_hv_avgminus14, lai_hv_avgminus30, lai_hv_avgminus60,
                                d2m_avgminus7, d2m_avgminus14, d2m_avgminus30, d2m_avgminus60)
    
x <- cor_urban %>% 
       correlate() %>% 
       focus(hv204)

x %>% mutate(term = factor(term, levels = term[order(hv204)])) %>% 
      mutate(color = ifelse(hv204 < 0, "negative", "positive")) %>% 
      ggplot(aes(x = term, y = hv204)) + 
      geom_bar(stat = 'identity', position = 'identity', aes(fill = color)) +
      ylab("Correlation with Water Walk Time (HV204)") + 
      xlab("Variable") +
      scale_x_discrete(guide = guide_axis(angle = 30)) +
      scale_fill_manual(values = c(positive = "royalblue3", negative = "firebrick1"), guide = "none")


# Interaction Assessment -------------------------------------------------






# Modeling ----------------------------------------------------------------
model <- lmer(log(hv204) ~ tp_totalminus30 + e_totalminus30 + sro_totalminus30 + skt +   
                (1|name_year/hv001), data = rural_nopipe)
model
summary(model)


rural_nopipe %>% ggplot(aes(x = hv204)) + geom_density()
qqnorm(rural_nopipe$hv204, pch = 1)
qqline(rural_nopipe$hv204, col = "red", lwd = 3)
  

rural_nopipe %>% ggplot(aes(x = log(hv204))) + geom_density()
qqnorm(log(rural_nopipe$hv204), pch = 1)
qqline(log(rural_nopipe$hv204), col = "red", lwd = 3)




plot(model, resid(., scaled=TRUE) ~ fitted(.), abline = 0, pch = 16, xlab = "Fitted Values", ylab = "Standard Resid")

qqnorm(resid(model), pch = 16)
qqline(resid(model))



plot(model)

qqnorm(residuals(model))

qqnorm(rural_nopipe$hv204)

cor(rural_nopipe$skt, rural_nopipe$t2m, use = "pairwise.complete.obs")

