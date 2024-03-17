# Header ------------------------------------------------------------------

# Load Libraries
library(readr)
library(janitor)
library(flextable)
library(skimr)
library(corrr)
library(corrplot)
library(lme4)
library(jtools)
library(BSDA)
library(ggborderline)
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

## Tables for the Household Summary ---------------------------------------

# number of households by year
data_aim1 %>%
  group_by(hv007) %>%
  summarise(num_households = n()) %>%
  mutate(Percentage = round((num_households / sum(num_households)) * 100, digits = 2)) %>%
  adorn_totals("row") %>%
  qflextable() %>% 
  set_header_labels(hv007 = "Year", num_households = "Number of HHs")

# Summary Table of water walk times
# Includes total households, urban, rural, and missing
# T-test is done via BSDA package, which allows for manual entry of parameters
# T-test parameters are based on the summary table that was just created
# Final mutate function creates a % of households that have missing U/R designations
dual_table <- data_aim1 %>%
                group_by(hv007) %>%
                summarise(num_hh_total = n(),
                avg_walk_total = round(mean(hv204, na.rm = TRUE), 2),
                num_hh_rural = sum(URBAN_RURA == "R", na.rm = TRUE),
                avg_walk_rural = round(mean(hv204[URBAN_RURA == "R"], na.rm = TRUE), 2),
                num_hh_urban = sum(URBAN_RURA == "U", na.rm = TRUE),
                avg_walk_urban = round(mean(hv204[URBAN_RURA == "U"], na.rm = TRUE), 2),
                num_hh_missing = sum(is.na(URBAN_RURA)),
                avg_walk_missing = round(mean(hv204[is.na(URBAN_RURA)], na.rm = TRUE), 2),
                t_test = format(round(tsum.test(mean.x = avg_walk_rural,
                                                s.x = sd(hv204[URBAN_RURA == "R"], na.rm = TRUE),
                                                n.x = num_hh_rural,
                                                mean.y = avg_walk_urban,
                                                s.y = sd(hv204[URBAN_RURA == "U"], na.rm = TRUE),
                                                n.y = num_hh_urban,
                                                alternative = "greater")$p.value, 3), nsmall = 2)) %>%
                mutate(pct_missing = round((num_hh_missing / num_hh_total) * 100, digits = 1))

# Creates the flextable of the previously created table with custom column names
# Zebra theme creates gray/white boxes, and Box theme creates outlines
# Headers (text / not text) are aligned
# Removed the comma from the year column (1st column)
dual_table %>% flextable() %>% 
               set_header_labels(hv007 = "Year", 
                                 num_hh_total = "Total HHs", avg_walk_total = "Total Avg Walk",
                                 num_hh_rural = "Rural HHs", avg_walk_rural = "Rural Avg Walk",
                                 num_hh_urban = "Urban HHs", avg_walk_urban = "Urban Avg Walk",
                                 num_hh_missing = "Missing HHs", avg_walk_missing = "Missing Avg Walk",
                                 t_test = "One-Side T-Test P-Value", pct_missing = "% HH Missing U/R") %>% 
               theme_zebra() %>% theme_box() %>% 
               align_nottext_col(align = "center", header = TRUE) %>%
               align_text_col(align = "center", header = TRUE) %>%
               colformat_num(j = "hv007", big.mark = "")

# Pivoting the data in the previous table from wide to long
# Needed to create proper graphs. 
dual_graph <- pivot_longer(dual_table, cols = c('num_hh_total', 'num_hh_rural', 'num_hh_urban', 'num_hh_missing'), 
                           names_to = "hh_type", values_to = "num_hh")

## Graphs for the Household Summary ---------------------------------------

# Define common elements for all plots
common_elements <- list(scale_x_continuous(breaks = seq(1990, 2022, 1), labels = seq(1990, 2022, 1), expand = c(0, 0)),
                        scale_y_continuous(expand = c(0, 0)),
                        xlab("Year"), ylab("Number of Households"),
                        theme_classic(),
                        theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, face = "bold")))

# Define a function to create the plot
create_plot <- function(data, hh_types, fill_colors, fill_labels) {
               ggplot(dual_graph %>% filter(hh_type %in% hh_types), 
                      aes(x = hv007, y = num_hh, fill = factor(hh_type, levels = hh_types))) +
                      geom_bar(stat = "identity", position = "dodge") +
                      scale_fill_manual(values = fill_colors, labels = fill_labels) +
                      guides(fill = guide_legend(title = "Type of\nHousehold\n(Urban/Rural)")) + common_elements}
     
# Geoms for the line graph
geom_line_hh <- geom_borderline(data = dual_table, aes(x = hv007, y = avg_walk_total*2000, group = 1, color = "All Households"), inherit.aes = FALSE, linewidth = 1, bordercolour = "black")
geom_point_hh <- geom_point(data = dual_table, aes(x = hv007, y = avg_walk_total*2000, group = 1, color = "All Households"), inherit.aes = FALSE, size = 3)
geom_point_hh_outline <- geom_point(data = dual_table, aes(x = hv007, y = avg_walk_total*2000, group = 1), inherit.aes = FALSE, size = 4, color = "#000000")
geom_line_rural <- geom_borderline(data = dual_table, aes(x = hv007, y = avg_walk_rural*2000, group = 1, color = "Rural Households"), inherit.aes = FALSE, linewidth = 1, bordercolour = "black")
geom_point_rural <- geom_point(data = dual_table, aes(x = hv007, y = avg_walk_rural*2000, group = 1, color = "Rural Households"), inherit.aes = FALSE, size = 3)
geom_point_rural_outline <- geom_point(data = dual_table, aes(x = hv007, y = avg_walk_rural*2000, group = 1), inherit.aes = FALSE, size = 4, color = "#000000")
geom_line_urban <- geom_borderline(data = dual_table, aes(x = hv007, y = avg_walk_urban*2000, group = 1, color = "Urban Households"), inherit.aes = FALSE, linewidth = 1, bordercolor = "black")
geom_point_urban <- geom_point(data = dual_table, aes(x = hv007, y = avg_walk_urban*2000, group = 1, color = "Urban Households"), inherit.aes = FALSE, size = 3)
geom_point_urban_outline <- geom_point(data = dual_table, aes(x = hv007, y = avg_walk_urban*2000, group = 1), inherit.aes = FALSE, size = 4, color = "#000000")

# Bar Chart: Total Only
create_plot(dual_graph, c("num_hh_total"), c("num_hh_total" = "#2c7bb6"), c("num_hh_total" = "Total"))

# Bar Chart: Total and Urban
create_plot(dual_graph, c("num_hh_total", "num_hh_urban"), 
            c("num_hh_urban" = "#fdae61", "num_hh_total" = "#2c7bb6"), 
            c("num_hh_urban" = "Urban", "num_hh_total" = "Total"))

# Bar Chart: Total, Urban, and Rural
create_plot(dual_graph, c("num_hh_total", "num_hh_urban", "num_hh_rural"), 
            c("num_hh_urban" = "#fdae61", "num_hh_total" = "#2c7bb6", "num_hh_rural" = "#d7191c"),
            c("num_hh_urban" = "Urban", "num_hh_total" = "Total", "num_hh_rural" = "Rural"))

# Bar Chart: Total, Urban, Rural, and Missing (Full Bar Chart)
(full_graph <- create_plot(dual_graph, c("num_hh_total", "num_hh_rural", "num_hh_urban", "num_hh_missing"), 
                          c("num_hh_rural" = "#d7191c", "num_hh_urban" = "#fdae61", "num_hh_total" = "#2c7bb6", "num_hh_missing" = "grey45"),
                          c("num_hh_rural" = "Rural", "num_hh_urban" = "Urban", "num_hh_total" = "Total", "num_hh_missing" = "Missing")))

# Full Bar Chart with All Households Line Chart
full_graph + geom_line_hh + geom_point_hh_outline + geom_point_hh +
             scale_color_manual(name = "Household Type",
                                values = c("All Households" = "#2c7bb6"),
                                labels = c("All Households" = "All Households")) +
             scale_y_continuous(sec.axis = sec_axis(~./2000, name = "Average Walk Time"), expand = c(0, 0))

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
               

# Wealth Quintiles --------------------------------------------------------


data_aim1 %>% tabyl(hv270, hv007)

data_aim1 %>% tabyl(hv270a, hv007)



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

# Variables to subset
weather_var <- c("e_totalminus7",    "e_totalminus14",    "e_totalminus30",    "e_totalminus60",
                 "tp_totalminus7",   "tp_totalminus14",   "tp_totalminus30",   "tp_totalminus60",
                 "sro_totalminus7",  "sro_totalminus14",  "sro_totalminus30",  "sro_totalminus60",
                 "ssro_totalminus7", "ssro_totalminus14", "ssro_totalminus30", "ssro_totalminus60",
                 "t2m_avgminus7",    "t2m_avgminus14",    "t2m_avgminus30",    "t2m_avgminus60",
                 "skt_avgminus7",    "skt_avgminus14",    "skt_avgminus30",    "skt_avgminus60",
                 "lai_lv_avgminus7", "lai_lv_avgminus14", "lai_lv_avgminus30", "lai_lv_avgminus60",
                 "lai_hv_avgminus7", "lai_hv_avgminus14", "lai_hv_avgminus30", "lai_hv_avgminus60",
                 "d2m_avgminus7",    "d2m_avgminus14",    "d2m_avgminus30",    "d2m_avgminus60")

# Correlation for full data
subset_dataaim1 <- dataaim1_nopipe %>% select(hv204, all_of(weather_var))
          
cor_dataaim1_focus <- subset_dataaim1 %>% 
                      correlate() %>% 
                      focus(hv204)

cor_dataaim1_focus %>% mutate(term = factor(term, levels = term[order(hv204)])) %>% 
                       mutate(color = ifelse(hv204 < 0, "negative", "positive")) %>% 
                       ggplot(aes(x = term, y = hv204)) + 
                       geom_bar(stat = 'identity', position = 'identity', aes(fill = color)) +
                       ylab("Correlation with Water Walk Time (HV204)") + 
                       xlab("Variable") +
                       scale_x_discrete(guide = guide_axis(angle = 30)) +
                       scale_fill_manual(values = c(positive = "royalblue3", negative = "firebrick1"), guide = "none")

# Corrplot
subset_dataaim1 <- subset_dataaim1 %>% select(-hv204)

cor_dataaim1_weather <-  cor(subset_dataaim1, use = "complete.obs")

testRes <- cor.mtest(cor_dataaim1_weather, conf.level = 0.95)

labelCol <-  c("purple", "purple", "purple", "purple", "black", "black", "black", "black")

corrplot(cor_dataaim1_weather, p.mat = testRes$p, method = "square", order = "alphabet",
         diag = TRUE, type = "lower", sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
         insig = "label_sig", pch.col = "grey1", tl.col = labelCol)

      
# Correlation for rural data
subset_rural <- rural_nopipe %>% select(hv204, all_of(weather_var))

cor_rural_focus <- subset_rural %>% 
                   correlate() %>% 
                   focus(hv204)

cor_rural_focus %>% mutate(term = factor(term, levels = term[order(hv204)])) %>% 
                    mutate(color = ifelse(hv204 < 0, "negative", "positive")) %>% 
                    ggplot(aes(x = term, y = hv204)) + 
                    geom_bar(stat = 'identity', position = 'identity', aes(fill = color)) +
                    ylab("Correlation with Water Walk Time (HV204)") + 
                    xlab("Variable") +
                    scale_x_discrete(guide = guide_axis(angle = 30)) +
                    scale_fill_manual(values = c(positive = "royalblue3", negative = "firebrick1"), guide = "none")
                  
# Corr Plot
subset_rural <- subset_rural %>% select(-hv204)

cor_rural_weather <-  cor(subset_rural, use = "complete.obs")

testRes <- cor.mtest(cor_rural_weather, conf.level = 0.95)

labelCol <-  c("purple", "purple", "purple", "purple", "black", "black", "black", "black")

corrplot(cor_rural_weather, p.mat = testRes$p, method = "square", order = "alphabet",
         diag = TRUE, type = "lower", sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
         insig = "label_sig", pch.col = "grey1", tl.col = labelCol)



# Correlation for urban data
subset_urban <- urban_nopipe %>% select(hv204, all_of(weather_var))
    
cor_urban_focus <- subset_urban %>% 
                   correlate() %>% 
                   focus(hv204)

cor_urban_focus %>% mutate(term = factor(term, levels = term[order(hv204)])) %>% 
                    mutate(color = ifelse(hv204 < 0, "negative", "positive")) %>% 
                    ggplot(aes(x = term, y = hv204)) + 
                    geom_bar(stat = 'identity', position = 'identity', aes(fill = color)) +
                    ylab("Correlation with Water Walk Time (HV204)") + 
                    xlab("Variable") +
                    scale_x_discrete(guide = guide_axis(angle = 30)) +
                    scale_fill_manual(values = c(positive = "royalblue3", negative = "firebrick1"), guide = "none")

# Corr Plot
subset_urban <- subset_urban %>% select(-hv204)

cor_urban_weather <-  cor(subset_urban, use = "complete.obs")

testRes <- cor.mtest(cor_urban_weather, conf.level = 0.95)

labelCol <-  c("purple", "purple", "purple", "purple", "black", "black", "black", "black")

corrplot(cor_urban_weather, p.mat = testRes$p, method = "square", order = "alphabet",
         diag = TRUE, type = "lower", sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
         insig = "label_sig", pch.col = "grey1", tl.col = labelCol)

# Interaction Assessment -------------------------------------------------

# Tabyl for people who carry water
rural_nopipe %>% tabyl(hv236_person) %>% adorn_pct_formatting() %>%
  adorn_totals("row") %>%
  qflextable() %>% 
  set_header_labels(hv236_person = "Person Carrying Water", n = "Number of HHs",
                    percent = "Total Percent", valid_percent = "Percent among Non-Missing")

rural_nopipe %>% group_by(hv236_person) %>%   
  dplyr::summarise(mean_walk = mean(hv204),
                   num_households = n()) %>% qflextable()



# Modeling ----------------------------------------------------------------

rural <- rural %>% slice(1:25000)

# Showing the effects of the log on the outcome
rural_nopipe %>% ggplot(aes(x = hv204)) + geom_density()
qqnorm(rural_nopipe$hv204, pch = 1)
qqline(rural_nopipe$hv204, col = "red", lwd = 3)


rural_nopipe %>% ggplot(aes(x = log(hv204))) + geom_density()
qqnorm(log(rural_nopipe$hv204), pch = 1)
qqline(log(rural_nopipe$hv204), col = "red", lwd = 3)

# model
model <- lmer(log(hv204) ~ tp_totalminus30 + e_totalminus30 + sro_totalminus30 + skt +   
                (1|name_year/hv001), data = rural)


library(DHARMa)
simoutput <- simulateResiduals(fittedModel = model, plot = F)

plot(simoutput)

# in Base R
par(mfrow = c(2,2))
base::plot(model)


# summary of the model
summ(model)


# Model diagnostics
plot(model, resid(., scaled=TRUE) ~ fitted(.), abline = 0, pch = 16, xlab = "Fitted Values", ylab = "Standard Resid")

qqnorm(resid(model), pch = 16)
qqline(resid(model), col = 2)

