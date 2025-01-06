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
library(gtsummary)
library(sf)
library(gt)
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

# Read in the datasets
# data_aim1 <- readRDS("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 1/data_aim1.Rdata")
# urban <- readRDS("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 1/urban.Rdata")
rural_hh <- readRDS("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 1/rural.rds")

# Create Rural Dataset ----------------------------------------------------

# Create the "Rural Final" Dataset
# This filters out where the water collection times are:
# Missing, 0, and Unknown
rural_final <- rural_hh %>% 
               # filter out the Missing, 0's, and 995
               filter(!is.na(hv204), hv204 != 0, hv204 != 995) %>% 
               # Filter out all infinitesimally small GPS points
               filter(!between(LATNUM, -0.0001, 0.0001))

# Mapping ------------------------------------------------------

# Get the Lat, Long, U/R designation, and Year from the dataset
# Drop all the missing and remove any improbable values
mapping <- data_aim1 %>% 
           select(LATNUM, LONGNUM, URBAN_RURA, hv007) %>% 
           drop_na() %>% 
           filter(!between(LATNUM, -0.0001, 0.0001))

# Read in the Africa Shapefile
shapefile <- read_sf("Mapping/Africa/afr_g2014_2013_0.shp") %>% clean_names()

# Check the maximum extent of the households
max(mapping$LATNUM)
min(mapping$LATNUM)
max(mapping$LONGNUM)
min(mapping$LONGNUM)

# Crop the shapefile using the numbers from above
shapefile <- shapefile %>% 
             st_crop(c(xmin = -18, ymin = -35, 
                       xmax = 51, ymax = 26))
  
# Set the CRS for the Africa Shapefile
shapefile <- st_transform(shapefile, crs = 4326)

# Set the CRS for the datapoints
points <- mapping %>% 
          st_as_sf(coords = c("LONGNUM", "LATNUM"), crs = 4326)

# Create separate datasets for the U/R designation
points_rural <- points %>% filter(URBAN_RURA == "R")
points_urban <- points %>% filter(URBAN_RURA == "U")

# Plot the urban households
ggplot() + 
  geom_sf(data = shapefile, fill = "white") +
  geom_sf(data = points_urban, aes(color = URBAN_RURA), size = 0.4, alpha = 0.1) +
  scale_color_manual(values = c("U" = "#d7191c")) +
  theme_void() + 
  theme(legend.position = "none") +
  ggtitle("Urban Households") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.9, size = 22))

# Plot the rural households
ggplot() + 
  geom_sf(data = shapefile, fill = "white") +
  geom_sf(data = points_rural, aes(color = URBAN_RURA), size = 0.4, alpha = 0.1) +
  scale_color_manual(values = c("R" = "#2c7bb6"))+
  theme_void() + 
  theme(legend.position = "none") +
  ggtitle("Rural Households") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.9, size = 22))

# Facet wrap the maps by year (hv007)
ggplot() + 
  geom_sf(data = shapefile, fill = "white") +
  geom_sf(data = points, mapping = aes(color = URBAN_RURA))+
  scale_color_manual(name = "Household Type",
                     values = c("R" = "#2c7bb6", "U" = "#d7191c"),
                     labels = c("R" = "Rural Households", "U" = "Urban Households")) +
  theme_void()+ 
  theme(legend.position = "none") +
  facet_wrap(~hv007, ncol = 8)

# Households Summary ------------------------------------------------------

## Tables for the Household Summary ---------------------------------------

# This table shows the number of HHs by year, 
# along with accompanying percent of the total
data_aim1 %>%
  group_by(hv007) %>%
  summarise(num_hh = n()) %>%
  mutate(percent = round((num_hh / nrow(data_aim1)) * 100, digits = 2)) %>%
  adorn_totals("row",,,,num_hh) %>%
  qflextable() %>% 
  set_header_labels(hv007 = "Year", 
                    num_hh = "Number of Households",
                    percent = "Percent of Total") %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

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
                avg_walk_missing = round(mean(hv204[is.na(URBAN_RURA)], na.rm = TRUE), 2)) %>% 
                # t_test = format(round(tsum.test(mean.x = avg_walk_rural,
                #                                 s.x = sd(hv204[URBAN_RURA == "R"], na.rm = TRUE),
                #                                 n.x = num_hh_rural,
                #                                 mean.y = avg_walk_urban,
                #                                 s.y = sd(hv204[URBAN_RURA == "U"], na.rm = TRUE),
                #                                 n.y = num_hh_urban,
                #                                 alternative = "greater")$p.value, 3), nsmall = 2)) %>%
                mutate(pct_missing = round((num_hh_missing / num_hh_total) * 100, digits = 1))

# Creates the flextable of the previously created table with custom column names
# Zebra theme creates gray/white boxes, and Box theme creates outlines
# Headers (text / not text) are aligned
# Removed the comma from the year column (1st column)
dual_table %>% flextable() %>% 
               set_header_labels(hv007 = "Year", 
                                 num_hh_total = "Total HHs", avg_walk_total = "Mean Collection Time",
                                 num_hh_rural = "Rural HHs", avg_walk_rural = "Rural Mean Collection Time",
                                 num_hh_urban = "Urban HHs", avg_walk_urban = "Urban Mean Collection Time",
                                 num_hh_missing = "HH Missing Urbanicity", avg_walk_missing = "Missing Mean Collection Time",
                                 pct_missing = "% HH Missing Urbanicity") %>% 
               theme_zebra() %>% theme_box() %>% 
               align_nottext_col(align = "center", header = TRUE) %>%
               align_text_col(align = "center", header = TRUE) %>%
               colformat_num(j = "hv007", big.mark = "")



# TABLE 1

# Summary Table of water walk times
# Includes total households, urban, rural, and missing
# T-test is done via BSDA package, which allows for manual entry of parameters
# T-test parameters are based on the summary table that was just created
# Final mutate function creates a % of households that have missing U/R designations

# Summarize your data
summary_data <- data_aim1 %>%
  summarise(
    num_hh_total = n(),
    avg_walk_total = round(mean(hv204, na.rm = TRUE), 1),
    num_hh_rural = sum(URBAN_RURA == "R", na.rm = TRUE),
    num_hh_rural_pct = round((num_hh_rural / num_hh_total) * 100, 1),
    avg_walk_rural = round(mean(hv204[URBAN_RURA == "R"], na.rm = TRUE), 1),
    num_hh_urban = sum(URBAN_RURA == "U", na.rm = TRUE),
    num_hh_urban_pct = round((num_hh_urban / num_hh_total) * 100, 1),
    avg_walk_urban = round(mean(hv204[URBAN_RURA == "U"], na.rm = TRUE), 1),
    num_hh_missing = sum(is.na(URBAN_RURA)),
    num_hh_missing_pct = round((num_hh_missing / num_hh_total) * 100, 1),
    avg_walk_missing = round(mean(hv204[is.na(URBAN_RURA)], na.rm = TRUE), 1)) 

# Reshape the data to align `num` and `avg_walk` on the same row
reshaped_data <- summary_data %>%
  pivot_longer(
    cols = everything(),
    names_to = c("measure", "category"),
    names_pattern = "(num|pct|avg_walk)_(.*)") %>%
  mutate(category = gsub("^hh_", "", category)) %>%  # Clean category names
  pivot_wider(
    names_from = "measure",
    values_from = "value")


tbl1 <- reshaped_data %>%
  mutate(
    pct = if_else(str_detect(category, "_pct"), num, NA_real_),
    category = str_replace(category, "_pct", ""),
    num = if_else(is.na(pct), num, NA_real_)) %>%
  fill(num) %>%
  mutate(
    num = formatC(num, format = "d", big.mark = ","),
    num = if_else(!is.na(pct), paste0(num, " (", pct, "%)"), num),
    category = str_to_title(category),
    category = factor(category, levels = c("Urban", "Rural", "Missing", "Total"))) %>%
  filter(!str_detect(category, "Pct")) %>%
  select(-pct) %>%
  arrange(category) %>%
  flextable() %>%
  set_header_labels(
    category = "Type of Household", 
    num = "Households\n(%)",
    avg_walk = "Average Water\nCollection Time\n(Minutes)") %>%
  theme_zebra() %>%
  theme_box() %>%
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE) %>% 
  autofit()
  
  
save_as_docx(tbl1, path = "/data/mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 1/table1.docx")
  

  

## Graphs for the Household Summary ---------------------------------------

# This graph uses the dual_graph df that was just created
# This is a bar chart that shows the number of households separated
# into Total, Rural, Urban, and Missing
ggplot(dual_graph, 
       aes(x = hv007, y = num_hh, 
           fill = factor(hh_type, levels = c("num_hh_total", "num_hh_rural", 
                                             "num_hh_urban", "num_hh_missing")))) +
       geom_bar(stat = "identity", position = "dodge") +
       scale_fill_manual(values = c("num_hh_rural" = "#d7191c", "num_hh_urban" = "#fdae61",
                                    "num_hh_total" = "#2c7bb6", "num_hh_missing" = "grey45"),
                         labels = c("num_hh_rural" = "Rural", "num_hh_urban" = "Urban",
                                    "num_hh_total" = "Total", "num_hh_missing" = "Missing")) +
       guides(fill = guide_legend(title = "Type of\nHousehold\n(Urban/Rural)")) + 
       scale_x_continuous(breaks = seq(1990, 2022, 1), labels = seq(1990, 2022, 1), expand = c(0, 0)) +
       scale_y_continuous(expand = c(0, 0)) +
       xlab("Year") + ylab("Number of Households") +
       theme_classic()+
       theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, face = "bold"))


# This is a line graph that summarizes the water collection times of the dual_table df
# The lines are given a black outline
# There are two points - a black point that is laid behind the colored points
ggplot(dual_table) +

  ## Borderline and points for All Households
  geom_borderline(data = dual_table, aes(x = hv007, y = avg_walk_total, group = 1, color = "All Households"), inherit.aes = FALSE, linewidth = 1, bordercolour = "black") +
  geom_point(data = dual_table, aes(x = hv007, y = avg_walk_total, group = 1), inherit.aes = FALSE, size = 4, color = "#000000") +
  geom_point(data = dual_table, aes(x = hv007, y = avg_walk_total, group = 1, color = "All Households"), inherit.aes = FALSE, size = 3) +

  ## Borderline and points for Rural Households
  geom_borderline(data = dual_table, aes(x = hv007, y = avg_walk_rural, group = 1, color = "Rural Households"), inherit.aes = FALSE, linewidth = 1, bordercolour = "black") +
  geom_point(data = dual_table, aes(x = hv007, y = avg_walk_rural, group = 1), inherit.aes = FALSE, size = 4, color = "#000000") +
  geom_point(data = dual_table, aes(x = hv007, y = avg_walk_rural, group = 1, color = "Rural Households"), inherit.aes = FALSE, size = 3) +

  ## Borderline and points for Urban Households
  geom_borderline(data = dual_table, aes(x = hv007, y = avg_walk_urban, group = 1, color = "Urban Households"), inherit.aes = FALSE, linewidth = 1, bordercolor = "black") +
  geom_point(data = dual_table, aes(x = hv007, y = avg_walk_urban, group = 1), inherit.aes = FALSE, size = 4, color = "#000000") +
  geom_point(data = dual_table, aes(x = hv007, y = avg_walk_urban, group = 1, color = "Urban Households"), inherit.aes = FALSE, size = 3) +

  scale_color_manual(name = "Household Type",
                     values = c("All Households" = "#2c7bb6", "Rural Households" = "#d7191c", "Urban Households" = "#fdae61"),
                     labels = c("All Households" = "All Households", "Rural Households" = "Rural Households", "Urban Households" = "Urban Households")) +
  scale_x_continuous(breaks = seq(1990, 2022, 1), labels = seq(1990, 2022, 1), expand = c(0, 0.5)) +
  scale_y_continuous(expand = c(0.01, 0.5))+
  xlab("Year") + ylab("Minutes to Collect Water") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, face = "bold", size = 12)) +
  theme(axis.text.y = element_text(face = "bold", size = 12)) +
  theme(axis.title = element_text(face = "bold", size = 12)) +
  guides(fill = guide_legend(title = "Type of\nHousehold\n(Urban/Rural)"))

# Wealth Quintiles --------------------------------------------------------

# The following graph is a grouped boxplot that measures
# the water collection time by household combined wealth (hv270)
rural_final %>%
  filter(!is.na(hv270)) %>%
  ggplot(mapping = aes(x = factor(hv007), y = log(hv204), fill = factor(hv270))) +
          geom_boxplot() +
          labs(fill="SES Level") +
          scale_fill_manual(values=c("Poorest"="#d7191c", "Poor"="#fdae61",
                                     "Middle"="#ffffbf", "Rich"="#abdda4",
                                     "Richest"="#2b83ba")) +
          scale_y_continuous(expand = c(0, 0.05)) +
          xlab("Year") + ylab("Log (Water Collection Time)") + 
          theme_classic() +
          theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, face = "bold")) +
          ggtitle("Water Collection Times Among Rural Households by Household Combined Wealth") +
          theme(plot.title = element_text(hjust = 0.5))

# Koppen-Geiger Zones -----------------------------------------------------

# This table shows the total number of households
# in the "fine" resolution Koppen-Geiger Zones
data_aim1 %>% tabyl(kgc_fine) %>% 
              adorn_pct_formatting() %>%
              adorn_totals("row") %>%
              replace_na(list(kgc_fine = "Unknown KGZ")) %>%
              qflextable() %>% 
              set_header_labels(kgc_fine = "KG Zone", n = "Number of HHs",
                                percent = "Total Percent", valid_percent = "Percent among Non-Missing")%>% 
              theme_zebra() %>% theme_box() %>% 
              align_nottext_col(align = "center", header = TRUE) %>%
              align_text_col(align = "center", header = TRUE)

# This table shows the total number of households
# in the "course" resolution Koppen-Geiger Zones
data_aim1 %>% tabyl(kgc_course) %>% 
              adorn_pct_formatting() %>%
              adorn_totals("row") %>%
              replace_na(list(kgc_course = "Unknown KGZ")) %>%
              qflextable() %>% 
              set_header_labels(kgc_course = "KG Zone", n = "Number of HHs",
                                percent = "Total Percent", valid_percent = "Percent among Non-Missing")%>% 
              theme_zebra() %>% theme_box() %>% 
              align_nottext_col(align = "center", header = TRUE) %>%
              align_text_col(align = "center", header = TRUE)

# This table shows the number of total, rural, and urban households
# in the "course" resolution Koppen-Geiger Zones
# along with their average water collection times
data_aim1 %>% group_by(kgc_course) %>%
              summarise(num_hh_total = n(),
              avg_walk_total = round(mean(hv204, na.rm = TRUE), 2),
              num_hh_rural = sum(URBAN_RURA == "R", na.rm = TRUE),
              avg_walk_rural = round(mean(hv204[URBAN_RURA == "R"], na.rm = TRUE), 2),
              num_hh_urban = sum(URBAN_RURA == "U", na.rm = TRUE),
              avg_walk_urban = round(mean(hv204[URBAN_RURA == "U"], na.rm = TRUE), 2)) %>%
              replace_na(list(kgc_course = "Unknown KGZ")) %>% 
              qflextable() %>% 
              set_header_labels(kgc_course = "KG Zone", 
                                num_hh_total = "Total HHs", avg_walk_total = "Total Avg Collect",
                                num_hh_rural = "Rural HHs", avg_walk_rural = "Rural Avg Collect",
                                num_hh_urban = "Urban HHs", avg_walk_urban = "Urban Avg Collect") %>% 
              theme_zebra() %>% theme_box() %>% 
              align_nottext_col(align = "center", header = TRUE) %>%
              align_text_col(align = "center", header = TRUE)

# This table shows only the rural households
# and which KG zone they belong in (fine resolution)
rural_final %>% tabyl(kgc_fine) %>% 
                adorn_pct_formatting() %>%
                adorn_totals("row") %>%
                replace_na(list(kgc_fine = "Unknown KGZ")) %>%
                qflextable() %>% 
                set_header_labels(kgc_fine = "KG Zone", n = "Number of HHs",
                                  percent = "Total Percent", valid_percent = "Percent among Non-Missing")%>% 
                theme_zebra() %>% theme_box() %>% 
                align_nottext_col(align = "center", header = TRUE) %>%
                align_text_col(align = "center", header = TRUE)

# This table shows only the rural households
# and which KG zone they belong in (course resolution)
# along with the average water collection time in each zone
tbl4 <- rural_final %>% group_by(kgc_course) %>% 
                summarise(num_hh = n(),
                          percent = round((num_hh / nrow(rural_final)), 3)*100,
                          avg_walk = round(mean(hv204, na.rm = TRUE), 1)) %>%
                mutate(kgc_course = recode(kgc_course, .missing = "Unknown KGZ")) %>%
  mutate(pct = round((num_hh / sum(num_hh)) * 100, 1)) %>% 
  mutate(
    num_hh = formatC(num_hh, format = "d", big.mark = ",")) %>% 
  mutate(num_hh = if_else(is.na(pct), as.character(num_hh), 
                                paste0(num_hh, " (", pct, ")"))) %>%
  select(-c(pct, percent)) %>% 
                arrange(desc(num_hh)) %>% 
                qflextable() %>% 
                set_header_labels(kgc_course = "Köppen-Geiger Zone", num_hh = "Households\n(%)",
                                  avg_walk = "Average Water \nCollection Time\n(Minutes)")%>% 
  add_body_row(
    values = c("Total", "781,761","-"),
    colwidths = c(1,1,1), top = FALSE) %>% 
                theme_zebra() %>% theme_box() %>% 
                align_nottext_col(align = "center", header = TRUE) %>%
                align_text_col(align = "center", header = TRUE)%>% 
  autofit()


save_as_docx(tbl4, path = "/data/mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 1/table4.docx")




rural_kwallis <- rural_final %>% drop_na(kgc_course)

rural_kwallis %>% tabyl(kgc_course)

# Perform the Kruskal-Wallis test
kruskal_test_result <- kruskal.test(hv204 ~ kgc_course, data = rural_kwallis)

# Display the results
print(kruskal_test_result)





# Weather Correlation ----------------------------------------------------------------

## Subset ------------------------------------------------------------------

# Weather variables to subset
weather_var <- c("e_totalminus7",    "e_totalminus14",    "e_totalminus30",    "e_totalminus60",
                 "tp_totalminus7",   "tp_totalminus14",   "tp_totalminus30",   "tp_totalminus60",
                 "sro_totalminus7",  "sro_totalminus14",  "sro_totalminus30",  "sro_totalminus60",
                 "ssro_totalminus7", "ssro_totalminus14", "ssro_totalminus30", "ssro_totalminus60",
                 "t2m_avgminus7",    "t2m_avgminus14",    "t2m_avgminus30",    "t2m_avgminus60",
                 "skt_avgminus7",    "skt_avgminus14",    "skt_avgminus30",    "skt_avgminus60",
                 "lai_lv_avgminus7", "lai_lv_avgminus14", "lai_lv_avgminus30", "lai_lv_avgminus60",
                 "lai_hv_avgminus7", "lai_hv_avgminus14", "lai_hv_avgminus30", "lai_hv_avgminus60",
                 "d2m_avgminus7",    "d2m_avgminus14",    "d2m_avgminus30",    "d2m_avgminus60")

# Subset the rural dataset so it only includes the water collection time (hv204)
# as well as all of the weather variables above.
subset_rural <- rural_final %>% select(hv204, all_of(weather_var))

## Focused Correlation -----------------------------------------------------

# The focus() function from the "corrr" package finds the correlations
# between all of the weather variables and the hv204 variable.
# Finally, arrange the correlations from least to most
cor_rural_focus <- subset_rural %>% 
                   correlate() %>% 
                   focus(hv204) %>% 
                   arrange(hv204)

# Create a brand new variable "color" and set this variable
# so that each weather variable group has a set color association.
cor_rural_focus <- cor_rural_focus %>%
                   mutate(color = case_when(
                                  grepl("e", term) ~ "#e41a1c",
                                  grepl("^sro_*", term) ~ "#377eb8",
                                  grepl("^ssro_*", term) ~ "#4daf4a",
                                  grepl("t2m", term) ~ "#984ea3",
                                  grepl("tp", term) ~ "#ff7f00",
                                  grepl("skt", term) ~ "#2b888f",
                                  grepl("lai_lv", term) ~ "#a65628",
                                  grepl("lai_hv", term) ~ "#f781bf",
                                  grepl("d2m", term) ~ "#999999",
                                  TRUE ~ "FAIL!!! TRY AGAIN"))

# Save the color scheme to a character list
color_chart <- cor_rural_focus$color

# Plot the results from the focus() function
# First, we reorder the weather variables so they're in order of correlation
# Then we create a new variable to see if correlations are negative or positive
# Then we fill the bars (fill = ) and color the bar outlines (color)
# alpha is opacity
# Then we define positive as blue and negative as red
cor_rural_focus %>% mutate(term = fct_reorder(term, hv204)) %>%
                    mutate(color = ifelse(hv204 < 0, "negative", "positive")) %>% 
                    ggplot(aes(x = term, y = hv204)) + 
                    geom_bar(stat = "identity", position = "identity", aes(fill = color), color = color_chart, linewidth = 1, alpha = 0.7) +
                    ylab("Correlation with Water Collection Time") + 
                    xlab("Variable") +
                    theme_bw() +
                    scale_fill_manual(values = c(positive = "royalblue3", negative = "firebrick1"), guide = "none")+
                    theme(axis.text.x = element_text(angle = 55, hjust = 1, colour = color_chart, face = "bold", size = 11))

## Correlation Plot --------------------------------------------------------

# Find the correlation between all of the weather observations
cor_rural_weather <- cor(subset_rural, use = "complete.obs")

# Find the significance of the correlations between all the variables
cor_test <- cor.mtest(cor_rural_weather, conf.level = 0.95)

# Colors for the labels of the correlation matrix
# We can do this since each group only has 4 variables per group
label_colors <-  c("purple", "purple", "purple", "purple", "black", "black", "black", "black")

# Create the correlation plot
corrplot(cor_rural_weather, p.mat = cor_test$p, method = "square", order = "alphabet",
         diag = TRUE, type = "lower", sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
         insig = "label_sig", pch.col = "grey1", tl.col = label_colors, tl.srt = 45,
         col=colorRampPalette(c("darkblue", "white", "darkred"))(200))



















# Person Carrying Water -------------------------------------------------

# Tabyl for people who carry water
rural_final %>% 
  group_by(hv236_person) %>% 
  summarise(num_hh_total = n())  %>% 
  replace_na(list(hv236_person = "Undetermined Person")) %>%
  adorn_totals("row") %>% 
  qflextable() %>% 
  set_header_labels(hv236_person = "Person Carrying Water", 
                    num_hh_total = "Total HHs") %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE) 



rural_final %>% tabyl(hv236_person_recode)


tbl2 <- rural_final %>% 
  group_by(hv236_person_recode) %>% 
  summarise(num_hh_total = n(),
            avg_walk = round(mean(hv204[URBAN_RURA == "R"], na.rm = TRUE), 1)) %>%
  mutate(pct = round((num_hh_total / sum(num_hh_total)) * 100, 1)) %>% 
  mutate(
    num_hh_total = formatC(num_hh_total, format = "d", big.mark = ",")) %>% 
  mutate(num_hh_total = if_else(is.na(pct), as.character(num_hh_total), 
                       paste0(num_hh_total, " (", pct, ")"))) %>%
  select(-pct)%>% 
  replace_na(list(hv236_person_recode = "Unknown Person")) %>%
  arrange(desc(num_hh_total)) %>% 
  qflextable() %>% 
  set_header_labels(hv236_person_recode = "Person Responsible for \nHousehold Water", 
                    num_hh_total = "Households\n(%)", avg_walk = "Average Water \nCollection Time\n(Minutes)") %>% 
  add_body_row(
               values = c("Total", "781,761",""),
               colwidths = c(1,1,1), top = FALSE) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE) %>% 
  autofit()

save_as_docx(tbl2, path = "/data/mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 1/table2.docx")


rural_kwallis <- rural_final %>% drop_na(hv236_person_recode)

rural_kwallis %>% tabyl(hv236_person_recode)

# Perform the Kruskal-Wallis test
kruskal_test_result <- kruskal.test(hv204 ~ hv236_person_recode, data = rural_kwallis)

# Display the results
print(kruskal_test_result)







data_aim1 %>% group_by(kgc_course) %>%
  summarise(num_hh_total = n(),
            avg_walk_total = round(mean(hv204, na.rm = TRUE), 2),
            num_hh_rural = sum(URBAN_RURA == "R", na.rm = TRUE),
            avg_walk_rural = round(mean(hv204[URBAN_RURA == "R"], na.rm = TRUE), 2),
            num_hh_urban = sum(URBAN_RURA == "U", na.rm = TRUE),
            avg_walk_urban = round(mean(hv204[URBAN_RURA == "U"], na.rm = TRUE), 2)) %>%
  replace_na(list(kgc_course = "Unknown KGZ")) %>% 
  qflextable() %>% 
  set_header_labels(kgc_course = "KG Zone", 
                    num_hh_total = "Total HHs", avg_walk_total = "Total Avg Walk",
                    num_hh_rural = "Rural HHs", avg_walk_rural = "Rural Avg Walk",
                    num_hh_urban = "Urban HHs", avg_walk_urban = "Urban Avg Walk") %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

# Improved vs. Unimproved -------------------------------------------------


# This table shows which water source (hv201_source) only had missing data (NaN)
# First, group by water source then only select those where avg is NaN
# Then you're left with the number of households that rely on that water source
rural %>% group_by(hv201_source) %>% 
  summarize(num = n(), 
            avg_walk = mean(hv204, na.rm = TRUE)) %>% 
  filter(is.nan(avg_walk)) %>% 
  select(-avg_walk) %>% 
  arrange(desc(num)) %>% 
  qflextable() %>% 
  set_header_labels(hv201_source = "Water Source", 
                    num = "Total HHs") %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)






##  Source of Water (decomposed)
rural_final %>% 
  group_by(hv201_source) %>%
  summarise(num_hh_total = n(),
            percent = round((num_hh_total / nrow(rural_final)), 4)*100,
            water_walk = round(mean(hv204, na.rm = TRUE), 2)) %>% 
  arrange(desc(num_hh_total)) %>% 
  head(15) %>% 
  qflextable() %>% 
  set_header_labels(hv201_source = "Source of Water", 
                    num_hh_total = "Rural HHs",
                    percent = "Percent of Rural HH",
                    water_walk = "Avg Water Time") %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

##  Source of Water (categorized)
rural_final %>% 
  group_by(hv201_sourcecat) %>%
  summarise(num_hh_total = n(),
            percent = round((num_hh_total / nrow(rural_final)), 4)*100,
            water_walk = round(mean(hv204, na.rm = TRUE), 2)) %>% 
  arrange(desc(num_hh_total)) %>%
  qflextable() %>% 
  set_header_labels(hv201_sourcecat = "Source of Water", 
                    num_hh_total = "Rural HHs",
                    percent = "Percent of Rural HH",
                    water_walk = "Avg Water Time") %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

## Improved vs. Unimproved
tbl3 <- rural_final %>% 
  group_by(hv201_improved) %>%
  summarise(num_hh_total = n(),
            percent = round((num_hh_total / nrow(rural_final)), 3)*100,
            water_walk = round(mean(hv204, na.rm = TRUE), 1)) %>% 
  mutate(pct = round((num_hh_total / sum(num_hh_total)) * 100, 1)) %>% 
  mutate(
    num_hh_total = formatC(num_hh_total, format = "d", big.mark = ",")) %>% 
  mutate(num_hh_total = if_else(is.na(pct), as.character(num_hh_total), 
                                paste0(num_hh_total, " (", pct, ")"))) %>%
  select(-c(pct, percent)) %>% 
  arrange(desc(num_hh_total)) %>%
  qflextable() %>% 
  set_header_labels(hv201_improved = "Type of Water Source", 
                    num_hh_total = "Households\n(%)",
                    water_walk ="Average Water \nCollection Time\n(Minutes)") %>% 
  add_body_row(
    values = c("Total", "781,761","-"),
    colwidths = c(1,1,1), top = FALSE) %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)%>% 
  autofit()

save_as_docx(tbl3, path = "/data/mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 1/table3.docx")






# Perform the Kruskal-Wallis test
kruskal_test_result <- kruskal.test(hv204 ~ hv201_improved, data = rural_final)

# Display the results
print(kruskal_test_result)






data_aim1 %>%
  group_by(hv201_improved) %>%
  summarise(num_hh_total = n(),
            avg_walk_total = round(mean(hv204, na.rm = TRUE), 2),
            num_hh_rural = sum(URBAN_RURA == "R", na.rm = TRUE),
            avg_walk_rural = round(mean(hv204[URBAN_RURA == "R"], na.rm = TRUE), 2),
            num_hh_urban = sum(URBAN_RURA == "U", na.rm = TRUE),
            avg_walk_urban = round(mean(hv204[URBAN_RURA == "U"], na.rm = TRUE), 2),
            num_hh_missing = sum(is.na(URBAN_RURA)),
            avg_walk_missing = round(mean(hv204[is.na(URBAN_RURA)], na.rm = TRUE), 2)) %>%
  arrange(desc(num_hh_total)) %>% 
flextable() %>% 
  set_header_labels(hv201_improved = "Improved/Unimproved", 
                    num_hh_total = "Total HHs", avg_walk_total = "Total Avg Walk",
                    num_hh_rural = "Rural HHs", avg_walk_rural = "Rural Avg Walk",
                    num_hh_urban = "Urban HHs", avg_walk_urban = "Urban Avg Walk",
                    num_hh_missing = "Missing HHs", avg_walk_missing = "Missing Avg Walk") %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)


rural_final <- rural_final %>%
  mutate(five_year = cut(hv007, 
                         breaks = seq(1990, 2025, by = 5), 
                         right = FALSE, 
                         labels = c("1990-1994", 
                                    "1995-1999", 
                                    "2000-2004", 
                                    "2005-2009", 
                                    "2010-2014", 
                                    "2015-2019", 
                                    "2020-2022")))

plot <- rural_final %>%
  group_by(five_year) %>% # Group by five_year FIRST to get total households in each five_year group
  mutate(total_households_in_group = n()) %>% # Calculate total households in each five_year group
  group_by(hv201_improved, five_year) %>% # THEN group by both hv201_improved AND five_year
  summarize(
    households = n(),
    percent = households / total_households_in_group[1], # Calculate percentage based on the total of the five_year group
    .groups = "drop" # Important to avoid nested grouping
  )

# Average water collection times 

rural_final %>%
  group_by(five_year) %>% # Group by five_year FIRST to get total households in each five_year group
  mutate(total_households_in_group = n()) %>% # Calculate total households in each five_year group
  group_by(hv201_improved, five_year) %>% # THEN group by both hv201_improved AND five_year
  summarize(
    households = n(),
    percent = scales::percent(households / total_households_in_group[1], accuracy = 0.1),# Calculate percentage based on the total of the five_year group
    average_collection = round(mean(hv204, na.rm = TRUE), 2),
    .groups = "drop") %>%  # Important to avoid nested grouping
  mutate(hv201_improved = factor(hv201_improved, levels = c("Improved", "Unimproved", "Other/Unknown"))) %>% 
  arrange(five_year, hv201_improved) %>% 
  qflextable() %>% 
  set_header_labels(hv201_improved = "Type of Water Source",
                    five_year = "Five Year Group",
    households = "Number of Households", 
                    average_collection = "Average water collection time (mins)",
                    percent = "Percent of Total HH in 5 year group") %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)



# Reorder the hv201_improved variable
plot$hv201_improved <- factor(plot$hv201_improved, levels = c("Improved", "Unimproved", "Other/Unknown"))

# Define custom colors
custom_colors <- c("Improved" = "#1b9e77", "Unimproved" = "#d95f02", "Other/Unknown" = "#7570b3")

# Create the plot
ggplot(data = plot, aes(x = five_year, y = households, fill = hv201_improved)) +
  geom_col(position = "dodge") +
  geom_borderline(data = plot %>% filter(hv201_improved == "Improved"), # Filter for "Improved"
            aes(x = five_year, y = percent * 145000, group = 1), # Scale percent to match primary axis
            color = "#a6cee3", inherit.aes = FALSE, linewidth = 2, bordercolour = "#1f78b4") +

  geom_point(data = plot %>% filter(hv201_improved == "Improved"), 
             aes(x = five_year, y = percent * 145000, group = 1), 
             inherit.aes = FALSE, size = 6, color = "#000000") +
  
  scale_fill_manual(values = custom_colors) +
  scale_x_discrete(expand = c(0, 0.002)) +
  scale_y_continuous(
    limits = c(0, 145000),
    breaks = seq(0, 145000, by = 25000),
    expand = c(0, 0),
    sec.axis = sec_axis(~./145000, name = "Percent of Households with an Improved Water Source", labels = scales::percent)) + # Corrected secondary axis label
  labs(x = "5-Year Groups", y = "Number of Households", fill = "Type of\nWater Source") +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1, vjust = 1, size = 13),
        legend.title = element_text(hjust = 0.5, size = 13),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 13))


plot %>%
  mutate(hv201_improved = factor(hv201_improved, levels = c("Improved", "Unimproved", "Other/Unknown"))) %>% 
  arrange(five_year, hv201_improved) %>% qflextable() %>% 
  set_header_labels(hv201_improved = "Water Source", 
                    five_year = "5-Year Categories",
                    households = "Number of Households",
                    percent = "Percent per 5-Year Group") %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)


# Flowchart ---------------------------------------------------------------

## All Households to Rural Households
data_aim1 %>% 
group_by(URBAN_RURA) %>%
  summarise(num_hh_total = n(),
            percent = round((num_hh_total / 1759320), 3)*100,
            water_walk = round(mean(hv204, na.rm = TRUE), 2)) %>%
  mutate(URBAN_RURA = recode(URBAN_RURA, "R" = "Rural",
                             "U" = "Urban",
                             .missing = "Unknown Urbanicity")) %>%
  mutate(URBAN_RURA = factor(URBAN_RURA, levels = c("Rural", "Urban", "Unknown Urbanicity"))) %>% 
  qflextable() %>% 
  set_header_labels(URBAN_RURA = "Urban/Rural", 
                    num_hh_total = "Total HHs",
                    percent = "Percent of Total HH",
                    water_walk = "Avg Water Time") %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)


# filter only "elsewhere"
rural_else <- rural %>% filter(hv235 == 3)

# Number of 0's and 995
rural_else %>% 
  summarise(
    num_hh_zero = sum(hv204 == 0, na.rm = TRUE),
    num_hh_1to5 = sum(hv204 >= 1 & hv204 <= 5, na.rm = TRUE),
    num_hh_6to10 = sum(hv204 >= 6 & hv204 <= 10, na.rm = TRUE),
    num_hh_11to20 = sum(hv204 >= 11 & hv204 <= 20, na.rm = TRUE),
    num_hh_21more = sum(hv204 >= 21, na.rm = TRUE),
    num_hh_995 = sum(hv204 == 995, na.rm = TRUE),
    num_hh_missing = sum(is.na(hv204))) %>%
  qflextable() %>% 
  set_header_labels(num_hh_zero = "Zero Walk Time",
                    num_hh_1to5 = "1-5 Minutes",
                    num_hh_6to10 = "6-10 Minutes",
                    num_hh_11to20 = "11-20 Minutes",
                    num_hh_21more = "21+ Minutes",
                    num_hh_995 = "Unknown Walk Time",
                    num_hh_missing = "Missing Walk Time") %>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

# filter out the 0's and 995
rural_clean <- rural_else %>% filter(!is.na(hv204), hv204 != 0, hv204 != 995)

## Create Modeling Dataset
rural_model <- rural_clean %>% filter(hv201_sourcecat %in% c("Borehole",
                                                             "Unprotected",
                                                             "River",
                                                             "Protected",
                                                             "Piped"))

# Modeling ----------------------------------------------------------------

# Showing the effects of the log on the outcome
rural_final %>% ggplot(aes(x = hv204)) + geom_density() + theme_minimal()
qqnorm(rural_final$hv204, pch = 1)
qqline(rural_final$hv204, col = "red", lwd = 3)


rural_final %>% ggplot(aes(x = hv204)) + geom_density() + theme_minimal() +
  geom_vline(xintercept = 60, linetype = "dotted", color = "red")+
  geom_vline(xintercept = 120, linetype = "dotted", color = "red")+
  geom_vline(xintercept = 180, linetype = "dotted", color = "red")+
  geom_vline(xintercept = 240, linetype = "dotted", color = "red")+
  geom_vline(xintercept = 300, linetype = "dotted", color = "red")+
  geom_vline(xintercept = 360, linetype = "dotted", color = "red")+
  geom_vline(xintercept = 420, linetype = "dotted", color = "red")+
  geom_vline(xintercept = 480, linetype = "dotted", color = "red")+
  geom_vline(xintercept = 540, linetype = "dotted", color = "red")



rural_final %>% ggplot(aes(x = log(hv204))) + geom_density() + theme_minimal()
qqnorm(log(rural_final$hv204), pch = 1)
qqline(log(rural_final$hv204), col = "red", lwd = 3)




theme_gtsummary_journal(journal = "jama")



## TP Model ----------------------------------------------------------------

model_tp7 <- lmer(log(hv204) ~ tp_totalminus7 + hv270 + kgc_course + hv236_person_recode + hv201_improved +
             (1|name_year/hv001), data = rural_final, REML = FALSE) %>% 
             tbl_regression(label = list(tp_totalminus7 ~ "7 Day Precip", hv270 ~ "SES",
                                         hv236_person_recode ~ "Person Carrying Water",
                                         hv201_improved ~ "Improved Water Source",
                                         kgc_course ~ "Koppen-Geiger Zone"), exponentiate = TRUE) %>% bold_labels()

model_tp14 <- lmer(log(hv204) ~ tp_totalminus14 + hv270 + kgc_course + hv236_person_recode + hv201_improved +
              (1|name_year/hv001), data = rural_final, REML = FALSE) %>% 
              tbl_regression(label = list(tp_totalminus14 ~ "14 Day Precip", hv270 ~ "SES",
                                          hv236_person_recode ~ "Person Carrying Water",
                                          hv201_improved ~ "Improved Water Source",
                                          kgc_course ~ "Koppen-Geiger Zone"), exponentiate = TRUE) %>% bold_labels()

model_tp30 <- lmer(log(hv204) ~ tp_totalminus30 + hv270 + kgc_course + hv236_person_recode + hv201_improved  +
              (1|name_year/hv001), data = rural_final, REML = FALSE) %>% 
              tbl_regression(label = list(tp_totalminus30 ~ "30 Day Precip", hv270 ~ "SES",
                                          hv236_person_recode ~ "Person Carrying Water",
                                          hv201_improved ~ "Improved Water Source",
                                          kgc_course ~ "Koppen-Geiger Zone"), exponentiate = TRUE) %>% bold_labels()            

model_tp60 <- lmer(log(hv204) ~ tp_totalminus60 + hv270 + kgc_course + hv236_person_recode + hv201_improved +
              (1|name_year/hv001), data = rural_final, REML = FALSE) %>% 
              tbl_regression(label = list(tp_totalminus60 ~ "60 Day Precip", hv270 ~ "SES",
                                          hv236_person_recode ~ "Person Carrying Water",
                                          hv201_improved ~ "Improved Water Source",
                                          kgc_course ~ "Koppen-Geiger Zone"), exponentiate = TRUE) %>% bold_labels()


tp_models <- tbl_merge(tbls = list(model_tp7, model_tp14, model_tp30, model_tp60),
                       tab_spanner = c("**Precipitation, 7**", "**Precipitation, 14**",
                                       "**Precipitation, 30**", "**Precipitation, 60**")) %>%
                       modify_table_body(~.x %>% 
                       dplyr::arrange(factor(var_label, levels =
                       c("7 Day Precip", "14 Day Precip", "30 Day Precip", "60 Day Precip",
                         "SES", "Koppen-Geiger Zone", "Improved Water Source", "Person Carrying Water"))))


tp_models <- tp_models %>% as_gt()


tp_models %>% 
  tab_style(
    style = cell_borders(
      sides = c("left", "right"),
      color = "gray80",
      weight = px(2),
      style = "solid"),
    locations = list(cells_body(),
                     cells_column_labels(),
                     cells_column_spanners()))

## SRO Model ----------------------------------------------------------------

model_sro7 <- lmer(log(hv204) ~ sro_totalminus7 + hv270 + kgc_course + hv236_person_recode + hv201_improved +
             (1|name_year/hv001), data = rural_final, REML = FALSE) %>% 
             tbl_regression(label = list(sro_totalminus7 ~ "7 Day SRO", hv270 ~ "SES",
                                         hv236_person_recode ~ "Person Carrying Water",
                                         hv201_improved ~ "Improved Water Source",
                                         kgc_course ~ "Koppen-Geiger Zone"), exponentiate = TRUE) %>% bold_labels()

model_sro14 <- lmer(log(hv204) ~ sro_totalminus14 + hv270 + kgc_course + hv236_person_recode + hv201_improved +
                     (1|name_year/hv001), data = rural_final, REML = FALSE) %>% 
  tbl_regression(label = list(sro_totalminus14 ~ "14 Day SRO", hv270 ~ "SES",
                              hv236_person_recode ~ "Person Carrying Water",
                              hv201_improved ~ "Improved Water Source",
                              kgc_course ~ "Koppen-Geiger Zone"), exponentiate = TRUE) %>% bold_labels()

model_sro30 <- lmer(log(hv204) ~ sro_totalminus30 + hv270 + kgc_course + hv236_person_recode + hv201_improved  +
                     (1|name_year/hv001), data = rural_final, REML = FALSE) %>% 
  tbl_regression(label = list(sro_totalminus30 ~ "30 Day SRO", hv270 ~ "SES",
                              hv236_person_recode ~ "Person Carrying Water",
                              hv201_improved ~ "Improved Water Source",
                              kgc_course ~ "Koppen-Geiger Zone"), exponentiate = TRUE) %>% bold_labels()            

model_sro60 <- lmer(log(hv204) ~ sro_totalminus60 + hv270 + kgc_course + hv236_person_recode + hv201_improved +
                     (1|name_year/hv001), data = rural_final, REML = FALSE) %>% 
  tbl_regression(label = list(sro_totalminus60 ~ "60 Day SRO", hv270 ~ "SES",
                              hv236_person_recode ~ "Person Carrying Water",
                              hv201_improved ~ "Improved Water Source",
                              kgc_course ~ "Koppen-Geiger Zone"), exponentiate = TRUE) %>% bold_labels()


sro_models <- tbl_merge(tbls = list(model_sro7, model_sro14, model_sro30, model_sro60),
                       tab_spanner = c("**SRO, 7**", "**SRO, 14**",
                                       "**SRO, 30**", "**SRO, 60**")) %>%
  modify_table_body(~.x %>% 
                      dplyr::arrange(factor(var_label, levels =
                                              c("7 Day SRO", "14 Day SRO", "30 Day SRO", "60 Day SRO",
                                                "SES", "Koppen-Geiger Zone", "Improved Water Source", "Person Carrying Water"))))


sro_models <- sro_models %>% as_gt()


sro_models %>% 
  tab_style(
    style = cell_borders(
      sides = c("left", "right"),
      color = "gray80",
      weight = px(2),
      style = "solid"),
    locations = list(cells_body(),
                     cells_column_labels(),
                     cells_column_spanners()))

## Evaporation Model ----------------------------------------------------------------

model_e7 <- lmer(log(hv204) ~ e_totalminus7 + hv270 + kgc_course + hv236_person_recode + hv201_improved +
                     (1|name_year/hv001), data = rural_final, REML = FALSE) %>% 
  tbl_regression(label = list(e_totalminus7 ~ "7 Day Evaporation", hv270 ~ "SES",
                              hv236_person_recode ~ "Person Carrying Water",
                              hv201_improved ~ "Improved Water Source",
                              kgc_course ~ "Koppen-Geiger Zone"), exponentiate = TRUE) %>% bold_labels()

model_e14 <- lmer(log(hv204) ~ e_totalminus14 + hv270 + kgc_course + hv236_person_recode + hv201_improved +
                      (1|name_year/hv001), data = rural_final, REML = FALSE) %>% 
  tbl_regression(label = list(e_totalminus14 ~ "14 Day Evaporation", hv270 ~ "SES",
                              hv236_person_recode ~ "Person Carrying Water",
                              hv201_improved ~ "Improved Water Source",
                              kgc_course ~ "Koppen-Geiger Zone"), exponentiate = TRUE) %>% bold_labels()

model_e30 <- lmer(log(hv204) ~ e_totalminus30 + hv270 + kgc_course + hv236_person_recode + hv201_improved  +
                      (1|name_year/hv001), data = rural_final, REML = FALSE) %>% 
  tbl_regression(label = list(e_totalminus30 ~ "30 Day Evaporation", hv270 ~ "SES",
                              hv236_person_recode ~ "Person Carrying Water",
                              hv201_improved ~ "Improved Water Source",
                              kgc_course ~ "Koppen-Geiger Zone"), exponentiate = TRUE) %>% bold_labels()            

model_e60 <- lmer(log(hv204) ~ e_totalminus60 + hv270 + kgc_course + hv236_person_recode + hv201_improved +
                      (1|name_year/hv001), data = rural_final, REML = FALSE) %>% 
  tbl_regression(label = list(e_totalminus60 ~ "60 Day Evaporation", hv270 ~ "SES",
                              hv236_person_recode ~ "Person Carrying Water",
                              hv201_improved ~ "Improved Water Source",
                              kgc_course ~ "Koppen-Geiger Zone"), exponentiate = TRUE) %>% bold_labels()


evap_models <- tbl_merge(tbls = list(model_e7, model_e14, model_e30, model_e60),
                        tab_spanner = c("**Evaporation, 7**", "**Evaporation, 14**",
                                        "**Evaporation, 30**", "**Evaporation, 60**")) %>%
  modify_table_body(~.x %>%
                      dplyr::arrange(factor(var_label, levels =
                                              c("7 Day Evaporation", "14 Day Evaporation", "30 Day Evaporation", "60 Day Evaporation",
                                                "SES", "Koppen-Geiger Zone", "Improved Water Source", "Person Carrying Water"))))


evap_models <- evap_models %>% as_gt()


evap_models %>% 
  tab_style(
    style = cell_borders(
      sides = c("left", "right"),
      color = "gray80",
      weight = px(2),
      style = "solid"),
    locations = list(cells_body(),
                     cells_column_labels(),
                     cells_column_spanners()))

## Skin Temperature Model ----------------------------------------------------------------

model_skt7 <- lmer(log(hv204) ~ skt_avgminus7 + hv270 + kgc_course + hv236_person_recode + hv201_improved +
                   (1|name_year/hv001), data = rural_final, REML = FALSE) %>% 
  tbl_regression(label = list(skt_avgminus7 ~ "7 Day SKT Avg", hv270 ~ "SES",
                              hv236_person_recode ~ "Person Carrying Water",
                              hv201_improved ~ "Improved Water Source",
                              kgc_course ~ "Koppen-Geiger Zone"), exponentiate = TRUE) %>% bold_labels()

model_skt14 <- lmer(log(hv204) ~ skt_avgminus14 + hv270 + kgc_course + hv236_person_recode + hv201_improved +
                    (1|name_year/hv001), data = rural_final, REML = FALSE) %>% 
  tbl_regression(label = list(skt_avgminus14 ~ "14 Day SKT Avg", hv270 ~ "SES",
                              hv236_person_recode ~ "Person Carrying Water",
                              hv201_improved ~ "Improved Water Source",
                              kgc_course ~ "Koppen-Geiger Zone"), exponentiate = TRUE) %>% bold_labels()

model_skt30 <- lmer(log(hv204) ~ skt_avgminus30 + hv270 + kgc_course + hv236_person_recode + hv201_improved  +
                    (1|name_year/hv001), data = rural_final, REML = FALSE) %>% 
  tbl_regression(label = list(skt_avgminus30 ~ "30 Day SKT Avg", hv270 ~ "SES",
                              hv236_person_recode ~ "Person Carrying Water",
                              hv201_improved ~ "Improved Water Source",
                              kgc_course ~ "Koppen-Geiger Zone"), exponentiate = TRUE) %>% bold_labels()            

model_skt60 <- lmer(log(hv204) ~ skt_avgminus60 + hv270 + kgc_course + hv236_person_recode + hv201_improved +
                    (1|name_year/hv001), data = rural_final, REML = FALSE) %>% 
  tbl_regression(label = list(skt_avgminus60 ~ "60 Day SKT Avg", hv270 ~ "SES",
                              hv236_person_recode ~ "Person Carrying Water",
                              hv201_improved ~ "Improved Water Source",
                              kgc_course ~ "Koppen-Geiger Zone"), exponentiate = TRUE) %>% bold_labels()


skt_models <- tbl_merge(tbls = list(model_skt7, model_skt14, model_skt30, model_skt60),
                         tab_spanner = c("**Skin Temp, 7**", "**Skin Temp, 14**",
                                         "**Skin Temp, 30**", "**Skin Temp, 60**")) %>%
  modify_table_body(~.x %>%
                    dplyr::arrange(factor(var_label, levels =
                                            c("7 Day SKT Avg", "14 Day SKT Avg", "30 Day SKT Avg", "60 Day SKT Avg",
                                              "SES", "Koppen-Geiger Zone", "Improved Water Source", "Person Carrying Water"))))


skt_models <- skt_models %>% as_gt()


skt_models %>% 
  tab_style(
    style = cell_borders(
      sides = c("left", "right"),
      color = "gray80",
      weight = px(2),
      style = "solid"),
    locations = list(cells_body(),
                     cells_column_labels(),
                     cells_column_spanners()))

## Combined ----------------------------------------------------------------

rural_final <- rural_final %>% mutate(tp_totalminus7_cm = if_else(tp_totalminus7_cm < 0.00000006, 0, tp_totalminus7_cm)) %>% 
                               mutate(tp_totalminus7_cm = if_else(tp_totalminus7_cm > 16.41, 16.41, tp_totalminus7_cm)) %>% 
                               mutate(tp_totalminus7_cm = if_else(tp_totalminus7_cm == 0, 0.0001, tp_totalminus7_cm))

skim(rural_final$tp_totalminus7_cm)

quantile(rural_final$tp_totalminus7_cm, c(0.0001, 0.01, .75, .80, .85, .90, .95, .99, .999))

test <- rural_final %>% select(c(num_range("sro_", 1:7, suffix = "_cm"), sro_totalminus7_cm))

rural_final <- rural_final %>% mutate(sro_totalminus7_cm = if_else(sro_totalminus7_cm < 0, 0, sro_totalminus7_cm)) %>% 
                               mutate(sro_totalminus7_cm = if_else(sro_totalminus7_cm > 4.84, 4.84, sro_totalminus7_cm)) %>% 
                               mutate(sro_totalminus7_cm = if_else(sro_totalminus7_cm == 0, 0.0001, sro_totalminus7_cm))

rural_final <- rural_final %>% select(hv204, tp_totalminus7_cm, sro_totalminus7_cm, skt_avgminus7, hv270, kgc_course, 
                                      hv236_person_recode, hv201_improved, name_year, hv001)

model_tp7 <- lmer(log(hv204) ~ tp_totalminus7_cm + hv270 + kgc_course + hv236_person_recode + hv201_improved +
                    (1|name_year/hv001), data = rural_final, REML = FALSE)


model_tp7 %>% tbl_regression(label = list(tp_totalminus7_cm ~ "7 Day Precipitation", hv270 ~ "SES",
                                          hv236_person_recode ~ "Person Carrying Water",
                                          hv201_improved ~ "Improved Water Source",
                                          kgc_course ~ "Koppen-Geiger Zone"), exponentiate = TRUE) %>% bold_labels()

# Extract coefficients and exponentiate
coefs <- data.frame(coef(summary(model_tp7)))
coefs$exp_estimate <- exp(coefs$Estimate)  # Exponentiated coefficients

# Calculate p-values using normal distribution approximation
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))

# View results
coefs



model_sro7 <- lmer(log(hv204) ~ sro_totalminus7_cm + hv270 + kgc_course + hv236_person_recode + hv201_improved +
                     (1|name_year/hv001), data = rural_final, REML = FALSE) 

model_sro7 %>% 
  tbl_regression(label = list(sro_totalminus7_cm ~ "7 Day SRO", hv270 ~ "SES",
                              hv236_person_recode ~ "Person Carrying Water",
                              hv201_improved ~ "Improved Water Source",
                              kgc_course ~ "Koppen-Geiger Zone"), exponentiate = TRUE) %>% bold_labels()

# Extract coefficients and exponentiate
coefs <- data.frame(coef(summary(model_sro7)))
coefs$exp_estimate <- exp(coefs$Estimate)  # Exponentiated coefficients

# Calculate p-values using normal distribution approximation
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))

# View results
coefs


model_skt7 <- lmer(log(hv204) ~ skt_avgminus7 + hv270 + kgc_course + hv236_person_recode + hv201_improved +
                     (1|name_year/hv001), data = rural_final, REML = FALSE) %>% 
  tbl_regression(label = list(skt_avgminus7 ~ "7 Day Skin Temp", hv270 ~ "SES",
                              hv236_person_recode ~ "Person Carrying Water",
                              hv201_improved ~ "Improved Water Source",
                              kgc_course ~ "Koppen-Geiger Zone"), exponentiate = TRUE) %>% bold_labels()


# Extract coefficients and exponentiate
coefs <- data.frame(coef(summary(model_skt7)))
coefs$exp_estimate <- exp(coefs$Estimate)  # Exponentiated coefficients

# Calculate p-values using normal distribution approximation
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))

# View results
coefs


class(model_skt7)

combined_models <- tbl_merge(tbls = list(model_tp7, model_sro7, model_skt7),
                        tab_spanner = c("**Precipitation, 7**", "**SRO, 7**",
                                         "**Skin Temp, 7**")) %>%
  modify_table_body(~.x %>%
                      dplyr::arrange(factor(var_label, levels =
                                              c("7 Day Precipitation", "7 Day SRO", "7 Day Skin Temp",
                                                "SES", "Koppen-Geiger Zone", "Improved Water Source", "Person Carrying Water"))))


combined_models <- combined_models %>% as_gt()


for_export <- combined_models %>% 
  tab_style(
    style = cell_borders(
      sides = c("left", "right"),
      color = "gray80",
      weight = px(2),
      style = "solid"),
    locations = list(cells_body(),
                     cells_column_labels(),
                     cells_column_spanners()))

model_skt7 %>%
  gtsummary::as_tibble() %>% 
  writexl::write_xlsx(., "model_skt7.xlsx")

model_tp7 %>%
  gtsummary::as_tibble() %>% 
  writexl::write_xlsx(., "model_tp7.xlsx")

model_sro7 %>%
  gtsummary::as_tibble() %>% 
  writexl::write_xlsx(., "model_sro7.xlsx")




## Combined Surface Water Only ----------------------------------------------------------------
# Split the dataset into surface water and non-surface water

surface <- rural_final %>% filter(hv201_sourcecat == "Surface water/River")
nonsurface <- rural_final %>% filter(!hv201_sourcecat == "Surface water/River")



model_tp7 <- lmer(log(hv204) ~ tp_totalminus7 + hv270 + kgc_course + hv236_person_recode +
                    (1|name_year/hv001), data = surface, REML = FALSE) %>% 
  tbl_regression(label = list(tp_totalminus7 ~ "7 Day Precipitation", hv270 ~ "SES",
                              hv236_person_recode ~ "Person Carrying Water",
                              
                              kgc_course ~ "Koppen-Geiger Zone"), exponentiate = TRUE) %>% bold_labels()

model_sro7 <- lmer(log(hv204) ~ sro_totalminus7 + hv270 + kgc_course + hv236_person_recode + 
                     (1|name_year/hv001), data = surface, REML = FALSE) %>% 
  tbl_regression(label = list(sro_totalminus7 ~ "7 Day SRO", hv270 ~ "SES",
                              hv236_person_recode ~ "Person Carrying Water",
                              
                              kgc_course ~ "Koppen-Geiger Zone"), exponentiate = TRUE) %>% bold_labels()

model_e7 <- lmer(log(hv204) ~ e_totalminus7 + hv270 + kgc_course + hv236_person_recode + 
                   (1|name_year/hv001), data = surface, REML = FALSE) %>% 
  tbl_regression(label = list(e_totalminus7 ~ "7 Day Evaporation", hv270 ~ "SES",
                              hv236_person_recode ~ "Person Carrying Water",
                              
                              kgc_course ~ "Koppen-Geiger Zone"), exponentiate = TRUE) %>% bold_labels()

model_skt7 <- lmer(log(hv204) ~ skt_avgminus7 + hv270 + kgc_course + hv236_person_recode +
                     (1|name_year/hv001), data = surface, REML = FALSE) %>% 
  tbl_regression(label = list(skt_avgminus7 ~ "7 Day Skin Temp", hv270 ~ "SES",
                              hv236_person_recode ~ "Person Carrying Water",
                              
                              kgc_course ~ "Koppen-Geiger Zone"), exponentiate = TRUE) %>% bold_labels()

combined_models <- tbl_merge(tbls = list(model_tp7, model_sro7, model_e7, model_skt7),
                             tab_spanner = c("**Precipitation, 7**", "**SRO, 7**",
                                             "**Evap, 7**", "**Skin Temp, 7**")) %>%
  modify_table_body(~.x %>%
                      dplyr::arrange(factor(var_label, levels =
                                              c("7 Day Precipitation", "7 Day SRO", "7 Day Evaporation", "7 Day Skin Temp",
                                                "SES", "Koppen-Geiger Zone", "Person Carrying Water"))))


combined_models <- combined_models %>% as_gt()


combined_models %>% 
  tab_style(
    style = cell_borders(
      sides = c("left", "right"),
      color = "gray80",
      weight = px(2),
      style = "solid"),
    locations = list(cells_body(),
                     cells_column_labels(),
                     cells_column_spanners()))









model_tp7 <- lmer(log(hv204) ~ tp_totalminus7 + hv270 + kgc_course + hv236_person_recode +
                    (1|name_year/hv001), data = nonsurface, REML = FALSE) %>% 
  tbl_regression(label = list(tp_totalminus7 ~ "7 Day Precipitation", hv270 ~ "SES",
                              hv236_person_recode ~ "Person Carrying Water",
                              
                              kgc_course ~ "Koppen-Geiger Zone"), exponentiate = TRUE) %>% bold_labels()

model_sro7 <- lmer(log(hv204) ~ sro_totalminus7 + hv270 + kgc_course + hv236_person_recode + 
                     (1|name_year/hv001), data = nonsurface, REML = FALSE) %>% 
  tbl_regression(label = list(sro_totalminus7 ~ "7 Day SRO", hv270 ~ "SES",
                              hv236_person_recode ~ "Person Carrying Water",
                              
                              kgc_course ~ "Koppen-Geiger Zone"), exponentiate = TRUE) %>% bold_labels()

model_e7 <- lmer(log(hv204) ~ e_totalminus7 + hv270 + kgc_course + hv236_person_recode + 
                   (1|name_year/hv001), data = nonsurface, REML = FALSE) %>% 
  tbl_regression(label = list(e_totalminus7 ~ "7 Day Evaporation", hv270 ~ "SES",
                              hv236_person_recode ~ "Person Carrying Water",
                              
                              kgc_course ~ "Koppen-Geiger Zone"), exponentiate = TRUE) %>% bold_labels()

model_skt7 <- lmer(log(hv204) ~ skt_avgminus7 + hv270 + kgc_course + hv236_person_recode +
                     (1|name_year/hv001), data = nonsurface, REML = FALSE) %>% 
  tbl_regression(label = list(skt_avgminus7 ~ "7 Day Skin Temp", hv270 ~ "SES",
                              hv236_person_recode ~ "Person Carrying Water",
                              
                              kgc_course ~ "Koppen-Geiger Zone"), exponentiate = TRUE) %>% bold_labels()

combined_models <- tbl_merge(tbls = list(model_tp7, model_sro7, model_e7, model_skt7),
                             tab_spanner = c("**Precipitation, 7**", "**SRO, 7**",
                                             "**Evap, 7**", "**Skin Temp, 7**")) %>%
  modify_table_body(~.x %>%
                      dplyr::arrange(factor(var_label, levels =
                                              c("7 Day Precipitation", "7 Day SRO", "7 Day Evaporation", "7 Day Skin Temp",
                                                "SES", "Koppen-Geiger Zone", "Person Carrying Water"))))


combined_models <- combined_models %>% as_gt()


combined_models %>% 
  tab_style(
    style = cell_borders(
      sides = c("left", "right"),
      color = "gray80",
      weight = px(2),
      style = "solid"),
    locations = list(cells_body(),
                     cells_column_labels(),
                     cells_column_spanners()))
