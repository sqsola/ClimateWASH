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

# Read in the rural datasets
rural <- readRDS("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/rural.rds")
under5_dia <- readRDS("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/under5_dia.rds")


# Venn Diagram ------------------------------------------------------------

lala <- rural %>% select(bull_cow_present,       cattle_present  ,       
                         chicken_poultry_present, horse_donkey_present)

test <- lala %>% mutate(across(contains("present"), as.logical))


test <- test %>%
  rename(
    'Bull/Cow' = bull_cow_present,
    Cattle = cattle_present,
    'Chicken/Poultry' = chicken_poultry_present,
    'Horse/Donkey' = horse_donkey_present
  )

ggvenn(test, c('Bull/Cow', "Cattle"  ,       
               'Chicken/Poultry', 'Horse/Donkey'))


# Summarise the diarrhea by any animals -----------------------------------
data %>% 
  group_by(animal_any) %>% 
  summarise(n = n(),
            n_under5 = sum(b8 <= 5, na.rm = TRUE),
            avg_num_animal = round(mean(animal_total, na.rm = TRUE), 2),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4)*100) %>%
  adorn_totals("row",,,,c(n, n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(animal_any = "Any Animals", n = "Number of People", n_under5 = "Number Under 5",
                    avg_num_animal = "Avg # Animals", num_diarrhea = "Under 5 w/ Dia",
                    percent_diarrhea = "% Diarrhea")%>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

# Summarise the diarrhea by any animals
data %>% 
  group_by(diarrhea_dichot) %>% 
  summarise(n = n(),
            n_under5 = sum(b8 <= 5, na.rm = TRUE),
            avg_num_animal = round(mean(animal_total, na.rm = TRUE), 2)) %>%
  adorn_totals("row",,,,c(n, n_under5)) %>% 
  qflextable() %>% 
  set_header_labels(diarrhea_dichot = "Any Diarrhea", n = "Number of People", n_under5 = "Number Under 5",
                    avg_num_animal = "Avg # Animals")%>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)

             
# Box plot for any animals
ggplot(data = data, mapping = aes(
  x = diarrhea_dichot,
  y = log(animal_total),
  group = diarrhea_dichot)) + 
  geom_boxplot() +
  ylim(0, 5) +
  theme_classic() + 
  xlab("Diarrhea Status") +
  ylab("LOG(Total Number of Animals)") +
  scale_fill_manual(values = c("brown", "lightblue"),
                    name="Diarrhea Status") +  # Corrected the legend title
  ggtitle("Boxplot of Total Number of Animals by Diarrhea Status") +  # Corrected the plot title
  theme(plot.title = element_text(hjust = 0.5))


# Summarise the diarrhea by total animals
data %>% 
  group_by(animal_total_category) %>% 
  summarise(n = n(),
            n_under5 = sum(b8 <= 5, na.rm = TRUE),
            avg_num_animal = round(mean(animal_total, na.rm = TRUE), 2),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4)*100) %>%
  adorn_totals("row",,,,c(n, n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(animal_total_category = "# Animal Category", n = "Number of People", n_under5 = "Number Under 5",
                    avg_num_animal = "Avg # Animals", num_diarrhea = "Under 5 w/ Dia",
                    percent_diarrhea = "% Diarrhea")%>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)


# Summarise the diarrhea by number of different animals exposed to
data %>% 
  group_by(animal_combo) %>% 
  summarise(n = n(),
            n_under5 = sum(b8 <= 5, na.rm = TRUE),
            avg_num_animal = round(mean(animal_total, na.rm = TRUE), 2),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4)*100) %>%
  adorn_totals("row",,,,c(n, n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(animal_combo = "Animal Combos", n = "Number of People", n_under5 = "Number Under 5",
                    avg_num_animal = "Avg # Animals", num_diarrhea = "Under 5 w/ Dia",
                    percent_diarrhea = "% Diarrhea")%>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)



# KGC ---------------------------------------------------------------

# Summarise the diarrhea by kgz
data %>% 
  group_by(kgc_course) %>% 
  summarise(n = n(),
            n_under5 = sum(b8 <= 5, na.rm = TRUE),
            avg_num_animal = round(mean(animal_total, na.rm = TRUE), 2),
            num_diarrhea = sum(diarrhea_dichot, na.rm = TRUE),
            percent_diarrhea = round((num_diarrhea / n_under5), 4)*100) %>%
  adorn_totals("row",,,,c(n, n_under5, num_diarrhea)) %>% 
  qflextable() %>% 
  set_header_labels(kgc_course = "KGZ", n = "Number of People", n_under5 = "Number Under 5",
                    avg_num_animal = "Avg # Animals", num_diarrhea = "Under 5 w/ Dia",
                    percent_diarrhea = "% Diarrhea")%>% 
  theme_zebra() %>% theme_box() %>% 
  align_nottext_col(align = "center", header = TRUE) %>%
  align_text_col(align = "center", header = TRUE)




# Check the animals-------------------------------------------------------

rural_final <- readRDS("~/data-mdavis65/steven_sola/0_Scripts/ClimateWASH/Aim 2/rural_final.Rdata")




exp(cbind(OR = coef(mylogit), confint(mylogit)))

paste0(exp(coef(mylogit)), "(", exp(confint(mylogit)), ")")


rural_final %>% tabyl(h11)


rural_final <- rural_final %>%
  mutate(diarrhea_cattle = case_when(animal_cattle_present == 0 ~ NA_integer_,
                                     animal_cattle_present == 1 & (h11 == 1 | h11 == 2) ~ 1, 
                                     TRUE ~ 0)) %>% 
  mutate(diarrhea_cow = case_when(animal_cow_present == 0 ~ NA_integer_,
                                  animal_cow_present == 1 & (h11 == 1 | h11 == 2) ~ 1, 
                                     TRUE ~ 0)) %>% 
  mutate(diarrhea_horse = case_when(animal_horse_present == 0 ~ NA_integer_,
                                    animal_horse_present == 1 & (h11 == 1 | h11 == 2) ~ 1, 
                                     TRUE ~ 0)) %>% 
  mutate(diarrhea_goat = case_when(animal_goat_present == 0 ~ NA_integer_,
                                   animal_goat_present == 1 & (h11 == 1 | h11 == 2) ~ 1, 
                                     TRUE ~ 0)) %>% 
  mutate(diarrhea_sheep = case_when(animal_sheep_present == 0 ~ NA_integer_,
                                     animal_sheep_present == 1 & (h11 == 1 | h11 == 2) ~ 1, 
                                     TRUE ~ 0)) %>% 
  mutate(diarrhea_chicken = case_when(animal_chicken_present == 0 ~ NA_integer_,
                                      animal_chicken_present == 1 & (h11 == 1 | h11 == 2) ~ 1, 
                                     TRUE ~ 0))

rural_final %>% tabyl(animal_cattle_present)
rural_final %>% tabyl(animal_cow_present)
rural_final %>% tabyl(animal_horse_present)
rural_final %>% tabyl(animal_goat_present)
rural_final %>% tabyl(animal_sheep_present)
rural_final %>% tabyl(animal_chicken_present)


logit_cattle <- glm(diarrhea_dichot ~ animal_cattle_present, data = rural_final, family = "binomial")
exp(cbind(OR = coef(logit_cattle), confint(logit_cattle)))

rural_final %>%
  summarise(num_children = n(),
            num_cattle = sum(animal_cattle_present),
            dia_yes = sum(diarrhea_cattle == 1, na.rm = TRUE),
            dia_no = sum(diarrhea_cattle == 0, na.rm = TRUE),
            percent_dia = round((dia_yes/dia_no)*100, digits = 1),
            or = paste0("0.83 (0.81, 0.85)"))%>% 
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




# Correlation Graphs ----------------------------------------------------------------

# Variable to subset
weather_var <- c("e_totalminus7",    "e_totalminus14",    "e_totalminus30",    "e_totalminus60",
                 "tp_totalminus7",   "tp_totalminus14",   "tp_totalminus30",   "tp_totalminus60",
                 "sro_totalminus7",  "sro_totalminus14",  "sro_totalminus30",  "sro_totalminus60",
                 "ssro_totalminus7", "ssro_totalminus14", "ssro_totalminus30", "ssro_totalminus60",
                 "t2m_avgminus7",    "t2m_avgminus14",    "t2m_avgminus30",    "t2m_avgminus60",
                 "skt_avgminus7",    "skt_avgminus14",    "skt_avgminus30",    "skt_avgminus60",
                 "lai_lv_avgminus7", "lai_lv_avgminus14", "lai_lv_avgminus30", "lai_lv_avgminus60",
                 "lai_hv_avgminus7", "lai_hv_avgminus14", "lai_hv_avgminus30", "lai_hv_avgminus60",
                 "d2m_avgminus7",    "d2m_avgminus14",    "d2m_avgminus30",    "d2m_avgminus60")


# Correlation for rural data
subset_rural <- rural_final %>% select(h11, all_of(weather_var))

cor_rural_focus <- subset_rural %>% 
  correlate() %>% 
  focus(hv204) %>% 
  arrange(hv204)

cor_rural_focus <- cor_rural_focus %>%
  mutate(color = case_when(
    grepl("e", term) ~ "#e41a1c",
    grepl("^sro$", term) ~ "#377eb8",
    grepl("ssro", term) ~ "#4daf4a",
    grepl("t2m", term) ~ "#984ea3",
    grepl("tp", term) ~ "#ff7f00",
    grepl("skt", term) ~ "#2b888f",
    grepl("lai_lv", term) ~ "#a65628",
    grepl("lai_hv", term) ~ "#f781bf",
    grepl("d2m", term) ~ "#999999",
    TRUE ~ "#377eb8"
  ))

color_chart <- cor_rural_focus$color

cor_rural_focus %>% mutate(term = factor(term, levels = term[order(hv204)])) %>% 
  mutate(color = ifelse(hv204 < 0, "negative", "positive")) %>% 
  ggplot(aes(x = term, y = hv204)) + 
  geom_bar(stat = "identity", position = "identity", aes(fill = color), color = color_chart, linewidth = 1, alpha = 0.7) +
  ylab("Correlation with Water Walk Time (HV204)") + 
  xlab("Variable") +
  theme_bw() +
  scale_fill_manual(values = c(positive = "royalblue3", negative = "firebrick1"), guide = "none")+
  theme(axis.text.x = element_text(angle = 55, hjust = 1, colour = color_chart, face = "bold", size = 11))

# Corr Plot
subset_rural <- rural_final %>% select(all_of(weather_var))

cor_rural_weather <-  cor(subset_rural, use = "complete.obs")

testRes <- cor.mtest(cor_rural_weather, conf.level = 0.95)

labelCol <-  c("purple", "purple", "purple", "purple", "black", "black", "black", "black")

corrplot(cor_rural_weather, p.mat = testRes$p, method = "square", order = "alphabet",
         diag = TRUE, type = "lower", sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
         insig = "label_sig", pch.col = "grey1", tl.col = labelCol, tl.srt = 45)






















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


rural_final %>%
  group_by(hv007) %>% 
  summarise(num_children = n(),
            exposed = sum(animal_cow_present),
            percent_exp = round((exposed/num_children)*100, digits = 1), 
            dia_yes = sum(diarrhea_cow == 1, na.rm = TRUE),
            dia_no = sum(diarrhea_cow == 0, na.rm = TRUE),
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

rural_final %>%
  group_by(hv007) %>% 
  summarise(num_children = n(),
            exposed = sum(animal_horse_present),
            percent_exp = round((exposed/num_children)*100, digits = 1), 
            dia_yes = sum(diarrhea_horse == 1, na.rm = TRUE),
            dia_no = sum(diarrhea_horse == 0, na.rm = TRUE),
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

rural_final %>%
  group_by(hv007) %>% 
  summarise(num_children = n(),
            exposed = sum(animal_goat_present),
            percent_exp = round((exposed/num_children)*100, digits = 1), 
            dia_yes = sum(diarrhea_goat == 1, na.rm = TRUE),
            dia_no = sum(diarrhea_goat == 0, na.rm = TRUE),
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

rural_final %>%
  group_by(hv007) %>% 
  summarise(num_children = n(),
            exposed = sum(animal_sheep_present),
            percent_exp = round((exposed/num_children)*100, digits = 1), 
            dia_yes = sum(diarrhea_sheep == 1, na.rm = TRUE),
            dia_no = sum(diarrhea_sheep == 0, na.rm = TRUE),
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

rural_final %>%
  group_by(hv007) %>% 
  summarise(num_children = n(),
            exposed = sum(animal_chicken_present),
            percent_exp = round((exposed/num_children)*100, digits = 1), 
            dia_yes = sum(diarrhea_chicken == 1, na.rm = TRUE),
            dia_no = sum(diarrhea_chicken == 0, na.rm = TRUE),
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
