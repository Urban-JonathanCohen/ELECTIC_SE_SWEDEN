library(ggplot2)
library(survey)
library(dplyr)
library(tidyr)


# rm(HH_DB, IN_DB)
 gc()
# 
# hh_clean$a_food_n_alcohol[is.na(hh_clean$a_food_n_alcohol)] <- 0
# hh_clean$b_cloth_n_foot[is.na(hh_clean$b_cloth_n_foot)] <- 0
# hh_clean$c_furniture_mantain_home[is.na(hh_clean$c_furniture_mantain_home)] <- 0
# hh_clean$d_transport[is.na(hh_clean$d_transport)] <- 0
# hh_clean$e_ict[is.na(hh_clean$e_ict)] <- 0
# hh_clean$f_recreation[is.na(hh_clean$f_recreation)] <- 0
# hh_clean$g_personal_care[is.na(hh_clean$g_personal_care)] <- 0


########################################################################
colnames(hh_clean)
hh_clean$tot_consumption <- ( hh_clean$a_food_n_alcohol +
                                hh_clean$b_cloth_n_foot+
                                hh_clean$c_furniture_mantain_home +
                                hh_clean$d_transport +
                                hh_clean$e_ict +
                                hh_clean$f_recreation +
                                hh_clean$g_personal_care +
                                hh_clean$e_ict_BUT+
                                hh_clean$c_furniture_mantain_home_BUT)


hh_clean <- hh_clean %>%
  filter(!is.na(tot_consumption))




quantile_95 <- quantile(hh_clean$tot_consumption, 0.95)
quantile_05 <- quantile(hh_clean$tot_consumption, 0.05)


hh_clean <- hh_clean %>% filter(
  tot_consumption <= quantile_95 &
    tot_consumption >= quantile_05 &
    tot_consumption >=0
  
  )

hh_clean$tot_consumption_pc <- hh_clean$tot_consumption/hh_clean$total_members



hh_clean <- hh_clean %>%
  mutate(d_income_decile = ntile(disposable_inc, 10))


hh_clean <- hh_clean %>%
  mutate(expenditure_decile = ntile(total_expenditure, 10))

hh_clean <- hh_clean %>%
  mutate(consume_decile = ntile(tot_consumption, 10))


hh_clean <- hh_clean %>%
  mutate(d_income_decile5 = ntile(disposable_inc, 5))




means_by_decile_base <- aggregate(
  cbind(
    a_food_n_alcohol,
    b_cloth_n_foot,
    c_furniture_mantain_home,
    d_transport,
    e_ict,
    f_recreation,
    g_personal_care,
    e_ict_BUT,
    c_furniture_mantain_home_BUT
  ) ~ d_income_decile,
  data = hh_clean,
  FUN = mean,
  na.rm = TRUE
)
means_by_decile_base_tot <- means_by_decile_base


means_by_decile_base_tot$tot <- means_by_decile_base$a_food_n_alcohol+
  means_by_decile_base$b_cloth_n_foot +
  means_by_decile_base$c_furniture_mantain_home +
  means_by_decile_base$d_transport +
  means_by_decile_base$e_ict +
  means_by_decile_base$f_recreation +
  means_by_decile_base$g_personal_care +
  means_by_decile_base$e_ict_BUT+
  means_by_decile_base$c_furniture_mantain_home_BUT


means_by_decile_base_tot_z <- means_by_decile_base_tot %>%
  mutate(
    perc_a_food_n_alcohol = (a_food_n_alcohol / tot) * 100,
    perc_b_cloth_n_foot = (b_cloth_n_foot / tot) * 100,
    perc_c_furniture_mantain_home = ((c_furniture_mantain_home +c_furniture_mantain_home_BUT) / tot) * 100,
    perc_d_transport = (d_transport / tot) * 100,
    perc_e_ict = ((e_ict + e_ict_BUT)/ tot) * 100,
    perc_f_recreation = (f_recreation / tot) * 100,
    perc_g_personal_care = (g_personal_care / tot) * 100
    
  )

# Select only the relevant columns to display
means_by_decile_base_tot_z <- means_by_decile_base_tot_z %>%
  select(d_income_decile, starts_with("perc_"))



means_by_decile_base_long <- means_by_decile_base %>%
  pivot_longer(
    cols = c(a_food_n_alcohol, b_cloth_n_foot, c_furniture_mantain_home, 
             d_transport, e_ict, f_recreation, g_personal_care),
    names_to = "consumption_category",
    values_to = "amount")


means_by_decile_base_tot_z_long <- means_by_decile_base_tot_z %>%
  pivot_longer(
    cols = c(perc_a_food_n_alcohol , perc_b_cloth_n_foot , perc_c_furniture_mantain_home , 
             perc_d_transport , perc_e_ict , perc_f_recreation , perc_g_personal_care),
    names_to = "consumption_category",
    values_to = "amount")





################################################

# hh_clean$CE_repair_tool[is.na(hh_clean$CE_repair_tool)] <- 0
# hh_clean$CE_repair_glass[is.na(hh_clean$CE_repair_glass)] <- 0
# hh_clean$CE_repair_white[is.na(hh_clean$CE_repair_white)] <- 0
# hh_clean$CE_repair_textiles[is.na(hh_clean$CE_repair_textiles)] <- 0
# hh_clean$CE_repair_furniture[is.na(hh_clean$CE_repair_furniture)] <- 0
# 
# 
# 
# hh_clean$tot_repair <- ( hh_clean$CE_repair_tool +
#                            hh_clean$CE_repair_glass+
#                            hh_clean$CE_repair_white +
#                            hh_clean$CE_repair_textiles +
#                            hh_clean$CE_repair_furniture )
# 
# 
# 
# 
# quantile_95_r <- quantile(hh_clean$tot_repair, 0.95)
# quantile_01_r <- quantile(hh_clean$tot_repair, 0.01)
# 
# 
# hh_clean <- hh_clean %>% filter(
#   tot_repair <= quantile_95_r &
#     tot_repair >=0
#   
# )
# 




#####################################################################################



hh_clean$a_food_pc <-     hh_clean$a_food_n_alcohol/hh_clean$total_members
hh_clean$b_cloth_pc <-    hh_clean$b_cloth_n_foot/hh_clean$total_members


hh_clean$c_furniture_pc <-    (hh_clean$c_furniture_mantain_home + hh_clean$c_furniture_mantain_home)/hh_clean$total_members


hh_clean$d_transport_pc <-   hh_clean$d_transport/hh_clean$total_members
hh_clean$e_ict_pc <-     (hh_clean$e_ict + hh_clean$e_ict_BUT)/hh_clean$total_members


hh_clean$f_recreation_pc  <-   hh_clean$f_recreation/hh_clean$total_members
hh_clean$g_personal_pc <-     hh_clean$g_personal_care/hh_clean$total_members

hh_clean$disposable_inc_pc <-     hh_clean$disposable_inc/hh_clean$total_members
hh_clean$total_expenditure_pc <-     hh_clean$total_expenditure/hh_clean$total_members
hh_clean$tot_consumption_pc <-     hh_clean$tot_consumption/hh_clean$total_members

describe(hh_clean$disposable_inc_pc)



hh_clean <- hh_clean %>%
  mutate(d_income_decile_pc = ntile(disposable_inc_pc, 10))


hh_clean <- hh_clean %>%
  mutate(expenditure_decile = ntile(total_expenditure_pc, 10))

hh_clean <- hh_clean %>%
  mutate(consume_decile = ntile(tot_consumption_pc, 10))






means_by_decile_base_pc <- aggregate(
  cbind(
    a_food_pc,
    b_cloth_pc,
    c_furniture_pc,
    d_transport_pc,
    e_ict_pc,
    f_recreation_pc,
    g_personal_pc
  ) ~ d_income_decile_pc,
  data = hh_clean,
  FUN = mean,
  na.rm = TRUE
)

means_by_decile_base_tot_pc <- means_by_decile_base_pc

means_by_decile_base_tot

means_by_decile_base_tot_pc$tot_pc <- means_by_decile_base_tot_pc$a_food_pc+
  means_by_decile_base_tot_pc$b_cloth_pc +
  means_by_decile_base_tot_pc$c_furniture_pc +
  means_by_decile_base_tot_pc$d_transport_pc +
  means_by_decile_base_tot_pc$e_ict_pc +
  means_by_decile_base_tot_pc$f_recreation_pc +
  means_by_decile_base_tot_pc$g_personal_pc


means_by_decile_base_tot_z_pc <- means_by_decile_base_tot_pc %>%
  mutate(
    perc_a_food_n_alcohol = (a_food_pc / tot_pc) * 100,
    perc_b_cloth_n_foot = (b_cloth_pc / tot_pc) * 100,
    perc_c_furniture_mantain_home = (c_furniture_pc / tot_pc) * 100,
    perc_d_transport = (d_transport_pc / tot_pc) * 100,
    perc_e_ict = (e_ict_pc / tot_pc) * 100,
    perc_f_recreation = (f_recreation_pc / tot_pc) * 100,
    perc_g_personal_care = (g_personal_pc / tot_pc) * 100
  )


# Select only the relevant columns to display
means_by_decile_base_tot_z_pc <- means_by_decile_base_tot_z_pc %>%
  dplyr::select(d_income_decile_pc, starts_with("perc_"))



means_by_decile_base_long_pc <- means_by_decile_base_pc %>%
  pivot_longer(
    cols = c(a_food_pc , b_cloth_pc , c_furniture_pc , 
             d_transport_pc , e_ict_pc  , f_recreation_pc , g_personal_pc),
    names_to = "consumption_category",
    values_to = "amount")


means_by_decile_base_tot_z_long_pc <- means_by_decile_base_tot_z_pc %>%
  pivot_longer(
    cols = c(perc_a_food_n_alcohol , perc_b_cloth_n_foot , perc_c_furniture_mantain_home , 
             perc_d_transport , perc_e_ict , perc_f_recreation , perc_g_personal_care),
    names_to = "consumption_category",
    values_to = "amount")



##################################################
#decile 5

means_by_decile_base5 <- aggregate(
  cbind(
    a_food_n_alcohol,
    b_cloth_n_foot,
    c_furniture_mantain_home,
    d_transport,
    e_ict,
    f_recreation,
    g_personal_care,
    e_ict_BUT,
    c_furniture_mantain_home_BUT
    
  ) ~ d_income_decile5,
  data = hh_clean,
  FUN = mean,
  na.rm = TRUE
)
means_by_decile_base_tot5 <- means_by_decile_base5


means_by_decile_base_tot5$tot <- means_by_decile_base5$a_food_n_alcohol+
  means_by_decile_base5$b_cloth_n_foot +
  means_by_decile_base5$c_furniture_mantain_home + means_by_decile_base5$c_furniture_mantain_home_BUT+
  means_by_decile_base5$d_transport +
  means_by_decile_base5$e_ict +  means_by_decile_base5$e_ict_BUT+
  means_by_decile_base5$f_recreation +
  means_by_decile_base5$g_personal_care


means_by_decile_base_tot_z5 <- means_by_decile_base_tot5 %>%
  mutate(
    perc_a_food_n_alcohol = (a_food_n_alcohol / tot) * 100,
    perc_b_cloth_n_foot = (b_cloth_n_foot / tot) * 100,
    perc_c_furniture_mantain_home = ((c_furniture_mantain_home  + c_furniture_mantain_home_BUT)/ tot) * 100,
    perc_d_transport = (d_transport / tot) * 100,
    perc_e_ict = ((e_ict + e_ict_BUT) / tot) * 100,
    perc_f_recreation = (f_recreation / tot) * 100,
    perc_g_personal_care = (g_personal_care / tot) * 100
  )

# Select only the relevant columns to display
means_by_decile_base_tot_z5 <- means_by_decile_base_tot_z5 %>%
  select(d_income_decile5, starts_with("perc_"))



means_by_decile_base_long5 <- means_by_decile_base5 %>%
  pivot_longer(
    cols = c(a_food_n_alcohol, b_cloth_n_foot, c_furniture_mantain_home, 
             d_transport, e_ict, f_recreation, g_personal_care),
    names_to = "consumption_category",
    values_to = "amount")

means_by_decile_base_tot_z_long_pc
means_by_decile_base_tot_z_long5 <- means_by_decile_base_tot_z5 %>%
  pivot_longer(
    cols = c(perc_a_food_n_alcohol , perc_b_cloth_n_foot , perc_c_furniture_mantain_home , 
             perc_d_transport , perc_e_ict , perc_f_recreation , perc_g_personal_care),
    names_to = "consumption_category",
    values_to = "amount")


########################################################################
# pc 5

hh_clean <- hh_clean %>%
  mutate(d_income_decile_pc5 = ntile(disposable_inc_pc, 5))


hh_clean <- hh_clean %>%
  mutate(expenditure_decile5 = ntile(total_expenditure_pc, 5))

hh_clean <- hh_clean %>%
  mutate(consume_decile5 = ntile(tot_consumption_pc, 5))






means_by_decile_base_pc5 <- aggregate(
  cbind(
    a_food_pc,
    b_cloth_pc,
    c_furniture_pc,
    d_transport_pc,
    e_ict_pc,
    f_recreation_pc,
    g_personal_pc
  ) ~ d_income_decile_pc5,
  data = hh_clean,
  FUN = mean,
  na.rm = TRUE
)

means_by_decile_base_tot_pc5 <- means_by_decile_base_pc5

means_by_decile_base_tot5

means_by_decile_base_tot_pc5$tot_pc <- 
  means_by_decile_base_tot_pc5$a_food_pc+
  means_by_decile_base_tot_pc5$b_cloth_pc +
  means_by_decile_base_tot_pc5$c_furniture_pc +
  means_by_decile_base_tot_pc5$d_transport_pc +
  means_by_decile_base_tot_pc5$e_ict_pc +
  means_by_decile_base_tot_pc5$f_recreation_pc +
  means_by_decile_base_tot_pc5$g_personal_pc


means_by_decile_base_tot_z_pc5 <- means_by_decile_base_tot_pc5 %>%
  mutate(
    perc_a_food_n_alcohol = (a_food_pc / tot_pc) * 100,
    perc_b_cloth_n_foot = (b_cloth_pc / tot_pc) * 100,
    perc_c_furniture_mantain_home = (c_furniture_pc / tot_pc) * 100,
    perc_d_transport = (d_transport_pc / tot_pc) * 100,
    perc_e_ict = (e_ict_pc / tot_pc) * 100,
    perc_f_recreation = (f_recreation_pc / tot_pc) * 100,
    perc_g_personal_care = (g_personal_pc / tot_pc) * 100
  )


# Select only the relevant columns to display
means_by_decile_base_tot_z_pc5 <- means_by_decile_base_tot_z_pc5 %>%
  dplyr::select(d_income_decile_pc5, starts_with("perc_"))



means_by_decile_base_long_pc5 <- means_by_decile_base_pc5 %>%
  pivot_longer(
    cols = c(a_food_pc , b_cloth_pc , c_furniture_pc , 
             d_transport_pc , e_ict_pc  , f_recreation_pc , g_personal_pc),
    names_to = "consumption_category",
    values_to = "amount")


means_by_decile_base_tot_z_long_pc5 <- means_by_decile_base_tot_z_pc5 %>%
  pivot_longer(
    cols = c(perc_a_food_n_alcohol , perc_b_cloth_n_foot , perc_c_furniture_mantain_home , 
             perc_d_transport , perc_e_ict , perc_f_recreation , perc_g_personal_care),
    names_to = "consumption_category",
    values_to = "amount")




###############


  ###############################################
# Plots
##################



ggplot(hh_clean, aes(x = as.factor(d_income_decile), y = tot_consumption)) +
  geom_boxplot(outlier.shape = NA, fill = "grey", color = "black", alpha = 0.7) +  # Box plot without outliers
  scale_y_continuous(labels = scales::label_number(scale = 1/10000, suffix = "0k SEK")) +
  labs(x = "Disposable Income Decile", 
       y = "Total Consumption (in 10,000 SEK)", title = "Total consumption by disposable income Decile") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the plot title








# ggplot(hh_clean, aes(x = as.factor(d_income_decile), y = tot_consumption)) +
#   geom_jitter(width = 0.2, color = "black", alpha = 0.5) +  # Jittered points
#   geom_boxplot(outlier.shape = NA, fill = "grey", color = "black", alpha = 0.7) +  # Box plot without outliers
#   scale_y_continuous(labels = scales::label_number(scale = 1/10000, suffix = "0k SEK")) +
#   labs(x = "Disposable Income Decile", 
#        y = "Total Consumption (in 10,000 SEK)", title = "Total consumption by disposable income Decile") +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5))  # Center the plot title
# 
# 




ggplot(hh_clean, aes(x = as.factor(d_income_decile5), y = tot_consumption)) +
  #geom_jitter(width = 0.2, color = "black", alpha = 0.5) +  # Jittered points
  geom_boxplot(outlier.shape = NA, fill = "grey", color = "black", alpha = 0.7) +  # Box plot without outliers
  scale_y_continuous(labels = scales::label_number(scale = 1/10000, suffix = "0k SEK")) +
  labs(x = "Disposable Income Decile", 
       y = "Total Consumption (in 10,000 SEK)", title = "Total consumption by disposable income Decile") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the plot title


ggplot(hh_clean, aes(x = as.factor(d_income_decile), y = tot_consumption_pc)) +
  #geom_jitter(width = 0.2, color = "black", alpha = 0.5) +  # Jittered points
  geom_boxplot(outlier.shape = NA, fill = "grey", color = "black", alpha = 0.7) +  # Box plot without outliers
  scale_y_continuous(labels = scales::label_number(scale = 1/10000, suffix = "0k SEK")) +
  labs(x = "Disposable Income Decile", 
       y = "Total Consumption (in 10,000 SEK) pc", title = "Total consumption per capita by disposable income Decile") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the plot title


ggplot(hh_clean, aes(x = as.factor(hh_gender), y = tot_consumption)) +
  #geom_jitter(width = 0.2, color = "black", alpha = 0.5) +  # Jittered points
  geom_boxplot(outlier.shape = NA, fill = "grey", color = "black", alpha = 0.7) +  # Box plot without outliers
  scale_y_continuous(labels = scales::label_number(scale = 1/10000, suffix = "0k SEK")) +
  labs(x = "Disposable Income Decile", 
       y = "Total Consumption (in 10,000 SEK) pc", title = "Total consumption per capita by disposable income Decile") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the plot title


ggplot(hh_clean, aes(x = as.factor(hh_gender), y = disposable_inc)) +
  #geom_jitter(width = 0.2, color = "black", alpha = 0.5) +  # Jittered points
  geom_boxplot(outlier.shape = NA, fill = "grey", color = "black", alpha = 0.7) +  # Box plot without outliers
  scale_y_continuous(limit=c(0,750000),labels = scales::label_number(scale = 1/10000, suffix = "0k SEK")) +
  labs(x = "Disposable Income Decile", 
       y = "Total Consumption (in 10,000 SEK) pc", title = "Total consumption per capita by disposable income Decile") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the plot title


t.test(disposable_inc ~ hh_gender, data = hh_clean)
wilcox.test(disposable_inc ~ hh_gender, data = hh_clean)

aggregate(disposable_inc ~ hh_gender, data = hh_clean, median)

# Create density plot to compare disposable income by gender
ggplot(hh_clean, aes(x = disposable_inc, fill = as.factor(hh_gender))) +
  geom_density(alpha = 0.5) +  # Density plot with transparency for overlap
  scale_x_continuous(limit = c(0, 2000000), labels = scales::label_number(scale = 1/10000, suffix = "0k SEK")) +
  scale_fill_manual(values = c("woman" = "#c889d6", "man" = "#a1e3d2")) +  # Set specific colors for each gender
  labs(x = "Disposable Income (in 10,000 SEK)", 
       y = "Density", 
       title = "Density of Disposable Income by Gender",
       fill = "Gender") +  # Fill legend title for gender
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the plot title




# Create density plot to compare disposable income by gender
ggplot(hh_clean, aes(x = disposable_inc, fill = as.factor(hh_gender))) +
  geom_density(alpha = 0.6) +  # Density plot with transparency for overlap
  scale_x_continuous(limit = c(0, 1750000), 
                     expand = c(0,0),
                     labels = scales::label_number(scale = 1/10000, suffix = "0k")) +
  scale_y_continuous(limit = c(0, 0.0000039),
                     expand = c(0,0),
                     labels = scales::label_number(scale = 1*1000000)) +
  scale_fill_manual(values = c("woman" = "#c889d6", "man" = "#a1e3d2"),
                    labels = c("woman" = "Woman", "man" = "Man")) +  # Set specific colors for each gender
  labs(x = "Yearly SEK", 
       y = "Density", 
       title = "Disposable income by gender in Sweden",
       fill = "Gender",
       caption = "Source: Based on Household Budget Survey (HBS) 2021") +  # Fill legend title for gender
  theme_minimal() +
  expand_limits(x = 0, y = 0) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.95, 0.35),  # Adjust legend to top-right inside the plot area
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = "#dddded", 
                                         color = "#dddded", 
                                         size = 0.5, linetype = "solid")) +  # Grey box around legend
  annotate("text", x = 25000, y = 0.00000385, label = expression(10^-6),
           hjust = 0, vjust = 1, size = 3)

  
medians <- aggregate(disposable_inc ~ hh_gender, data = hh_clean, FUN = median)




# Create density plot to compare disposable income by gender with median lines
ggplot(hh_clean, aes(x = disposable_inc, fill = as.factor(hh_gender))) +
  geom_density(alpha = 0.6) +  # Density plot with transparency for overlap
  scale_x_continuous(limit = c(0, 1750000), 
                     expand = c(0,0),
                     labels = scales::label_number(scale = 1/10000, suffix = "0k")) +
  scale_y_continuous(limit = c(0, 0.0000039),
                     expand = c(0,0),
                     labels = scales::label_number(scale = 1*1000000)) +
  scale_fill_manual(values = c("woman" = "#c889d6", "man" = "#a1e3d2"),
                    labels = c("woman" = "Woman", "man" = "Man")) +  # Set specific colors for each gender
  labs(x = "Yearly SEK", 
       y = "Density", 
       title = "Disposable Income by Gender in Sweden",
       fill = "Gender",
       caption = "Source: Based on Household Budget Survey (HBS) 2021") +  # Caption
  theme_minimal() +
  expand_limits(x = 0, y = 0) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.95, 0.35),  # Adjust legend position
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = "#dddded", 
                                         color = "#dddded", 
                                         size = 0.5, linetype = "solid")) +  # Grey box around legend
  # Add vertical lines for the medians of each group with matching colors
  geom_vline(data = medians, aes(xintercept = disposable_inc, color = hh_gender), 
             linetype = "dashed", size = 1, show.legend = FALSE) +  # Remove legend for median lines
  scale_color_manual(values = c("woman" = "#c889d6", "man" = "#a1e3d2")) +  # Match colors for median lines
  annotate("text", x = 25000, y = 0.00000385, label = expression(10^-6),
           hjust = 0, vjust = 1, size = 3)














ggplot(hh_clean, aes(x = as.factor(hh_gender), y = tot_consumption_pc)) +
  #geom_jitter(width = 0.2, color = "black", alpha = 0.5) +  # Jittered points
  geom_boxplot(outlier.shape = NA, fill = "grey", color = "black", alpha = 0.7) +  # Box plot without outliers
  scale_y_continuous(labels = scales::label_number(scale = 1/10000, suffix = "0k SEK")) +
  labs(x = "Disposable Income Decile", 
       y = "Total Consumption (in 10,000 SEK) pc", title = "Total consumption per capita by disposable income Decile") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the plot title





############################################################################

ggplot(means_by_decile_base_long, aes(x = as.factor(d_income_decile), y = amount, fill = consumption_category)) +
  geom_bar(stat = "identity") +  # Use "identity" to use the actual values
  labs(x = "Disposable Income Decile", 
       y = "Amount", 
       title = "Total Consumption by Category for Each Income Decile") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the plot title
  scale_fill_brewer(palette = "Set3")  # Change the color palette if desired


ggplot(means_by_decile_base_long5, aes(x = as.factor(d_income_decile5), y = amount, fill = consumption_category)) +
  geom_bar(stat = "identity") +  # Use "identity" to use the actual values
  labs(x = "Disposable Income Decile", 
       y = "Amount", 
       title = "Total Consumption by Category for Each Income Decile") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the plot title
  scale_fill_brewer(palette = "Set3")  # Change the color palette if desired







ggplot(means_by_decile_base_tot_z_long, aes(x = as.factor(d_income_decile), y = amount, fill = consumption_category)) +
  geom_bar(stat = "identity") +  # Use "identity" to use the actual values
  labs(x = "Disposable Income Decile", 
       y = "Amount", 
       title = "Total Consumption by Category for Each Income Decile") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the plot title
  scale_fill_brewer(palette = "Set3")  # Change the color palette if desired












ggplot(means_by_decile_base_tot_z_long5, aes(x = as.factor(d_income_decile5), y = amount, fill = consumption_category)) +
  geom_bar(stat = "identity") +  # Use "identity" to use the actual values
  labs(x = "Disposable Income Decile", 
       y = "Amount", 
       title = "Total Consumption by Category for Each Income Decile") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the plot title
  scale_fill_brewer(palette = "Set3")  # Change the color palette if desired




ggplot(means_by_gender_base_tot_z_long, aes(x = as.factor(hh_gender), y = amount, fill = consumption_category)) +
  geom_bar(stat = "identity") +  # Use "identity" to use the actual values
  labs(x = "Disposable Income Decile", 
       y = "Amount", 
       title = "Total Consumption by Category for Each Income Decile") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the plot title
  scale_fill_brewer(palette = "Set3")  # Change the color palette if desired



ggplot(means_by_gender_base_tot_z_long, aes(x = as.factor(hh_gender), y = amount, fill = consumption_category)) +
  geom_bar(stat = "identity") +  # Use "identity" to use the actual values
  labs(x = "Disposable Income Decile", 
       y = "Amount", 
       title = "Total Consumption by Category for Each Income Decile") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the plot title
  scale_fill_brewer(palette = "Set3")  # Change the color palette if desired



# repair sucks



ggplot(hh_clean, aes(x = as.factor(d_income_decile), y = tot_repair)) +
  #geom_jitter(width = 0.2, color = "black", alpha = 0.5) +  # Jittered points
  geom_boxplot(outlier.shape = NA, fill = "grey", color = "black", alpha = 0.7) +  # Box plot without outliers
  #scale_y_continuous(labels = scales::label_number(scale = 1/1000, suffix = "k SEK")) +
  labs(x = "Disposable Income Decile", 
       y = "Total Consumption (in 10,000 SEK)", title = "Total Expenditure by Income Decile") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the plot title

################################################################################


ggplot(hh_clean, aes(x = as.factor(hh_gender), y = disposable_inc_pc)) +
  #geom_jitter(width = 0.2, color = "black", alpha = 0.5) +  # Jittered points
  geom_boxplot(outlier.shape = NA, fill = "grey", color = "black", alpha = 0.7) +  # Box plot without outliers
  scale_y_continuous(limits=c(0,5000),
    labels = scales::label_number(scale = 1/1000, suffix = "k SEK")) +
  labs(x = "Disposable Income Decile", 
       y = "Total Consumption (in 10,000 SEK) pc", title = "Total consumption per capita by disposable income Decile") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the plot title








ggplot(hh_clean, aes(x = as.factor(d_income_decile_pc), y = tot_consumption_pc)) +
  #geom_jitter(width = 0.2, color = "black", alpha = 0.5) +  # Jittered points
  geom_boxplot(outlier.shape = NA, fill = "grey", color = "black", alpha = 0.7) +  # Box plot without outliers
  scale_y_continuous(labels = scales::label_number(scale = 1/10000, suffix = "0k SEK")) +
  labs(x = "Disposable Income Decile pc", 
       y = "Total Consumption (in 10,000 SEK) pc", 
       title = "Total consumption per capita by disposable income pc Decile") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the plot title


ggplot(hh_clean, aes(x = as.factor(d_income_decile_pc), y = tot_consumption)) +
  #geom_jitter(width = 0.2, color = "black", alpha = 0.5) +  # Jittered points
  geom_boxplot(outlier.shape = NA, fill = "grey", color = "black", alpha = 0.7) +  # Box plot without outliers
  scale_y_continuous(labels = scales::label_number(scale = 1/10000, suffix = "0k SEK")) +
  labs(x = "Disposable Income Decile", 
       y = "Total Consumption (in 10,000 SEK)", 
       title = "Total consumption by disposable income pc Decile") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the plot title



############################################################

ggplot(means_by_decile_base_long_pc, 
       aes(x = as.factor(d_income_decile_pc), y = amount, fill = consumption_category)) +
  geom_bar(stat = "identity") +  # Use "identity" to use the actual values
  labs(x = "Disposable Income Decile pc", 
       y = "Amount pc", 
       title = "Total Consumption pc by Category for Each Income Decile pc") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the plot title
  scale_fill_brewer(palette = "Set3")  # Change the color palette if desired




ggplot(means_by_decile_base_tot_z_long_pc,
       aes(x = as.factor(d_income_decile_pc), y = amount, fill = consumption_category)) +
  geom_bar(stat = "identity") +  # Use "identity" to use the actual values
  labs(x = "Disposable Income Decile pc", 
       y = "Amount pc", 
       title = "Consumption per capita
       by Category for Each Income Decile pc (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the plot title
  scale_fill_brewer(palette = "Set3")  # Change the color palette if desired



# Create a named vector to map short consumption categories to full names
category_labels <- c(
  "perc_a_food_n_alcohol" = "Food
  and 
  beverages",
  "perc_b_cloth_n_foot" = "Clothing
  and footwear",
  "perc_c_furniture_mantain_home" = "Furnishings, 
  household equipment, 
  and home maintenance",
  "perc_d_transport" = "Transport",
  "perc_e_ict" = "Information
  and communication",
  "perc_f_recreation" = "Recreation, sports, 
  culture, garden, and pets",
  "perc_g_personal_care" = "Personal care, 
  social protection,
  and other"
)


# Create a bar plot with customized labels for consumption categories
ggplot(means_by_decile_base_tot_z_long_pc,
       aes(x = as.factor(d_income_decile_pc), y = amount, fill = consumption_category)) +
  geom_bar(stat = "identity") +  # Use "identity" to use the actual values
  labs(x = "Deciles of disposable income per captia", 
       y = "Percentage (%)", 
       fill = "Consumption Category",
       title = "Distribution of househould consumption per capita by deciles") +
  geom_text(aes(label = paste0(round(amount, 1))), 
            position = position_stack(vjust = 0.5),  # Positioning text in the middle of each bar
            size = 2.8, color = "black") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the plot title
  scale_fill_manual(
    values = RColorBrewer::brewer.pal(7, "Set3"),  # Color palette
                    labels = category_labels)  # Custom labels for consumption categories





# Create a bar plot with customized labels for consumption categories
ggplot(means_by_decile_base_tot_z_long5,
       aes(x = as.factor(d_income_decile5 ), y = amount, fill = consumption_category)) +
  geom_bar(stat = "identity") +  # Use "identity" to use the actual values
  labs(x = "Deciles of disposable income", 
       y = "Percentage (%)", 
       fill = "Consumption Category",
       title = "Distribution of househould consumption per capita by deciles") +
  geom_text(aes(label = paste0(round(amount, 1))), 
            position = position_stack(vjust = 0.5),  # Positioning text in the middle of each bar
            size = 2.8, color = "black") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the plot title
  scale_fill_manual(
    values = RColorBrewer::brewer.pal(7, "Set3"),  # Color palette
    labels = category_labels)  # Custom labels for consumption categories





# Create a bar plot with customized labels for consumption categories
ggplot(means_by_decile_base_tot_z_long_pc5,
       aes(x = as.factor(d_income_decile_pc5), y = amount, fill = consumption_category)) +
  geom_bar(stat = "identity") +  # Use "identity" to use the actual values
  labs(x = "Deciles of disposable income per captia", 
       y = "Percentage (%)", 
       fill = "Consumption Category",
       title = "Distribution of househould consumption per capita by deciles") +
  geom_text(aes(label = paste0(round(amount, 1))), 
            position = position_stack(vjust = 0.5),  # Positioning text in the middle of each bar
            size = 2.8, color = "black") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the plot title
  scale_fill_manual(
    values = RColorBrewer::brewer.pal(7, "Set3"),  # Color palette
    labels = category_labels)  # Custom labels for consumption categories


# Create a bar plot with customized labels for consumption categories
ggplot(means_by_decile_base_tot_z_long,
       aes(x = as.factor(d_income_decile), y = amount, fill = consumption_category)) +
  geom_bar(stat = "identity") +  # Use "identity" to use the actual values
  labs(x = "Deciles of disposable income per captia", 
       y = "Percentage (%)", 
       fill = "Consumption Category",
       title = "Distribution of househould consumption per capita by deciles") +
  geom_text(aes(label = paste0(round(amount, 1))), 
            position = position_stack(vjust = 0.5),  # Positioning text in the middle of each bar
            size = 2.8, color = "black") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the plot title
  scale_fill_manual(
    values = RColorBrewer::brewer.pal(7, "Set3"),  # Color palette
    labels = category_labels)  # Custom labels for consumption categories




##################################################
# pople in home


describeBy(data=hh_clean, total_members ~ d_income_decile5)
describeBy(data=hh_clean, total_adults ~ d_income_decile5)
describeBy(data=hh_clean, kids_0_19 ~ d_income_decile)



people <- hh_clean %>%
  group_by(d_income_decile5) %>%
  summarise(average_total_adults = mean(total_adults, na.rm = TRUE))


ggplot(hh_clean, aes(x = as.factor(d_income_decile5), y = total_adults)) +
  geom_boxplot(outlier.shape = NA, fill = "grey", color = "black", alpha = 0.7)




ggplot(hh_clean, aes(x=total_adults)) + 
  geom_histogram(binwidth=.5, colour="black", fill="white") + 
  facet_grid(d_income_decile5 ~ .) 


ggplot(hh_clean, aes(x=total_members)) + 
  geom_histogram(binwidth=.5, colour="black", fill="white") + 
  facet_grid(d_income_decile5 ~ .) 










#####################################################
# Gender
####################################

hh_clean$hh_gender


means_by_gender_base <- aggregate(
  cbind(
    a_food_n_alcohol,
    b_cloth_n_foot,
    c_furniture_mantain_home,
    d_transport,
    e_ict,
    f_recreation,
    g_personal_care
  ) ~ hh_gender,
  data = hh_clean,
  FUN = mean,
  na.rm = TRUE
)
means_by_gender_base_tot <- means_by_gender_base


means_by_gender_base_tot$tot <- means_by_gender_base_tot$a_food_n_alcohol+
  means_by_gender_base_tot$b_cloth_n_foot +
  means_by_gender_base_tot$c_furniture_mantain_home +
  means_by_gender_base_tot$d_transport +
  means_by_gender_base_tot$e_ict +
  means_by_gender_base_tot$f_recreation +
  means_by_gender_base_tot$g_personal_care


means_by_gender_base_tot_z <- means_by_gender_base_tot %>%
  mutate(
    perc_a_food_n_alcohol = (a_food_n_alcohol / tot) * 100,
    perc_b_cloth_n_foot = (b_cloth_n_foot / tot) * 100,
    perc_c_furniture_mantain_home = (c_furniture_mantain_home / tot) * 100,
    perc_d_transport = (d_transport / tot) * 100,
    perc_e_ict = (e_ict / tot) * 100,
    perc_f_recreation = (f_recreation / tot) * 100,
    perc_g_personal_care = (g_personal_care / tot) * 100
  )

# Select only the relevant columns to display
means_by_gender_base_tot_z <- means_by_gender_base_tot_z %>%
  select(hh_gender , starts_with("perc_"))



means_by_gender_base_long <- means_by_gender_base %>%
  pivot_longer(
    cols = c(a_food_n_alcohol, b_cloth_n_foot, c_furniture_mantain_home, 
             d_transport, e_ict, f_recreation, g_personal_care),
    names_to = "consumption_category",
    values_to = "amount")


means_by_gender_base_tot_z_long <- means_by_gender_base_tot_z %>%
  pivot_longer(
    cols = c(perc_a_food_n_alcohol , perc_b_cloth_n_foot , perc_c_furniture_mantain_home , 
             perc_d_transport , perc_e_ict , perc_f_recreation , perc_g_personal_care),
    names_to = "consumption_category",
    values_to = "amount")




hh_clean$kids_dummy <- ifelse(hh_clean$kids_0_19 >= 1, 1, 0)


describe(hh_clean$kids_0_19)
describe(hh_clean$kids_dummy)

describe(hh_clean_no_expand$kids_0_19)


##############################3


describeBy(data=hh_clean, total_adults ~ d_income_decile5)


hh_clean$adults <- ifelse(hh_clean$total_adults == 1, 1,
                          ifelse(hh_clean$total_adults == 2, 2, 3))




###########################################################

unique(hh_clean$tenure)
unique( as.numeric( hh_clean$tenure))


hh_clean$tenure2 <- ifelse(as.numeric(hh_clean$tenure) == 1, 1,
                           ifelse(as.numeric(hh_clean$tenure) == 2, 2, 3))

unique(hh_clean$tenure2)  
# factor(hh_clean$tenure,
#                         levels = c(1, 2, 3, 4, 6),
#                         labels = c("Ownership of a house or farm",
#                                    "Housing cooperative",
#                                    "Rental, first-hand",
#                                    "Rental, second-hand",
#                                    "Other"))
# 
# 


###########################################################
length(na.omit((hh_clean$CE_repair_white)))

describe(hh_clean$CE_repair_white)


####################################################


# Assuming your data frame is named hh_clean
hh_clean <- hh_clean %>%
  mutate(archeotype = case_when(
    d_income_decile5 == 1 & tenure2 == 1 & adults == 1 & kids_dummy == 0 ~ 1,
    d_income_decile5 == 2 & tenure2 == 1 & adults == 1 & kids_dummy == 0 ~ 2,
    d_income_decile5 == 3 & tenure2 == 1 & adults == 1 & kids_dummy == 0 ~ 3,
    d_income_decile5 == 4 & tenure2 == 1 & adults == 1 & kids_dummy == 0 ~ 4,
    d_income_decile5 == 5 & tenure2 == 1 & adults == 1 & kids_dummy == 0 ~ 5,
    d_income_decile5 == 1 & tenure2 == 2 & adults == 1 & kids_dummy == 0 ~ 6,
    d_income_decile5 == 2 & tenure2 == 2 & adults == 1 & kids_dummy == 0 ~ 7,
    d_income_decile5 == 3 & tenure2 == 2 & adults == 1 & kids_dummy == 0 ~ 8,
    d_income_decile5 == 4 & tenure2 == 2 & adults == 1 & kids_dummy == 0 ~ 9,
    d_income_decile5 == 5 & tenure2 == 2 & adults == 1 & kids_dummy == 0 ~ 10,
    d_income_decile5 == 1 & tenure2 == 3 & adults == 1 & kids_dummy == 0 ~ 11,
    d_income_decile5 == 2 & tenure2 == 3 & adults == 1 & kids_dummy == 0 ~ 12,
    d_income_decile5 == 3 & tenure2 == 3 & adults == 1 & kids_dummy == 0 ~ 13,
    d_income_decile5 == 4 & tenure2 == 3 & adults == 1 & kids_dummy == 0 ~ 14,
    d_income_decile5 == 5 & tenure2 == 3 & adults == 1 & kids_dummy == 0 ~ 15,
    d_income_decile5 == 1 & tenure2 == 1 & adults == 2 & kids_dummy == 0 ~ 16,
    d_income_decile5 == 2 & tenure2 == 1 & adults == 2 & kids_dummy == 0 ~ 17,
    d_income_decile5 == 3 & tenure2 == 1 & adults == 2 & kids_dummy == 0 ~ 18,
    d_income_decile5 == 4 & tenure2 == 1 & adults == 2 & kids_dummy == 0 ~ 19,
    d_income_decile5 == 5 & tenure2 == 1 & adults == 2 & kids_dummy == 0 ~ 20,
    d_income_decile5 == 1 & tenure2 == 2 & adults == 2 & kids_dummy == 0 ~ 21,
    d_income_decile5 == 2 & tenure2 == 2 & adults == 2 & kids_dummy == 0 ~ 22,
    d_income_decile5 == 3 & tenure2 == 2 & adults == 2 & kids_dummy == 0 ~ 23,
    d_income_decile5 == 4 & tenure2 == 2 & adults == 2 & kids_dummy == 0 ~ 24,
    d_income_decile5 == 5 & tenure2 == 2 & adults == 2 & kids_dummy == 0 ~ 25,
    d_income_decile5 == 1 & tenure2 == 3 & adults == 2 & kids_dummy == 0 ~ 26,
    d_income_decile5 == 2 & tenure2 == 3 & adults == 2 & kids_dummy == 0 ~ 27,
    d_income_decile5 == 3 & tenure2 == 3 & adults == 2 & kids_dummy == 0 ~ 28,
    d_income_decile5 == 4 & tenure2 == 3 & adults == 2 & kids_dummy == 0 ~ 29,
    d_income_decile5 == 5 & tenure2 == 3 & adults == 2 & kids_dummy == 0 ~ 30,
    d_income_decile5 == 1 & tenure2 == 1 & adults == 3 & kids_dummy == 0 ~ 31,
    d_income_decile5 == 2 & tenure2 == 1 & adults == 3 & kids_dummy == 0 ~ 32,
    d_income_decile5 == 3 & tenure2 == 1 & adults == 3 & kids_dummy == 0 ~ 33,
    d_income_decile5 == 4 & tenure2 == 1 & adults == 3 & kids_dummy == 0 ~ 34,
    d_income_decile5 == 5 & tenure2 == 1 & adults == 3 & kids_dummy == 0 ~ 35,
    d_income_decile5 == 1 & tenure2 == 2 & adults == 3 & kids_dummy == 0 ~ 36,
    d_income_decile5 == 2 & tenure2 == 2 & adults == 3 & kids_dummy == 0 ~ 37,
    d_income_decile5 == 3 & tenure2 == 2 & adults == 3 & kids_dummy == 0 ~ 38,
    d_income_decile5 == 4 & tenure2 == 2 & adults == 3 & kids_dummy == 0 ~ 39,
    d_income_decile5 == 5 & tenure2 == 2 & adults == 3 & kids_dummy == 0 ~ 40,
    d_income_decile5 == 1 & tenure2 == 3 & adults == 3 & kids_dummy == 0 ~ 41,
    d_income_decile5 == 2 & tenure2 == 3 & adults == 3 & kids_dummy == 0 ~ 42,
    d_income_decile5 == 3 & tenure2 == 3 & adults == 3 & kids_dummy == 0 ~ 43,
    d_income_decile5 == 4 & tenure2 == 3 & adults == 3 & kids_dummy == 0 ~ 44,
    d_income_decile5 == 5 & tenure2 == 3 & adults == 3 & kids_dummy == 0 ~ 45,
    d_income_decile5 == 1 & tenure2 == 1 & adults == 1 & kids_dummy == 1 ~ 46,
    d_income_decile5 == 2 & tenure2 == 1 & adults == 1 & kids_dummy == 1 ~ 47,
    d_income_decile5 == 3 & tenure2 == 1 & adults == 1 & kids_dummy == 1 ~ 48,
    d_income_decile5 == 4 & tenure2 == 1 & adults == 1 & kids_dummy == 1 ~ 49,
    d_income_decile5 == 5 & tenure2 == 1 & adults == 1 & kids_dummy == 1 ~ 50,
    d_income_decile5 == 1 & tenure2 == 2 & adults == 1 & kids_dummy == 1 ~ 51,
    d_income_decile5 == 2 & tenure2 == 2 & adults == 1 & kids_dummy == 1 ~ 52,
    d_income_decile5 == 3 & tenure2 == 2 & adults == 1 & kids_dummy == 1 ~ 53,
    d_income_decile5 == 4 & tenure2 == 2 & adults == 1 & kids_dummy == 1 ~ 54,
    d_income_decile5 == 5 & tenure2 == 2 & adults == 1 & kids_dummy == 1 ~ 55,
    d_income_decile5 == 1 & tenure2 == 3 & adults == 1 & kids_dummy == 1 ~ 56,
    d_income_decile5 == 2 & tenure2 == 3 & adults == 1 & kids_dummy == 1 ~ 57,
    d_income_decile5 == 3 & tenure2 == 3 & adults == 1 & kids_dummy == 1 ~ 58,
    d_income_decile5 == 4 & tenure2 == 3 & adults == 1 & kids_dummy == 1 ~ 59,
    d_income_decile5 == 5 & tenure2 == 3 & adults == 1 & kids_dummy == 1 ~ 60,
    d_income_decile5 == 1 & tenure2 == 1 & adults == 2 & kids_dummy == 1 ~ 61,
    d_income_decile5 == 2 & tenure2 == 1 & adults == 2 & kids_dummy == 1 ~ 62,
    d_income_decile5 == 3 & tenure2 == 1 & adults == 2 & kids_dummy == 1 ~ 63,
    d_income_decile5 == 4 & tenure2 == 1 & adults == 2 & kids_dummy == 1 ~ 64,
    d_income_decile5 == 5 & tenure2 == 1 & adults == 2 & kids_dummy == 1 ~ 65,
    d_income_decile5 == 1 & tenure2 == 2 & adults == 2 & kids_dummy == 1 ~ 66,
    d_income_decile5 == 2 & tenure2 == 2 & adults == 2 & kids_dummy == 1 ~ 67,
    d_income_decile5 == 3 & tenure2 == 2 & adults == 2 & kids_dummy == 1 ~ 68,
    d_income_decile5 == 4 & tenure2 == 2 & adults == 2 & kids_dummy == 1 ~ 69,
    d_income_decile5 == 5 & tenure2 == 2 & adults == 2 & kids_dummy == 1 ~ 70,
    d_income_decile5 == 1 & tenure2 == 3 & adults == 2 & kids_dummy == 1 ~ 71,
    d_income_decile5 == 2 & tenure2 == 3 & adults == 2 & kids_dummy == 1 ~ 72,
    d_income_decile5 == 3 & tenure2 == 3 & adults == 2 & kids_dummy == 1 ~ 73,
    d_income_decile5 == 4 & tenure2 == 3 & adults == 2 & kids_dummy == 1 ~ 74,
    d_income_decile5 == 5 & tenure2 == 3 & adults == 2 & kids_dummy == 1 ~ 75,
    d_income_decile5 == 1 & tenure2 == 1 & adults == 3 & kids_dummy == 1 ~ 76,
    d_income_decile5 == 2 & tenure2 == 1 & adults == 3 & kids_dummy == 1 ~ 77,
    d_income_decile5 == 3 & tenure2 == 1 & adults == 3 & kids_dummy == 1 ~ 78,
    d_income_decile5 == 4 & tenure2 == 1 & adults == 3 & kids_dummy == 1 ~ 79,
    d_income_decile5 == 5 & tenure2 == 1 & adults == 3 & kids_dummy == 1 ~ 80,
    d_income_decile5 == 1 & tenure2 == 2 & adults == 3 & kids_dummy == 1 ~ 81,
    d_income_decile5 == 2 & tenure2 == 2 & adults == 3 & kids_dummy == 1 ~ 82,
    d_income_decile5 == 3 & tenure2 == 2 & adults == 3 & kids_dummy == 1 ~ 83,
    d_income_decile5 == 4 & tenure2 == 2 & adults == 3 & kids_dummy == 1 ~ 84,
    d_income_decile5 == 5 & tenure2 == 2 & adults == 3 & kids_dummy == 1 ~ 85,
    d_income_decile5 == 1 & tenure2 == 3 & adults == 3 & kids_dummy == 1 ~ 86,
    d_income_decile5 == 2 & tenure2 == 3 & adults == 3 & kids_dummy == 1 ~ 87,
    d_income_decile5 == 3 & tenure2 == 3 & adults == 3 & kids_dummy == 1 ~ 88,
    d_income_decile5 == 4 & tenure2 == 3 & adults == 3 & kids_dummy == 1 ~ 89,
    d_income_decile5 == 5 & tenure2 == 3 & adults == 3 & kids_dummy == 1 ~ 90,
    TRUE ~ NA_real_ # Fallback for unexpected values
  ))



colnames(hh_clean %>%
           select(22:170))

unique(hh_clean$archeotype)




hh_subset <- hh_clean %>%
  select(22:170)



average_by_archeotype <- hh_subset %>%
  group_by(archeotype) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

average_by_archeotype2 <- hh_subset %>%
  group_by(archeotype) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))

colnames(average_by_archeotype)



library(writexl)

# Export the data to an Excel file
write_xlsx(average_by_archeotype, "average_by_archeotype.xlsx")





