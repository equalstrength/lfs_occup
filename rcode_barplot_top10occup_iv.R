# EqualStrength
##  Jeremy Kuhnle
### Graphs: additional occupations
####  Data Source: EU-LFS 2019
##### Date: 04.08.2023
##### Updated: 11.08.2023


# Install and load necessary packages

install.packages("ggplot2")
install.packages("dplyr")
install.packages('haven')
library(ggplot2)
library(dplyr)
library(haven)
library(tidyr)

# Load the data
df <- read_dta("C:/Users/jkuhnle/OneDrive - Université de Lausanne/Documents/projects/equalstrength/general/code/top10occup.dta")

# Label the ISCO codes in the "occupation" variable. They are labeled in STATA but
# read-dta does not carry over the labels.
# Still working on this...use "mapping" command.
# You can update the labels with the correct ISCO-08 codes.


# Step 2: Group by country and occupation to find the frequency
df_top10 <- df %>%
  group_by(country, occupation) %>%
  summarise(freq = n()) %>%
  ungroup()

# Step 3: Find the top ten most frequent occupations for each country
df_top10 <- df_top10 %>%
  group_by(country) %>%
  arrange(country, desc(freq)) %>%
  top_n(10, wt = freq) %>%
  ungroup()

# Step 4: Create a new variable "gender" with labels "Male" and "Female"
# variable "sex" in STATA was not labeled.
df <- df %>%
  mutate(gender = case_when(
    sex == 1 ~ "Male",
    sex == 2 ~ "Female",
    TRUE ~ NA_character_  # Handle any other cases (if any)
  ))

# Step 5: Calculate proportion of female workers in each occupation
df_gender <- df %>%
  group_by(country, occupation) %>%
  summarise(total_workers = n(),
            pct_female = sum(gender == "Female") / total_workers * 100) %>%
  select(-total_workers)

# Step 6: Calculate the percentage of observations for each education level "H", "M", and "L" for each occupation
df_education <- df %>%
  group_by(country, occupation) %>%
  summarise(total_workers = n(),
            pct_H = sum(isced == "H") / total_workers * 100,
            pct_M = sum(isced == "M") / total_workers * 100,
            pct_L = sum(isced == "L") / total_workers * 100)

# Step 7: Calculate the percentage of "natives" for each unique occupation in each country
df_nationality <- df %>%
  group_by(country, occupation) %>%
  summarise(total_workers = n(),
            pct_natives = sum(national == "000-OWN COUNTRY") / total_workers * 100) %>%
  select(-total_workers)

# Merge the data to get the final dataset
df_final <- left_join(df_top10, df_gender, by = c("country", "occupation")) %>%
  left_join(df_education, by = c("country", "occupation")) %>%
  left_join(df_nationality, by = c("country", "occupation"))

# Save the final dataset
write.csv(df_final, "top_10_occupations_analysis.csv", row.names = FALSE)

########################################################################
# Now let us plot this information
########################################################################

# Load necessary libraries
install.packages("patchwork")
library(ggplot2)
library(patchwork)

# Convert ISCO-08 codes to a numeric vector for ordering
df_final$occupation_order <- as.numeric(gsub("ISCO-08:", "", df_final$occupation))

# Filter the top 10 occupations from plot_top10
top_10_occupations <- df_final %>%
  filter(country %in% unique(df_final$country), occupation %in% unique(df_final$occupation))

# Step 1: Plot the Top 10 Occupations for Each Country
plot_top10 <- ggplot(top_10_occupations, aes(x = reorder(occupation, occupation_order), y = freq, fill = occupation)) +
  geom_bar(stat = "identity", width = 0.8, fill = "blue") +
  labs(title = "Top 10 Occupations (EU-LFS 2019)",
       x = "Occupation(ISCO-08)", y = "Number of Workers") +
  scale_y_continuous(limits = c(0, 1300)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~country, scales = "free", ncol = 3)

# Step 2: Plot the Top 10 Occupations for Each Country (except Germany "DE")
## Germany has the largest N, much larger than others. Several occups == 3000 or larger
plot_top10_other <- ggplot(top_10_occupations %>% filter(country != "DE"),
                           aes(x = reorder(occupation, occupation_order),
                               y = freq, fill = occupation)) +
  geom_bar(stat = "identity", width = 0.8, fill = "blue") +
  labs(title = "Top 10 Occupations (Without DE: EU-LFS 2019)",
       x = "Occupation(ISCO-08)", y = "Number of Workers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 1300)) +
  guides(fill = FALSE)

# Step 3: Plot the Top 10 Occupations for Germany "DE"
## Use this to plot single graphs per country - replace "DE" w/ "country x". 
plot_top10_germany <- ggplot(top_10_occupations %>% filter(country == "DE"),
                             aes(x = reorder(occupation, occupation_order),
                                 y = freq, fill = occupation)) +
  geom_bar(stat = "identity", width = 0.8, fill = "blue") +
  labs(title = "Top 10 Occupations (Germany: EU-LFS 2019)",
       x = "Occupation(ISCO-08)", y = "Number of Workers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 5000)) +
  guides(fill = FALSE)

# Step 4: Plot the Percentage of Female Workers in the Top 10 Occupations for Each Country
plot_gender <- ggplot(top_10_occupations, aes(x = reorder(occupation, occupation_order, levels = unique(occupation)[order(freq)]), y = pct_female, fill = occupation)) +
  geom_bar(stat = "identity", width = 0.8, fill = "orange") +
  labs(title = "Percentage of Women in Top 10 Occupations (EU-LFS 2019)",
       x = "Occupation(ISCO-08)", y = "Percentage Female") +
  scale_y_continuous(limits = c(0, 100)) +  # Adjust the y-axis limits
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~country, scales = "free", ncol = 3)


# Step 5: Plot the Percentage of Highly Educated Workers in the Top 10 Occupations for Each Country
plot_education <- ggplot(top_10_occupations, aes(x = reorder(occupation, occupation_order, levels = unique(occupation)[order(freq)]), y = pct_H, fill = occupation)) +
  geom_bar(stat = "identity", width = 0.8, fill = "green") +
  labs(title = "Percentage of Highly Educated in Top 10 Occupations (EU-LFS 2019)",
       x = "Occupation(ISCO-08)", y = "Percentage Highly Educated") +
  scale_y_continuous(limits = c(0, 100)) +  # Adjust the y-axis limits
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~country, scales = "free", ncol = 3)

# Step 6: Calculate the percentage of natives for each of the top 10 occupations in each country
plot_natives <- ggplot(top_10_occupations, aes(x = reorder(occupation, occupation_order, levels = unique(occupation)[order(freq)]), y = pct_natives, fill = occupation)) +
  geom_bar(stat = "identity", width = 0.8, fill = "pink") +
  labs(title = "Percentage of Natives in Top 10 Occupations (EU-LFS 2019)",
       x = "Occupation(ISCO-08)", y = "Percentage Natives") +
  scale_y_continuous(limits = c(0, 100)) +  # Adjust the y-axis limits
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~country, scales = "free", ncol = 3)


# Display the plots
plot_top10
plot_top10_germany
plot_gender
plot_education
plot_natives


# Combine the plots using patchwork
combined_plot1 <- (plot_top10 | plot_top10_germany)
combined_plot2 <- (plot_gender | plot_education | plot_natives)
combined_plot3 <- (plot_gender | plot_education / plot_natives)
combined_plot4 <- (plot_gender | plot_education)



# Display the combined plot
print(combined_plot1)
print(combined_plot2)
print(combined_plot3)
print(combined_plot4)


