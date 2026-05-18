library(tidyverse)


#-----------------------------------READING IN DATA--------------------------
url = "https://raw.githubusercontent.com/WendyR20/Data-608-Story-7/refs/heads/main/US_Mineral_Import_Reliance%20-%20US_Mineral_Import_Reliance%20(1).csv"

minerals_df <- read.csv(url)


url1 = "https://raw.githubusercontent.com/WendyR20/Data-608-Story-7/refs/heads/main/5047a38a-408a-4892-aa87-737d71ed249d_Data.csv"

stability_df <- read.csv(url1)


head(minerals_df)
tail(minerals_df)

colnames(minerals_df)
sapply(minerals_df, class)

head(stability_df)
tail(stability_df)

colnames(stability_df)
sapply(stability_df, class)

#------------------------DATA CLEANING-------------------------------------
#removing footnotes from stability_df

stability_df2 <- stability_df %>% slice(1:(n() - 5))
tail(stability_df2)

#renaming the columns in stability_df

stability_df2 <- stability_df2 %>%
  rename(
    Country_Name = Country.Name,
    Country_Code = Country.Code,
    Score_Description = Series.Name,
    Series_Code = Series.Code,
    Stability_Estimate = "X2024..YR2024."
  )

#removing Country code, score description and series code columns

stability_df2 <- stability_df2 %>%
  select(-c(Country_Code, Score_Description, Series_Code))

#Adding a stability score column (we will use it to compute overall risk later),
#the WGI scale goes from (-2.5) (very unstable) to (+2.5) (very stable)
#only scores above 1.0 will be given the most stable score

stability_df2 <- stability_df2 %>%
  mutate(
    Stability_Risk = case_when(
      
      Stability_Estimate > 1 ~ 0,
      
      Stability_Estimate >= 0 &
        Stability_Estimate <= 1 ~ 1,
      
      Stability_Estimate >= -1 &
        Stability_Estimate < 0 ~ 2,
      
      Stability_Estimate < -1 ~ 3,
      
      TRUE ~ NA_real_
    )
  )

#the stability dataframe will be ready to be joined to the mineral dataframe
#once all the country names align

stability_df2 <- stability_df2 %>%
  mutate(Country_Name = case_when(
    Country_Name == "Russian Federation" ~ "Russia",
    TRUE ~ Country_Name
  ))


#There are some NA rows in the dataset, minerals for which the data was unavailable
#as the U.S. was less than 20% reliant on exports 
#Let's make sure the mineral dataframe is clean
colSums(is.na(minerals_df))

#there is definitely missing data but it may be saved in the columns as a string
unique(minerals_df$Mineral)
unique(minerals_df$US_Net_Import_Reliance)

#replace "N/A" with NA across ALL columns since all columns are of character type
minerals_df <- minerals_df %>%
  mutate(across(everything(), ~ ifelse(.x == "N/A", NA, .x)))

#adding a geopolitical risk column based on the whether a country is an ally,
#neutral or a competitor. Since there are only three categories for this scale,
#and there were four for the stability risk category, we will use symmetric spacing here

#saving as a new dataframe
minerals_df2 <- minerals_df %>%
  mutate(Geopolitical_Risk = case_when(
    Geopolitical_Category == "Ally"       ~ 0,
    Geopolitical_Category == "Neutral"    ~ 1.5,
    Geopolitical_Category == "Competitor" ~ 3,
    .default = NA                 
  ))
 

#now we can add a dependency score for each mineral based on the net reliance
#on imports the U.S. has for each
unique(minerals_df2$US_Net_Import_Reliance)
unique(minerals_df2$Import_Reliance_Status)

#we will use the import_reliance status column to create the dependency score,

minerals_df2 <- minerals_df2 %>%
  mutate(
    Dependency_Risk = case_when(
      
      Import_Reliance_Status ==
        "Low Import Reliance" ~ 0,
      
      Import_Reliance_Status ==
        "Moderate Import Reliance" ~ 1,
      
      Import_Reliance_Status ==
        "Moderate-High Import Reliance" ~ 2,
      
      Import_Reliance_Status %in% c(
        "High Import Reliance",
        "Very High Import Reliance",
        "Complete Import Reliance"
      ) ~ 3,
      
      TRUE ~ NA_real_
    )
  )


#Now we are able to join the two dataframes
minerals_stability <- minerals_df2 %>%
  left_join(
    stability_df2,
    by = c("Major_Import_Source" = "Country_Name")
  )


#checking for mismatches:
minerals_stability %>%
  filter(is.na(Stability_Estimate)) %>%
  distinct(Major_Import_Source)

#We will now use all three score to create a total risk assessment and store
#this new total risk score as it's own column
#the total risk scale will have four tiers: low risk, moderate risk, high risk
#and extreme risk

#total risk for each mineral will only be summed if all three columns: geopolitical risk,
#stability risk and dependency risk are not empty

minerals_stability <- minerals_stability %>%
  mutate(
    Total_Risk = ifelse(
      is.na(Geopolitical_Risk) |
        is.na(Stability_Risk) |
        is.na(Dependency_Risk),
      NA,
      Geopolitical_Risk + Stability_Risk + Dependency_Risk
    )
  )



#----------------------------AT RISK MINERALS-------------------------------
minerals_stability %>% summarise(count = sum(Total_Risk >= 6.0, na.rm = TRUE))


#filtering out nas from total_risk before plotting, and flagging when
#minerals have a total risk score higher than 3.0
data <- minerals_stability %>%
  filter(!is.na(Total_Risk)) %>%
  mutate(
    Mineral = reorder(Mineral, Total_Risk),
    Risk_Category = ifelse(Total_Risk >= 6.0, "High Risk (≥ 6.0)", "Standard")
  )

ggplot(data = data, aes(x = Mineral, y = Total_Risk, fill = Risk_Category)) +
  geom_col(width = 0.75) +
  coord_flip() +
  
  scale_fill_manual(values = c("High Risk (≥ 6.0)" = "#8B0000", "Standard" = "#708090")) +
  
  geom_hline(yintercept = 3.0, linetype = "dashed", color = "#333333", alpha = 0.6) +
  
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(), 
    panel.grid.major.x = element_line(color = "#EBEBEB"),
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 5)),
    plot.subtitle = element_text(color = "gray30", size = 10, margin = margin(b = 15)),
    plot.caption = element_text(color = "gray40", size = 8, hjust = 0, margin = margin(t = 15)),
    
    axis.text.y = element_text(face = "bold", color = "#222222", size = 9),
    
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank()
  ) +
  labs(
    title = "Critical Minerals with the Highest Supply Chain Risk to the United States",
    subtitle = "Total Risk Score reflects U.S. import reliance, whether source countries\nare allies, and their WGI Political Stability Estimates.",
    x = NULL, # Dropped because mineral names are self-explanatory
    y = "Total Risk Score",
    caption = "Source: 2022 U.S. Department of the Interior & U.S. Geological Survey"
  )

#--------------------SOURCES----------------------------------------------

data$Geopolitical_Category <- factor(
  data$Geopolitical_Category,
  levels = c("Ally", "Neutral", "Competitor")
)



ggplot(data, aes(x = Geopolitical_Category, fill = Geopolitical_Category)) +
  geom_bar(width = 0.7) +
  
  #adding counts directly above the bars
  geom_text(
    stat = "count", 
    aes(label = after_stat(count)), 
    vjust = -0.5, 
    fontface = "bold", 
    color = "#222222",
    size = 4
  ) +
  
  #highlighting ONLY competitors
  scale_fill_manual(
    values = c(
      "Ally" = "#D3D3D3", 
      "Neutral" = "#D3D3D3", 
      "Competitor" = "#B22222"
    )
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_line(color = "#EBEBEB"),
    
    plot.title = element_text(face = "bold", size = 15, margin = margin(b = 15)),
    axis.title.x = element_text(margin = margin(t = 10), face = "bold"),
    axis.title.y = element_text(margin = margin(r = 10), face = "bold"),
    axis.text = element_text(color = "#222222", face = "bold"),
    
    legend.position = "none" 
  ) +
  labs(
    title = "The U.S. Relies Heavily on Competitor Nations for Critical Mineral Supplies",
    x = "Supplier Nation Alignment",
    y = "Number of Minerals Supplied"
  )

#-----------------------COMPETITORS------------------------------------------

competitor_counts <- data %>%
  filter(Geopolitical_Category == "Competitor") %>%
  count(Major_Import_Source, sort = TRUE) %>%
  mutate(Major_Import_Source = factor(Major_Import_Source, levels = c("China", "Russia")))


ggplot(competitor_counts,
       aes(
         x = reorder(Major_Import_Source, n),
         y = n,
         fill = Major_Import_Source
       )) +
  
  geom_col(width = 0.6) +
  
  #adding counts directly above the bars
  geom_text(
    aes(label = n), 
    hjust = -0.3,          
    fontface = "bold", 
    color = "#222222",
    size = 4
  ) +
  
  #highlighting ONLY china
  scale_fill_manual(
    values = c(
      "Russia" = "#D3D3D3", 
      "China" = "#B22222"
    )
  ) +
  
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#EBEBEB"),
    panel.grid.major.y = element_blank(),
    
    plot.title = element_text(face = "bold", size = 15, margin = margin(b = 15)),
    axis.title.x = element_text(margin = margin(t = 10), face = "bold"),
    axis.title.y = element_text(margin = margin(r = 10), face = "bold"),
    axis.text = element_text(color = "#222222", face = "bold"),
    
    legend.position = "none" 
  ) +
  
  labs(
    title = "China Dominates U.S. Critical Mineral Import Dependencies",
    subtitle = "Competitor Nations Supplying U.S. Critical Minerals",
    x = "Competitor Nation",
    y = "Number of Minerals Supplied"
  )


#-------------------------------Minerals By Risk Type and Scores-------------

heat_df <- data %>%
  select(Mineral,
         Geopolitical_Risk,
         Stability_Risk,
         Dependency_Risk) %>%
  pivot_longer(-Mineral,
               names_to = "Risk_Type",
               values_to = "Score")

ggplot(heat_df,
       aes(x = Risk_Type,
           y = Mineral,
           fill = Score)) +
  geom_tile()




heat_df <- minerals_stability %>%
  select(Mineral, Geopolitical_Risk, Stability_Risk, Dependency_Risk, Total_Risk) %>%
  #filtering out rows with missing data 
  filter(!is.na(Total_Risk)) %>%
  #highest risk minerals at the top
  mutate(Mineral = reorder(Mineral, Total_Risk)) %>%
  select(-Total_Risk) %>%
  pivot_longer(-Mineral, names_to = "Risk_Type", values_to = "Score") %>%
  mutate(
    Risk_Type = str_replace(Risk_Type, "_", " ")
  )

#plotting a heatmap
ggplot(heat_df, aes(x = Risk_Type, y = Mineral, fill = Score)) +
  geom_tile(color = "#FFFFFF", linewidth = 0.2) +
  
  scale_fill_gradient(
    low = "beige",    #low risk
    high = "red",   #high risk is red
    breaks = c(0, 1, 2, 3), 
    labels = c("0 (Low)", "1", "2", "3 (Extreme)")
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 15, margin = margin(b = 5)),
    plot.subtitle = element_text(color = "gray30", size = 10, margin = margin(b = 15)),
    axis.text.y = element_text(face = "bold", color = "#222222", size = 8.5),
    axis.text.x = element_text(face = "bold", color = "#222222", size = 10),
    axis.title = element_blank(), # Subtitles and headers make titles redundant
    
    legend.title = element_text(face = "bold", size = 9),
    legend.key.height = unit(1.2, "cm")
  ) +
  
  labs(
    title = "Supply Chain Vulnerability Across U.S. Critical Minerals",
    subtitle = "Risk exposure based on supplier geopolitics, political stability, and import dependence",
    fill = "Risk Intensity"
  )



#-----------------------SECONDARY SOURCES------------------------------------

#Though many of the essential minerals are majorly imported from China, 
#there are secondary sources the U.S. can try to focus on more

#removing "europe" from the dataframe
secondary_sources <- minerals_stability %>%
  filter(Geopolitical_Category == "Competitor" & Major_Import_Source != "Europe") 

#giving each seocndary import source it's own row
secondary_sources <- secondary_sources %>%
  separate_rows(
    Secondary_Import_Sources,
    sep = ", "
  )

#filtering out all empty secondary sources rows
secondary_sources <- secondary_sources %>%
  filter(
    Secondary_Import_Sources != "N/A"
  )

#assigning geopolitical category to each of the secondary sources
#difficult to do so without extensive researc
secondary_sources <- secondary_sources %>%
  mutate(
    Secondary_Geopolitical_Category = case_when(
      
      Secondary_Import_Sources %in% c(
        "Canada",
        "Germany",
        "Japan",
        "United Kingdom",
        "Australia",
        "Belgium",
        "France",
        "Finland",
        "Norway",
        "Republic of Korea",
        "Italy",
        "Spain"
      ) ~ "Ally",
      
      Secondary_Import_Sources %in% c(
        "China",
        "Russia"
      ) ~ "Competitor",
      
      Secondary_Import_Sources %in% c(
        "Brazil",
        "Mexico",
        "South Africa",
        "Indonesia",
        "Morocco",
        "India",
        "Bolivia",
        "Peru",
        "Kazakhstan",
        "Ukraine",
        "Georgia",
        "Chile",
        "Philippines",
        "United Arab Emirates",
        "Vietnam",
        "Senegal",
        "Israel"
      ) ~ "Neutral",
      
      TRUE ~ NA_character_
    )
  )


#filtering to include only ally and neutral nations imports
secondary_sources3 <- secondary_sources %>%
  filter(Secondary_Geopolitical_Category == "Ally" | Secondary_Geopolitical_Category == "Neutral") %>%
  count(
    Secondary_Import_Sources,
    Secondary_Geopolitical_Category
  )


#South Korea appears as the Republic of Korea in the original USGS table
#and when I created the dataset I kept the name but it does not make sense 
#for an audience

secondary_sources3 <- secondary_sources3 %>%
  mutate(Secondary_Import_Sources = case_when(
    Secondary_Import_Sources == "Republic of Korea" ~ "South Korea",
    TRUE ~ Secondary_Import_Sources
  ))

#creating a bar chart of ally/neutral sources for critical minerals
ggplot(
  secondary_sources3,
  aes(
    x = reorder(Secondary_Import_Sources, n),
    y = n,
    fill = Secondary_Geopolitical_Category
  )
) +
  
  geom_col(width = 0.65) +
  
  #adding counts next to each bar
  geom_text(
    aes(label = n), 
    hjust = -0.3, 
    fontface = "bold", 
    color = "#222222", 
    size = 3.5
  ) +
  
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Ally" = "#4682B4",   
      "Neutral" = "#D3D3D3"
    )
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(), 
    panel.grid.major.x = element_line(color = "#EBEBEB"),
    plot.title = element_text(face = "bold", size = 15, margin = margin(b = 5)),
    plot.subtitle = element_text(color = "gray30", size = 10, margin = margin(b = 15)),
    axis.title.x = element_text(margin = margin(t = 10), face = "bold"),
    axis.title.y = element_text(margin = margin(r = 10), face = "bold"),
    axis.text = element_text(color = "#222222", face = "bold"),
    
    #asjusting the legend
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank()
  ) +
  
  labs(
    title = "De-Risking the Supply Chain: Prioritizing Secondary Mineral Suppliers ",
    subtitle = "Secondary import sources for essential minerals currently dependent on competitor nations",
    x = "Secondary Supplier of Critical Minerals",
    y = "Number of Minerals"
  )