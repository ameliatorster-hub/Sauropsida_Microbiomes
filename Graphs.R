install.packages("readxl")
install.packages("ggplot2")

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)

Wild <- read_excel("/Users/amelia/Documents/Biol601-ATorster/Project_AT/Table_S2_orig.xlsx", sheet = 2) #Wild
Captive <- read_excel("/Users/amelia/Documents/Biol601-ATorster/Project_AT/Table_S2_orig.xlsx", sheet = 3) #Captive
Males <- read_excel("/Users/amelia/Documents/Biol601-ATorster/Project_AT/Table_S2_orig.xlsx", sheet = 4) #Males
Females <- read_excel("/Users/amelia/Documents/Biol601-ATorster/Project_AT/Table_S2_orig.xlsx", sheet = 5) #Females
Juvenile<- read_excel("/Users/amelia/Documents/Biol601-ATorster/Project_AT/Table_S2_orig.xlsx", sheet = 6) #Juveniles
Adult <- read_excel("/Users/amelia/Documents/Biol601-ATorster/Project_AT/Table_S2_orig.xlsx", sheet = 7) #Adult
Healthy <- read_excel("/Users/amelia/Documents/Biol601-ATorster/Project_AT/Table_S2_orig.xlsx", sheet = 8) #Healthy
Sick <- read_excel("/Users/amelia/Documents/Biol601-ATorster/Project_AT/Table_S2_orig.xlsx", sheet = 9) #Sick
Non_polluted <- read_excel("/Users/amelia/Documents/Biol601-ATorster/Project_AT/Table_S2_orig.xlsx", sheet = 10) #nonpolluted
Polluted <- read_excel("/Users/amelia/Documents/Biol601-ATorster/Project_AT/Table_S2_orig.xlsx", sheet = 11) #Polluted
Snakes <- read_excel("/Users/amelia/Documents/Biol601-ATorster/Project_AT/Table_S2_orig.xlsx", sheet = 13) #Snakes
Lizards<- read_excel("/Users/amelia/Documents/Biol601-ATorster/Project_AT/Table_S2_orig.xlsx", sheet = 14) #Lizards
Crocs <- read_excel("/Users/amelia/Documents/Biol601-ATorster/Project_AT/Table_S2_orig.xlsx", sheet = 15) #Crocs
Turtles <- read_excel("/Users/amelia/Documents/Biol601-ATorster/Project_AT/Table_S2_orig.xlsx", sheet = 16) #Turtles
Passerine <- read_excel("/Users/amelia/Documents/Biol601-ATorster/Project_AT/Table_S2_orig.xlsx", sheet = 17) #Passerine
Non_Passerine <- read_excel("/Users/amelia/Documents/Biol601-ATorster/Project_AT/Table_S2_orig.xlsx", sheet = 18) #Non-Passerine

#Colors for the graphs
custom_colors <- c(
  "Firmicutes" = "#A289BB",
  "Bacteroidetes" = "#E6BB28",
  "Proteobacteria" = "#A4B392",
  "Actinobacteria" = "#F2A346",
  "Fusobacteria" = "#C995A8",
  "Verrucomicrobia" = "darkgreen",
  "Cyanobacteria"="#715A97",
  "Deinococcota"="#614B0F",
  "Campylobacterota"="#BE6400",
  "Tenericutes"="#85B0C1",
  "Pseduomonas"="#2E6F40",
  "Chloroflexi"="#6488ea",
  "Desulfobacteria"="#E07A1F",
  "Chlaymdiae"="#BC9E82",
  "Patescibacteria"="#3B719F",
  "Gracilibacteria"="#A9588C"
)



colnames(Wild)[1] <- "Group"

# Fill missing group labels and remove empty rows
Wild <- Wild %>%
  fill(Group) %>%
  filter(!is.na(Group))

# Create ordered samples
Wild <- Wild %>%
  mutate(
    Order = row_number(),
    Sample = paste0("Sample_", Order))

# Convert to long format
Wild_long <- Wild %>%
  pivot_longer(
    cols = -c(Group, Sample, Order),
    names_to = "Phylum",
    values_to = "Count")

# Set stack order (bottom -> top)
Wild_long$Phylum <- factor(
  Wild_long$Phylum,
  levels = c(
    "Gracilibacteria", "Patescibacteria","Chlaymdiae", "Desulfobacteria", "Cyanobacteria", "Chloroflexi", "Deinococcota", "Campylobacterota","Verrucomicrobia" ,"Pseduomonas","Tenericutes","Firmicutes","Fusobacteria","Bacteroidetes","Actinobacteria","Proteobacteria"))

# Preserve original sample order
Wild_long$Sample <- factor(
  Wild_long$Sample,
  levels = unique(Wild$Sample))

# Plot
png(
  filename = "/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Wild.png",
  width = 10,
  height = 6,
  units = "in",
  res = 300)

ggplot(
  Wild_long,
  aes(
    x = Sample,
    y = Count,
    fill = Phylum )) +
  geom_bar(stat = "identity") +
  
  geom_text(
    aes(label = ifelse(Count > 0, Count, "")),
    position = position_stack(vjust = 0.5),
    size = 2.5,
    color = "black") +
  
  scale_fill_manual(values = custom_colors) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 8 ),
    
    axis.title.x = element_blank(),
    
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.key.size = unit(0.4, "cm"),
    
    plot.title = element_text(
      hjust = 0.5,
      face = "bold")) +
  
  labs( title = "Wild",y = "Number of Mentions" ) +
  
  scale_x_discrete( labels = c("Most Dominant (1)","2", "3","4","Least Dominant (5)") )

dev.off()

