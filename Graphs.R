install.packages("readxl")
install.packages("ggplot2")

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)

Wild <- read_excel("/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Table_S2.xlsx", sheet = 2) #Wild
Captive <- read_excel("/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Table_S2.xlsx", sheet = 3) #Captive
Males <- read_excel("/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Table_S2.xlsx", sheet = 4) #Males
Females <- read_excel("/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Table_S2.xlsx", sheet = 5) #Females
Juvenile<- read_excel("/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Table_S2.xlsx", sheet = 6) #Juveniles
Adult <- read_excel("/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Table_S2.xlsx", sheet = 7) #Adult
Healthy <- read_excel("/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Table_S2.xlsx", sheet = 8) #Healthy
Sick <- read_excel("/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Table_S2.xlsx", sheet = 9) #Sick
Non_polluted <- read_excel("/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Table_S2.xlsx", sheet = 10) #nonpolluted
Polluted <- read_excel("/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Table_S2.xlsx", sheet = 11) #Polluted
Snakes <- read_excel("/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Table_S2.xlsx", sheet = 13) #Snakes
Lizards<- read_excel("/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Table_S2.xlsx", sheet = 14) #Lizards
Crocs <- read_excel("/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Table_S2.xlsx", sheet = 15) #Crocs
Turtles <- read_excel("/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Table_S2.xlsx", sheet = 16) #Turtles
Passerine <- read_excel("/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Table_S2.xlsx", sheet = 17) #Passerine
Non_Passerine <- read_excel("/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Table_S2.xlsx", sheet = 18) #Non-Passerine

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


#WILD
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



#Captive
colnames(Captive)[1] <- "Group"

# Fill missing group labels and remove empty rows
Captive <- Captive %>%
  fill(Group) %>%
  filter(!is.na(Group))

# Create ordered samples
Captive <- Captive %>%
  mutate(
    Order = row_number(),
    Sample = paste0("Sample_", Order))

# Convert to long format
Captive_long <- Captive %>%
  pivot_longer(
    cols = -c(Group, Sample, Order),
    names_to = "Phylum",
    values_to = "Count")

# Set stack order (bottom -> top)
Captive_long$Phylum <- factor(
  Captive_long$Phylum,
  levels = c(
    "Gracilibacteria", "Patescibacteria","Chlaymdiae", "Desulfobacteria", "Cyanobacteria", "Chloroflexi", "Deinococcota", "Campylobacterota","Verrucomicrobia" ,"Pseduomonas","Tenericutes","Firmicutes","Fusobacteria","Bacteroidetes","Actinobacteria","Proteobacteria"))

# Preserve original sample order
Captive_long$Sample <- factor(
  Captive_long$Sample,
  levels = unique(Captive$Sample))

# Plot
png(
  filename = "/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Captive.png",
  width = 10,
  height = 6,
  units = "in",
  res = 300)

ggplot(
  Captive_long,
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
  
  labs( title = "Captive",y = "Number of Mentions" ) +
  
  scale_x_discrete( labels = c("Most Dominant (1)","2", "3","4","Least Dominant (5)") )

dev.off()


#Males
colnames(Males)[1] <- "Group"

# Fill missing group labels and remove empty rows
Males<- Males %>%
  fill(Group) %>%
  filter(!is.na(Group))

# Create ordered samples
Males <- Males %>%
  mutate(
    Order = row_number(),
    Sample = paste0("Sample_", Order))

# Convert to long format
Males_long <- Males %>%
  pivot_longer(
    cols = -c(Group, Sample, Order),
    names_to = "Phylum",
    values_to = "Count")

# Set stack order (bottom -> top)
Males_long$Phylum <- factor(
  Males_long$Phylum,
  levels = c(
    "Gracilibacteria", "Patescibacteria","Chlaymdiae", "Desulfobacteria", "Cyanobacteria", "Chloroflexi", "Deinococcota", "Campylobacterota","Verrucomicrobia" ,"Pseduomonas","Tenericutes","Firmicutes","Fusobacteria","Bacteroidetes","Actinobacteria","Proteobacteria"))

# Preserve original sample order
Males_long$Sample <- factor(
  Males_long$Sample,
  levels = unique(Males$Sample))

# Plot
png(
  filename = "/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Males.png",
  width = 10,
  height = 6,
  units = "in",
  res = 300)

ggplot(
  Males_long,
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
  
  labs( title = "Males",y = "Number of Mentions" ) +
  
  scale_x_discrete( labels = c("Most Dominant","Least Dominant") )

dev.off()



#Females
colnames(Females)[1] <- "Group"

# Fill missing group labels and remove empty rows
Females<- Females %>%
  fill(Group) %>%
  filter(!is.na(Group))

# Create ordered samples
Females <- Females %>%
  mutate(
    Order = row_number(),
    Sample = paste0("Sample_", Order))

# Convert to long format
Females_long <- Females %>%
  pivot_longer(
    cols = -c(Group, Sample, Order),
    names_to = "Phylum",
    values_to = "Count")

# Set stack order (bottom -> top)
Females_long$Phylum <- factor(
  Females_long$Phylum,
  levels = c(
    "Gracilibacteria", "Patescibacteria","Chlaymdiae", "Desulfobacteria", "Cyanobacteria", "Chloroflexi", "Deinococcota", "Campylobacterota","Verrucomicrobia" ,"Pseduomonas","Tenericutes","Firmicutes","Fusobacteria","Bacteroidetes","Actinobacteria","Proteobacteria"))

# Preserve original sample order
Females_long$Sample <- factor(
  Females_long$Sample,
  levels = unique(Females$Sample))

# Plot
png(
  filename = "/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Females.png",
  width = 10,
  height = 6,
  units = "in",
  res = 300)

ggplot(
  Females_long,
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
  
  labs( title = "Females",y = "Number of Mentions" ) +
  
  scale_x_discrete( labels = c("Most Dominant (1)","2","Least Dominant (3)") )

dev.off()




#Juveniles
colnames(Juvenile)[1] <- "Group"

# Fill missing group labels and remove empty rows
Juvenile<- Juvenile %>%
  fill(Group) %>%
  filter(!is.na(Group))

# Create ordered samples
Juvenile <- Juvenile %>%
  mutate(
    Order = row_number(),
    Sample = paste0("Sample_", Order))

# Convert to long format
Juvenile_long <- Juvenile %>%
  pivot_longer(
    cols = -c(Group, Sample, Order),
    names_to = "Phylum",
    values_to = "Count")

# Set stack order (bottom -> top)
Juvenile_long$Phylum <- factor(
  Juvenile_long$Phylum,
  levels = c(
    "Gracilibacteria", "Patescibacteria","Chlaymdiae", "Desulfobacteria", "Cyanobacteria", "Chloroflexi", "Deinococcota", "Campylobacterota","Verrucomicrobia" ,"Pseduomonas","Tenericutes","Firmicutes","Fusobacteria","Bacteroidetes","Actinobacteria","Proteobacteria"))

# Preserve original sample order
Juvenile_long$Sample <- factor(
  Juvenile_long$Sample,
  levels = unique(Juvenile$Sample))

# Plot
png(
  filename = "/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Juvenile.png",
  width = 10,
  height = 6,
  units = "in",
  res = 300)

ggplot(
  Juvenile_long,
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
  
  labs( title = "Juvenile",y = "Number of Mentions" ) +
  
  scale_x_discrete( labels = c("Most Dominant (1)","2", "3","4","Least Dominant (5)") )

dev.off()


 #Adult
colnames(Adult)[1] <- "Group"

# Fill missing group labels and remove empty rows
Adult<- Adult %>%
  fill(Group) %>%
  filter(!is.na(Group))

# Create ordered samples
Adult <- Adult %>%
  mutate(
    Order = row_number(),
    Sample = paste0("Sample_", Order))

# Convert to long format
Adult_long <- Adult %>%
  pivot_longer(
    cols = -c(Group, Sample, Order),
    names_to = "Phylum",
    values_to = "Count")

# Set stack order (bottom -> top)
Adult_long$Phylum <- factor(
  Adult_long$Phylum,
  levels = c(
    "Gracilibacteria", "Patescibacteria","Chlaymdiae", "Desulfobacteria", "Cyanobacteria", "Chloroflexi", "Deinococcota", "Campylobacterota","Verrucomicrobia" ,"Pseduomonas","Tenericutes","Firmicutes","Fusobacteria","Bacteroidetes","Actinobacteria","Proteobacteria"))

# Preserve original sample order
Adult_long$Sample <- factor(
  Adult_long$Sample,
  levels = unique(Adult$Sample))

# Plot
png(
  filename = "/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Adult.png",
  width = 10,
  height = 6,
  units = "in",
  res = 300)

ggplot(
  Adult_long,
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
  
  labs( title = "Adult",y = "Number of Mentions" ) +
  
  scale_x_discrete( labels = c("Most Dominant (1)","2", "3","4","Least Dominant (5)") )

dev.off()


 #Healthy
colnames(Healthy)[1] <- "Group"

# Fill missing group labels and remove empty rows
Healthy <- Healthy %>%
  fill(Group) %>%
  filter(!is.na(Group))

# Create ordered samples
Healthy <-Healthy %>%
  mutate(
    Order = row_number(),
    Sample = paste0("Sample_", Order))

# Convert to long format
Healthy_long <- Healthy %>%
  pivot_longer(
    cols = -c(Group, Sample, Order),
    names_to = "Phylum",
    values_to = "Count")

# Set stack order (bottom -> top)
Healthy_long$Phylum <- factor(
  Healthy_long$Phylum,
  levels = c(
    "Gracilibacteria", "Patescibacteria","Chlaymdiae", "Desulfobacteria", "Cyanobacteria", "Chloroflexi", "Deinococcota", "Campylobacterota","Verrucomicrobia" ,"Pseduomonas","Tenericutes","Firmicutes","Fusobacteria","Bacteroidetes","Actinobacteria","Proteobacteria"))

# Preserve original sample order
Healthy_long$Sample <- factor(
  Healthy_long$Sample,
  levels = unique(Healthy$Sample))

# Plot
png(
  filename = "/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Healthy.png",
  width = 10,
  height = 6,
  units = "in",
  res = 300)

ggplot(
  Healthy_long,
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
  
  labs( title = "Healthy",y = "Number of Mentions" ) +
  
  scale_x_discrete( labels = c("Most Dominant (1)","2", "3","4","Least Dominant (5)") )

dev.off()


#Sick
colnames(Sick)[1] <- "Group"

# Fill missing group labels and remove empty rows
Sick<- Sick%>%
  fill(Group) %>%
  filter(!is.na(Group))

# Create ordered samples
Sick <- Sick%>%
  mutate(
    Order = row_number(),
    Sample = paste0("Sample_", Order))

# Convert to long format
Sick_long <- Sick%>%
  pivot_longer(
    cols = -c(Group, Sample, Order),
    names_to = "Phylum",
    values_to = "Count")

# Set stack order (bottom -> top)
Sick_long$Phylum <- factor(
  Sick_long$Phylum,
  levels = c(
    "Gracilibacteria", "Patescibacteria","Chlaymdiae", "Desulfobacteria", "Cyanobacteria", "Chloroflexi", "Deinococcota", "Campylobacterota","Verrucomicrobia" ,"Pseduomonas","Tenericutes","Firmicutes","Fusobacteria","Bacteroidetes","Actinobacteria","Proteobacteria"))

# Preserve original sample order
Sick_long$Sample <- factor(
  Sick_long$Sample,
  levels = unique(Sick$Sample))

# Plot
png(
  filename = "/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Sick.png",
  width = 10,
  height = 6,
  units = "in",
  res = 300)

ggplot(
  Sick_long,
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
  
  labs( title = "Sick",y = "Number of Mentions" ) +
  
  scale_x_discrete( labels = c("Most Dominant (1)","2", "3", "4", "Least Dominant (5)") )

dev.off()




 #nonpolluted
colnames(Non_polluted)[1] <- "Group"

# Fill missing group labels and remove empty rows
Non_polluted<- Non_polluted%>%
  fill(Group) %>%
  filter(!is.na(Group))

# Create ordered samples
Non_polluted <- Non_polluted%>%
  mutate(
    Order = row_number(),
    Sample = paste0("Sample_", Order))

# Convert to long format
Non_polluted_long <- Non_polluted%>%
  pivot_longer(
    cols = -c(Group, Sample, Order),
    names_to = "Phylum",
    values_to = "Count")

# Set stack order (bottom -> top)
Non_polluted_long$Phylum <- factor(
  Non_polluted_long$Phylum,
  levels = c(
    "Gracilibacteria", "Patescibacteria","Chlaymdiae", "Desulfobacteria", "Cyanobacteria", "Chloroflexi", "Deinococcota", "Campylobacterota","Verrucomicrobia" ,"Pseduomonas","Tenericutes","Firmicutes","Fusobacteria","Bacteroidetes","Actinobacteria","Proteobacteria"))

# Preserve original sample order
Non_polluted_long$Sample <- factor(
  Non_polluted_long$Sample,
  levels = unique(Non_polluted$Sample))

# Plot
png(
  filename = "/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Non_polluted.png",
  width = 10,
  height = 6,
  units = "in",
  res = 300)

ggplot(
  Non_polluted_long,
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
  
  labs( title = "Non-polluted",y = "Number of Mentions" ) +
  
  scale_x_discrete( labels = c("Most Dominant (1)","2", "3", "4", "Least Dominant (5)") )

dev.off()




 #Polluted
colnames(Polluted)[1] <- "Group"

# Fill missing group labels and remove empty rows
Polluted<- Polluted%>%
  fill(Group) %>%
  filter(!is.na(Group))

# Create ordered samples
Polluted <- Polluted%>%
  mutate(
    Order = row_number(),
    Sample = paste0("Sample_", Order))

# Convert to long format
Polluted_long <- Polluted%>%
  pivot_longer(
    cols = -c(Group, Sample, Order),
    names_to = "Phylum",
    values_to = "Count")

# Set stack order (bottom -> top)
Polluted_long$Phylum <- factor(
  Polluted_long$Phylum,
  levels = c(
    "Gracilibacteria", "Patescibacteria","Chlaymdiae", "Desulfobacteria", "Cyanobacteria", "Chloroflexi", "Deinococcota", "Campylobacterota","Verrucomicrobia" ,"Pseduomonas","Tenericutes","Firmicutes","Fusobacteria","Bacteroidetes","Actinobacteria","Proteobacteria"))

# Preserve original sample order
Polluted_long$Sample <- factor(
  Polluted_long$Sample,
  levels = unique(Polluted$Sample))

# Plot
png(
  filename = "/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Polluted.png",
  width = 10,
  height = 6,
  units = "in",
  res = 300)

ggplot(
  Polluted_long,
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
  
  labs( title = "Polluted",y = "Number of Mentions" ) +
  
  scale_x_discrete( labels = c("Most Dominant (1)","2", "3", "4", "Least Dominant (5)") )

dev.off()





 #Snakes
colnames(Snakes)[1] <- "Group"

# Fill missing group labels and remove empty rows
Snakes<- Snakes%>%
  fill(Group) %>%
  filter(!is.na(Group))

# Create ordered samples
Snakes <- Snakes%>%
  mutate(
    Order = row_number(),
    Sample = paste0("Sample_", Order))

# Convert to long format
Snakes_long <- Snakes%>%
  pivot_longer(
    cols = -c(Group, Sample, Order),
    names_to = "Phylum",
    values_to = "Count")

# Set stack order (bottom -> top)
Snakes_long$Phylum <- factor(
  Snakes_long$Phylum,
  levels = c(
    "Gracilibacteria", "Patescibacteria","Chlaymdiae", "Desulfobacteria", "Cyanobacteria", "Chloroflexi", "Deinococcota", "Campylobacterota","Verrucomicrobia" ,"Pseduomonas","Tenericutes","Firmicutes","Fusobacteria","Bacteroidetes","Actinobacteria","Proteobacteria"))

# Preserve original sample order
Snakes_long$Sample <- factor(
  Snakes_long$Sample,
  levels = unique(Snakes$Sample))

# Plot
png(
  filename = "/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Snakes.png",
  width = 10,
  height = 6,
  units = "in",
  res = 300)

ggplot(
  Snakes_long,
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
  
  labs( title = "Snakes",y = "Number of Mentions" ) +
  
  scale_x_discrete( labels = c("Most Dominant (1)","2", "3", "4", "Least Dominant (5)") )

dev.off()



 #Lizards
colnames(Lizards)[1] <- "Group"

# Fill missing group labels and remove empty rows
Lizards<- Lizards%>%
  fill(Group) %>%
  filter(!is.na(Group))

# Create ordered samples
Lizards <- Lizards%>%
  mutate(
    Order = row_number(),
    Sample = paste0("Sample_", Order))

# Convert to long format
Lizards_long <- Lizards%>%
  pivot_longer(
    cols = -c(Group, Sample, Order),
    names_to = "Phylum",
    values_to = "Count")

# Set stack order (bottom -> top)
Lizards_long$Phylum <- factor(
  Lizards_long$Phylum,
  levels = c(
    "Gracilibacteria", "Patescibacteria","Chlaymdiae", "Desulfobacteria", "Cyanobacteria", "Chloroflexi", "Deinococcota", "Campylobacterota","Verrucomicrobia" ,"Pseduomonas","Tenericutes","Firmicutes","Fusobacteria","Bacteroidetes","Actinobacteria","Proteobacteria"))

# Preserve original sample order
Lizards_long$Sample <- factor(
  Lizards_long$Sample,
  levels = unique(Lizards$Sample))

# Plot
png(
  filename = "/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Lizards.png",
  width = 10,
  height = 6,
  units = "in",
  res = 300)

ggplot(
  Lizards_long,
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
  
  labs( title = "Lizards",y = "Number of Mentions" ) +
  
  scale_x_discrete( labels = c("Most Dominant (1)","2", "3", "4", "Least Dominant (5)") )

dev.off()




 #Crocs
colnames(Crocs)[1] <- "Group"

# Fill missing group labels and remove empty rows
Crocs<- Crocs%>%
  fill(Group) %>%
  filter(!is.na(Group))

# Create ordered samples
Crocs <- Crocs%>%
  mutate(
    Order = row_number(),
    Sample = paste0("Sample_", Order))

# Convert to long format
Crocs_long <- Crocs%>%
  pivot_longer(
    cols = -c(Group, Sample, Order),
    names_to = "Phylum",
    values_to = "Count")

# Set stack order (bottom -> top)
Crocs_long$Phylum <- factor(
  Crocs_long$Phylum,
  levels = c(
    "Gracilibacteria", "Patescibacteria","Chlaymdiae", "Desulfobacteria", "Cyanobacteria", "Chloroflexi", "Deinococcota", "Campylobacterota","Verrucomicrobia" ,"Pseduomonas","Tenericutes","Firmicutes","Fusobacteria","Bacteroidetes","Actinobacteria","Proteobacteria"))

# Preserve original sample order
Crocs_long$Sample <- factor(
  Crocs_long$Sample,
  levels = unique(Crocs$Sample))

# Plot
png(
  filename = "/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Crocs.png",
  width = 10,
  height = 6,
  units = "in",
  res = 300)

ggplot(
  Crocs_long,
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
  
  labs( title = "Crocs",y = "Number of Mentions" ) +
  
  scale_x_discrete( labels = c("Most Dominant (1)","2", "3", "4", "Least Dominant (5)") )

dev.off()




 #Turtles
colnames(Turtles)[1] <- "Group"

# Fill missing group labels and remove empty rows
Turtles<- Turtles%>%
  fill(Group) %>%
  filter(!is.na(Group))

# Create ordered samples
Turtles <- Turtles%>%
  mutate(
    Order = row_number(),
    Sample = paste0("Sample_", Order))

# Convert to long format
Turtles_long <- Turtles%>%
  pivot_longer(
    cols = -c(Group, Sample, Order),
    names_to = "Phylum",
    values_to = "Count")

# Set stack order (bottom -> top)
Turtles_long$Phylum <- factor(
  Turtles_long$Phylum,
  levels = c(
    "Gracilibacteria", "Patescibacteria","Chlaymdiae", "Desulfobacteria", "Cyanobacteria", "Chloroflexi", "Deinococcota", "Campylobacterota","Verrucomicrobia" ,"Pseduomonas","Tenericutes","Firmicutes","Fusobacteria","Bacteroidetes","Actinobacteria","Proteobacteria"))

# Preserve original sample order
Turtles_long$Sample <- factor(
  Turtles_long$Sample,
  levels = unique(Turtles$Sample))

# Plot
png(
  filename = "/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Turtles.png",
  width = 10,
  height = 6,
  units = "in",
  res = 300)

ggplot(
  Turtles_long,
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
  
  labs( title = "Turtles",y = "Number of Mentions" ) +
  
  scale_x_discrete( labels = c("Most Dominant (1)","2", "3", "4", "Least Dominant (5)") )

dev.off()




 #Passerine
colnames(Passerine)[1] <- "Group"

# Fill missing group labels and remove empty rows
Passerine<- Passerine%>%
  fill(Group) %>%
  filter(!is.na(Group))

# Create ordered samples
Passerine <- Passerine%>%
  mutate(
    Order = row_number(),
    Sample = paste0("Sample_", Order))

# Convert to long format
Passerine_long <- Passerine%>%
  pivot_longer(
    cols = -c(Group, Sample, Order),
    names_to = "Phylum",
    values_to = "Count")

# Set stack order (bottom -> top)
Passerine_long$Phylum <- factor(
  Passerine_long$Phylum,
  levels = c(
    "Gracilibacteria", "Patescibacteria","Chlaymdiae", "Desulfobacteria", "Cyanobacteria", "Chloroflexi", "Deinococcota", "Campylobacterota","Verrucomicrobia" ,"Pseduomonas","Tenericutes","Firmicutes","Fusobacteria","Bacteroidetes","Actinobacteria","Proteobacteria"))

# Preserve original sample order
Passerine_long$Sample <- factor(
  Passerine_long$Sample,
  levels = unique(Passerine$Sample))

# Plot
png(
  filename = "/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Passerine.png",
  width = 10,
  height = 6,
  units = "in",
  res = 300)

ggplot(
  Passerine_long,
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
  
  labs( title = "Passerine",y = "Number of Mentions" ) +
  
  scale_x_discrete( labels = c("Most Dominant (1)","2", "3", "4", "Least Dominant (5)") )

dev.off()


#Non-Passerine
colnames(Non_Passerine)[1] <- "Group"

# Fill missing group labels and remove empty rows
Non_Passerine<- Non_Passerine%>%
  fill(Group) %>%
  filter(!is.na(Group))

# Create ordered samples
Non_Passerine <- Non_Passerine%>%
  mutate(
    Order = row_number(),
    Sample = paste0("Sample_", Order))

# Convert to long format
Non_Passerine_long <- Non_Passerine%>%
  pivot_longer(
    cols = -c(Group, Sample, Order),
    names_to = "Phylum",
    values_to = "Count")

# Set stack order (bottom -> top)
Non_Passerine_long$Phylum <- factor(
  Non_Passerine_long$Phylum,
  levels = c(
    "Gracilibacteria", "Patescibacteria","Chlaymdiae", "Desulfobacteria", "Cyanobacteria", "Chloroflexi", "Deinococcota", "Campylobacterota","Verrucomicrobia" ,"Pseduomonas","Tenericutes","Firmicutes","Fusobacteria","Bacteroidetes","Actinobacteria","Proteobacteria"))

# Preserve original sample order
Non_Passerine_long$Sample <- factor(
  Non_Passerine_long$Sample,
  levels = unique(Non_Passerine$Sample))

# Plot
png(
  filename = "/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Non_Passerine.png",
  width = 10,
  height = 6,
  units = "in",
  res = 300)

ggplot(
  Non_Passerine_long,
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
  
  labs( title = "Non-Passerine",y = "Number of Mentions" ) +
  
  scale_x_discrete( labels = c("Most Dominant (1)","2", "3", "4", "Least Dominant (5)") )

dev.off()

#Alpha Diversities
png("/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Alpha_Diversity_Barplot.png", width = 3000,
    height = 2400,
    res = 300)
Alpha<-data.frame(Metric=c("Shannon", "Chao1", "Simpson", "Ace", "Faith", "Other"), Value=c(53, 31,17,17,10,53))
Alpha$Metric <- factor(
  Alpha$Metric,
  levels = Alpha$Metric
)
ggplot(Alpha, aes(x = Metric, y = Value)) +
  geom_bar(stat = "identity", fill = "pink3") +
  geom_text(aes(label = Value), vjust = 0.5) +
  labs(
    title = "Alpha Diversities",
    x = "",
    y = "Number of times Metric is Used"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )
dev.off()

#Beta Diversities
png("/Users/amelia/Documents/Reserach Microbiome Work/Sauropsida_Microbiomes/Beta_Diversity_Barplot.png", width = 3000,
    height = 2400,
    res = 300)
Beta<-data.frame(Metric=c("Bray-Curtis", "UniFrac", "Other"), Value=c(34, 27,39))
Beta$Metric <- factor(
  Beta$Metric,
  levels = Beta$Metric
)
ggplot(Beta, aes(x = Metric, y = Value)) +
  geom_bar(stat = "identity", fill = "lightblue3") +
  geom_text(aes(label = Value), vjust = 0.5) +
  labs(
    title = "Beta Diversities",
    x = "",
    y = "Number of times Metric is Used"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )
dev.off()
