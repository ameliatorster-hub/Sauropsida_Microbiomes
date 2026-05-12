install.packages("sf")
library(sf)

#reads layers avialble to you in a geo database
st_layers("/Users/amelia/Documents/Biol601-ATorster/Reptile_Data")

#allows you to read in shape file and a gdb database
Reptile_Rich <- st_read(dsn="/Users/amelia/Documents/Biol601-ATorster/Reptile_Data", layer="Gard_1_7_ranges")

list.files("//Users/amelia/Documents/Biol601-ATorster/Reptile_Data")
#Gard_1_7_ranges.shp main geometry file
#Gard_1_7_ranges.dbf attibute data
#Gard_1_7_ranges.shx spatial index

#to read into R need to use this, will read shapefile, loads data:
shapefile_path <- "/Users/amelia/Documents/Biol601-ATorster/Reptile_Data/Gard_1_7_ranges.shp"
Reptile_Rich <- st_read(shapefile_path)

#view data
head(Reptile_Rich)
#I have data with species name, taxon ID, broad group name, taxa family, area of range, and multipolygons

#tells me how distances and areas are measured. States data is ub EPSG:4326 (WGS 84) in degrees essentially
st_crs(Reptile_Rich)
#there is a way to change it to something else like km but later problem.

#Plot geometries, though maybe avoid quartz due to it sometimes using a lot of memory. quartz()
plot(Reptile_Rich["geometry"]) #maybe don't do this

#saving the plot as a png
png("reptile_map.png", width = 1000, height = 800)
plot(Reptile_Rich["geometry"])
dev.off()


Reptile_Rich_preview <- st_read(shapefile_path, n_max = 10)

head(Reptile_Rich)
library(terra)
library(dplyr)

#land rasters so that I can only look at land
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
library(rnaturalearth)
library(rnaturalearthdata)


#offical line of code working with orginal shp above

Turtle<-read.csv('/Users/amelia/Documents/Biol601-ATorster/Project_AT/Reptile_CSVs/Turtle copy.csv')
Snake<-read.csv('/Users/amelia/Documents/Biol601-ATorster/Project_AT/Reptile_CSVs/Snake copy.csv')
Lizard<-read.csv('/Users/amelia/Documents/Biol601-ATorster/Project_AT/Reptile_CSVs/Lizard copy.csv')
Crocodilia<-read.csv('/Users/amelia/Documents/Biol601-ATorster/Project_AT/Reptile_CSVs/Crocodilia copy.csv')


#add color options
install.packages("viridis")
library(viridis)

install.packages("fields")
library(fields)

#Map with points
Reptile_Rich_proj <- st_transform(Reptile_Rich, crs = "+proj=moll")#Transform to equal-area projection for accurate area computation
Reptile_vect <- vect(Reptile_Rich_proj)#Convert to a SpatVector
r <- rast(Reptile_vect, res = 50000)  # grid cells ~100 km; adjust as needed
overlap_raster <- rasterize(Reptile_vect, r, field = 1, fun = "sum", background = 0)#overlaps rasters
land <- ne_countries(scale = "medium", returnclass = "sf")
land_vect <- vect(st_transform(land, crs(overlap_raster)))

# Mask water cells
overlap_raster_land <- mask(overlap_raster, land_vect)

color_gradient <- rocket(100, direction=-1)

#csv as sf
Turtle_sf <- st_as_sf(Turtle, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs(overlap_raster_land))
Snake_sf <- st_as_sf(Snake, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs(overlap_raster_land))
Lizard_sf <- st_as_sf(Lizard, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs(overlap_raster_land))
Croc_sf <- st_as_sf(Crocodilia, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs(overlap_raster_land))

png("/Users/amelia/Documents/Biol601-ATorster/Project_AT/OfficalMap.png", width = 1200, height = 900, res = 150)
plot(
  overlap_raster_land,
  main = "Reptile Species Density",
  col = color_gradient,  # nice readable color palette
  axes = FALSE,
  breaks = seq(0, max(values(overlap_raster_land), na.rm = TRUE), length.out = 201),
  legend=FALSE
)
lines(land_vect, col="black", lwd=1)

plot(st_geometry(Turtle_sf), add = TRUE, pch = 21, bg = "aquamarine3", cex = 1)
plot(st_geometry(Snake_sf), add = TRUE, pch = 22, bg = "lightcyan3", cex = 1)
plot(st_geometry(Lizard_sf), add = TRUE, pch = 23, bg = "darkolivegreen1", cex = 1)
plot(st_geometry(Croc_sf), add = TRUE, pch = 24, bg = "deepskyblue", cex = 1)

image.plot(
  legend.only = TRUE,
  horizontal=TRUE,
  zlim = range(values(overlap_raster_land), na.rm = TRUE),
  col = color_gradient,
  legend.lab = "Number of Species Overlaping",
  legend.width = 1.2,
  legend.mar = 4.5,
  axis.args = list(
    at = seq(0, max(values(overlap_raster_land), na.rm = TRUE), by = 10),
    labels = seq(0, max(values(overlap_raster_land), na.rm = TRUE), by = 10),
    cex.axis = 0.8
  )
)

legend(
  "topleft",
  legend=c("Crocodilians","Turtles", "Snakes","Lizards"),
  pch=c(24,21,22,23),
  pt.bg=c("deepskyblue","aquamarine3","lightcyan3","darkolivegreen1"),
  pt.cex=1,
  cex=0.8,
  bty="o",
  box.lwd=1.5,
  title="Reptile Groups"
)
dev.off()





#extract raster info at each point
Turtle_vals <- terra::extract(overlap_raster_land, vect(Turtle_sf))
Snake_vals  <- terra::extract(overlap_raster_land, vect(Snake_sf))
Lizard_vals <- terra::extract(overlap_raster_land, vect(Lizard_sf))
Croc_vals   <- terra::extract(overlap_raster_land, vect(Croc_sf))

#add raster values to sf table
Turtle_table <- cbind(Turtle_sf, density = Turtle_vals[,2])  # [,2] to get raster value column
Snake_table  <- cbind(Snake_sf,  density = Snake_vals[,2])
Lizard_table <- cbind(Lizard_sf, density = Lizard_vals[,2])
Croc_table   <- cbind(Croc_sf,   density = Croc_vals[,2])

#one table
all_reptiles <- bind_rows(
  mutate(Turtle_table, Species = "Turtle"),
  mutate(Snake_table,  Species = "Snake"),
  mutate(Lizard_table, Species = "Lizard"),
  mutate(Croc_table,   Species = "Crocodilian")
)

head(all_reptiles)

write.csv(
  all_reptiles, 
  "/Users/amelia/Documents/Biol601-ATorster/Project_AT/Reptile_Raster_Density.csv", 
  row.names = FALSE
)

#making a dotplot all species on plot
library(ggplot2)
ggplot(all_reptiles, aes(x = Species, y = density)) +
  geom_dotlot(binaxis = 'y', stackdir = 'center', dotsize = 0.5) +
  theme_minimal() +
  labs(title = "Reptile Density Distribution by Species",
       x = "Species",
       y = "Species Density")
#turtle
p_turtle<-ggplot(Turtle_table, aes(x = density)) +
  geom_histogram(binwidth = 1, fill = "aquamarine3", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size=3) +
  theme_classic() +
  labs(title = "Turtle Density Distribution",
       x = "Species Density",
       y = "Number of Study Sites")
ggsave(
  filename = "/Users/amelia/Documents/Biol601-ATorster/Project_AT/Density_to_Species/Turtle_density_barplot.png",
  plot = p_turtle,
  width = 6, height = 4, dpi = 150
)
#snake
p_snake<-ggplot(subset(all_reptiles, Species == "Snake"), aes(x = density)) +
  geom_histogram(binwidth = 1, fill = "lightcyan3", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size=3) +
  theme_classic() +
  labs(title = "Snake Density Distribution",
       x = "Species Density",
       y = "Number of Study Sites")
ggsave(
  filename = "/Users/amelia/Documents/Biol601-ATorster/Project_AT/Density_to_Species/Snake_density_barplot.png",
  plot = p_snake,
  width = 6, height = 4, dpi = 150
)
#lizard
p_lizard<-ggplot(subset(all_reptiles, Species == "Lizard"), aes(x = density)) +
  geom_histogram(binwidth = 1, fill = "darkolivegreen1", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size=3)+
  theme_classic() +
  labs(title = "Lizard Density Distribution",
       x = "Species Density",
       y = "Number of Study Sites")
ggsave(
  filename = "/Users/amelia/Documents/Biol601-ATorster/Project_AT/Density_to_Species/Lizard_density_barplot.png",
  plot = p_lizard,
  width = 6, height = 4, dpi = 150
)
#croc
p_croc<-ggplot(subset(all_reptiles, Species == "Crocodilian"), aes(x = density)) +
  geom_histogram(binwidth = 1, fill = "deepskyblue", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size=3) +
  theme_classic() +
  labs(title = "Crocodilian Density Distribution",
       x = "Species Density",
       y = "Number of Study Sites")
ggsave(
  filename = "/Users/amelia/Documents/Biol601-ATorster/Project_AT/Density_to_Species/Crocodile_density_barplot.png",
  plot = p_croc,
  width = 6, height = 4, dpi = 150
)


#statistical significance for each bar plot
#turtle stats
#do summary statistic whats mean, range, max etc.
turtle_mean <- mean(Turtle_table$density, na.rm = TRUE)
turtle_max <- max(Turtle_table$density, na.rm = TRUE)

# Print results
cat("Mean density:", turtle_mean, "\n") #32.875
cat("Max density:", turtle_max, "\n") #148


#snakes
snake_mean <- mean(Snake_table$density, na.rm = TRUE)
snake_max <- max(Snake_table$density, na.rm = TRUE)

# Print results
cat("Mean density:", snake_mean, "\n") #48.3
cat("Max density:", snake_max, "\n") #103

#lizard
lizard_mean <- mean(Lizard_table$density, na.rm = TRUE)
lizard_max <- max(Lizard_table$density, na.rm = TRUE)

# Print results
cat("Mean density:", lizard_mean, "\n") #32
cat("Max density:", lizard_max, "\n") #122


#croc
croc_mean <- mean(Croc_table$density, na.rm = TRUE)
croc_max <- max(Croc_table$density, na.rm = TRUE)

# Print results
cat("Mean density:", croc_mean, "\n") #52.111
cat("Max density:", croc_max, "\n") #92


#making a table
install.packages(c("ggplot2", "gridExtra"))
library(ggplot2)
library(gridExtra)
library(grid)

# Your data frame
summary_df <- data.frame(
  Group = c("Turtle", "Snake", "Lizard", "Crocodile"),
  Mean_Density = c(32.875, 48.3, 32, 52.111),
  Max_Density  = c(148, 103, 122, 92)
)

# Create table graphic
table_plot <- tableGrob(summary_df)

# File path where you want to save the PNG
out_path <- "/Users/amelia/Documents/Biol601-ATorster/Project_AT/density_summary.png"

# Save PNG
png(filename = out_path, width = 800, height = 400, res = 200)
grid::grid.draw(table_plot)
dev.off()

cat("Saved PNG to:", out_path, "\n") 