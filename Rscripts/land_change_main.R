setwd("~/Documents/land change/data/Zheng 2023/")
# 0.Packages
#----------
#install.packages("terra")      # ou install.packages("raster")
# install.packages("viridis")  # palette de couleurs lisible
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")
# install.packages("sf") 
# install.packages("raster")
# install.packages("geodata")
# install.packages("leaflet")
# install.packages("mapview")

library(terra)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(raster)
library(geodata)
library(ggplot2)
library(leaflet)
library(mapview)

# 1.Load data
#----------
## 1.1 Land abandonement from Zheng et al. 2023 (https://doi.org/10.5281/zenodo.8010675)
fichier_tif <- "~/Documents/land change/data/Zheng 2023/france.tif"
zheng <- rast(fichier_tif)
print(zheng)

##1.2 Charger la carte du monde
world <- ne_countries(scale = "medium", returnclass = "sf")

## 2.3 Carte des Friches
st_layers("~/Documents/land change/data/friche/friches-surfaces-2025-10-20.gpkg")

# Lire la couche
friches <- st_read(
  "~/Documents/land change/data/friche/friches-surfaces-2025-10-20.gpkg",
  layer = "friches_surfaces"
)
# corrige les problèmes de géométrie invalides pour garder que l'ariege
# Vérifie la validité
valid_index <- st_is_valid(friches)
# Garde uniquement les géométries valides
friches_valid <- friches[valid_index, ]
# Vérifie le résultat
sum(!st_is_valid(friches_valid)) 
friches_ariege <- st_intersection(friches_valid, ariege)

## 2.4 RPG 2015-2023 https://entrepot.recherche.data.gouv.fr/dataset.xhtml?persistentId=doi:10.57745/VMYCYM
st_layers("~/Documents/land change/data/RPG/d09.gpkg")
# Lire la couche
rpg_change <- st_read("~/Documents/land change/data/RPG/d09.gpkg")
mapview(rpg_change, col.regions = "forestgreen", alpha.regions = 0.2) 

# 2. Plot Ariege
#---------------
fra_adm2 <- geodata::gadm(country="FRA", level=2, path=".")
departements <- st_as_sf(fra_adm2)
ariege <- departements[departements$NAME_2 == "Ariège", ]

# mets les bonne coordonées
friches_ariege <- st_transform(friches_ariege, 4326)
ariege <- st_transform(ariege, 4326)

# Conversion sf → vecteur terra
ariege_vect <- vect(ariege)
# Découpage spatial
zheng_ariege <- mask(crop(zheng, ariege_vect), ariege_vect)
#plot



# Nom du fichier
outfile <- "~/Documents/land change/outputs/abandoned_cropland_ariege_Zheng2023.png"

# Ouvrir le périphérique graphique PNG
png(filename = outfile, width = 2000, height = 2000, res = 300)

# Extraire uniquement les pixels = 1
coords <- xyFromCell(zheng_ariege, which(values(zheng_ariege)==1))
# Plot fond carte
plot(st_geometry(ariege), col="lightgray", border="gray", main="Abandoned Cropland - Ariège")
plot(st_geometry(friches_ariege), add = TRUE, col = "forestgreen", border = "darkgreen")

# Ajouter les pixels rouges
points(coords, col="red", pch=15, cex=0.5)

# Ajouter les villes
plot(st_geometry(cities_ariege), add=TRUE, col="blue", pch=19, cex=0.7)

# Ajouter noms des villes
text(st_coordinates(cities_ariege), labels=cities_ariege$NAME, pos=3, cex=0.6, col="blue")


legend("topright", legend=c("Abandonné"), fill="red", border="black")

# Fermer le périphérique graphique
dev.off()


#2.2 Carte interactive d'ariege
#-------------------
# Si coords est une matrice ou data.frame
pixels <- data.frame(coords)
colnames(pixels) <- c("x", "y")

# --- Carte interactive ---
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%  # Fond clair et propre
  addPolygons(
    data = ariege,
    color = "gray",
    weight = 2,
    fillColor = "lightgray",
    fillOpacity = 0.3,
    group = "Ariège"
  ) %>%
  addPolygons(
    data = friches_ariege,
    color = "darkgreen",
    fillColor = "forestgreen",
    fillOpacity = 0.5,
    weight = 1,
    group = "Friches"
  ) %>%
  addCircleMarkers(
    data = pixels,
    lng = ~x, lat = ~y,
    color = "red",
    fillOpacity = 0.5,
    radius = 2,
    group = "Pixels rouges"
  ) %>%
  addLayersControl(
    overlayGroups = c("Ariège", "Friches", "Pixels rouges"),
    options = layersControlOptions(collapsed = FALSE)
  )

#3. France
#---------

# Créer un polygone de la France entière (union de tous les départements)
france <- st_union(departements)

# Découper le raster sur la France
france_vect <- vect(france)
zheng_france <- mask(crop(zheng, france_vect), france_vect)

# Nom du fichier
outfile <- "~/Documents/land change/outputs/abandoned_cropland_France_Zheng2023.png"

# Ouvrir le périphérique graphique PNG
png(filename = outfile, width = 2000, height = 2000, res = 300)

# Extraire uniquement les pixels = 1
coords <- xyFromCell(zheng_france, which(values(zheng_france)==1))
# Plot fond carte
plot(st_geometry(france), col="lightgray", border="gray", main="Abandoned Cropland - France")

# Ajouter les pixels rouges
points(coords, col="red", pch=15, cex=0.5)

legend("topright", legend=c("Abandonné"), fill="red", border="black")

# Fermer le périphérique graphique
dev.off()

#4-Europe
#--------
## 1.1 Land abandonement from Zheng et al. 2023 (https://doi.org/10.5281/zenodo.8010675)
fichier_tif <- "~/Documents/land change/data/Zheng 2023/europe.tif"
zheng <- rast(fichier_tif)
print(zheng)

# Nom du fichier
outfile <- "~/Documents/land change/outputs/abandoned_cropland_Europe_Zheng2023.png"

# Ouvrir le périphérique graphique PNG
png(filename = outfile, width = 2000, height = 2000, res = 300)
plot(zheng)
dev.off()

#5-Travaill sur le RPG change en ariege

# Supprimer la géométrie
rpg <- st_drop_geometry(rpg_change)
# Afficher les premières lignes
head(rpg)
# dominant culutre in 2023
sort(table(rpg$cult2023),decreasing = T)
sort(table(rpg$cult2015),decreasing = T)
# non déclaré depuis 5 ans
sum(is.na(rpg$cult2023)&is.na(rpg$cult2022)&is.na(rpg$cult2021)&is.na(rpg$cult2020)
      &is.na(rpg$cult2019))

ind=rowSums(is.na(rpg[,4:8]))==5
# code de toute les parcelles non déclarée depuis 5 ans
ab=rpg$id_unique[ind]

rpg_ab=rpg_change[rpg$id_unique %in% ab,]
mapview(rpg_ab, col.regions = "red", alpha.regions = 0.2) 


# XX_Fonction


# Fonction pour afficher un raster binaire sur fond carte du monde
plot_raster_on_map <- function(raster_file, region_name = "Region") {
  
  # Charger le raster
  rast_bin <- rast(raster_file)
  
  # Charger la carte du monde
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Définir l'étendue du raster pour zoom automatique
  ext_r <- ext(rast_bin)
  
  # Découper la carte du monde sur l'étendue du raster
  world_crop <- st_crop(world, xmin=ext_r[1], xmax=ext_r[2], ymin=ext_r[3], ymax=ext_r[4])
  
  # Plot fond carte
  plot(st_geometry(world_crop), col="lightgray", border="gray", main=paste("Abandoned Cropland -", region_name))
  
  # Plot du raster binaire
  plot(rast_bin, col=c("white","red"), add=TRUE, legend=FALSE)
  
  # Ajouter une légende simple
  legend("topright", legend=c("Non abandonné", "Abandonné"), fill=c("white","red"), border="black")
}

#-----------------------------
# Affichage France
#-----------------------------
plot_raster_on_map("france.tif", region_name="France")

#-----------------------------
# Affichage Europe
#-----------------------------
plot_raster_on_map("europe.tif", region_name="Europe")



# 2.Read data