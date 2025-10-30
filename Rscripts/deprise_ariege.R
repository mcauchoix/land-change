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
#-----------
## RPG 2015-2023 https://entrepot.recherche.data.gouv.fr/dataset.xhtml?persistentId=doi:10.57745/VMYCYM
st_layers("~/Documents/land change/data/RPG/d09.gpkg")
# Lire la couche
rpg_change <- st_read("~/Documents/land change/data/RPG/d09.gpkg")

# 2-Show all
#-----------
mapview(rpg_change, col.regions = "forestgreen", alpha.regions = 0.2) 


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