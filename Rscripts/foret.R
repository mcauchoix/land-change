# 0.Packages
#----------

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
library(dplyr)
library(pbapply)

# 1.Load data
#-----------
## BD topo 
communes_file="~/Documents/land change/data/divers/BDTOPO_3-5_TOUSTHEMES_SHP_LAMB93_D009_2025-09-15/BDTOPO/1_DONNEES_LIVRAISON_2025-09-00199/BDT_3-5_SHP_LAMB93_D009_ED2025-09-15/ADMINISTRATIF/COMMUNE.shp"
communes=st_read(communes_file)
names(communes)      # Liste des attributs disponibles
head(communes)       # Aperçu des premières lignes
st_crs(communes)     # Système de coordonnées (souvent EPSG:2154)
#plot(st_geometry(communes), border="gray")
#communes_wgs84 <- st_transform(communes, crs = 4326)
#mapview(communes_wgs84)
gajan <- communes[communes$NOM=="Gajan",]
moulis <- communes[communes$NOM=="Moulis",]
plot(gajan$geometry)


## Foret anciennes
foret_anc_file="~/Documents/land change/data/divers/FORETS-ANCIENNES_1-0__GPKG_LAMB93_D009_2025-05-01/foret_ancienne_09.gpkg"
st_layers(foret_anc_file)
# Lire la couche R
foret_anc <- st_read(foret_anc_file,layer = "foret_ancienne_09")
#explore
foret_anc_gajan <- st_intersection(rpg_change, gajan)
mapview(foret_anc, zcol = "Nature", alpha.regions = 0.2) 

head(foret_anc)
unique(foret_anc$Nature)
