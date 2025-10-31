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
#install.packages("pbapply")


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


## RPG 2015-2023 https://entrepot.recherche.data.gouv.fr/dataset.xhtml?persistentId=doi:10.57745/VMYCYM
st_layers("~/Documents/land change/data/RPG/d09.gpkg")
# Lire la couche R
rpg_change <- st_read("~/Documents/land change/data/RPG/d09.gpkg")
#explore
rpg_gajan <- st_intersection(rpg_change, gajan)
mapview(rpg_gajan, col.regions = "forestgreen", alpha.regions = 0.2) +
  mapview(gajan, col.regions = NA, color = "black", lwd = 2, 
          layer.name = "Limite de Gajan")

## Occupation des sols 2022
OCSfile="/Users/maxime.cauchoix/Documents/land\ change/data/divers/OCS-GE_2-0__GPKG_LAMB93_D009_2022-01-01/OCS-GE/1_DONNEES_LIVRAISON_2025-05-00080/OCSGE_2-0_GPKG_LAMB93_D09-2022/OCCUPATION_SOL.gpkg"
codes_ocs <- read.csv("/Users/maxime.cauchoix/Documents/land change/data/divers/OCS-GE_2-0__GPKG_LAMB93_D009_2022-01-01/OCS-GE/3_SUPPLEMENTS_LIVRAISON_2025-05-00080/NomenclatureOCSGE.csv", sep=";", stringsAsFactors = FALSE)
codes_ocs=codes_ocs[,1:2]
names(codes_ocs)=c("code_cs","type")
st_layers(OCSfile)
ocs <- st_read(OCSfile)

# Fusion avec ton objet spatial
ocs <- merge(ocs, codes_ocs, by = "code_cs", all.x = TRUE)

#explore
head(ocs)
names(ocs)
ocs_gajan <- st_intersection(ocs, gajan)
ocs_gajan=ocs_gajan[,c("type","the_geom")]
mapview(ocs_gajan, zcol="type", layer.name="Code CS", legend=TRUE)


# 2-Show all
#-------------
mapview(rpg_gajan, col.regions = "red", alpha.regions = 0.2) + 
  mapview(ocs_gajan, zcol="type", layer.name="Code CS", legend=TRUE)+
  mapview(gajan, col.regions = NA, color = "black", lwd = 2, 
          layer.name = "Limite de Gajan")

# 3-Show et compute sol occupation for non RPG surface
#--------------------------------------
ocs_non_rpg <- st_difference(ocs_gajan, st_union(rpg_gajan))
mapview(ocs_non_rpg, zcol="type", layer.name="Code CS", legend=TRUE)+
  mapview(rpg_gajan, col.regions = "red", alpha.regions = 0.2)
# compute % of each CS 
# Ajouter une colonne et calcul la surface en m² pour chaque polygon
ocs_non_rpg <- ocs_non_rpg %>%
  mutate(surface_m2 = st_area(the_geom))

# Somme des surfaces par type OCS
surf_type <- ocs_non_rpg %>%
  st_drop_geometry() %>%          # Ignorer la géométrie pour le calcul
  group_by(type) %>%
  summarise(surface_totale_m2 = sum(as.numeric(surface_m2))) %>%
  ungroup()
# fait le %
surf_type <- surf_type %>%
  mutate(pourcentage = surface_totale_m2 / sum(surface_totale_m2) * 100)

# 4-years of andandonement
#-------------
# Colonnes annuelles dans l'ordre 2023 → 2015
cols_annees <- paste0("cult", 2023:2015)

# Fonction pour compter les années consécutives de non-déclaration ou de déclaration on jachère
count_consec_non_decl <- function(x) {
  count <- 0
  for (val in x) {
    if (is.na(val) | val %in% c("abs","J6P","J5M")) {
      count <- count + 1
    } else {
      break  # dès qu'une déclaration est trouvée, on arrête
    }
  }
  return(count)
}

# Extraire la table sous forme de data.frame pour pbapply
rpg_df <- rpg_change %>% st_drop_geometry()
# Appliquer la fonction avec une barre de progression
nb_annees_non_decl <- pbapply::pbapply(rpg_df[ , cols_annees], 1, count_consec_non_decl)
# Ajouter le résultat à ton sf
rpg_change$nb_annees_non_decl <- nb_annees_non_decl
# vérification
rpg_change %>%
  select(id_unique, all_of(cols_annees), nb_annees_non_decl)

# au moins 3 ans sans declaration 
rpg_non_decl_3plus <- rpg_change %>%
  filter(nb_annees_non_decl >= 3)

# plot
deprise_moulis <- st_intersection(rpg_non_decl_3plus, moulis)
mapview(deprise_moulis, zcol = "nb_annees_non_decl",layer.name = "Années non déclarées", alpha = 0.7,legend = TRUE)+
  mapview(moulis, col.regions = NA, color = "black", lwd = 2, 
          layer.name = "Limite de Moulis")

# export pour smartphone
rpg_non_decl_3plus_wgs <- st_transform(rpg_non_decl_3plus, 4326)
st_write(rpg_non_decl_3plus_wgs, "~/Documents/land change/outputs/rpg_non_decl_3plus_wgs.gpkg", delete_dsn = TRUE)

st_write(rpg_non_decl_3plus_wgs, "~/Documents/land change/outputs/rpg_non_decl_3plus_wgs.geojson",driver = "GeoJSON", delete_dsn = TRUE)

# Lire rapidement
rpg_check <- st_read("~/Documents/land change/outputs/rpg_non_decl_3plus_wgs.geojson")
plot(rpg_check["nb_annees_non_decl"])
# 
# 
# #5-Travaill sur le RPG change en ariege
# 
# # Supprimer la géométrie
# rpg <- st_drop_geometry(rpg_change)
# # Afficher les premières lignes
# head(rpg)
# # dominant culutre in 2023
# sort(table(rpg$cult2023),decreasing = T)
# sort(table(rpg$cult2015),decreasing = T)
# # non déclaré depuis 5 ans
# sum(is.na(rpg$cult2023)&is.na(rpg$cult2022)&is.na(rpg$cult2021)&is.na(rpg$cult2020)
#     &is.na(rpg$cult2019))
# 
# ind=rowSums(is.na(rpg[,4:8]))==5
# # code de toute les parcelles non déclarée depuis 5 ans
# ab=rpg$id_unique[ind]
# 
# rpg_ab=rpg_change[rpg$id_unique %in% ab,]
# mapview(rpg_ab, col.regions = "red", alpha.regions = 0.2) 
# 
