##Data=group
##Préparation sortie ROE=name
##Fond_de_carte=enum literal ign_plan;osm ign_plan
##QgsProcessingParameterFeatureSource|selection_roe|Couche des obstacles|0
##QgsProcessingParameterFile|mapping|Correspondance des noms de champs|0|csv|C:\QGIS-CUSTOM\PROFILS\profiles\GeOFB\processing\rscripts\mapping.csv|True
##QgsProcessingParameterFile|dossier_sortie|Dossier de sortie|1||C:\QGIS-CUSTOM-PERSO\TRAITEMENTS\SORTIE|False
##Nom_du_fichier_de_sortie=string export_roe

if (!require(remotes))
    install.packages("remotes")
if (!require(RROE))
    remotes::install_github("CedricMondy/RROE")

cat("\nRenommer les champs\n")

if (mapping != "")
    selection_roe <- RROE::renommer_couche(selection_roe, mapping)

dir.create(file.path(dossier_sortie, "export"))

cat("\nGenerer les fiches\n")

RROE::generer_fiches_terrain(
    donnees = selection_roe,
    date_export = Sys.Date(),
    dossier_sortie = file.path(dossier_sortie, "export"),
    fond_carte = Fond_de_carte
)

cat("\nCreer le zip\n")

fichiers_crees <- list.files(file.path(dossier_sortie, "export"), full.names = TRUE)

zip::zip(
    zipfile = paste0(Nom_du_fichier_de_sortie, ".zip"),
    files = fichiers_crees,
    recurse = FALSE,
    include_directories = FALSE,
    root = dossier_sortie,
    mode = "cherry-pick"
)

unlink(file.path(dossier_sortie, "export"), recursive = TRUE)
