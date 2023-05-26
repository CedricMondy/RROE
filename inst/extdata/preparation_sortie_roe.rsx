##Data=group
##Préparation sortie ROE=name
##Fond_de_carte=string ign
##QgsProcessingParameterFeatureSource|selection_roe|Couche des obstacles|0
##QgsProcessingParameterFile|mapping|Correspondance des noms de champs|0|csv||True
##Dossier_sortie=output folder NULL

if (!require(remotes))
    install.packages("remotes")
if (!require(RROE))
    remotes::install_github("CedricMondy/RROE")

cat("\nRenommer les champs\n")

if (mapping != "")
    selection_roe <- RROE::renommer_couche(selection_roe, mapping)

dir.create(file.path(Dossier_sortie, "export"))

cat("\nGenerer les fiches\n")

RROE::generer_fiches_terrain(
    donnees = selection_roe,
    date_export = Sys.Date(),
    dossier_sortie = file.path(Dossier_sortie, "export"),
    fond_carte = Fond_de_carte
)

cat("\nCreer le zip\n")

fichiers_crees <- list.files(file.path(Dossier_sortie, "export"), full.names = TRUE)

zip::zip(
    zipfile = "export_roe.zip",
    files = fichiers_crees,
    recurse = FALSE,
    include_directories = FALSE,
    root = Dossier_sortie,
    mode = "cherry-pick"
)

unlink(file.path(Dossier_sortie, "export"), recursive = TRUE)
