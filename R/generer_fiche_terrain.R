
#' Générer des fiches terrain ROE
#'
#' Cette fonction permet de générer des fiches terrain pré-remplies avec les
#' données issues d'un export du ROE. Ces fiches pourront alors servir lors de
#' visites sur site pour la mise en qualité des données du ROE.
#'
#' @param donnees données issus d'un export ROE
#' @param codes_roe vecteur de codes ROE des obstacles pour lesquels on veut
#'   générer des fiches terrain.
#' @param date_export date à laquelle l'export du ROE utilisé a été réalisée
#' @param dossier_sortie chemin du dossier dans lequel seront enregistrées les
#'   fiches terrain générées.
#' @param sous_dossiers vecteur contenant les noms de colonnes du tableau
#'   `donnees` qui serviront à hiérarchiser les enregistrements des fiches (e.g.
#'   par département en spécifiant "dept_nom")
#'
#' @export
#'
#' @importFrom dplyr mutate select all_of group_by summarise left_join filter arrange distinct n pull rename bind_cols case_when contains starts_with
#' @importFrom openxlsx loadWorkbook writeData openxlsx_setOp insertImage saveWorkbook
#' @importFrom progress progress_bar
#' @importFrom purrr walk map
#' @importFrom sf st_drop_geometry st_transform st_as_sf st_coordinates
#' @importFrom stringr str_split str_detect str_replace fixed
#' @importFrom tidyr pivot_longer
generer_fiches_terrain <- function(donnees, codes_roe = NULL, date_export = Sys.Date(), dossier_sortie = getwd(), sous_dossiers = NULL, fond_carte = c("ign", "osm")) {

    formater_cellule <- function(nom_cellule) {
        nom_cellule %>%
            stringr::str_split(pattern = "-") %>%
            '[['(1)
    }

    FichiersSortie <- donnees %>%
        sf::st_drop_geometry() %>%
        dplyr::mutate(dossier = dossier_sortie) %>%
        dplyr::select(dossier, dplyr::all_of(sous_dossiers), CdObstEcoul) %>%
        tidyr::pivot_longer(cols = -CdObstEcoul) %>%
        dplyr::group_by(CdObstEcoul) %>%
        dplyr::summarise(chemin = paste(value, collapse = "/")) %>%
        dplyr::mutate(fichier_sortie = paste0(chemin, "/FicheTerrain_", CdObstEcoul, ".xlsx"))

    donnees <- donnees %>%
        dplyr::left_join(FichiersSortie, by = "CdObstEcoul")

    if (is.null(codes_roe))
        codes_roe <- unique(donnees$CdObstEcoul)

    donnees <- donnees %>%
        dplyr::filter(CdObstEcoul %in% codes_roe) %>%
        dplyr::arrange(fichier_sortie)

    if ("sf" %in% class(donnees)) {
        donnees <- donnees %>%
            sf::st_transform(crs = 2154)
    } else {
        donnees <- donnees %>%
            sf::st_as_sf(
                coords = c("CoordXPointCarOuvrage", "CoordYPointCarOuvrage"),
                crs = 2154,
                remove = FALSE
                )
    }


    if (!is.null(sous_dossiers)) {
        donnees %>%
            dplyr::distinct(across(all_of(sous_dossiers))) %>%
            dplyr::mutate(id = seq(dplyr::n())) %>%
            tidyr::pivot_longer(
                cols = -id,
                names_to = "niveau",
                values_to = "valeur"
                ) %>%
            dplyr::group_by(id) %>%
            dplyr::summarise(
                chemin = paste(c(dossier_sortie, valeur), collapse = "/")
                ) %>%
            dplyr::pull(chemin) %>%
            purrr::walk(
                .f = function(x) {
                    if (!dir.exists(x))
                        dir.create(path = x, recursive = TRUE)
                }
            )

    }

    pb <- progress::progress_bar$new(
        total = length(codes_roe),
        format = ":current/:total (:eta)"
    )

    generer_fiche_terrain <- function(code_roe) {

        cat("\n", code_roe, "\n")

        FicheTerrain <- openxlsx::loadWorkbook(
            file = system.file(
                "extdata",
                "FicheTerrain.xlsx",
                package = "RROE"
                )
        )

        ecrire_donnee <- function(cellule, valeur) {
            if (any(!is.na(cellule)))
                openxlsx::writeData(
                    wb = FicheTerrain,
                    sheet = 1,
                    x = valeur,
                    xy = cellule
                )
        }

        donnees_ouvrage <- donnees %>%
            dplyr::filter(CdObstEcoul == code_roe) %>%
            (function(df) {
                coords_wgs84 <- df %>%
                    sf::st_transform(crs = 4326) %>%
                    sf::st_coordinates() %>%
                    as.data.frame() %>%
                    dplyr::rename(
                        x_wgs84 = X, y_wgs84 = Y
                    )

                dplyr::bind_cols(
                    df, coords_wgs84
                )

            })

        openxlsx::openxlsx_setOp("keepNA", FALSE)

        # Identification et implantation de l'ouvrage----
        if ("NomPrincipalObstEcoul" %in% colnames(donnees_ouvrage)) {
            if (is.na(donnees_ouvrage$NomPrincipalObstEcoul))
                ecrire_donnee(
                    valeur = "Nom de l'ouvrage: ",
                    cellule = c("D", 9)
                    )
            ecrire_donnee(
                valeur = donnees_ouvrage$NomPrincipalObstEcoul,
                cellule = c("H", 9)
            )
        }

        ecrire_donnee(
            valeur = donnees_ouvrage$CdObstEcoul,
            cellule = c("Q", 9)
        )

        if ("NomEntiteHydrographique" %in% colnames(donnees_ouvrage)) {
            ecrire_donnee(
                valeur = donnees_ouvrage$NomEntiteHydrographique,
                cellule = c("H", 10)
            )
        }

        if (any(is.na(c(donnees_ouvrage$x_wgs84, donnees_ouvrage$y_wgs84))))
            ecrire_donnee(
                valeur = iconv("Coordonnées GPS: ", to = "latin1"),
                cellule = c("D", 11)
            )
        ecrire_donnee(
            valeur = donnees_ouvrage$x_wgs84,
            cellule = c("J", 11)
        )
        ecrire_donnee(
            valeur = donnees_ouvrage$y_wgs84,
            cellule = c("O", 11)
        )
        ecrire_donnee(
            valeur = donnees_ouvrage$StObstEcoul,
            cellule = c("Q", 14)
        )

        # Etat----
        if ("LbEtOuvrage" %in% colnames(donnees_ouvrage)) {
            if (is.na(donnees_ouvrage$LbEtOuvrage))
                ecrire_donnee(
                    valeur = "Etat: ",
                    cellule = c("D", 17)
                )

            cellule_etat <- dplyr::case_when(
                donnees_ouvrage$LbEtOuvrage == "En projet" ~ "F-17",
                donnees_ouvrage$LbEtOuvrage == "En construction" ~ "H-17",
                donnees_ouvrage$LbEtOuvrage == "Existant" ~ "M-17",
                donnees_ouvrage$LbEtOuvrage == "Détruit partiellement" ~ "F-18",
                donnees_ouvrage$LbEtOuvrage == "Détruit entièrement" ~ "H-18"
            ) %>%
                formater_cellule()
            ecrire_donnee(
                valeur = "X",
                cellule = cellule_etat
            )
        }

        # Typologie-----
        if ("CdTypeOuvrage" %in% colnames(donnees_ouvrage)) {
            donnees_ouvrage <- donnees_ouvrage %>%
                dplyr::mutate(
                    type_nom = dplyr::case_when(
                        stringr::str_detect(CdTypeOuvrage, pattern = "^1\\.1.*") ~ "Barrage",
                        stringr::str_detect(CdTypeOuvrage, pattern = "^1\\.2.*") ~ "Seuil en rivière",
                        stringr::str_detect(CdTypeOuvrage, pattern = "^1\\.3.*") ~ "Digue",
                        stringr::str_detect(CdTypeOuvrage, pattern = "^1\\.4.*") ~ "Obstacle induit par un pont",
                        stringr::str_detect(CdTypeOuvrage, pattern = "^1\\.5.*") ~ "Epis en rivière",
                        stringr::str_detect(CdTypeOuvrage, pattern = "^1\\.6.*") ~ "Grille de pisciculture"
                    )
                )

            cellule_type <- dplyr::case_when(
                donnees_ouvrage$type_nom == "Barrage" ~ "B-22",
                donnees_ouvrage$type_nom == "Seuil en rivière" ~ "B-34",
                donnees_ouvrage$type_nom == "Obstacle induit par un pont" ~ "H-22",
                donnees_ouvrage$type_nom == "Digue" ~ "H-30",
                donnees_ouvrage$type_nom == "Grille de pisciculture" ~ "H-36",
                donnees_ouvrage$type_nom == "Epis en rivière" ~ "H-38"
            ) %>%
                formater_cellule()
            ecrire_donnee(
                valeur = "X",
                cellule = cellule_type
            )
            cellule_sous_type <- dplyr::case_when(
                ## Barrage----
                donnees_ouvrage$CdTypeOuvrage %in% c("1.1", "1.1.0") ~ "D-31",
                donnees_ouvrage$CdTypeOuvrage == "1.1.1" ~ "D-23",
                donnees_ouvrage$CdTypeOuvrage == "1.1.2" ~ "D-24",
                donnees_ouvrage$CdTypeOuvrage == "1.1.3" ~ "D-25",
                donnees_ouvrage$CdTypeOuvrage == "1.1.4" ~ "D-26",
                donnees_ouvrage$CdTypeOuvrage == "1.1.5" ~ "D-27",
                donnees_ouvrage$CdTypeOuvrage == "1.1.6" ~ "D-29",
                donnees_ouvrage$CdTypeOuvrage == "1.1.7" ~ "D-30",
                donnees_ouvrage$CdTypeOuvrage == "1.1.X" ~ "D-32",
                ## Seuil en rivière----
                donnees_ouvrage$CdTypeOuvrage %in% c("1.2", "1.2.0") ~ "D-38",
                donnees_ouvrage$CdTypeOuvrage == "1.2.1" ~ "D-35",
                donnees_ouvrage$CdTypeOuvrage == "1.2.2" ~ "D-36",
                donnees_ouvrage$CdTypeOuvrage == "1.2.3" ~ "D-37",
                donnees_ouvrage$CdTypeOuvrage == "1.2.X" ~ "D-39",
                ## Obstacle induit par un pont----
                donnees_ouvrage$CdTypeOuvrage %in% c("1.4", "1.4.0") ~ "J-26",
                donnees_ouvrage$CdTypeOuvrage == "1.4.1" ~ "J-23",
                donnees_ouvrage$CdTypeOuvrage == "1.4.2" ~ "J-24",
                donnees_ouvrage$CdTypeOuvrage == "1.4.3" ~ "J-25",
                donnees_ouvrage$CdTypeOuvrage == "1.4.X" ~ "J-27",
                ## Digue----
                donnees_ouvrage$CdTypeOuvrage == "1.3" ~ "H-30",
                donnees_ouvrage$CdTypeOuvrage == "1.3.1" ~ "J-31",
                donnees_ouvrage$CdTypeOuvrage == "1.3.2" ~ "J-32",
                donnees_ouvrage$CdTypeOuvrage == "1.3.3" ~ "J-34"
            ) %>%
                formater_cellule()
            ecrire_donnee(
                valeur = "X",
                cellule = cellule_sous_type
            )

            if (!is.na(donnees_ouvrage$CdTypeOuvrage)) {
                if (
                    donnees_ouvrage$CdTypeOuvrage == "1.1.X"
                )
                    ecrire_donnee(
                        valeur = "... ",
                        cellule = c("E", 33)
                    )

                if (
                    donnees_ouvrage$CdTypeOuvrage == "1.2.X"
                )
                    ecrire_donnee(
                        valeur = "... ",
                        cellule = c("E", 40)
                    )

                if (
                    donnees_ouvrage$CdTypeOuvrage == "1.4.X"
                )
                    ecrire_donnee(
                        valeur = "... ",
                        cellule = c("K", 28)
                    )
            }
        }

        # Eléments mobiles ----
        if ("CdTypeElMobSeuil" %in% colnames(donnees_ouvrage)) {
            elements_mobiles <- donnees_ouvrage %>%
                dplyr::select(
                    CdObstEcoul,
                    dplyr::contains("CdTypeElMobSeuil")
                ) %>%
                tidyr::pivot_longer(
                    cols = dplyr::starts_with("CdTypeElMobSeuil"),
                    names_to = "colonne",
                    values_to = "element_mobile"
                ) %>%
                dplyr::filter(
                    !(stringr::str_detect(colonne, "CdTypeElMobSeuil") &
                          colonne != "CdTypeElMobSeuil1" &
                          is.na(element_mobile))
                ) %>%
                dplyr::pull(element_mobile)

            cellule_element_mobile <- elements_mobiles %>%
                purrr::map(
                    function(x) {
                        dplyr::case_when(
                            x == "10" ~ "T-22",
                            x == "1" ~ "T-23",
                            x == "2" ~ "T-24",
                            x == "3" ~ "T-25",
                            x == "4" ~ "T-26",
                            x == "5" ~ "T-27",
                            x == "6" ~ "T-28",
                            x == "7" ~ "T-29",
                            x == "8" ~ "T-30",
                            x == "0" ~ "T-31",
                            x == "9" ~ "T-32"
                        ) %>%
                            formater_cellule()
                    }
                )

            purrr::walk(
                cellule_element_mobile,
                ecrire_donnee, valeur = "X"
            )

            if (
                any(!is.na(elements_mobiles)) &
                any(elements_mobiles == "9")
            )
                ecrire_donnee(
                    valeur = "... ",
                    cellule = c("V", 33)
                )
        }


        # Hauteur de chute à l'étiage----
        if ("HautChutEtObstEcoul" %in% colnames(donnees_ouvrage)) {
            if (!is.na(donnees_ouvrage$HautChutEtObstEcoul)) {
                ecrire_donnee(
                    valeur = donnees_ouvrage$HautChutEtObstEcoul,
                    cellule = c("B", 45)
                )
            }

        }

        if ("CdHautChutClObstEcoul" %in% colnames(donnees_ouvrage)) {
            cellule_classe_hauteur <- dplyr::case_when(
                donnees_ouvrage$CdHautChutClObstEcoul == "0" ~ "H-44",
                donnees_ouvrage$CdHautChutClObstEcoul == "1" ~ "F-45",
                donnees_ouvrage$CdHautChutClObstEcoul == "2" ~ "F-46",
                donnees_ouvrage$CdHautChutClObstEcoul == "3" ~ "H-45",
                donnees_ouvrage$CdHautChutClObstEcoul == "4" ~ "H-46",
                donnees_ouvrage$CdHautChutClObstEcoul == "5" ~ "M-45",
                donnees_ouvrage$CdHautChutClObstEcoul == "6" ~ "M-46",
                donnees_ouvrage$CdHautChutClObstEcoul == "7" ~ "T-45",
                donnees_ouvrage$CdHautChutClObstEcoul == "8" ~ "T-46"
            ) %>%
                formater_cellule()

            ecrire_donnee(
                valeur = "X",
                cellule = cellule_classe_hauteur
            )
        }


        # Usages----
        if ("CdUsageObstEcoul" %in% colnames(donnees_ouvrage)) {
            usages <- donnees_ouvrage %>%
                dplyr::select(
                    CdObstEcoul,
                    dplyr::starts_with("CdUsageObstEcoul")
                ) %>%
                tidyr::pivot_longer(
                    cols = dplyr::starts_with("CdUsageObstEcoul"),
                    names_to = "colonne",
                    values_to = "usage"
                ) %>%
                dplyr::filter(
                    !(stringr::str_detect(colonne, "CdUsageObstEcoul") &
                          colonne != "CdUsageObstEcoul1" &
                          is.na(usage))
                ) %>%
                dplyr::pull(usage)

            cellule_usage <- usages %>%
                purrr::map(
                    function(x) {
                        dplyr::case_when(
                            x == "0" ~ "B-50",
                            x == "1" ~ "B-51",
                            x == "2" ~ "B-52",
                            x == "3" ~ "B-54",
                            x == "4" ~ "B-55",
                            x == "5" ~ "B-57",
                            x == "6" ~ "H-50",
                            x == "8" ~ "H-53",
                            x == "10" ~ "H-54",
                            x == "11" ~ "Q-50",
                            x == "12" ~ "Q-54",
                            x == "13" ~ "Q-52",
                            x == "14" ~ "Q-55"#,
                            # x == "Autre usage (préciser)" ~ "Q-56"
                        ) %>%
                            formater_cellule()
                    }
                )

            purrr::walk(
                cellule_usage,
                ecrire_donnee, valeur = "X"
            )

            if ("2" %in% usages)
                ecrire_donnee(valeur = "Extraction granulats ", cellule = c("E", 53))
            if ("4" %in% usages)
                ecrire_donnee(valeur = "Baignade ", cellule = c("E", 56))
            if ("6" %in% usages){
                ecrire_donnee(valeur = "Pisciculture ", cellule = c("K", 51))
                ecrire_donnee(
                    valeur = iconv("Pêche professionnelle ", to = "latin1"),
                    cellule = c("K", 52)
                )
            }
            if ("10" %in% usages){
                ecrire_donnee(
                    valeur = iconv("Défense contre les crues ", to = "latin1"),
                    cellule = c("K", 55)
                )
                ecrire_donnee(
                    valeur = iconv("Soutien d'étiage ", to = "latin1"),
                    cellule = c("K", 56)
                )
                ecrire_donnee(
                    valeur = "Stockage de l'eau pour l'incendie ",
                    cellule = c("K", 57)
                )
            }
            # if ("Autre usage (préciser)" %in% usages)
            #     ecrire_donnee(valeur = "... ", cellule = c("S", 57))
        }


        # Dispositif de franchissement piscicole----
        if ("CdTypeDispFranchPiscicole" %in% colnames(donnees_ouvrage)) {
            passes <- donnees_ouvrage %>%
                dplyr::select(
                    CdObstEcoul,
                    dplyr::starts_with("CdTypeDispFranchPiscicole")
                ) %>%
                tidyr::pivot_longer(
                    cols = dplyr::starts_with("CdTypeDispFranchPiscicole"),
                    names_to = "colonne",
                    values_to = "passe"
                ) %>%
                dplyr::filter(
                    !(stringr::str_detect(colonne, "CdTypeDispFranchPiscicole") &
                          colonne != "CdTypeDispFranchPiscicole1" &
                          is.na(passe))
                ) %>%
                dplyr::pull(passe)

            if ("0" %in% passes) {
                ecrire_donnee(valeur = "X", cellule = c("B", 61))
            }
            if ("1" %in% passes) {
                ecrire_donnee(valeur = "X", cellule = c("B", 62))
                ecrire_donnee(valeur = " ", cellule = c("H", 62))
            }
            if ("2" %in% passes) {
                ecrire_donnee(valeur = "X", cellule = c("B", 63))
                ecrire_donnee(valeur = " ", cellule = c("H", 63))
            }
            if ("3" %in% passes) {
                ecrire_donnee(valeur = "X", cellule = c("B", 64))
                ecrire_donnee(valeur = " ", cellule = c("H", 64))
            }
            if ("5" %in% passes) {
                ecrire_donnee(valeur = "X", cellule = c("B", 65))
                ecrire_donnee(valeur = " ", cellule = c("H", 65))
                ecrire_donnee(valeur = "tapis brosse ", cellule = c("D", 66))
                ecrire_donnee(valeur = " ", cellule = c("H", 66))
                ecrire_donnee(valeur = "substrat rugueux ", cellule = c("D", 67))
                ecrire_donnee(valeur = " ", cellule = c("H", 67))
                ecrire_donnee(
                    valeur = iconv("passe piège ", to = "latin1"),
                    cellule = c("D", 68)
                )
                ecrire_donnee(valeur = " ", cellule = c("H", 68))
            }
            if ("6" %in% passes) {
                ecrire_donnee(valeur = "X", cellule = c("B", 69))
                ecrire_donnee(valeur = " ", cellule = c("H", 69))
            }
            if ("7" %in% passes) {
                ecrire_donnee(valeur = "X", cellule = c("M", 62))
                ecrire_donnee(valeur = " ", cellule = c("T", 62))
            }
            if ("8" %in% passes) {
                ecrire_donnee(valeur = "X", cellule = c("M", 63))
                ecrire_donnee(valeur = " ", cellule = c("T", 63))
                ecrire_donnee(
                    valeur = "sur partie de la largeur ",
                    cellule = c("O", 64)
                )
                ecrire_donnee(valeur = " ", cellule = c("T", 64))
                ecrire_donnee(
                    valeur = iconv("sur totalité de la largeur ", to = "latin1"),
                    cellule = c("O", 65)
                )
                ecrire_donnee(valeur = " ", cellule = c("T", 65))
            }
            if ("9" %in% passes) {
                ecrire_donnee(valeur = "X", cellule = c("M", 66))
                ecrire_donnee(valeur = " ", cellule = c("T", 66))
            }
            if ("4" %in% passes) {
                ecrire_donnee(valeur = "X", cellule = c("M", 67))
                ecrire_donnee(valeur = " ", cellule = c("T", 67))
            }
            if ("11" %in% passes) {
                ecrire_donnee(valeur = "X", cellule = c("M", 68))
                ecrire_donnee(valeur = " ", cellule = c("T", 68))
            }
            if ("10" %in% passes) {
                ecrire_donnee(valeur = "X", cellule = c("M", 69))
                ecrire_donnee(valeur = " ", cellule = c("T", 69))
                ecrire_donnee(valeur = "... ", cellule = c("O", 70))
            }
        }


        # Dispositif de franchissement pour la navigation----
        if ("CdTypeDispFranchNavig" %in% colnames(donnees_ouvrage)) {
            cellule_navigation <- donnees_ouvrage %>%
                dplyr::select(
                    CdObstEcoul,
                    dplyr::starts_with("CdTypeDispFranchNavig")
                ) %>%
                tidyr::pivot_longer(
                    cols = dplyr::starts_with("CdTypeDispFranchNavig"),
                    names_to = "colonne",
                    values_to = "navigation"
                ) %>%
                dplyr::filter(
                    !(stringr::str_detect(colonne, "CdTypeDispFranchNavig") &
                          colonne != "CdTypeDispFranchNavig1" &
                          is.na(navigation))
                ) %>%
                dplyr::pull(navigation) %>%
                purrr::map(
                    function(x) {
                        dplyr::case_when(
                            x == "4" ~ "B-73",
                            x == "0" ~ "B-74",
                            x == "1" ~ "H-74",
                            x == "2" ~ "M-74",
                            x == "3" ~ "Q-74"
                        ) %>%
                            formater_cellule()
                    }
                )

            purrr::walk(
                cellule_navigation,
                ecrire_donnee, valeur = "X"
            )
        }

        # Date export ----
        ecrire_donnee(
            valeur = iconv(paste0("Export ROE: ", date_export), to = "latin1"),
            cellule = c("B", 81)
        )

        # Carte de localisation ----
        fichier_carte <- stringr::str_replace(
                string = donnees_ouvrage$fichier_sortie,
                pattern = stringr::fixed(".xlsx"),
                replacement = ".png"
            )

        generer_carte_obstacle(
            donnees_ouvrage = donnees_ouvrage,
            fichier = fichier_carte,
            fond_carte = fond_carte
        )


        if (file.exists(fichier_carte))
            openxlsx::insertImage(
                wb = FicheTerrain,
                sheet = 1,
                file = fichier_carte,
                width = 15,
                height = 15,
                units = "cm",
                dpi = 300,
                startRow = 2,
                startCol = 26
            )

       # Enregistrement de la fiche
        openxlsx::saveWorkbook(
            wb = FicheTerrain,
            file = donnees_ouvrage$fichier_sortie,
            overwrite = TRUE
        )

        unlink(fichier_carte)
        unlink(paste0(fichier_carte, ".aux.xml"))


        pb$tick()

    }

    purrr::walk(
        codes_roe,
        generer_fiche_terrain
    )

    list.files(
        dossier_sortie,
        pattern = "png"
        ) %>%
        purrr::walk(unlink)

}
