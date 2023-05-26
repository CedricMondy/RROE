#' Copie les scripts de traitements pour être accessible dans QGIS
#'
#' @param dest_folder dossier d'où les scripts R seront accessibles pour QGIS.
#'   Typiquement, il s'agit d'un dossier `processing/rscripts` dans le dossier
#'   du profil utilisé.
#'
#' @return
#' @export
#'
#' @examples
install_traitements_qgis <- function(dest_folder, overwrite = TRUE) {
    fichiers_a_installer <- c(
        ".*rsx",
        "mapping.csv"
    )

    list.files(
        system.file("extdata", package = "RROE"),
        pattern = paste(fichiers_a_installer, collapse = "|"),
        full.names = TRUE
    ) %>%
        file.copy(to = dest_folder, overwrite = overwrite)
}
