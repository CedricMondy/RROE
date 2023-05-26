#' Renommer les champs d'une couche obstacles pour utilisation des fonctions du
#' package {RROE}
#'
#' @param couche Couche géographique contenant les informations sur les
#'   obstacles à l'écoulement
#' @param tableau_correspondances Chemin vers le fichier csv contenant la correspondance
#'   entre les noms de champ de la couche et les noms de champ utilisés dans le
#'   script
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr select rename_with
#' @importFrom purrr set_names
renommer_couche <- function(couche, tableau_correspondances = NULL) {
    if (!is.null(tableau_correspondances)) {
        map_vec <- read.csv2(tableau_correspondances) %>%
            (function(df) {
                df$NomScript %>%
                    purrr::set_names(df$NomCouche)
            }) %>%
            c("geometry" = "geometry")

        couche %>%
            dplyr::select(names(map_vec), geometry) %>%
            dplyr::rename_with(
                .fn = function(x) {map_vec[x]}
            )
    } else {
        couche
    }
}
