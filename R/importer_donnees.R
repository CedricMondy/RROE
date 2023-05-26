#' Télécharger le ROE
#'
#' Cette fonction permet de télécharger le ROE diffusé via un [fluw WFS du
#' Sandre](https://www.sandre.eaufrance.fr/atlas/srv/fre/catalog.search#/metadata/59057026-b40c-4cf9-9e3e-7296e0aa1a78).
#'
#' La requête peut être filtrée pour un ou plusieurs départements, pour un ou
#' plusieurs bassins hydrographiques et pour un ou plusieurs ouvrages en
#' particulier. Si un code ROE est fourni, les autres filtres (département et
#' bassin) sont ignorés. Si des filtres sont fournis à la fois sur les
#' départements et les bassins sont fournis, le résultat correspond aux ouvrages
#' répondant à l'ensemble des filtres (opérateur ET).
#'
#' @param code_dpt character Code INSEE du ou départements
#' @param nom_bassin character Nom du bassin hydrographique parmi:
#'   SEINE-NORMANDIE, RHIN-MEUSE, RHONE-MEDITERRANEE, LOIRE-BRETAGNE,
#'   ADOUR-GARONNE, ARTOIS-PICARDIE, CORSE
#' @param code_roe character Code du ou des ouvrages ROE
#'
#' @return a sf data frame
#' @export
#'
#' @importFrom httr parse_url build_url
#' @importFrom purrr list_merge map_chr
#' @importFrom sf st_read
telecharger_roe_sandre <- function(code_dpt = NULL, nom_bassin = NULL, code_roe = NULL) {

    requete <- "https://services.sandre.eaufrance.fr/geo/obs?SERVICE=WFS" %>%
        httr::parse_url() %>%
        purrr::list_merge(
            query = list(
                service = "wfs",
                version = "2.0.0",
                request = "GetFeature",
                typename = "sa:ObstEcoul",
                srsName = "EPSG:4326"
            )
        ) %>%
        httr::build_url()

    filtre <- ""

    if (!is.null(code_roe)) {
        filtre_roe <- purrr::map_chr(
            code_roe,
            function(roe) {
                paste0(
                    "<PropertyIsEqualTo><PropertyName>CdObstEcoul</PropertyName> <Literal>", roe, "</Literal></PropertyIsEqualTo>"
                )
            }
        ) %>%
            paste(collapse = "")

        if (length(code_roe) > 1) {
            filtre <- paste0(filtre, "<Or>", filtre_roe, "</Or>")
        } else {
            filtre <- paste0(filtre, filtre_roe)
        }

    } else {

        if (!is.null(code_dpt)) {
            filtre_dpt <- paste(
                purrr::map_chr(
                    code_dpt,
                    function(d) {
                        paste0(
                            "<PropertyIsEqualTo><PropertyName>CdDepartement</PropertyName> <Literal>", d, "</Literal></PropertyIsEqualTo>"
                        )
                    }
                ),
                collapse = ""
            )

            if (length(code_dpt) > 1) {
                filtre <- paste0(filtre, "<Or>", filtre_dpt, "</Or>")
            } else {
                filtre <- paste0(filtre, filtre_dpt)
            }

        }

        if (!is.null(nom_bassin)) {
            filtre_bassin <- paste0(
                purrr::map_chr(
                    nom_bassin,
                    function(b) {
                        paste0(
                            "<PropertyIsEqualTo><PropertyName>NomCircAdminBassin</PropertyName> <Literal>", b, "</Literal></PropertyIsEqualTo>"

                        )
                    }
                ),
                collapse = ""
            )

            if (length(nom_bassin) > 1) {
                filtre <- paste0(filtre, "<Or>", filtre_bassin, "</Or>")
            } else {
                filtre <- paste0(filtre, filtre_bassin)
            }

        }

        if ((!is.null(code_dpt) & !is.null(nom_bassin)))
            filtre <- paste0("<And>", filtre, "</And>")


    }

    if (!is.null(code_dpt) | !is.null(nom_bassin) | !is.null(code_roe)) {
        requete <- paste0(
        requete,
        "&filter=<?xml version='1.0'?><Filter>",
        filtre,
        "</Filter>"
    ) %>%
        httr::parse_url() %>%
        httr::build_url()
    }

    sf::st_read(requete)
}
