#' Générer des cartes de localisation des ouvrages ROE
#'
#' @param donnees_ouvrage données d'un ouvrage
#' @param fichier
#' @param flux_ign vecteur dont le premier élément correspond à la clé et le
#'   deuxième au nom technique de la couche parmi [les services WMS de
#'   l'IGN](https://geoservices.ign.fr/services-web-experts)
#'
#' @return
#'
#' @importFrom ggplot2 ggplot geom_sf theme_void geom_point aes ggsave
#' @importFrom happign get_wms_raster
#' @importFrom OpenStreetMap openmap openproj
#' @importFrom sf st_transform st_buffer st_bbox st_as_sfc st_drop_geometry
#' @importFrom sp CRS
#' @importFrom stars read_stars st_rgb geom_stars
generer_carte_obstacle <- function(donnees_ouvrage, fichier, fond_carte = c("ign", "osm")) {

        ouvrage_bbox <- donnees_ouvrage %>%
            sf::st_transform(crs = 2154) %>%
            sf::st_buffer(600) %>%
            sf::st_bbox()

        if (fond_carte == "ign") {
            happign::get_wms_raster(
                shape = sf::st_as_sfc(ouvrage_bbox),
                apikey = "cartes",
                layer_name = "GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2",
                crs = 2154,
                resolution = 2,
                filename = fichier,
                overwrite = TRUE
            )

        fond_ouvrage <- stars::read_stars(fichier) %>%
            (function(x) x[,,,1:3]) %>%
            stars::st_rgb(
                dimension = 3,
                use_alpha = FALSE,
                maxColorValue = 255,
                stretch = FALSE
            )

        carte_ouvrage <- ggplot2::ggplot() +
            stars::geom_stars(data = fond_ouvrage) +
            ggplot2::geom_sf(
                data = donnees_ouvrage,
                size = 5.75, colour = "black"
                    ) +
            ggplot2::geom_sf(
                data = donnees_ouvrage,
                size = 5, colour = "darkgrey"
                    ) +
            ggplot2::theme_void()
        }

        if (fond_carte == "osm") {
            ouvrage_bbox_wgs84 <- donnees_ouvrage %>%
                sf::st_transform(crs = 2154) %>%
                sf::st_buffer(600) %>%
                sf::st_transform(crs = 4326) %>%
                sf::st_bbox()

            fond_ouvrage <- OpenStreetMap::openmap(
                type = "osm",
                upperLeft = unname(ouvrage_bbox_wgs84[c("ymax", "xmin")]),
                lowerRight = unname(ouvrage_bbox_wgs84[c("ymin", "xmax")])
            ) %>%
                OpenStreetMap::openproj(projection = sp::CRS(SRS_string = "EPSG:2154"))

            carte_ouvrage <- OpenStreetMap:::autoplot.OpenStreetMap(fond_ouvrage) +
                ggplot2::geom_point(
                    data = sf::st_drop_geometry(donnees_ouvrage),
                    mapping = ggplot2::aes(x = CoordXPointCarOuvrage, y = CoordYPointCarOuvrage),
                    size = 5.75, colour = "black"
                ) +
                ggplot2::geom_point(
                    data = sf::st_drop_geometry(donnees_ouvrage),
                    mapping = ggplot2::aes(x = CoordXPointCarOuvrage, y = CoordYPointCarOuvrage),
                    size = 5, colour = "darkgrey"
                ) +
                ggplot2::theme_void()
        }


        # carte_ouvrage
        ggplot2::ggsave(
            plot = carte_ouvrage,
            filename = fichier,
            width = 15, height = 15, units = "cm",
            dpi = 300
        )


}
