#' Hennepin County-inspired ggplot2 theme
#'
#' Sensible default theme based on Hennepin County visualization guide. Can use package color palettes to supplement.
#' @export

theme_hennepin <- function (db_path = NULL){

  #I'm not sure if this is a great way to handle this or not
  #I would rather not make these full dependencies, since users may not use this function
  for (package in c("systemfonts", "extrafont")){
    if (!package %in% installed.packages()){
      message(paste("Installing", package))
      install.packages(package)
    }
  }

  #If using Databricks, must add font files
  if (Sys.info()['sysname'] == "Linux"){
    file.copy(from = db_path,
              to = "/usr/share/fonts/",
              recursive = TRUE)
  }

  #if using fonts for the first time, will need to perform some extra operations
  if (!"Segoe UI Light" %in% extrafont::fonts()){
    extrafont::ttf_import(pattern = "segoeui")
  }

  extrafont::loadfonts(device = "win", quiet = TRUE)

  systemfonts::register_variant(
    name = "Segoe UI Light",
    family = "Segoe UI",
    weight = "light"
  )

  ggplot2::theme_minimal(base_family = "Segoe UI") %+replace%
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 12, color = "black", vjust = 2, family = "Segoe UI"),
      plot.subtitle = ggplot2::element_text(size = 12, vjust = 1, family = "Segoe UI Light"),
      plot.caption = ggplot2::element_text(size = 10, hjust = 1, family = "Segoe UI Light"),
      plot.background = ggplot2::element_rect(fill = "#F8F8F8", color = "#F8F8F8"),
      axis.title = ggplot2::element_text(size = 11),
      axis.text = ggplot2::element_text(size = 11, family = "Segoe UI Light"),
      axis.ticks = ggplot2::element_line(colour = "light grey"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "light grey"),
      legend.title = ggplot2::element_text(size = 11, family = "Segoe UI Light"),
      legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
      legend.key = ggplot2::element_rect(fill = "transparent", color = NA),
      axis.line = ggplot2::element_line(colour = "light grey", linewidth = rel(1)),
      complete = FALSE
    )
}
