#' Hennepin County-inspired ggplot2 theme
#'
#' Sensible default theme based on Hennepin County visualization guide. Can use package color palettes to supplement.
#' @export

theme_hennepin <- function (){

  for (package in c("systemfonts", "extrafont")){
    if (!package %in% installed.packages()){
      install.packages(package)
    }
  }

  #If using Databricks, must add font files
  if (Sys.info()['sysname'] == "Linux"){
    file.copy(from = "/dbfs/mnt/phmdw/Trusted/PublicHealth/Utilities/Segoe UI/",
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

  theme_minimal(base_family = "Segoe UI") %+replace%
    theme(
      plot.title = element_text(size = 12, color = "black", vjust = 2, family = "Segoe UI"),
      plot.subtitle = element_text(size = 12, vjust = 1, family = "Segoe UI Light"),
      plot.caption = element_text(size = 10, hjust = 1, family = "Segoe UI Light"),
      plot.background = element_rect(fill = "#F8F8F8", color = "#F8F8F8"),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 11, family = "Segoe UI Light"),
      axis.ticks = element_line(colour = "light grey"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "light grey"),
      legend.title = element_text(size = 11, family = "Segoe UI Light"),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.key = element_rect(fill = "transparent", color = NA),
      axis.line = element_line(colour = "light grey", linewidth = rel(1)),
      complete = FALSE
    )
}
