# Trabajo Final: Programación y Manejo de datos en la Era del Big Data
#
# Alumnos: Amparo Mocholí   NPA:
#           Hugo Sáiz       NPA:
#           Javier Cotanda  NPA: RB51209
#
# Archivo: Funciones varias
#

# https://stackoverflow.com/questions/13548266/define-all-functions-in-one-r-file-call-them-from-another-r-file-how-if-pos

# Vectores
partidosIdentity <- c("PSOE" = "#ef5350", "PP" = "#64b5f6", 
                      "EAJ-PNV"= "#4db6ac", "ERC" = "#A24936", 
                      "NA+" = "#f44336", "UP-IU-EQUO" = "purple", "UP-EN-MAREA" = "purple",
                      "CiU" = "orange", "AMAIUR" = "gray", 
                      "DL" = "purple", "UP" = "purple",
                      "ECP" = "yellow", "EN COM\u00DA" = "purple",
                      "PRC" = "red", "JXCAT-JUNTS" = "")


# Funciones
limpiarProv <- function(x){
    x <- tolower(x)
    x <- gsub(", comunidad de", "", x)
    x <- gsub(", regi\u00F3n de", "", x)
    x <- gsub(", comunidad foral de", "", x)
    x <- gsub(", principado de", "", x)
    x <- gsub("coruña, a", "A Coruña", x) 
    x <- gsub("balears, illes", "illes balears", x) 
    x <- gsub("rioja, la", "la rioja", x)
    x <- gsub("palmas, las", "las palmas", x) 
    x <- tools::toTitleCase(x)
}

# st_set_geometry(NULL)


# https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/
theme_map <- function(...) {
    theme_minimal() +
        theme(panel.grid.major = element_line(color = "#dbdbd9", size = 0.2), 
              panel.grid.minor = element_line(colour = "gray99"), 
              plot.background = element_rect(fill = "#f5f5f2", color = NA),
              panel.background = element_rect(fill = "#f5f5f2", color = NA),
              legend.background = element_rect(fill = "#f5f5f2", color = NA),
              legend.key = element_rect(fill = NA),
              legend.position = "top",
              legend.direction = "horizontal",
              axis.line = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              text = element_text(color = "#22211d"),
              plot.title = element_text(hjust = 0.5, face = "bold", lineheight = "20px", size = "15"),
              plot.subtitle = element_text(hjust = 0.5, size = "9"))

}

theme_plot <- function(...){
    theme_minimal() +
        theme(panel.grid.major = element_line(colour = "gray99"), 
              panel.grid.minor = element_line(colour = "gray90"), 
              plot.background = element_rect(fill = "#f5f5f2", color = NA),
              panel.background = element_rect(fill = "#f5f5f2", color = NA),
              legend.background = element_rect(fill = "#f5f5f2", color = NA),
              legend.position = "bottom",
              legend.direction = "horizontal",
              axis.text.x = element_text(angle = 90, hjust = 1),
              axis.title.x = element_blank(),
              text = element_text(family = "sans", color = "#22211d"),
              plot.title = element_text(hjust = 0.5, face = "bold", lineheight = "15", size = "12"),
              plot.subtitle = element_text(hjust = 0.5, size = "9"),
              plot.margin = margin(1, 1, 1, 1, "cm"))
}

theme_plot_facet <- function(...){
        theme(panel.grid.major = element_line(colour = "gray99"), 
              panel.grid.minor = element_line(colour = "gray90"), 
              plot.background = element_rect(fill = "#f5f5f2", color = NA),
              panel.background = element_rect(fill = "#f5f5f2", color = NA),
              legend.background = element_rect(fill = "#f5f5f2", color = NA),
              legend.position = "bottom",
              legend.direction = "horizontal",
              axis.text.x = element_text(angle = 90, hjust = 1),
              axis.title.x = element_blank(),
              text = element_text(family = "sans", color = "#22211d"),
              plot.title = element_text(hjust = 0.5, face = "bold", lineheight = "15", size = "12"),
              plot.subtitle = element_text(hjust = 0.5, size = "9"),
              plot.margin = margin(1, 1, 1, 1, "cm"))
}

default_caption <- "Elaboraci\u00F3n propia. Datos del Ministerio del Interior"

# Acortamos nombres de los partidos
partidosNom <- function(x) {
    x$partido <- toupper(x$partido)
    x$partido <- gsub(".*ERC.*", "ERC", x$partido)
    x$partido <- gsub(".*MAREA.*", "UP-EN MAREA", x$partido)
    x$partido <- gsub(".*JxCAT.*", "JxCAT", x$partido)
    x$partido <- gsub("ECP-GUANYEM EL CANVI", "EN COM\u00DA PODEM",  x$partido)
    x$partido <- gsub(".*COMPROM.*", "COMPROMIS-EUPV",  x$partido)
    x$partido <- gsub("PODEMOS", "UP",  x$partido)
    x$partido <- gsub("C's", "Cs",  x$partido)
    # https://stat.ethz.ch/R-manual/R-devel/library/base/html/trimws.html
    x$partido <- trimws(x$partido, "r")
    return(x)
}

## Cargar geometria
# 1: solo peninsula, 2: canarias + peninsula, 3: provincias por codigo, 4: municipios 2018, 5-6: filtro municipios 2018
loadGeometry <- function(mode = 1, provincias = NULL, municipios = NULL){

    library(sf)
    library(LAU2boundaries4spain)
    
    if(mode==1){
        peninsula <- Provincias %>% filter( !(INECodProv %in% c(35, 38)) )
        colnames(peninsula)[1] <- c("codigo.prov")
        return(peninsula)
    }
    
    if(mode==2){
        # https://github.com/perezp44/LAU2boundaries4spain
        canarias <- Provincias %>% filter(INECodProv %in% c(35,38))
        peninsula <- Provincias %>% filter( !(INECodProv %in% c(35, 38)) )
        my_shift <- st_bbox(peninsula)[c(1,2)]- (st_bbox(canarias)[c(1,2)]) + c(-2.4, -1.1)
        canarias$geometry <- canarias$geometry + my_shift
        st_crs(canarias)  <- st_crs(peninsula)
        peninsula_a <- rbind(peninsula, canarias)
        colnames(peninsula_a)[1] <- c("codigo.prov")
        return(peninsula_a)
    }
    
    if(mode==3){
        
        x <- Provincias %>% filter(INECodProv %in% provincias)
        colnames(x)[1] <- c("codigo.prov")
        return(x)
    }
    
    if(mode==4){
        x <- municipios_2018
        return(x)
    }
    
    if(mode==5){
        x <- municipios_2018 %>% filter(INECodMuni %in% municipios)
        return(x)
    }
    if(mode==6){
        x <- municipios_2018 %>% filter(INECodProv %in% provincias)
        return(x)
    }

}

