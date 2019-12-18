# Trabajo Final: Programación y Manejo de datos en la Era del Big Data
#
# Alumnos: Amparo Mocholí   
#           Hugo Sáiz       
#           Javier Cotanda  
#
# Archivo: Filtrado de datos y preparacion
#

# Librerias y dfs -------
library(tidyverse)
library(sf)
library(LAU2boundaries4spain)

#Importamos las funciones del file funciones.R ------
source("funciones.R")

# Cargamos df de las elecciones pasadas
generales28A <- rio::import(here::here("./data/", "generales28A.rdata")) %>% partidosNom(.)
municipios28A <- rio::import(here::here("./data/", "municipios28A.rdata")) %>% partidosNom(.)
CCAAProvCodigos <- rio::import(here::here("./data/", "CCAAProvCodigos.rdata")) # With leading 0s
#CCAAProvCodigos <- rio::import(here::here("./data/", "CCAAProvCodigos2.rdata")) # Without leading 0s

partidograndes <- c("PSOE", "PP", "VOX", "Cs", "UP-IU-EQUO", "COMPROM\u00CDS 2019", "UP-EU-MAREAS")
grandesyvascos <- c(partidograndes, "EH-BILDU", "EAJ-PNV")
grandesycat <- c(partidograndes, "ERC", "JxCAT")

# 1: Resultados por provincia:escaños y votos + % poblacion extranjera

temp0 <- rio::import("./data/raw/pobExtMuni.xls")

colnames(temp0) <- c("provincia", "porcentaje")

# Igualamos los nombres de las provincias para poder unirlos después
    CCAAProvCodigos$provincia <- limpiarProv(CCAAProvCodigos$provincia)
    temp0$provincia <- limpiarProv(temp0$provincia)

    temp0$porcentaje <- as.numeric(temp0$porcentaje)

temp1 <- left_join(CCAAProvCodigos, generales28A, by="codigo.prov")

temp2 <- left_join(CCAAProvCodigos, temp0, by="provincia")

rio::export(temp2, "./data/poblacionExtranjera.rdata")

pobExtEscanos <- left_join(temp1, temp2, by="codigo.prov") %>% select(., c(3,5,7:10,13:14))

pobExtEscanos <- pobExtEscanos[!is.na(pobExtEscanos$codigo.prov),] # Eliminamos las columnas en codigo.prov con NA

# rio::export(pobExtEscanos, "./data/poblacion_Extranjera.rdata")

pobExtEscanos <- rio::import("./data/poblacion_Extranjera.rdata")

## Grafico 01a
VOX28A <- municipios28A %>% group_by(comunidad, codigo.prov, codigo.muni) %>% top_n(1,votos) %>%
    filter(partido == "VOX") %>% ungroup() %>% group_by(codigo.prov) %>% mutate(total.votos = sum(votos), total.muni = n()) %>%
    ungroup() %>%  select(-c(4:8)) %>% unique()

ggplot(VOX28A, aes(forcats::fct_reorder(provincia, total.muni), total.muni)) + geom_col(aes(fill = total.votos)) +
    geom_text(aes(label = total.votos), nudge_y = -0.5) +
    scale_fill_viridis_c(option = "magma",
                         name = "",
                         alpha = 0.8,
                         begin = 0.3,
                         end = 0.7,
                         direction = 1,
                         guide = guide_colorbar(
                             direction = "horizontal",
                             barheight = unit(2, units = "mm"),
                             barwidth = unit(50, units = "mm"),
                             draw.ulim = F,
                             title.position = 'top',
                             # some shifting around
                             title.hjust = 0.5,
                             label.hjust = 0.5
                         )) +
    theme_plot() + theme(legend.text = element_text(size = 8)) +
    labs(title = "Provincias en las que Vox ha sido la fuerza más votada",
         subtitle = "Número total de municipios por provincias",
         caption = default_caption) + theme(axis.title.y = element_blank())

## Grafico 01b
prov_inmi <- temp2 %>% top_n(14, porcentaje) 

# mapa_prov_imi <- left_join(loadGeometry(2), temp2, by="codigo.prov")

# ggplot(mapa_prov_imi) + geom_sf(aes(fill=porcentaje))


ggplot(prov_inmi, aes(forcats::fct_reorder(provincia,porcentaje), porcentaje/100)) + 
    geom_col(aes(fill = porcentaje)) + theme_plot() + scale_y_continuous(labels = scales::percent) + 
    scale_fill_viridis_c(option = "viridis",
                         alpha = 0.8, # make fill a bit brighter
                         begin = 0.1, 
                         end = 0.9,
                         direction = -1) + 
    theme(legend.position ="none") + coord_polar() + 
    theme( axis.text.x = element_text(angle = 0, hjust = 1), axis.text = element_blank()) +
    labs(title = "Provincias con mayor porcentaje de población extranjera",
         caption = "Estadísticas del Padrón Continuo. 1 de Enero de 2019.") + ylab("")

library(treemapify)
ggplot(prov_inmi, aes(area = porcentaje, fill = porcentaje, label = provincia)) +
    geom_treemap() +
    geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                      grow = FALSE) + 
    scale_fill_viridis_c(option = "viridis",
                         name = "",
                         alpha = 0.8,
                         begin = 0.3,
                         end = 0.7,
                         direction = 1,
                         guide = guide_colorbar(
                             direction = "horizontal",
                             barheight = unit(2, units = "mm"),
                             barwidth = unit(50, units = "mm"),
                             draw.ulim = F,
                             title = "Porcentaje de población extranjera",
                             title.position = 'top',
                             # some shifting around
                             title.hjust = 0.5,
                             label.hjust = 0.5
                         )) + 
    theme(legend.position = "bottom")

# Gráfico 01c
diputados28A <- pobExtEscanos %>% filter(complete.cases(.), diputados > 0) %>% mutate(dipu_quantile = cut(as.numeric(diputados), breaks = c(1,2,5,7,11), include.lowest = TRUE)) %>% 
    mutate(porc_quantile = cut(porcentaje, breaks = 4, labels = c("Bajo", "Medio", "Medio-Alto", "Alto")))

dipu_label = c("1-2", "3-5", "6-7", "8-11")


ggplot(diputados28A, aes(forcats::fct_reorder(provincia, poblacion), forcats::fct_reorder(partido, poblacion))) + 
    geom_point(aes(size=porc_quantile, color = dipu_quantile)) + 
    theme_plot() + scale_x_discrete(breaks = waiver()) + 
    theme(panel.grid.major = element_line(colour = "gray80"), plot.subtitle = element_text(hjust = 1, size = "9")) +
    scale_fill_viridis_d(aesthetics = "color", direction = 1, option = "viridis", 
                         name = "Diputados", begin = 0.1, end = 0.8, label = dipu_label) +
    labs(title = "Relación representación-población extranjera por provincias",
         subtitle = "Mayor tamaño poblacional ⟶️", size = "Porcentaje", color = "Diputados") + ylab("") + xlab("")


# -----------------

rm(list = ls(pattern = "^temp"))
rm(list = ls(pattern = "^diputados"))
# -----



# -------------------------------------------------------------------------------
# 2: Coste de los votos

costeVotos <- generales28A %>% filter(diputados > 0) %>%  mutate(coste_dipu = votos/diputados) %>% mutate(pct = coste_dipu/poblacion)
costeVotos$partido <- as.factor(costeVotos$partido)

df3 <- costeVotos %>% group_by(partido) %>% top_n(1, coste_dipu) %>% ungroup()

ggplot(df3, aes(forcats::fct_reorder(partido, coste_dipu), coste_dipu)) + geom_col(aes(fill = provincia)) + 
    theme_plot() + 
    scale_fill_viridis_d(option="magma", alpha = 0.8, begin = 0.1, end = 0.9) + geom_text(aes(label= provincia), check_overlap = TRUE) +
    labs(title = "Circunscripción con el coste por dipitados más alto para cada partido")

    # probar coord polar
    
ggplot(df3, aes(forcats::fct_reorder(partido, coste_dipu), pct*100)) + geom_col(aes(fill = provincia)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") + geom_text(aes(label= provincia), check_overlap = TRUE) +
    scale_fill_viridis_d(option="magma") + labs(title = )

df2 <- costeVotos %>% group_by(provincia) %>% top_n(1, votos) %>% ungroup()

# Coste de diputados en Cataluña, País Vasco y Castilla-La Mancha

costeVotos$codigo.prov <- as.numeric(costeVotos$codigo.prov)

df4 <- costeVotos %>% filter(codigo.prov %in% c(17,25,43,1,20,48, 2, 13, 16, 19, 45, 8))
ggplot(df4, aes(forcats::fct_reorder(provincia, coste_dipu), coste_dipu/1000, fill=factor(partido))) + geom_col(stat = "identity", position = position_dodge()) + 
    theme_plot_facet() + 
    facet_grid(~comunidad, scales = "free", space = "free") + scale_fill_viridis_d(option="magma", alpha = 0.8, begin = 0.1, end = 0.9, name = "Partido") +
    labs(title = "Votos necesarios para obtener un diputado",
         caption = default_caption)+
    ylab("Miles de personas") + xlab("")

ggplot(df4, aes(forcats::fct_reorder(provincia, coste_dipu), pct, fill=factor(partido))) + geom_col(stat = "identity", position = position_dodge()) + 
    theme_plot_facet() + scale_y_continuous(labels = scales::percent) +
    facet_grid(~comunidad, scales = "free", space = "free") + scale_fill_viridis_d(option="magma", name = "Partido", alpha = 0.8, begin = 0.1, end = 0.9) +
    labs(title = "Importancia relativa de un voto para la obtención de un diputado",
         subtitle = "A mayor porcentaje, más 'valor' tiene el voto del ciudadano", 
         caption = default_caption) +
    ylab("Porcentaje") + xlab("")

rm(list = ls(pattern = "^df"))

#-----------------------------------------------------

# Edad media de la población por provincias y tamaño de municipio. Relacionar usando intervalos definidos por el INE

edadMedia <- readxl::read_excel("data/raw/0tamu005.xlsx", skip = 6)
colnames(edadMedia) <- edadMedia[1,]
edadMedia <- edadMedia[4:64,c(1,3:12)] %>% gather(poblacion, edad.media, 2:11)

# para futuro join dar el mismo aspecto que tendra cuando haga el cut
edadMedia$poblacion <- edadMedia %>% pull(poblacion) %>% gsub("Menos de 101 hab.", "[0,101]", .) %>% 
    gsub("Más de 100.000 hab.", "(100.001,10.000.000]", .) %>%  gsub("0 hab.", "1]", .) %>% 
    gsub("De ", "(", .) %>% gsub(" a ", ",", .) %>% gsub("\\.", "", .)
colnames(edadMedia) <- c("provincia", "edadmedia_quantile", "edad.media")

# tramos para hacer cut, el break no son intervalos sino el lower value del intervalo, "más de XX" --> establecer top value muy alto
tramos <- c(0, 101, 501, 1001, 2001, 5001, 10001, 20001, 50001, 100001, 10000000)

municipios28A <- municipios28A %>% mutate(edadmedia_quantile = cut(poblacion, breaks = tramos, include.lowest = TRUE, dig.lab=10)) %>% 
    group_by(codigo.prov, codigo.muni) %>% top_n(1, votos) %>% ungroup()

edadMedia$provincia <- edadMedia$provincia %>% limpiarProv()
edadMedia <- left_join(CCAAProvCodigos[,c(3,4)], edadMedia, by="provincia")

# unir usando factores no funciona ni a la de tres, lo paso a caracter
municipios28A$edadmedia_quantile <- as.character(municipios28A$edadmedia_quantile)
# le quitamos la columna de la provincia
municipios28A <- municipios28A[,-c(3)]

# Vamos a unirlos por el codigo de las provincias
edadmedia_map_a <- full_join(edadMedia, municipios28A) %>% mutate(quantile = ntile(as.numeric(edad.media), 5))

edadmedia_map_a$codigo.prov <- as.numeric(edadmedia_map_a$codigo.prov)
edadmedia_map_a$codigo.muni <- as.numeric(edadmedia_map_a$codigo.muni)
edadmedia_map_a <- edadmedia_map_a %>% mutate(codigo.prov = sprintf("%02d", codigo.prov),
                                              codigo.muni = sprintf("%03d", codigo.muni),
                                              INECodMuni = paste(codigo.prov, codigo.muni, sep = ""))

#rio::export(edadmedia_map_a, here::here("data", "edadMediaPob2019.rdata"))
edadmedia_map_a <- rio::import(here::here("data", "edadMediaPob2019.rdata"))

edadmedia_map_b <- left_join(edadmedia_map_a, loadGeometry(4)) 

#Mapa edad media españa
# ggplot(edadmedia_map_b) + geom_sf(aes(geometry = geometry, fill = quantile)) + theme_map() + coord_sf(xlim = c(-10,-4), ylim = c(40,45))


#Mapa edad media CV
valenciaSolo <- c("03","12","46")
valenciaLindes <- c("03","12","46", "43", "16", "44", "30", "02")

edadmedia_map_cv <- edadmedia_map_b %>% filter(as.numeric(codigo.prov) %in% as.numeric(valenciaSolo))
edadmedia_map_cvlindes <- edadmedia_map_b %>% filter(as.numeric(codigo.prov) %in% as.numeric(valenciaLindes))



# valenciaLindes <- loadGeometry(1, provincias = valenciaLindes)
## mapa solo CV edad media
ggplot() + geom_sf(data=loadGeometry(1, provincias = valenciaLindes), aes(geometry = geometry)) + 
    geom_sf(data=edadmedia_map_cv, aes(geometry = geometry, fill = edad.media)) + 
    coord_sf(xlim = c(-1.5, 0.7), ylim = c(37.5,40.8)) + theme_map() + theme(legend.position = "none") +
    scale_fill_viridis_d(option = "magma", direction = -1, begin = 0.1, end = 0.8, alpha = 0.8)


## mapa CV y provincias que lindan --> todos los municipios que salen en el cut de los límites
ggplot() + geom_sf(data=loadGeometry(6, provincias = valenciaLindes), aes(geometry = geometry), color = "gray75") + 
    geom_sf(data=loadGeometry(3, valenciaLindes), aes(geometry = geometry), alpha = 0.05, size = 1) +
    geom_sf(data=edadmedia_map_cvlindes, aes(geometry = geometry, fill = quantile, alpha = 0.20)) +
    geom_sf(data=edadmedia_map_cv, aes(geometry = geometry, fill = quantile)) + 
    coord_sf(xlim = c(-1.5, 0.7), ylim = c(37.5,40.8)) + theme_map() + theme(legend.position = "right") +
    scale_fill_viridis_c(option = "magma", direction = -1, begin = 0.1, end = 0.7, alpha = 0.8)

# testGeom <- loadGeometry(6, provinvias=valenciaLindes) %>% st_set_geometry(NULL)

# ----------------------------
# ¿ Existe relación entre coste de votos y representación ?

votosRelacion <- generales28A %>% group_by(codigo.prov) %>%
    filter(diputados > 0) %>%
    mutate(total.diputados = sum(diputados, na.rm=TRUE)) %>% 
    top_n(1, votos) %>% ungroup()
 
votantes28A <- rio::import("./data/generales28ATipoVoto.rdata") %>% filter(tipo.voto == "total.censo.electoral")

votosRelacion <- left_join(votosRelacion, votantes28A)

rm("votantes28A")

# votosRelacion_map <- left_join(votosRelacion_b, loadGeometry(1))

# ggplot(votosRelacion_map) + geom_sf(aes(geometry = geometry, fill = total.diputados))
    
votosRelacion <- votosRelacion %>% mutate(diputados_quantile = cut(total.diputados, breaks = c(1,3,7,40), include.lowest = TRUE, labels = c(1,2,3)),
                                              total.votantes_quantile = ntile(tipo.voto.num, 3),
                                  group = paste(diputados_quantile, total.votantes_quantile, sep = " - ")) %>% 
    group_by(total.diputados) %>% mutate(mean = mean(tipo.voto.num), desv = tipo.voto.num - mean)


unique(votosRelacion$group)

bivariate_color_scale <- tibble(
    "3 - 3" = "#3F2949", # high, high #3F2949
    "2 - 3" = "#435786", # medium, high
    "1 - 3" = "#4885C1", # low, high
    "3 - 2" = "#77324C", # high, medium
    "2 - 2" = "#806A8A", # medium, medium #806A8A
    "1 - 2" = "#89A1C8", # low, medium
    "3 - 1" = "#AE3A4E", # high, low
    "2 - 1" = "#BC7C8F", # medium, low
    "1 - 1" = "#CABED0" # low , low #CABED0
) %>%
    gather("group", "fill")

votosRelacion <- left_join(votosRelacion, bivariate_color_scale)

votosRelacion_map <- left_join(votosRelacion, loadGeometry(2))

boundariesShape <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
    filter(adm0_a3 %in% c("FRA", "PRT", "MAR", "ITA", "DZA"))

plot <- ggplot(votosRelacion_map) + geom_sf(data=boundariesShape, color = "gray85") + 
    geom_sf(aes(geometry = geometry, fill=fill), show.legend = FALSE, color = "gray85") + 
    annotate("text", label="Media representatividad \n Censo electoral bajo", x=5, y=42.5 ) +
    annotate("text", label="Media representatividad \n Censo electoral alto", x=2.5, y=38 ) +
    geom_curve(aes(x = 1, y = 42, xend = 5, yend = 42), curvature = 0.2) +
    geom_curve(aes(x = -3.5, y = 37, xend = 2.5, yend = 37.5), curvature = 0.2) +
    scale_fill_identity() + coord_sf(xlim = c(-12,7), ylim = c(34,44)) +
    labs(title = "¿Son todas las provincias de España igual de representativas?",
         caption = default_caption) + theme_map()

plot

# Draw the legend
bivariate_color_scale <- bivariate_color_scale %>%
    separate(group, into = c("diputados", "votantes"), sep = " - ") %>%
    mutate(votantes = as.factor(votantes),
           diputados = as.factor(diputados))

legend <- ggplot() +
    geom_tile(
        data = bivariate_color_scale,
        mapping = aes(
            x = diputados,
            y = votantes,
            fill = fill)
    ) +
    scale_fill_identity() +
    labs(x = "Mayor representación ⟶️",
         y = "Mayor censo electoral ⟶️") +
    # make font small enough
    theme(
        axis.title = element_text(size = 9)
    ) +
    # quadratic tiles
    coord_fixed() +
    theme(axis.text = element_blank(),
          plot.background = element_rect(fill = "gray95"),
          panel.background = element_rect(fill = "gray95"),
          panel.grid.major = element_line(color = "gray95"),
          axis.ticks = element_blank())

legend


#gridExtra::grid.arrange(plot, legend, ncol=2)

library(cowplot)
ggdraw() +
    draw_plot(plot, 0, 0, 1, 1) +
    draw_plot(legend, 0.65, 0.10, 0.25, 0.25)

# plotly::ggplotly(plot)

# --------------------------
# unique(municipios28A$partido)

# Sección 3:  Mapa + tabla

compromisERC <- municipios28A %>% filter(codigo.prov %in% c("03","12","46", "08","17", "43", "25"), 
                                      partido %in% c("COMPROMIS-EUPV", "ERC"),
                                      votos > 0) %>% 
                mutate(INECodMuni = paste(codigo.prov, codigo.muni, sep = ""),
                       votos_quantile = ntile(votos, 5)) 

compromisERC_mapa <- left_join(compromisERC, loadGeometry(6, provincias = c("03","12","46", "08","17", "43", "25")))

provinciasLindes <- c("22", "44", "50", "16", "44", "30", "02", "07", "31", "19")

fraSF <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% filter(adm0_a3 == "FRA")

ggplot() + geom_sf(data = fraSF, aes(geometry = geometry)) +
    geom_sf(aes(geometry = geometry), data=filter(loadGeometry(6, provincias = provinciasLindes)), color="gray85") +
    geom_sf(aes(geometry = geometry), data=filter(loadGeometry(1, provincias = provinciasLindes)), alpha=0.1) +
    geom_sf(aes(geometry = geometry, fill=votos_quantile), data=compromisERC_mapa) +
    scale_fill_viridis(
        option = "magma", 
        direction = -1,
        begin = 0.3, end = 0.8,
        name = "Votos",
        # here we use guide_colourbar because it is still a continuous scale --> https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/
        guide = guide_colorbar(
            direction = "horizontal",
            barheight = unit(2, units = "mm"),
            barwidth = unit(50, units = "mm"),
            draw.ulim = F,
            title.position = 'top',
            # some shifting around
            title.hjust = 0.5,
            label.hjust = 0.5
        )) +
    coord_sf(xlim = c(-1.5, 4), ylim = c(38,42.7)) + theme_map() + theme(legend.position = "bottom")

# ----

votantes28A <- rio::import("./data/generales28ATipoVoto.rdata") %>% filter(tipo.voto == "total.censo.electoral",
                                                                           codigo.prov %in% c("03","12","46", "08","17", "43", "25"))

tableCompromis <- generales28A %>% filter(partido %in% c("COMPROMIS-EUPV", "ERC"), votos > 0) %>% left_join(votantes28A) %>% 
    select(c(3,5,6,7,10))
colnames(tableCompromis) <- c("Provincia", "Partido", "Nº votos", "Diputados", "Total censo electoral")
    
Provincias %>% st_set_geometry(NULL)

# ------------

edadmedia <- rio::import(here::here("data", "edadMediaPob2019.rdata")) %>% filter(codigo.prov %in% c("03","12","46", "08","17", "43", "25"))

edadmedia_map <- left_join(edadmedia, loadGeometry(6, provincias = c("03","12","46", "08","17", "43", "25")))

ggplot() + geom_sf(data = fraSF, aes(geometry = geometry)) +
    geom_sf(data=loadGeometry(6, provincias = provinciasLindes), aes(geometry = geometry), color = "gray85") + 
    geom_sf(data=loadGeometry(3, provinciasLindes), aes(geometry = geometry), alpha = 0.1) +
    geom_sf(data=edadmedia_map, aes(geometry = geometry, fill = quantile)) + 
    scale_fill_viridis(
        option = "magma", 
        direction = -1,
        begin = 0.3, end = 0.8,
        name = "Edad Media",
        # here we use guide_colourbar because it is still a continuous scale --> https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/
        guide = guide_colorbar(
            direction = "horizontal",
            barheight = unit(2, units = "mm"),
            barwidth = unit(50, units = "mm"),
            draw.ulim = F,
            title.position = 'top',
            # some shifting around
            title.hjust = 0.5,
            label.hjust = 0.5
        )) +
    coord_sf(xlim = c(-1.5, 4), ylim = c(38,42.7)) + theme_map() + theme(legend.position = "bottom")
