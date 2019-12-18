# Trabajo Final: Programación y Manejo de datos en la Era del Big Data
#
# Alumnos: Amparo Mocholí  
#           Hugo Sáiz   
#           Javier Cotanda 
#
# Archivo: Datos: Mapas
# 

# Librerias
library(tidyverse)
library(sf)
library(LAU2boundaries4spain)


generales28A <- rio::import(here::here("./data/", "generales28A.rdata")) %>% partidosNom(.)
municipios28A <- rio::import(here::here("./data/", "municipios28A.rdata"))
CCAAProvCodigos <- rio::import(here::here("./data/", "CCAAProvCodigos.rdata"))
#Importamos las funciones del file funciones.R
source("funciones.R")

# Ejemplo 3: Mapa de los municipios
# Fuentes: https://pybonacci.org/2017/04/17/como-hacer-un-mapa-muy-bonito-de-espana-en-ggplot2/

municipios28A$codigo.muni <- as.numeric(municipios28A$codigo.muni)
municipios28A$codigo.prov <- as.numeric(municipios28A$codigo.prov)

municipios28A <- municipios28A %>% mutate(codigo.prov = sprintf("%02d", codigo.prov)) %>% 
    mutate(codigo.muni = sprintf("%03d", codigo.muni))

municipios28A$INECodMuni <- paste(municipios28A$codigo.prov, municipios28A$codigo.muni, sep = "")

df <- municipios28A %>% group_by(INECodMuni) %>% top_n(1, votos) %>% ungroup() %>% filter(votos > 0, !codigo.prov %in% c(35,38)) 
df <- left_join(df, loadGeometry(4), by="INECodMuni")

ggplot(df) + geom_sf(aes(fill = partido, geometry = geometry)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") + 
    scale_fill_viridis_d()

# mapa animado por provincias coste diputados, 2011 y ss, 

# Mapa: Ganador de las elecciones por provincias


provGanador <- left_join(
    genMasVotado(generales28A, 2019), 
    loadGeometry(2), 
    by="codigo.prov"
)

# provGanador <- joinGeomProv(genMasVotado(generales28A, 2019), 2)

ggplot(provGanador) + geom_sf(aes(fill = partido, geometry = geometry), color = "gray90", size = 0.06) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") + 
    geom_segment(aes(x = -0.5, y = 35, xend = -0.5, yend = 36.5), color="gray85") +
    geom_segment(aes(x = -0.5, y = 36.5, xend = 0.8, yend = 37.8), color="gray85") +
    geom_segment(aes(x = 0.8, y = 37.8, xend = 5, yend = 37.8), colour="gray85") +
    scale_fill_manual(values = partidosIdentity) + theme_map() +
    labs(title = "Partido más votado el 26A",
         subtitle = "Por provincias",
         caption = "Elaboración propia. Datos: Ministerio del Interior",
         fill = "")

# ---------------------------------------
# Mapa animado de los ganadores de las elecciones desde 2011

generales28A <- rio::import(here::here("./data/", "generales28A.rdata")) %>% partidosNom(.)
generales26J16 <- rio::import(here::here("./data/", "generales26J16.rdata")) %>% partidosNom(.)
generales20D15 <- rio::import(here::here("./data/", "generales20D15.rdata")) %>% partidosNom(.)
generales20N11 <- rio::import(here::here("./data/", "generales20N11.rdata")) %>% partidosNom(.)



temp1 <- genMasVotado(generales28A, 2019) 
temp2 <- genMasVotado(generales26J16, 2016)
temp3 <- genMasVotado(generales20D15, 2015)
temp4 <- genMasVotado(generales20N11, 2011)

temp5 <- rbind(temp1, temp2, temp3, temp4) %>% mutate(partido = as.factor(partido))

mapaDf <- joinGeomProv(temp5)

rm(list = ls(pattern = "^temp"))

ggplot(mapaDf) + geom_sf(aes(fill = partido, geometry = geometry), color = "gray90", size = 0.06) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") + 
    geom_segment(aes(x = -0.5, y = 35, xend = -0.5, yend = 36.5), color="gray85") +
    geom_segment(aes(x = -0.5, y = 36.5, xend = 0.8, yend = 37.8), color="gray85") +
    geom_segment(aes(x = 0.8, y = 37.8, xend = 5, yend = 37.8), colour="gray85") +
    scale_fill_manual(values = partidosIdentity) + theme_map() +
    labs(title = 'Partido más votado el 26A',
         subtitle = "Por provincias",
         caption = "Elaboración propia. Datos: Ministerio del Interior",
         fill = "")  + facet_wrap(~anyo)


# -----------------------------

municipios28A$codigo.prov <- as.numeric(municipios28A$codigo.prov)

cv28A <- municipios28A %>% filter(codigo.prov %in% c(3,12,46)) %>% group_by(codigo.prov, codigo.muni) %>% top_n(1,votos) %>% ungroup() 

municipios_2018_b <- municipios_2018 %>% filter(INECodProv %in% c("03", "12", "46"))

cv28A$codigo.prov <- as.numeric(cv28A$codigo.prov)
cv28A$codigo.muni <- as.numeric(cv28A$codigo.muni)

cv28A <- cv28A %>% mutate(codigo.prov = sprintf("%02d", codigo.prov)) %>% 
    mutate(codigo.muni = sprintf("%03d", codigo.muni))
cv28A$INECodMuni <- paste(cv28A$codigo.prov, cv28A$codigo.muni, sep = "")

municipios_b <- municipios_2018 %>% st_set_geometry(NULL)

mapaCV <- left_join(cv28A, municipios_2018_b, by="INECodMuni")

ggplot(mapaCV) + geom_sf(aes(fill=partido, geometry = geometry), color = "gray75", size = 0.06) + 
    scale_fill_manual(values = c("PSOE" = "#ef5350", "PP" = "#64b5f6", 
                                 "VOX" = "green", "UP_IU_EQUO" = "purple", "CS" = "orange", 
                                 "COMPROMIS_2019" = "yellow")) +
    theme_map()



    