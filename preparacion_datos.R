# Trabajo Final: Programación y Manejo de datos en la Era del Big Data
#
# Alumnos: Amparo Mocholí   
#           Hugo Sáiz       
#           Javier Cotanda  
#
# Título: 
# Archivo: Pre-filtrado de datos
#


# Cargamos librerias y funciones (automatización en el filtrado)
library(tidyverse)

resultadoDiputados <- function(df, rows){
    g <- df
    rows <- rows
    
    ## Generamos un df solo con los datos de las provincias y su poblacion
    pobCom <- g[5:rows, 1:4]
    colnames(pobCom)[1:4] <- c("comunidad", "codigo.prov", "provincia", "poblacion")

    
    ## Generamos un df para los votos y diputados del 
    gen <- g %>% mutate("codigo.prov" = X2 ) %>% select(-c(1, 3:16)) %>% select(c(1, length(.), 2:(length(.)-1)))
    names(gen) <- as.character(gen[3,])
    
    even_indexes<-seq(2,length(gen),2)
    odd_indexes<-seq(1,length(gen),2)
    
    diputados <- gen[,even_indexes]
    votos <- gen[,odd_indexes]
    
    diputados <- diputados[5:rows,]
    colnames(diputados) <- colnames(votos)
    diputados <- gather(diputados, partido, diputados, 2:length(diputados))
    colnames(diputados)[1] <- "codigo.prov"
    
    votos <- votos[5:rows,]
    votos <- gather(votos, partido, votos, 2:length(votos))
    colnames(votos)[1] <- "codigo.prov"
    
    diputados <- left_join(pobCom, diputados, by="codigo.prov")
    
    
    votos <- left_join(pobCom, votos, by="codigo.prov")
    votos <- filter(votos, votos > 0)
    
    # Unimos los df para crear el definitivo
    gen <- full_join(votos, diputados)
    
    gen$codigo.prov <- as.numeric(gen$codigo.prov)
    gen$codigo.prov <- sprintf("%02d", gen$codigo.prov)
    gen$provincia <- trimws(gen$provincia, "r")
    gen$comunidad <- trimws(gen$comunidad, "r")
    
    gen[,c(4,6,7)] <- sapply(gen[,c(4,6,7)], as.numeric)

    rm(list = c("g", "pobCom", "votos", "diputados"))
    
    return(gen)
    
}

resultadoDiputadosTipoVotos <- function(df, rows){
    g <- df
    rows <- rows
    
    ## Generamos un df solo con los datos de las provincias y su poblacion
    pobCom <- g[4:rows, 1:16]
    colnames(pobCom) <- pobCom[1,] %>% tolower(.) %>% gsub(" de ", " ", .) %>% 
        gsub(" ", ".", .) %>% chartr('áéíóúñ','aeioun', .)
    colnames(pobCom)[1:3] <- c("comunidad", "codigo.prov", "provincia")
    pobCom <- pobCom[-c(1),] %>% gather(tipo.voto, tipo.voto.num, 6:16 )
    
    pobCom$codigo.prov <- as.numeric(pobCom$codigo.prov)
    pobCom$codigo.prov <- sprintf("%02d", pobCom$codigo.prov)
    pobCom$provincia <- trimws(pobCom$provincia, "r")
    pobCom$comunidad <- trimws(pobCom$comunidad, "r")

    pobCom[,c(4,5,7)] <- sapply(pobCom[,c(4,5,7)], as.numeric)
    
    rm(list = c("g", "rows"))
    
    return(pobCom)
    
}

resultadoMunicipios <- function(df, rows){
    g <- df
    rows <- rows
    
    colnames(g) <- as.character(g[3,])
    # g <- janitor::clean_names(g)
    municipios <- g[4:rows,]
    municipios <- municipios %>% select(-c(7:13))
    municipios <- municipios %>% gather(partido, votos, 7:length(.))
    colnames(municipios) <- c("comunidad", "codigo.prov", "provincia", "codigo.muni", 
                              "municipio","poblacion", "partido", "votos")
    municipios$poblacion <- as.numeric(municipios$poblacion)
    municipios$votos <- as.numeric(municipios$votos)
    municipios$codigo.prov <- as.numeric(municipios$codigo.prov)
    municipios$codigo.prov <- sprintf("%02d", municipios$codigo.prov)
    municipios$codigo.muni <- as.numeric(municipios$codigo.muni)
    municipios$codigo.muni <- sprintf("%03d", municipios$codigo.muni)
    municipios$provincia <- trimws(municipios$provincia, "r")
    municipios$comunidad <- trimws(municipios$comunidad, "r")

    return(municipios)
    
    rm(list = c("g", "municipios", "rows"))
}

resultadoMunicipiosTipoVotos <- function(df, rows){
    g <- df
    rows <- rows
    
    colnames(g) <- as.character(g[3,])
    # g <- janitor::clean_names(g)
    municipios <- g[4:rows,1:13]
    municipios <- municipios %>% select(-c(7)) %>% gather(tipo.voto, tipo.voto.num, 8:length(.))
    colnames(municipios) <- c("comunidad", "codigo.prov", "provincia", "codigo.muni", 
                              "municipio","poblacion", "censo.electoral", "tipo.voto", "tipo.voto.num")
    municipios$poblacion <- as.numeric(municipios$poblacion)
    municipios$votos <- as.numeric(municipios$tipo.voto.num)
    municipios$codigo.prov <- as.numeric(municipios$codigo.prov)
    municipios$codigo.prov <- sprintf("%02d", municipios$codigo.prov)
    municipios$codigo.muni <- as.numeric(municipios$codigo.muni)
    municipios$codigo.muni <- sprintf("%03d", municipios$codigo.muni)
    municipios$provincia <- trimws(municipios$provincia, "r")
    municipios$comunidad <- trimws(municipios$comunidad, "r")
    
    return(municipios)
    
    rm(list = c("g", "municipios", "rows"))
}

# Impotarmos datos, covertimos y exportamos
generales28A <- openxlsx::read.xlsx(xlsxFile = here::here("data/raw", "PROV_02_201904_1.xlsx"), 
                                    fillMergedCells = TRUE, colNames = FALSE) %>% 
    resultadoDiputados(., 56)

rio::export(generales28A, "./data/generales28A.rdata")

generales26J16 <- openxlsx::read.xlsx(xlsxFile = here::here("data/raw", "PROV_02_201606_1.xlsx"), 
                                    fillMergedCells = TRUE, colNames = FALSE) %>% 
    resultadoDiputados(., 56)

rio::export(generales26J16, "./data/generales26J16.rdata")

generales20D15 <- openxlsx::read.xlsx(xlsxFile = here::here("data/raw", "PROV_02_201512_1.xlsx"), 
                                    fillMergedCells = TRUE, colNames = FALSE) %>% 
    resultadoDiputados(., 56)

rio::export(generales20D15, "./data/generales20D15.rdata")

generales20N11 <- openxlsx::read.xlsx(xlsxFile = here::here("data/raw", "PROV_02_201111_1.xlsx"), 
                                    fillMergedCells = TRUE, colNames = FALSE) %>% 
    resultadoDiputados(., 56)

rio::export(generales20N11, "./data/generales20N11.rdata")



municipios28A <- openxlsx::read.xlsx(xlsxFile = here::here("data/raw", "02_201904_1.xlsx"), 
                                     fillMergedCells = TRUE, colNames = FALSE) %>% 
    resultadoMunicipios(., 8134)

rio::export(municipios28A, "./data/municipios28A.rdata")

municipios28ATipoVoto <- openxlsx::read.xlsx(xlsxFile = here::here("data/raw", "02_201904_1.xlsx"), 
                                     fillMergedCells = TRUE, colNames = FALSE) %>% 
    resultadoMunicipiosTipoVotos(., 8134)

rio::export(municipios28ATipoVoto, "./data/municipios28ATipoVoto.rdata")


generales28ATipoVoto <- openxlsx::read.xlsx(xlsxFile = here::here("data/raw", "PROV_02_201904_1.xlsx"), 
                                    fillMergedCells = TRUE, colNames = FALSE) %>% 
    resultadoDiputadosTipoVotos(., 56)

rio::export(generales28ATipoVoto, "./data/generales28ATipoVoto.rdata")
