## Análisis de población en edad de estudiar y ubicación de escuelas
# Ciudad del Carmen, Campeche, México

# Cargamos librerías a utilizar 
library(tidyverse)
library(sf)
library(sp)
library(tmap)

# Cargar datos y cambio a sf en algunos objetos
# Censo de población y vivienda 
censo2020 <- read_csv("data/conjunto_de_datos_ageb_urbana_04_cpv2020.csv")
# Mapa a nivel ageb
agebmap <- st_read("data", "04a")
# Mapa DENUE y cambio a objeto sf
#denueCAM <- rgdal::readOGR("data", "denue_inegi_04_", encoding ="latin1")
#denueCAM <- st_as_sf(denueCAM)
#Exportamos denueCAM a RDS para trabajar posteriormente
#saveRDS(denueCAM, "data/denueCAM.rds")
# Importar denueCAM.rds 
denueCAM <- readRDS("data/denueCAM.rds")

# Mapa municipal, cambio a objeto sf y filtro de municipio elegido
munmapa <- rgdal::readOGR("data", "04mun", encoding ="latin1")
munmapa <- st_as_sf(munmapa) %>%
    filter(NOMGEO == "Carmen")
# Mapa de ejes y selección de variables de interes
ejesmapa <- st_read("data", "04e")
ejesmapa <- ejesmapa %>% 
  filter(CVE_MUN %in% c("004", "013")) %>% 
  filter(TIPOVIAL %in% c("Avenida", "Periférico", 
                         "Calzada", "Eje Vial", 
                         "Carretera", "Boulevard"))

# Limpieza y procesado de Censo2020 para obtener datos a nivel AGEB
# del municipio de Carmen
# vector con variables de interés 
variables <- c("POBTOT", "POBFEM", "POBMAS", 
               "P_3A5", "P_3A5_F", "P_3A5_M",
               "P_6A11", "P_6A11_F", "P_6A11_M", 
               "P_12A14", "P_12A14_F", "P_12A14_M", 
               "P_15A17", "P_15A17_F", "P_15A17_M", 
               "P_18A24", "P_18A24_F", "P_18A24_M", 
               "P3A5_NOA", "P3A5_NOA_F", "P3A5_NOA_M", 
               "P6A11_NOA", "P6A11_NOAF", "P6A11_NOAM",
               "P12A14NOA", "P12A14NOAF","P12A14NOAM", 
               "P15A17A", "P15A17A_F", "P15A17A_M", 
               "P18A24A", "P18A24A_F", "P18A24A_M", 
               "GRAPROES", "GRAPROES_F", "GRAPROES_M",
               "CVEGEO"
)

# Filtrado de datos de interés
ageb2020data <- censo2020 %>%
  filter(NOM_LOC == "Total AGEB urbana", NOM_MUN == "Carmen") %>%
  mutate(CVEGEO = paste(ENTIDAD, MUN, LOC, AGEB, sep = "")) %>%
  select(any_of(variables))

# Reemplazar * por NA 
ageb2020data[ageb2020data == "*"] <- NA

# Cambiar datos chr a dbl con la función APPLY
# Definir vector con columnas a transformar
vec <- c(1:36)
ageb2020data[ ,vec] <- apply(ageb2020data[ , vec, drop=F], 2,
                             function(x) as.double(as.character(x)))
# Eliminar datos innecesarios 
rm(variables, vec, censo2020)

glimpse(ageb2020data)

## Limpieza y procesado de denueCAM para filtrar solo valores instituciones 
# educativas de Carmen 
# Actividades de interes
scian <- c(611111, 611112, # Preescolar 1 privado 2 particular
           611121, 611122, # Primaria
           611131, 611132, # Secundaria general
           611141, 611142, # Secundaria técnica
           611151, 611152, # Media técnica
           611161, 611162, # Media superior
           611211, 611212, # Técnica superior
           611311, 611312) # Educación superior  

# Variables de interés
variables <- c(
  "nom_estab", "nombre_act", 
  "nomb_asent", "geometry", 
  "CVEGEO"
)

# Crear base de datos filtrando municipios de interés, actividades de interés 
denueESC <- denueCAM %>%
  filter(municipio == "Carmen", codigo_act %in% scian) %>%
  # Crear variable CVEGEO para unir a la ageb2020data
  mutate(CVEGEO = paste(cve_ent, cve_mun, cve_loc, ageb, sep = "")) %>%
  select(any_of(variables)) 
rm(denueCAM, scian, variables)

# Crear nuevas categorías para el nombre de actividad 
denueESC$tipo_esc <- "a"
denueESC$tipo_esc <- with(denueESC, 
                          ifelse(denueESC$nombre_act == "Escuelas de educación preescolar del sector público",
                                 "PREE_PUB", tipo_esc))
denueESC$tipo_esc <- with(denueESC,
                          ifelse(denueESC$nombre_act == "Escuelas de educación preescolar del sector privado",
                                 "PREE_PRI", tipo_esc))
denueESC$tipo_esc <- with(denueESC,
                          ifelse(denueESC$nombre_act == "Escuelas de educación primaria del sector privado" ,
                                 "PRIM_PRI", denueESC$tipo_esc))
denueESC$tipo_esc <- with(denueESC,
                          ifelse(denueESC$nombre_act == "Escuelas de educación primaria del sector público",
                                 "PRIM_PUB", denueESC$tipo_esc))
denueESC$tipo_esc <- with(denueESC,
                          ifelse(denueESC$nombre_act == "Escuelas de educación secundaria general del sector público",
                                 "SEC_PUB", denueESC$tipo_esc))
denueESC$tipo_esc <- with(denueESC,
                          ifelse(denueESC$nombre_act == "Escuelas de educación secundaria general del sector privado",
                                 "SEC_PRI", denueESC$tipo_esc))
denueESC$tipo_esc <- with(denueESC,
                          ifelse(denueESC$nombre_act == "Escuelas de educación secundaria técnica del sector público" ,
                                 "SECTEC_PUB", denueESC$tipo_esc))
denueESC$tipo_esc <- with(denueESC,
                          ifelse(denueESC$nombre_act == "Escuelas de educación media superior del sector público"  ,
                                 "EMS_PUB", denueESC$tipo_esc))
denueESC$tipo_esc <- with(denueESC,
                          ifelse(denueESC$nombre_act == "Escuelas de educación media superior del sector privado"  ,
                                 "EMS_PRI", denueESC$tipo_esc))
denueESC$tipo_esc <- with(denueESC,
                          ifelse(denueESC$nombre_act == "Escuelas de educación media técnica terminal del sector privado"  ,
                                 "EMSTEC_PRI", denueESC$tipo_esc))
denueESC$tipo_esc <- with(denueESC,
                          ifelse(denueESC$nombre_act == "Escuelas de educación superior del sector privado" ,
                                 "ESCSUP_PRI", denueESC$tipo_esc))
denueESC$tipo_esc <- with(denueESC,
                          ifelse(denueESC$nombre_act == "Escuelas de educación superior del sector público" ,
                                 "ESCSUP_PUB", denueESC$tipo_esc))

glimpse(denueESC)
head(denueESC)

# Crear tabla con el numero de escuelas por AGEB 
denueAGEB <- denueESC %>%
  # Se convierte a tibble para descartar que datos espaciales causen error 
  as_tibble() %>% 
  select(tipo_esc, CVEGEO) %>% 
  count(CVEGEO, tipo_esc) %>%
  pivot_wider(names_from = "tipo_esc", 
              values_from = "n", 
              values_fill = 0) %>%
  mutate(total = rowSums(.[2:12]),
  ) # obtener total de escuelas por AGEB

glimpse(denueAGEB)
head(denueAGEB)

# Agregar denueAGEB a AGEBdata2020
datos <- ageb2020data %>%
  full_join(denueAGEB, by = "CVEGEO")
rm(ageb2020data, denueAGEB)

glimpse(datos)

# Elegir solo datos para preescolares y primarias publicas y privadas 
pres_prim <- datos %>%
  select(CVEGEO, POBTOT, 
         P_3A5, #P_3A5_F, P_3A5_M, 
         P_6A11, #P_6A11_F, P_6A11_M, 
         #P3A5_NOA, #P3A5_NOA_F, P3A5_NOA_M, 
         #P6A11_NOA, #P6A11_NOAF, P6A11_NOAM, 
         PREE_PUB, PREE_PRI, 
         PRIM_PUB, PRIM_PRI) %>%
  # Se crean variables con valores totales 
  # Según las variables elegidas 
  mutate(TOTAL = rowSums(.[5:8], na.rm = TRUE), 
         TOTALPRIV = rowSums(.[c(6, 8)], na.rm = TRUE),
         TOTALPUB = rowSums(.[c(5, 7)], na.rm = TRUE), 
         TOTALPRIM = rowSums(. [c(7,8)], na.rm = TRUE), 
         TOTALPREES = rowSums(. [c(5,6)], na.rm = TRUE)
  )

rm(datos)
glimpse(pres_prim)

# Crear denue_PRES_PRIM solo con preescolares y primarias de dataframe denueESC
denue_PRES_PRIM <- denueESC %>%
  filter(tipo_esc %in% c("PREE_PUB", 
                         "PREE_PRI", 
                         "PRIM_PUB", 
                         "PRIM_PRI"))
# Remover elementos utilizados
rm(denueESC)

# Unir datos a mapa 
mapa_pree_prim <- agebmap %>%
  filter(CVE_MUN == "003") %>%
  left_join(pres_prim, by = "CVEGEO") %>%
  select(-CVE_ENT, -CVE_LOC, -CVE_AGEB) 
rm(agebmap)

# Conocer las AGEB que se pierden de tabla datos al unir con capa espacial 
pres_prim$CVEGEO[pres_prim$CVEGEO %in% mapa_pree_prim$CVEGEO == FALSE]
# Crear vector con datos que coinciden para filtrar 
sobrantes <- pres_prim$CVEGEO[pres_prim$CVEGEO %in% mapa_pree_prim$CVEGEO == TRUE] 

# Eliminar de datos AGEB que no se integran a mapadatos 
pres_prim <- pres_prim %>%
  filter(CVEGEO %in% sobrantes)
rm(sobrantes)
# Comprobar eliminación de sobrantes
pres_prim$CVEGEO[pres_prim$CVEGEO %in% mapa_pree_prim$CVEGEO == FALSE] 

glimpse(pres_prim)
glimpse(mapa_pree_prim)

