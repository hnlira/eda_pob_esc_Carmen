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

