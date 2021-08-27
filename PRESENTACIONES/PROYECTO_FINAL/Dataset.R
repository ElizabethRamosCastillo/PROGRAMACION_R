
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(tidyverse)



##PRIMERA OLA

#FALLECIDOS

data2 <- read.csv("D:/UNMSM_CICLOS/Ciclo_9/Programacion/PROYECTO FINAL/fallecidos_covid.csv")
head(data)

df2 <- tibble(data2)

FallecidosCovid <- df2 %>% 
  dplyr::select(-UUID) %>% 
  mutate(Numero = rep(1)) %>% 
  mutate(FECHA_FALLECIMIENTO = paste0(str_sub(FECHA_FALLECIMIENTO,1, 4),"-",str_sub(FECHA_FALLECIMIENTO, 5, 6),"-",str_sub(FECHA_FALLECIMIENTO, 7, 8))) %>% 
  mutate(FECHA_FALLECIMIENTO = as.Date(sprintf("%1$s", FECHA_FALLECIMIENTO))) %>% 
  arrange(FECHA_FALLECIMIENTO) %>% 
  dplyr::filter(FECHA_FALLECIMIENTO >= "2020-03-03" & FECHA_FALLECIMIENTO <= "2020-11-15" ) %>% 
  group_by(PROVINCIA) %>% 
  summarise(Fallecidos_1ola = sum(Numero,na.rm = TRUE))

  FallecidosCovid <- FallecidosCovid[-1,] 

View(FallecidosCovid)



#CASOS COVID

data <- read.csv("D:/UNMSM_CICLOS/Ciclo_9/Programacion/PROYECTO FINAL/positivos_covid.csv",)
head(data)

df <- tibble(data)
View(df)

CasosCovid <- df %>% 
  mutate(Numero = rep(1)) %>% 
  mutate(FECHA_RESULTADO = paste0(str_sub(FECHA_RESULTADO,1, 4),"-",str_sub(FECHA_RESULTADO, 5, 6),"-",str_sub(FECHA_RESULTADO, 7, 8))) %>% 
  mutate(FECHA_RESULTADO = as.Date(sprintf("%1$s", FECHA_RESULTADO))) %>% 
  arrange(FECHA_RESULTADO) %>% 
  dplyr::filter(FECHA_RESULTADO >= "2020-03-29" & FECHA_RESULTADO <= "2020-11-15" ) %>% 
  group_by(PROVINCIA) %>% 
  summarise(CasosCovid_1ola = sum(Numero,na.rm = TRUE))


CasosCovid <- CasosCovid[-72,] %>% 
    mutate(PROVINCIA = ifelse(PROVINCIA == "CAÃ‘ETE","CANIETE",PROVINCIA)) %>% 
  arrange(PROVINCIA)

View(CasosCovid)


##########################


#CAMAS UCI, UCIN y VENTILADORES TOTALES


data_ucit <- read.csv("data/camas_uci/Camas_30-03-2021_0.csv", sep = "|") %>% tibble()

#data_ucit %>% group_by(PROVINCIA) %>% summarise() %>% view()


Camas_VENT1 <- data_ucit %>% 
  dplyr::select(2:32, -FECHA_REGISTRO,-INSTITUCION, UBIGEO, CODIGO) %>% 
  mutate(FECHA_CORTE = paste0(str_sub(FECHA_CORTE,1, 4),"-",str_sub(FECHA_CORTE, 5, 6),"-",str_sub(FECHA_CORTE, 7, 8))) %>% 
  mutate(FECHA_CORTE = as.Date(sprintf("%1$s", FECHA_CORTE))) %>% 
  arrange(FECHA_CORTE) %>% 
  dplyr::filter(FECHA_CORTE >= "2020-03-03" & FECHA_CORTE <= "2020-11-15" ) %>% 
  group_by(PROVINCIA) %>% 
  summarise(UCI_1ola = sum(CAMAS_ZC_TOTAL, na.rm = TRUE),
            UCIN_1ola = sum(CAMAS_ZNC_TOTAL,na.rm = TRUE),
            VENT_1ola = sum(VENTILADORES_UCI_ZC_TOTAL,na.rm = TRUE))


Camas_VENT1 <- Camas_VENT1[-1,]

View(Camas_VENT1)

#UCI - UCIN - VENTIL_M

Camas_VENT1 

PROVINCIA <- c("AIJA","ACOMAYO", "AMBO", "ANTA", "ANTABAMBA", "ANTONIO RAIMONDI", 
               "ASUNCIÓN", "ATALAYA", "AYMARAES","BELLAVISTA","BOLOGNESI", "BONGARA", 
               "CAJATAMBO", "CALCA", "CANAS", "CANDARAVE", "CANTA", 
               "CARLOS FERMÍN FITZCARRALD", "CASTROVIRREYNA","CHUPACA", "CHURCAMPA", 
               "CONDESUYOS", "CONTUMAZÁ", "CORONGO", "DANIEL ALCIDES CARRIÓN",
               "DATEM DEL MARAÑÓN", "DOS DE MAYO", "EL DORADO", "GENERAL SÁNCHEZ CERRO",
               "GRAU", "HUACAYBAMBA", "HUALLAGA", "HUAMALÍES","HUANCA SANCOS",
               "HUAYTARÁ", "LA UNIÓN", "LAURICOCHA", "LORETO", "LUYA", "MANU",
               "MARAÑÓN", "MARISCAL LUZURIAGA", "MOHO", "OCROS", "OYÓN",
               "PACHITEA", "PALLASCA", "PALPA","PARURO", "PÁUCAR DEL SARA SARA",
               "PAUCARTAMBO", "PICOTA", "PUERTO INCA", "PURÚS", "PUTUMAYO",
               "QUISPICANCHI", "SAN ANTONIO DE PUTINA","SAN MARCOS", "SAN MIGUEL", 
               "SAN PABLO", "SANTA CRUZ", "SECHURA", "SUCRE", "TARATA","URUBAMBA", 
               "VÍCTOR FAJARDO", "VILCASHUAMÁN", "YAROWILCA", "YAUYOS")
UCI_1ola <- c(rep(0))
UCIN_1ola <- c(rep(0))
VENT_1ola <- c(rep(0))

df <- data_frame(PROVINCIA, UCI_1ola, UCIN_1ola, VENT_1ola)


Camas_Final <- 
      rbind(Camas_VENT1, df) %>% 
      mutate(PROVINCIA = ifelse(PROVINCIA == "CAÃ‘ETE","CANIETE",PROVINCIA)) %>% 
      arrange(PROVINCIA) %>% 
      view()





######################################################################################################


##SEGUNDA OLA

#FALLECIDOS:
FallecidosCovid2 <- df2 %>% 
  dplyr::select(-UUID) %>% 
  mutate(Numero = rep(1)) %>% 
  mutate(FECHA_FALLECIMIENTO = paste0(str_sub(FECHA_FALLECIMIENTO,1, 4),"-",str_sub(FECHA_FALLECIMIENTO, 5, 6),"-",str_sub(FECHA_FALLECIMIENTO, 7, 8))) %>% 
  mutate(FECHA_FALLECIMIENTO = as.Date(sprintf("%1$s", FECHA_FALLECIMIENTO))) %>% 
  arrange(FECHA_FALLECIMIENTO) %>% 
  dplyr::filter(FECHA_FALLECIMIENTO >= "2020-11-15" & FECHA_FALLECIMIENTO <= "2021-08-16" ) %>% 
  group_by(PROVINCIA) %>% 
  summarise(Fallecidos_2ola = sum(Numero,na.rm = TRUE))

View(FallecidosCovid2)


#CASOS COVID

CasosCovid2 <- df %>% 
  mutate(Numero = rep(1)) %>% 
  mutate(FECHA_RESULTADO = paste0(str_sub(FECHA_RESULTADO,1, 4),"-",str_sub(FECHA_RESULTADO, 5, 6),"-",str_sub(FECHA_RESULTADO, 7, 8))) %>% 
  mutate(FECHA_RESULTADO = as.Date(sprintf("%1$s", FECHA_RESULTADO))) %>% 
  arrange(FECHA_RESULTADO) %>% 
  dplyr::filter(FECHA_RESULTADO >= "2020-11-15" & FECHA_RESULTADO <= "2021-08-12" ) %>% 
  group_by(PROVINCIA) %>% 
  summarise(CasosCovid_2ola = sum(Numero,na.rm = TRUE)) 


CasosCovid2 <- CasosCovid2[-72,] %>% 
  mutate(PROVINCIA = ifelse(PROVINCIA == "CAÃ‘ETE","CANIETE",PROVINCIA)) %>% 
  arrange(PROVINCIA)

View(CasosCovid2)


##########################


#CAMAS UCI TOTALES


data_ucit2 <- read.csv("data/camas_uci/Camas_30-03-2021_0.csv", sep = "|") %>% tibble()
data_ucit3 <- read.csv("data/camas_uci/Camas_19-08-2021.csv", sep = "|") %>% tibble()

#data_ucit %>% group_by(PROVINCIA) %>% summarise() %>% view()

#1
Camas_T2 <- data_ucit2 %>% 
  dplyr::select(2:32, -FECHA_REGISTRO,-INSTITUCION, UBIGEO, CODIGO) %>% 
  mutate(FECHA_CORTE = paste0(str_sub(FECHA_CORTE,1, 4),"-",str_sub(FECHA_CORTE, 5, 6),"-",str_sub(FECHA_CORTE, 7, 8))) %>% 
  mutate(FECHA_CORTE = as.Date(sprintf("%1$s", FECHA_CORTE))) %>% 
  arrange(FECHA_CORTE) %>% 
  dplyr::filter( FECHA_CORTE >= "2020-11-15" ) %>% 
  group_by(PROVINCIA) %>% 
  summarise(
    UCI_2ola = sum(CAMAS_ZC_TOTAL, na.rm = TRUE), 
    UCIN_2ola = sum(CAMAS_ZNC_TOTAL,na.rm = TRUE),
    VENT_2ola = sum(VENTILADORES_UCI_ZC_TOTAL,na.rm = TRUE)
    )


View(Camas_T2)



#2
Camas_T22 <- data_ucit3 %>% 
  dplyr::select(1, 11:60) %>% 
  mutate(FECHACORTE = paste0(str_sub(FECHACORTE,1, 4),"-",str_sub(FECHACORTE, 5, 6),"-",str_sub(FECHACORTE, 7, 8))) %>% 
  mutate(FECHACORTE = as.Date(sprintf("%1$s", FECHACORTE))) %>% 
  arrange(FECHACORTE) %>% 
  dplyr::filter( FECHACORTE <= "2021-08-12") %>% 
  mutate(PROVINCIA = ifelse(PROVINCIA == "CAÃ‘ETE","CANIETE",PROVINCIA))
  group_by(PROVINCIA) %>% 
  summarise(
    UCI_A_2ola = sum(ZC_UCI_ADUL_CAM_TOT_OPER), 
    UCI_N_2ola = sum(ZC_UCI_NEONATAL_CAM_TOT_OPER),
    UCI_P_2ola = sum(ZC_UCI_PEDIA_CAM_TOT_OPER))

Camas_T22 <- Camas_T22[-1,]

View(Camas_T22)





#UCI - UCIN - VENTIL_M

UCI_VENT <- cbind(Camas_T, UCIN_T, VENT_M) %>%
  subset(select = !duplicated(names(.))) %>% as_tibble()

PROVINCIA <- c("AIJA","ACOMAYO", "AMBO", "ANTA", "ANTABAMBA", "ANTONIO RAIMONDI", 
               "ASUNCIÓN", "ATALAYA", "AYMARAES","BELLAVISTA","BOLOGNESI", "BONGARA", 
               "CAJATAMBO", "CALCA", "CANAS", "CANDARAVE", "CANTA", 
               "CARLOS FERMÍN FITZCARRALD", "CASTROVIRREYNA","CHUPACA", "CHURCAMPA", 
               "CONDESUYOS", "CONTUMAZÁ", "CORONGO", "DANIEL ALCIDES CARRIÓN",
               "DATEM DEL MARAÑÓN", "DOS DE MAYO", "EL DORADO", "GENERAL SÁNCHEZ CERRO",
               "GRAU", "HUACAYBAMBA", "HUALLAGA", "HUAMALÍES","HUANCA SANCOS",
               "HUAYTARÁ", "LA UNIÓN", "LAURICOCHA", "LORETO", "LUYA", "MANU",
               "MARAÑÓN", "MARISCAL LUZURIAGA", "MOHO", "OCROS", "OYÓN",
               "PACHITEA", "PALLASCA", "PALPA","PARURO", "PÁUCAR DEL SARA SARA",
               "PAUCARTAMBO", "PICOTA", "PUERTO INCA", "PURÚS", "PUTUMAYO",
               "QUISPICANCHI", "SAN ANTONIO DE PUTINA","SAN MARCOS", "SAN MIGUEL", 
               "SAN PABLO", "SANTA CRUZ", "SECHURA", "SUCRE", "TARATA","URUBAMBA", 
               "VÍCTOR FAJARDO", "VILCASHUAMÁN", "YAROWILCA", "YAUYOS")
UCI_1ola <- c(rep(0))
UCIN_1ola <- c(rep(0))
VENT_1ola <- c(rep(0))

df <- data_frame(PROVINCIA, UCI_1ola, UCIN_1ola, VENT_1ola)


Camas_Final <- 
  rbind(UCI_VENT, df) %>% 
  mutate(PROVINCIA = ifelse(PROVINCIA == "CAÃ‘ETE","CANIETE",PROVINCIA)) %>% 
  arrange(PROVINCIA) %>% 
  view()





#------------------------------


Data_1_Ola <- data.frame(FallecidosCovid,CasosCovid) %>% 
  dplyr::select(-PROVINCIA.1)

Data_2_Ola <- data.frame(FallecidosCovid2,CasosCovid2) %>% 
  dplyr::select(-PROVINCIA.1)

View(Data_1_Ola )
View(Data_2_Ola )


