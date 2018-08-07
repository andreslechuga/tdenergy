#########################################################################################################
#
#        ARCHIVO SOURCE (UTILS) PARA ENRIQUECER LOS DATOS DE LOS MEDIDORES ENERGÉTICOS
#
#########################################################################################################

############################################ LIBRERIAS ##################################################
library(dplyr)
library(tidyr)
library(ggplot2)
require(gdata)
library(chron)
library(lubridate)
library(rlist)

############################################ FUNCIONES ##################################################

###################
# Funcion 1
###################
create_calendar <- function(year,fecha_verano,fecha_invierno){
  #agregar calendario de horaio de verano/invierno. year = 2018, fecha_verano = "aaaa/mm/dd", fecha_invierno = "aaaa/mm/dd"###
  cal_summer_winter_hours <- data.frame(dates=seq(as.Date(paste0(as.character(year),"/1/1")), as.Date(paste0(as.character(year),"/12/31")), "days"))
  
  #make filters  
  cal_summer_winter_hours_ <- cal_summer_winter_hours %>%
    mutate(hora_adelantada = ifelse(cal_summer_winter_hours$dates >= as.Date(as.character(fecha_verano)) & cal_summer_winter_hours$dates < as.Date(as.character(fecha_invierno)),1,0))
  
  #return calendar
  cal_summer_winter_hours_
}
#calendario_2018 <- create_calendar(2018,"2018/03/25","2018/10/28")

###################
# Funcion 2
###################
get_folders_data <- function(root,destination){
  # recibe como input un folder nodo y lee todos los archivos que los contiene y agrega el nombre de la carpeta como nombre de variable
  # copia los archivos del root a destination y borra los archivos en root
  clean_energy_data <- data.frame()
  for(i in list.files(root)){
    if(i == "Icon\r"){next}
    for(j in list.files(paste0(root,i))){
      if(j == "Icon\r"){next}
      print(paste0(root,i,"/",j))
      enery_data <- read.csv(paste0(root,i,"/",j), header=FALSE) %>%
        mutate(idmedidor=strsplit(j,"_")[[1]][1])
      clean_energy_data <- rbind(clean_energy_data,enery_data)
      if(destination!=FALSE){
        dir.create(paste0(destination,i), showWarnings = FALSE) #creates a directory
        file.copy(paste0(root,i,"/",j),paste0(destination,i,"/",j)) # moves the data
        file.remove(paste0(root,i,"/",j)) #deletes data in old folder (root)
      }
    }
  }
  return(clean_energy_data)
}

#root <- 'Google Drive/TastyData2.0/proyectos/Tastenergy Data/Datos/'
#destination <- 'Google Drive/TastyData2.0/proyectos/Tastenergy Data/Staging/'
#data_test <- get_folders_data(root,destination)

###################
# Funcion 3
###################

separate_date <- function(enery_dataframe){
  #recibe como input un dataframe que hereda de la funcion "get_folders_data"
  enery_data <- enery_dataframe %>%
    separate(fecha, c("fecha","hora"), sep = " ") %>%
    separate(hora, c("hora","min"), sep = ":") %>%
    mutate(fecha=as.Date(fecha,format = "%m/%d/%y")) %>%
    select(fecha,hora,min,WH,VARH,idmedidor) 
  
  return(enery_data)
}


###################
# Funcion 4
###################

calculate_consume <- function(enery_dataframe){
  catalogo <- read.csv("~/Google Drive/TastyData2.0/proyectos/Tastenergy Data/GRUPO1 MEDIDORES.xlsx - Sheet1.csv") ### cambiar esto por un sheet de Google
  catalogo$SEMHuntID <- gsub("FF","",catalogo$SEMHuntID)
  join_data <- left_join(enery_dataframe, catalogo, by = c("idmedidor" = "SEMHuntID") , all = TRUE, incomparables = NA)  
  energy_data <- join_data %>%
    mutate(KWH = (WH*Multiplicador.KWH)/1000,
           KVARH =(VARH*Multiplicador.KVARH)/1000,
           KW = ((WH*Multiplicador.KWH)/1000)*12)
  
  return(energy_data)
}
