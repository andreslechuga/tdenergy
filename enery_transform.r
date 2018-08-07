#########################################################################################################
#
#                ARCHIVO PARA PROCESAR Y ACTUALIZAR LA INFORMACIÓN EN LAS CARPETAS
#                       DONDE SE RECIBE LA INFORMACION DE LOS MEDIDORES
#
#########################################################################################################

#####################################   PROCESS  #######################################################

### main paths
root <- 'Google Drive/TastyData2.0/proyectos/Tastenergy Data/Datos/'
destination <- 'Google Drive/TastyData2.0/proyectos/Tastenergy Data/Staging/'
root_source <- 'Google Drive/TastyData2.0/proyectos/Tastenergy Data/desarrollo/utils_enery_transform.r'

### source
source(root_source)

### get data
energy_dataset <- get_folders_data(root,destination = FALSE)
colnames(energy_dataset) <- c("fecha","WH","VARH","V4","V5","V6","V7","V8","V9","idmedidor")

### separate dates
energy_dataset <- separate_date(energy_dataset)

### arrange calendar
calendario_2018 <- create_calendar(2018,"2018/03/25","2018/10/28")
energy_dataset <- left_join(energy_dataset, calendario_2018, by = c("fecha" = "dates") , all = TRUE, incomparables = NA) %>%
  mutate(hora = (as.numeric(hora) + hora_adelantada)%%24,
         fecha = if_else(hora == 0 & hora_adelantada == 1, fecha+1, fecha))

### calculate energy consume
energy_dataset <- calculate_consume(energy_dataset)

### store data in DB
write.csv(energy_dataset,"Google Drive/TastyData2.0/proyectos/Tastenergy Data/desarrollo/test_data.csv",row.names = F)



