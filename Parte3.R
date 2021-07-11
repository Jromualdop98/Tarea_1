#Se tiene el conjuntos de datos de precipitación diaria (período 1980 - 2013) 
#de ciertas estaciones meteorológicas (raingaugeDataset.csv), donde cada una de 
#estas están asociadas a un código único (p.e. qc00000208). Asimismo, se tiene 
#una lista con los nombres, códigos, coordenadas y elevación de cada una de las 
#estaciones (listRaingauge.csv).

cod_lambayeque <- 
  read.csv('Data/listRaingauge.csv') %>% 
  as.tibble() %>% 
  dplyr::filter(NOM_EST == 'LAMBAYEQUE') %>% 
  select(CODIGO) %>% 
  collect %>% .[[1]]

data_lambayeque <- 
  read.csv('Data/raingaugeDataset.csv') %>% 
  as.tibble() %>% 
  select(date, cod_lambayeque) %>% 
  mutate(date = as.Date(date, format = '%d/%m/%Y')) %>% 
  rename(pp = cod_lambayeque) %>% 
  arrange(date)

View(data_lambayeque)

#a. Determine la cantidad de missing values de la serie de tiempo a paso diario.
missValue_diario <- sum(is.na(data_lambayeque$pp))

#b. Calcule la serie de tiempo de precipitación acumulada mensual (si el # de días 
#con missing values, en un mes, supera el 10%, la precipitación acumulada mensual 
#será considerado como un NA).
lambayeque_mensual <- 
  data_lambayeque %>% 
  group_by(date = str_sub(date, 1,7)) %>% 
  mutate(
    missVal = sum(is.na(pp) * 100 / n()) #10% de datos faltantes, no se trabaja
  ) %>% 
  summarize(
    pp = sum(pp, na.rm = T),
    missVal = unique(missVal)
  ) %>% 
  mutate(
    pp = ifelse(missVal >= 10, NA, pp),
    date = as.Date(sprintf('%1$s-01', date)),
    month = str_sub(date, 6,7))

#c.Determine la cantidad de missing values de la serie de tiempo a paso mensual.
missValue_mensual <- sum(is.na(lambayeque_mensual$pp))

#d. Cree una función que calcule, a partir de los datos de preicpitación mensual,
#la climatología (Ene-Dic) para el período 1980-2010.

pp_mean <- function(data, date_init, date_end ){
  data <- lambayeque_mensual %>% 
    dplyr::filter(date >= date_init & date <= date_end) %>% #date: columna de 'data' donde se encuentran las fechas
    group_by(month) %>% #meses de la columna date
    summarise(ppmean= mean(pp, na.rm = T)) %>%  #pp = columna de precipitacion
    print()
}
pp_mean(lambayeque_mensual, '1980-01-01','2010-01-01')

#e. Plotear (boxplot) la variabilidad de los valores mensuales (Ene-Dic) para el 
#período 1980-2013.

plot <- ggplot(lambayeque_mensual, aes(month, pp, color = month)) + #en el eje x van los meses
  ggtitle("Variabilidad de la precipitación mensual (Ene-Dic) 1980-2013")+
  geom_boxplot()+ #definimos la geometria de boxplot
  theme_bw()+ #se le agrega un tema
  scale_x_discrete( #le damos la etiqueta a los meses 
    labels = month.abb #.abb primeras tres letras ene, feb, mar, etc
  )
