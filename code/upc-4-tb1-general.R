rm(list=ls(all=TRUE))
graphics.off()
cat("\014")

library(readr)
df.original <- read.csv("data/hotel_bookings.csv",na.strings = c("NA", "NULL", "Undefined"))
summary(df.original)

df.original$hotel<-as.factor(df.original$hotel)
df.original$is_canceled<-as.factor(df.original$is_canceled)
df.original$meal<-as.factor(df.original$meal)
df.original$country<-as.factor(df.original$country)
df.original$market_segment<-as.factor(df.original$market_segment)
df.original$distribution_channel<-as.factor(df.original$distribution_channel)
df.original$is_repeated_guest<-as.factor(df.original$is_repeated_guest)
df.original$reserved_room_type<-as.factor(df.original$reserved_room_type)
df.original$assigned_room_type<-as.factor(df.original$assigned_room_type)
df.original$deposit_type<-as.factor(df.original$deposit_type)
df.original$customer_type<-as.factor(df.original$customer_type)
df.original$reservation_status<-as.factor(df.original$reservation_status)
df.original$reservation_status_date<-as.Date(df.original$reservation_status_date)
df.original$agent<-as.factor(df.original$agent)
df.original$company<-as.factor(df.original$company)
df.original$is_canceled<-as.factor(df.original$is_canceled)
df.original$arrival_date_month<-as.factor(df.original$arrival_date_month)
df.original$arrival_date_day_of_month<-as.factor(df.original$arrival_date_day_of_month)
df.original$arrival_date_year<-as.factor(df.original$arrival_date_year)

summary(df.original)
#Pre-procesamiento de datos
library(ggplot2)
library(cowplot)
library(patchwork)
library(VIM)
library(mlr)

summary(df.original$arrival_date_month)
#children meal distribution_channel adr agent company
#numerico: lead_time arrival_date_week_nummber stays_in_weekend_nights adults children babies previous_cancellations 
#days_in_waiting_list adr required_car_parking_spaces total_of_sepecial_requests reservation_status_date
str(df.original)
#Vemos la proporcion de NA's presentes en el dataset
aggr(df.original,numbers=T,sortVar=T) 
#En el sector numerico, ponemos como 0's los NA's presentes en esa categoria
df.original$children[is.na(df.original$children)] <- 0
summary(df.original)
#Verificamos cuales son los NA's restantes
aggr(df.original,numbers=T,sortVar=T) 
#Para los factores: En el caso de company, los NA's se interpretan como que no fueron 
#con una compañia, igual en agente, por lo tanto, eliminarlo sería erroneo
levels(df.original$company) <- c(levels(df.original$company), "No company")
df.original$company[is.na(df.original$company)] <- "No company"
levels(df.original$agent) <- c(levels(df.original$agent), "No agency")
df.original$agent[is.na(df.original$agent)] <- "No agency"
#Para meal: No meal package tambien esta dentro de SC, ya que no hubo registro
df.original$meal[is.na(df.original$meal)] <- "SC"
#Para country: Como no hubo pais de origen, entonces se supondria que es el mismo pais (a debatir)
paises_na<- sum(is.na(df.original$country))
paises_na
df.original <- df.original[!is.na(df.original$country), ]
#Para distribution_channel y market_segment: Debido a que son pocas las variables que presentan NA's se usará moda
df <- impute(df.original,classes = list(factor=imputeMode(),
                                      integer = imputeMode(),
                                      numeric = imputeMedian()),
                    dummy.classes = c("integer","factor"), dummy.type = "numeric")
df=df$data[,1:min(dim(df.original))] 
#Verificamos la presencia de NA's
summary(df)
aggr(df,numbers=T,sortVar=T) 

#Valores numericos: Vemos el diagrama de cajas de adr

p1<-ggplot(df, aes(x =adr)) +
  geom_boxplot(fill="steelblue") +
  labs(title = "Boxplot",
  )+
  theme_classic()
p1
summary(df$adr)
lim_inferior <- quantile(df$adr, 0.01) 
Q1<-quantile(df$adr,0.25)
Q3<-quantile(df$adr,0.75)
IQR<-Q3-Q1
lim_superior<-Q3+1.5*IQR
atipicos <- df$adr[df$adr < lim_inferior | df$adr > lim_superior]
atipicos #Hay 3793 valores atipicos
df$adr <- ifelse(df$adr < lim_inferior, lim_inferior,  
                        ifelse(df$adr > lim_superior, lim_superior,df$adr))

atipicos <- df$adr[df$adr < lim_inferior | df$adr > lim_superior]
atipicos #Hay 0 valores atipicos
boxplot(df$adr,main="ADR sin outliners",col=5)
summary(df$adr)

df<- df[df$adults>=1 & df$adults<=5,]
df<-df[df$children<=4 & df$babies<=4, ]
df<-df[df$lead_time<=365, ]
df <- df[df$stays_in_week_nights + df$stays_in_weekend_nights <= 30, ]
df <- df[df$required_car_parking_spaces <= 2, ]

summary(df)

write.csv(df,'hotel_bookings_limpio.csv', row.names = TRUE)


library(ggplot2)
library(dplyr)

# ¿Cuál es la duración promedio de las estancias por tipo de hotel?
# Calculamos la duración total de cada estancia
df$total_nights <- df$stays_in_weekend_nights + df$stays_in_week_nights

# Calculamos la duración promedio por tipo de hotel
duracion_promedio <- df %>%
  group_by(hotel) %>%
  summarise(duracion_promedio = mean(total_nights, na.rm = TRUE))

head(duracion_promedio)
# La duracion promedio para City Hotel 3.00 noches
# La duracion promedio para Resort Hotel 4.32 noches

# Visualizamos con un gráfico de barras
ggplot(duracion_promedio, aes(x = hotel, y = duracion_promedio, fill = hotel)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(duracion_promedio, 2)), vjust = -0.5, size = 4) +
  labs(title = "Duración promedio de estancias por tipo de hotel",
       x = "Tipo de hotel",
       y = "Duración promedio (noches)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none")

# ¿Cuántas reservas incluyen niños y/o bebés?	Sebas
df$con_ninos_o_bebes <- (df$children > 0 | df$babies > 0)

# Contamos las reservas con y sin niños/bebés
reservas_ninos <- df %>%
  group_by(con_ninos_o_bebes) %>%
  summarise(cantidad = n()) %>%
  mutate(porcentaje = cantidad / sum(cantidad) * 100)

# Revisamos las cantidades:
head(reservas_ninos)
#   con_ninos_o_bebes cantidad porcentaje etiqueta              
# 1 FALSE             106238   92.1       Sin niños/bebés
# 2 TRUE              9084     7.88       Con niños/bebés

# Etiquetas para el gráfico
reservas_ninos$etiqueta <- ifelse(reservas_ninos$con_ninos_o_bebes,
                                  "Con niños/bebés", 
                                  "Sin niños/bebés")

# Visualizamos con un gráfico de pastel
ggplot(reservas_ninos, aes(x = "", y = cantidad, fill = etiqueta)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%\n(", cantidad, " reservas)")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Proporción de reservas con niños y/o bebés",
       fill = "Tipo de reserva") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())

# También podemos ver la distribución por tipo de hotel
reservas_ninos_hotel <- df %>%
  group_by(hotel, con_ninos_o_bebes) %>%
  summarise(cantidad = n()) %>%
  mutate(porcentaje = cantidad / sum(cantidad) * 100)

# Etiquetas para el gráfico
reservas_ninos_hotel$etiqueta <- ifelse(reservas_ninos_hotel$con_ninos_o_bebes, 
                                        "Con niños/bebés", 
                                        "Sin niños/bebés")
head(reservas_ninos_hotel)
# hotel        con_ninos_o_bebes cantidad porcentaje etiqueta       
# 1 City Hotel   FALSE                71037      93.2  Sin niños/bebés
# 2 City Hotel   TRUE                  5177       6.79 Con niños/bebés
# 3 Resort Hotel FALSE                35201      90.0  Sin niños/bebés
# 4 Resort Hotel TRUE                  3907       9.99 Con niños/bebés

# Visualizamos con un gráfico de barras apiladas
ggplot(reservas_ninos_hotel, aes(x = hotel, y = cantidad, fill = etiqueta)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(porcentaje, 1), "% (", cantidad, ")")), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Reservas con niños y/o bebés por tipo de hotel",
       x = "Tipo de hotel",
       y = "Número de reservas",
       fill = "Tipo de reserva") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

head(reservas_ninos_hotel)

# ¿Es importante contar con espacios de estacionamiento?	Sebas

# Analizamos la distribución de solicitudes de estacionamiento
estacionamiento <- df %>%
  group_by(required_car_parking_spaces) %>%
  summarise(cantidad = n()) %>%
  mutate(porcentaje = cantidad / sum(cantidad) * 100)

# Visualizamos con un gráfico de barras
ggplot(estacionamiento, aes(x = as.factor(required_car_parking_spaces), y = cantidad, fill = as.factor(required_car_parking_spaces))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(porcentaje, 2), "%")), vjust = -0.5) +
  labs(title = "Distribución de solicitudes de espacios de estacionamiento",
       x = "Número de espacios solicitados",
       y = "Número de reservas") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues") +
  theme(legend.position = "none")

# Analizamos por tipo de hotel
estacionamiento_hotel <- df %>%
  group_by(hotel, required_car_parking_spaces) %>%
  summarise(cantidad = n()) %>%
  mutate(porcentaje = cantidad / sum(cantidad) * 100)

# Visualizamos con un gráfico de barras agrupadas
ggplot(estacionamiento_hotel, aes(x = as.factor(required_car_parking_spaces), y = cantidad, fill = hotel)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Solicitudes de estacionamiento por tipo de hotel",
       x = "Número de espacios solicitados",
       y = "Número de reservas",
       fill = "Tipo de hotel") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

View(estacionamiento_hotel)

# Análisis adicional: Influye el estacionamiento en la cancelación?
estacionamiento_cancelacion <- df %>%
  group_by(is_canceled, required_car_parking_spaces > 0) %>%
  summarise(cantidad = n()) %>%
  mutate(porcentaje = cantidad / sum(cantidad) * 100)

# Renombramos las columnas para mejor interpretación
names(estacionamiento_cancelacion)[2] <- "solicita_estacionamiento"
estacionamiento_cancelacion$estado <- ifelse(estacionamiento_cancelacion$is_canceled == 1, 
                                             "Cancelada", 
                                             "No cancelada")
estacionamiento_cancelacion$estacionamiento <- ifelse(estacionamiento_cancelacion$solicita_estacionamiento, 
                                                      "Con estacionamiento", 
                                                      "Sin estacionamiento")
# Visualizamos la relación entre estacionamiento y cancelación
ggplot(estacionamiento_cancelacion, aes(x = estacionamiento, y = porcentaje, fill = estado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(porcentaje, 1), "% (", cantidad, ")")), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Relación entre solicitud de estacionamiento y cancelación",
       x = "Solicitud de estacionamiento",
       y = "Porcentaje de reservas",
       fill = "Estado de la reserva") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")


#¿Cuantas reservas se realizaron por tipo de hotel? ¿Que tipo de hotel prefiere la gente?
library(ggplot2)
library(ggplot2)
library(dplyr)
library(ggrepel)
# Tabla de frecuencias
tabla_hoteles <- as.data.frame(table(df$hotel))
colnames(tabla_hoteles) <- c("hotel", "cantidad")

# Calcular porcentaje
tabla_hoteles$porcentaje <- round(100 * tabla_hoteles$cantidad / sum(tabla_hoteles$cantidad), 1)
tabla_hoteles$etiqueta <- paste0( tabla_hoteles$porcentaje, "% (", tabla_hoteles$cantidad, ")")

# Gráfico de pastel
ggplot(tabla_hoteles, aes(x = "", y = cantidad, fill = hotel)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_label_repel(aes(label = etiqueta),
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE,
                   box.padding = 0.5,
                   segment.color = "grey50") +
  labs(title = "Preferencia y cantidad por tipo de hotel",
       fill = "Tipo de hotel") +
  theme_void()


#¿Esta aumentando la demanda?
#2015
año_2015<-as.data.frame(table(df$arrival_date_month[df$arrival_date_year==2015]))
ggplot(año_2015,
       aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = Freq), color = "black", size = 5,vjust=1.5)+
  labs(title = "Reservas por mes en 2015",
       x = "Mes", y = "Cantidad de reservas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limits = month.name[month.name %in% unique(df$arrival_date_month[df$arrival_date_year == 2015])])
#En el 2015 se logra visualizar que la demanda era demasiado baja solo teniendo reservas en la segunda parte del año

#2016
año_2016<-as.data.frame(table(df$arrival_date_month[df$arrival_date_year==2016]))
ggplot(año_2016,
       aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_text(aes(label = Freq), color = "black", size = 5,vjust=1.5)+
  labs(title = "Reservas por mes en 2016",
       x = "Mes", y = "Cantidad de reservas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limits = month.name[month.name %in% unique(df$arrival_date_month[df$arrival_date_year == 2016])])
#En el 2016 se logra visualizar una gran demanda llegando a cubrir todo el año teniendo grande numeros en octubre

#2017
año_2017<-as.data.frame(table(df$arrival_date_month[df$arrival_date_year==2017]))
ggplot(año_2017,
       aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "green") +
  geom_text(aes(label = Freq), color = "black", size = 5,vjust=1.5)+
  labs(title = "Reservas por mes en 2017",
       x = "Mes", y = "Cantidad de reservas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limits = month.name[month.name %in% unique(df$arrival_date_month[df$arrival_date_year == 2017])])
#En 2017 se visualiza que solo se tuvo demanda en la primera parte del año

#Demanda juntando todos los años
tabla_años_demanda<-as.data.frame(table(df$arrival_date_year))
ggplot(tabla_años_demanda,
       aes(x = factor(Var1), y = Freq)) +
  geom_bar(stat = "identity", fill = "darkolivegreen4") +
  geom_text(aes(label = Freq), color = "black", size = 5,vjust=1.5)+
  labs(title = "Reservas totales por año",
       x = "Año", y = "Cantidad de reservas") +
  theme_minimal()
#Viendo el grafico de 2015, 2016 y 2017 se entiende la demanda si aumento pero que no se mantuvo 
#debido a que de 2016 a 2017 la demanda bajo pero aun asi siendo mayor a 2015, logrando un buen numero de reservas
#con menores reservas por mes

#¿Cuáles son las temporadas de reservas (alta, media, baja)?
tabla_mes <- as.data.frame(table(df$arrival_date_month))
colnames(tabla_mes) <- c("Mes", "Reservas")

tabla_mes$Mes <- factor(tabla_mes$Mes, levels = month.name)

# Calcular percentiles
p33 <- quantile(tabla_mes$Reservas, 0.33)
p66 <- quantile(tabla_mes$Reservas, 0.66)
# Clasificar en temporadas
tabla_mes <- tabla_mes %>%
  mutate(Temporada = case_when(
    Reservas <= p33 ~ "Baja",
    Reservas <= p66 ~ "Media",
    TRUE ~ "Alta"
  ))

# Gráfico por temporada
ggplot(tabla_mes, aes(x = Mes, y = Reservas, fill = Temporada)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Reservas), vjust = 1.5, color = "white") +
  scale_fill_manual(values = c("Baja" = "skyblue", "Media" = "orange", "Alta" = "darkred")) +
  labs(title = "Temporadas de reservas por mes (promedio anual)",
       x = "Mes", y = "Total de reservas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Se muesta en el grafico las temporadas categorisadas con alta, media y alta
#Alta: Abril, mayo, julio y agosto
#Media: Marzo, Junio, Setiembre y Octubre
#Baja: Enero, Febrero, Noviembre y Diciembre
#¿En que meses del año se producen más cancelaciones de reserva?
df$reservation_status_date <- as.Date(df$reservation_status_date)
df$mes <- format(df$reservation_status_date, "%B")
df$mes <- as.factor(df$mes)
canceladas <- df[df$is_canceled == 1, ]
ggplot(canceladas, aes(x = mes)) +
  geom_bar(fill = "tomato") +
  geom_text(stat = "count", aes(label = ..count..), vjust = 3) +
  labs(title = "Cancelaciones de reserva por mes",
       x = "Mes",
       y = "Número de cancelaciones") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Con 5947 cancelaciones, Enero es el mes con mayor numero de cancelaciones de reservas
#¿Cual es el pais con mayor numero de reservas?
top_paises <- as.data.frame(table(df$country))
top_paises <- top_paises[order(-top_paises$Freq), ]
top10 <- head(top_paises, 10)
ggplot(top10, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Freq), vjust = -0.5, size = 4) +
  labs(title = "Top 10 países con más reservas",
       x = "País",
       y = "Número de reservas") +
  theme_minimal()
#Con 46215 reservas realizadas, el país con mayor numero de reservas es PRT (Portugal)

