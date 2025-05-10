rm(list=ls(all=TRUE))
graphics.off()
cat("\014")

library(readr)

df <- read_csv("data/hotel_bookings_limpio.csv")

summary(df)

df$hotel<-as.factor(df$hotel)
df$is_canceled<-as.factor(df$is_canceled)
df$meal<-as.factor(df$meal)
df$country<-as.factor(df$country)
df$market_segment<-as.factor(df$market_segment)
df$distribution_channel<-as.factor(df$distribution_channel)
df$is_repeated_guest<-as.factor(df$is_repeated_guest)
df$reserved_room_type<-as.factor(df$reserved_room_type)
df$assigned_room_type<-as.factor(df$assigned_room_type)
df$deposit_type<-as.factor(df$deposit_type)
df$customer_type<-as.factor(df$customer_type)
df$reservation_status<-as.factor(df$reservation_status)
df$reservation_status_date<-as.Date(df$reservation_status_date)
df$agent<-as.factor(df$agent)
df$company<-as.factor(df$company)
df$is_canceled<-as.factor(df$is_canceled)
df$arrival_date_month<-as.factor(df$arrival_date_month)
df$arrival_date_day_of_month<-as.factor(df$arrival_date_day_of_month)
df$arrival_date_year<-as.factor(df$arrival_date_year)
summary(df)
library(ggplot2)

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
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "none")

# ¿Cuántas reservas incluyen niños y/o bebés?	Sebas
df$con_ninos_o_bebes <- (df$children > 0 | df$babies > 0)

# Contamos las reservas con y sin niños/bebés
reservas_ninos <- df %>%
  group_by(con_ninos_o_bebes) %>%
  summarise(cantidad = n()) %>%
  mutate(porcentaje = cantidad / sum(cantidad) * 100)

# Etiquetas para el gráfico
reservas_ninos$etiqueta <- ifelse(reservas_ninos$con_ninos_o_bebes,
                                  "Con niños/bebés", 
                                  "Sin niños/bebés")

# Visualizamos con un gráfico de pastel
ggplot(reservas_ninos, aes(x = "", y = cantidad, fill = etiqueta)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")), 
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


#¿En que meses del año se producen más cancelaciones de reserva?
df$reservation_status_date <- as.Date(df$reservation_status_date)
df$mes <- format(df$reservation_status_date, "%B")
df$mes <- as.factor(df$mes)
canceladas <- df[df$is_canceled == 1, ]
ggplot(canceladas, aes(x = mes)) +
  geom_bar(fill = "tomato") +
  labs(title = "Cancelaciones de reserva por mes",
       x = "Mes",
       y = "Número de cancelaciones") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
table_cancelaciones <- sort(table(canceladas$mes), decreasing = TRUE)
table_cancelaciones
#Con 5947 cancelaciones, Enero es el mes con mayor numero de cancelaciones de reservas
#¿Cual es el pais con mayor numero de reservas?
top_paises <- as.data.frame(table(df$country))
top_paises <- top_countries[order(-top_countries$Freq), ]
top10 <- head(top_paises, 10)
ggplot(top10, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 países con más reservas",
       x = "País",
       y = "Número de reservas") +
  theme_minimal()
table_paises <- sort(table(df$country), decreasing = TRUE)
table_paises
#Con 46215 reservas realizadas, el país con mayor numero de reservas es PRT (Portugal)

