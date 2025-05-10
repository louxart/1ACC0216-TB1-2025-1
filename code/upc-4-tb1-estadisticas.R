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

#Demando juntando todos los años
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
library(ggplot2)
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

