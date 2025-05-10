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

