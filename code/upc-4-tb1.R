rm(list=ls(all=TRUE))
graphics.off()
cat("\014")

library(readr)
df<-read.csv('GitHub/1ACC0216-TB1-2025-1/data/hotel_bookings.csv', header=TRUE, sep=',',stringsAsFactors = FALSE,dec='.')
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

#Pre-procesamiento de datos
summary(df)
#Identificacion de datos faltantes
df$agent[df$agent %in% c("NULL", "undefined")] <- NA
df$company[df$company %in% c("NULL", "undefined")] <- NA

levels(df$company) <- c(levels(df$company), "Sin compañía")
df$company[is.na(df$company)] <- "Sin compañía"

levels(df$agent) <- c(levels(df$agent), "Sin agencia")
df$agent[is.na(df$agent)] <- "Sin agencia"

df <- df[df$adr >= 0, ]

summary(df)
table(df)
