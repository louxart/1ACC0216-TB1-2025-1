rm(list=ls(all=TRUE))
graphics.off()
cat("\014")

library(readr)
df <- read.csv("data/hotel_bookings.csv",na.strings = c("NA", "NULL", "Undefined"))
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

#Pre-procesamiento de datos
library(ggplot2)
library(cowplot)
library(patchwork)
library(VIM)
library(mlr)

summary(df$arrival_date_month)
#children meal distribution_channel adr agent company
#numerico: lead_time arrival_date_week_nummber stays_in_weekend_nights adults children babies previous_cancellations 
#days_in_waiting_list adr required_car_parking_spaces total_of_sepecial_requests reservation_status_date
str(df)
#Vemos la proporcion de NA's presentes en el dataset
aggr(df,numbers=T,sortVar=T) 
#En el sector numerico, ponemos como 0's los NA's presentes en esa categoria
df$children[is.na(df$children)] <- 0
summary(df)
#Verificamos cuales son los NA's restantes
aggr(df,numbers=T,sortVar=T) 
#Para los factores: En el caso de company, los NA's se interpretan como que no fueron 
#con una compañia, igual en agente, por lo tanto, eliminarlo sería erroneo
levels(df$company) <- c(levels(df$company), "No usó compañía")
df$company[is.na(df$company)] <- "No usó compañía"
levels(df$agent) <- c(levels(df$agent), "No usó agencia")
df$agent[is.na(df$agent)] <- "No usó agencia"
#Para meal: No meal package
levels(df$meal) <- c(levels(df$meal),"No meal package")
df$meal[is.na(df$meal)] <- "No meal package"
#Para country: Como no hubo pais de origen, entonces se supondria que es el mismo pais (a debatir)
levels(df$country)<-c(levels(df$country),"OH") #Origin Here
df$country[is.na(df$country)] <- "OH"
#Para distribution_channel y market_segment: Debido a que son pocas las variables que presentan NA's se usará moda
df.limpia <- impute(df,classes = list(factor=imputeMode(),
                                      integer = imputeMode(),
                                      numeric = imputeMedian()),
                    dummy.classes = c("integer","factor"), dummy.type = "numeric")
df.limpia=df.limpia$data[,1:min(dim(df))] 
#Verificamos la presencia de NA's
summary(df.limpia)
aggr(df.limpia,numbers=T,sortVar=T) 

#Valores numericos: Vemos el diagrama de cajas de adr

p1<-ggplot(df.limpia, aes(x =adr)) +
  geom_boxplot(fill="steelblue") +
  labs(title = "Boxplot",
  )+
  theme_classic()
p1

Q1 <- quantile(df.limpia$adr, 0.25)
Q3 <- quantile(df.limpia$adr, 0.75)
IQR <- Q3 - Q1
lim_inferior <- Q1 - 1.5 * IQR
lim_superior <- Q3 + 1.5 * IQR

atipicos <- df.limpia$adr[df.limpia$adr < lim_inferior | df.limpia$adr > lim_superior]
atipicos #Hay 3793 valores atipicos
df.limpia_median <- median(df.limpia$adr)
df.limpia$adr<- ifelse(df.limpia$adr < lim_inferior | 
                                       df.limpia$adr>lim_superior,
                                     df.limpia_median,df.limpia$adr)

par(mfrow=c(1,2))
boxplot(df.limpia$adr,main="ADR sin tanto outliners",col=5)
write.csv(df.limpia,'hotel_bookings_limpio.csv', row.names = TRUE)
