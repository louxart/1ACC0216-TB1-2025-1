# 1ACC0216-TB1-2025-1

## Análisis de Reservas Hoteleras

### Objetivo del trabajo

Este proyecto tiene como objetivo analizar los patrones de reservas en hoteles tipo Resort y City para identificar tendencias en la duración de estancias, composición familiar de los huéspedes y necesidades de estacionamiento. A través del análisis estadístico y visualización de datos, buscamos proporcionar información valiosa para la gestión hotelera y la toma de decisiones estratégicas.

### Alumnos participantes

- Alessandro Daniel Bravo Castillo
- Sebastian Garcia Melendez
- Nicole Yessenia Vasquez Tinco

### Descripción del dataset

El conjunto de datos "hotel_bookings_limpio.csv" contiene información sobre reservas de hoteles, incluyendo dos tipos de establecimientos: Resort Hotel y City Hotel. El dataset incluye variables como:

- Tipo de hotel
- Estado de cancelación
- Tiempo de anticipación de la reserva
- Fechas de llegada
- Duración de la estancia (noches en fin de semana y entre semana)
- Número de adultos, niños y bebés
- Tipo de comidas incluidas
- País de origen
- Segmento de mercado
- Canal de distribución
- Tipo de habitación reservada y asignada
- Solicitudes de estacionamiento
- Entre otras variables relevantes

Este conjunto de datos se encuentra disponible en el archivo [`hotel_bookings_limpio.csv`](./data/hotel_bookings_limpio.csv) incluido en este repositorio. Contiene aproximadamente 155322 registros con 32 variables. Los datos originales han sido procesados y limpiados para los fines de este análisis.

Este conjunto de datos permite analizar patrones de reserva, comportamiento de los huéspedes y factores que influyen en la ocupación hotelera.

### Conclusiones

###  Cantidad de reservas que se realizan por tipo de hotel y que tipo de hotel prefiere el publico

El tipo de hotel más reservado es el City Hotel, con 76,214 reservas, superando ampliamente al Resort Hotel, que cuenta con 39,108 reservas. Esto indica una clara preferencia por los hoteles urbanos, posiblemente debido a su ubicación céntrica y accesibilidad.

### Aumento de la demanda con el tiempo

La demanda aumentó significativamente de 2015 a 2016, extendiéndose a lo largo de todo el año, aunque en 2017 hubo una caída, concentrándose solo en la primera mitad del año. A pesar de la reducción en 2017, la demanda total fue aún mayor que en 2015, lo que sugiere una tendencia general de crecimiento, aunque con cierta inestabilidad.

### Temporadas con reservas altas, medias y bajas

Las reservas se concentran en los meses de abril, mayo, julio y agosto (temporada alta), lo que coincide con vacaciones y clima favorable. Marzo, junio, septiembre y octubre se consideran temporada media, mientras que enero, febrero, noviembre y diciembre corresponden a la temporada baja, con menor actividad de reservas.

#### Duración promedio de estancias por tipo de hotel

El análisis de la duración promedio de estancias revela diferencias significativas entre los tipos de hotel. Los hoteles tipo "Resort" muestran una duración promedio de estancia más larga que los hoteles tipo "City", lo que sugiere que los huéspedes tienden a permanecer más tiempo en resorts, probablemente por su naturaleza vacacional. Esta tendencia se confirma en el boxplot, donde además se observa una mayor variabilidad en la duración de estancias en resorts, con más valores atípicos hacia duraciones largas, mientras que los hoteles urbanos presentan estancias más cortas y homogéneas, posiblemente relacionadas con viajes de negocios o visitas breves a la ciudad.

#### Reservas que incluyen niños y/o bebés

La visualización de reservas con niños y/o bebés muestra que solo una pequeña proporción del total de reservas incluye menores, lo que indica que la mayoría de los huéspedes viajan sin niños. Al analizar por tipo de hotel, se observa una diferencia notable: los hoteles tipo "Resort" tienen un porcentaje significativamente mayor de reservas con niños en comparación con los hoteles "City", lo cual es coherente con el perfil familiar de los resorts que suelen ofrecer más instalaciones y actividades orientadas a familias. Esta información es valiosa para la gestión hotelera, ya que permite adaptar servicios y estrategias de marketing según el público objetivo predominante en cada tipo de establecimiento.

#### Importancia de contar con espacios de estacionamiento

El análisis sobre la importancia de los espacios de estacionamiento revela que la gran mayoría de las reservas (más del 90%) no solicitan plazas de aparcamiento, lo que podría indicar que la mayoría de los huéspedes no viajan en vehículo propio o no consideran el estacionamiento como una necesidad prioritaria. Al examinar por tipo de hotel, se observa que los hoteles "City" tienen una proporción ligeramente menor de solicitudes de estacionamiento que los "Resort", posiblemente debido a la mejor conectividad mediante transporte público en entornos urbanos. Adicionalmente, el análisis de la relación entre solicitudes de estacionamiento y cancelaciones sugiere que las reservas con solicitud de estacionamiento tienen una tasa de cancelación ligeramente menor, lo que podría indicar un mayor compromiso o planificación por parte de estos huéspedes.

### Meses del año en el cual se producen más cancelaciones de reservas

El mes con mayor cantidad de cancelaciones es enero, con un total de 5,947 reservas canceladas. Esto sugiere que, a pesar de ser parte de la temporada baja, enero presenta una alta tasa de cancelaciones, posiblemente debido a cambios de planes tras las festividades de fin de año o a factores económicos y climáticos que afectan la decisión de viajar.

### Pais con mayor numero de reservas

El país con mayor número de reservas entre 2015 y 2017 es Portugal, con 46,215 reservas registradas. Este resultado es coherente con el hecho de que los datos fueron recolectados en dicho país, lo que sugiere una alta proporción de clientes nacionales o una fuerte demanda interna en el mercado hotelero portugués.

### Licencia

Este proyecto está licenciado bajo la licencia MIT - vea el archivo [LICENSE.md](LICENSE.md) para más detalles.

Los datos utilizados en este análisis son de acceso público y han sido obtenidos de hotel_bookings.csv, siendo procesados y limpiados para los fines académicos de este proyecto.
