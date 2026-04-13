library(readr)
library(ggplot2)

hotel_data <- read_csv("hotel_bookings - hotel_bookings.csv")

View(hotel_bookings_hotel_bookings)

# 1. GEOMETRÍA: DISTRIBUCIÓN (Histograma)
# Objetivo: Ver el volumen de reservas según la antelación
ggplot(hotel_data, aes(x = lead_time)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white") +
  labs(title = "Distribución de la Antelación de Reserva",
       subtitle = "La mayoría de las reservas ocurren con poca antelación",
       x = "Días de antelación (Lead Time)", y = "Frecuencia") +
  theme_minimal()

# 2. GEOMETRÍA: RELACIÓN (Boxplot)
# Objetivo: Comparar precios (ADR) entre los dos tipos de hoteles
# Usamos Boxplot para detectar la mediana y los valores atípicos (outliers)
ggplot(hotel_data, aes(x = hotel, y = adr, fill = hotel)) +
  geom_boxplot() +
  ylim(0, 500) + 
  labs(title = "Relación: Precio Diario por Tipo de Hotel",
       x = "Hotel", y = "Tarifa Diaria Promedio (ADR)") +
  theme_light()

# 3. GEOMETRÍA: COMPARACIÓN (Barras Agrupadas / Lado a Lado)
# Objetivo: Ver el volumen real de reservas vs cancelaciones por segmento
ggplot(hotel_data, aes(x = market_segment, fill = as.factor(is_canceled))) +
  geom_bar(position = "dodge") + 
  coord_flip() + 
  labs(title = "Volumen de Reservas y Cancelaciones por Segmento",
       subtitle = "Comparación directa entre reservas completadas y canceladas",
       x = "Segmento de Mercado", 
       y = "Número Total de Reservas", 
       fill = "¿Estado?") +
  scale_fill_manual(values = c("0" = "#2E86C1", "1" = "#E67E22"), 
                    labels = c("Completada", "Cancelada")) +
  theme_minimal()


# Primero creamos una variable para el total de personas
hotel_data$total_guests <- hotel_data$adults + hotel_data$children

# Gráfico de burbujas
ggplot(hotel_data %>% filter(total_guests > 0 & adr < 600), 
       aes(x = lead_time, y = adr, size = total_guests, color = as.factor(is_canceled))) +
  geom_point(alpha = 0.4) +
  scale_size(range = c(1, 10)) + 
  labs(title = "Gráfica de Burbujas: Precio, Antelación y Tamaño del Grupo",
       x = "Antelación", y = "Precio", 
       size = "Huéspedes", color = "¿Cancelado?") +
  theme_minimal()


# Dispersión con color por categoría
ggplot(hotel_data, aes(x = lead_time, y = adr, color = hotel)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~hotel) +
  ylim(0, 600) +
  labs(title = "Dispersión de Precio por Tipo de Hotel",
       x = "Días de Antelación", y = "Tarifa Diaria") +
  theme_light()


# 6. GEOMETRÍA: EVOLUCIÓN (Líneas)
# Objetivo: Ver la estacionalidad de las llegadas
# Primero ordenamos los meses cronológicamente
hotel_data$arrival_date_month <- factor(hotel_data$arrival_date_month, 
                                        levels = c("January", "February", "March", "April", "May", "June", 
                                                   "July", "August", "September", "October", "November", "December"))

ggplot(hotel_data, aes(x = arrival_date_month, group = 1)) +
  geom_line(stat = "count", color = "firebrick", size = 1) +
  geom_point(stat = "count", color = "firebrick") +
  labs(title = "Evolución: Total de Llegadas por Mes",
       x = "Mes", y = "Número de Reservas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Gráfico de dispersión simple
ggplot(hotel_data, aes(x = lead_time, y = adr)) +
  geom_point(alpha = 0.3, color = "darkblue") +
  geom_smooth(method = "lm", color = "red") +   
  ylim(0, 600) +
  labs(title = "Relación: Antelación vs Precio (ADR)",
       x = "Días de Antelación", y = "Tarifa Diaria") +
  theme_minimal()
