# Objetivo Específico 2: Distribución de Precio FOB en Puerto Rodman

## Variables
- **US$ FOB**: Cuantitativa continua - Razón
- **Puerto**: Cualitativa - Nominal

## Código R

```r
# Librerías
library(readxl)
library(dplyr)
library(knitr)
library(ggplot2)

# Leer datos
dataset <- read_excel("Documentos/Estadistica/TF/data/dataset.xlsx")

# Filtrar Puerto Rodman y América del Norte
paises_norte <- c("Estados Unidos", "USA", "United States", "Canadá", "Canada", "México", "Mexico")
datos_filtrados <- dataset %>%
  filter(Puerto == "RODMAN" | grepl("RODMAN", Puerto, ignore.case = TRUE)) %>%
  filter(País %in% paises_norte | grepl("Estados Unidos|USA|United States|Canadá|Canada|México|Mexico", País, ignore.case = TRUE))

# TABLA DE FRECUENCIAS CON INTERVALOS
precio <- datos_filtrados$`US$ FOB`
n_clases <- 8  # Según la imagen

# Crear intervalos
rango <- max(precio) - min(precio)
amplitud <- rango / n_clases
limites <- seq(min(precio), max(precio), by = amplitud)

# Crear intervalos
datos_filtrados$intervalo <- cut(precio,
                                  breaks = limites,
                                  include.lowest = TRUE,
                                  right = TRUE)

# Tabla de frecuencias
tabla_freq <- datos_filtrados %>%
  group_by(intervalo) %>%
  summarise(
    Li = min(precio),
    Ls = max(precio),
    fi = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    hi = fi / sum(fi),
    `hi%` = round(hi * 100, 2),
    Fi = cumsum(fi),
    Hi = cumsum(hi),
    `Hi%` = round(Hi * 100, 2)
  ) %>%
  select(Li, Ls, fi, `hi%`, `Hi%`, Fi)

# Agregar total
total <- data.frame(
  Li = NA,
  Ls = "Total",
  fi = sum(tabla_freq$fi),
  `hi%` = sum(tabla_freq$`hi%`),
  `Hi%` = NA,
  Fi = NA,
  check.names = FALSE
)

tabla_final <- rbind(tabla_freq, total)
kable(tabla_final, align = 'r', digits = 2)

# HISTOGRAMA
ggplot(datos_filtrados, aes(x = `US$ FOB`)) +
  geom_histogram(aes(y = after_stat(count)/sum(after_stat(count))*100),
                 breaks = limites,
                 fill = "#FDB813",
                 color = "black") +
  geom_text(stat = 'bin',
            aes(y = after_stat(count)/sum(after_stat(count))*100,
                label = paste0(round(after_stat(count)/sum(after_stat(count))*100, 2), "%")),
            breaks = limites,
            vjust = -0.5,
            size = 3) +
  labs(title = "Distribución de lotes exportados a través de puerto Rodman según el precio total en US$",
       x = "Precio total en US$",
       y = "Porcentaje de lotes de exportaciones") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 10))
```
