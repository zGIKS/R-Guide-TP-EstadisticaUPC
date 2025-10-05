# Objetivo Específico 1: Análisis de Vías de Transporte en Exportación de Palta

## Variables
- **País**: Cualitativa - Nominal
- **Vía de transporte**: Cualitativa - Nominal

## Código R

```r
# Librerías
library(readxl)
library(dplyr)
library(knitr)
library(plotly)

# Leer datos
dataset <- read_excel("Documentos/Estadistica/TF/data/dataset.xlsx")

# Filtrar América del Norte
paises_norte <- c("Estados Unidos", "USA", "United States", "Canadá", "Canada", "México", "Mexico")
datos_filtrados <- dataset %>%
  filter(País %in% paises_norte | grepl("Estados Unidos|USA|United States|Canadá|Canada|México|Mexico", País, ignore.case = TRUE))

# TABLA DE PORCENTAJES
tabla <- table(datos_filtrados$País, datos_filtrados$`Vía de transporte`)
tabla_pct <- prop.table(tabla, margin = 1) * 100
tabla_pct <- cbind(tabla_pct, `Total general` = 100)

# Agregar fila Total general
totales <- colSums(tabla)
total_pct <- c((totales / sum(tabla)) * 100, 100)
tabla_final <- rbind(tabla_pct, `Total general` = total_pct)

# Mostrar tabla
kable(tabla_final, digits = 2, align = 'r')

# GRÁFICO DE BARRAS 3D APILADO
datos_grafico <- datos_filtrados %>%
  group_by(País, `Vía de transporte`) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(País) %>%
  mutate(porcentaje = n / sum(n) * 100)

# Crear gráfico 3D con barras apiladas
fig <- plot_ly(datos_grafico,
               x = ~País,
               y = ~porcentaje,
               z = ~`Vía de transporte`,
               color = ~`Vía de transporte`,
               type = 'bar',
               colors = c('#E95420', '#004B87')) %>%
  layout(barmode = 'stack',
         title = "Distribución de Exportaciones por País y Vía de Transporte",
         yaxis = list(title = "%"),
         xaxis = list(title = "País"))
fig
```
