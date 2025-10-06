```r
library(readxl)
library(dplyr)
library(knitr)
library(plotly)

dataset <- read_excel("Documentos/Estadistica/TF/data/dataset.xlsx")

paises_norte <- c("Estados Unidos", "USA", "United States", "Canadá", "Canada", "México", "Mexico")
datos_filtrados <- dataset %>%
  filter(País %in% paises_norte | grepl("Estados Unidos|USA|United States|Canadá|Canada|México|Mexico", País, ignore.case = TRUE))

tabla <- table(datos_filtrados$País, datos_filtrados$`Vía de transporte`)
tabla_pct <- prop.table(tabla, margin = 1) * 100
tabla_pct <- cbind(tabla_pct, `Total general` = 100)

totales <- colSums(tabla)
total_pct <- c((totales / sum(tabla)) * 100, 100)
tabla_final <- rbind(tabla_pct, `Total general` = total_pct)

kable(tabla_final, digits = 2, align = 'r')

datos_grafico <- datos_filtrados %>%
  group_by(País, `Vía de transporte`) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(País) %>%
  mutate(porcentaje = n / sum(n) * 100)

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
