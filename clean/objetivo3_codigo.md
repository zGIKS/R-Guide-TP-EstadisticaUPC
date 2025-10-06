```r
library(readxl)
library(dplyr)
library(knitr)
library(tidyr)

dataset <- read_excel("Documentos/Estadistica/TF/data/dataset.xlsx")

datos_filtrados <- dataset %>%
  filter(Puerto %in% c("BALBOA", "MIAMI") | grepl("BALBOA|MIAMI", Puerto, ignore.case = TRUE))

datos_mensuales <- datos_filtrados %>%
  group_by(Mes, Puerto) %>%
  summarise(Cantidad_mensual = sum(Cantidad, na.rm = TRUE), .groups = 'drop')

medidas_resumen <- datos_mensuales %>%
  group_by(Puerto) %>%
  summarise(Media = mean(Cantidad_mensual, na.rm = TRUE),
            Mediana = median(Cantidad_mensual, na.rm = TRUE),
            Minimo = min(Cantidad_mensual, na.rm = TRUE),
            Maximo = max(Cantidad_mensual, na.rm = TRUE),
            Rango = Maximo - Minimo,
            Desviacion_estandar = sd(Cantidad_mensual, na.rm = TRUE),
            Varianza = var(Cantidad_mensual, na.rm = TRUE),
            CV_porcentaje = (Desviacion_estandar / Media)) %>%
  pivot_longer(cols = -Puerto, names_to = "Medida", values_to = "Valor") %>%
  pivot_wider(names_from = Puerto, values_from = Valor)

kable(medidas_resumen, digits = 2, format.args = list(big.mark = ",", scientific = FALSE), align = 'r')
```
