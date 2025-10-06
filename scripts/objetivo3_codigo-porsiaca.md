```r
library(readxl)
library(dplyr)
library(knitr)
library(ggplot2)
library(tidyr)
library(scales)

dataset <- read_excel("Documentos/Estadistica/TF/data/dataset.xlsx")

datos_filtrados <- dataset %>%
  filter(Puerto %in% c("BALBOA", "MIAMI") | grepl("BALBOA|MIAMI", Puerto, ignore.case = TRUE))

datos_mensuales <- datos_filtrados %>%
  group_by(Mes, Puerto) %>%
  summarise(Cantidad_mensual = sum(Cantidad, na.rm = TRUE), .groups = 'drop')

medidas_resumen <- datos_mensuales %>%
  group_by(Puerto) %>%
  summarise(Media = mean(Cantidad_mensual, na.rm = TRUE), Mediana = median(Cantidad_mensual, na.rm = TRUE), 
            Minimo = min(Cantidad_mensual, na.rm = TRUE), Maximo = max(Cantidad_mensual, na.rm = TRUE),
            Rango = Maximo - Minimo, Desviacion_estandar = sd(Cantidad_mensual, na.rm = TRUE),
            Varianza = var(Cantidad_mensual, na.rm = TRUE), CV_porcentaje = (Desviacion_estandar / Media)) %>%
  pivot_longer(cols = -Puerto, names_to = "Medida", values_to = "Valor") %>%
  pivot_wider(names_from = Puerto, values_from = Valor)

kable(medidas_resumen, digits = 2, format.args = list(big.mark = ",", scientific = FALSE), align = 'r')

ggplot(datos_mensuales, aes(x = Mes, y = Cantidad_mensual, fill = Puerto, group = Puerto)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = comma(Cantidad_mensual)), position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c("BALBOA" = "#E95420", "MIAMI" = "#004B87")) +
  labs(title = "Volumen de exportaciones mensuales de palta\nhacia los puertos de Balboa y Miami, 2025", x = "Mes", y = "Cantidad de unidades", fill = "Puerto") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

ggplot(datos_mensuales, aes(x = Puerto, y = Cantidad_mensual, fill = Puerto)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, fill = "white", color = "black") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("BALBOA" = "#E95420", "MIAMI" = "#004B87")) +
  labs(title = "Distribucion de la cantidad de unidades exportadas\npor puerto de destino (Balboa vs Miami), 2025", x = "Puerto de destino", y = "Cantidad de unidades") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"), legend.position = "none")
```
