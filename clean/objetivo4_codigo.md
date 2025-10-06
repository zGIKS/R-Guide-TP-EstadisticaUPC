```r
library(readxl)
library(dplyr)
library(knitr)
library(ggplot2)
library(scales)

dataset <- read_excel("Documentos/Estadistica/TF/data/dataset.xlsx")

empresas_seleccionadas <- c("COSCO SHIPPING LINES (PERU) S.A", "IAN TAYLOR PERU S.A.C.", "MEDITERRANEAN SHIPPING COMPANY DEL PERU SAC")

datos_filtrados <- dataset %>%
  filter(`Empresa de Transporte` %in% empresas_seleccionadas)

tabla_resumen <- datos_filtrados %>%
  group_by(`Empresa de Transporte`) %>%
  summarise(`Rango total (US$)` = max(`US$ FOB`, na.rm = TRUE) - min(`US$ FOB`, na.rm = TRUE),
            `Mediana (US$)` = median(`US$ FOB`, na.rm = TRUE)) %>%
  arrange(desc(`Rango total (US$)`))

kable(tabla_resumen, digits = 2, format.args = list(big.mark = ",", scientific = FALSE), align = c('l', 'r', 'r'))

ggplot(datos_filtrados, aes(x = `Empresa de Transporte`, y = `US$ FOB`, fill = `Empresa de Transporte`)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2, outlier.color = "red") +
  stat_summary(fun = median, geom = "point", shape = 23, size = 4, fill = "white", color = "black") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("COSCO SHIPPING LINES (PERU) S.A" = "#1f77b4", "IAN TAYLOR PERU S.A.C." = "#ff7f0e", "MEDITERRANEAN SHIPPING COMPANY DEL PERU SAC" = "#2ca02c")) +
  labs(title = "Diagrama de cajas del Valor FOB (US$)\npor empresa de transporte", x = "Empresa de Transporte", y = "Valor FOB (US$)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"), axis.text.x = element_text(angle = 15, hjust = 1, size = 8), legend.position = "none")
```
