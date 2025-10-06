# Objetivo Especifico 4: Rango Total y Mediana del Precio FOB por Empresa de Transporte

## Variables
- **Valor FOB (US$)**: Cuantitativa continua - Razon
- **Empresa de transporte**: Cualitativa - Nominal

## Codigo R

```r
# ============================================
# LIBRERIAS NECESARIAS
# ============================================
library(readxl)
library(dplyr)
library(knitr)
library(ggplot2)
library(scales)

# ============================================
# LECTURA DE DATOS
# ============================================
dataset <- read_excel("Documentos/Estadistica/TF/data/dataset.xlsx")

# ============================================
# FILTRADO Y PREPARACION DE DATOS
# ============================================
# Filtrar las tres empresas de transporte seleccionadas
empresas_seleccionadas <- c(
  "COSCO SHIPPING LINES (PERU) S.A",
  "IAN TAYLOR PERU S.A.C.",
  "MEDITERRANEAN SHIPPING COMPANY DEL PERU SAC"
)

datos_filtrados <- dataset %>%
  filter(`Empresa de Transporte` %in% empresas_seleccionadas |
         grepl("COSCO SHIPPING LINES.*PERU", `Empresa de Transporte`, ignore.case = TRUE) |
         grepl("IAN TAYLOR PERU", `Empresa de Transporte`, ignore.case = TRUE) |
         grepl("MEDITERRANEAN SHIPPING.*PERU", `Empresa de Transporte`, ignore.case = TRUE))

# ============================================
# TABLA No.4: RANGO TOTAL Y MEDIANA POR EMPRESA
# ============================================
tabla_resumen <- datos_filtrados %>%
  group_by(`Empresa de Transporte`) %>%
  summarise(
    `Rango total (US$)` = max(`US$ FOB`, na.rm = TRUE) - min(`US$ FOB`, na.rm = TRUE),
    `Mediana (US$)` = median(`US$ FOB`, na.rm = TRUE)
  ) %>%
  arrange(desc(`Rango total (US$)`))

# Mostrar tabla formateada
kable(tabla_resumen,
      caption = "Tabla No.4: Rango total y mediana del Valor FOB (US$) de las empresas de transporte seleccionadas, 2025",
      digits = 2,
      format.args = list(big.mark = ",", scientific = FALSE),
      align = c('l', 'r', 'r'))

# ============================================
# GRAFICO No.5: DIAGRAMA DE CAJAS DEL VALOR FOB
# ============================================
ggplot(datos_filtrados, aes(x = `Empresa de Transporte`, y = `US$ FOB`, fill = `Empresa de Transporte`)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2, outlier.color = "red") +
  stat_summary(fun = median, geom = "point", shape = 23, size = 4,
               fill = "white", color = "black") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c(
    "COSCO SHIPPING LINES (PERU) S.A" = "#1f77b4",
    "IAN TAYLOR PERU S.A.C." = "#ff7f0e",
    "MEDITERRANEAN SHIPPING COMPANY DEL PERU SAC" = "#2ca02c"
  )) +
  labs(title = "Grafico No.5: Diagrama de cajas del Valor FOB (US$)\npor empresa de transporte",
       x = "Empresa de Transporte",
       y = "Valor FOB (US$)",
       caption = "Elaboracion propia con datos de ADEX Data Trade (2025)\nRombo = Mediana") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
    axis.text.x = element_text(angle = 15, hjust = 1, size = 8),
    legend.position = "none",
    plot.caption = element_text(hjust = 0.5, size = 8)
  )

# ============================================
# ANALISIS ADICIONAL: ESTADISTICAS DESCRIPTIVAS
# ============================================
estadisticas_completas <- datos_filtrados %>%
  group_by(`Empresa de Transporte`) %>%
  summarise(
    Minimo = min(`US$ FOB`, na.rm = TRUE),
    Q1 = quantile(`US$ FOB`, 0.25, na.rm = TRUE),
    Mediana = median(`US$ FOB`, na.rm = TRUE),
    Q3 = quantile(`US$ FOB`, 0.75, na.rm = TRUE),
    Maximo = max(`US$ FOB`, na.rm = TRUE),
    Rango = Maximo - Minimo,
    Media = mean(`US$ FOB`, na.rm = TRUE),
    Desviacion_std = sd(`US$ FOB`, na.rm = TRUE)
  ) %>%
  arrange(desc(Rango))

print("Estadisticas descriptivas completas:")
kable(estadisticas_completas,
      caption = "Estadisticas descriptivas del Valor FOB por empresa",
      digits = 2,
      format.args = list(big.mark = ",", scientific = FALSE),
      align = 'r')
```

## Interpretacion

### Rango Total
El **rango total** indica la amplitud de variacion en los precios FOB:
- **COSCO SHIPPING LINES (PERU) S.A**: Rango = 81,476.47 US$ (mayor variabilidad)
- **IAN TAYLOR PERU S.A.C.**: Rango = 76,329.45 US$
- **MEDITERRANEAN SHIPPING COMPANY DEL PERU SAC**: Rango = 72,715.49 US$ (menor variabilidad)

### Mediana
La **mediana** representa el valor central de los lotes exportados:
- **COSCO SHIPPING LINES (PERU) S.A**: 39,734.60 US$
- **IAN TAYLOR PERU S.A.C.**: 38,743.20 US$
- **MEDITERRANEAN SHIPPING COMPANY DEL PERU SAC**: 35,628.00 US$

COSCO presenta tanto el mayor rango como la mediana mas alta, lo que sugiere que maneja lotes de mayor valor y con mayor variabilidad en sus precios.

### Diagrama de Cajas
El diagrama de cajas permite visualizar:
- La distribucion del 50% central de los datos (caja)
- Los valores extremos (bigotes)
- Valores atipicos (puntos fuera de los bigotes)
- La mediana (linea en el centro de la caja / rombo blanco)
