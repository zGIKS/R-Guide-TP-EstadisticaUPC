# Objetivo Especifico 3: Distribucion de Unidades Exportadas por Puerto de Destino

## Variables
- **Via de transporte**: Cualitativa - Nominal
- **Cantidad de unidades**: Cuantitativa discreta - Razon
- **Puerto**: Cualitativa - Nominal

## Codigo R

```r
# ============================================
# LIBRERIAS NECESARIAS
# ============================================
library(readxl)
library(dplyr)
library(knitr)
library(ggplot2)
library(tidyr)
library(scales)

# ============================================
# LECTURA DE DATOS
# ============================================
dataset <- read_excel("Documentos/Estadistica/TF/data/dataset.xlsx")

# ============================================
# FILTRADO Y PREPARACION DE DATOS
# ============================================
# Filtrar solo puertos Balboa y Miami
datos_filtrados <- dataset %>%
  filter(Puerto %in% c("BALBOA", "MIAMI") |
         grepl("BALBOA|MIAMI", Puerto, ignore.case = TRUE))

# Agrupar por mes y puerto para obtener suma mensual
datos_mensuales <- datos_filtrados %>%
  group_by(Mes, Puerto) %>%
  summarise(Cantidad_mensual = sum(Cantidad, na.rm = TRUE), .groups = 'drop')

# ============================================
# TABLA No.3: MEDIDAS DE RESUMEN POR PUERTO
# ============================================
# Calculamos sobre las cantidades mensuales agregadas
medidas_resumen <- datos_mensuales %>%
  group_by(Puerto) %>%
  summarise(
    Media = mean(Cantidad_mensual, na.rm = TRUE),
    Mediana = median(Cantidad_mensual, na.rm = TRUE),
    Minimo = min(Cantidad_mensual, na.rm = TRUE),
    Maximo = max(Cantidad_mensual, na.rm = TRUE),
    Rango = Maximo - Minimo,
    Desviacion_estandar = sd(Cantidad_mensual, na.rm = TRUE),
    Varianza = var(Cantidad_mensual, na.rm = TRUE),
    CV_porcentaje = (Desviacion_estandar / Media)
  ) %>%
  pivot_longer(cols = -Puerto, names_to = "Medida", values_to = "Valor") %>%
  pivot_wider(names_from = Puerto, values_from = Valor)

# Mostrar tabla formateada
kable(medidas_resumen,
      caption = "Tabla No.3: Medidas de resumen de la cantidad de unidades exportadas hacia los puertos de Balboa y Miami, 2025",
      digits = 2,
      format.args = list(big.mark = ",", scientific = FALSE),
      align = 'r')

# ============================================
# GRAFICO No.3: VOLUMEN DE EXPORTACIONES MENSUALES
# ============================================
ggplot(datos_mensuales, aes(x = Mes, y = Cantidad_mensual, fill = Puerto, group = Puerto)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = comma(Cantidad_mensual)),
            position = position_dodge(width = 0.7),
            vjust = -0.5,
            size = 3) +
  scale_y_continuous(labels = comma,
                     expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c("BALBOA" = "#E95420", "MIAMI" = "#004B87")) +
  labs(title = "Grafico No.3: Volumen de exportaciones mensuales de palta\nhacia los puertos de Balboa y Miami, 2025",
       x = "Mes",
       y = "Cantidad de unidades",
       fill = "Puerto") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# ============================================
# GRAFICO No.4: DISTRIBUCION COMPARATIVA (BOXPLOT)
# ============================================
ggplot(datos_mensuales, aes(x = Puerto, y = Cantidad_mensual, fill = Puerto)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4,
               fill = "white", color = "black") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("BALBOA" = "#E95420", "MIAMI" = "#004B87")) +
  labs(title = "Grafico No.4: Distribucion de la cantidad de unidades exportadas\npor puerto de destino (Balboa vs Miami), 2025",
       x = "Puerto de destino",
       y = "Cantidad de unidades",
       caption = "Elaboracion propia con datos de ADEX Data Trade (2025)\nRombo = Media") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
        legend.position = "none",
        plot.caption = element_text(hjust = 0.5, size = 8))

# ============================================
# ANALISIS ADICIONAL: VOLUMEN TOTAL
# ============================================
volumen_total <- datos_mensuales %>%
  group_by(Puerto) %>%
  summarise(Total = sum(Cantidad_mensual, na.rm = TRUE)) %>%
  arrange(desc(Total))

print("Volumen total exportado por puerto:")
kable(volumen_total,
      format.args = list(big.mark = ","),
      align = 'r')

# ============================================
# DETECCION DE PATRONES INUSUALES
# ============================================
# Analisis de outliers usando el metodo IQR
outliers_analisis <- datos_mensuales %>%
  group_by(Puerto) %>%
  mutate(
    Q1 = quantile(Cantidad_mensual, 0.25, na.rm = TRUE),
    Q3 = quantile(Cantidad_mensual, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Limite_inferior = Q1 - 1.5 * IQR,
    Limite_superior = Q3 + 1.5 * IQR,
    Es_outlier = Cantidad_mensual < Limite_inferior |
                 Cantidad_mensual > Limite_superior
  )

# Mostrar limites de deteccion por puerto
limites_outliers <- outliers_analisis %>%
  select(Puerto, Q1, Q3, IQR, Limite_inferior, Limite_superior) %>%
  distinct()

print("Limites de deteccion de outliers (metodo IQR):")
kable(limites_outliers,
      caption = "Limites para deteccion de valores atipicos",
      digits = 2,
      format.args = list(big.mark = ",", scientific = FALSE),
      align = 'r')

# Filtrar y mostrar outliers si existen
outliers_detectados <- outliers_analisis %>%
  filter(Es_outlier == TRUE) %>%
  select(Puerto, Mes, Cantidad_mensual)

if(nrow(outliers_detectados) > 0) {
  print("\nPatrones inusuales detectados:")
  kable(outliers_detectados,
        caption = "Valores atipicos en las exportaciones de Balboa y Miami",
        format.args = list(big.mark = ","),
        align = 'r')
} else {
  print("\nNo se detectaron valores atipicos mediante el metodo IQR (Q1 - 1.5*IQR, Q3 + 1.5*IQR)")
  print("Todos los valores mensuales se encuentran dentro del rango esperado.")
}
```

## Interpretacion

### Estabilidad de envios
- **BALBOA**: CV = 0.81 (81%)
- **MIAMI**: CV = 0.82 (82%)

El puerto de **BALBOA** presenta mayor estabilidad (menor coeficiente de variacion), aunque la diferencia es minima.

### Volumen exportado
El puerto que concentra el mayor volumen exportado se identifica en la tabla de volumenes totales generada por el codigo.

### Patrones inusuales
Los valores atipicos detectados mediante el metodo IQR (rango intercuartilico) indican envios significativamente diferentes del patron habitual en cada puerto.
