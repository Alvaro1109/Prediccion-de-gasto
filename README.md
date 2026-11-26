# Predicción del gasto en comunicación y TICs de empresas

Proyecto de econometría y análisis de datos en **R** que busca explicar **si una empresa decide gastar** en servicios de comunicación y TICs y **cuánto gasta**, a partir de sus variables financieras, de personal y de equipamiento tecnológico.

> Trabajo final universitario (diciembre 2022) — Ingeniería en Información y Control de Gestión, Universidad Católica de la Santísima Concepción.
> Autores: María José Aravena, Alvaro Concha, Francisca Cabrera y Carolina Véjar.

📄 **Informe completo:** abrir [`TRABAJO-FINAL.html`](TRABAJO-FINAL.html) (descargarlo y abrirlo en el navegador).

---

## Objetivos

1. **Decisión de gasto (variable dicotómica):** explicar si una empresa gasta o no en comunicación y TICs durante 2019.
2. **Nivel de gasto (variable continua):** predecir cuánto gasta una empresa en comunicación y TICs.

## Datos

- `EC1116C dataf.xlsx`: base con más de 440 variables numéricas por empresa (ingresos, gastos, activos, pasivos, remuneraciones, subcontratación, equipamiento, etc.).
- `EC1116C dataf_dicc.xlsx`: diccionario de variables (código y glosa de cada pregunta).

## Metodología

1. **Limpieza:** selección de variables numéricas e imputación de valores faltantes.
2. **Selección de variables:** la base se dividió en bloques y se calcularon las correlaciones de cada variable con el gasto en comunicación y TICs, conservando las más relevantes (correlaciones entre 0,35 y 0,52 para el nivel de gasto).
3. **Modelos para la decisión de gasto:** modelos de probabilidad lineal, **probit** y **logit**, incluyendo interacciones entre variables (por ejemplo `Software × Equipos`).
4. **Modelos para el nivel de gasto:** regresiones lineales con distintas combinaciones e interacciones de ingresos, gastos y remuneraciones.
5. **Comparación de modelos:** R², AIC y BIC.
6. **Validación:** corrección de **heterocedasticidad y autocorrelación** con errores estándar robustos (matriz HAC).

## Principales hallazgos

- **Decisión de gasto:** el mejor modelo combina la interacción `Software × Equipos` con el año de inicio de la empresa. Tener software y equipos computacionales aumenta la probabilidad de gastar en comunicación y TICs.
- **Nivel de gasto:** los mejores modelos alcanzan un R² de **26,3 %**. Ingresos y gastos de 2019 por separado no resultan significativos tras la corrección HAC, pero **su interacción sí lo es**.
- Las remuneraciones totales de mujeres resultan significativas para explicar el nivel de gasto, a diferencia de las de hombres.

## Tecnologías

![R](https://img.shields.io/badge/R-276DC3?logo=r&logoColor=white)
![RStudio](https://img.shields.io/badge/RStudio-75AADB?logo=rstudioide&logoColor=white)
![R Markdown](https://img.shields.io/badge/R%20Markdown-1A162D?logo=markdown&logoColor=white)

Paquetes: `tidyverse`, `readxl`, `skimr`, `corrplot`, `broom`, `lmtest`, `plm`.

## Cómo ejecutarlo

1. Clonar el repositorio y abrir `TRABAJO FINAL.Rproj` en RStudio.
2. Instalar los paquetes:
   ```r
   install.packages(c("tidyverse", "readxl", "skimr", "corrplot", "broom", "lmtest", "plm"))
   ```
3. Abrir `TRABAJO FINAL.Rmd` y presionar **Knit** para regenerar el informe HTML.
   (`TRABAJO FINAL.R` contiene el mismo análisis como script).

## Estructura

```
├── TRABAJO FINAL.Rmd      # Análisis completo en R Markdown
├── TRABAJO FINAL.R        # Versión en script
├── TRABAJO-FINAL.html     # Informe generado
├── EC1116C dataf.xlsx     # Datos
├── EC1116C dataf_dicc.xlsx# Diccionario de variables
└── TRABAJO FINAL.Rproj    # Proyecto de RStudio
```
