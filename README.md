# Pilot Dashboard (Shiny)

Pilot_dashboard is a local Shiny application designed for the exploratory analysis and quality control of behavioral pilot data. 
The app allows flexible column mapping from CSV files (e.g., PsychoPy outputs), visualization of reaction times and accuracy, 
computation of Simon effects at the subject and group level, and inspection of trial retention after filtering.

The dashboard is intended for rapid diagnostics of pilot studies and early-stage experiments in cognitive and experimental psychology.

Dashboard local para visualizar RT/Accuracy y métricas del Simon task.

## Requisitos
- R >= 4.2
- Paquetes: shiny, tidyverse, DT, plotly, readr, readxl, writexl.

## Ejecutar
```r
shiny::runApp()
```

## Simulated Data 
He agregado conjuntos de datos que son ficticios para probar la correcta ejecución de la shiny app