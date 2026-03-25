# 🏦 Clasificación de Potenciales Clientes Bancarios — Data Mining

Aplicación de técnicas de minería de datos y aprendizaje supervisado para identificar y segmentar **potenciales clientes bancarios**, apoyando decisiones de captación y campañas comerciales.

## 📊 Datos

| Atributo | Detalle |
|----------|---------|
| Fuente | Dataset de caracterización de clientes |
| Contexto | Sector bancario / financiero |
| Objetivo | Clasificar clientes potenciales según perfil de comportamiento y características socioeconómicas |

## ⚙️ Métodos

- Análisis exploratorio de datos (EDA): distribuciones, correlaciones, valores faltantes
- Preprocesamiento: imputación, codificación de variables categóricas, normalización
- Selección de variables: filtros de entropía basados en ganancia de información
- Modelado: algoritmos de clasificación supervisada
- Evaluación: métricas de clasificación, matrices de confusión y curvas ROC

## 🤖 Modelos aplicados

- K-Nearest Neighbors (KNN)
- Regresión Logística
- Árboles de Decisión (con poda por validación cruzada)
- Random Forest
- Support Vector Machine (SVM)

## 💡 Resultados clave

- Identificación de variables socioeconómicas con mayor poder predictivo para clasificar clientes potenciales
- Comparación de modelos por AUC, accuracy, sensibilidad y especificidad
- Generación de perfiles de cliente con base en clustering complementario

## 🛠 Herramientas

`R` · `caret` · `pROC` · `FSelector` · `ggplot2` · `mice`

## 📁 Contenido del repositorio

```
├── script_clientes_bancarios.R     # Código completo
├── informe_final.pdf               # Informe con resultados y análisis
└── README.md
```
