# Trabajo Final EYP3407 - Tópicos Aplicados en Estadística

Este repositorio contiene el código y los resultados del trabajo final del curso EYP3407 (Segundo Semestre 2026, Pontificia Universidad Católica de Chile).

## Identificación del Equipo
* **Diego Rodríguez**
* **Eilenne Jaque**
* **Benjamín Mondaca**
* **Matías Gomez**

## Caso Asignado
**Caso 05:** Predicción de incidencias operativas en vuelos programados.
*   **Problema:** Identificar, con información de programación previa al vuelo, qué operaciones domésticas con salida desde BOS, JFK o LGA (2022–2024) terminarán canceladas, desviadas o con un retraso de llegada de al menos 15 minutos.

## Estructura del Repositorio
El repositorio debe contar con la siguiente estructura mínima para ejecutarse correctamente:
```text
├── datos/                  # Directorio para los archivos de entrada (vuelos.csv, diccionario.csv)
├── main.R                  # Script principal y punto de entrada único
├── id_predictores.R        # Script auxiliar para auditoría y exclusión de variables (cargado por main.R)
├── Informe.tex             # Código fuente del informe en LaTeX
├── caso5.pdf        # Diapositivas de la presentación final
└── README.md               # Este archivo
```

## Obtención y Preparación de los Datos
Los datos provienen del *Bureau of Transportation Statistics (BTS): Reporting Carrier On-Time Performance*. 

1. Se puede encontrar el archivo `caso05_bts_vuelos.zip` con los datos en este enlace: https://drive.google.com/file/d/1Mp4zFvCUtrN7Muzpu36aT8MNnyMI-NzS/view?usp=drive_link.
2. Descomprimir el archivo `.zip`.
3. Mover los archivos resultantes (`vuelos.csv` y `diccionario.csv`) dentro de la carpeta `datos/` en la raíz de este repositorio.

## Entorno y Dependencias
El análisis fue desarrollado y validado en el siguiente entorno:
*   **Sistema Operativo:** Windows x86_64
*   **Versión de R:** 4.6.0 (2026-04-24)
*   **Hardware utilizado (referencia de tiempos):** Procesador Intel Core i7, 16 GB RAM.

### Paquetes requeridos
Asegúrese de tener instalados los siguientes paquetes antes de ejecutar:
*   `dplyr`
*   `tidyr`
*   `glmnet` (v. 5.0)
*   `rpart`
*   `ranger` (v. 0.18.0)

## Instrucciones de Ejecución
Una vez instaladas las dependencias y ubicados los datos en la carpeta `datos/`, abra una terminal o consola de comandos en la raíz de este repositorio y ejecute **un único comando**:

```bash
Rscript --vanilla main.R
```

Este comando ejecutará de manera secuencial la limpieza de datos, particiones temporales, selección de hiperparámetros, entrenamiento de modelos, evaluación en la reserva de prueba de 2024, cálculo de incertidumbre (bootstrap) y sensibilidades. Los resultados se guardarán automáticamente en la carpeta `resultados/`. El tiempo de ejecución aproximado es de **84,2 minutos**.
Las semillas aleatorias están fijadas internamente en el código (`set.seed(2026)` y `seed = 2026` para `ranger`) para garantizar resultados exactos.

## Prueba desde Copia Limpia
*   **Fecha de prueba:** 10 de noviembre de 2026.
*   **Comando:** `Rscript --vanilla main.R`
*   **Resultado:** La ejecución finalizó de manera exitosa en un entorno clonado, sin depender de objetos del *Workspace* previos. Las tablas, métricas de validación y la figura de calibración generadas en `resultados/` coinciden de forma idéntica con los valores reportados en el informe escrito y la presentación.

## Síntesis de Aportes del Equipo
*   **Diego Rodríguez:** Diseño general, creación y arquitectura del repositorio de código, definición del esquema de validación temporal y revisión del informe final.
*   **Eilenne Jaque:** Implementación completa del pipeline de procesamiento de datos, codificación de los procedimientos predictivos (`glmnet`, `rpart`, `ranger`), ejecución de los experimentos de validación, prueba y análisis de sensibilidad.
*   **Benjamín Mondaca:** Análisis e interpretación de los resultados en desarrollo y prueba, evaluación de matrices de confusión por umbral, soporte en la construcción conceptual de las tasas históricas y revisión del análisis de incertidumbre via bootstrap.
*   **Matías Gomez:** Auditoría de predictores y variables excluidas, diseño y verificación de las tablas de desempeño del informe, elaboración del análisis de heterogeneidad (por aeropuerto/temporada) y preparación del material para la presentación.

## Declaración de uso de Inteligencia Artificial
Se utilizó asistencia del modelo de lenguaje Claude con el objetivo de:
*   Generación y estructuración de borradores iniciales de secciones textuales del informe, así como revisión de sintaxis, pulido de estilo y formateo en LaTeX.
*   Recibir sugerencias para la optimización y vectorización del código en R (específicamente en el cálculo histórico por grupos temporales).
*   Estructurar borradores del guion para la presentación.
*   **Verificación:** Todo el código estadístico, los resultados matemáticos y las conclusiones fueron revisados, ejecutados y validados independientemente por el equipo.
