# Revisión del Laboratorio 1

Referencia: Nery2004/Laboratorio-1.-Series-de-Tiempo, commit
`5a7d8af1c9cbcaafc6cf1ade07baa8369683cafc`.

La revisión incluyó los módulos 00–08, README y reporte de preflight. La copia local de
referencia se conserva en `.local/lab1/`, ignorada por Git.

| Componente | Definición conservada o ajuste |
|---|---|
| Limpieza | Nombres normalizados, espacios, año/mes numéricos, viajeros decimales, suma por dimensiones naturales |
| Universo | Turista + Excursionista; exclusión de Viajero y Cruceristas |
| Siete series | Total comparable, Aérea, Terrestre, Marítima, Top 3 por Región dos |
| Estacionalidad | STL periódica robusta sobre entrenamiento, fuerza acotada a [0,1] |
| Diagnóstico | Transformación Box-Cox/log1p, ADF, KPSS, diferencias y residuos |
| ARIMA | Búsqueda exhaustiva y tres candidatos cercanos; parámetros elegidos con entrenamiento |
| Prophet | Estacionalidad anual; semanal y diaria desactivadas; banda real 80 % |
| ETS y Seasonal Naive | Ajuste original, horizonte igual a prueba |
| Holt-Winters | Mismas variantes, selección con validación interna de 24 meses; reajuste sobre entrenamiento completo |
| Métricas | RMSE y MAE originales, predicción no negativa; verificación independiente |
| Inversa Box-Cox | Si la corrección de sesgo no es finita, usar inversa sin corrección y registrar; se exigen 63 predicciones finitas, sin omitir meses |
| Ganador total | Mínimo RMSE sobre prueba, MAE como desempate |

No hay resultados numéricos originales publicados que se puedan contrastar: el reporte
de validación del repositorio indica que no encontró Excel ni guía. No se copian cifras
de un informe inexistente. El laboratorio actual genera resultados reales y contrasta
las series con agregación independiente de la base.

La selección de mercados por acumulado total se conserva como definición retrospectiva
del análisis. La comparación de modelos sobre prueba no demuestra rendimiento fuera de
esa prueba. La validación interna de Holt-Winters evita ajustar hiperparámetros sobre
los 63 meses destinados a la comparación final.
