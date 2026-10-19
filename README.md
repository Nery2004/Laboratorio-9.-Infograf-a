# Laboratorio 9 · Visualización Estática

**Universidad del Valle de Guatemala · CC3084 Data Science**  
**Nery Molina · 23218**

Implementación local de una infografía vertical sobre visitantes de Guatemala, enero
2009–junio 2026. Todos los indicadores, modelos, recomendaciones cuantificadas y gráficos
se calculan desde el Excel. No se usan datos simulados.

## Ejecutar

1. Instalar R 4.4 o posterior. La ejecución inicial requiere internet para paquetes CRAN.
2. Colocar el original, sin modificar, en
   `data/raw/Base_Migracion_2009-2026jun(3).xlsx`.
3. Desde este directorio ejecutar:

```powershell
Rscript run_lab9.R
```

En este equipo también funciona la instalación privada preparada para el laboratorio:

```powershell
.\.local\R\bin\Rscript.exe run_lab9.R
```

El comando instala dependencias faltantes, ejecuta los análisis, genera los entregables y
valida resultados. Un fallo produce código de salida distinto de cero y
`validation/estado.txt`; no se acepta como entrega completa. Los ajustes ARIMA exhaustivos
y Prophet para siete series pueden tardar varios minutos. El directorio de trabajo se
resuelve a partir de la ubicación del script.

## Datos y correspondencia con Laboratorio 1

Base de 161,036 registros, hojas `Datos` y `Notas`, 210 meses. Se mantiene la limpieza del
[Laboratorio 1](https://github.com/Nery2004/Laboratorio-1.-Series-de-Tiempo), fijada al commit
`5a7d8af1c9cbcaafc6cf1ade07baa8369683cafc`. Los módulos adaptados están en `R/lab1/`.
Se agregan por suma las combinaciones repetidas, conservando totales; visitantes significa
**Turista + Excursionista**. No incluye Viajero ni Cruceristas. Los decimales se conservan.

Se construyen total, tres vías y las tres regiones de `Región dos` con mayor acumulado.
El Top 3 usa el período completo para conservar la definición descriptiva del Lab 1; no
se seleccionan mercados usando su error de prueba. Es una selección retrospectiva del
universo de series y no una evaluación de selección de mercados futuros.

Los modelos reciben exclusivamente los 147 meses iniciales (enero 2009–marzo 2021).
Los 63 restantes (abril 2021–junio 2026) son prueba. Se mantienen candidatos ARIMA,
transformación elegida con entrenamiento, Prophet anual, ETS y Seasonal Naive.
**Cambio necesario:** Holt-Winters elige su configuración en los últimos 24 meses del
entrenamiento y se reajusta con los 147; el original la elegía sobre prueba. Las
predicciones se limitan a cero al evaluar, como en Lab 1. Los intervalos reales del
modelo se conservan sin fabricar márgenes ni recortarlos.

La validación encontró otra limitación del original: la corrección de sesgo de la
inversa Box-Cox puede generar medias no finitas y sus métricas omitían esos horizontes.
En ese caso se usa la inversa sin corrección de sesgo, se registra en
`outputs/tables/ajustes_pronostico.csv` y se evalúan los 63 meses completos. La regla no
consulta valores reales de prueba. Los candidatos que no convergen quedan registrados;
se exige al menos un ajuste válido de cada una de las cinco familias por serie.

La comparación final selecciona por RMSE de prueba (MAE desempata). Por tanto, ese error
describe el conjunto usado para comparar modelos, no una nueva estimación independiente
del rendimiento del ganador. Se muestra la predicción de prueba, no una extrapolación
posterior a junio de 2026. La banda se rotula como estimación predictiva del 80 %.

El repositorio del Lab 1 consultado no publica resultados calculados: su reporte de
preflight indica ausencia de Excel. La equivalencia se valida contra sus definiciones y
una reconstrucción independiente desde las filas originales; no se afirma igualdad con
métricas históricas que no están disponibles. Véase `docs/revision_lab1.md`.

## Entregables locales

- `infografia/infografia_final.png`: 3300 × 5100 píxeles, 300 dpi.
- `infografia/infografia_final.pdf`: una página vertical de 11 × 17 pulgadas.
- `docs/explicacion_diseno.docx`: carátula UVG, Times New Roman 12, márgenes de una pulgada.
- `docs/decisiones_diseno.md`: contenido del documento generado.
- `outputs/figures/`: gráficos auxiliares y diagnósticos.
- `outputs/tables/`: KPIs, estacionalidad, cuadrante, modelos, métricas y predicciones.
- `outputs/models/`: ajustes reproducibles RDS.
- `outputs/tables/insumo_sha256.txt`: hash del Excel usado.
- `validation/resultados.csv`, `validation/estado.txt`, `validation/sessionInfo.txt`.

El cuadrante compara fuerza estacional STL del entrenamiento, CAGR entre 2009 y 2025,
coeficiente de variación mensual del período completo y caída anual 2020/2019.
El promedio por mes incorpora todos los datos disponibles; enero–junio tiene un año más
de observaciones que julio–diciembre por el semestre parcial de 2026.

## Fuente, comparabilidad y publicación

La hoja `Notas` describe una base **exclusivamente académica; no son cifras oficiales**,
con respaldos históricos, entrega IGM y sistema depurado INGUAT. Se conserva el cambio de
granularidad de países desde 2023, el quiebre metodológico de 2023, la pérdida de detalle
marítimo desde 2017 y la cobertura parcial de 2026. El gráfico no atribuye causalmente
todo cambio a la demanda: la medición también cambia.

El archivo local encontrado como `Base_Migracion_2009-2026jun.xlsx` se copió con el nombre
de entrada solicitado conservando sus bytes. La estructura y los controles numéricos se
verifican al ejecutar; el nombre por sí mismo no certifica una versión del insumo.

**No se publica nada automáticamente.** Excel, datos procesados, tablas, modelos,
infografía, documento generado y validaciones con resultados están ignorados por Git.
Solo se prepara código y documentación metodológica para revisión. No se debe forzar la
inclusión de derivados académicos sin autorización expresa.

## Validaciones

El pipeline comprueba filas, meses consecutivos, conservación de totales, definición
comparable, las siete series reconstruidas desde el original, fechas sin solapamiento,
cinco familias por serie, los cuatro KPIs de control y RMSE/MAE recalculados desde cada
predicción. También verifica ganador, intervalo finito y ordenado, dimensiones del PNG,
una página PDF y tamaño físico de impresión. Los ajustes guardados permiten inspeccionar
las entradas de los modelos; ninguna transformación ARIMA se estima con prueba.

La lista de entrega está en `docs/lista_entrega.md`.
