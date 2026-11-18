# Pronóstico de la contratación pública en Colombia

App Shiny en R que pronostica cuántos contratos firmará el Estado colombiano en SECOP II en los próximos meses, y deja ver cómo se eligió el modelo.

**App publicada:** [Posit Connect Cloud](https://connect.posit.cloud/forero25)

## Qué tiene

- **Pronóstico.** Los próximos 12 meses con el modelo elegido, su intervalo del 80 % y los efectos del calendario electoral.
- **Laboratorio.** Cambias el modelo, el período de entrenamiento, el horizonte o el intervalo, y el servidor vuelve a estimar el modelo en R.
- **Evaluación.** Lo que habría predicho cada modelo desde cualquier mes desde diciembre de 2022, contra lo que pasó, y el error según el horizonte.
- **Método.** Datos, modelos, cómo se eligió y qué no hace.

## El modelo

Regresión sobre el logaritmo de los contratos mensuales con un efecto por mes del año y errores ARIMA(0,1,1), más dos variables del calendario electoral: enero de un año de elección presidencial y los meses de febrero a junio de ese año, cuando la Ley de Garantías restringe la contratación directa. Compite contra un modelo estacional sin elecciones y contra el ingenuo estacional (el mismo mes del año anterior) en una evaluación con origen móvil. Solo usa R base (`arima()`); el código está en [`R/modelo.R`](R/modelo.R).

## Datos

La app lee al abrirse los CSV agregados que el [observatorio de contratación](https://github.com/Forero25/observatorio-secop-datos) publica cada madrugada: la serie mensual, el calendario y los resultados de la evaluación. No hay datos de contratistas.

Fuente: SECOP II – Contratos Electrónicos, Colombia Compra Eficiente, en datos.gov.co, licencia CC BY-SA 4.0.

## Correrla

```r
install.packages(c("shiny", "bslib", "plotly"))
shiny::runApp()
```

`manifest.json` lo genera el flujo de GitHub Actions con `rsconnect::writeManifest()` después de probar que la app arranca. Posit Connect Cloud lo usa para instalar la misma versión de R y de cada paquete, y vuelve a publicar la app con cada push a `main`.

## Limitaciones

- Cuenta contratos, no pesos.
- Es nacional: no pronostica por municipio ni por entidad.
- No conoce las elecciones territoriales, que tienen su propia restricción.
- Los efectos electorales salen de solo dos elecciones presidenciales en la serie.

José Nicolás Forero Urrego · [nicolasforero.pages.dev](https://nicolasforero.pages.dev)
