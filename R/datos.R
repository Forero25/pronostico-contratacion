# Lee los CSV agregados que el observatorio publica cada día en un repositorio público.
# No hay datos de contratistas: solo conteos mensuales y resultados del modelo.

ORIGEN <- Sys.getenv(
  "ORIGEN_DATOS",
  "https://github.com/Forero25/observatorio-secop-datos/releases/download/datos/"
)

leer <- function(archivo) {
  ruta <- paste0(ORIGEN, archivo)
  read.csv(ruta, stringsAsFactors = FALSE, encoding = "UTF-8")
}

cargar_datos <- function() {
  serie <- leer("serie_mensual.csv")
  serie <- serie[serie$completo %in% c("true", TRUE), c("fecha", "contratos")]
  serie$fecha <- as.Date(serie$fecha)
  serie <- serie[order(serie$fecha), ]

  cal <- leer("calendario.csv")
  anios_eleccion <- sort(unique(cal$anio[cal$eleccion_presidencial == "Año de elección presidencial"]))
  if (length(anios_eleccion) > 0) anios_eleccion <- sort(unique(c(anios_eleccion, max(anios_eleccion) + 4)))

  retro <- leer("pronostico_retro.csv")
  retro$origen <- as.Date(retro$origen)
  retro$fecha <- as.Date(retro$fecha)

  evaluacion <- leer("pronostico_evaluacion.csv")
  resumen <- leer("pronostico_resumen.csv")

  list(
    serie = serie,
    anios_eleccion = anios_eleccion,
    retro = retro,
    evaluacion = evaluacion,
    resumen = setNames(resumen$valor, resumen$clave),
    elegido = resumen$texto[resumen$texto != ""][1],
    leido = Sys.time()
  )
}
