# Modelos de pronóstico de contratos mensuales en SECOP II.
#
# Es el mismo modelo del observatorio (github.com/Forero25/observatorio-secop, modelo/pronostico.R):
# regresión sobre el logaritmo de la serie con un efecto por mes, errores ARIMA(0,1,1) y, en el
# modelo elegido, dos regresores del calendario electoral. Solo usa R base.

MESES_RESTRICCION <- 2:6   # Ley de Garantías: de febrero a junio del año de elección presidencial

MODELOS <- c(
  "Ingenuo estacional",
  "Estacional",
  "Estacional con calendario electoral"
)

sumar_meses <- function(fecha, k) seq(fecha, by = "month", length.out = k + 1)[k + 1]

regresores <- function(fechas, anios_eleccion) {
  anio <- as.integer(format(fechas, "%Y"))
  mes <- as.integer(format(fechas, "%m"))
  cbind(
    enero_eleccion = as.numeric(anio %in% anios_eleccion & mes == 1),
    restriccion = as.numeric(anio %in% anios_eleccion & mes %in% MESES_RESTRICCION)
  )
}

efectos_mes <- function(fechas) {
  mes <- as.integer(format(fechas, "%m"))
  matrix(sapply(2:12, function(k) as.numeric(mes == k)), ncol = 11,
         dimnames = list(NULL, paste0("mes_", 2:12)))
}

# Devuelve media y desviación del pronóstico en logaritmos, y los coeficientes si los hay.
ajustar <- function(modelo, y, fechas, h, anios_eleccion) {
  if (modelo == "Ingenuo estacional") {
    n <- length(y)
    res <- y[13:n] - y[1:(n - 12)]
    return(list(media = y[n - 12 + ((seq_len(h) - 1) %% 12) + 1], sd = rep(sd(res), h), coef = NULL))
  }
  futuras <- seq(sumar_meses(fechas[length(fechas)], 1), by = "month", length.out = h)
  xreg <- efectos_mes(fechas)
  newxreg <- efectos_mes(futuras)
  if (modelo == "Estacional con calendario electoral") {
    xreg <- cbind(xreg, regresores(fechas, anios_eleccion))
    newxreg <- cbind(newxreg, regresores(futuras, anios_eleccion))
  }
  usar <- colSums(xreg) > 0   # un regresor que nunca vale 1 en el entrenamiento no se puede estimar
  xreg <- xreg[, usar, drop = FALSE]
  newxreg <- newxreg[, usar, drop = FALSE]
  for (metodo in c("ML", "CSS-ML", "CSS")) {
    m <- tryCatch(arima(y, order = c(0, 1, 1), xreg = xreg, method = metodo),
                  error = function(e) NULL, warning = function(w) NULL)
    if (!is.null(m)) {
      p <- predict(m, n.ahead = h, newxreg = newxreg)
      return(list(media = as.numeric(p$pred), sd = as.numeric(p$se), coef = coef(m),
                  se_coef = sqrt(diag(m$var.coef))))
    }
  }
  stop("El modelo no convergió con ningún método de estimación.")
}

# Pronóstico en contratos, con intervalo del nivel pedido.
pronosticar <- function(modelo, serie, h, anios_eleccion, nivel = 0.8) {
  z <- qnorm(1 - (1 - nivel) / 2)
  p <- ajustar(modelo, log(serie$contratos), serie$fecha, h, anios_eleccion)
  fechas <- seq(sumar_meses(serie$fecha[nrow(serie)], 1), by = "month", length.out = h)
  list(
    tabla = data.frame(fecha = fechas, pronostico = exp(p$media),
                       inferior = exp(p$media - z * p$sd), superior = exp(p$media + z * p$sd)),
    coef = p$coef, se_coef = p$se_coef
  )
}
