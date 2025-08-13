#' @title Model
#'
#' @description
#' Creates a Model class
#'
#' @export
#'
Model <- R6::R6Class(
  classname = "Model",
  public = list(
    normalize = function(dt, period, quantiles = c(0.1, 0.9)) {
      checkmate::assertDataTable(dt)
      checkmate::assertInteger(period, lower = 1L)
      checkmate::assertNumeric(quantiles, len = 2L, lower = 0, upper = 1, sorted = TRUE)
      private$.dt <- data.table::copy(dt)
      private$.dt[, group := period %% period]
      index <- private$.dt[, .I[which.min(value)], by = .(group)]$V1
      private$.dt[index, value := NA_integer_]
      dt_stats <- private$.dt[, .(value = round(quantile(value, probs = quantiles[1], na.rm = TRUE))), by = .(group)]
      private$.dt[is.na(value), value := dt_stats[.SD, value, on = .(group)]]
      index <- private$.dt[, .I[which.max(value)], by = .(group)]$V1
      private$.dt[index, value := NA_integer_]
      dt_stats <- private$.dt[, .(value = round(quantile(value, probs = quantiles[2], na.rm = TRUE))), by = .(group)]
      private$.dt[is.na(value), value := dt_stats[.SD, value, on = .(group)]]
      private$.dt[, group := NULL]
      private$.serie <- stats::ts(private$.dt$value, start = c(1L, 1L), frequency = period)
      invisible(self)
    },
    train = function(k = 4L) {
      checkmate::assertInt(k, lower = 1L, upper = 5L)
      if (checkmate::testNull(private$.serie)) stop("Normalize the values first")
      fourier <- forecast::fourier(private$.serie, K = k)
      private$.model <- forecast::tslm(private$.serie ~ fourier)
      invisible(self)
    },
    predict = function(period, level = 99L, k = 4L) {
      checkmate::assertInt(period, lower = 1L)
      checkmate::assertInt(level, lower = 80L, upper = 99L)
      checkmate::assertInt(k, lower = 1L, upper = 5L)
      if (checkmate::testNull(private$.model)) stop("Train the model first")
      fourier <- data.frame(values = forecast::fourier(private$.serie, K = k, h = period))
      linear <- forecast::forecast(private$.model, newdata = fourier, level = level)
      private$.prediction <- sapply(c("lower", "mean", "upper"), function(x) {
        values <- linear[[x]]
        values[values < 0] <- 0L
        values
      }, simplify = FALSE)
      invisible(self)
    }
  ),
  active = list(
    dt = function() private$.dt,
    serie = function() private$.serie,
    model = function() private$.model,
    prediction = function() private$.prediction
  ),
  private = list(
    .dt = NULL,
    .serie = NULL,
    .model = NULL,
    .prediction = NULL
  )
)
