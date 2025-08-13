#' @title Metric
#'
#' @description
#' Creates a Metric class
#'
#' @export
#'
Metric <- R6::R6Class(
  classname = "Metric",
  public = list(
    check = function(dt, validator, sequence) {
      checkmate::assertDataTable(dt)
      checkmate::assertDataTable(validator)
      checkmate::assertInteger(sequence, len = nrow(validator))
      dt[, timestamp := as.POSIXct(timestamp)]
      data.table::setorder(dt, timestamp)
      dt[, day := .GRP - 1L, by = .(data.table::yday(timestamp))]
      dt[, period := 1440L * day + 60L * data.table::hour(timestamp) + data.table::minute(timestamp)]
      dt[, day := NULL]
      validator <- data.table::copy(validator)
      validator[dt, value := i.value, on = .(period)]
      private$.dt <- validator[, .(value = sum(value)), by = .(period = sequence)]
      data.table::setkey(private$.dt, period)
      invisible(self)
    }
  ),
  active = list(
    dt = function() private$.dt
  ),
  private = list(
    .dt = NULL
  )
)
