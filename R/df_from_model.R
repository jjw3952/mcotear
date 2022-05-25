#' Creates a data.frame from a model
#'
#' \code{df_from_model} creates and returns a data.frame from a
#' model object such as a(n): \code{\link[stats]{aov}}, 
#' \code{\link[stats]{lm}}, or \code{\link[stats]{glm}} object.
#' The return data.frame will contain two columns, one for
#' residuals ('res'), and a second for fitted values ('fit').
#'
#' @param model A model object such as a(n): \code{\link[stats]{aov}}, 
#'   \code{\link[stats]{lm}}, or \code{\link[stats]{glm}} object, that
#'   is compatible with \code{\link[stats:fitted.values]{fitted}} and
#'   \code{\link[stats]{residuals}} methods.
#' @param type The type of residuals to be returned in the data.frame.
#'   The residual type supplied must match a function name, for example:
#'   \code{\link[stats]{residuals}}, \code{\link[stats:influence.measures]{rstandard}},
#'   and \code{\link[stats:influence.measures]{rstudent}}. Values can be supplied as quoted or
#'   unquoted elements, for example: "rstandard" or rstandard.
#'
#' @return The output will be a two-column \code{\link[base]{data.frame}}
#'   with columns 'res' and 'fit' containing the residuals and fitted values.
#'
#' @seealso  \link[stats]{plot.lm}, \link{residual_plots},
#'   \link{gg_residual_plots}
#'
#' @examples
#' data(npk)
#' npk.aov <- aov(yield ~ block + N*P*K, npk)
#' summary(npk.aov)
#' npk.aov1 <- aov(yield ~ block + N + K, data = npk)
#' summary(npk.aov1)
#' summary.lm(npk.aov1)
#' model.tables(npk.aov1, type = "means", se = TRUE)
#' plot(npk.aov1)
#'
#' npk.table <- df_from_model(npk.aov1, type = "rstandard")
#' df_from_model(npk.aov1, type = rstandard)
#' df_from_model(npk.aov1)
#' 
#' df_from_model(npk.aov1, type = "rstudent")
#' df_from_model(npk.aov1, type = "residuals")
#' df_from_model(npk.aov1, type = "resid")
#' 
#' \dontrun{
#' df_from_model(npk.aov1, resid)
#' }
#' 
#' @export

df_from_model <- function(model, type = "rstandard"){

  res <- do.call(what = type, list(model))
  fit <- fitted(model) 

  return(data.frame(res, fit))
}