## multivariate Rsq and adjusted Rsq
##

#' Multiple correlation coefficient methods
#'
#' Calculates R^2, by dafault adjusted, for basic models
#'
#' @param model The fitted model object
#' @param adjust logical: should the value be bias adjusted?
#' @param ... currently ignored, may be used in future methods
#'
#' @return Multiple correlation estimates as required
#' @export
#'
#' @examples
#' .fm <- lm(medv ~ ., Boston)
#' c(Rsq(.fm), summary(.fm)$adj.r.squared)
#' c(Rsq(.fm, adjust = FALSE), summary(.fm)$r.squared)
#' .tst <- glm.nb(Days ~ Sex/(Age + Eth*Lrn), quine)
#' Rsq(.tst)
#' Rsq(.tst, FALSE)
#' rm(.fm, .tst)
Rsq <- function(model, ...) {
  UseMethod("Rsq")
}

#' @rdname Rsq
#' @export
Rsq.lm <- function(model, adjust = TRUE, ...) {
  R2 <- with(model, cor(fitted.values, fitted.values + residuals))^2
  if(adjust) {
    R2 <- 1 - (1 - R2)*(nobs(model) - 1)/df.residual(model)
  }
  R2
}

#' @rdname Rsq
#' @export
Rsq.mlm <- function(model, adjust = TRUE, ...) {
  R2 <- with(model, diag(cor(fitted.values, fitted.values + residuals)))^2
  if(adjust) {
    R2 <- 1 - (1 - R2)*(nobs(model) - 1)/df.residual(model)
  }
  R2
}

#' @rdname Rsq
#' @export
Rsq.glm <- function(model, adjust = TRUE, ...) {
  1 - with(model, if(adjust) {
    (deviance/df.residual)/(null.deviance/df.null)
  } else {
    deviance/null.deviance
  })
}

