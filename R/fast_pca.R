#' Fast PCA
#'
#' This function implements a fast version of PCA
#'
#' @importFrom rARPACK svds
#' @export
#'
#' @param expr numeric matrix with data.
#' @param scale logical. Whether to scale the data.
#' @param k numeric. The number of components to compute.
#'
#' @return numeric matrix with the principal components.
#'
#' @examples
#' x <- matrix(data = rnorm(30), nrow=10, ncol=3)
#' pca <- fastpca(x, k=2)
fastpca <- function(expr, scale=FALSE, k=50) {
  svd_raw <- svds(scale(t(expr), center=TRUE, scale=scale), k=k, nu=k, nv=0)
  pc_raw <- svd_raw$u %*% diag(svd_raw$d[1:k])
  return(pc_raw)
}
