context("Test that fastpca works.")

test_that("PCA works", {
    x <- matrix(data=rnorm(100), ncol=10, nrow=10)
    expect_silent(fastpca(x, k=4))
    expect_warning(fastpca(x, k=10),
                   "all singular values are requested")
    expect_error(fastpca(x, k=30))
})

context("Test that fastpca is equivalent to PCA.")

test_that("Fast PCA is equivalent to PCA", {
    x <- matrix(data=rnorm(100), ncol=10, nrow=10)
    pc1 <- fastpca(x, k=2)
    pc2 <- prcomp(t(x))

    expect_equivalent(abs(pc1), abs(pc2$x[,1:2]))
})
