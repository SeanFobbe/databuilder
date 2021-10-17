


test_that("Replacement works.", {
    expect_equal(vec_empty_NA(c("", "", "")),
                 c("NA", "NA", "NA"))
})
