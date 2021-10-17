test_that("Default transformation works.", {
    expect_equal(year_iso(0:99),
                 c(2000:2050,
                   1951:1999))
})




test_that("Non-default transformation works.", {
    expect_equal(year_iso(0:99,
                          boundary = 20,
                          century = 1400),
                 c(1500:1520,
                   1421:1499))
})
