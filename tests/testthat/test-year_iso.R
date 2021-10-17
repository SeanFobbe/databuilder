test_that("Default transformation works.", {
    expect_equal(year_iso(c(11,
                            34,
                            67,
                            99)),
                 c(2011,
                   2034,
                   1967,
                   1999))
})
