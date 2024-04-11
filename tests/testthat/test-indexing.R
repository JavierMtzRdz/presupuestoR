# Toy dataset
set.seed(545)

df <- data.frame(date = seq(as.Date("2023-01-01"),
                        as.Date("2025-12-01"),
                        "month"),
             values = runif(12*3, 50, 150)*
               (2 + cumsum(runif(12*3, 0, 0.15))))

# indexing() test

test_that("Testing indexing() function", {
  # This test verifies that the returned data frame contains the right information.
  indexing(df,
           values,
           date == as.Date("2023-01-01")) %>%
    expect_s3_class("data.frame") %>%
    expect_length(3) %>%
    .$index %>%
    round(2) %>%
    expect_equal(c(100.00, 126.03, 112.95, 91.23, 72.75, 127.58, 69.83, 110.30, 89.48, 150.21, 64.93, 169.35, 160.01, 146.73, 141.48, 163.35, 135.19, 158.06, 187.77, 98.58, 144.43, 220.37, 81.46, 148.74, 219.24, 229.90, 221.50, 127.25, 163.00, 230.75, 157.80, 180.81, 212.09, 183.55, 105.85, 100.87))
  # This test evaluates the error when a non numeric variable is provided
  expect_error(indexing(df,
                        date,
                        date == as.Date("2023-01-01")),
               "'date' is not a numeric variable.")
  # This test evaluates the error when reference is not found
  expect_error(indexing(df,
                        values,
                        date == as.Date("2021-01-01")),
               "Reference not found.")
  # This test evaluates the warning when More than one row is being used as a reference period.
  expect_warning(indexing(df,
                          values,
                          (format(date, format = "%Y")) == "2023"),
                 "More than one row is being used as a reference.")

})


rm('df')
