# logit ----
expect_equal(logit(0.5), 0)
expect_equal(logit(0), -Inf)
expect_equal(logit(1), Inf)
expect_warning(logit(-1))
expect_warning(logit(2))

# expit ----
expect_equal(expit(0), 0.5)
expect_equal(expit(-Inf), 0)
expect_equal(expit(Inf), 1)

# cumulative_logit ---
expect_equal(cumulative_logit(c(0.5, 0.5)), 0)
expect_equal(cumulative_logit(c(1 / 3, 1 / 3, 1 / 3)), logit(c(1 / 3, 2 / 3)))

# cumulative_expit ---
expect_equal(cumulative_expit(0), c(0.5, 0.5))
expect_equal(cumulative_expit(logit(c(1 / 3, 2 / 3))), rep(1 / 3, 3))
