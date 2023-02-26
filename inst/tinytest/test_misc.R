expect_equal(odds_ratio_to_p(0.5, 1), 0.5)
expect_equal(odds_ratio_to_p(0.25, 3), 0.5)
expect_equal(odds_ratio_to_p(0.75, 1 / 3), 0.5)
