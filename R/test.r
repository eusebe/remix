options(width = 100)
data <- data.frame(num1 = rnorm(10),
                   num2 = rnorm(10, 2),
                   num3 = rnorm(10, 3),
                   num4 = rnorm(10, 4),
                   fac1 = sample(rep(LETTERS[3:4], length = 10)),
                   fac2 = sample(rep(LETTERS[5:6], length = 10)),
                   fac3 = sample(rep(LETTERS[7:8], length = 10)), 
                   fac4 = sample(rep(LETTERS[9:10], length = 10)))
data

mix(num1 ~ ., data)
mix(num1 + num2 ~ ., data)
mix(c(num1, num2, num3, num4) ~ ., data)

data2 <- data
data2[1, 1] <- NA
mix(num1 ~ ., data2, na.rm = F)
mix(num1 ~ ., data2, na.rm = T)

mix(. ~ num1, data)
mix(. ~ num1 + num2, data)
mix(. ~ c(num1, num2), data)

mix(num1 ~ ., data, funs = c(median, mad, quantile))
mix(num1 ~ ., data, funs = quantile, probs = c(1/3, 2/3))

mix(num1 ~ fac1, data)
mix(num1 + num2 ~ fac1, data)
mix(c(num1, num2, num3, num4) ~ fac1, data)

mix(fac1 ~ num1, data)
mix(fac1 ~ num1 + num2, data)
mix(fac1 ~ c(num1, num2), data)

mix(fac1 ~ num1 + ., data)
mix(num1 + . ~ fac1, data)

mix(fac1:fac2 ~ c(num1, num2), data)

data$num5 <- rep(1:2)
mix(num1 ~ factor(num5), data)
mix(num1 + num2 ~ factor(num5):fac1, data)

mix(num1 ~ fac1 + fac2, data)
mix(num1 + num2 ~ fac1 + fac2, data)

mix(fac1 + fac2 ~ num1, data)
mix(fac1 + fac2 ~ num1 + num2, data)

mix(fac1 ~ ., data)
mix(fac1 + fac2 ~ ., data)
mix(. ~ fac1, data)
mix(. ~ fac1 + fac2, data)

mix(fac1 ~ ., data, cum = T)

mix(. ~ c(fac1, fac2), data)
mix(c(fac1, fac2) ~ ., data, cum = T)

mix(fac1 ~ fac2, data)
mix(fac1 ~ fac2 + factor(num5), data)
mix(fac1 + fac2 ~ fac3 + fac4, data, margin = 0, useNA = "always")

mix(num1 ~ num2, data)
mix(num1 ~ num2, data, method = "spearman")

mix(num1 ~ num2 + num3, data)

mix(c(num1, num2) ~ num3, data)
mix(c(num1, num2) ~ c(num3, num4), data)

mix(fac1 ~ ..., data)

mix(fac1 ~ fac2 | fac3, data)
mix(fac1 ~ fac2 | fac3 + fac4, data)
mix(fac1 ~ fac2 | fac3:fac4, data)
mix(c(num1, num2, num3) ~ fac1 | fac2 + fac3, data)
mix(c(num1, num2, num3) ~ fac1 | c(fac2, fac3), data)

mix(c(fac1, factor(num5)) ~ c(num3, I(num4^3)), data)

data2 <- data[, c("fac1", "fac2", "fac3")]
mix(fac1 ~ . | ..., data2)
