# Test script: run with  Rscript test_indicators.R  (from this folder)
for (f in c("sma","ema","macd","stdev","linreg","rsi","stoch_rsi","crossover","crossunder")) source(paste0(f, ".R"))

ok <- function(name, cond) { cat(sprintf("%-38s %s\n", name, if (isTRUE(cond)) "PASS" else "FAIL")); if (!isTRUE(cond)) quit(status = 1) }
d <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

# SMA - compared with a hand-built reference (window means) and error handling
ok("sma values",        isTRUE(all.equal(sma(d, 3), c(37,47,53,60,65,71,70)/3)))
ok("sma length n-p+1",  length(sma(d, 4)) == length(d) - 4 + 1)
ok("sma error",         inherits(try(sma(1:2, 3), silent = TRUE), "try-error"))

# EMA - first value equals first data point; recursive formula
e <- ema(d, 3)
ok("ema first value",   e[1] == d[1])
ok("ema recursion",     isTRUE(all.equal(e[2], (12 - 10) * 0.5 + 10)))
ok("ema values",        isTRUE(all.equal(e, c(10,11,13,16.5,17.25,19.625,22.3125,23.15625,22.078125))))

# MACD - line = short - long; histogram = macd - signal
m <- macd(c(100,105,110,115,120,125,130), 3, 5, 2)
ok("macd names",        identical(names(m), c("macd_line","signal_line","histogram")))
ok("macd identity",     isTRUE(all.equal(m$histogram, m$macd_line - m$signal_line)))
ok("macd last value",   isTRUE(all.equal(m$macd_line[7], 4.2002100, tolerance = 1e-6)))

# stdev - compared with base R's sd() rescaled to the population formula
ok("stdev vs sd()",     isTRUE(all.equal(stdev(d), sd(d) * sqrt((length(d)-1)/length(d)))))
ok("stdev constant = 0", stdev(c(5,5,5,5)) == 0)

# linreg - compared with lm() on the same window; offset/length errors
l <- linreg(d, 5, 0)
w <- d[max(1, length(d) - 5 + 0):min(length(d), length(d) - 0)]
fit <- lm(w ~ seq_along(w))
ok("linreg slope vs lm",     isTRUE(all.equal(l$slope, unname(coef(fit)[2]))))
ok("linreg intercept vs lm", isTRUE(all.equal(l$intercept, unname(coef(fit)[1]))))
ok("linreg perfect line",    isTRUE(all.equal(linreg(c(2,4,6,8,10), 3, 0)$slope, 2)))
ok("linreg length error",    inherits(try(linreg(1:3, 5, 0), silent = TRUE), "try-error"))
ok("linreg offset error",    inherits(try(linreg(1:9, 3, 3), silent = TRUE), "try-error"))

# RSI - bounded 0..100, NA warm-up, known values
r <- rsi(c(45,50,48,55,52,49,58,60,65,62), 5)
ok("rsi warm-up NA",    all(is.na(r[1:5])))
ok("rsi range",         all(r[!is.na(r)] >= 0 & r[!is.na(r)] <= 100))
ok("rsi values",       isTRUE(all.equal(r[6:10], c(50.52632,68.92562,71.83521,78.21079,66.85966), tolerance = 1e-6)))
ok("rsi rising = 100",  all(rsi(1:20, 5)[6:20] == 100))

# StochRSI - K in [0,1]; same warm-up
s <- stoch_rsi(c(45,50,48,55,52,49,58,60,65,62), 5, 3, 3)
ok("stoch_rsi K range", all(s$k_line[!is.na(s$k_line)] >= 0 & s$k_line[!is.na(s$k_line)] <= 1))
ok("stoch_rsi list",    identical(names(s), c("k_line","d_line")))
ok("stoch_rsi short data (period 14)", all(is.na(stoch_rsi(c(45,50,48,55,52,49,58,60,65,62), 14, 3, 3)$k_line)))

# Crossover / Crossunder - the assignment's example arrays
a1 <- c(10,12,15,20,18,22,25,24,21); a2 <- c(18,20,22,18,15,12,10,11,13)
co <- crossover(a1, a2); cu <- crossunder(a1, a2)
ok("crossover Up at index 4", co[4] == "Up" && sum(co == "Up") == 1)
ok("crossover length",        length(co) == length(a1))
ok("crossunder none here",    !any(cu == "True"))
x1 <- c(5, 3, 1); x2 <- c(2, 2, 2)                    # arr1 falls through arr2
ok("crossunder True at 3",    crossunder(x1, x2)[3] == "True")
ok("crossover Down at 3",     crossover(x1, x2)[3] == "Down")
ok("crossover length error",  inherits(try(crossover(1:3, 1:4), silent = TRUE), "try-error"))
cat("\nAll tests passed.\n")
