## (S-13) Random starting points for optimization on the surrogate
## نقاط شروع تصادفی برای بهینه‌سازی روی مدل جایگزین
x0 <- getMultiStartPoints(x, y, control)
resSurr <- matrix(NA, nrow = nrow(x0), ncol = ncol(x0) + 1)

## (S-14b) Search on the surrogate with starting point/s x0:
## جستجو روی مدل جایگزین با نقطه/نقاط شروع x0:
for (i in 1:nrow(x0)) {
  optimResSurr <- control$optimizer(
    x = x0[i, , drop = FALSE],
    funSurrogate,
    lower,
    upper,
    control$optimizerControl
  )
  resSurr[i, ] <- c(optimResSurr$xbest, optimResSurr$ybest)
}

## (S-15) Compile surrogate results:
## گردآوری نتایج مدل جایگزین:
m <- which.min(resSurr[, ncol(x) + 1])

## Determine xnew based on multi start results
## تعیین xnew بر اساس نتایج شروع چندگانه
xnew <- resSurr[m, 1:ncol(x), drop = FALSE]

## value on the surrogate (can be "y", "s2", "ei", "negLog10ei" etc.)
## مقدار روی مدل جایگزین (می‌تواند "y"، "s2"، "ei"، "negLog10ei" و غیره باشد)
ySurrNew <- resSurr[m, ncol(x) + 1]

## (S-16) Duplicate handling:
##  مدیریت موارد تکراری:
xnew <- duplicateAndReplicateHandling(xnew, x, lower, upper, control)

# Repair non-numeric results
# تعمیر نتایج غیرعددی
xnew <- repairNonNumeric(xnew, control$types)
