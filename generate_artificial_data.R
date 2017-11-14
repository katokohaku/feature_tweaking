# generate artificial data
set.seed(42)

resol <- seq(-1, 1 ,by=1e-3)

x1 <- sample(resol, 1e3, replace = TRUE)
x2 <- sample(resol, 1e3, replace = TRUE)
x3 <- sample(resol, 1e3, replace = TRUE)
x4 <- sample(resol, 1e3, replace = TRUE)

y <- ifelse((x1-0.4)^2 + (x2-0.4)^2 < 0.4^2, "in", "out")
y <- as.factor(ifelse((x1+0.4)^2 + (x2+0.4)^2 < 0.4^2, "in", y))
x1 <- jitter(x1, factor=0, amount=0.05)
x2 <- jitter(x2, factor=0, amount=0.05)
plot(x1~x2, col=y, pch=16)

dataset <- data.frame(x1,x2,x3,x4,y)
set.seed(NULL)