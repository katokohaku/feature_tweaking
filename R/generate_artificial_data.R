# generate artificial data: see plot
set.seed(42)

resol <- seq(-1, 1 ,by=1e-3)

x1 <- sample(resol, 1e3, replace = TRUE)
x2 <- sample(resol, 1e3, replace = TRUE)
x3 <- sample(resol, 1e3, replace = TRUE)
x4 <- sample(resol, 1e3, replace = TRUE)

y <- as.factor(ifelse((x1+0.6)^2 + (x2+0.6)^2 < 0.8^2, "in", "out"))

# x1 <- jitter(x1, factor=0, amount=0.1)
# x2 <- jitter(x2, factor=0, amount=0.1)
plot(x1~x2, col=y, pch=16)

dataset <- data.frame(x1,x2,x3,x4,y)
dataset <- rbind(dataset[dataset$y=="in", ], dataset[dataset$y=="out", ])
rownames(dataset) <- NULL

rm(x1,x2,x3,x4,y)
set.seed(NULL)