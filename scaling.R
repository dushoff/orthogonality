set.seed(281)
l <- 20

x <- rlnorm(l)
y <- rlnorm(l)
z <- rlnorm(l)

mean(x)
sd(x)

m <- model.matrix( ~ x+y)

f <- lm.fit(m, z)
c <- f$coef

sm <- matrix(
	c(
		1, mean(x), mean(y)
		, 0, sd(x), 0
		, 0, 0, sd(y)
	)
	, nrow=3, byrow=TRUE
)

print(sm)
smInv <- solve(sm)
print(smInv)

sm %*% smInv

ms <- model.matrix( ~ scale(x)+scale(y))

fs <- lm.fit(ms, z)
cs <- fs$coef
print(cs)

print(sm %*% c)

print(c)
