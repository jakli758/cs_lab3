# a. 
data <- read.table("lab5/kresseertrag.dat", header=FALSE)

#model <- lm(V3~V2+I(V2^2)+I(V2^3), data=data)
model <- lm(V3~I(V2^2)+I(V2^3), data=data)

summary(model)

# b. 
model$coefficients
confint.lm(model, level=0.95)

plot(data$V2, data$V3, main="Regression fitted using lm", ylab="yield", xlab="concentration of fertilizer")
x <- seq(0,1.2,0.01)
df_x <- data.frame(V2=x)
y <- predict(model, newdata=df_x)
lines(x,y, col="red", lwd=2)

# c. 

# adjusted code from lecture slides

bo <- 10000 
bs <- numeric(length(bo)) 
for (l in 1:bo){
  x  <- data[sample(nrow(data), replace=TRUE), ]
  m <- lm(V3~V2+I(V2^2)+I(V2^3), data=x) 
  bs <- c(bs, m$coefficients[2]) # getting coefficient for beta1
}
hist(bs)
bss <- sort(bs)
ci95 <- c(bss[round(bo*0.025)], bss[round(bo*0.975)])
ci95


# d. 
fert <- function(data, i){
  m <- lm(V3~V2+I(V2^2)+I(V2^3), subset=i,data=data)
  m$coefficients[2]
}

cb <- boot(data, fert, R=10000)

ci_percentile <- boot.ci(cb, type = "perc")
ci_bca <- boot.ci(cb, type = "bca")

ci_percentile
ci_bca


# e.


