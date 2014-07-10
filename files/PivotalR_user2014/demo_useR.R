library(PivotalR)

db.connect(port = 5333, dbname = 'madlib')

db.list()

db.objects("abalone")

.db("select version()")

.db("select madlib.version()")

## A simple GUI using Shiny
pivotalr()

## wrapper of the data table
x <- db.data.frame("abalone")

dim(x) # dimension of the data table

names(x) # column names of the data table

madlib.summary(x) # look at a summary for each column

lk(x, 20) # look at a sample of the data

## ==================================================

.db("select * from abalone limit 10")

## look at a sample sorted by id column
lookat(sort(x, decreasing = FALSE, x$rings), 20)
lookat(sort(x, FALSE, NULL), 20) # look at a sample ordered randomly

## linear regression Examples --------
## fit one different model to each group of data with the same sex
fit1 <- madlib.lm(rings ~ . - id | sex, data = x)

fit1 # view the result

lookat(mean((x$rings - predict(fit1, x))^2)) # mean square error

## plot the predicted values v.s. the true values
ap <- x$rings # true values
ap$pred <- predict(fit1, x) # add a column which is the predicted values

## If the data set is very big, you do not want to load all the
## data points into R and plot. We can just plot a random sample.
random.sample <- lk(sort(ap, FALSE, "random"), 1000) # sort randomly
plot(random.sample) # plot a random sample

## Group by Examples --------
## mean value of each column except the "id" column
lk(by(x[,-1], x$sex, mean))

## standard deviation of each column except the "id" column
lookat(by(x[,-1], x$sex, sd))

## Merge Examples --------
## create two objects with different rows and columns
key(x) <- "id"
y <- x[1:300, 1:6]
z <- x[201:400, c(1,2,4,5)]

## get 100 rows
m <- merge(y, z, by = c("id", "sex"))

lookat(m, 20)

## operator Examples --------
y <- x$length + x$height + 2.3
z <- x$length * x$height / 3

lk(y < z, 10)

## Formula and categorical variables

fit <- madlib.glm(rings < 10 ~ . - id - sex, data = x, family = "logistic")

fit

fit <- madlib.glm(rings < 10 ~ . - id | sex, data = x, family = "logistic")

fit

fit <- madlib.glm(rings < 10 ~ . - id, data = x, family = "logistic")

fit

margins(fit)

## ===================================================

## Quickly create prototypes that can run in Database
## normal R script, computation runs in memory
linregr.in.mem <- function (x, y)
{
    a <- crossprod(x)
    b <- crossprod(x, y)
    solve(a) %*% b
}

## data in memory
dat <- lk(x, -1)
linregr.in.mem(as.matrix(cbind(1, dat[,-c(1,2,10)])), dat$rings)

## ------------------------------------------------
## PivotalR script, computation runs in database
linregr <- function (x, y)
{
    a <- crossprod(x)
    b <- crossprod(x, y)
    solve(lookat(a, "all")) %*% lookat(b, "all")
}

linregr(db.array(1, x[,-c(1,2,10)]), x$rings)

## =============================================================

## Compute all eigenvectors
## Can be used for tables with features < 1000, similar limitation as
## madlib.linregr
pca <- function (x, center = TRUE, scale = FALSE)
{
    ## centering and scaling
    y <- scale(x, center = center, scale = scale)
    ## create an intermediate table to speed up computation
    z <- as.db.data.frame(y, verbose = FALSE)
    m <- lk(crossprod(z)) # one scan of the table to compute Z^T * Z
    d <- delete(z) # delete the intermediate table
    res <- eigen(m) # only this computation is in R, size of m is small enough
    n <- attr(y, "row.number") # save the computation to count rows

    ## return the result
    list(val = sqrt(res$values/(n-1)), # eigenvalues
         vec = res$vectors, # columns of this matrix are eigenvectors
         center = attr(y, "scaled:center"),
         scale = attr(y, "scaled:scale"))
}

y <- db.data.frame("madlibtestdata.pca_mat_600_100", conn.id = 1)

q <- pca(y[,-1])

q$val # eigenvalues

## ===============================================================

## poisson regression

poisson <- function(x, y, init = rep(0, 4)) {
    fn <- function(beta) {
        f <- y*rowSums(beta*x) - exp(rowSums(beta*x))
        lk(mean(f))
    }
    gr <- function(beta) {
        g <- (y - exp(rowSums(beta*x))) * x
        lk(mean(g))
    }
    ## optimx has more optimization methods
    ## including those that use Hessian
    ## Then we need a third function "he" for computing hessian
    optim(init, fn, gr, method = "L-BFGS-B",
          control = list(fnscale = -1, trace = 0),
          hessian = FALSE) # do not compute the final Hessian
}

## ----------------------------------------------------------------------

## twice faster
poisson.faster <- function(x, y, init = rep(0,4))
{
    compute <- function(beta) {
        f <- y*rowSums(beta*x) - exp(rowSums(beta*x))
        g <- (y - exp(rowSums(beta*x))) * x
        lk(mean(cbind(f, g)))
    }

    res <- numeric(0)

    fn <- function(beta) {
        res <<- compute(beta)
        res[1]
    }

    gr <- function(beta) res[-1]

    optim(init, fn, gr, method = "L-BFGS-B",
          control = list(fnscale = -1, trace = 0),
          hessian = FALSE)
}

## ----------------------------------------------------------------------

## Full version, Run this
poisson.regr <- function(x, y)
{
    compute <- function(beta) {
        f <- y*rowSums(beta*x) - exp(rowSums(beta*x))
        g <- (y - exp(rowSums(beta*x))) * x
        lk(mean(cbind(f, g)))
    }

    res <- numeric(0)

    fn <- function(beta) {
        res <<- compute(beta)
        res[1]
    }

    gr <- function(beta) res[-1]

    ## trace = 5 prints useful information
    ## hessian is computed using finite difference, and expensive to compute
    res <- optim(rnorm(length(names(x)), 0, 0.01), fn, gr, method = "L-BFGS-B",
                 control = list(fnscale = -1, trace = 5), hessian = FALSE)

    coef <- res$par

    ## But we know the exact formula of hessian.
    ## Use crossprod operator again.
    ## Restriction: The Hessian matrix must fit in memory.
    hessian <- lk(crossprod(-x * exp(rowSums(coef*x)), x))
    std.err <- sqrt(diag(solve(-hessian)))

    z <- coef / std.err
    p <- 2*(1 - pnorm(abs(z)))

    data.frame(Estimate = coef, `Std. Error` = std.err,
               `z value` = z, `Pr(>|t|)` = p, check.names = FALSE)
}


g <- poisson.regr(cbind(1, x[,c("length", "diameter", "shell")]), x$rings)

rownames(g) <- c("(Intercept)", "length", "diameter", "shell")
printCoefmat(g)

## =========================================================================

## Left inverse of a Matrix
A <- x[ , -c(1,2)]

lk(A, 10)

iaa <- solve(lk(crossprod(A))) # inverse of (A^T A)

iaa

## (A^T A)^(-1) A^T
z <- Reduce(cbind, Map(function(i) rowSums(iaa[i,] * A), 1:nrow(iaa)))

names(z) <- paste0("val", seq(ncol(A)))

lk(z, 10)

## Verify that this is the inverse
lk(crossprod(z, A))

## Store the db.Rquery into a table in the database
delete("A_left_inverse")
as.db.data.frame(z, "A_left_inverse")

## ===========================================================================

## Also AdaBoost
## Adaboost function. Use the algorithm on pg. 339 of "The Elements of
## Statistical Learning (2nd)" (a little different from the Wiki page)
adaboost <- function(x, y, id, maxit=100, weak.learner = logit, method = "L-BFGS-B")
{
    n <- nrow(x)
    dat <- id
    dat$w <- 1/n
    w <- dat$w

    alpha <- rep(0, maxit)
    ep <- rep(0, maxit)
    models <- list()
    for (i in 1:maxit) {
        old.dat <- dat
        train <- merge(cbind(x, y, id), dat, by = "id")
        x1 <- train[,names(x)]
        y1 <- train[,names(y)]
        dat <- train[,names(dat)]
        w <- dat$w
        g <- weak.learner(x1, y1, w) # in database
        models[[i]] <- g
        p <- predict(g, x1, type = "response") # in database
        ep[i] <- lk(sum(w * as.integer(y1 != p)))
        print(ep[i])
        alpha[i] <- log((1-ep[i])/ep[i])
        w <- w * exp(alpha[i] * as.integer(y1 != p)) # weight as a db.Rquery object
        dat$w <- w / lk(sum(w))
        dat <- as.db.data.frame(dat, verbose = FALSE)
        delete(old.dat)
    }

    return (list(models = models, alpha = alpha, error = ep))
}



