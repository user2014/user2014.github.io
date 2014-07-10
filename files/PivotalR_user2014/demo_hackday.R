library(PivotalR)

db.connect(port = 5333, dbname = 'madlib')

# -------------------------------------------

tree <- function(label, length, diameter,
                 shell)
{
    declare(label = 'boolean[]',
            length = 'float8[]',
            diameter = 'float8[]',
            shell = 'float8[]',
            bytea)
    library(rpart)
    model <- rpart(label ~ length + diameter
                   + shell)
    return (serialize(model, NULL))
}

plr.tree <- plr(tree)

# -------------------------------------------

tree.pred <- function(result, length,
                      diameter, shell)
{
    declare(result = bytea,
            length = 'float8[]',
            diameter = 'float8[]',
            shell = 'float8[]',
            'float8[]')
    model <- unserialize(result)
    data <- data.frame(length = length,
                       diameter = diameter,
                       shell = shell)

    pred <- predict(model, data)
    return (pred)
}

plr.pred <- plr(tree.pred)

# -------------------------------------------

dat <- db.data.frame('abalone')
dat$label <- dat$rings < 10
grp.dat <- by(dat, dat$id %% 3, colAgg)

model <- plr.tree(grp.dat)

# -------------------------------------------

dat1 <- colAgg(dat[dat$sex == "M", ])
pred.data <- merge(model, dat1, by = NULL)

pred <- plr.pred(pred.data)

p <- lk(pred)

dim(p)

hist(p, main = "Bagging Decision Tree Prediction")

result <- apply(p, 2, function(v) sum(v) >= 2)

result[]

