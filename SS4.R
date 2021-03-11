rm(list=ls())
library(ggplot2)

pizzaDesign <- expand.grid(flour = gl(2, 1, labels = c("-",
                                                       "+")),
                           salt = gl(2, 1, labels = c("-", "+")),
                           bakPow = gl(2, 1, labels = c("-", "+")),
                           score = NA)

pizzaDesign$ord <- sample(1:8, 8)
pizzaDesign[order(pizzaDesign$ord),]

ss.data.doe1 <- data.frame(repl = rep(1:2, each = 8), rbind(pizzaDesign[, -6], pizzaDesign[, -6]))
ss.data.doe1$score <- c(5.33, 6.99, 4.23, 6.61, 2.26, 5.75, 3.26, 6.24, 5.7, 7.71, 5.13, 6.76, 2.79, 4.57, 2.48, 6.18)

aggregate(score ~ flour + salt + bakPow, FUN = mean, data = ss.data.doe1)

doe.model1 <- lm(score ~ flour + salt + bakPow + flour * salt + flour * bakPow + salt * bakPow + flour * salt * bakPow, data = ss.data.doe1)

summary(doe.model1)

doe.model2 <- lm(score ~ flour + bakPow, data = ss.data.doe1)
summary(doe.model2)

predict(doe.model2)

prinEf <- data.frame(Factor = rep(c("A_Flour", "C_Baking Powder"), each = 2), Level = rep(c(-1, 1), 2), Score = c(aggregate(score ~ flour, FUN = mean, data = ss.data.doe1)[,2], aggregate(score ~ bakPow, FUN = mean, data = ss.data.doe1)[,2]))
p <- ggplot(prinEf, aes(x = Level, y = Score)) + geom_point() + geom_line() +geom_hline(yintercept =mean(ss.data.doe1$score),linetype="dashed",
                                                                                        color = "blue")+scale_x_continuous(breaks = c(-1, 1)) + facet_grid(. ~ Factor)+ggtitle("Plot of Factor Effects")
print(p)

intEf <- aggregate(score ~ flour + bakPow, FUN = mean, data = ss.data.doe1)
q <- ggplot(intEf, aes(x = flour, y = score, color = bakPow )) + geom_point() + geom_line(aes(group=bakPow)) +geom_hline(yintercept =mean(ss.data.doe1$score),linetype="dashed",
                                                                                                                         color = "blue")+ggtitle("Interaction Plot")
print(q)

shapiro.test(residuals(doe.model2))

