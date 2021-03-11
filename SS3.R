rm(list=ls())

library("SixSigma")
with(ss.data.pc.r,
     barplot(pc.def,
             names.arg = pc.regions,
             las = 2,
             main = "Barplot of defects by region",
             sub = "Printer cartridge example"))

hist(ss.data.pc$pc.volume,
     main="Printer Cartridge Volume",
     xlab="Volume",
     col="#DDDDDD")

hist(ss.data.pc$pc.volume,
     main = "Printer Cartridge Volume",
     xlab = "Volume",
     col = "#BBBBBB",
     border = "white",
     bg = "red",
     freq = FALSE,
     ylim = c(0,0.4))
curve(dnorm(x,16,1),
      add = TRUE,
      lty = 2,
      lwd = 2)
lines(density(ss.data.pc$pc.volume),
      lwd = 2)

plot(pc.volume ~ pc.density,
     data = ss.data.pc,
     main = "Searching correlation between Density
and Volume",
     col = "#666666",
     pch = 16,
     sub = "Printer Cartridge Example",
     xlab = "Volume of Ink",
     ylab = "Density")


boxplot(pc.volume ~ pc.filler,
        data = ss.data.pc,
        col = "#CCCCCC",
        main = "Box Plot of Volume by Filler",
        sub = "Printer Cartridge Example",
        xlab = "Filler",
        ylab = "Volume")


# Example: Hypothesis test to verify if the length of the strings is different from the target value of 950 mm:H0 : μ = 950,H1 : μ not equal to 950.

t.test(ss.data.strings$len,
       mu = 950,
       conf.level = 0.95)

data.E1 <- ss.data.strings$len[ss.data.strings$type == "E1"]
data.E6 <- ss.data.strings$len[ss.data.strings$type == "E6"]
t.test(data.E1, data.E6)

var.test(data.E1, data.E6)

defects <- data.frame(type = ss.data.strings$type, res = ss.data.strings$res < 3)
defects <- aggregate(res ~ type, data = defects, sum)
prop.test(defects$res, rep(20,6))

shapiro.test(ss.data.strings$len)

power.t.test(delta = 0.1, power = 0.9, sig.level = 0.05,
             sd = sd (ss.data.strings$len))

