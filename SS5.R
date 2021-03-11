rm(list=ls())

require(qcc)
head(ss.data.pb1)

pb.groups.one <- with(ss.data.pb1, qcc.groups(pb.humidity,
                                              pb.group))
pb.xbar.one <- qcc(pb.groups.one, type="xbar.one")
summary(pb.xbar.one)

#Individual control chart- X-bar
plot(qcc(data = pb.groups.one,
         type = "xbar.one",
         center = pb.xbar.one$center,
         limits = pb.xbar.one$limits))

library(SixSigma)
with(ss.data.pb3,head(ss.data.pb3))

with(ss.data.pb3, plot(qcc(stockouts, orders, type ="p")) )

