rm(list=ls())

# Create gage R and R data for 3 piston rings , 2 operators and each operator 3 measurements per piston
Operator<- factor(rep(1:2, each = 9))
Pistonring<- factor(rep(rep(1:3, each = 3), 2))
run<- factor(rep(1:3, 6))
diameter<-c(1.4727, 1.4206, 1.4754, 1.5083, 1.5739,
            1.4341, 1.5517, 1.5483, 1.4614, 1.3337,
            1.6078, 1.4767, 1.4066, 1.5951, 1.8419,
            1.7087, 1.8259, 1.5444)
pistondata<-data.frame(Operator,Pistonring,run,diameter)

#Load package
library("SixSigma")

#Perform gage R & R
my.rr <- ss.rr(var = diameter, part = Pistonring,
               appr = Operator,
               data = pistondata,
               main = "Six Sigma Gage R&R Measure",
               sub = "Piston ring MSA")


# Create data for Process capability analysis
foodsample<-c(755.81, 750.54, 751.05, 749.52, 749.21, 748.38,
              748.11, 753.07, 749.56, 750.08, 747.16, 747.53,
              749.22, 746.76, 747.64, 750.46, 749.27, 750.33,
              750.26, 751.29)

x <- foodsample

ss.ca.cp(x,740, 760)
ss.ca.cpk(x,740, 760)
ss.ca.cp(x, 740, 760, ci = TRUE)
ss.ca.cpk(x, 740, 760, ci = TRUE)

# perform Capability Study
ss.study.ca(x, LSL = 740, USL = 760,
            Target = 750, alpha = 0.5,
            f.su = "Food Sample Example")


