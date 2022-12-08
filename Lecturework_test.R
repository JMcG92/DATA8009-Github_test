setwd("C:\\Users\\justin.mcguinness\\OneDrive - Munster Technological University\\Justins_Work\\CIT\\DATA8009\\Notes\\Final\\Week 9")

library(xlsx)
library(readxl)

df <-read.xlsx("CCexamples.xlsx",1, header = F)

names(df) <- c("Time",excel_sheets("CCexamples.xlsx"))
str(df)

summary(df$`PDI after Centrifuge`)

library(ggplot2)
library(ggQC)

#ggQC::

Cpk()

library(qcc)

q1 <- qcc(df$`PDI after Centrifuge`, type = "xbar.one")
process.capability(q1, spec.limits = c(-0.05,0.05), std.dev = sd(df$`PDI after Centrifuge`))

ggplot(df, aes(`PDI after Centrifuge`))+
  geom_histogram(bins=15,colour="blue", fill="lightblue" )+
  stat_QC_Capability(LSL = -0.05,USL = 0.05, # manually specified specification limits
                     show.lines = c("LCL", "LSL", "X", "USL", "UCL"),
                     show.cap.summary = c("TOL","DNS", "Cp", "Cpk", "Pp", "Ppk", "LCL", "X", "UCL", "Sig"), #selected summary
                     digits = 3,                        #report two digits
                     method="XmR",
                     cap.summary.size = 3,
                     px = 0.15)+
  labs(title="Process Capability")


Cpk(-0.05, 0.05, mean(df$`PDI after Centrifuge`), sd(df$`PDI after Centrifuge`))



df$`PDI after Centrifuge`
hist(df$`PDI after Centrifuge`)

shapiro.test(df$`PDI after Centrifuge`)

ks.test(df$`PDI after Centrifuge`, "pnorm",
        mean = mean(df$`PDI after Centrifuge`), 
        sd = sd(df$`PDI after Centrifuge`))


n <- nrow(df)

set.seed(10)
sim <- rnorm(n, mean(df$`PDI after Centrifuge`), sd(df$`PDI after Centrifuge`))

hist(sim, xlim = c(-0.06, 0.06))
abline(v=-0.05, col = "red")
abline(v=0.05, col = "red")

sum(sim>=0.05)
sum(sim<=-0.05)

2/96




2200/100000

#I've added this line


