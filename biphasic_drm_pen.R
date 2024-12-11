conda activate nls; R
library(drc)

df1 <- readr::read_tsv("pen.tsv")
p1 <- subset(df1, a < 0.1)
m1 <- drm(slopes ~ a, data = p1, fct = LL.4())
p2 <- subset(df1, a >= 0.1)
m2 <- drm(slopes ~ a, data = p2, fct = LL.4())
m <- drm(slopes ~ a, data = df1, fct = twophase(), type = "continuous")
# Fit a 4-parameter biphasic model, and show the problem
plot(drm(slopes ~ a, data = df1, fct = LL.4()))
plot(drm(slopes ~ a, data = df1, fct = twophase(), type = "continuous"))

df2 <- subset(df1, a < 0.6)
plot(drm(slopes ~ a, data = df2, fct = twophase(), type = "continuous"))

plot(df1$a, df1$slopes, log = "x")
curve(-5 + 
      0.1/(1+(1e-4/x)^-2.5) + 
      0.4/(1+(0.02/x)^-3) +
      0.6/(1+(0.02/x)^-3),from = 1e-4, to = 10, add=TRUE)


plot(drm(slopes ~ a, data = df1, fct = twophase(fixed = c(2.45, -2.174198e-01, 3.317354e-01, 1e-4, -10, 0.283264, 0.01104)), type = "continuous"))

psimin, psimid, psimax, ec501, slope2, slope1, ec502
c(-2.37, -1.5, 0.28, 1e-4, 2.45, 10, 0.01104)

b1     c1    d1    e1
-2.17,     	       ec50_1

c(psimin, psimid, psimax, ec501, slope2, slope1, ec502)
guess <- c(NA, -0.217, 0.28, 1e-4, NA, -2, NA)
guess <- c(4, -0.2, 0.28, 1e-4, NA, -2, NA)
guess <- c(4, -0.2, 0.28, 1e-4, NA, -2, NA)
guess <- c(2, -0.2, 0.28, 0.01, NA, NA, 0.01)
guess <- c(1, -0.7, 0.1, 0.01, -5, NA, 0.01)


          "b1","c1","d1","e1","b2","d2","e2")


y ~ bot +
  (span * frac1) / (1 + (EC50_1/x)^NH1) +  # First phase contribution
  (span * frac2) / (1 + (EC50_2/x)^NH2) +  # Second phase contribution
  (span * (1 - frac1 - frac2)) / (1 + (EC50_3/x)^NH3)  # Third phase contribution

span <- max(df1$slopes)-min(df1$slopes)

frac1 <- (df1$slopes[2]-df1$slopes[7])/span
frac2 <- (df1$slopes[7]-mean(df1$slopes[9:10]))/span

plot(df1$a, df1$slopes, log = "x")
curve(-5.25 + 
      (span*frac1)/(1+(1e-2/x)^-2) + 
      (span*frac2)/(1+(0.12/x)^-8) +
      (span*(1 - frac1 - frac2))/(1+(0.5/x)^-20),from = 1e-4, to = 10, add=TRUE)


