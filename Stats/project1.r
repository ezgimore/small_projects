#Q1
#PART I & II
#max of Xa and Xb because satellite only fails when the 2nd one does
draws <- replicate(100000, max(rexp(1, .1), rexp(1, .1)))

#PART III
hist(draws, freq = FALSE)
curve(0.2 * exp(-0.1 * x) - 0.2 * exp(-0.2 * x), add = TRUE, col = "red")

#PART IV
print(mean(draws)) #should be approx 15

#PART V
print(1 - pexp(15, 1 / mean(draws)))

# Q2 Monte Carlo estimation of π (10,000 replications)
# generate 10,000 (x,y) points
x <- runif(10000)
y <- runif(10000)

# calculate x^2 + y^2 taking into consideration circle not centered on origin
z <- ((x - 0.5)^2 + (y - 0.5)^2)

# if z <= 0.25, the point is in the circle (r^2 = .25)
# sum(z) is the number of TRUE values (points in circle)
# length(z) is the total number of points (points in the square)
pi <- 4 * sum(z <= 0.25) / length(z)
print(pi)