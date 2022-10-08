summary <- fivenum(motorcycle[, 2])
motor_IQR <- 1.5 * IQR(motorcycle[, 2])
trouble_county <- subset(motorcycle[, 1], motorcycle[, 2] < (summary[2] - motor_IQR) | motorcycle[, 2] > (summary[4] + motor_IQR))
print(trouble_county)