##computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
library(ROCR)
preds_list <- c(1:100)
m <- length(preds_list)

actual_list <- c(0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
actual_list2 <- c(1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)


#Algorithmus X

if (dev.cur() <= 1) dev.new()

opar <- par(ask = interactive() &&
            (.Device %in% c("X11", "GTK", "gnome", "windows", "quartz")))

#data(ROCR.hiv)
#pp <- ROCR.hiv$hiv.svm$predictions
#ll <- ROCR.hiv$hiv.svm$labels
pp <- preds_list
ll <- actual_list

par(mfrow = c(2, 2))
pred <- prediction(pnorm(pp), ll)

perf <- performance(pred, "tpr", "fpr")
plot(perf, avg = "threshold", color = "green", lwd = 3,
     main = "X-Algorithmus")
plot(perf, lty = 3, col = "grey78", add = T)

#Y-Algorithmus
if (dev.cur() <= 1) dev.new()


pp <- preds_list
ll <- actual_list2

par(mfrow = c(2, 2))
pred <- prediction(pnorm(pp), ll)

perf <- performance(pred, "tpr", "fpr")
plot(perf, avg = "threshold", colo = "red", lwd = 3,
     main = "Y-Algorithmus")
plot(perf, lty = 3, col = "grey78", add = T)


h <- function(x) {
    return(x)
}
)
