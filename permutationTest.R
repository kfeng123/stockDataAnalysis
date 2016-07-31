library(ggplot2)
# do permutation
myPermutation = function(X1, X2, n1, n2, rmax = 10, B) {
    chen = chenStat(X1, X2, n1, n2)
    my = myStat(X1, X2, n1, n2, rmax)$stat
    chenTemp = NULL
    myTemp = NULL
    for (i in 1:B) {
        myOrder = sample(n1 + n2, size = n1 + n2, replace = FALSE)
        tempX1 = rbind(X1,X2)[myOrder[1:n1], ]
        tempX2 = rbind(X1,X2)[myOrder[(n1+1):(n1+n2)], ]
        chenTemp[i] = chenStat(tempX1, tempX2, n1, n2)
        myTemp[i] = myStat(tempX1, tempX2, n1, n2)$stat
    }
    return(list(
        my = sum(myTemp > my) / length(myTemp),
        chen = sum(chenTemp > chen) / length(chenTemp)
    ))
}

