getwd()
setwd("/Users/dalaohuhan/Documents/GitHub/assignment1")

# problem1_a
my_data <- read.csv("wine.data", head = FALSE)
names(my_data) <- c("Class",
                  "Alcohol",
                  "Malic acid",
                  "Ash",
                  "Alcalinity of ash",
                  "Magnesium",
                  "Total phenols",
                  "Flavanoids",
                  "Nonflavanoid phenols",
                  "Proanthocyanins",
                  "Color intensity",
                  "Hue",
                  "OD280/OD315 of diluted wines",
                  "Proline")

# problem1_b
table(my_data$Class, useNA = "ifany")  # class1 59, class2 71, class3 48

# problem1_c(1)
which.max(my_data$Alcohol)
High_Alcohol_Class <- my_data$Class[9]
High_Alcohol_Class  # belongs to class1

# problem1_c(2)
which.min(my_data$Alcohol)
Low_Alcohol_Class <- my_data$Class[116]
Low_Alcohol_Class  # belongs to class2

# problem1_c(3)
length(which(my_data$Magnesium > 114))  # 26

# problem1_c(4)
length(which((my_data$Magnesium > 114) & my_data$Class == 1))  # 15
length(which((my_data$Magnesium > 114) & my_data$Class == 2))  # 6
length(which((my_data$Magnesium > 114) & my_data$Class == 3))  # 5

# problem1_d
average <- colMeans(my_data)
data_class1 <- my_data[which(my_data$Class == 1),]
data_class2 <- my_data[which(my_data$Class == 2),]
data_class3 <- my_data[which(my_data$Class == 3),]
average_c1 <- colMeans(data_class1)
average_c2 <- colMeans(data_class2)
average_c3 <- colMeans(data_class3)
average_data <- data.frame(average, average_c1, average_c2, average_c3)

# problem2_a
isPerfectPower <- function(n,p) {
  r_max <- as.integer(sqrt(n))
  r_seq <- (1: r_max)
  n_seq <- r_seq^p
  n_root <- which(n_seq == n)
  if (length(n_root) > 0) {
    return(list("isPerfect" = TRUE, "root" = r_seq[n_root]))
  }
  else {
    return(list("isPerfect" = FALSE, "root" = "no integer root"))
  }
}

# problem2_b
findRootPower<-function(n) {
  p <-2
  while (isPerfectPower(n,p)[1] == FALSE) {
    p <- p+1
  }
  return(paste(n, "=", isPerfectPower(n,p)[2], "^", p))
}
findRootPower(27)  # "27 = 3 ^ 3"
findRootPower(13060694016)  # "13060694016 = 6 ^ 13"
findRootPower(7776)  # "7776 = 6 ^ 5"
findRootPower(170859375)  # "170859375 = 15 ^ 7"
findRootPower(58247422)  # is not perfect
findRootPower(94143178827)  # "94143178827 = 3 ^ 23"

# problem3_a






