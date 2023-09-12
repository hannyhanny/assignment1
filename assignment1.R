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

# problem1_e
t.test(data_class1[["Ash"]],data_class2[["Ash"]])
t.test(data_class1[["Ash"]],data_class3[["Ash"]])
t.test(data_class2[["Ash"]],data_class3[["Ash"]])

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
#findRootPower(58247422) is not perfect
findRootPower(94143178827)  # "94143178827 = 3 ^ 23"

# problem3_a

# the code chatGPT generate
# Function to determine the name of a 5-card poker hand
determine_poker_hand <- function(suits, ranks) {
  is_flush <- length(unique(suits)) == 1
  rank_counts <- table(ranks)
  max_rank_count <- max(rank_counts)
  sorted_ranks <- sort(unique(ranks))
  num_ranks <- length(sorted_ranks)
  
  if (is_flush && num_ranks == 5 && max_rank_count == 1) {
    if (max(sorted_ranks) - min(sorted_ranks) == 4) {
      return(ifelse(max(sorted_ranks) == 14, "Royal Flush", "Straight Flush"))
    }
  }
  
  if (max_rank_count == 4) {
    return("Four of a Kind")
  }
  
  if (max_rank_count == 3 && num_ranks == 2) {
    return("Full House")
  }
  
  if (is_flush) {
    return("Flush")
  }
  
  if (num_ranks == 5 && max(sorted_ranks) - min(sorted_ranks) == 4) {
    return("Straight")
  }
  
  if (max_rank_count == 3) {
    return("Three of a Kind")
  }
  
  if (num_ranks == 3 && max_rank_count == 2) {
    return("Two Pair")
  }
  
  if (max_rank_count == 2) {
    return("One Pair")
  }
  
  return("High Card")
}

# Function to simulate dealing a round of poker with a specified number of players
simulate_poker_round <- function(num_players) {
  # Define the deck of cards
  suits <- rep(c("Hearts", "Diamonds", "Clubs", "Spades"), each = 13)
  ranks <- rep(2:10, times = 4)
  ranks <- c(ranks, "Jack", "Queen", "King", "Ace")
  
  # Create an empty list to store player hands
  player_hands <- vector("list", length = num_players)
  
  # Shuffle the deck
  deck <- sample(paste(ranks, suits))
  
  # Deal cards to players
  for (i in 1:num_players) {
    start <- (i - 1) * 5 + 1
    end <- i * 5
    player_hand <- deck[start:end]
    player_hands[[i]] <- player_hand
    
    cat("Player", i, "Hand:", player_hand, "\n")
    cat("Player", i, "Hand Name:", determine_poker_hand(substr(player_hand, nchar(player_hand) - 7, nchar(player_hand) - 6), substr(player_hand, 1, nchar(player_hand) - 8)), "\n\n")
  }
  
  return(player_hands)
}

# Simulate a poker round with 4 players
simulate_poker_round(4)







