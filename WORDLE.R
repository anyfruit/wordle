library(stringi)
library(ggplot2)
library(carData)
library(car)
library(hrbrthemes)
library(GGally)
library(readxl)

Problem_C_Data_Wordle <- read_excel("Problem_C_Data_Wordle (version 1).xlsb.xlsx", 
                                                   col_types = c("numeric", "numeric", "text", 
                                                                 "numeric", "numeric", "numeric", 
                                                                 "numeric", "numeric", "numeric", 
                                                                 "numeric", "numeric", "numeric", 
                                                                 "numeric", "numeric", "numeric", 
                                                                 "numeric", "numeric", "numeric", 
                                                                 "numeric", "numeric", "numeric", 
                                                                 "text"))


#import solution
solution <- read.table("~/solutions.txt", quote="\"", comment.char="")

#data deleted date and ID number
data = Problem_C_Data_Wordle
data = data[,3:20]
detach(data)
attach(data)
colnames(data) <- c('word', 'num_report', 'num_hard', 'one', 'two', 'three', 'four', 'five', 'six', 'seven','sum_of_5+6','sum_of_2+3','none3','none4','mean','medium','sd','skw')

colnames(all_count) <- c('Letter', 'Frequency','Rank')

n <- length(data$word) #sample size of given data
n_all <- length(solution[,1]) #sample size of dictionary

#frequency table of letters of given words
count <- matrix(data = c(c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'), rep(0, 26)), nrow = 26, ncol = 2)
count <- data.frame(count)
count[,2] <- as.numeric(count[,2])
alphabet = c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z')
for (k in 1:n) {
  for (i in 1:5){
    for (j in 1:26){
      s1 <- stri_sub(data$word[k],i,i)
      if (s1 == alphabet[j]) {
      count[j,2] = count[j,2] + 1
      }
    }
  }
}

all_count <- matrix(data = c(c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'), rep(0, 26)), nrow = 26, ncol = 2)
all_count <- data.frame(all_count)
all_count[,2] <- as.numeric(all_count[,2])
#frequency table of letter from all words
for (k in 1:n_all) {
  for (i in 1:5) {
    for (j in 1:26) {
      s1 <- stri_sub(solution[k,1],i,i)
      if (s1 == alphabet[j]) {
        all_count[j,2] = all_count[j,2] + 1
      }
    }
  }
}
all_count <- cbind(all_count,rank(all_count[,2]))


#change to numerical data
data <- data.frame(data)
data[,2] <- as.numeric(data[,2])
data[,3] <- as.numeric(data[,3])
data[,4] <- as.numeric(data[,4])
data[,5] <- as.numeric(data[,5])
data[,6] <- as.numeric(data[,6])
data[,7] <- as.numeric(data[,7])
data[,8] <- as.numeric(data[,8])
data[,9] <- as.numeric(data[,9])
data[,10] <- as.numeric(data[,10])
data[,18] <- as.numeric(data[,18])

#percentage
perc <- data$num_hard/data$num_report
summary(perc)
plot(num_hard,num_report)
hard_report <- lm(num_report~num_hard)
resi <- rstandard(hard_report)
plot(num_hard, resi, main = 'Standardized Residual for Num_report ~ Num_hard', xlab = 'Number of Hard Mode', ylab = 'Standardized Residual')

#MLR
reg <- lm(num_report~num_hard+one+two+three+four+five+six+seven, data = data)

ggpairs(data, upper = list(continuous = wrap("points", alpha = 0.3, size = 0.1)), 
        lower = list(continuous = wrap('cor', size = 4)))


scatterplotMatrix(~num_report+num_hard+one+two+three+four+five+six+seven)

ggcorr(data = data)

#bunch of plots
plot(one, seven)
plot(five, six)
plot(six, seven)
plot(two, seven)
plot(three, six)
plot(two, seven)
plot(three, five)
plot(three, seven)
plot(four, seven)



# ------ Monte Carlo ------- #
counter <- 1
dist_table <- matrix(data=0, nrow = 1, ncol = 7)
len_list = length(solution[,1])
result_matrix <- matrix(data = NA, nrow = 1, ncol = 5) #FIXED
solution_copy <- solution

while (counter < 50){
  #updates
  solution_copy <- solution
  signal <- FALSE #indicate where to stop (ignore the following steps)
  result_matrix <- matrix(data = NA, nrow = 1, ncol = 5) #FIXED
  new_list <- c()
  
  target_word <- 'eerie'
  #target_num <- sample.int(len_list,1)
  #target_word <- solution[target_num, 1] #get a random word
  #while (is.na(target_word)) {
    #target_num <- sample.int(len_list,1)
    #target_word <- solution[target_num, 1]
  #}
  
  # ------------FIRST TRIAL-------------#
  first_num <- sample.int(len_list,1)
  first_word <- solution_copy[first_num, 1]
  while (is.na(first_word)) {
    first_num <- sample.int(len_list,1)
    first_word <- solution_copy[first_num, 1]
  }
  
  if (first_word == target_word) {
    print(target_word)
    dist_table[1,1] <- dist_table[1,1] + 1
    signal <- TRUE
  }
  
  if (signal == FALSE) {
    #check letters
    for (i in 1:5) {#for guess
      
      result_matrix[1, i] <- 0
      
      for (j in 1:5) {#for target
        
        if (grepl(substring(first_word,i,i), substring(target_word,j,j))) {
          result_matrix[1,i] <- 1
        }
          
          
        if (i == j) {
          if (grepl(substring(first_word,i,i), substring(target_word,j,j)))  {
            result_matrix[1,i] <- 2
          } 
        }
        
      }
      
    }
  
    true_letter <- c()
    for(i in 1:5) {
      if(result_matrix[i] == 2) {
        true_letter <- append(true_letter, substring(first_word,i,i))
        true_letter <- append(true_letter, i)
      }
    }
    
    #delete all the words contain grey letters in dictionary
    for (j in 1:n_all) {#all words in dictionary
      for (i in 1:5) {#loop for letters in result matrix
        if (result_matrix[i] == 0){
          if (is.na(solution_copy[j,1]) == FALSE) {
            if (grepl(substring(first_word,i,i), solution_copy[j,1])) {
              solution_copy[j,1] <- NA
            }
          }
        }
      }
    }
    
    #把1的但是在原位的都删掉
    for (i in 1:len_list) {
      for (j in 1:5) {
        if (result_matrix[j] == 1) {
          if (is.na(solution_copy[i,1]) == FALSE && is.na(first_word) == FALSE) {
            if (substring(solution_copy[i,1],j,j) == substring(first_word,j,j)) {
              solution_copy[i,1] <- NA
            }
          }
        }
      }
    }
    
    #secure words with letters in right position
    new_list <- c()
    if (length(true_letter) != 0) {
      for (k in 1:n_all) {
        if (is.na(solution_copy[k,1]) != TRUE) {
          len_true <- length(true_letter)/2
          if (len_true == 1) {
            p1 <- as.numeric(true_letter[2])
            if (grepl(substring(solution_copy[k,1],p1,p1), true_letter[1])) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
          if (len_true == 2) {
            p1 <- as.numeric(true_letter[2])
            p2 <- as.numeric(true_letter[4])
            if (grepl(substring(solution_copy[k,1],p1,p1), true_letter[1]) &&
                grepl(substring(solution_copy[k,1],p2,p2), true_letter[3])) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
          if (len_true == 3) {
            p1 <- as.numeric(true_letter[2])
            p2 <- as.numeric(true_letter[4])
            p3 <- as.numeric(true_letter[6])
            if (substring(solution_copy[k,1],p1,p1) == true_letter[1] &&
                substring(solution_copy[k,1],p2,p2) == true_letter[3] &&
                substring(solution_copy[k,1],p3,p3) == true_letter[5]) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
          if (len_true == 4) {
            p1 <- as.numeric(true_letter[2])
            p2 <- as.numeric(true_letter[4])
            p3 <- as.numeric(true_letter[6])
            p4 <- as.numeric(true_letter[8])
            if (substring(solution_copy[k,1],p1,p1) == true_letter[1] &&
                substring(solution_copy[k,1],p2,p2) == true_letter[3] &&
                substring(solution_copy[k,1],p3,p3) == true_letter[5] &&
                substring(solution_copy[k,1],p4,p4) == true_letter[7]) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
          if (len_true == 5) {
            p1 <- as.numeric(true_letter[2])
            p2 <- as.numeric(true_letter[4])
            p3 <- as.numeric(true_letter[6])
            p4 <- as.numeric(true_letter[8])
            p5 <- as.numeric(true_letter[10])
            if (substring(solution_copy[k,1],p1,p1) == true_letter[1] &&
                substring(solution_copy[k,1],p2,p2) == true_letter[3] &&
                substring(solution_copy[k,1],p3,p3) == true_letter[5] &&
                substring(solution_copy[k,1],p4,p4) == true_letter[7] &&
                substring(solution_copy[k,1],p5,p5) == true_letter[9]) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
        }
      }
    }
    len_new_list <- length(new_list)
  }

  #现在如果有2的话，从new_list抓词
  #else，从solution_copy里抓，该删的都删完了
  # -------------END OF CYCLE 111111---------------#
  
  # ----------------TRIAL 2 STARTS-----------------#
  if (signal == FALSE) {
    if (len_new_list > 0) {
      first_num <- sample.int(len_new_list,1)
      first_word <- new_list[first_num]
      while (is.na(first_word)) {
        first_num <- sample.int(len_new_list,1)
        first_word <- new_list[first_num]
      }
    } else {
      first_num <- sample.int(len_list,1)
      first_word <- solution_copy[first_num, 1]
      while (is.na(first_word)) {
        first_num <- sample.int(len_list,1)
        first_word <- solution_copy[first_num, 1]
      }
    }
    
    if (first_word == target_word) {
      print(target_word)
      dist_table[1,2] <- dist_table[1,2] + 1
      signal <- TRUE
    }
    
    #check letters
    for (i in 1:5) {#for guess
      
      result_matrix[1, i] <- 0
      
      for (j in 1:5) {#for target
        +
        if (substring(first_word,i,i) == substring(target_word,j,j)) {
          result_matrix[1,i] <- 1
        }
        
        
        if (i == j) {
          if (grepl(substring(first_word,j,j), substring(target_word,j,j)))  {
            result_matrix[1,i] <- 2
          } 
        }
        
      }
      
    }
    
    true_letter <- c()
    for(i in 1:5) {
      if(result_matrix[i] == 2) {
        true_letter <- append(true_letter, substring(first_word,i,i))
        true_letter <- append(true_letter, i)
      }
    }
    
    #delete all the words contain grey letters in dictionary
    for (j in 1:n_all) {#all words in dictionary
      for (i in 1:5) {#loop for letters in result matrix
        if (result_matrix[i] == 0){
          if (is.na(solution_copy[j,1]) == FALSE) {
            if (grepl(substring(first_word,i,i), solution_copy[j,1])) {
              solution_copy[j,1] <- NA
            }
          }
        }
      }
    }
    
    #把1的但是在原位的都删掉
    for (i in 1:len_list) {
      for (j in 1:5) {
        if (result_matrix[j] == 1) {
          if (is.na(solution_copy[i,1]) == FALSE && is.na(first_word) == FALSE) {
            if (substring(solution_copy[i,1],j,j) == substring(first_word,j,j)) {
              solution_copy[i,1] <- NA
            }
          }
        }
      }
    }
    
    #secure words with letters in right position
    new_list <- c()
    if (length(true_letter) != 0) {
      for (k in 1:n_all) {
        if (is.na(solution_copy[k,1]) != TRUE) {
          len_true <- length(true_letter)/2
          if (len_true == 1) {
            p1 <- as.numeric(true_letter[2])
            if (substring(solution_copy[k,1],p1,p1) == true_letter[1]) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
          if (len_true == 2) {
            p1 <- as.numeric(true_letter[2])
            p2 <- as.numeric(true_letter[4])
            if (substring(solution_copy[k,1],p1,p1) == true_letter[1] &&
                substring(solution_copy[k,1],p2,p2) == true_letter[3]) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
          if (len_true == 3) {
            p1 <- as.numeric(true_letter[2])
            p2 <- as.numeric(true_letter[4])
            p3 <- as.numeric(true_letter[6])
            if (substring(solution_copy[k,1],p1,p1) == true_letter[1] &&
                substring(solution_copy[k,1],p2,p2) == true_letter[3] &&
                substring(solution_copy[k,1],p3,p3) == true_letter[5]) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
          if (len_true == 4) {
            p1 <- as.numeric(true_letter[2])
            p2 <- as.numeric(true_letter[4])
            p3 <- as.numeric(true_letter[6])
            p4 <- as.numeric(true_letter[8])
            if (substring(solution_copy[k,1],p1,p1) == true_letter[1] &&
                substring(solution_copy[k,1],p2,p2) == true_letter[3] &&
                substring(solution_copy[k,1],p3,p3) == true_letter[5] &&
                substring(solution_copy[k,1],p4,p4) == true_letter[7]) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
          if (len_true == 5) {
            p1 <- as.numeric(true_letter[2])
            p2 <- as.numeric(true_letter[4])
            p3 <- as.numeric(true_letter[6])
            p4 <- as.numeric(true_letter[8])
            p5 <- as.numeric(true_letter[10])
            if (substring(solution_copy[k,1],p1,p1) == true_letter[1] &&
                substring(solution_copy[k,1],p2,p2) == true_letter[3] &&
                substring(solution_copy[k,1],p3,p3) == true_letter[5] &&
                substring(solution_copy[k,1],p4,p4) == true_letter[7] &&
                substring(solution_copy[k,1],p5,p5) == true_letter[9]) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
        }
      }
    }

    len_new_list <- length(new_list)
  }

  # -------------END OF CYCLE 222222---------------#
  
  # ----------------TRIAL 3 STARTS-----------------#
  if (signal == FALSE) {
    if (len_new_list > 0) {
      first_num <- sample.int(len_new_list,1)
      first_word <- new_list[first_num]
      while (is.na(first_word)) {
        first_num <- sample.int(len_new_list,1)
        first_word <- new_list[first_num]
      }
    } else {
      first_num <- sample.int(len_list,1)
      first_word <- solution_copy[first_num, 1]
      while (is.na(first_word)) {
        first_num <- sample.int(len_list,1)
        first_word <- solution_copy[first_num, 1]
      }
    }
    
    if (first_word == target_word) {
      print(target_word)
      dist_table[1,3] <- dist_table[1,3] + 1
      signal <- TRUE
    }
    
    #check letters
    for (i in 1:5) {#for guess
      
      result_matrix[1, i] <- 0
      
      for (j in 1:5) {#for target
        
        if (substring(first_word,i,i) == substring(target_word,j,j)) {
          result_matrix[1,i] <- 1
        }
        
        
        if (i == j) {
          if (substring(first_word,i,i) == substring(target_word,j,j))  {
            result_matrix[1,i] <- 2
          } 
        }
        
      }
      
    }
    
    true_letter <- c()
    for(i in 1:5) {
      if(result_matrix[i] == 2) {
        true_letter <- append(true_letter, substring(first_word,i,i))
        true_letter <- append(true_letter, i)
      }
    }
    
    #delete all the words contain grey letters in dictionary
    for (j in 1:n_all) {#all words in dictionary
      for (i in 1:5) {#loop for letters in result matrix
        if (result_matrix[i] == 0){
          if (is.na(solution_copy[j,1]) == FALSE) {
            if (grepl(substring(first_word,i,i), solution_copy[j,1])) {
              solution_copy[j,1] <- NA
            }
          }
        }
      }
    }
    
    #把1的但是在原位的都删掉
    for (i in 1:len_list) {
      for (j in 1:5) {
        if (result_matrix[j] == 1) {
          if (is.na(solution_copy[i,1]) == FALSE && is.na(first_word) == FALSE) {
            if (substring(solution_copy[i,1],j,j) == substring(first_word,j,j)) {
              solution_copy[i,1] <- NA
            }
          }
        }
      }
    }
    
    #secure words with letters in right position
    new_list <- c()
    if (length(true_letter) != 0) {
      for (k in 1:n_all) {
        if (is.na(solution_copy[k,1]) != TRUE) {
          len_true <- length(true_letter)/2
          if (len_true == 1) {
            p1 <- as.numeric(true_letter[2])
            if (substring(solution_copy[k,1],p1,p1) == true_letter[1]) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
          if (len_true == 2) {
            p1 <- as.numeric(true_letter[2])
            p2 <- as.numeric(true_letter[4])
            if (substring(solution_copy[k,1],p1,p1) == true_letter[1] &&
                substring(solution_copy[k,1],p2,p2) == true_letter[3]) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
          if (len_true == 3) {
            p1 <- as.numeric(true_letter[2])
            p2 <- as.numeric(true_letter[4])
            p3 <- as.numeric(true_letter[6])
            if (substring(solution_copy[k,1],p1,p1) == true_letter[1] &&
                substring(solution_copy[k,1],p2,p2) == true_letter[3] &&
                substring(solution_copy[k,1],p3,p3) == true_letter[5]) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
          if (len_true == 4) {
            p1 <- as.numeric(true_letter[2])
            p2 <- as.numeric(true_letter[4])
            p3 <- as.numeric(true_letter[6])
            p4 <- as.numeric(true_letter[8])
            if (substring(solution_copy[k,1],p1,p1) == true_letter[1] &&
                substring(solution_copy[k,1],p2,p2) == true_letter[3] &&
                substring(solution_copy[k,1],p3,p3) == true_letter[5] &&
                substring(solution_copy[k,1],p4,p4) == true_letter[7]) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
          if (len_true == 5) {
            p1 <- as.numeric(true_letter[2])
            p2 <- as.numeric(true_letter[4])
            p3 <- as.numeric(true_letter[6])
            p4 <- as.numeric(true_letter[8])
            p5 <- as.numeric(true_letter[10])
            if (substring(solution_copy[k,1],p1,p1) == true_letter[1] &&
                substring(solution_copy[k,1],p2,p2) == true_letter[3] &&
                substring(solution_copy[k,1],p3,p3) == true_letter[5] &&
                substring(solution_copy[k,1],p4,p4) == true_letter[7] &&
                substring(solution_copy[k,1],p5,p5) == true_letter[9]) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
        }
      }
    }

    len_new_list <- length(new_list)

  }

  
  # -------------END OF CYCLE 333333---------------#
  
  # ----------------TRIAL 4 STARTS-----------------#
  if (signal == FALSE) {
    if (len_new_list > 0) {
      first_num <- sample.int(len_new_list,1)
      first_word <- new_list[first_num]
      while (is.na(first_word)) {
        first_num <- sample.int(len_new_list,1)
        first_word <- new_list[first_num]
      }
    } else {
      first_num <- sample.int(len_list,1)
      first_word <- solution_copy[first_num, 1]
      while (is.na(first_word)) {
        first_num <- sample.int(len_list,1)
        first_word <- solution_copy[first_num, 1]
      }
    }
    
    if (first_word == target_word) {
      print(target_word)
      dist_table[1,4] <- dist_table[1,4] + 1
      signal <- TRUE
    }
    
    #check letters
    for (i in 1:5) {#for guess
      
      result_matrix[1, i] <- 0
      
      for (j in 1:5) {#for target
        
        if (substring(first_word,i,i) == substring(target_word,j,j)) {
          result_matrix[1,i] <- 1
        }
        
        
        if (i == j) {
          if (substring(first_word,i,i) == substring(target_word,j,j))  {
            result_matrix[1,i] <- 2
          } 
        }
        
      }
      
    }
    
    true_letter <- c()
    for(i in 1:5) {
      if(result_matrix[i] == 2) {
        true_letter <- append(true_letter, substring(first_word,i,i))
        true_letter <- append(true_letter, i)
      }
    }
    
    #delete all the words contain grey letters in dictionary
    for (j in 1:n_all) {#all words in dictionary
      for (i in 1:5) {#loop for letters in result matrix
        if (result_matrix[i] == 0){
          if (is.na(solution_copy[j,1]) == FALSE) {
            if (grepl(substring(first_word,i,i), solution_copy[j,1])) {
              solution_copy[j,1] <- NA
            }
          }
        }
      }
    }
    
    #把1的但是在原位的都删掉
    for (i in 1:len_list) {
      for (j in 1:5) {
        if (result_matrix[j] == 1) {
          if (is.na(solution_copy[i,1]) == FALSE) {
            if (substring(solution_copy[i,1],j,j) == substring(first_word,j,j)) {
              solution_copy[i,1] <- NA
            }
          }
        }
      }
    }
    
    #secure words with letters in right position
    new_list <- c()
    if (length(true_letter) != 0) {
      for (k in 1:n_all) {
        if (is.na(solution_copy[k,1]) != TRUE) {
          len_true <- length(true_letter)/2
          if (len_true == 1) {
            p1 <- as.numeric(true_letter[2])
            if (substring(solution_copy[k,1],p1,p1) == true_letter[1]) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
          if (len_true == 2) {
            p1 <- as.numeric(true_letter[2])
            p2 <- as.numeric(true_letter[4])
            if (substring(solution_copy[k,1],p1,p1) == true_letter[1] &&
                substring(solution_copy[k,1],p2,p2) == true_letter[3]) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
          if (len_true == 3) {
            p1 <- as.numeric(true_letter[2])
            p2 <- as.numeric(true_letter[4])
            p3 <- as.numeric(true_letter[6])
            if (substring(solution_copy[k,1],p1,p1) == true_letter[1] &&
                substring(solution_copy[k,1],p2,p2) == true_letter[3] &&
                substring(solution_copy[k,1],p3,p3) == true_letter[5]) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
          if (len_true == 4) {
            p1 <- as.numeric(true_letter[2])
            p2 <- as.numeric(true_letter[4])
            p3 <- as.numeric(true_letter[6])
            p4 <- as.numeric(true_letter[8])
            if (substring(solution_copy[k,1],p1,p1) == true_letter[1] &&
                substring(solution_copy[k,1],p2,p2) == true_letter[3] &&
                substring(solution_copy[k,1],p3,p3) == true_letter[5] &&
                substring(solution_copy[k,1],p4,p4) == true_letter[7]) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
          if (len_true == 5) {
            p1 <- as.numeric(true_letter[2])
            p2 <- as.numeric(true_letter[4])
            p3 <- as.numeric(true_letter[6])
            p4 <- as.numeric(true_letter[8])
            p5 <- as.numeric(true_letter[10])
            if (substring(solution_copy[k,1],p1,p1) == true_letter[1] &&
                substring(solution_copy[k,1],p2,p2) == true_letter[3] &&
                substring(solution_copy[k,1],p3,p3) == true_letter[5] &&
                substring(solution_copy[k,1],p4,p4) == true_letter[7] &&
                substring(solution_copy[k,1],p5,p5) == true_letter[9]) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
        }
      }
    }

    len_new_list <- length(new_list)
  }

  # -------------END OF CYCLE 444444---------------#
  
  # ----------------TRIAL 5 STARTS-----------------#
  if (signal == FALSE) {
    if (len_new_list > 0) {
      first_num <- sample.int(len_new_list,1)
      first_word <- new_list[first_num]
      while (is.na(first_word)) {
        first_num <- sample.int(len_new_list,1)
        first_word <- new_list[first_num]
      }
    } else {
      first_num <- sample.int(len_list,1)
      first_word <- solution_copy[first_num, 1]
      while (is.na(first_word)) {
        first_num <- sample.int(len_list,1)
        first_word <- solution_copy[first_num, 1]
      }
    }
    
    if (first_word == target_word) {
      print(target_word)
      dist_table[1,5] <- dist_table[1,5] + 1
      signal <- TRUE
    }
    
    #check letters
    for (i in 1:5) {#for guess
      
      result_matrix[1, i] <- 0
      
      for (j in 1:5) {#for target
        
        if (substring(first_word,i,i) == substring(target_word,j,j)) {
          result_matrix[1,i] <- 1
        }
        
        
        if (i == j) {
          if (substring(first_word,i,i) == substring(target_word,j,j))  {
            result_matrix[1,i] <- 2
          } 
        }
        
      }
      
    }
    
    true_letter <- c()
    for(i in 1:5) {
      if(result_matrix[i] == 2) {
        true_letter <- append(true_letter, substring(first_word,i,i))
        true_letter <- append(true_letter, i)
      }
    }
    
    #delete all the words contain grey letters in dictionary
    for (j in 1:n_all) {#all words in dictionary
      for (i in 1:5) {#loop for letters in result matrix
        if (result_matrix[i] == 0){
          if (is.na(solution_copy[j,1]) == FALSE) {
            if (grepl(substring(first_word,i,i), solution_copy[j,1])) {
              solution_copy[j,1] <- NA
            }
          }
        }
      }
    }
    
    #把1的但是在原位的都删掉
    for (i in 1:len_list) {
      for (j in 1:5) {
        if (result_matrix[j] == 1) {
          if (is.na(solution_copy[i,1]) == FALSE) {
            if (substring(solution_copy[i,1],j,j) == substring(first_word,j,j)) {
              solution_copy[i,1] <- NA
            }
          }
        }
      }
    }
    
    #secure words with letters in right position
    new_list <- c()
    if (length(true_letter) != 0) {
      for (k in 1:n_all) {
        if (is.na(solution_copy[k,1]) != TRUE) {
          len_true <- length(true_letter)/2
          if (len_true == 1) {
            p1 <- as.numeric(true_letter[2])
            if (substring(solution_copy[k,1],p1,p1) == true_letter[1]) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
          if (len_true == 2) {
            p1 <- as.numeric(true_letter[2])
            p2 <- as.numeric(true_letter[4])
            if (substring(solution_copy[k,1],p1,p1) == true_letter[1] &&
                substring(solution_copy[k,1],p2,p2) == true_letter[3]) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
          if (len_true == 3) {
            p1 <- as.numeric(true_letter[2])
            p2 <- as.numeric(true_letter[4])
            p3 <- as.numeric(true_letter[6])
            if (substring(solution_copy[k,1],p1,p1) == true_letter[1] &&
                substring(solution_copy[k,1],p2,p2) == true_letter[3] &&
                substring(solution_copy[k,1],p3,p3) == true_letter[5]) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
          if (len_true == 4) {
            p1 <- as.numeric(true_letter[2])
            p2 <- as.numeric(true_letter[4])
            p3 <- as.numeric(true_letter[6])
            p4 <- as.numeric(true_letter[8])
            if (substring(solution_copy[k,1],p1,p1) == true_letter[1] &&
                substring(solution_copy[k,1],p2,p2) == true_letter[3] &&
                substring(solution_copy[k,1],p3,p3) == true_letter[5] &&
                substring(solution_copy[k,1],p4,p4) == true_letter[7]) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
          if (len_true == 5) {
            p1 <- as.numeric(true_letter[2])
            p2 <- as.numeric(true_letter[4])
            p3 <- as.numeric(true_letter[6])
            p4 <- as.numeric(true_letter[8])
            p5 <- as.numeric(true_letter[10])
            if (substring(solution_copy[k,1],p1,p1) == true_letter[1] &&
                substring(solution_copy[k,1],p2,p2) == true_letter[3] &&
                substring(solution_copy[k,1],p3,p3) == true_letter[5] &&
                substring(solution_copy[k,1],p4,p4) == true_letter[7] &&
                substring(solution_copy[k,1],p5,p5) == true_letter[9]) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
        }
      }
    }

    len_new_list <- length(new_list)
  }
  
  # -------------END OF CYCLE 555555---------------#
  
  # ----------------TRIAL 6 STARTS-----------------#
  if (signal == FALSE) {
    if (len_new_list > 0) {
      first_num <- sample.int(len_new_list,1)
      first_word <- new_list[first_num]
      while (is.na(first_word)) {
        first_num <- sample.int(len_new_list,1)
        first_word <- new_list[first_num]
      }
    } else {
      first_num <- sample.int(len_list,1)
      first_word <- solution_copy[first_num, 1]
      while (is.na(first_word)) {
        first_num <- sample.int(len_list,1)
        first_word <- solution_copy[first_num, 1]
      }
    }
    
    if (first_word == target_word) {
      print(target_word)
      dist_table[1,6] <- dist_table[1,6] + 1
      signal <- TRUE
    }
    
    #check letters
    for (i in 1:5) {#for guess
      
      result_matrix[1, i] <- 0
      
      for (j in 1:5) {#for target
        
        if (substring(first_word,i,i) == substring(target_word,j,j)) {
          result_matrix[1,i] <- 1
        }
        
        
        if (i == j) {
          if (substring(first_word,i,i) == substring(target_word,j,j))  {
            result_matrix[1,i] <- 2
          } 
        }
        
      }
      
    }
    
    true_letter <- c()
    for(i in 1:5) {
      if(result_matrix[i] == 2) {
        true_letter <- append(true_letter, substring(first_word,i,i))
        true_letter <- append(true_letter, i)
      }
    }
    
    #delete all the words contain grey letters in dictionary
    for (j in 1:n_all) {#all words in dictionary
      for (i in 1:5) {#loop for letters in result matrix
        if (result_matrix[i] == 0){
          if (is.na(solution_copy[j,1]) == FALSE) {
            if (grepl(substring(first_word,i,i), solution_copy[j,1])) {
              solution_copy[j,1] <- NA
            }
          }
        }
      }
    }
    
    #把1的但是在原位的都删掉
    for (i in 1:len_list) {
      for (j in 1:5) {
        if (result_matrix[j] == 1) {
          if (is.na(solution_copy[i,1]) == FALSE) {
            if (substring(solution_copy[i,1],j,j) == substring(first_word,j,j)) {
              solution_copy[i,1] <- NA
            }
          }
        }
      }
    }
    
    #secure words with letters in right position
    new_list <- c()
    if (length(true_letter) != 0) {
      for (k in 1:n_all) {
        if (is.na(solution_copy[k,1]) != TRUE) {
          len_true <- length(true_letter)/2
          if (len_true == 1) {
            p1 <- as.numeric(true_letter[2])
            if (substring(solution_copy[k,1],p1,p1) == true_letter[1]) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
          if (len_true == 2) {
            p1 <- as.numeric(true_letter[2])
            p2 <- as.numeric(true_letter[4])
            if (substring(solution_copy[k,1],p1,p1) == true_letter[1] &&
                substring(solution_copy[k,1],p2,p2) == true_letter[3]) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
          if (len_true == 3) {
            p1 <- as.numeric(true_letter[2])
            p2 <- as.numeric(true_letter[4])
            p3 <- as.numeric(true_letter[6])
            if (substring(solution_copy[k,1],p1,p1) == true_letter[1] &&
                substring(solution_copy[k,1],p2,p2) == true_letter[3] &&
                substring(solution_copy[k,1],p3,p3) == true_letter[5]) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
          if (len_true == 4) {
            p1 <- as.numeric(true_letter[2])
            p2 <- as.numeric(true_letter[4])
            p3 <- as.numeric(true_letter[6])
            p4 <- as.numeric(true_letter[8])
            if (substring(solution_copy[k,1],p1,p1) == true_letter[1] &&
                substring(solution_copy[k,1],p2,p2) == true_letter[3] &&
                substring(solution_copy[k,1],p3,p3) == true_letter[5] &&
                substring(solution_copy[k,1],p4,p4) == true_letter[7]) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
          if (len_true == 5) {
            p1 <- as.numeric(true_letter[2])
            p2 <- as.numeric(true_letter[4])
            p3 <- as.numeric(true_letter[6])
            p4 <- as.numeric(true_letter[8])
            p5 <- as.numeric(true_letter[10])
            if (substring(solution_copy[k,1],p1,p1) == true_letter[1] &&
                substring(solution_copy[k,1],p2,p2) == true_letter[3] &&
                substring(solution_copy[k,1],p3,p3) == true_letter[5] &&
                substring(solution_copy[k,1],p4,p4) == true_letter[7] &&
                substring(solution_copy[k,1],p5,p5) == true_letter[9]) {
              new_list <- append(new_list, solution_copy[k,1])
            }
          }
        }
      }
    }

    len_new_list <- length(new_list)
  }
  
  if (signal == FALSE) {
    dist_table[1,7] <- dist_table[1,7] + 1
    print('fail')
  }
  
  counter = counter+1
  
}

# ------ ENDS ------ #


reg2 <- lm(num_report~one)
plot(one, num_report)
abline(reg2, col='red')

reg3 <- lm(num_report~seven)
plot(seven, num_report)
abline(reg3, col='red')


plot(Problem_C_Data_Wordle$...1[2:360],num_report, xlab = 'Date', ylab = 'Number Reported')
abline(v=44592, col = 'green', lwd = 2)

plot(Problem_C_Data_Wordle$...1[2:360],num_hard , xlab = 'Date', ylab = 'Number of Hard Mode')
abline(v=44592, col = 'green', lwd = 2)



#num report against date
plot(Problem_C_Data_Wordle$...1[2:360], num_report)
plot(Problem_C_Data_Wordle$...1[2:360], log(num_report))

#try to fit into beta distribution
gfg = seq(0, 1, length.out=359) #create a sequence between 0 to 1

comp <- 10^10000
a <- NA
b <- NA
for (i in 1:100) {
  for (j in -100:0) {
    error <- 0
    for (k in 1:n) {
      error = error + (i*(exp(j*k)) - num_report)
    }
    error <- error^2
  if (error < comp) {
    comp <- error
    a <- i
    b <- j
  }
  print(comp)
  }
}

SSE <- sum((breaks-dbeta(gfg,3,5))^2)




#regression for difficulty
#y <- skewness
#x1 <- frequency of single letter in all words
#x2 <- continuous two same letters; 1 <- there is consecutive same letter; 0<- no consecutive same letter
#x3 <- 
#x3 <- textstat_readability

#y <- skw
y <- none1

x1 <- c()
for (i in 1:n) {
  
  score1 <- all_count$`rank(all_count[, 2])`[all_count$X1==substring(word[i],1,1)]
  score2 <- all_count$`rank(all_count[, 2])`[all_count$X1==substring(word[i],2,2)]
  score3 <- all_count$`rank(all_count[, 2])`[all_count$X1==substring(word[i],3,3)]
  score4 <- all_count$`rank(all_count[, 2])`[all_count$X1==substring(word[i],4,4)]
  score5 <- all_count$`rank(all_count[, 2])`[all_count$X1==substring(word[i],5,5)]
  sumofeach <- sum(score1, score2, score3, score4, score5)
  sumofeach <- sumofeach/5
  
  
  x1 <- append(x1, sumofeach)
}
x1 #frequency mean

x2 <- c() #2 set of 2, despite consecutive or not
for (i in 1:n) {
  tab <- table(rle(strsplit(word[i],"")[[1]]))
  wlen <- length(tab[1,])
  count_for_repeat <- 0
  for (j in 1:wlen) {
    if (tab[1,j] == 2) {
     count_for_repeat = count_for_repeat + 1
   }
    if (length(tab[,1]) > 1) {
      if (tab[2,j] == 1) {
        count_for_repeat = count_for_repeat + 1
      }x
    }
  }
  if (count_for_repeat == 2) {
    x2 <- append(x2, 1)
  } else {
    x2 <- append(x2, 0)
  }
  
}

x3 <- c()
for (i in 1:n) {
  tab <- table(rle(strsplit(word[i],"")[[1]]))
  wlen <- length(tab[1,])
  count_for_repeat <- 0
  for (j in 1:wlen) {
    if (tab[1,j] == 3) {
      count_for_repeat = count_for_repeat + 1
    }
    if (length(tab[,1]) > 1) {
      if (tab[1,j] == 1 && tab[2,j] == 1) {
        count_for_repeat = count_for_repeat + 1
      }
    }
    
  }
  if (count_for_repeat == 1) {
    x3 <- append(x3, 1)
  } else {
    x3 <- append(x3, 0)
  }
  
}


x4 <- c() #consecutive letters
for (i in 1:n) {
  lengs <- length(table(rle(strsplit(word[i],"")[[1]]))[,1])
  if (lengs > 1) {
    if (x3[n] == 1 || x2[n] == 1) {
      x4 <- append(x4, 0)
    } else {
      x4 <- append(x4, 1)
    }
  } else {
    x4 <- append(x4, 0)
  }
}

x5 <- c() #2 same letter not consecutive
for (i in 1:n) {
  lengs <- length(table(rle(strsplit(word[i],"")[[1]]))[,1])
  wlen <- length(table(rle(strsplit(word[i],"")[[1]]))[1,])
  count_for_repeat <- 0
  if (lengs == 1) {
    for (j in 1:wlen) {
      if (table(rle(strsplit(word[i],"")[[1]]))[1,j] == 2) {
        count_for_repeat = count_for_repeat + 1
      }
      
    }
    if (count_for_repeat == 1) {
      if (x3[n] == 1 || x2[n] == 1 || x4[n] == 1) {
        x5 <- append(x5, 0)
      } else {
        x5 <- append(x5, 1)
      }
    } else {
      x5 <- append(x5, 0)
    }
  } else {
    x5 <- append(x5, 0)
  }
}

regre <- lm(y~x1+x2+x3+x4+x5)
summary(regre)
avPlots(regre)
ggpairs(data.frame(y,x1,x2,x3,x4,x5))


#NQQ plot  indicating not normal -> why?
qqnorm(num_report, pch = 1, frame = FALSE)
qqline(num_report, col = "steelblue", lwd = 2)
qqnorm(num_hard, pch = 1, frame = FALSE)
qqline(num_hard, col = "steelblue", lwd = 2)


#difficulty for ocean
oceansum <- 0
for (i in 1:5) {
  numb <- all_count$`rank(all_count[, 2])`[all_count$X1 == substring('ocean',i,i)]
  oceansum = oceansum + numb
}
oceansum/5
  
regre$coefficients[1] + (oceansum/5)*regre$coefficients[2]

barplot(c(0.32, 8.59, 24.64,37.38,19.19,8.98,0.92),main = 'Monte-Carlo Simulation Percentage Distribution', xlab = 'Number of Tries', names.arg = c('1','2','3','4','5','6','X'))
barplot(c(0.47, 5.78, 22.64,32.95,23.71,11.61,2.82),main = 'Average Percentage Distribution from Real Data', xlab = 'Number of Tries', names.arg = c('1','2','3','4','5','6','X'))

eeriesum <- 0
for (i in 1:5) {
  numb <- all_count$`rank(all_count[, 2])`[all_count$X1 == substring('eerie',i,i)]
  eeriesum = eeriesum + numb
}
eeriesum/5

elatesum <- 0
for (i in 1:5) {
  numb <- all_count$Rank[all_count$Letter == substring('elate',i,i)]
  elatesum = elatesum + numb
}
elatesum/5
regre$coefficients[1] + regre$coefficients[2]*(elatesum/5)


plot(data$none1, data$num_report)
plot(data$none1, data$num_hard)
reg1 <- lm(num_hard~none1)
reg2 <- lm(num_report~none1)
plot(six, num_hard)
summary(lm(num_hard~six))
summary(lm(num_hard~one))
plot(two, num_hard)
summary(lm(num_hard~two))
plot(three, num_hard)
#related: num_hard and four
summary(lm(num_hard~four))
plot(four,num_hard)
summary(lm(num_hard~five))

hist(none1)
