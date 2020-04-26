### This is partial exam 1 of PROGRAMMING R.

### This exam consists of ten questions with three levels of difficulty, ranging from
### basic (*) to advance (***). Each question is worth 1 point.

### In the exercises where a final answer is requested (these questions will be indicated by
### the expression "(QUESTION X)", where X is the corresponding number of the question), 
### please fill the line "### ANSWER TO X:", which you will find at the end of the exercise 
### where QUESTION X was formulated, with the corresponding answer (there can be more than
### one QUESTION in the same exercise).

### Good luck!
######################################################################################################
set.seed(14);


#### (*) EXERCISE 1: Suppose you have the following vector ####

v <- sample(1:100, 100, replace = TRUE);


#### Create a loop that iterates through all the positions of 'v' and, for each
#### iteration, prints the value of 'v' in that position only if the value is greater 
#### than 10. Do this using both a for and a while loop. Each loop should contain an if
#### (or if-else) clause.

#### For Loop
for (x in v){
  if (x>10){
    print(x)
    }else{
  }
};

#### While loop
index <- 1;
while (index <= length(v)){
  if (v[index] > 10){
    print(v[index])
  }else{
  }
  index <- index + 1
};


rm(list=ls());

#### (*) EXERCISE 2: Assuming you have the following matrix: ####

M <- matrix(c("Rex", "Tom", "Jerry", "Bobby",
               "dog", "cat", "mouse", "cat",
               3, 5, 2, 7), nrow = 4, ncol = 3);
rownames(M) <- 1:4
colnames(M) <- c("name", "type", "age");
M;


### Use numeric indexation to get the value of column 'age' in the second row.
M[2,3];

### Use only logical indexation to get the rows where ''type''age' is greater than 4. Do not create
### the logical index manually, it should be the output of a logical operator (>, <, ==, !=, etc.)
M[M[,colnames(M) == "age"]>4,];

### Use only indexation by name to get the type of Jerry. 
M[3,"type"];


rm(list=ls());
#### (*) EXERCISE 3: A hidden encrypted message has been recently discovered stating: ####
### 'R is cool since 2018-03-08 11:35:20'.
### The date and time of this message is encrypted in the following variable

encrypted <- "008R235_11;  1803...20";


### You want to know how many hours has passed since the encrypted date.
### Write the R commands required to get this number.

Enc <- strptime(encrypted, format = "0%dR2%M_%H;  %y%m...%S");
time <-difftime(Sys.time(),Enc, units = "hours" );
print(paste(round(time, digits=2),"hours has passed since the encrypted date."));


rm(list=ls());

#### (*) EXERCISE 4: Create a function, called 'equal_classes', that returns as output  ####
#### TRUE only if the first argument, named 'var1', and the second argument, named 'var2', 
#### belong to the same class. Use default values so that the output of

equal_classes <- function(var1 = 1, var2 = "character"){
  x<- class(var1)==class(var2)
  return(x)
  };
equal_classes();

#### is FALSE


rm(list=ls());
#### (*) EXERCISE 5: Suppose You have the following vectors: ####

v1 <- c(TRUE, FALSE, TRUE,FALSE);
v2 <- c("A", "B", "C","D")
v3 <- c(1,2 )


### Combine these three vectors in a single matrix, in a manner that v1
### corresponds to the first column in the matrix, v2 to the second and v3
### to the last colum (it is ok if you get a warning due to v2 having
### a different length).
### To which R class belong each column of this matrix? (QUESTION 1).
### v2 has only 2 elements, which value has been assigned to the third row
### of this column? Why? (QUESTION 2). 

#### There are two method that can be use to create matrix for these vectors i performed both method
first_method<-matrix(c(v1,v2,v3),nrow=3,ncol=3);
colnames(first_method) <- c("v1","v2","v3");

class(first_method);

####or

second_method<-cbind(v1,v2,v3);
class(second_method);

## there is another way of creating matrix from a vector by setting its dimension using dim() 
## but in this case it will not work due to unmatched vector length : 
## we will get this error :Error in dim(a) <- c(3, 3) : 
## dims [product 9] do not match the length of object [8]



### ANSWER TO 1: "character" 



### ANSWER TO 2: depending on which method we're using the empty value will be filled accordingly.
### in Matrix(first_method) it will start to fill the missing values with the values from the first vector in
### the matrix. so in this case it will start recycling from v1 to v2 and so on...
### in the cbind(second method) it will fill the missing values from the begining of the same vector so in this one 
### the v3

## first method missing value filled with "True"
## second method missing value replaced with "1"

### as stated by ?matrix and ?cbind documentation, 
### matrix can hold one class object (list or expression vector) 
### and all non-atomic classed objects are coerced by as.vector.
### the type of matrix is dependend on the class of inputs based on
### the hierarchy raw < logical < integer < double < complex < character < list"
### since we don't have a list class in our variables the second in
### the hierachy is character therefore our values will change to character.



rm(list=ls());
#### (**) EXERCISE 6: Suppose you have the following matrix ####


M <- matrix(sample(1:100, 100, replace = TRUE), nrow = 10, ncol = 10);

#### Your objective now is to create a double for loop (one for the rows, one for the columns)
#### that iterates through all the positions of M. In the inner (second) loop you should check
#### if the value of M in that position (row and column) is greater than 90. If that is the case
#### print the indexes corresponding to that row and column and run a next statement.

for(r in 1:nrow(M)){
  for(c in 1:ncol(M)){
    if(M[r, c] > 90){
      print(paste("The value on row:",r, "and column:",c, "is:",  M[r, c], "which is greater than 90"))
      next}
    else{
    }
  }
}

rm(list=ls());
#### (**) Exercise 7: Try to replicate the plot given in quiz.png together  #### 
#### with this quiz using basic plot functions. You must use these vectors

v1 <- iris$Sepal.Length;
v2 <- iris$Sepal.Width;
v3 <- iris$Species;

#1. setting the parameters
par(mfcol=c(1,2), cex.main = 0.80, cex.axis = 0.80, cex.lab = 0.80);
#2. plot the first graph
#2.1 green line
plot(v1, type = "l", col="green", ylim = c(0,10), xlim = c(0,150), main = "PLOT 1", xlab = "Index", ylab = "CM");
#2.2 red point scatter
lines(v2,type= "p", col="red");

#3.plot the second graph
barplot(table(v3), col ="blue", main = "PLOT 2", xlab = "Value", ylab = "Frequency", 
        horiz = TRUE, font = 1, las = 0);



rm(list=ls());
### (**) EXERCISE 8: Source the file "auxiliary.R" that comes ####
#### with this test. Compute the function inside this script for each column 
#### of the following dummy dataset and store the result in a variable. 
#### Note: If you have memory problems, reduce the size of df.

source('C:/Users/Sarang.Zendehrooh/Desktop/IE stuff/Statistical Programming - R/indv-work/quiz/auxiliary.R');

df <- data.frame(v1 = sample(1:10, 10^8, replace = TRUE),
               v2 = sample(11:20, 10^8, replace = TRUE),
               v3 = sample(21:30, 10^8, replace = TRUE));
colnames(df) <- c("v1", "v2", "v3");
head(df);

variable1 <- compute_average(df$v1)
variable2 <- compute_average(df$v2)
variable3 <- compute_average(df$v3)



#### Do this using both a while loop and sapply(). Measure the computational time
#### required for each method. Which one runs faster in your computer? (QUESTION 3)
v_while <-c()
col <- 1;
while(col <= length(df)){
  v_while <-  c(v_while,compute_average(df[,col]))
  col <- 1 + col
};
v_while;

v_sapply <- sapply(df, compute_average);
v_sapply;





while_t <- system.time({
  v_while <-c();
  col <- 1;
  while(col <= length(df)){
    v_while <-  c(v_while,compute_average(df[,col]))
    col <- 1 + col
}})[3]

sapply_t <- system.time(v_sapply <- sapply(df, compute_average))[3]

print(sprintf("average function computational time-> while loop: %f - sapply: %f",
              while_t, sapply_t));


#### answer3: sapply method is faster for average function




#### Now create your own function, called compute_median, to perform the same operations 
#### as with compute_average but this time computing the median for each column of df.

compute_median <- function(x){
  ret <- median(x);
  return(ret);
}



v_while <-c()
col <- 1;
while(col <= length(df)){
  v_while <-  c(v_while,compute_median(df[,col]))
  col <- 1 + col
};
v_while;

v_sapply <- sapply(df, compute_median);
v_sapply;




#### now to get the time elapsed for compute_median function
while_t <- system.time({
  v_while <-c();
  col <- 1;
  while(col <= length(df)){
    v_while <-  c(v_while,compute_median(df[,col]))
    col <- 1 + col
  }})[3]

sapply_t <- system.time(v_sapply <- sapply(df, compute_median))[3]

print(sprintf("median function computational time->  while loop: %f - sapply: %f",
              while_t, sapply_t));

##answer3(median): inconsistent computational time difference for median function
## as some runs while loop appears to be faster, other times sapply. Larger sample of tries is required to determain
## which one will be faster on average 



rm(list = ls());
#### (***) Exercise 9: Read the R object "userbase.RData" provided with this exam. It represents a ####
### database of clients of an airline, where each row corresponds to a flight purchase. Taking into  
### account only flights bought on "2018-11-02", get the  5 users that have made the most
### expensive purchases, i.e if you get that the most expensive purchases are:

# user 1 100
# user 5 90
# user 7 80
# user 5 70
# user 3 60
# user 7 50
# user 8 40

# Your code should return:

# "user1"  "user5"  "user7"  "user3"  "user8"
df <- readRDS("C:/Users/Sarang.Zendehrooh/Desktop/IE stuff/Statistical Programming - R/indv-work/quiz/userbase.RData")
##1.filter the data for the date we want and mask only for user and price column
df_filtered<-df[(df$booking_date==as.Date("2018-11-02")),c("user","price")]
##2. now to group the data by user to get the sum of the purchases by user
df_agg <-  aggregate(list(sum_of_purchases = df_filtered$price), by =list(user=df_filtered$user), FUN=sum)
##3. now to order in descending order
df_output<-df_agg[order(df_agg$sum_of_purchases,decreasing=TRUE),]  
df_output[1:5,"user"]

  


### if we are allowed to use dplyr library then we can use Piping (chaining) method 
### which would make it much more cleaner:


library(dplyr)

df_output <- 
  df %>%
  filter(booking_date == "2018-11-02") %>%
  group_by(user) %>%
  summarise(sum_of_purchases = sum(price)) %>%
  arrange(desc(sum_of_purchases));

c(df_output[1:5,"user"]);

rm(list=ls());

#### (***) EXERCISE 10: Create a function called 'count_pairs_and_trios' which takes advantage of  ####
#### the ... argument functionality to return a list with two variables: the total number of objects
#### with a length equal to 2,  stored in a variable called 'n_duos', 
#### and the total number of objects with length 3, stored in a variable called 'n_trios'. For instance, the 
#### following call




count_pairs_and_trios <- function(...){
  arguments <- list(...);
  n_duos <- 0;
  n_trios <- 0;
  for(var in arguments){
    n_duos <- n_duos + sum(length(var) == 2)
    n_trios <- n_trios + sum(length(var) == 3)
  }
  return(list(n_duos = n_duos, n_trios = n_trios));
}

count_pairs_and_trios(v1 = c("cat", "cat"), 
                      v2 = letters[1:4], 
                      v3 = c("1", "2"),
                      v4 = c("dog"),
                      v5 = factor(c("cat", "dog", "dog"), levels = c("cat", "dog")));

#### should give this output

# $n_duos
# [1] 2
# 
# $n_trios
# [1] 1


