#set working directory
setwd("C:\\Users\\it24103291\\Desktop\\It24103291")
getwd()

#1
branch_data <- read.table("Exercise.txt", header = TRUE, sep = ",")


#2
head(branch_data)
str(branch_data)



#3
windows(width = 10, height = 7)
boxplot(branch_data$Sales_X1, main = "Boxplot for Sales")



#4
summary(branch_data$Advertising_X2)
IQR(branch_data$Advertising_X2)



#5
get.outliers <- function(z) {
  q1 <- quantile(z)[2]
  q3 <- quantile(z)[4]
  
  iqr <- q3 - q1
  
  ub <- q3 + 1.5 * iqr
  ib <- q1 - 1.5 * iqr
  
  print(paste("Upper Bound =", ub))
  print(paste("Lower Bound =", ib))
  
  outliers <- sort(z[z < ib | z > ub])
  
  if (length(outliers) > 0) {
    print(paste("Outliers:", paste(outliers, collapse = ",")))
  } else {
    print("No outliers detected.")
  }
}
get.outliers(branch_data$Years_X3)


