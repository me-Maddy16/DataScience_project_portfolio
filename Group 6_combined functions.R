#classify b
data_set<-read.csv("dataset_b4.csv",header=FALSE)

bn<-data_set$V2[data_set$V1=='bn']
bo<-data_set$V2[data_set$V1=='bo']

bn_mean<-mean(bn)
bn_sd<-sd(bn)
b0_mean<-mean(bo)
b0_sd<-sd(bo)
#Using normal approximation
B_Classifier<-function(z){
  if(((bn_mean-3*bn_sd) < z) && ((bn_mean+3*bn_sd) > z)){
    return ('bn')
  } else{
    return ('bo')
  }  
  
}
#random test case
B_Classifier(0.64378305341546)



#classify c
library(data.table)
list_files = c()
for(i in 1:500){
  list_files[i]=paste("c",(i-1),".csv", sep = "")
}
list_files
myfiles = lapply(list_files, read.csv,header=FALSE)
combined_data <- rbindlist(myfiles, fill = TRUE)
View(combined_data)

cw_all_mean<-mean(combined_data[combined_data$V1=="cw"]$V2)
cw_all_sd<-sd(combined_data[combined_data$V1=="cw"]$V2)

cb_all_mean<-mean(combined_data[combined_data$V1=="cb"]$V2)
cb_all_sd<-sd(combined_data[combined_data$V1=="cb"]$V2)
cb_min<-min(combined_data[combined_data$V1=="cb"]$V2)
#upon visualising using histogram, we see 'cw' values are approximately normal distributed, hence max values should come within 2 sds of the mean
C_Classifier<-function(vec){
  output_vec=c()
  for(i in 1:length(vec)){
    if(vec[i]<(cw_all_mean+2*(cw_all_sd)) && vec[i]>(cw_all_mean-2*(cw_all_sd))){
      output_vec[i]<-'cw'
    }else{
      #Allowing 20% tolerance to exception 
      if(vec[i]>(cw_all_mean+2*(cw_all_sd))*1.2){
        output_vec[i]<-'cw'
      }else if(vec[i]<(cw_all_mean-2*(cw_all_sd)) & vec[i]>cb_min)
        output_vec[i]<-'cb'
    }
  }
  return(output_vec)
}
#Random test case 
C_Classifier(c(142.227738944532,55.2758869049999,151.539414977966, 108.671319479751,51.4554080723492, 119.711776258432))



#Classify O
# Function to predict the class based on the least sum of absolute differences
dataset_o4 <- read.csv("dataset_o4.csv", header = FALSE)  # Read the dataset from the CSV file
O_Classifier <- function(input_vector) {
  
  unique_classes <- sort(unique(dataset_o4$V1))
  class_averages <- aggregate(. ~ V1, data = dataset_o4, FUN = mean)
  class_sd<-aggregate(. ~ V1, data = dataset_o4, FUN = sd)
  total_differences <- rep(0, length(unique_classes))
  
  for (i in seq_along(unique_classes)) {
    class <- unique_classes[i]
    total_diff <- 0
    
    for (col_index in 2:9) {
      input_value <- input_vector[col_index - 1]
      class_avg <- class_averages[class_averages$V1 == class, col_index]
      abs_diff <- abs(input_value - class_avg)
      total_diff <- total_diff + abs_diff
    }
    
    total_differences[i] <- total_diff
  }
  
  min_class_index <- which.min(total_differences)
  min_difference <- total_differences[min_class_index]
  
  if (min_difference > 0.45) {
    cat("The input vector doesn't belong to any class-Returning NA")  # Return NA if the input vector doesn't belong to any class
  } else {
    predicted_class <- unique_classes[min_class_index]
    cat("The input vector belongs to class", predicted_class, "\n")
  }
}
#random test case
O_Classifier(c(0.942,0.0026,0.0078,0.0016,0.830,0.0503,1.1188,0.04393))#o3
O_Classifier(c(1.159,0.0075,0.00380,0.0103,1.078,0.127,1.04,0.085))#o3
O_Classifier(c(0.943,1.036,0.913,0.004,0.016,0.0008,0.019,0.005))#o0
O_Classifier(c(1.03,0.0005,0.008,0.01,0.045,0.756,1.08,0.0215))#o5
O_Classifier(c(1.09,1.03,0.312,0.763,0.012,0.009,0.012,0.003))#o8
O_Classifier(c(0.987,0.865,0.027,0.683,0.015,0.858,0.050,0.081))#o6
O_Classifier(c(1.091,0.957,0.264,0.005,0.0329,0.114,1.200,0.010))#o9
O_Classifier(c(1.08,0.0003,0.012,0.016,0.951,0.025,0.012,0.9388))#o2
O_Classifier(c(1.84,0.009,0.014,0.008,0.07,0.006,0.027,0.001))#o1
O_Classifier(c(1000000009.159,1111110.0075,111111110.00380,111111110.0103,1.078,0.127,1.04,0.085))#NA

