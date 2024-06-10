##### Clear memory
rm(list=ls())

##### Set working directory


library(data.table)

bank = fread(file = "bank-full.csv",
                  na.strings = c("NA", ""), 
                  sep = ";",
                  stringsAsFactors = FALSE,
                  data.table = TRUE
)
#write.csv(bank, "bank.csv")

#Checking for duplicates
dup_vector <- bank[duplicated(bank), ]
# Output duplicates
print(dup_vector)


bank$y <- ifelse(bank$y == "yes", 1, 0)

# Convert month abbreviations to numbers using ifelse
bank$month <- ifelse(bank$month == "jan", 1,
                     ifelse(bank$month == "feb", 2,
                            ifelse(bank$month == "mar", 3,
                                   ifelse(bank$month == "apr", 4,
                                          ifelse(bank$month == "may", 5,
                                                 ifelse(bank$month == "jun", 6,
                                                        ifelse(bank$month == "jul", 7,
                                                               ifelse(bank$month == "aug", 8,
                                                                      ifelse(bank$month == "sep", 9,
                                                                             ifelse(bank$month == "oct", 10,
                                                                                    ifelse(bank$month == "nov", 11, 12)))))))))))
unique(bank$job)

job_groups <- list(
  management_entrepreneur = c("management", "entrepreneur", "self-employed"),
  technical_service = c("technician", "admin.", "services"),
  low_pay = c("blue-collar", "housemaid"),
  unemployed=c("unemployed"),
  unknown=c("unknown"),
  retired = c("retired"),
  student=c("student")
)

# Create a new column for job groups
bank$job_group <- NA

# Assign job groups based on the defined categories
for (group in names(job_groups)) {
  bank$job_group[bank$job %in% job_groups[[group]]] <- group
}

# Now, let's check the unique values in the job_group column
unique(bank$job_group)
#one hot encoding for job column


encoded_jobs <- model.matrix(~ job_group - 1, data = bank)
encoded_jobs <- as.data.frame(encoded_jobs)
# Add the encoded job features to your dataset
bank <- cbind(bank, encoded_jobs)

#one hot encoding for marital column

encoded_marital <- model.matrix(~ marital - 1, data = bank)
encoded_marital <- as.data.frame(encoded_marital)
# Add the encoded job features to your dataset
bank <- cbind(bank, encoded_marital)

#one hot encoding for education column

encoded_education <- model.matrix(~ education - 1, data = bank)
encoded_education <- as.data.frame(encoded_education)
# Add the encoded job features to your dataset
bank <- cbind(bank, encoded_education)

bank$default <- ifelse(bank$y == "yes", 1, 0)
bank$housing <- ifelse(bank$y == "yes", 1, 0)
bank$loan <- ifelse(bank$y == "yes", 1, 0)

#one hot encoding for poutcome column
unique(bank$poutcome)
# Define groups for the "loan" column
poutcome_groups <- list(
  success = c("success"),
  failure=c("failure"),
  unknown = c("other", "unknown")
)

# Create a new column for loan groups
bank$poutcome_group<- NA

# Assign loan groups based on the defined categories
for (group in names(poutcome_groups)) {
  bank$poutcome_group[bank$poutcome %in% poutcome_groups[[group]]] <- group
}

# Now, let's check the unique values in the loan_group column
unique(bank$poutcome_group)




encoded_poutcome <- model.matrix(~ poutcome_group - 1, data = bank)
encoded_poutcome <- as.data.frame(encoded_poutcome)
# Add the encoded job features to your dataset
bank <- cbind(bank, encoded_poutcome)

#bank = bank[,-c(2,3,4,16)]

#Contact TBD
