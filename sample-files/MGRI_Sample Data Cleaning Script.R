
### MGRI Data Cleaning Toolkit Ver. 1.0.0. Updated on July 00, 2023
### Please review the handbook at https://ccwt.wisc.edu/applied-research/nca/ to learn details on the script
### Please cite below if you found this toolkit for your research and publication. Thank you!
### Citation: Jang-Tucci, K. (2023). A Step-by-Step Guide for Implementing the Multiple Generator Random Interpreter (MGRI) in Online Surveys. Center for Research on College-Workforce Transitions. University of Wisconsin-Madison. 


raw <- read.csv('insert file path here') 
raw <- replace(raw, raw == '', NA)
raw$ego_ID <- 10000+1:nrow(raw)

# Creating an index for identifying questions and ChoiceTextEntry values.
qid_varnames = c("SN_IM_name1", "SN_IM_name2", "SN_IM_name3", "SN_AC_name1", "SN_AC_name2", "SN_AC_name3") # Insert variable names of the name generators.
qid_varqids = c("QID49", "QID126", "QID127", "QID129", "QID130", "QID131") # Insert QIDs of the name generators.
a <- 5  # Insert the number of fields available in each name generator.
b <- 6 # Insert the number of name generator questions.
c <- a*b
qid <- data.frame(matrix(ncol = 3, nrow = c)) # Creates a data frame that has three columns and "c (=a*b)" number of rows.
colnames(qid) <- c("QID1", "QID2", "QID3") # Assign column names to be QID1, QID2, QID3 each.
counter <- 1 # designating the first row to write the generated data (see following script)

for (m in 1:b) { 
  for (n in 1:a){
    qid$QID1[counter] <- paste0(qid_varnames[m], "_", n) # Each row of QID1 column will have "variable name(e.g., Q1_NG_IM1)"_n (number)
    qid$QID2[counter] <- counter 
    qid$QID3[counter] <- paste0("{q://", qid_varqids[m], "/ChoiceTextEntryValue/", n, "}")
    counter <- counter + 1
  }
}

for (i in seq_len(nrow(qid))) {
  qid1_val <- qid[i, "QID1"]
  qid2_val <- qid[i, "QID2"]
  qid3_val <- qid[i, "QID3"]
}
# Creating an index for identifying density questions and numbers
k <- nrow(qid)
h <- which(colnames(raw) == "SN_density_IM_1_1_1") # Insert the first density question's variable name here.

data <- list()
  for (i in 1:(k-1)) {
    for (j in (i+1):k) {
      data <- c(data, list(c(i, j)))
    }
  }
  densityid <- do.call(rbind, data)
  colnames(densityid) <- c('fromid', 'toid')

densityid <- generate_densityid(k)
densityid <- as.data.frame(densityid)
densityid$densityqid <- colnames(raw)[h:(h-1+((k*(k-1))/2))]

# Create an empty data frame "alter." Revise the structure to fit your data.
alter_raw <- data.frame(
  response_ID = character(),
  ego_ID = numeric(),
  alter_name = character(),
  alter_ID_char = character(),
  alter_ID = numeric(),
  alter_relationship = character(),
  alter_gender = character(),
  alter_group = character(),
  alter_sampled = character(),
  stringsAsFactors = FALSE
)

# Create an empty data frame "alter_temp." Revise the structure to fit your data.
num <- nrow(raw)
alter_temp <- data.frame(
  response_ID = character(num),
  ego_ID = numeric(num),
  alter_name = character(num),
  alter_ID_char = character(num),
  alter_ID = numeric(num),
  alter_relationship = character(num),
  alter_gender = character(num),
  alter_group character(num)
  alter_sampled = character(num),
  stringsAsFactors = FALSE
)

# Identifying variables & recording the responses. Note: Adjust the column/variable names (e.g., raw$SN_showsample_DO) to align with your dataset.  
  if (!is.null(raw[[qid1_val]])) {
    alter_temp$response_ID <- raw$ResponseId # Stores Qualtrics response IDs
    alter_temp$ego_ID <- raw$ego_ID # Stores ego IDs generated in the previous step
    alter_temp$alter_name <- raw[[qid1_val]] # Stores names listed in the name genertors
    alter_temp$alter_ID_char <- paste(raw$ResponseId, "_alter", qid2_val, sep = "") # Assigns alter IDs in charater (responde ID_alter_number)
    alter_temp$alter_ID <- paste((raw$ego_ID) * 100 + qid2_val, sep = "") # Assigns alter IDs in numeric values ("egoID"*100 + number)
    alter_temp$alter_relationship <- raw[[paste("SN_relationship_", qid2_val, sep = "")]] # Stores information provided in the "SN_relationship" column.
    alter_temp$alter_gender <- raw[[paste("SN_gender_", qid2_val, sep = "")]] # Stores information provided in the "SN_gender" column.
    alter_temp$alter_group <- ifelse(!is.na(raw$Q3_NG_IM_AC_select & raw$Q3_NG_IM_AC_select!= "" & grepl(paste0("\\$\\", qid3_val), raw$Q3_NG_IM_AC_select), "IMAC", ifelse(qid2_val > (a*(b-1)), "AC", "IM")) # Stores information about alter's group affiliation (e.g., 'important matters', 'academic and career matters', or both)
    alter_temp$alter_sampled <- ifelse(!is.na(raw$SN_showsample_DO) & raw$SN_showsample_DO != "" & grepl(paste0("\\$\\", qid3_val), raw$SN_showsample_DO), "Yes", "No") # Stores information about whether the alter is sampled for the name interpreters.
    alter_raw <- rbind(alter_raw, alter_temp)
  }

alter_raw <- replace(alter_raw, alter_raw=='', NA)
alter_clean <- alter_raw[complete.cases(alter_raw$alter_name), ]

# Creating density dataset
alter_ties_raw <- data.frame(
  response_ID = character(),
  ego_ID = numeric(),
  from = character(),
  to = character(),
  weight = numeric(),
  stringsAsFactors = FALSE
)

alter_ties_temp <- data.frame(
  response_ID = character(num),
  ego_ID = numeric(num),
  from = character(num),
  to = character(num),
  weight = numeric(num),
  stringsAsFactors = FALSE
)


for (j in seq_len(nrow(densityid))) {
  denid_val1 <- densityid[j, "densityqid"]
  denid_val2 <- densityid[j, "fromid"]
  denid_val3 <- densityid[j, "toid"]
  if (!is.null(raw[[denid_val1]])) {
    alter_ties_temp$response_ID <- raw$ResponseId
    alter_ties_temp$ego_ID <- raw$ego_ID
    alter_ties_temp$from <- ifelse(!is.na(raw[[denid_val1]]), paste((raw$ego_ID * 100) + denid_val2, sep = ""), NA)
    alter_ties_temp$to <- ifelse(!is.na(raw[[denid_val1]]), paste((raw$ego_ID * 100) + denid_val3, sep = ""), NA)
    alter_ties_temp$weight <- ifelse(!is.na(raw[[denid_val1]]), 1, NA)
    alter_ties_raw <- rbind(alter_ties_raw, alter_ties_temp)
  }
}
alter_ties_raw <- replace(alter_ties_raw, alter_ties_raw == '', NA)
alter_ties_clean <- alter_ties_raw[complete.cases(alter_ties_raw$weight), ]

# Saving the created datasets
ego <- raw[, c("ego_ID", "ResponseId")]
colnames(ego) <- c("ego_ID", response_ID")
write.csv(ego, "ego_data.csv")
write.csv(alter_clean, "alter_attr.csv")
write.csv(alter_ties_clean, "alter_ties.csv")
