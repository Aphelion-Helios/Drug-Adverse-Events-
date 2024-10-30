#### Set up ####
rm(list = ls())

#### speedy_rename ####
speedy_rename <- function(data, path = "") {
  require(stringr)

  # This automates the code based on the provided dataset
  line1 <- paste("data"," <- ", "data", " %>%", sep = "")
  line2a <- "rename("
  line2b <- paste('NEW_NAME', " = ", colnames(data[1]), ",",  sep = "")
  line3 <- paste('NEW_NAME', " = ", colnames(data[c(-1, -length(data))]), ",", sep = "")
  line4 <- paste('NEW_NAME', " = ", colnames(data[length(data)]), "\n", ")", sep = "")
  newcode <- paste(line3, "\n", sep = "")


  # This prints the code in the console
  cat(line1, "\n", line2a, "\n", line2b, "\n", newcode, line4)


  # This throws an error if the code is saved out to a file that is NOT an R script
  final_path_char <- stringr::str_sub(path,-2,-1)
  if (stringr::str_detect(path, ".") && final_path_char != ".R") {
    stop('Must specify ".R" at the end of the path to save formatted code to an R script file')
  }


  # This writes the code to a separate R script
  suppressWarnings(capture.output(cat(line1, "\n", line2a, "\n", line2b, "\n", newcode, line4), file = path))
}

#### speedy_classes ####
speedy_classes <- function(data, path = "") {
  require(stringr)

  # This automates the code based on the provided dataset
  class_list <- unlist(lapply(data, class))
  class_list <- as.data.frame(class_list)
  line1 <- paste("data"," <- ", "data", " %>%", sep = "")
  line2a <- "mutate("
  line2b <- paste(colnames(data[1]), " = ", "as.", class_list[1,], "(", colnames(data[1]), ")", ",",  sep = "")
  line3 <- paste(colnames(data[c(-1, -length(data))]), " = ", "as.", class_list[2:(nrow(class_list)-1),], "(", colnames(data[c(-1, -length(data))]), ")", ",", sep = "")
  line4 <- paste(colnames(data[length(data)]), " = ", "as.", class_list[nrow(class_list),], "(", colnames(data[length(data)]), ")\n)", sep = "")
  newcode <- paste(line3, "\n", sep = "")


  # This prints the code in the console
  cat(line1, "\n", line2a, "\n", line2b, "\n", newcode, line4)


  # This throws an error if the code is saved out to a file that is NOT an R script
  final_path_char <- stringr::str_sub(path,-2,-1)
  if (stringr::str_detect(path, ".") && final_path_char != ".R") {
    stop('Must specify ".R" at the end of the path to save formatted code to an R script file')
  }


  # This writes the code to a separate R script
  suppressWarnings(capture.output(cat(line1, "\n", line2a, "\n", line2b, "\n", newcode, line4), file = path))
}

#### speedy_labels (only need to remove the pesky comma at the end) ####
speedy_labels <- function(data, nrows = 5, path = "") {
  require(purrr)
  require(dplyr)
  require(stringr)


  # This creates the code for labelling the variables
  qline1 <- paste("data"," <- ", "data", " %>%", sep = "")
  qline2a <- "set_variable_labels("
  qline2b <- paste(colnames(data[1]), " = ", '"QTEXT_HERE",', sep = "")
  qline3 <- paste(colnames(data[c(-1, -length(data))]), " = ", '"QTEXT_HERE"', ",", sep = "")
  qline4 <- paste(colnames(data[length(data)]), " = ", '"QTEXT_HERE"', ")", sep = "")
  qnewcode <- paste(qline3, "\n", sep = "")


  # This pastes the code for labelling the variables in the console
  cat(qline1, "\n", qline2a, "\n", qline2b, "\n", qnewcode, qline4, " %>% ", "\n", "set_value_labels(", "\n")


  # This creates the code for labelling the values for each questions
  # line1 <- paste(" %>% ", "set_value_labels(", "\n", sep = "")

  val_code <- function(num) {

    new_data <- data %>%
      dplyr::select({{num}})

    new_data <- new_data %>%
      dplyr::arrange(new_data[1])

    if (nrows == 2) {
      main_body <- paste(colnames(new_data[1]), " = ", "c(", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[1,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[2,], "\n", ")", ",", "\n",
                          sep = "")
    } else if (nrows == 3) {
      main_body <- paste(colnames(new_data[1]), " = ", "c(", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[1,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[2,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[3,], "\n", ")", ",", "\n",
                          sep = "")
    } else if (nrows == 4) {
      main_body <- paste(colnames(new_data[1]), " = ", "c(", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[1,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[2,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[3,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[4,], "\n", ")", ",", "\n",
                          sep = "")
    } else if (nrows == 5) {
      main_body <- paste(colnames(new_data[1]), " = ", "c(", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[1,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[2,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[3,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[4,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[5,], "\n", ")", ",", "\n",
                          sep = "")
    } else if (nrows == 6) {
      main_body <- paste(colnames(new_data[1]), " = ", "c(", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[1,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[2,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[3,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[4,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[5,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[6,], "\n", ")", ",", "\n",
                          sep = "")
    } else if (nrows == 7) {
      main_body <- paste(colnames(new_data[1]), " = ", "c(", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[1,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[2,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[3,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[4,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[5,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[6,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[7,], "\n", ")", ",", "\n",
                          sep = "")
    } else if (nrows == 8) {
      main_body <- paste(colnames(new_data[1]), " = ", "c(", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[1,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[2,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[3,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[4,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[5,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[6,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[7,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[8,], "\n", ")", ",", "\n",
                          sep = "")
    } else if (nrows == 9) {
      main_body <- paste(colnames(new_data[1]), " = ", "c(", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[1,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[2,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[3,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[4,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[5,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[6,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[7,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[8,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[9,], "\n", ")", ",", "\n",
                          sep = "")
    } else if (nrows == 10) {
      main_body <- paste(colnames(new_data[1]), " = ", "c(", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[1,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[2,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[3,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[4,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[5,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[6,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[7,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[8,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[9,], ",", "\n",
                          "NEW_LABEL", " = ", unique(new_data[1])[10,], "\n", ")", ",", "\n",
                          sep = "")
    } else {
      stop("Must choose a number between 2 and 10")
    }

    # This prints the value labeling code to the console

    cat(main_body)
  }

  purrr::map_dfr(1:length(data),val_code)

  cat(")")


  # This throws an error if the code is saved out to a file that is NOT an R script
  final_path_char <- stringr::str_sub(path,-2,-1)
  if(stringr::str_detect(path, ".") && final_path_char != ".R") {
    stop('Must specify ".R" at the end of the path to save formatted code to an R script file')
  }


  # This writes the code to a separate R script
  suppressWarnings(capture.output(cat(qline1, "\n", qline2a, "\n", qline2b, "\n", qnewcode, qline4, " %>% ", "\n", "set_value_labels(", "\n"), purrr::map_dfr(1:length(data),val_code), file = path))

}

