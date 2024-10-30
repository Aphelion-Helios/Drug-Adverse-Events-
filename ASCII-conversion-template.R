#### Set up ####
remove(list = ls())
library(speedycode)
library(readroper)
library(dplyr)
library(sjlabelled)
library(labelled)
library(haven)
library(devtools)
library(stringr)
library(purrr)

# IMPORTANT #
# You can try to read the file from online but that can be tricky with ASCII files for a number of reasons. I recommend downloading the file and uploading from a directory in your computer.

#### Card 1 ####
card1 <- read_rpr(
  col_positions = c(),
  widths = c(),
  col_names = c(),
  filepath = "~",
  card_read = ,
  cards = 
)

#### Card 1 question labels ####
card1 <- card1 %>% 
  mutate_all(as.numeric)
  
# The speedy_labels function will provide you with the code you need, but I also provide a code template below for labelling if you choose to do it manually. 
speedy_labels(card2,
              nrows = 5,
              path = "~/SAVE-TO-FILE")

card1 <- card1 %>% 
  set_variable_labels(VARNAME = "VARLABEL",
                      VARNAME = "VARLABEL",
                      VARNAME = "VARLABEL",
                      VARNAME = "VARLABEL",
                      VARNAME = "VARLABEL"
                      # AND SO ON
                      )

#### Card 1 value labels ####
card1 <- card1 %>% 
  set_value_labels(VARNAME = c(
    "NEW_LABEL" = 99999,
    "NEW_LABEL" = 99999,
    "NEW_LABEL" = 99999),
    VARNAME = c(
      "NEW_LABEL" = 99999,
      "NEW_LABEL" = 99999,
      "NEW_LABEL" = 99999),
    VARNAME = c(
      "NEW_LABEL" = 99999,
      "NEW_LABEL" = 99999,
      "NEW_LABEL" = 99999),
    VARNAME = c(
      "NEW_LABEL" = 99999,
      "NEW_LABEL" = 99999,
      "NEW_LABEL" = 99999)
  )
# AND SO ON 

#### Card 2 ####
card1 <- read_rpr(
  col_positions = c(),
  widths = c(),
  col_names = c(),
  filepath = "~",
  card_read = ,
  cards = 
)

#### Card 2 question labels ####
card2 <- card2 %>% 
  mutate_all(as.numeric)

# The speedy_labels function will provide you with the code you need, but I also provide a code template below for labelling if you choose to do it manually. 
speedy_labels(card2,
              nrows = 5,
              path = "~/SAVE-TO-FILE")

card2 <- card2 %>% 
  set_variable_labels(VARNAME = "VARLABEL",
                      VARNAME = "VARLABEL",
                      VARNAME = "VARLABEL",
                      VARNAME = "VARLABEL",
                      VARNAME = "VARLABEL"
                      # AND SO ON
  )

#### Card 2 value labels ####
card2 <- card2 %>% 
  set_value_labels(VARNAME = c(
    "NEW_LABEL" = 99999,
    "NEW_LABEL" = 99999,
    "NEW_LABEL" = 99999),
    VARNAME = c(
      "NEW_LABEL" = 99999,
      "NEW_LABEL" = 99999,
      "NEW_LABEL" = 99999),
    VARNAME = c(
      "NEW_LABEL" = 99999,
      "NEW_LABEL" = 99999,
      "NEW_LABEL" = 99999),
    VARNAME = c(
      "NEW_LABEL" = 99999,
      "NEW_LABEL" = 99999,
      "NEW_LABEL" = 99999)
    # AND SO ON 
  )

#### Merge cards ####
df <- full_join(card1, card2, by = "respondent_id")
# OR, if no unique identifier
df <- cbind(card1,card2)
  
#### Save out data ####
write_dta(df, "~/")

#### Final comments ####
# Most ASCII conversions won't be quite so simple as this outline suggests. For instance, if there are errors in the documentation of characters instead of numbers you will need to make some minor adjustments. Following this outline, however, will get you almost all of the way there for most ASCII conversions.