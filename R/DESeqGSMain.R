library(tidyverse)
library(googledrive)
library(googlesheets4)

#drive_auth(email = TRUE)
#sheets_auth(email = TRUE)

count_file_id <- "1uiwd2XxdicvlaRpUizuKQ9Y4C56Q2yjeKq15KIA87HE"
lval1 <- sheets_get(ss = count_file_id)

# There should be four tabs: metadata, samples, groups, tests

lsheets1 <- lval1$sheets

get_sheet_id <- function(lsheets, sheet_name) {
    lid <- lsheets[lsheets$name == sheet_name, ]$id
    return (lid)
}

metadata_sheet <- read_sheet(ss = count_file_id, sheet = "metadata")
samples_sheet <- read_sheet(ss = count_file_id, sheet = "samples")
groups_sid <- read_sheet(ss = count_file_id, sheet = "groups")
tests_sid <- read_sheet(ss = count_file_id, sheet = "tests")

# Get the metadata

get_metadata_val <- function(lsheet, lname) {
    lval1 <- lsheet[lsheet$`@name` == lname,]$`@value`
    lval2 <- lval1[[1]][1]
    return (lval2)
}

count_file <- get_metadata_val(metadata_sheet, "count_file")
outdir <- get_metadata_val(metadata_sheet, "outdir")
CDS_only <- get_metadata_val(metadata_sheet, "CDS_only")




