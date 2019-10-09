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
groups_sheet <- read_sheet(ss = count_file_id, sheet = "groups")
tests_sheet <- read_sheet(ss = count_file_id, sheet = "tests")

# Get the metadata

get_metadata_val <- function(lsheet, lname) {
    lval1 <- lsheet[lsheet$`@name` == lname,]$`@value`
    lval2 <- lval1[[1]][1]
    return (lval2)
}

count_file <- get_metadata_val(metadata_sheet, "count_file")
outdir <- get_metadata_val(metadata_sheet, "outdir")
CDS_only <- get_metadata_val(metadata_sheet, "CDS_only")

# Now get information from samples_sheet

samples_sheet_nrow <- dim(samples_sheet)[1]
samples_sheet_ncol <- dim(samples_sheet)[2]
samples_sheet_cols <- colnames(samples_sheet)

factor_sets <- grep('@factor', samples_sheet_cols, value = TRUE)

# Function for getting value of a factor
get_factor_val <- function(factor_str) {
    factor_val <- sub("@factor\\s+(\\S+)", "\\1", factor_str)
    return (factor_val)
}

is_factor <- function(lstr) {
    lval <- grepl('@factor', lstr)
    return (lval)
}

is_sample_name <- function(lstr) {
    lval <- grepl('@sample_name', lstr)
    return (lval)
}

get_sample_name <- function(lstr) {
    lval <- lstr[['@sample_name']]
    return (lval)
}

factor_map <- list()
sample_map <- list()

for (j in 1:samples_sheet_nrow) {
    lrow <- samples_sheet[j, ]
    sample_name <- get_sample_name(lrow)

    col_names <- names(lrow)

    lsample <- lrow[['@sample_name']]
    for (col_name in col_names) {
        if (is_factor(col_name)) {
            factor_val <- lrow[[col_name]]
            if (!is.na(factor_val)) {
                col_name_tr <- sub("\\S+\\s+(\\S+)", "\\1", col_name)
                full_factor_val <- paste0(col_name_tr, '::', factor_val)
                if (full_factor_val %in% names(sample_map)) {
                    lval1 <- sample_map[[full_factor_val]]
                    if (!(lsample %in% lval1)) {
                        sample_map[[full_factor_val]] <- c(lval1, lsample)
                    }
                } else {
                    sample_map[[full_factor_val]] <- lsample
                }
                
                if (factor_val %in% names(factor_map)) {
                    lval <- factor_map[[factor_val]]
                    if (lval != full_factor_val) {
                        # Throw an exception
                        err_str <- paste0("Conflict of factors: ", lval, " ; ", full_factor_val)
                        stop(err_str)
                        
                    }
                } else {
                    factor_map[[factor_val]] <- full_factor_val
                } 
            }
        }
    }
}

# Now load information from groups_sheet
# We shall have two maps here; for non_ref and for ref

ref_map <- list()
no_ref_map <- list()


groups_sheet_nrow <- dim(groups_sheet)[1]
groups_sheet_ncol <- dim(groups_sheet)[2]


# At this moment the assumption is that:
#    the group_str would just contain a single predicate or predicates
#    connected with AND; if in the future there are other kinds of predicates
#    we shall see.

get_group_samples <- function() {



}

for (j in 1:groups_sheet_nrow) {
    lrow <- groups_sheet[j, ]
    lgroup_name <- lrow[["@group_name"]]
    print(lgroup_name)

    lnon_ref_str <- lrow[["@non_ref"]]
    lnon_ref_samples <- get_group_samples(lnon_ref_str)\
    non_ref_map[[lgroup_name]] <- lnon_ref_samples
    print(lnon_ref)

    lref_str <- lrow[["@ref"]]
    lref_samples <- get_group_samples(lref_str)
    ref_map[[lgroup_name]] <- lref_samples

    print(lref)
    print("....")
}







