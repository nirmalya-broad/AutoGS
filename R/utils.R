# Function for getting value of a factor
get_factor_val <- function(factor_str) {
    factor_val <- sub("@factor\\s+(\\S+)", "\\1", factor_str)
    return (factor_val)
}

is_factor <- function(lstr) {
    lval <- grepl('@factor\\s+', lstr)
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

get_sheet_id <- function(lsheets, sheet_name) {
    lid <- lsheets[lsheets$name == sheet_name, ]$id
    return (lid)
}

get_metadata_val <- function(lsheet, lname) {
    lval1 <- lsheet[lsheet$`@name` == lname,]$`@value`
    lval2 <- lval1[[1]][1]
    return (lval2)
}

get_cond_samples <- function(lcond, factor_map, sample_map) {
    lfactor <- factor_map[[lcond]]
    lsamples <- sample_map[[lfactor]]
    return (lsamples)
}


# At this moment the assumption is that:
#    the group_str would just contain a single predicate or predicates
#    connected with AND; if in the future there are other kinds of predicates
#    we shall see.

get_group_samples <- function(lgroup_name, factor_map, sample_map) {

    # This would get all the predicates under AND clause and shall do an
    # intersection over the samples

    if (!grepl("AND", lgroup_name, ignore.case = TRUE)) {

        # Remove leading and trailing whitespace, if any
        lgroup_arr <- gsub("^\\s*|\\s*$", "", lgroup_name)

    } else {

        # Otherwise the format is AND (a1, b1, c1)
        # 1. get rid of AND
        # 2. get rid of ()
        # 3. remove leading and training space if any
        # 4. split with respect to space and comma (,)

        lval2 <- sub("AND", "", lgroup_name, ignore.case = TRUE)
        lval3 <- gsub("\\(|\\)", "", lval2)
        lval4 <- gsub("^\\s*|\\s*$", "", lval3)
        lgroup_arr <- strsplit(lval4, split = "\\s*,\\s*")[[1]]
        
    }   

    lgroup_paste <- paste(lgroup_arr, collapse = ", ")
    #print(paste0(lgroup_name, " --> ", lgroup_paste))

    sample_lst1 <- lapply(lgroup_arr, get_cond_samples, factor_map, sample_map)
    sample_names <- Reduce(intersect, sample_lst1)
    return (sample_names)
}

getColData <- function(ltest_desc, group_samples_map) {

    # 1. Assumping it's a single factor test get the group_name
    lgroup_name <- sub("~\\s*(\\S+)\\s*", "\\1", ltest_desc)
    lsample_df <- group_samples_map[[lgroup_name]]
    return (lsample_df)
}

getCountData <- function(count_tab, colData, CDS_only) {
    lsamples <- rownames(colData)
    ldata <- count_tab[, lsamples]
    ldata_final <- NA
    if (CDS_only) {
        lrows1 <- rownames(ldata)
        lrows_CDS <- grep("^CDS", lrows1, value = TRUE)
        ldata_final <- ldata[lrows_CDS, ]
    } else {
        ldata_final <- ldata
    }
    return (ldata_final)
}


exe_map_factors <- function(samples_sheet) {
    samples_sheet_nrow <- dim(samples_sheet)[1]
    samples_sheet_ncol <- dim(samples_sheet)[2]
    samples_sheet_cols <- colnames(samples_sheet)

    factor_sets <- grep('@factor', samples_sheet_cols, value = TRUE)

    map_factors <- list()

    lsample_name <- samples_sheet[, "@sample_name"]

    for (lfactor_name in factor_sets) {
        lfactor1 <- (samples_sheet[, lfactor_name])[[1]]

        names(lfactor1) <- lsample_name[[1]]
        lfactor <- addNA(lfactor1)
        map_factors[[lfactor_name]] <- lfactor
        
    }

    return (map_factors)

}


exe_samples_sheet <- function(samples_sheet, all_sample_names) {

    samples_sheet_nrow <- dim(samples_sheet)[1]
    samples_sheet_ncol <- dim(samples_sheet)[2]
    samples_sheet_cols <- colnames(samples_sheet)

    factor_sets <- grep('@factor', samples_sheet_cols, value = TRUE)

    factor_map <- list()
    sample_map <- list()

    for (j in 1:samples_sheet_nrow) {
        lrow <- samples_sheet[j, ]
        sample_name <- get_sample_name(lrow)

        col_names <- names(lrow)

        lsample_tag <- lrow[['@sample_name']]
        
        # Expand it to sample names
        lsamples <- grep(lsample_tag, all_sample_names, value = TRUE)
        print("....")
        print(lsample_tag)
        print(lsamples)
        
        for (col_name in col_names) {
            if (is_factor(col_name)) {
                factor_val <- lrow[[col_name]]
                if (!is.na(factor_val)) {
                    col_name_tr <- sub("\\S+\\s+(\\S+)", "\\1", col_name)
                    full_factor_val <- paste0(col_name_tr, '::', factor_val)
                    if (full_factor_val %in% names(sample_map)) {
                        lval1 <- sample_map[[full_factor_val]]
                        if (!any((lsamples %in% lval1))) {
                            sample_map[[full_factor_val]] <- c(lval1, lsamples)
                        } else {
                            err_str <- paste0("Conflicting sample_tag: ", lsample_tag)
                            stop(err_str)
                        }
                    } else {
                        sample_map[[full_factor_val]] <- lsamples
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

    ret_lst <- list("factor_map" = factor_map, "sample_map" = sample_map)
    return (ret_lst)
}


exe_groups_sheet <- function(groups_sheet, factor_map, sample_map) {
    groups_sheet_nrow <- dim(groups_sheet)[1]
    groups_sheet_ncol <- dim(groups_sheet)[2]

    group_samples_map <- list()

    for (j in 1:groups_sheet_nrow) {
        lrow <- groups_sheet[j, ]
        lgroup_name <- lrow[["@group_name"]]

        lnon_ref_str <- lrow[["@non_ref"]]
        lnon_ref_samples <- get_group_samples(lnon_ref_str, factor_map, 
            sample_map)

        lref_str <- lrow[["@ref"]]
        lref_samples <- get_group_samples(lref_str, factor_map, sample_map)

        condVec <- c(rep("ref", length(lref_samples)), 
            rep("non_ref", length(lnon_ref_samples)))
        colData <- data.frame(condVec)
        colnames(colData) <- c(lgroup_name)
        all_samples <- c(lref_samples, lnon_ref_samples)
        rownames(colData) <- all_samples

        group_samples_map[[lgroup_name]] <- colData
    }

    ret_lst <- lst("group_samples_map" = group_samples_map)
    return (ret_lst)

}


get_normalized_data <- function(infile, sep_str, CDS_only, do_vst) {
    ltab1 <- read.table(infile, sep = sep_str, header = TRUE, row.names = 1, stringsAsFactors = FALSE, check.names = FALSE)
    # Get the genes with CDS only and ignore the other tags.
    rnames1 <- rownames(ltab1)
    ltab2 <- NA
    if (CDS_only) {
        rnames <- rnames1[grep("^CDS", rnames1)]
        ltab2 <- ltab1[rnames, ]
    } else {
        ltab2 <- ltab1
    }

    ltab3 <- ltab2
    ltab3[is.na(ltab2)] <- 0
    ltab4 <- as.matrix(ltab3)
    rownames(ltab4) <- rownames(ltab3)
    colnames(ltab4) <- colnames(ltab3)

    ltab4_vt <- NA
    if (do_vst) {
        ltab4_vt <- vst(ltab4, blind = FALSE)
    } else {
        ltab4_vt <- ltab4
    }

    return (ltab4_vt)
}

