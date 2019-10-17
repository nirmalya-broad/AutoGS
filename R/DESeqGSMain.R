#library(tidyverse)
#library(googledrive)
#library(googlesheets4)
#library(DESeq2)
#library(data.table)
#library(docopt)

# Function for getting value of a factor
get_factor_val <- function(factor_str) {
    factor_val <- sub("@factor\\s+(\\S+)", "\\1", factor_str)
    return (factor_val)
}

is_factor <- function(lstr) {
    lval <- grepl('^@factor\\s+', lstr)
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


#drive_auth(email = TRUE)
#sheets_auth(email = TRUE)

DESeqGS <- function(gs_id) {
    sep_str = "\t"
    #gs_id <- "1uiwd2XxdicvlaRpUizuKQ9Y4C56Q2yjeKq15KIA87HE"
    lval1 <- sheets_get(ss = gs_id)

    # There should be four tabs: metadata, samples, groups, tests

    lsheets1 <- lval1$sheets

    metadata_sheet <- read_sheet(ss = gs_id, sheet = "metadata")
    samples_sheet <- read_sheet(ss = gs_id, sheet = "samples")
    groups_sheet <- read_sheet(ss = gs_id, sheet = "groups")
    tests_sheet <- read_sheet(ss = gs_id, sheet = "tests")

    # Get the metadata



    count_file <- get_metadata_val(metadata_sheet, "count_file")
    outdir <- get_metadata_val(metadata_sheet, "outdir")
    CDS_only <- get_metadata_val(metadata_sheet, "CDS_only")
    sep_tag = get_metadata_val(metadata_sheet, "sep")

    sep_str <- "\t"
    if (sep_tag == "tab") {
        sep_str = "\t"
    } else if (sep_tag == "comma") {
        sep_str = ","
    } else {
        err_str <- paste0("Unknown sep: ", sep_tag)
        stop(err_str)
    }

    # Now get information from samples_sheet

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

    groups_sheet_nrow <- dim(groups_sheet)[1]
    groups_sheet_ncol <- dim(groups_sheet)[2]

    ref_map <- list()
    non_ref_map <- list()
    group_samples_map <- list()

    for (j in 1:groups_sheet_nrow) {
        lrow <- groups_sheet[j, ]
        lgroup_name <- lrow[["@group_name"]]
        #print(lgroup_name)

        lnon_ref_str <- lrow[["@non_ref"]]
        lnon_ref_samples <- get_group_samples(lnon_ref_str, factor_map, sample_map)
        non_ref_map[[lgroup_name]] <- lnon_ref_samples
        #print(lnon_ref)

        lref_str <- lrow[["@ref"]]
        lref_samples <- get_group_samples(lref_str, factor_map, sample_map)
        ref_map[[lgroup_name]] <- lref_samples


        condVec <- c(rep("ref", length(lref_samples)), 
            rep("non_ref", length(lnon_ref_samples)))
        colData <- data.frame(condVec)
        colnames(colData) <- c(lgroup_name)
        all_samples <- c(lref_samples, lnon_ref_samples)
        rownames(colData) <- all_samples

        group_samples_map[[lgroup_name]] <- colData
    }

    # Now process the tests_sheet

    tests_sheet_nrow <- dim(tests_sheet)[1]
    tests_sheet_ncol <- dim(tests_sheet)[2]

    count_tab <- read.csv(count_file, sep = sep_str, header = TRUE, row.names = 1,
        stringsAsFactors = FALSE, check.names = FALSE)

    for (j in 1:tests_sheet_nrow) {

        lrow <- tests_sheet[j, ]
        ldo_test1 <- lrow[["@do_test"]]
        ltest_name <- lrow[["@test_name"]]
        ltest_desc <- lrow[["@test_desc"]]

        ldo_test <-  NA
        if (is.na(ldo_test1)) {
            ldo_test <- ""
        } else {
            ldo_test <- ldo_test1
        }

        if ('no' == tolower(ldo_test)) {
        } else {
        
        
            loutdir <- paste0(outdir, "/", ltest_name)
            lprefix <- ltest_name

            dir.create(file.path(loutdir), recursive = TRUE)
            logfile <- paste0(loutdir, "/", lprefix, "_logfile.txt")
            #print(logfile)

            file.create(logfile)
            sink(logfile)

            print(ltest_name)
            print(ltest_desc)
            print("........")

            # Build the test
            colData <- getColData(ltest_desc, group_samples_map)
            lcount_tab <- getCountData(count_tab, colData, CDS_only)

            print(colData)
            design_col <- colnames(colData)
            colnames(colData) <- "condition"
            # 
            dds <- DESeqDataSetFromMatrix(countData = lcount_tab,
                colData = colData,
                design = ~ condition)


            dds$condition <- relevel(dds$condition, ref="ref")
            dds <- DESeq(dds)
            res <- results(dds)
            resOrdered <- res[order(res$padj),]
            resOrdered2 = setDT(data.frame(resOrdered), keep.rownames = TRUE)[]
            colnames(resOrdered2)[1] <- "Gene_id"

            # Write DESeq2 results and MA plots
            outfile = paste0(loutdir, "/", lprefix, ".tsv")
            write.table(resOrdered2, outfile, sep = "\t", row.names = FALSE)

            ma_pdf_file = paste0(loutdir, "/", lprefix, "_MA.pdf")
            pdf(ma_pdf_file)
            plotMA(res, ylim=c(-10,10), main = lprefix)
            dev.off()

            print("")
            sink()
        }
    }
}


