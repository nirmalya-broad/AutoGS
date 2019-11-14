exe_single_way_test <- function(ltest_name, ltest_desc, group_samples_map, 
        count_tab, outdir, CDS_only) {
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
    write.table(resOrdered2, outfile, sep = "\t", row.names = FALSE, quote = FALSE)

    ma_pdf_file = paste0(loutdir, "/", lprefix, "_MA.pdf")
    pdf(ma_pdf_file)
    plotMA(res, ylim=c(-10,10), main = lprefix)
    dev.off()

    print("")
    sink()
}

exe_tests_sheet <- function (tests_sheet, count_tab, group_samples_map, outdir,
         CDS_only, sep_str) {
    tests_sheet_nrow <- dim(tests_sheet)[1]
    tests_sheet_ncol <- dim(tests_sheet)[2]


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
        
            exe_single_way_test(ltest_name, ltest_desc, group_samples_map, 
                count_tab, outdir, CDS_only)    
        }
    }
}

DESeqGS <- function(gs_id) {

    start_str <- "https://docs.google.com/spreadsheets/d/"
    start_str_no_https <- "docs.google.com/spreadsheets/d/"
    
    gs_id_f <- NA
    if (startsWith(gs_id, start_str)) {
        # Hope that the entire url is copy pasted 
        pat_str <- "^https://docs.google.com/spreadsheets/d/(\\w+?)/edit.*$"
        gs_id_f = sub(pat_str, "\\1", gs_id)
    } else if (startsWith(gs_id, start_str_no_https)) {
        pat_str <- "^docs.google.com/spreadsheets/d/(\\w+?)/edit.*$"
        gs_id_f <- sub(pat_str, "\\1", gs_id)
    }else {
        # Hope that the entire gs_id is the actual googledoc id
        gs_id_f <- gs_id
    }

    lval1 <- sheets_get(ss = gs_id_f)

    # There should be four tabs: metadata, samples, groups, tests

    lsheets1 <- lval1$sheets

    metadata_sheet <- read_sheet(ss = gs_id_f, sheet = "metadata")
    samples_sheet <- read_sheet(ss = gs_id_f, sheet = "samples")
    groups_sheet <- read_sheet(ss = gs_id_f, sheet = "groups")
    tests_sheet <- read_sheet(ss = gs_id_f, sheet = "tests")

    # Get the metadata

    count_file <- get_metadata_val(metadata_sheet, "count_file")
    outdir <- get_metadata_val(metadata_sheet, "outdir")
    CDS_only <- get_metadata_val(metadata_sheet, "CDS_only")
    sep_tag = get_metadata_val(metadata_sheet, "sep")

    sep_str <- NA
    if (sep_tag == "tab") {
        sep_str = "\t"
    } else if (sep_tag == "comma") {
        sep_str = ","
    } else {
        err_str <- paste0("Unknown sep: ", sep_tag)
        stop(err_str)
    }

    count_tab <- read.csv(count_file, sep = sep_str, header = TRUE, 
        row.names = 1, stringsAsFactors = FALSE, check.names = FALSE)
    all_sample_names <- colnames(count_tab)

    # 1. Get information from samples_sheet

    ret_val1 <- exe_samples_sheet(samples_sheet, all_sample_names)
    factor_map <- ret_val1$factor_map
    sample_map <- ret_val1$sample_map

    # 2. Load information from groups_sheet

    ret_val2 <- exe_groups_sheet(groups_sheet, factor_map = factor_map, 
        sample_map = sample_map)
    group_samples_map <- ret_val2$group_samples_map

    # 3. Process the tests_sheet
    
    exe_tests_sheet(tests_sheet, count_tab, group_samples_map, outdir, 
        CDS_only, sep_str)

}


