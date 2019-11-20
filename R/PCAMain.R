# Customized code for updating color, shape and size to the input plot.

append_factor_to_df <- function(pca_df, aes_type_str, aes_sheet, map_factors) {

    lrow1 <- aes_sheet[aes_sheet[, "@aes_type"] == aes_type_str,]
    row_count <- dim(lrow1)[1]
    if (row_count) {
        lfactor_name <- lrow1["@factor_name"][[1]]
        lfactor <- map_factors[[lfactor_name]]
        pca_df[, aes_type_str] <- lfactor
    }

    return (pca_df)
}


exe_aes_sheet <- function(aes_sheet, all_sample_names, factor_map, sample_map) {

    
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
    aes_sheet <- read_sheet(ss = gs_id_f, sheet = "aes")

    # Get the metadata

    count_file <- get_metadata_val(metadata_sheet, "count_file")
    outdir <- get_metadata_val(metadata_sheet, "outdir")
    viz_type <- get_metadata_val(metadata_sheet, "viz_type")
    PCA_scale_str <- get_metadata_val(metadata_sheet, "PCA_scale")
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

    PCA_scale <- as.logical(toupper(PCA_scale_str))

    count_tab <- read.csv(count_file, sep = sep_str, header = TRUE, 
        row.names = 1, stringsAsFactors = FALSE, check.names = FALSE)
    all_sample_names <- colnames(count_tab)

    # 1. Get information from samples_sheet

    ret_val1 <- exe_samples_sheet(samples_sheet, all_sample_names)
    factor_map <- ret_val1$factor_map
    sample_map <- ret_val1$sample_map

    # 2. Get map of factors

    map_factors <- exe_map_factors(samples_sheet)

    # 3. do PCA
    
    pca_tab <- t(count_tab)
    df_pca1 <- prcomp(pca_tab, scale. = PCA_scale)
    pca_df1 <- as.data.frame(df_pca1$x)

    # 4. Append factors

    pca_df2 <- append_factor_to_df(pca_df1, "color", aes_sheet, map_factors)
    pca_df3 <- append_factor_to_df(pca_df2, "shape", aes_sheet, map_factors)
    pca_df <- append_factor_to_df(pca_df3, "size", aes_sheet, map_factors)


    # 5. Start building ggplot2

    plt <- ggplot(pca_df, aes(x = PC1, y = PC2, color = color))

    plt2 <- plt + geom_point(size = 2)

    
}


