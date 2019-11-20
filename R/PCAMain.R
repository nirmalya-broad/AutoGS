# Customized code for updating color, shape and size to the input plot.

append_factor <- function(pca_df, aes_type_str, aes_sheet, map_factors) {

    lrow1 <- aes_sheet[aes_sheet[, "@aes_type"] == aes_type_str,]
    row_count <- dim(lrow1)[1]
    if (row_count) {
        lfactor_name <- lrow1["@factor_name"][[1]]
        lfactor <- map_factors[[lfactor_name]]
        pca_df[, aes_type_str] <- lfactor
    }

    return (pca_df)
}


append_aes_map <- function(lplt1, laes_type, aes_types, aes_sheet) {

    lplt2 <- NA
    if (laes_type == "color") {
        lplt2 <- append_color_map(lplt1, aes_types, aes_sheet)
    } else if (laes_type == "shape") {
        lplt2 <- append_shape_map(lplt1, aes_types, aes_sheet)
    } else if (laes_type == "size") {
        lplt2 <- append_size_map(lplt1, aes_types, aes_sheet)
    } else {
        err_str <- paste0("Illegal aes_type", laes_type)
        stop(err_str)
    }

    return (lplt2)
}

append_color_map <- function(lplt1, aes_types, aes_sheet) {

    lplt2 <- NA
    if ("color" %in% aes_types) {
    
        # Get the palette and create the aesthetic mapping
        lrow1 <- aes_sheet[aes_sheet[, "@aes_type"] == "color", ]
        
        if (lrow1$`@aes_assign_proc` == "palette") {
            # get the name of the palette
            palette_val <- lrow1$`@aes_assign_value`

            lplt2 <- lplt1 + scale_colour_brewer(palette = palette_val) 
            
        } else {
            err_str <- paste0("aes color format not supported")
            stop(err_str)
        }   

    } else {

        lplt2 <- lplt1
    }

    return (lplt2)
}

append_shape_map <- function(lplt1, aes_types, aes_sheet) {

    lplt2 <- NA
    if ("shape" %in% aes_types) {

        lrow1 <- aes_sheet[aes_sheet[, "@aes_type"] == "shape", ]
        
        if (lrow1$`@aes_assign_proc` == "list") {
            # get the name of the palette
            shape_val1 <- lrow1$`@aes_assign_value`
            shape_val2 <- trim(shape_val1)
            shape_vals <- strsplit(shape_val2, split = "\\s*,\\s*", perl = TRUE) 

            lplt2 <- lplt1 + scale_shape_manual(values = shape_vals) 
            
        } else {
            err_str <- paste0("aes color format not supported")
            stop(err_str)
        }   

    } else {

        lplt2 <- lplt1
    }

    return (lplt2)
}

append_size_map <- function(lplt1, aes_types, aes_sheet) {

    lplt2 <- NA

    if ("size" %in% aes_types) {

        lrow1 <- aes_sheet[aes_sheet[, "@aes_type"] == "size", ]

        if (lrow1$`@aes_assign_proc` == "list") {
            # get the name of the palette
            shape_val1 <- lrow1$`@aes_assign_value`
            shape_val2 <- trim(shape_val1)
            shape_vals <- strsplit(shape_val2, split = "\\s*,\\s*", perl = TRUE) 
            lplt2 <- lplt1 + scale_shape_manual(values = shape_vals)
            
        } else {
            err_str <- paste0("aes color format not supported")
            stop(err_str)
        }

    } else {

        lplt2 <- lplt1
    }

    return (lplt2)
}
    

# From https://stackoverflow.com/a/2261149
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

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
    sep_tag <- get_metadata_val(metadata_sheet, "sep")
    aes_types_str <- get_metadata_val(metadata_sheet, "aes_types")

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

    aes_types1 <- trim(aes_types_str)
    aes_types2 <-  strsplit(aes_types1, split = "\\s*,\\s*", perl = TRUE)
    aes_types <- aes_types2[[1]]

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

    pca_df2 <- append_factor(pca_df1, "color", aes_sheet, map_factors)
    pca_df3 <- append_factor(pca_df2, "shape", aes_sheet, map_factors)
    pca_df <- append_factor(pca_df3, "size", aes_sheet, map_factors)


    # 5. Start building ggplot2

    plt1 <- ggplot(pca_df, aes(x = PC1, y = PC2))

    # 6. Add aesthetic mappings

    pl2 <- append_aes_map(plt1, "shape", aes_types, aes_sheet)        
    plt3 <- append_aes_map(plt2, "size", aes_types, aes_sheet)        
    plt_f <- append_aes_map(plt3, "color", aes_types, aes_sheet)



    
}


