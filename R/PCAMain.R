# Customized code for updating color, shape and size to the input plot.

append_factor <- function(pca_df, aes_type_str, aes_sheet, map_factors, 
    aes_types, default_aes) {

    lrow1 <- aes_sheet[aes_sheet[, "@aes_type"] == aes_type_str,]
    default_aes_val <- default_aes[[aes_type_str]]
    row_count <- dim(lrow1)[1]
    if (row_count) {
        lfactor_name <- lrow1["@factor_name"][[1]]
        lfactor <- map_factors[[lfactor_name]]
      
        if (aes_type_str %in% aes_types) {
            if (aes_type_str == "size") {
                pca_df[, aes_type_str] <- as.numeric(as.character(lfactor))
            } else {
                pca_df[, aes_type_str] <- lfactor
            }
        } else {
            lfactor_len <- length(lfactor)
            lfactor_arr <- rep(default_aes_val, lfactor_len)
            if (aes_type_str == "size") {
                pca_df[, aes_type_str] <- lfactor_arr
            } else {
                
                pca_df[, aes_type_str] <- factor(lfactor_arr)
            }
            
        }
    }

    return (pca_df)
}

append_aes_map <- function(lplt1, laes_type, aes_sheet) {

    lplt2 <- NA
    if (laes_type == "color") {
        lplt2 <- append_color_map(lplt1, aes_sheet)
    } else if (laes_type == "shape") {
        lplt2 <- append_shape_map(lplt1, aes_sheet)
    } else if (laes_type == "size") {
        lplt2 <- append_size_map(lplt1, aes_sheet)
    } else {
        err_str <- paste0("Illegal aes_type", laes_type)
        stop(err_str)
    }

    return (lplt2)
}

append_color_map <- function(lplt1, aes_sheet) {

    lplt2 <- NA
    
    # Get the palette and create the aesthetic mapping
    lrow1 <- aes_sheet[aes_sheet[, "@aes_type"] == "color", ]
    
    if (lrow1$`@aes_assign_proc` == "palette") {
        # get the name of the palette
        palette_val <- lrow1$`@aes_assign_value`

        # Check number of colors in the palette
        lmaxcolors <- brewer.pal.info["Paired", "maxcolors"]
        num_levels <- length(levels(lplt1$data$color))

        if (num_levels > lmaxcolors) {
            err_msg <- paste0("Number of levels ", num_levels,  
                " in color factor is more than maxcolors ", lmaxcolors, 
                " in palette ", palette_val)
            stop(err_msg)
        }

        lplt2 <- lplt1 + scale_colour_brewer(palette = palette_val) 
        print("Adding scale_colour_brewer.")
        
    } else {
        err_str <- paste0("aes color format not supported")
        stop(err_str)
    }   

    return (lplt2)
}

# https://stackoverflow.com/a/83162
last_val <- function(x) { tail(x, n = 1) }

append_shape_map <- function(lplt1, aes_sheet) {

    lplt2 <- NA

    lrow1 <- aes_sheet[aes_sheet[, "@aes_type"] == "shape", ]
    
    if (lrow1$`@aes_assign_proc` == "list") {
        # get the name of the palette
        shape_val1 <- lrow1$`@aes_assign_value`
        shape_val2 <- trim(shape_val1)
        shape_vals1 <- strsplit(shape_val2, split = "\\s*,\\s*", perl = TRUE)

        # This is the number of levels specified by users; 21,22,23,24,25
        # are preferred.

        shape_vals <- shape_vals1[[1]]
        spec_shape_len <- length(shape_vals)

        data_shape_levels <- levels(lplt1$data$shape)

        data_shape_len <- length(data_shape_levels)
        if (data_shape_len > spec_shape_len) {
            err_str <- paste0("Number of shapes on spreadsheet is less than required.")
            stop(err_str)
        }

        lplt2 <- lplt1 + scale_shape_manual(values = as.numeric(shape_vals)) 

        print("Adding scale_shape_manual.")
        
    } else {
        err_str <- paste0("aes shape format not supported.")
        stop(err_str)
    }   

    return (lplt2)
}

append_size_map <- function(lplt1, aes_sheet) {

    lplt2 <- NA

    lrow1 <- aes_sheet[aes_sheet[, "@aes_type"] == "size", ]

    if (lrow1$`@aes_assign_proc` == "range") {
        # get the name of the palette
        size_val1 <- lrow1$`@aes_assign_value`
        size_val2 <- trim(size_val1)
        size_vals1 <- strsplit(size_val2, split = "\\s*,\\s*", perl = TRUE) 
        size_vals <- size_vals1[[1]]

        lplt2 <- lplt1 + scale_size_continuous(range = as.numeric(size_vals))
        print("Adding scale_size.")
        
    } else {
        err_str <- paste0("aes size format not supported.")
        stop(err_str)
    }

    return (lplt2)
}
    

# From https://stackoverflow.com/a/2261149
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

PCA_GS <- function(gs_id) {

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
    plot_prefix <- get_metadata_val(metadata_sheet, "plot_prefix")
    viz_type <- get_metadata_val(metadata_sheet, "viz_type")
    PCA_scale_str <- get_metadata_val(metadata_sheet, "PCA_scale")
    sep_tag <- get_metadata_val(metadata_sheet, "sep")
    aes_types_str <- get_metadata_val(metadata_sheet, "aes_types")
    draw_labels_str <- get_metadata_val(metadata_sheet, "draw_labels")
    CDS_only_str <- get_metadata_val(metadata_sheet, "CDS_only")
    do_vst_str <- get_metadata_val(metadata_sheet, "do_vst")
    plot_width_str <- get_metadata_val(metadata_sheet, "plot_width")
    plot_height_str <- get_metadata_val(metadata_sheet, "plot_height")

    default_color <- get_metadata_val(metadata_sheet, "default_color")
    default_shape_str <- get_metadata_val(metadata_sheet, "default_shape")
    default_size_str <- get_metadata_val(metadata_sheet, "default_size")

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
    draw_labels <- as.logical(toupper(draw_labels_str))
    CDS_only <- as.logical(toupper(CDS_only_str))
    do_vst <- as.logical(toupper(do_vst_str))
    default_shape <- as.numeric(default_shape_str)
    default_size <- as.numeric(default_size_str)
    plot_width <- as.numeric(plot_width_str)
    plot_height <- as.numeric(plot_height_str)

    default_aes <- list()
    default_aes[["size"]] <- default_size
    default_aes[["color"]] <- default_color
    default_aes[["shape"]] <- default_shape

    aes_types1 <- trim(aes_types_str)
    aes_types2 <-  strsplit(aes_types1, split = "\\s*,\\s*", perl = TRUE)
    aes_types <- aes_types2[[1]]

    count_tab <- get_normalized_data(count_file, sep_str, CDS_only, do_vst)
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

    pca_df2 <- append_factor(pca_df1, "color", aes_sheet, map_factors, aes_types, default_aes)
    pca_df3 <- append_factor(pca_df2, "shape", aes_sheet, map_factors, aes_types, default_aes)
    pca_df <- append_factor(pca_df3, "size", aes_sheet, map_factors, aes_types, default_aes)

    # 5. Start building ggplot2

    plt1 <- ggplot(pca_df, aes(x = PC1, y = PC2, size = size, color = color, shape = shape))

    # 6. Add aesthetic mappings

    plt2 <- append_aes_map(plt1, "shape", aes_sheet)        
    plt3 <- append_aes_map(plt2, "size", aes_sheet)        
    plt4 <- append_aes_map(plt3, "color", aes_sheet)

    plt_f <- plt4 + geom_point()

    dir.create(file.path(outdir), recursive = TRUE)
    plot_path <- paste0(outdir, "/", plot_prefix, ".pdf")
    ggsave(plot_path, width = plot_width, height = plot_height)

    
}


