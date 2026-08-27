#I copied this from plot_posterior_bar from bmediatR
#but just returned posterior probabilities instead of 
#creating the plot object

get_posterior <- function(bmediatR_object, med_annot = NULL, mediator_id, med_var = "protein.id", 
    stack = FALSE, bar_col = c("seagreen4", "seagreen1", "skyblue", 
        "goldenrod1", "goldenrod4", "gray"), relabel_x = NULL, 
    add_number_labels = FALSE, label_size = 5, num_dig = 3, main = NULL) 
{
    post_mat <- bmediatR_object$ln_post_c[1, , drop = FALSE]
    model_flag <- is.finite(post_mat)
    names(model_flag) <- colnames(post_mat)
    long_names <- c("other non-med", "other non-med", "other non-med", 
        "complete med", "other non-med", "other non-med", "co-local", 
        "partial med", "other non-med", "complete med (react)", 
        "other non-med", "partial med (react)")
    names(long_names) <- colnames(post_mat)
    bar_col <- bar_col[c(model_flag[c("1,1,0", "1,1,1", "1,0,1", 
        "1,*,1", "0,*,1")], TRUE)]
    posterior_dat <- exp(bmediatR_object$ln_post_c) %>% as.data.frame %>% 
        tibble::rownames_to_column(med_var) %>% dplyr::rename(`partial med` = `1,1,1`, 
        `complete med` = `1,1,0`, `co-local` = `1,0,1`, `partial med (react)` = `1,*,1`, 
        `complete med (react)` = `0,*,1`)
    return(posterior_dat)
    if (!is.null(med_annot)) {
        posterior_dat <- posterior_dat %>% dplyr::left_join(med_annot %>% 
            dplyr::select(tidyselect::all_of(med_var), symbol))
    }
    else {
        posterior_dat <- posterior_dat %>% dplyr::mutate(symbol = get(med_var))
    }

    posterior_dat <- posterior_dat %>% dplyr::left_join(posterior_dat %>% 
        dplyr::select(tidyselect::all_of(med_var), contains(",")) %>% 
        dplyr::mutate(`other non-med` = rowSums(.[-1]))) %>% 
        dplyr::select(-contains(",")) %>% tidyr::gather(key = model, 
        value = post_p, -c(tidyselect::all_of(med_var), symbol))
    models_use <- unique(long_names[model_flag])
    posterior_dat <- posterior_dat %>% dplyr::filter(model %in% 
        models_use) %>% dplyr::mutate(model = factor(model, levels = c("complete med", 
        "partial med", "co-local", "partial med (react)", "complete med (react)", 
        "other non-med")))
    bar_theme <- ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
        panel.grid.minor = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), 
        axis.line = ggplot2::element_line(colour = "black"), 
        plot.title = ggplot2::element_text(hjust = 0.5, size = 16, 
            face = "plain"), axis.title.x = ggplot2::element_blank(), 
        axis.text.x = ggplot2::element_text(hjust = 0.5, size = 14, 
            face = "plain"), axis.title.y = ggplot2::element_text(size = 14, 
            face = "plain"), axis.text.y = ggplot2::element_text(size = 14, 
            face = "plain"), axis.ticks.y = ggplot2::element_blank(), 
        legend.title = ggplot2::element_text(size = 14), legend.text = ggplot2::element_text(size = 14))
    if (!is.null(relabel_x)) {
        posterior_dat$symbol <- relabel_x  
    }
    return(posterior_dat)
   }