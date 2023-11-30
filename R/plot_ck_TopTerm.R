


#' Title
#'
#' @param ck
#' @param col_n
#' @param label_format
#' @param n
#' @param class
#'
#' @return
#' @export
#'
#' @examples
plot_ck_topterm <- function(ck, col_n, label_format = 40,
                                 n = 25, class = "KEGG") {
  ck_res <- ck@compareClusterResult %>% as_tibble()

  top10_term <- ck_res %>%
    pivot_wider(
      id_cols = c(ID, Description), names_from = "Cluster",
      values_from = "p.adjust"
    ) %>%
    rowwise() %>%
    mutate(range = min(c_across(all_of(col_n)), na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(range) %>%
    slice_head(n = n) %>%
    pull(Description)

  enrichplot::dotplot(ck,
                      label_format = label_format,
                      showCategory = top10_term,
                      title = glue::glue("{class} TopTerm")
  ) +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_fill_gradientn(
      colors = RColorBrewer::brewer.pal(11, "RdBu"),
      breaks = c(0, 0.025, 0.05),
      na.value = "gray",
      # labels=c("Minimum",0.05,"Maximum"),
      limits = c(0, 0.05)
    )
}

