


#' Title
#'
#' @param ck
#' @param class
#' @param p.adjust
#' @param qvalue
#' @param showCategory
#' @param label_format
#' @param font.size
#'
#' @return
#' @export
#'
#' @examples
plot_ck_pvalue <- function(ck, class = "KEGG", p.adjust = 0.05,
                           qvalue = 0.2, showCategory = 15,
                           label_format = 40,
                           font.size = 12
) {
  ck_f <- ck %>% dplyr::filter(p.adjust < {{ p.adjust }}, qvalue < {{ qvalue }})

  enrichplot::dotplot(ck_f,
                      label_format = label_format,
                      showCategory = showCategory,
                      group = F,
                      color = "p.adjust",
                      by = "geneRatio",
                      size = "geneRatio",
                      title = glue::glue("{class} P<0.05"),
                      font.size = font.size
                      # label_format = f
  ) +
    theme(
      axis.text.x = element_text(angle = 90),
      # axis.text.y = element_markdown()
    ) +
    scale_fill_gradientn(
      colors = RColorBrewer::brewer.pal(11, "RdBu"),
      breaks = c(0, 0.025, 0.05),
      limits = c(0, 0.05)
    )
}


