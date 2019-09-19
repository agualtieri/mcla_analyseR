my_custom_md_table<- function (result) 
{
  md_out <- ""
  if (!is.null(result$summary.statistic)) {
    if (!is.null(result$parameters$independent.var)) {
      hypegrammaR:::md_add_lines(md_out) <- (paste("by",  labelled::var_label(result$parameters$independent.var)))
    }
    table <- result %>% (hypegrammaR:::map_to_table)
    hypegrammaR:::md_add_lines(md_out) <- knitr::kable(table, format = "html") %>% 
      kable_styling()
  }
  if (!is.null(result$hypothesis.test$result$p.value)) {
    hypegrammaR:::md_add_lines(md_out) <- c(as.character(unique(result$hypothesis.test$name)), 
                                            "P Value:")
    result$hypothesis.test$result$p.value %>% print
  }
  hypegrammaR:::md_add_lines(md_out) <- knitr::kable(result$hypothesis.test %>% 
                                                       as.data.frame, format = "html")
  md_out
}
