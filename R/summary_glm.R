#' @title summary_glm
#'
#' @description Provides a summary table of regression in the form of scientific paper
#'
#' @param model an R glm object
#' @param exp TRUE by default. You need to specify exp = FALSE if your model is has the indentity link function (linear regression, etc).
#' @param digits Number of digits to print for the main part.
#' @param pDigits Number of digits to print for the p-values.
#' @param printToogle Whether to print the output. If FALSE, no output is created, and a matrix is invisibly returned.
#' @param quote Whether to show everything in quotes. The default is FALSE. If TRUE, everything including the row and column names are quoted so that you can copy it to Excel easily.
#' @param ciFun Function used for calculation. confint is the default. For generalized linear models this gives the profile likelihood-based calculation, which may take too much time for large models, use confint.default for simple normal approximation method (+/- 1.96 * standard error).
#' @param n1 Set the number of first row you want to get. By defult n1 = 2
#' @param n2 Set the number of last row you want to get. By default n2 = 2
#'
#' @return A data frame object that contains a summary of a sample that
#'     can later be converted to a TeX output using \code{overview_print}
#' @examples
#' data(toydata)
#' output_table <- overview_tab(dat = toydata, id = ccode, time = year)
#' @export
#' @importFrom dplyr "%>%" tableone
summary_glm <- function(model, exp = T, digits = 2, pDigits = 4, printToggle = T, quote = F,n1 = 2, n2 = 2, ciFun = confint){
  table1 <- tableone::ShowRegTable(model = model, exp = exp, digits = digits, pDigits = pDigits, printToggle = printToggle, quote = quote, ciFun = ciFun) %>%
    as.data.frame()
  table2 <- broom::tidy(x) %>% as.data.frame()
  beta <- table2[["estimate"]] %>% as.numeric() %>% sprintf(paste0("%.", digits, "f"), .)
  se <- table2[["std.error"]] %>% as.numeric() %>% sprintf(paste0("%.", digits, "f"), .)
  OR95 <- table1[["exp(coef) [confint]"]]
  `beta (se)` <- paste0(beta, " (", se, ")")
  p <- a[["p"]]
  df <- cbind(`beta (se)`, OR95, p) %>% as.data.frame() %>% slice(n1:n2)
  rwnm <- rownames(a[n1:n2,])
  rownames(df) <- rwnm
  return(df)

}
