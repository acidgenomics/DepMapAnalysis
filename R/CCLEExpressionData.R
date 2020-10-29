## FIXME Improve metadata here.



#' Import CCLE expression data
#'
#' @export
#' @note Updated 2020-09-30.
#'
#' @inheritParams params
#'
#' @return `CCLEExpressionData`.
#'
#' @seealso
#' - https://github.com/UCLouvain-CBIO/depmap/blob/master/inst/scripts/
#'       make-data_20Q3.R
#'
#' @examples
#' object <- CCLEExpressionData()
#' dim(object)
CCLEExpressionData <-  # nolint
    function(release = NULL) {
        df <- .importDataFile(
            fileName = "ccle_expression.csv",
            release = release,
            rownamesCol = 1L
        )
        assert(is(df, "DataFrame"))
        new("CCLEExpressionData", df)
    }
