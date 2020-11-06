## FIXME Improve metadata here.
## FIXME CONVERT THE EXPRESSION DATA TO MATRIX.



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
    function(
        release = NULL,
        rowRanges = TRUE,
        colData = TRUE
    ) {
        if (is.null(release)) {
            release <- .currentRelease
        }
        assert(
            isString(release),
            isFlag(rowRanges),
            isFlag(colData)
        )
        cli_alert("Preparing CCLE RNA-seq expression data.")
        mat <- .importDataFile(
            fileName = "ccle_expression.csv",
            release = release,
            rownamesCol = 1L,
            return = "matrix"
        )
        ## Need to transpose, putting cells in the columns, genes in rows.
        mat <- t(mat)

        if (isTRUE(colData)) {
            colData <- .importCellLineSampleData(release = release)
        } else {
            colData <- NULL
        }


        retiredCells <-

        ok <- colnames(mat) %in% rownames(colData)
        colnames(mat)[!ok]
        ## MISSING: "ACH_001316"
        all()



        assert(is(df, "DataFrame"))
        new("CCLEExpressionData", df)
    }
