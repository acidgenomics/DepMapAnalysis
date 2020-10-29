## FIXME Improve metadata here.



#' Import CCLE copy number data
#'
#' @export
#' @note Updated 2020-09-30.
#'
#' @inheritParams params
#'
#' @return `CCLECopyNumberData`.
#'
#' @examples
#' object <- CCLECopyNumberData()
#' dim(object)
CCLECopyNumberData <-  # nolint
    function(release = NULL) {
        df <- .importDataFile(
            fileName = "ccle_gene_cn.csv",
            release = release,
            rownamesCol = 1L
        )
        assert(is(df, "DataFrame"))
        new("CCLECopyNumberData", df)
    }
