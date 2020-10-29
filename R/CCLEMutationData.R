## FIXME SHOULD RETURN THESE AS MATRICES.
## FIXME RETURN USING SUMMARIZED EXPERIMENT SO WE CAN SLOT METADATA?


## FIXME IMPROVE METADATA HERE.


#' Import CCLE mutation data
#'
#' @export
#' @note Updated 2020-09-30.
#'
#' @inheritParams params
#'
#' @return `CCLEMutationData`.
#'
#' @examples
#' object <- CCLEMutationData()
#' dim(object)
CCLEMutationData <-  # nolint
    function(release = NULL) {
        df <- .importDataFile(
            fileName = "ccle_mutations.csv",
            format = "tsv",
            release = release,
            rownamesCol = NULL
        )
        assert(is(df, "DataFrame"))
        new("CCLEMutationData", df)
    }
