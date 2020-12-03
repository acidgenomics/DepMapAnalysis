## FIXME Improve metadata here.
## FIXME CONVERT THE EXPRESSION DATA TO MATRIX.
## FIXME Do we need retiredGenes here too?



#' Import CCLE expression data
#'
#' @export
#' @note Updated 2020-11-11.
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
        retiredCells <- character()
        if (is.null(release)) {
            release <- .currentRelease
        }
        assert(
            isString(release),
            isFlag(rowRanges),
            isFlag(colData)
        )
        cli_alert("Preparing CCLE RNA-seq expression data.")
        assays <- list(
            "tpm" = .importDataFile(
                fileName = "ccle_expression.csv",
                release = release,
                rownamesCol = 1L,
                return = "matrix"
            )
        )
        ## Need to transpose, putting cells in the columns, genes in rows.
        assays <- lapply(X = assays, FUN = t)
        if (isTRUE(colData)) {
            colData <- .importCellLineSampleData(release = release)
            ok <- colnames(mat) %in% rownames(colData)
            if (!all(ok)) {
                retiredCells <- colnames(mat)[!ok]
                cells <- intersect(colnames(mat), rownames(colData))
            }
        } else {
            colData <- NULL
        }
        if (isTRUE(rowRanges)) {
            ## FIXME Need to standardize this code with `Achilles()`.
            entrez <- as.integer(str_extract(
                string = rownames(assays[[1L]]),
                pattern = "[0-9]+$"
            ))
            entrez2ensembl <- readRDS(system.file(
                "extdata", "entrez2ensembl.rds",
                package = packageName()
            ))
            assert(
                is(entrez2ensembl, "DataFrame"),
                identical(
                    x = colnames(entrez2ensembl),
                    y = c("entrez", "ensembl", "retired")
                ),
                isSubset(entrez, entrez2ensembl[["entrez"]])
            )
            idx <- match(x = entrez, table = entrez2ensembl[["entrez"]])
            assert(!any(is.na(idx)))
            entrez2ensembl <- entrez2ensembl[idx, ]
            ## Drop any retired genes from analysis.
            entrez2ensembl[["retired"]][
                is.na(entrez2ensembl[["retired"]])] <- FALSE
            drop <- entrez2ensembl[["retired"]]
            if (any(drop)) {
                keep <- !drop
                retiredGenes <- rownames(assays[[1L]])[drop]
                cli_alert_warning(sprintf(
                    "Dropping %d retired %s: %s.",
                    length(retiredGenes),
                    ngettext(
                        n = length(retiredGenes),
                        msg1 = "gene",
                        msg2 = "genes"
                    ),
                    toString(retiredGenes, width = 200L)
                ))
                entrez2ensembl <- entrez2ensembl[keep, ]
                assays <- lapply(
                    X = assays,
                    FUN = function(assay) {
                        assay[keep, ]
                    }
                )
            }
            rowRanges <- makeGRangesFromEnsembl(
                organism = "Homo sapiens",
                release = 101L,
                synonyms = TRUE
            )
            idx <- match(
                x = entrez2ensembl[["ensembl"]],
                table = names(rowRanges)
            )
            assert(!any(is.na(idx)))
            rowRanges <- rowRanges[idx]
            names(rowRanges) <- rownames(assays[[1L]])
        } else {
            rowRanges <- NULL
        }
        assert(is(df, "DataFrame"))
        new("CCLEExpressionData", df)
    }
