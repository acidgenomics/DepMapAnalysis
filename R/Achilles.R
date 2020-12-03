## FIXME Need to rework, sharing internal code with CCLEExpressionData.



#' Import Project Achilles CRISPR gene effect data
#'
#' @section Assays:
#'
#' - `effect`: **CERES data** with principle components strongly related to
#'   known batch effects removed, then shifted and scaled per cell line so the
#'   median nonessential KO effect is 0 and the median essential KO effect is
#'   -1.
#' - `probability`: **Probability** that knocking out the gene has a real
#'   depletion effect using `gene_effect`.
#'
#' @export
#' @note Updated 2020-11-11.
#'
#' @inheritParams params
#'
#' @return `Achilles`.
#'
#' @examples
#' object <- Achilles(rowRanges = FALSE, colData = FALSE)
#' print(object)
Achilles <-  # nolint
    function(
        release = NULL,
        rowRanges = TRUE,
        colData = TRUE
    ) {
        retiredGenes <- character()
        if (is.null(release)) {
            release <- .currentRelease
        }
        assert(
            isString(release),
            isFlag(rowRanges),
            isFlag(colData)
        )
        ## CSV formatting: genes in columns, cells in rows.
        assays <- list(
            "effect" = .importDataFile(
                fileName = "achilles_gene_effect.csv",
                release = release,
                rownamesCol = 1L,
                return = "matrix"
            ),
            "probability" = .importDataFile(
                fileName = "achilles_gene_dependency.csv",
                release = release,
                rownamesCol = 1L,
                return = "matrix"
            )
        )
        ## Transposing here to match DEMETER2 formatting, and standard
        ## SummarizedExperiment conventions for NGS data.
        assays <- lapply(X = assays, FUN = t)
        ## Sample metadata.
        if (isTRUE(colData)) {
            colData <- .importCellLineSampleData(release = release)
        } else {
            colData <- NULL
        }
        ## Gene metadata.
        ## FIXME Need to generalize this, so we can use in CCLEExpressionData...
        if (isTRUE(rowRanges)) {
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
        ## FIXME STANDARDIZE THIS WITH CCLEEXPRESSIONDATA.
        metadata <- list(
            "version" = .version,
            "release" = release,
            "genes" = list(
                "commonEssentials" =
                    .importCommonEssentials(release = release),
                "controlCommonEssentials" =
                    .importControlCommonEssentials(release = release),
                "controlNonessentials" =
                    .importControlNonessentials(release = release),
                "retired" = retiredGenes
            )
        )
        metadata <- Filter(Negate(is.null), metadata)
        args <- list(
            assays = assays,
            rowRanges = rowRanges,
            colData = colData,
            metadata = metadata
        )
        args <- Filter(Negate(is.null), args)
        rse <- do.call(what = makeSummarizedExperiment, args = args)
        if (
            is(rse, "SummarizedExperiment") &&
            !is(rse, "RangedSummarizedExperiment")
        ) {
            rse <- as(rse, "RangedSummarizedExperiment")
        }
        assert(is(rse, "RangedSummarizedExperiment"))
        rownames(rse) <- tolower(rownames(rse))
        colnames(rse) <- tolower(colnames(rse))
        validObject(rse)
        new("Achilles", rse)
    }
