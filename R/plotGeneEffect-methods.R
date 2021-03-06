#' Plot gene effect
#'
#' @name plotGeneEffect
#' @note Updated 2020-10-07.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @return `ggplot`.
#'
#' @examples
#' data(ach, dem)
#'
#' ## Achilles ====
#' object <- ach
#' genes <- rownames(object)[seq_len(5L)]
#' plotGeneEffect(object, genes = genes)
#'
#' ## DEMETER2 ====
#' object <- dem
#' genes <- rownames(object)[seq_len(5L)]
#' plotGeneEffect(object, genes = genes)
NULL



## Updated 2020-10-07.
`plotGeneEffect,Achilles` <-  # nolint
    function(
        object,
        genes,
        geom = c("boxplot", "density")
    ) {
        validObject(object)
        geom <- match.arg(geom)
        assert(
            isCharacter(genes),
            length(genes) <= 100L
        )
        mat <- assay(object, i = "effect")
        rownames <- mapGenesToRownames(object, genes = genes)
        mat <- mat[rownames, , drop = FALSE]
        data <- as_tibble(melt(mat))
        data <- data[complete.cases(data), ]
        if (identical(geom, "boxplot")) {
            p <- ggplot(
                data = data,
                mapping = aes(
                    x = reorder(
                        !!sym("rowname"),
                        !!sym("value"),
                        mean
                    ),
                    ## > x = !!sym("rowname"),
                    y = !!sym("value"),
                    fill = !!sym("rowname")
                )
            ) +
                geom_hline(
                    color = "red",
                    linetype = "dashed",
                    size = 1.25,
                    yintercept = -1L
                ) +
                geom_boxplot(
                    color = "black",
                    show.legend = FALSE,
                    size = 0.75
                ) +
                scale_x_discrete(limits = rev) +
                coord_flip() +
                labs(
                    title = paste(
                        class(object)[[1L]],
                        "gene effect"
                    ),
                    x = "gene",
                    y = "gene effect"
                )
        } else if (identical(geom, "density")) {
            p <- ggplot(
                data = data,
                mapping = aes(
                    x = !!sym("value"),
                    fill = !!sym("rowname")
                )
            ) +
                geom_vline(
                    color = "red",
                    linetype = "dashed",
                    size = 1.25,
                    xintercept = -1L
                ) +
                geom_density(
                    color = "black",
                    show.legend = FALSE,
                    size = 0.75
                ) +
                facet_wrap(
                    facets = sym("rowname"),
                    scales = "fixed"
                ) +
                labs(
                    title = "Achilles gene effect",
                    x = "gene effect"
                )
        }
        p
    }



#' @rdname plotGeneEffect
#' @export
setMethod(
    f = "plotGeneEffect",
    signature = signature("Achilles"),
    definition = `plotGeneEffect,Achilles`
)



## Updated 2021-02-24.
`plotGeneEffect,DEMETER2` <-  # nolint
    `plotGeneEffect,Achilles`



#' @rdname plotGeneEffect
#' @export
setMethod(
    f = "plotGeneEffect",
    signature = signature("DEMETER2"),
    definition = `plotGeneEffect,DEMETER2`
)
