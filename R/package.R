#' DepMapAnalysis
#'
#' Cancer Dependency Map (DepMap) analysis toolkit.
#'
#' @keywords internal

#' @importClassesFrom basejump DataFrame SummarizedExperiment
#'
#' @importFrom basejump EntrezGeneInfo alert alertWarning assay assayNames
#'   as_tibble cacheURL camelCase complete.cases decode do.call geneSynonyms
#'   import lapply makeDimnames makeSummarizedExperiment mapGenesToRownames
#'   mcols melt metadata packageName packageVersion reorder rbind snakeCase t
#' @importFrom ggplot2 aes coord_flip facet_wrap geom_boxplot geom_density
#'   geom_hline geom_vline ggplot labs scale_x_discrete
#' @importFrom goalie assert hasDimnames hasLength hasRownames isAFile isAURL
#'   isCharacter isFlag isScalar isString isSubset validate validateClasses
#' @importFrom methods as is new setClass setValidity validObject
#' @importFrom rlang !! sym
#' @importFrom stringr str_match
"_PACKAGE"