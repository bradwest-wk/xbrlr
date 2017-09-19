# =============================================================================
#       Functions for creating ggplot2 adjacency matricies
# -----------------------------------------------------------------------------
# Functions for creating matricies to be used in heatmap plots
# =============================================================================


#' Create Relationship Matrix
#'
#' @param df Dataframe, two columns, a report identifier and an element
#' identifier.  The report column must be first
#' @param scale Boolean, should the matrix be scaled, resulting in an upperleft
#' matrix that is scaled according to the type. Lower right matrix remains the
#' same.
#' @param scale_type String, currently just the default, 'minimum'
#' @return Matrix, each cell representing the counts of how many times those two
#' element identifiers were used together in the same report identifier
relationship_matrix <- function(df, scale = FALSE, scale_type = 'minimum') {
    colnames(df) <- c('report', 'element')
    df <- df %>% arrange(report)
    els <- unique(df$element)
    # initialize empty matrix
    mat <- matrix(data = rep(0, length(els)^2),
                  nrow = length(els))
    rownames(mat) <- els
    colnames(mat) <- els
    # increment matrix cell counts
    i <- 1
    while (i <= nrow(df)) {
        report_num <- df$report[i]
        j <- i
        while( j <= nrow(df) & df$report[j] == report_num ) {
            curr_diag <- mat[df$element[j], df$element[j]]
            mat[df$element[j], df$element[j]] <- curr_diag + 1
            if ( i != j ) {
                curr_val <- mat[df$element[i], df$element[j]]
                mat[df$element[i], df$element[j]] <- curr_val + 1
                mat[df$element[j], df$element[i]] <-
                    mat[df$element[i], df$element[j]]
            }
            j <- j + 1
        }
        i <- j
    }
    if (scale) {
        scaled <- scale_matrix(mat, type = scale_type)
        return(scaled)
    }
    return(mat)
}


#' Scale Matrix
#'
#' Scales the adjacency matrix according to the type specified. Currently the
#' only type is minimum, which scales (for the upper left of the matrix)
#' each count by the minimum of i,i or j,j.  The reasoning for this is that the
#' maximum value an off-diagonal cell (i,j) can have is the minimum of i,i and
#' j,j. Example: Given two elements A and B, let element A have been used 5
#' times and element B have been used 10 times.  Then A will have been used
#' every time with B iff cell i,j = 5.
#'
#' @param mat the adjacency matrix to scale
#' @param type the type of scaling to do. Currently 'minimun'
scale_matrix <- function(mat, type = 'minimum') {
    if (type == 'minimum') {
        for (n in 1:(nrow(mat)-1)) {
            for (m in (n+1):ncol(mat)) {
                scaler <- min(mat[n,n], mat[m,m])
                mat[n,m] <- round(mat[n,m] / scaler, 2)
            }
        }
    }
    return(mat)
}


# =============================================================================
# ggplot2
# =============================================================================

#' Plot Matrix ggplot2
#'
#' Plots an adjacency matrix with ggplot2 and values superimposed on cells
#'
#' @param frequency_matrix the frequency matrix to plot
#' @param main the title of the plot
#' @return an adcacency matrix image
plot_ggplot_mat <- function(frequency_matrix, main) {
    blues <- RColorBrewer::brewer.pal(9, 'Blues')
    df <- reshape2::melt(frequency_matrix)
    plot <- ggplot(df, aes(Var1, Var2)) +
        geom_tile(aes(fill = value), color = 'lightgray') +
        geom_text(aes(label =
                          ifelse(value>0.01, format(value, format = f, digits=2, nsmall = 0,
                                                    drop0trailing = TRUE,
                                                    scientific = FALSE), ''),
                      color = value),size = 6) +
        scale_color_gradient(na.value = 'black',
                             name = 'Prop Usage',
                             limits = c(0,0.99),
                             position = 'right',
                             low = 'grey90',
                             high = blues[9]
        ) +
        scale_fill_gradient(low='white', high='red') +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 13),
              axis.text.y = element_text(size = 13),
              plot.title = element_text(hjust = 0.5, size = 20)) +
        labs(color = 'Prop Usage',
             fill = "Usage",
             title = main) +
        xlab("") + ylab("")
    return(plot)
}



# load("~/drive/projects/DQ_requests/sm_support/data/ref_elements/asc_230_ref_elements/asc_230_10_45_all.Rdata")
# test <- cf %>% filter(ref == 'asc_230-10-45-25(b)') %>%
#     select(accession_number, element_name) %>% arrange(accession_number)
# sim <- relationship_matrix(test)
# sim_scaled <- relationship_matrix(test, scale = TRUE)
# rownames(sim) <- c()
# colnames(sim) <- c()
# rownames(sim_scaled) <- c()
# colnames(sim_scaled) <- c()
# sim2 <- create_sim_matrix(test)
# rownames(sim2) <- c()
# colnames(sim2) <- c()
# all.equal(sim, sim2)
