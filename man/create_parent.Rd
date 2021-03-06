% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree_animation.R
\name{create_parent}
\alias{create_parent}
\title{Create a New Parent Vertex}
\usage{
create_parent(g_old, g_new, v, filename)
}
\arguments{
\item{g_old}{an igraph graph, the working graph}

\item{g_new}{an igraph graph, the desired graph}

\item{v}{an igraph vertex name, the vertex whose parent must be created}

\item{filename}{The output filename}
}
\value{
a new igraph graph with the correct parent in place
}
\description{
Creates a new vertex and places accordingly, checking if its parents are in
are in the graph.  Works recursively
}
