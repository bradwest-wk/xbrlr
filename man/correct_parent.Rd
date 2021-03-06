% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree_animation.R
\name{correct_parent}
\alias{correct_parent}
\title{Is Parent Correct?}
\usage{
correct_parent(g_old, g_new, v)
}
\arguments{
\item{g_old}{an igraph graph}

\item{g_new}{an igraph graph in which to check if the parent is correct}

\item{v}{an igraph vertex name in both graphs g_old and g_new}
}
\value{
boolean; if the parent is correct: true, else: false
}
\description{
Checks if the current vertex has the correct parents.
}
