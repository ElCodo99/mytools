% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vis_gg_scatter.R
\name{gg_scatter}
\alias{gg_scatter}
\title{Streamlined Scatter Plots with ggplot2}
\usage{
gg_scatter(
  x,
  y,
  data,
  factor_var = NULL,
  col = "steelblue",
  legend_title = NULL,
  x_limit = NULL
)
}
\arguments{
\item{x}{A string specifying the variable to plot on the x-axis.}

\item{y}{A string specifying the variable to plot on the y-axis.}

\item{data}{The data frame containing the data to plot.}

\item{factor_var}{A string specifying a dichotomous factor variable for coloring points. Default is NULL.}

\item{col}{A string specifying the color of the points. Default is "steelblue".}

\item{legend_title}{A string specifying the title of the legend. Default is NULL.}

\item{x_limit}{A numeric vector specifying the x-axis limits. Default is NULL.}
}
\value{
A ggplot object, with aesthetic mappings and other graphical parameters pre-configured
for ease of use and visual appeal.
}
\description{
This function aims to strike a balance between the quick but often less visually appealing plots
generated by R's internal plotting functions, and the highly customizable but verbose syntax
often required by ggplot2. It allows for the creation of visually pleasing scatter plots with
minimal syntax.
}
\note{
This function uses `eval()` and `parse()` on the user-supplied variable names.
Be cautious when using this function in an environment where the data or variable names
are not trusted.
}
\examples{
\dontrun{
data(mtcars)
gg_scatter("mpg", "wt", data = mtcars, factor_var = "am")
}

}
\seealso{
For more advanced customizations, see the ggplot2 package documentation.
}
