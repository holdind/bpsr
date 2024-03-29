% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/titanFunctions.R
\name{sepQuantAndUnit}
\alias{sepQuantAndUnit}
\title{Split a text units column into the number and the unit.}
\usage{
sepQuantAndUnit(df, var, nameTotal, nameUnits)
}
\arguments{
\item{df}{A Titan report containing a string units column formatted 'n
units'.}

\item{var}{The Variable containing a string units column formatted 'n units'.}

\item{nameTotal}{What you want to name the new total items variable.}

\item{nameUnits}{What you want to name the new units variable}
}
\value{
a data frame with a string units column and a numeric total units
  column.
}
\description{
Titan reports build their units columns as strings, formatted as such 'n
units'. This function splits the number from the units, and converts the
number to a numeric value.
}
