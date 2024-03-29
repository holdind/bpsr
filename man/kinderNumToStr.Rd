% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pandaFunctions.R
\name{kinderNumToStr}
\alias{kinderNumToStr}
\title{Convert a numeric grade vector to a character grade vector}
\usage{
kinderNumToStr(vector)
}
\arguments{
\item{vector}{a numeric grade vector (kindergarten expressed as 0, -1, -2)}
}
\value{
a character grade vector (K0,K1,K2,01,02)
}
\description{
This function takes a numeric grade vector or column and converts it to a
character. For example, -2 is converted to 'K0', 0 is converted to 'K2', 9 to
'09', and 11 to '11'
}
