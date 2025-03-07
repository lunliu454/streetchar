% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/building_num.R
\name{building_num}
\alias{building_num}
\title{Calculate building num}
\usage{
Building_num(building, street, option = ("fixed","street"), k = )
}
\arguments{
\item{option}{Defines the method by which building num is calculated.
If option = "fixed", k should be defined by the user (default = 40 meters).
If option = "street", k equals to the width of street on each side.}

\item{k}{Defines the distance from streets. Buildings within which area will be considered as buildings alongside streets.}

\item{building}{A Simple Feature containing the building footprints of a city}

\item{street}{A Simple Feature containing the street network of a city.}
}
\description{
Calculates the number of buildings on two sides of streets with two options available.
It performs geometric translation to the lines ("street") by distance k, and defines polygons ("buildings") that intersect with the lines as building alongside streets.
}
\details{
Calculate building num with two options available.
It returns the summary statistics (max, min, average, weighted average, standard deviation) of building area alongside every street to the Simple Feature "street". If the calculation is performed in option "street", the width of street should first be calculated
If the translation lines of streets and the polygons ("building") does not have intersection, it will return NA.
}
\examples{
## Not run:
building <-data(building)
street<-data(street)
building_num (building, street, option = "fixed", k = 40)
building <-data(building)
street<-data(street)
building_num (building, street, option = "street", k = 40)
## End(Not run)
}
