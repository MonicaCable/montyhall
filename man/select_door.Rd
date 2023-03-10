% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem-cable.R
\name{select_door}
\alias{select_door}
\title{Select a door.}
\usage{
select_door()
}
\arguments{
\item{No}{arguments are used by the function.}
}
\value{
The function returns a length 1 numeric vector
indicating the selected door.
}
\description{
\code{select_door()} sets three doors labeled "1", "2", and "3",
then takes a random sample of one of those doors,
and returns the value of the door that was selected.
}
\details{
At this point in the game the contestant is choosing their
first door in the game. They have no information about any of
the doors, so their choice is functionally random.
}
\examples{
  select_door()
  
}
