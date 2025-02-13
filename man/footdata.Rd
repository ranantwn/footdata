\name{footdata-package}
\alias{footdata}
\title{Load, Process, and Merge Datasets with Footnotes}
\usage{
footdata(dataset_code, base_dir)
}
\description{
Retrieves a dataset in SDMX format from Eurostat’s API using a dataset code.
The function also retrieves footnotes providing additional context.
It merges the main dataset with the footnotes into a single annotated dataset
and exports the final result as a CSV file to a local directory.
}
\arguments{
  \item{dataset_code}{A string representing the Eurostat dataset code. It identifies the dataset and associated footnotes.}
  \item{base_dir}{A string specifying the path to the directory where the final annotated Microsoft Excel CSV file will be saved.}
}
\value{
  Returns the path to the generated Microsoft Excel CSV file containing the merged dataset and footnotes.
  The output file combines data from the SDMX dataset and associated footnotes, enhancing
  the dataset with additional contextual information.
}
\author{
  Ourania Antonopoulou, \email{ran.antonopoulou@gmail.com}
}
\examples{
  footdata("ef_m_pry", "C:/User/Downloads")
}
\note{
  Ensure the specified directory does not already contain an Microsoft Excel CSV file with the same name as the output file,
  as it will be overwritten.
}
\seealso{
\code{\link[eurostat]{get_eurostat}}, \code{\link[eurostat]{eurostat-package}}
}
