% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kobo_file.R
\name{kobo_asset_file_list}
\alias{kobo_asset_file_list}
\title{List all uploaded files related to a KoboToolbox API asset}
\usage{
kobo_asset_file_list(x)
}
\arguments{
\item{x}{the asset uid or the \code{kobo_asset} object.}
}
\value{
A \code{data.frame} containing the list of all your KoboToolbox API files under
the asset:
\itemize{
\item \code{uid} the asset unique identifier
\item \code{url} url of the files API endpoint
\item \code{asset} url of the files associated asset API endpoint
\item \code{user} the user account of the owner of the asset
\item \code{user__username} when the asset was created
\item \code{file_type} files type either \code{form_media} or \code{map_layer}
\item \code{description} files description
\item \code{date_created} date when the files were created
\item \code{content} url to download the files
\item \code{hash}md5 hash of the files
\item \code{filename} names of the files
\item \code{mimetype} mime type of the files
}
}
\description{
List all uploaded files related to a KoboToolbox API asset
}
\examples{
\dontrun{
kobo_setup()
uid <- "a9cwEQcbWqWzA5hzkjRUWi"
kobo_file_list(uid)
}

}
