% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crawler.R
\name{get_name_return_detail_url_by_party}
\alias{get_name_return_detail_url_by_party}
\title{Get Member Detail URLs and Names by Party}
\usage{
get_name_return_detail_url_by_party(html, parti)
}
\arguments{
\item{html}{An HTML document object obtained using `read_html`.}

\item{parti}{A string representing the political party.}
}
\value{
<<<<<<< HEAD
A list containing two elements: 
=======
A list containing two elements:
>>>>>>> c70c6e3b195ab3ed0ee9e1f3ccd22fdfd1a015ea
        - `member_detail_urls`: A vector of member detail URLs.
        - `member_names`: A vector of member names.
}
\description{
Extracts member detail URLs and names from an HTML document for a given party.
}
\examples{
# Example usage:
<<<<<<< HEAD
# Assume `html` is an HTML document object
html <- read_html("https://example.com/members")
=======
library(rvest)
# Assume `html` is an HTML document object
html <- read_html("https://www.riksdagen.se/en/members-and-parties/members/?parti=socdem")
>>>>>>> c70c6e3b195ab3ed0ee9e1f3ccd22fdfd1a015ea
get_name_return_detail_url_by_party(html, "socdem")
}
