#' @title Genomic Data Retrieval
#'
#' @description This package interacts with a suite of web Application
#' Programming Interfaces and FTP sites to perform automated genomic data
#' retieval and annotation information retrieval.
#'
#' @section About:
#' To automate the retrieval process on a meta-genomic scale, this package
#'  provides useful interface functions for genomic sequence retrieval and
#'  functional annotation retrieval.
#'  The major aim of \code{biomartr} is to facilitate computational
#'  reproducibility and large-scale handling of genomic data for
#'  (meta-)genomic analyses.
#'
#' In detail, \code{biomartr} aims to provide users with an easy to use
#' framework to obtain genome, proteome, CDS, GFF (annotation), genome
#' assembly quality, and metagenome project data. Furthermore, an interface to
#' the Ensembl Biomart database allows users to retrieve functional annotation
#' for genomic loci.
#' Users can download entire databases
#' such as
#' \itemize{
#' \item \code{NCBI RefSeq}
#' \item \code{NCBI nr}
#' \item \code{NCBI nt}
#' \item \code{NCBI Genbank}
#' \item \code{NCBI nt}
#' \item \code{Ensembl}
#' \item \code{Ensembl Genomes}
#' \item \code{UniProt}
#' }
#' @name biomartr-package
#' @aliases biomartr
#' @docType package
#' @author Hajk-Georg Drost \email{hajk-georg.drost@tuebingen.mpg.de}
#' @keywords package
## usethis namespace: start
#' @importFrom data.table data.table
#' @importFrom data.table :=
#' @importFrom data.table .SD
#' @importFrom data.table .BY
#' @importFrom data.table .N
#' @importFrom data.table .I
#' @importFrom data.table .GRP
#' @importFrom data.table .NGRP
#' @importFrom data.table .EACHI
#' @importFrom data.table fread fwrite rbindlist data.table chmatch
## usethis namespace: end
NULL
