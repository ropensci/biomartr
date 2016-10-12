

docFile <- function(file.name, 
                    organism, 
                    url, 
                    database, 
                    path, 
                    refseq_category, 
                    assembly_accession,
                    bioproject,
                    biosample,
                    taxid,
                    infraspecific_name,
                    version_status,
                    release_type,
                    genome_rep,
                    seq_rel_date,
                    submitter){

        cwd <- getwd()

        setwd(path)

        sink(paste0("doc_",organism,"_db_",database,".txt"))

        cat(paste0("File Name: ", file.name))
        cat("\n")
        cat(paste0("Organism Name: ", organism))
        cat("\n")
        cat(paste0("Database: NCBI ", database))
        cat("\n")
        cat(paste0("URL: ", url))
        cat("\n")
        cat(paste0("Download_Date: ", date()))
        cat("\n")
        cat(paste0("refseq_category: ", refseq_category))
        cat("\n")
        cat(paste0("assembly_accession: ", assembly_accession))
        cat("\n")
        cat(paste0("bioproject: ", bioproject))
        cat("\n")
        cat(paste0("biosample: ", biosample))
        cat("\n")
        cat(paste0("taxid: ", taxid))
        cat("\n")
        cat(paste0("infraspecific_name: ", infraspecific_name))
        cat("\n")
        cat(paste0("version_status: ", version_status))
        cat("\n")
        cat(paste0("release_type: ", release_type))
        cat("\n")
        cat(paste0("genome_rep: ", genome_rep))
        cat("\n")
        cat(paste0("seq_rel_date: ", seq_rel_date))
        cat("\n")
        cat(paste0("submitter: ", submitter))
        
        sink()

        setwd(cwd)

}



setTMPFile <- function(file.name){

        tempfile(pattern = file.name, tmpdir = tempdir(), fileext = "")
}


getTMPFile <- function(file.name){

        file.path(tempdir(),file.name)

}



test <- function(x){ print(paste0("Test ",x," passed.","\n"))}
