CreateBackUp <- function(from = "C:/", to = "C:/R_Backups", exts = c(".R", ".cpp")){
  
  dirTop = from#"L:\\R-Programs\\"
  today = as.character(Sys.Date())
  dirBackup = paste0(to, "/" ,gsub("-","_",today), "/")
  dir.create(dirBackup)
  
  .ListDirs <- function(path=".", pattern=NULL, all.dirs=FALSE,
                       full.names=FALSE, ignore.case=TRUE) {
    # use full.names=TRUE to pass to file.info
    all <- list.files(path, pattern, all.dirs,
                      full.names=TRUE, recursive=FALSE, ignore.case)
    dirs <- all[file.info(all)$isdir]
    # determine whether to return full names or just dir names
    if(isTRUE(full.names))
      return(dirs)
    else
      return(basename(dirs))
  }
  
  .ListTree <- function(dir) {
    if (file.info(dir)$isdir) {
      #if directory, recursivelly get files
      files <- list.files(dir, full.names   = TRUE, include.dirs = TRUE)
      out <- parallel::mclapply(files, .ListTree) #lapply -> go through all dirs/files recursivelly and return list of files
      # it is recursive - it'll keep digging through everything until it has exhausted all folders and files
      names(out) <- basename(files)
    } else {
      #if not directory, it is a file, add it to output
      out <- dir
    }
    out #returns the file
  }
  
  listFiles <- .ListTree(from)
  R_Files = unlist(listFiles)[grep("\\.R$", unlist(listFiles), ignore.case = TRUE)]
  file.copy(from = R_Files, to = dirBackup, copy.date = TRUE, recursive = TRUE)
  
}


