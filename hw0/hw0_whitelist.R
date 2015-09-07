allowed_files = c("hw0.Rmd",
                  "README.md",
                  "wercker.yml")

pass = TRUE
for(file in dir())
{
  if (!(file %in% allowed_files))
  {
    cat("Disallowed file:",file,"\n")
    pass = FALSE
  }
}

if (!pass)
    stop("Disallowed files found:\n\n")

