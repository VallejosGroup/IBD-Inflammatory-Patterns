###########################################
#-- Create directories and readme files --#
###########################################

if (!dir.exists("plots")) {
  dir.create("plots")

  fileConn <- file("plots/README.md")

  # write lines to the file connection
  writeLines(
    c(
      "# README",
      "",
      paste(
        "This directory contains plots created by the analysis",
        "but are not provided as figures (supplementary or",
        "otherwise) in the paper"
      )
    ),
    fileConn
  )
  # close the file connection
  close(fileConn)
  rm(fileConn)
}

if (!dir.exists("cache")) {
  dir.create("cache")
  fileConn <- file("cache/README.md")
  writeLines(
    c(
      "# README",
      "",
      paste(
        "This directory stores cached versions of R objects which",
        "take a long time to create (such as LCMM fit objects)."
      )
    ),
    fileConn
  )
  close(fileConn)
  rm(fileConn)
}

if (!dir.exists("paper")) {
  dir.create("paper")
  fileConn <- file("paper/README.md")
  writeLines(
    c(
      "# README",
      "",
      paste(
        "This directory contains all figures used in the paper.",
        "The sup subdirectory contains supplementary figures"
      )
    ),
    fileConn
  )
  close(fileConn)
  rm(fileConn)
}

if (!dir.exists("paper/sup")) dir.create("paper/sup")
if (!dir.exists("plots/residuals")) dir.create("plots/residuals")
if (!dir.exists("plots/ma-nocor")) dir.create("plots/ma-nocor")
