
#setwd("/home/tim/git/BPLE/BPLE")
system("pdflatex Manuscript.tex Manuscript.pdf")
system("bibtex Manuscript.aux") # to generate .bbl
system("pdflatex Manuscript.tex Manuscript.pdf")
system("pdflatex Manuscript.tex Manuscript.pdf")

file.copy("Manuscript.pdf","/home/tim/Dropbox/BPLE/Manuscript/Manuscript.pdf",
		overwrite=TRUE)


