setwd('.build') # if necessary

require(knitr) # required for knitting from rmd to md
require(markdown) # required for md to html
rmd_to_html <- function(input_rmd, output_stem, markdown_only=FALSE) {
    output_rmd <- paste0(output_stem, '.rmd')
    output_md <- paste0(output_stem, '.md')
    output_html <- paste0(output_stem, '.html')
    output_tex <- paste0(output_stem, '.tex')
    file.copy(input_rmd, output_rmd, overwrite=TRUE)
    knit(output_rmd, output_md) # creates md file
    markdownToHTML(output_md, output_html,
               options='fragment_only') # creates html file
    output_html
}

# Combine component HTML files
html_files <- rmd_to_html('../cheat-sheet-lavaan/cheat-sheet-lavaan.rmd', 'cheat-sheet-lavaan')
html_files <- c(html_files,
                rmd_to_html('../cfa-example/cfa-example.rmd', 'cfa-example'))
html_files <- c(html_files, 
                rmd_to_html('../ex1-paper/ex1-paper.rmd', 'ex1-paper'))
html_files <- c(html_files, 
                rmd_to_html('../ex2-paper/ex2-paper.rmd', 'ex2-paper'))
html_files <- c(html_files, 
                rmd_to_html('../path-analysis/path-analysis.rmd', 'path-analysis'))

# HTML to LaTeX to PDF
combined_stem <- 'combined'
combined_html <- paste0(combined_stem, '.html')
combined_tex <- paste0(combined_stem, '.tex')
combined_pdf <- paste0(combined_stem, '.pdf')
system(paste('cat', paste(html_files, collapse=' '), '>', combined_html))

system(paste('pandoc --toc -s', combined_html, ' -o', combined_tex))
system(paste('pdflatex -interaction nonstopmode', combined_tex))
system(paste('pdflatex -interaction nonstopmode', combined_tex))
system(paste('pdflatex -interaction nonstopmode', combined_tex))
system(paste('gnome-open', combined_pdf))
