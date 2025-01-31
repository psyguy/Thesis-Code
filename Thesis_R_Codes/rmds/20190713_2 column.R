---
  title: "Two column test"
# author: "Grant McDermott"
# date: "6/28/2019"
output:
  pdf_document:
  includes:
  in_header: preamble.tex
html_document:
  css: preamble.css
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, cache = T)
```

This is an example Rmarkdown script that allows for two column (side-by-side) code environments knitted to both PDF and HTML output. The key is to include two preamble files --- `preamble.tex` for PDF and `preamble.css` for HTML --- in the YAML. You can then mix LaTeX and HTML code within the same document, and the knitr engine will safely ignore any commands that aren't relevant to that output format. For example, it will ignore any HTML `<div>` tags when knitting to PDF. Conversely, it will ignore any LaTeX commands (e.g. `\btwocol`, `\etwocol`, and `\columnbreak`) when knitting to HTML.

To illustrate, here comes some code in two columns.

<div class="column-left">
\btwocol
#### Stata
```{stata, eval=F}
## This where your Stata code would go.
```
</div>
\columnbreak
<div class="column-right">
#### R
```{r, eval=F}
## And this is where your R code would go.
```
\etwocol
</div>

And then we could continue writing. We could still use regular code chunks spanning the whole page if we wanted.

```{r p}
library(ggplot2)
set.seed(1234)

p = 
  ggplot(
  data.frame(x= rnorm(1e4), y = rnorm(1e4)), 
  aes(x = x, y = y)
  ) +
  geom_hex() + 
  coord_fixed() +
  theme_void()
```

And then revert back to the side-by-side format when needed.

<div class="column-left">
\btwocol
#### Some people like viridis default
```{r viridis_default, dependson=p}
p + scale_fill_viridis_c()
```
</div>
\columnbreak
<div class="column-right">
#### Others prefer viridis magma
```{r viridis_magma, dependson=p}
p + scale_fill_viridis_c(option="A")
```
\etwocol
</div>
