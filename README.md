# plotannotate
This is an R package with some tools to annotate plots made with R's built-in
plotting functions. It is primarily intended to help automate the creation of
plots for print publication.

The motivation behind this package was my own use of R to prepare simple plots and
charts for publication. Plots using base graphics came very close to fulfilling
my needs, but I found myself repeatedly adding certain types of annotations to
R's output manually using vector graphics editors. This works, but it is slow
and you need to redo it when you change the data, so I wrote these functions
based on tasks I had to do many times as datasets grew.

* `errorbars()` draws error bars (SEM or CI) and/or a marker of central tendency
  (mean, median, mode) on an existing stripchart, beeswarm, or boxplot. It can
  also draw an error bar-only plot. It supports calculating error bar limits
  using raw data.
* `connectionlines()` draws lines/brackets between all or some boxes, bars,
  stripcharts, etc. with optional labels. These are commonly used to report 
  p-values, but you could use them for other things.
* `simpleFormat()` lets you use a simple markdown inspired notation to generate
  `plotmath` expressions for bold, italic, superscript, subscript. Basically 
  things you might want in a text label (not math).

# Installation
At an R console, issue these commands:
```
install.packages("devtools")
library("devtools")
install_github("jil24/plotannotate")
```

_Note 1_: Since I wrote the `errorbars()` function I discovered the more mature
`gplots` CRAN package that contains the function `plotCI()`, which performs a
similar task and may be a better choice if it meets your needs.
