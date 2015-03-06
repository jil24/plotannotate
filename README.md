# plotannotate
This is an R package with some tools to annotate R plots made with its built-in plotting functions. It is primarily intended to help automate the creation of plots for print publication.

The motivation behind this package was my use of R to prepare simple plots and charts for publication. Plots using base graphics came very close to fulfilling my needs, but I found myself repeatedly adding certain types of annotations to R's output manually using vector graphics editors. This is fine but it is slow and error-prone, so I wrote these functions based essentially on my own needs.

* `errorbars()` draws error bars (SEM or CI) and/or a marker of central tendency (mean, median, mode) on an existing stripchart, beeswarm, or boxplot. It can also draw an error bar-only plot. It supports calculating error bar limits using raw data.
* `connectionlines()` draws lines/brackets between all or some boxes, bars, stripcharts, etc. with optional labels. These are commonly used to report p-values, but you could use them for other things.

_Note 1_: Since I wrote the `errorbars()` function I discovered the more mature `gplots` CRAN package that contains the function `plotCI()`, which performs a similar task and may be a better choice if it meets your needs.

_Note 2_: More advanced graphing frameworks such as ggplot2 provide many more features than this and may be a more appropriate choice, depending on your desired application.