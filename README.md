# prettyods - Creating ODS files using R

There is currently no straightforward way to create ODS files from R with full control over text styles, sheet styles, and other formatting.
**prettyods** is an early prototype and the first step toward a fully featured R package that provides these capabilities.

You can test the current version with:

```r
source("https://raw.githubusercontent.com/Jakob256/prettyods/main/src.R")
```

## 👋🌍

```r
myStyle <- ODS_createStyle(font="Arial", size=20, italic=TRUE, rotate=20, color="orange")
sheet   <- ODS_createSheet("my first ODS sheet")

ODS_writeCell(sheet, "Hello World!", row=1, col=1, myStyle)
ODS_write(sheet, "hello.ods")
```
<p align="center" width="100%">
    <img width="33%" src="https://raw.github.com/Jakob256/prettyods/main/helloWorld.png"> 
</p>

