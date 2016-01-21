`dinosvg` package version 0.2.0
===============================

### purpose of package

### rendering svgs

create a plot with `gsplot`

``` r
library(gsplot)
```

    ## This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information. Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

    ## 
    ## Attaching package: 'gsplot'

    ## The following objects are masked from 'package:graphics':
    ## 
    ##     abline, arrows, axis, curve, grid, legend, lines, mtext, par,
    ##     points, polygon, rect, segments, symbols, text, title

``` r
gs <- gsplot() %>% 
  points(y=1:11, x=1:11, 
           col="blue", pch=18, xlab='pizza', ylab='dogs') %>% 
  points(x=4:11, y=11:4, 
           col="red", pch=1) %>% 
  points(3:5,4:6,side=c(1,4), col='green', pch=14, ylab='cats') %>% 
  lines(2:4, c(2,2.6,2.3), col='blue')
gs
```

![](README_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
library(dinosvg)
```

    ## 
    ## Attaching package: 'dinosvg'

    ## The following object is masked from 'package:grDevices':
    ## 
    ##     svg

``` r
gs <- gsplot() %>% 
  points(y=1:11, x=1:11, 
           col="blue", pch=18, hovertext=paste0('text:',1:11), xlab='pizza', ylab='dogs', 
           id=paste0('point',1:11), 'fill-opacity'='0.3', 'stroke-opacity'=seq(0,1, length.out = 11)) %>% 
  points(x=4:11, y=11:4, 
           col="red", pch=1, hovertext=paste0('text:',11:4)) %>% 
  points(3:5,4:6,side=c(1,4), col='green', hovertext='green', pch=14, ylab='cats') %>% 
  lines(2:4, c(2,2.6,2.3), col='blue')
cat(svg(gs, as.string=TRUE))
```
<svg width="100" height="100">
  <circle cx="50" cy="50" r="40" stroke="green" stroke-width="4" fill="yellow" />
</svg>
