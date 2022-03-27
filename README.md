# classingFunction
Groups a variable by its own quartiles (the thresholds for each group being p25, p50 and p75) then outputs an aggregate of 2nd variable.

### Languages and Tools
<div>
  <img src="https://github.com/devicons/devicon/blob/master/icons/r/r-original.svg" title="r" alt="r" width="70" height="70"/>&nbsp;
  <img src="https://github.com/devicons/devicon/blob/master/icons/rstudio/rstudio-original.svg" title="RStudio" alt="RStudio" width="70" height="70"/>&nbsp;
</div>

### Packages
<div>
  <img src="https://github.com/tidyverse/dplyr/raw/main/man/figures/logo.png" height="100" style="max-width: 100%;"/>&nbsp;
  <img src="https://github.com/tidyverse/rlang/raw/main/man/figures/logo.png" height="100" style="max-width: 100%;"/>&nbsp;
</div>
<br>
<br>

```r
classingFunction <- function(.data, dimension, metric) {
  require(dplyr)
  require(rlang)
  
  if (is.numeric( .data %>% select({{ dimension }}) %>% as.matrix() )  )   {
    vector1 <- .data %>% select({{ dimension }}) %>% as.matrix()
    vector2 <- if_else(vector1 <= quantile(vector1, prob = 0.25), '> p0, <= p25',
                       if_else(vector1 <= quantile(vector1, prob = 0.50), '> p25, <= p50',
                               if_else(vector1 <= quantile(vector1, prob = 0.75), '> p50, <= p75', '> p75, <= p99')
                               )
                       ) %>%
      as.data.frame() %>%
      rename(dimension_class := '.') 
      
    
    bind_cols(.data, vector2) %>% 
      group_by(dimension_class) %>% 
      summarise(mean = mean({{ metric }}), .groups = 'drop')
  } else {
    print('Cast the dimension variable to either a numeric or integer. Only numerical data is allowed')
  }
}
```
