# Tutorial

<!-- badges: start -->

[![R-CMD-check](https://github.com/MeikeWaaijers/theoraizer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MeikeWaaijers/theoraizer/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

Welcome to the `theoraizer` package! This R package can be used to develop a theory from scratch with the assistance of a Large Language Model (LLM). Follow the installation steps provided below to install `theoraizer` from this GitHub repository. Once installed, you can start exploring its features.

The functions in `theoraizer` and their output should be approached with caution. Depending on the specific LLM used, there may be financial implications. Furthermore, we wish to emphasise that the answers generated by an LLM should not be taken as absolute truth.


## How to Use theoraizer

#### Install theoraizer

```{r eval=FALSE}
if(!require("remotes")) {
  install.packages("remotes", dependencies = T)
}

if(!require("theoraizer")) {
  remotes::install_github("MeikeWaaijers/theoraizer")
}
library(theoraizer)
```

#### Quick Tryout

To quickly understand the capabilities of theoraizer, you can use the `cld()` function to generate a Causal Loop Diagram (CLD). This function use `causal_relation()`, `causal_direction()`, `causal_sign()` and `cld_plot()` functions included in theoraizer with default settings. For greater control and access to additional output, it's recommended to use these functions individually.

Access the help file for further instructions.

```{r eval=FALSE}
?cld
```

#### Individual Function Use

To create a theory from scratch, the functions in this R-packaged should be used in the following order:

`var_list()` --\> `causal_relation()` --\> `causal_direction()` --\> `causal_sign()` --\> `cld_plot()`

Access help files for specific functions as needed.

```{r eval=FALSE}
?var_list
?causal_relation
?causal_direction
?causal_sign
?cld
?cld_plot
```

#### Data

Ready-made data is available to get an insight into the various outputs generated by theoraizer. For further details, please refer to the help files associated with these datasets.

```{r eval=FALSE}
?vars
?rel
?dir
?rel_sign # Data for sign function when relation_df is inputted
?dir_sign # Data for sign function when direction_df is inputted
?cld_example
?edge_lists
```
