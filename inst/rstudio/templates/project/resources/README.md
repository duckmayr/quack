# Title Here

The `quack` research project template is designed to help you produce reproducible research.
An important part of that product is a useful README file.
This file should tell others:

- what to find in the replication archive
- what software was used to produce all results presented in the paper
    + not *just* what R version you used, but also the versions of all R extension packages used
    + also the operating system used, since in rare cases small differences could be obtained on different operating systems
- what specific steps to take to reproduce all results presented in the paper

The `quack` developer(s) recommend using [`renv`](https://cran.r-project.org/package=renv) to ensure that the same R package versions are used when others run your code, but if not (and perhaps even if so), this information should be listed in the README.
