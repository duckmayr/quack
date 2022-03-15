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

## Included Files

## Software Used to Obtain Reported Results

- Operating System: [Your OS, including version, here]
- R version: [The R version last used to run your replication code here]
- RStudio version: [The RStudio version last used to run your replication code here]
- R extension packages:
    + ExamplePackageTitle, version VersionNumber
    + ExamplePackageTitle2, version VersionNumber

## Steps to Take to Reproduce Reported Results

1. Ideally the first step here builds your analyzed dataset from raw data (though sometimes this isn't possible due to sharing restrictions)
2. Ideally the next step is to simply knit the R Markdown file that generates the paper
3. But in some circumstances it would be appropriate for Step 2 to run (a) separate analysis script(s) or knit a separate analysis R Markdown file (perhaps in its/their own `analysis/` subdirectory), then to knit the main paper R Markdown file in Step 3
4. Finally, you may have an appendix R Markdown file that needs to be knit to reproduce other reported results (though of course your specific reproduction steps may vary from the suggested sequence here)
