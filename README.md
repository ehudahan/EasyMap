# EasyMap
An interactive web tool for evaluating and comparing associations of clinical variables and microbiome composition.

EasyMap, an interactive online tool allowing for \
(1) running multiple multivariate linear regression models, on the same features and metadata; \
(2) visualizing the associations between microbial features and clinical metadata found in each model; \
(3) comparing across the various models to identify the critical metadata variables and select the optimal model. \

EasyMap provides the user a side-by-side visualization of association results across the various models, each with additional metadata variables, enabling the user to evaluate the impact of each metadata variable on the associated feature. EasyMap’s interface enables filtering associations by significance, focusing on specific microbes and finding the robust associations that are found across multiple models. EasyMap takes the common task of multivariate linear regression to the next level, with an intuitive and simple user interface, allowing for wide comparisons of multiple models to identify the robust microbial feature associations.


## Step 1: Input Data Upload
The first step is uploading the user input data, which is a separator based file (csv, tsv etc.) including all relevant data: clinical metadata variables and taxonomic features’ abundance for each sample (Figure 1). This file should follow MaAsLin(Morgan et al., 2012) format (largely described below) and should have a header line. Suppose there are n metadata variables and m taxonomic features, the input file will have three sections of columns: (a) The first column will contain the sample ID, which is a unique identifier of samples; (b) The next n columns will contain the metadata variables, where each of the variables can be either all strings or all numeric but not mixture. These can include clinical measurements and also other information, such as subject ID, or collecting clinic; (c) The last m columns will contain the abundance of the taxonomic features (relative or absolute). All abundance data will be normalized by total sum scaling (TSS) normalization (MaAsLin2 default) and then will be transformed by the arc-sinus transformation (AST, Methods).

