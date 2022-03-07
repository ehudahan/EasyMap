# EasyMap user manual #
EasyMap is an interactive web tool for evaluating and comparing associations of clinical variables and microbiome composition.

[EasyMap](https://ehudahan.shinyapps.io/easymap), an interactive online tool allowing for \
(1) running multiple multivariate linear regression models, on the same features and metadata; \
(2) visualizing the associations between microbial features and clinical metadata found in each model; \
(3) comparing across the various models to identify the critical metadata variables and select the optimal model.


#### If you use the EasyMap software, please cite our manuscript:
#### Ehud Dahan,Victoria M. Martin, Moran Yassour (2022). [EasyMap - An interactive web tool for evaluating and comparing associations of clinical variables and microbiome composition](???). Frontiers, ???

#### If you have questions, please don't hasistate to conctact us:
#### [Ehud Dahan](ehud.dahan@mail.huji.ac.il)
#### [Moran Yassour](???)

## Contents ##
* [Description](#description)
* [How to Run](#how-to-run)
    * [Step 1: Input data upload](#step-1-input-data-upload)
    * [Step 2: Variables type definition](#step-2-variables-type-definition)
    * [Step 3: Model construction](#step-3-model-construction)
    * [Output description](#output-description)
    * [Step 4: A birds-eye view of all significant results](#step-4-a-birds-eye-view-of-all-significant-results)
    * [Step 5a: Detailed view of selected associations](#step-5a-detailed-view-of-selected-associations)
    * [Step 5b: Alternating views of the raw data of selected associations](#step-5b-alternating-views-of-the-raw-data-of-selected-associations)
* [Run an example](#run-an-example)
* [Non-microbiome related](#non-microbiome-related)

# Tutorial

## Description ##

EasyMap provides the user a side-by-side visualization of association results across the various models, each with additional metadata variables, enabling the user to evaluate the impact of each metadata variable on the associated feature. EasyMap’s interface enables filtering associations by significance, focusing on specific microbes and finding the robust associations that are found across multiple models. EasyMap takes the common task of multivariate linear regression to the next level, with an intuitive and simple user interface, allowing for wide comparisons of multiple models to identify the robust microbial feature associations.

## How to run ##
## Step 1: Input data upload ##
The first step is uploading the user input data, which is a separator based file (csv, tsv etc.) including all relevant data: clinical metadata variables and taxonomic features’ abundance for each sample. This file should follow [MaAsLin](https://huttenhower.sph.harvard.edu/maaslin) format (largely described below) and should have a header line. Suppose there are n metadata variables and m taxonomic features, the input file will have three sections of columns:\
(a) The first column will contain the sample ID, which is a unique identifier of samples; \
(b) The next n columns will contain the metadata variables, where each of the variables can be either all strings or all numeric but not mixture. These can include clinical measurements and also other information, such as subject ID, or collecting clinic; \
(c) The last m columns will contain the abundance of the taxonomic features (relative or absolute). All abundance data will be normalized by total sum scaling (TSS) normalization (MaAsLin2 default) and then will be transformed by the arc-sinus transformation (AST).
Users can upload their separator based files (comma, tab) through the ‘Upload Files’ tab. Once uploaded, all the identified columns will appear and the user can click the “Submit” button and continue with the analysis. If there is any problem with parsing the file, the error will be presented to the user.

## Step 2: Variables type definition ##
After uploading the data, it is necessary to define the type of all clinical metadata variables. First, the user selects the column that represents the unique sample ID. Second, the user selects the variables that will be used as random effects in the linear model. The model will account for these variables, but will not search for associations between the random variables and the microbial features. Next, the user selects the fixed effect variables, which can be assigned as either continuous or categorical. All variables that remain unselected are automatically defined as the microbial features, thus all clinical variables must be selected as either random or fixed effect variables.


Categorical variables are automatically sorted alphabetically (for example, always, never, sometimes), however, if the user has a specific relevant order, the variable values can include a prefix to maintain this order (like, a_never, b_sometimes, c_always). Numeric variables that have four or less unique values will be treated as categorical variables. Once all variables are defined an “approve” button will appear at the bottom of the screen. 

## Step 3: Model construction ##
When searching for statistical-significant associations, we first need to choose the clinical variables that our model should account for. These variables are usually chosen based on prior understanding of the clinical situation, and also including factors that are known to impact the microbial community composition. Naively, one can include all collected variables in the model, however, including too many variables would lead to overfitting the data, and diluting the signal across too many variables, potentially missing the significant association altogether. Oftentimes, we choose multiple models, each containing a different set of examined variables, with the aim to compare the results across these models. EasyMap was built to enable a comprehensive comparison across various models, thus highlighting the strong, consistent associations across multiple models. 

After defining and approving the variables (as described in step 2) the user will next move to selecting the variables to be used in the first model. In the case of categorical variables, the user can also specify the reference value to be used for each variable. For example, if delivery_mode has two possible values: “C-section” or “vaginal”, the user can specify that “vaginal” will be the reference value. By default, the tool sorts the values alphabetically and the first value is used as reference. In the example above this would have been “C-section”.

Additional models can be added by clicking on the “add new set” button, and repeating this selection step for each model. By default, the new model is initiated with the selection of variables of the most recently defined model. While many models can be added and compared across, it impacts the total running time and the ease of results viewing in the next step, thus comparing 3-5 models seems ideal.

## Output description ##
The output of the EasyMap is composed of two sections: A heatmap of all significant results and a detailed view of selected associations (for example using box plots), with the ability to facet and color the raw data. All the results that are shown on the screen (heatmap with the detailed plots) can be exported to a pdf file.

## Step 4: A birds-eye view of all significant results ##
The first step in comparing the models is a high-level comparison of all microbial associations that were found to be significant in at least one model. Heatmap color represents the significance, and by default, the FDR q-value threshold is set to be 0.2 (only associations that pass this threshold appear in color). The user can further filter the presented microbial features, using the drop-down menus on the left. The user can select a different threshold, and also choose which models to include in the heatmap.

## Step 5a: Detailed view of selected associations ##
One unique and useful feature of EasyMap is the ability to toggle quickly between the bird’s eye view of all associations in the heatmap and zooming in on specific associations of interest. When the user hovers on a single cell in the heatmap, the cell is highlighted, and the relevant microbial feature together with the selected model, and associated clinical variable appear as text at the bottom of the panel. When the user clicks on a certain cell in the heatmap, the bottom panel is populated with a detailed plot showing the relative abundance (AST) of the selected microbial feature by the selected clinical variable (this can be either a box plot for a categorical variable or a scatter plot for a continuous clinical variable). Note that the relative abundance values (y-axis) are arc-sinus transformed thus can exceed 1, and range in [0, 1.57079]. The detailed plot also displays the q-values that are outputted by MaAsLin2 for all tested associations in this variable (using brackets comparing each value to the selected reference). Significance analysis appears for all possible comparisons between the reference and other values, with their respective q values, even for the non-significant comparisons.

## Step 5b: Alternating views of the raw data of selected associations ##
Finally, to include additional metadata to the existing plot, the user can facet the box plot and/or color the dots, by a specific variable. When the plot is faceted, the MaAsLin2 q-values are removed from the plot, and instead a t-test is performed, and p-values are presented. Finally, the user can color the dots based on the categorical variables of the model, and add labels to the dots, based on the random variables of the model. 

## Run an example ##
#### Clicking on the ‘example’ button in the ‘Upload Files’ tab loads a case study data as example.
In this case study, we considered six clinical variables that were collected in our cohort, and are also known to have an impact on gut microbiome composition: mode of delivery (vaginal or C-section), age (at time of visit), ethnicity, use of probiotics in the first year of life, infant diet at each time point (breastfed, formula-fed, mixed); and finally the allergy status (case/control). In this study we are searching for microbial features that are associated with the allergy status, taking into account all other clinical variables.
#### Add more steps to case study? because after loading the example - everything is the same for any data.

#### check this title
## Non-microbiome related ##
We developed EasyMap to assist us in analyzing data from our lab’s studies. It was built as a wrapper for MaAsLin2, with added visualization and comparison abilities, tailored for microbiome studies. However, it can be used in many additional contexts where multivariate linear models are commonly used, maintaining all its added value. EasyMap is also extremely useful for sharing results with collaborators, and enabling all participants to dig deeper in the analysis of their data.

