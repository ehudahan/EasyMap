# EasyMap
An interactive web tool for evaluating and comparing associations of clinical variables and microbiome composition.

EasyMap, an interactive online tool allowing for \
(1) running multiple multivariate linear regression models, on the same features and metadata; \
(2) visualizing the associations between microbial features and clinical metadata found in each model; \
(3) comparing across the various models to identify the critical metadata variables and select the optimal model.

EasyMap provides the user a side-by-side visualization of association results across the various models, each with additional metadata variables, enabling the user to evaluate the impact of each metadata variable on the associated feature. EasyMap’s interface enables filtering associations by significance, focusing on specific microbes and finding the robust associations that are found across multiple models. EasyMap takes the common task of multivariate linear regression to the next level, with an intuitive and simple user interface, allowing for wide comparisons of multiple models to identify the robust microbial feature associations.


## Step 1: Input Data Upload
The first step is uploading the user input data, which is a separator based file (csv, tsv etc.) including all relevant data: clinical metadata variables and taxonomic features’ abundance for each sample. This file should follow [MaAsLin](https://huttenhower.sph.harvard.edu/maaslin) format (largely described below) and should have a header line. Suppose there are n metadata variables and m taxonomic features, the input file will have three sections of columns:\
(a) The first column will contain the sample ID, which is a unique identifier of samples; \
(b) The next n columns will contain the metadata variables, where each of the variables can be either all strings or all numeric but not mixture. These can include clinical measurements and also other information, such as subject ID, or collecting clinic; \
(c) The last m columns will contain the abundance of the taxonomic features (relative or absolute). All abundance data will be normalized by total sum scaling (TSS) normalization (MaAsLin2 default) and then will be transformed by the arc-sinus transformation (AST).
Users can upload their separator based files (comma, tab) through the ‘Upload Files’ tab. Once uploaded, all the identified columns will appear and the user can click the “Submit” button and continue with the analysis. If there is any problem with parsing the file, the error will be presented to the user.

## Step 2: Variables type definition
After uploading the data, it is necessary to define the type of all clinical metadata variables. First, the user selects the column that represents the unique sample ID. Second, the user selects the variables that will be used as random effects in the linear model. The model will account for these variables, but will not search for associations between the random variables and the microbial features. Next, the user selects the fixed effect variables, which can be assigned as either continuous or categorical. All variables that remain unselected are automatically defined as the microbial features, thus all clinical variables must be selected as either random or fixed effect variables.


Categorical variables are automatically sorted alphabetically (for example, always, never, sometimes), however, if the user has a specific relevant order, the variable values can include a prefix to maintain this order (like, a_never, b_sometimes, c_always). Numeric variables that have four or less unique values will be treated as categorical variables. Once all variables are defined an “approve” button will appear at the bottom of the screen. 

## Step 3 - Model construction
When searching for statistical-significant associations, we first need to choose the clinical variables that our model should account for. These variables are usually chosen based on prior understanding of the clinical situation, and also including factors that are known to impact the microbial community composition. Naively, one can include all collected variables in the model, however, including too many variables would lead to overfitting the data, and diluting the signal across too many variables, potentially missing the significant association altogether. Oftentimes, we choose multiple models, each containing a different set of examined variables, with the aim to compare the results across these models. EasyMap was built to enable a comprehensive comparison across various models, thus highlighting the strong, consistent associations across multiple models. 
After defining and approving the variables (as described in step 2) the user will next move to selecting the variables to be used in the first model (Figure 1). In the case of categorical variables, the user can also specify the reference value to be used for each variable. For example, if delivery_mode has two possible values: “C-section” or “vaginal”, the user can specify that “vaginal” will be the reference value. By default, the tool sorts the values alphabetically and the first value is used as reference. In the example above this would have been “C-section”. 
Additional models can be added by clicking on the “add new set” button, and repeating this selection step for each model. By default, the new model is initiated with the selection of variables of the most recently defined model. While many models can be added and compared across, it impacts the total running time and the ease of results viewing in the next step, thus comparing 3-5 models seems ideal.


