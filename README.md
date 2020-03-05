# MetaboAnalyst
Project objective: To provide a user-friendly, web-based analytical pipeline for high-throughput metabolomics studies. In particular, MetaboAnalyst aims to offer a variety of commonly used procedures for metabolomic data processing, normalization, multivariate statistical analysis, as well as data annotation. The current implementation focuses on exploratory statistical analysis, functional interpretation, and advanced statistics for translational metabolomics studies.

Data formats: Diverse data types from current metabolomic studies are supported (details) including compound concentrations, NMR/MS spectral bins, NMR/MS peak intensity table, NMR/MS peak lists, and LC/GC-MS spectra.

Data processing: Depending on the type of the uploaded data, different data processing options are available (details). This is followed by data normalization steps including normalization by constant sum, normalization by a reference sample/feature, sample specific normalization, auto/Pareto/range scaling, etc.

Statistical analysis: A wide array of commonly used statistical and machine learning methods are available: univariate - fold change analysis, t-tests,volcano plot, and one-way ANOVA, correlation analysis; multivariate - principal component analysis (PCA), partial least squares - discriminant analysis (PLS-DA) and orthogonal partial least squares - discriminant analysis (Orthogonal PLS-DA); high-dimensional feature selection - significance analysis of microarrays (and metabolites) (SAM) and empirical Bayesian analysis of microarrays (and metabolites) (EBAM); clustering - dendrogram, heatmap, K-means, and self organizing map (SOM)); supervised classification - random forests and support vector machine (SVM).

Functional enrichment analysis: The service performs metatolite set enrichment analysis (MSEA) for human and mammalian species. It can accept a list of compound names, a list of compound names with concentrations, or a concentration table. The analysis is based on several libraries containing ~6300 groups of biologically meaningful metabolite sets collected primarily from human studies;

Metabolic pathway analysis: The service currently supports pathway analysis (including pathway enrichment analysis and pathway topology analysis) and visualization for 21 model organisms, including Human, Mouse, Rat, Cow, Chicken, Zebrafish, Arabidopsis thaliana, Rice, Drosophila, Malaria, Budding yeast, E.coli., etc., with a total of 1600 pathways;

Time series and Two-factor data analysis: The service currently supports clustering and visualization (including interactive 3D PCA visualization and two-way heatmaps with hierarchical clustering), two-way ANOVA for univariate two-factor analysis, multivariate empirical Bayes time-series analysis (MEBA) for detecting distinctive temporal profiles across different experimental conditions, and ANOVA-simultaneous component analysis (ASCA) for identification of major patterns associated with each experimental factor (and their interactions);

Biomarker analysis: The service provides receiver operating characteristic (ROC) curve based approach for evaluating the performance of potential biomarkers. It offers classical univariate ROC analysis as well as more modern multivariate ROC curve analysis based on PLS-DA, SVM or Random Forests. In addition, users can manually pick biomarkers or to set up hold-out samples for flexible evaluation and validation;

Sample size and power analysis: Users can upload a dataset either from a pilot study or from a similar study to compute the minimum number of samples required to detect the effect within a certain degree of confidence, as well as to estimate the power of the current study design.

Joint pathway analysis: The service allows users to simultaneously analyze genes and metabolites of interest within the context of metabolic pathways. Only data from human, mouse and rat are supported currently.

MS peaks to pathway activities: Users can upload LC-MS peaks to perform metabolic pathway enrichment analysis and visual exploration based on the well-established mummichog and GSEA algorithms. It currently supports 21 organisms including Human, Mouse, Zebrafish, C. elegans, and other species.

Network explorer: Users can upload one or two lists of metabolites, genes, or KEGG orthologs (i.e. generated from metagenomics), and then visually explore these molecules of interest within the context of biological networks such as KEGG global metabolic network, as well as several networks created based on known associations between genes, metabolites, and diseases.

Biomarker meta-analysis: Users can upload several metabolomics data sets obtained under comparable conditions to identify robust biomarkers across multiple studies. It currently supports meta-analysis approaches based on p-value combination, vote counts and direct merging. The results can be explored in interactive Venn diagram.

Image generation: Important images can be re-produced in high resolution in various format such as .png, .tiff, .ps, etc for publication purposes

Report generation: Upon completion, a comprehensive PDF report will be generated documenting each step performed along with corresponding tabular and graphical results. The processed data and images are also available for download.
