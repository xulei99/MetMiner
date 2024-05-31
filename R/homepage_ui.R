#' homepage of UI
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bsicons bs_icon
#' @noRd

## Part1.1 home page ---------------------------------------------------------------

homepage_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
  title = 'Home page',
  icon = bs_icon('house'),
  fluidRow(
    # HTML(
    #   "
    #   <div style='text-align:center;'>
    #   <img src='https://shawnmagic-1257599720.cos.ap-chengdu.myqcloud.com/picgoMetMiner.jpg' style='width: 320px; height: auto; margin: 10px auto;'>
    #   </div>
    #   "
    # ),
    tags$div(
      style = "text-align: center;",
      tags$img(src = "www/MetMiner.jpg", style = "width: 320px; height: auto; margin: 10px auto;")
    ),
    hr_head(),
    column(
      width = 8,offset = 2,
      style = "border: 2px dashed rgba(0, 128, 128, 0.5); border-radius: 10px; padding: 10px;",
      HTML(
        "
              <div style='text-align:center;'>
              <h1>What can I do ?</h1>
              </div>
              <hr style='border-top: 6px double #008080; border-bottom: 3px solid #008080;'> </hr>
              <div id = 'content' style='float:left'; margin-left: 20px; margin-right: 20px'>
                  This Shiny App is development based on <a href='https://www.tidymass.org/' target='_blank'>tidyMass</a> package for untargeted/pseudotargeted metabolomics data analysis. Developed by Dr. Xiaotao Shen, tidyMass is an outstanding open-source software for processing metabolomics data. Following the tidyverse development concept, tidyMass greatly enhances code readability. Moreover, the introduction of the <font style='bold' color = 'green'>mass_dataset object</font> makes the metabolomics data analysis process more transparent and reproducible. <br>
                  We developed this Shiny app to facilitate the use of tidyMass for non-programmers in metabolomics research. Additionally, we incorporated data visualization capabilities into the downstream analysis pipeline to enable more intuitive observation and presentation of results.<br>
                  As a result, this app provides a powerful and intuitive platform for analyzing and visualizing untargeted-metabolomics data.

              <h3> Data Import </h3>

              <ol>
              <li> Convert .raw data to .mzXML format Reference: <a href='https://www.tidymass.org/start/data_convert/' target='_blank'>Tidymass: data convert</a><br></li>
              <li> <font style='bold'>Option 1: </font>Import from MS file (.mzXML format) and do peak picking progress. Reference: <a href='https://www.tidymass.org/start/raw_data_processing/' target='_blank'>Tidymass: data processing</a><br></li>
              <li> <font style='bold'>Option 2: </font>Import metabolites accumulation profile data from table. Reference: <a href='https://massdataset.tidymass.org/articles/data_import_and_export.html' target='_blank'>mass_dataset: import from table</a><br></li>
              <li> Import MS2 information.<br></li>
              </ol>

              <h3> Data cleaning </h3>
              Based on <a href='https://www.tidymass.org/start/cleaning/' target='_blank'>Tidymass: Data cleaning</a>
              <ol>
              <li> Missing value detection.<br></li>
              <li> Remove noisy features.<br></li>
              <li> Remove outliers.<br></li>
              <li> Data normalization and integration.<br></li>
              </ol>

              <h3> Metabolite annotation </h3>
              Public databases used for metabolite annotation.
              <ol>
              <li> MS2: <a href='https://www.shenxt.info/ms2_data/mona_database0.0.3.rda' target='_blank'>MoNA.</a><br></li>
              <li> MS2: <a href='https://www.shenxt.info/ms2_data/massbank_database0.0.3.rda' target='_blank'>MassBank</a><br></li>
              <li> MS2: <a href='https://mona.fiehnlab.ucdavis.edu/spectra/browse?query=tags.text:%27ReSpect%27' target='_blank'>ReSpect</a><br></li>
              <li> MS2: <a href='https://mona.fiehnlab.ucdavis.edu/spectra/browse?query=tags.text%3D%3D%22RIKEN%20PlaSMA%22' target='_blank'>RIKEN_PlaSMA</a><br></li>
              <li> MS1: <a href='http://www.knapsackfamily.com/KNApSAcK_Family/' target='_blank'>KNApSAcK</a><br></li>
              <li> MS1: <a href='https://www.shenxt.info/ms2_data/kegg_ms1_database0.0.3.rda' target='_blank'>KEGG</a><br></li>
              <li> MS1: <a href='https://plantcyc.org/' target='_blank'>plantcyc</a><br></li>
              <li> Customized MS1 or MS2 database.<br></li>
              </ol>

              <h3> Metabolite classification </h3>
              The classification of metabolites is based on the <a href='https://jcheminf.biomedcentral.com/articles/10.1186/s13321-016-0174-y' target='_blank'>ClassyFire</a> database..
              <ol>
              <li> For metabolites that match the database we provide, classifiction information will directly provided.<br></li>
              <li> For metabolites matching the customized database, we will match them with the PubChem database based on their names and obtain classification information through <a href='https://cfb.fiehnlab.ucdavis.edu/' target='_blank'>Fiehn lab CFB API</a> after obtaining InChIKey.<br></li>
              </ol>

              <h3> Differential Metabolite Analysis (DAM) </h3>

              <ol>
              <li> Statistical difference test: t-test (pairwised), Analysis of Variance (ANOVA) <br></li>
              <li> Univariate analysis method: For pairwised DAM analysis, fold change (FC). <br></li>
              <li> Multivariate statistical analysis (unsupervised): Principal Component Analysis (PCA) <br></li>
              <li> Multivariate statistical analysis (supervised): Partial Least Squares Discriminant Analysis (PLS-DA) and Orthogonal Partial Least Squares Discriminant Analysis (OPLS-DA). <br></li>
              </ol>

              <h3> Metabolic pathway analysis </h3>

              <ol>
              <li> KEGG pathway analysis of annotated metabolites <br></li>
              <li> KEGG pathway analysis of DAM. <br></li>
              <li> KEGG enrichment of DAM (Detected metabolites as background) <br></li>
              <li> KEGG enrichment of DAM (The metabolites annotated in the corresponding species in the KEGG database are used as background.). <br></li>
              </ol>
              </div>
          "
      ),
      HTML(
        "
           <div id = 'header' style='text-align:center;'>
              <h1>How to cite ?</h1>
           </div>
           <hr style='border-top: 6px double #008080; border-bottom: 3px solid #008080;'> </hr>
           <div id = 'content' style='float:left'; margin-left: 20px; margin-right: 20px'>

           If you have used this app for metabolomics analysis in your publication, please cite following papers:<br>
           <h3> MetMiner </h3>
           <br>
           MetMiner: A user-friendly pipeline for large-scale plant metabolomics data

           <h3> TidyMass </h3>
           <br>
           Shen X, Yan H, Wang C, Gao P, Johnson CH, Snyder MP. TidyMass an object-oriented reproducible analysis framework for LC-MS data. Nat Commun. 2022;13(1):4365. Published 2022 Jul 28. <a href='https://www.nature.com/articles/s41467-022-32155-w' target='_blank'>  doi:10.1038/s41467-022-32155-w</a>.<br>
           <h3> TBtools </h3>
           TBtools plugin solves the problem of installing dependencies. If you start this app through TBtools, please cite this parper at the same time:<br>
           <br>
           Chen, C. J., et al. (2023). TBtools-II: 'A one for all, all for one' bioinformatics platform for biological big-data mining. Molecular Plant 16(11): 1733-1742. <a href='10.1016/j.molp.2023.09.010' target='_blank'> doi:10.1016/j.molp.2023.09.010 </a><br>
           <br>
           Thanks very much!
           </div>
          "
      )
    )
  ),
  # 添加Footer
  hr_head(),
  HTML(
    "
      <div style='text-align:center; margin-top: 20px;'>
          Prof. Xuebin Zhang's Lab<br>
          State Key Laboratory of Crop Stress Adaptation and Improvement, <br>
          Henan Joint International Laboratory for Crop Multi-Omics Research, <br>
          School of Life Sciences, Henan University, Kaifeng 475004, China
      </div>
    "
  )
)
}
