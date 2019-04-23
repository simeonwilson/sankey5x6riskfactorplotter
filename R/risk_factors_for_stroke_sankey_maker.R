#' create a sankey diagram.
#'
#' it is only necessary to use the function, "risk_factors_for_stroke_diagram" with the properly defined parameters
#' takes in 5X6 dataframes with specific variables in the first column and the data for each particular year in the following columns
#' @param data_to_be_plotted1 a 5x6 dataframe with properly defined variables the labels row should be defined with risk_type  year1  year2 year3 year4 year5. and the risk_ type column should be defined as risk_type, Obesity, Hypertension, Diabetes, Hypercholesterolemia, and Smoking.    order of the first column does not matter, however the labels row must be in the specified order
#' @param plot_title_in_quotes1 The preferred title of the plot in " "
#' @param horizontal_spaces adds horizontal spaces, must be defined as TRUE or FALSE
#' @param vertical_spaces adds vertical spaces, must be defined TRUE or FALSE
#' @export
risk_factors_for_stroke_sankey_diagram<- function(data_to_be_plotted1, plot_title_in_quotes1, horizontal_spaces = TRUE ,vertical_spaces = TRUE){
  library(ggplot2)
  print("data_to_be_plotted must be a 5x6 data.frame with types Obesity, Hypertension, Diabetes, Hypercholesterolemia, and Smoking in the first column, and years in the first row, the order of the type variables does not matter but the year columns should be in order.
        #####################################################################                                                                       risk_type  year1  year2 year3 year4 year5                                                                                                       type1      VALUE  VALUE VALUE VALUE VALUE                                                                                                   type2      VALUE  VALUE VALUE VALUE VALUE                                                                                                    type3      VALUE  VALUE VALUE VALUE VALUE                                                                                                    type4      VALUE  VALUE VALUE VALUE VALUE                                                                                                    type5      VALUE  VALUE VALUE VALUE VALUE                       ")

  if (horizontal_spaces == TRUE & vertical_spaces == TRUE) { return(risk_factors_for_stroke_sankey_diagram_with_space(data_to_be_plotted1, plot_title_in_quotes1))
  }
  if (horizontal_spaces == FALSE & vertical_spaces == FALSE) { return(risk_factors_for_stroke_sankey_diagram_with_no_spaces(data_to_be_plotted1, plot_title_in_quotes1))
    # resume
  }
  if (horizontal_spaces == TRUE & vertical_spaces == FALSE) { return(risk_factors_for_stroke_sankey_diagram_with_horizontal_space_only(data_to_be_plotted1, plot_title_in_quotes1))

  }
  if (horizontal_spaces == FALSE & vertical_spaces == TRUE) { return(risk_factors_for_stroke_sankey_diagram_with_vertical_space_only(data_to_be_plotted1, plot_title_in_quotes1))

  }}
