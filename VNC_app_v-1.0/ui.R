library(shiny)
library(markdown)

# Content for credits panel, shown on each page
creds <- sidebarPanel(
  h4("Credits"),
  br(),
  HTML("<p> This tool was created to implement the 
      Visual Neighbourhood Configuration (VNC) 
      approach first introduced in this paper:</p> 
      <p> <strong>Brughmans, T., Garderen, M. van, & Gillings, M.</strong> (In press). 
      Introducing visual neighbourhood configurations for total viewsheds.
      <em>Journal of Archaeological Science.</em></p>"),
  br(),br(),
  HTML("<p>We would like to thank the following people for discussing, reviewing and enabling this research: 
       Ulrik Brandes, Zoran &#268;u&#269;kovi&cacute;, Corinne Hofman, Till Sonnemann.</p>"),
  br(),br(),
  h6(
    HTML(
      "The work presented here was performed as part of the Caribbean Connections:
      Cultural Encounters in a New World Setting project, financially supported by
      the HERA Joint Research Programme, and the European Union's Seventh Framework
      Programme for research, technological development and demonstration under
      grant agreement n&ordm; 1133; and as part of the project NEXUS1492
      (http://www.nexus1492.eu/), which has received funding from the European
      Research Council under the European Union's Seventh Framework Programme (FP7/2007-2013)
      / ERC grant agreement n&ordm; 319209."
    )
  ),
  img(
    src = "combined.png",
    height = 75,
    width = 200
  ),
  br(),br(),br(),
  HTML("&copy; Mereke van Garderen, 2017")
)# end of credits panel

# Interface
navbarPage(
  title = "VNC Analysis Tool v.1.0",
  id = "navbar",
  
  tabPanel(
    title = "Introduction",
    value = "0",
    sidebarLayout(
      creds,
      mainPanel(
        h2("Instructions"),
        br(),
        h4("Welcome to the VNC Analysis Tool!"),
        br(),
        HTML("<p>This tool allows you to upload total viewsheds, create visual neighbourhood configurations 
            with a desired shape, size and structure, and to use computational methods to analyse the total 
            viewsheds using the generated configurations.</p>
            <p>The <strong>Visual Neighbourhood Configuration (VNC)</strong> approach is designed to 
            enable formally expressing hypotheses about the way in which a particular visual property 
            structures space in a small area.</p> 
            <p>A VNC specifies the size and shape of the surrounding area (i.e. the <em>neighbourhood</em>) 
            that is taken into account when analysing a specific location. A <em>structure</em>, subdividing 
            the neighbourhood into smaller areas for which different visual properties are assumed, 
            and <em>expected visual property values</em> for specific locations within the neighbourhood 
            can also be incorporated in the VNC to explore more complex assumptions.</p>
            <p> Subsequently a total viewshed of the study area 
            can be analysed with respect to the VNC, computing for each location a value that reflects 
            the visual properties of the neighbourhood. Archaeological assumptions can then be evaluated 
            by comparing the resulting values of the locations of known settlements or other 
            archaeological features to those in areas where no such features are located.</p>
            <p>On the following pages you can upload the files you want to use, specify the analyses you 
             would like to run, and select the desired output files.</p>"),
        br(),br(),
        actionButton("next1", strong(span("Continue", style = "color:blue"))),
        textOutput("tnx1"),
        br(),br()
      )
    )
  ),
  
  tabPanel(
    title = "Viewshed",
    value = "1",
    sidebarLayout(
      creds,
      mainPanel(
        h3("Upload total viewshed(s)"),
        h5(em("A viewshed file should be a text file (*.txt, *.asc, *.csv, *.tab) containing a table.
           The table represents the study area, each cell should contain a number representing 
              the visual property value of the corresponding grid cell.")),
        br(),
        radioButtons(
          inputId = "vsmode",
          label = h4("Viewshed mode"),
          choiceNames = list(
            HTML("Use a single total viewshed <br> (e.g. undirected, views-to or views-from)"),
            HTML("Use two total viewsheds <br> (e.g. views-to and views-from)")
          ),
          choiceValues = list(1,2),
          selected = 1
        ),
        br(),
        fileInput("vsfile1", label = h4("Viewshed file 1")),
        conditionalPanel(
          "input.vsmode == 2",
          fileInput("vsfile2", label = h4("Viewshed file 2"))
        ),
        textOutput("vs"),
        h4("Additional file settings"),
        selectInput(
          inputId = "vssep",
          label = h5("Column separator:"),
          choices = list(
            "Space" = 1,
            "Tab" = 2,
            "Semicolon" = 3,
            "Comma" = 4
          ),
          selected = 1
        ),
        checkboxInput("vshead", label = "File contains headers", value = FALSE),
        checkboxInput("remneg", label = "Exclude negative values from study area", value = TRUE),
        checkboxInput("nonorm", label = HTML("Turn off normalization
                                             <br> <em> Use only if you know what you are doing! </em>"), value = FALSE),
        br(),br(),
        actionButton("back0", strong(span("Back", style = "color:blue"))),
        actionButton("next2", strong(span("Continue", style = "color:blue"))),
        textOutput("tbk0"),
        textOutput("tnx2"),
        br(),br()
      )
    )
  ),
  
  tabPanel(
    title = "Neighbourhood",
    value = "2",
    sidebarLayout(
      creds,
      mainPanel(
        h3("Define neighbourhood"),
        h5(em("The neighbourhood is the area around each focal cell that will be taken into consideration in the analysis. Here you can define the size and structure of the neighbourhood you would like to analyze.")),
        br(),
        radioButtons(
          inputId = "nbmode",
          label = h4("Neighbourhood mode"),
          choices = list(
            "Use existing neighbourhood mask" = 1,
            "Generate neighbourhood mask with distance bands" = 2,
            "Generate neighbourhood mask with gradual increase/decrease of expectation value" = 3,
            "Generate neighbourhood mask with wedges" = 4
          ),
          selected = 2
        ),
        br(),
        conditionalPanel(
          "input.nbmode == 1",
          br(),br(),
          fileInput("nbfile", label = h4("Neighbourhood mask")),
          h4("Additional file settings"),
          checkboxInput("nbhead", label = "File contains headers", value = FALSE),
          selectInput(
            "nbsep",
            label = h5("Column separator"),
            choices = list(
              "Space" = 1,
              "Tab" = 2,
              "Semicolon" = 3,
              "Comma" = 4
            ),
            selected = 1
          )
        ),
        conditionalPanel(
          "input.nbmode == 2",
          h4("Generate neighbourhood mask"),
          radioButtons(
            inputId = "equidist",
            label = h5("Band width"),
            choiceNames = list(
              HTML("Equal-size bands"),
              HTML("Different band widths")
            ),
            choiceValues = list(1,2),
            selected = 1,
            inline = TRUE
          ),
          sliderInput("nbbands",label = h5("Number of bands"), value = 2, min = 1, max = 9, step = 1),
          checkboxInput("sepfocus", label = "Separate group for focal cell", value = TRUE),
          conditionalPanel(
            "input.equidist==1",
            uiOutput("radcontrol")
          ),
          uiOutput("nbcontrols")
        ),
        conditionalPanel(
          "input.nbmode == 3",
          h4("Generate neighbourhood mask"),
          sliderInput("gradradius",label = h5("Radius (number of cells)"), value = 10, min = 1, max = 100, step = 1)
        ),
        conditionalPanel(
          "input.nbmode == 4",
          h4("Generate neighbourhood mask"),
          radioButtons(
            inputId = "wedgenr",
            label = h5("Number of wedges"),
            choiceNames = list(
              HTML("4 wedges (orientation 1)"),
              HTML("4 wedges (orientation 2)"),
              HTML("8 wedges")
            ),
            choiceValues = list(1,2,3),
            selected = 1
          ),
          sliderInput("wedgeradius",label = h5("Radius (number of cells)"), value = 10, min = 1, max = 100, step = 1)
        ),
        plotOutput("nbplot",height = "300px",width = "80%"),
        br(),
        actionButton("back1", strong(span("Back", style = "color:blue"))),
        actionButton("next3", strong(span("Continue", style = "color:blue"))),
        textOutput("tbk1"),
        textOutput("tnx3"),
        br(),br()
      )
    )
  ),
  
  tabPanel(
    title = "Expectation",
    value = "3",
    sidebarLayout(
      creds,
      mainPanel(
        conditionalPanel(
          "input.nbmode == 3",
          h3("Set parameters"),
          h5(em("The visual property values in the input viewshed(s) will be compared to the expected values you define here.")),
          br(),
          sliderInput("exprange",label = h5("Expectation range"), value = c(0,1), min = 0, max = 1, step = 0.01),
          radioButtons(
            inputId = "incdec",
            label = h4("Direction"),
            choices = list(
              "Increase with distance" = 1,
              "Decrease with distance" = 2
            ),
            selected = 1,
            inline = TRUE
          ),
          conditionalPanel(
            "input.vsmode == 2",
            radioButtons(
              inputId = "expvs",
              label = h4("Apply to viewshed:"),
              choices = list(
                "Viewshed 1" = 1,
                "Viewshed 2" = 2
              ),
              selected = 1,
              inline = TRUE
            )
          )
        ),
        conditionalPanel(
          "input.nbmode != 3",
          h3("Set expectation values"),
          h5(em("The visual property values in the input vieshed(s) will be compared to the expected values you define here.")),
          br(),
          br(),
          radioButtons(
            inputId = "expmode",
            label = h4("Expectation mode"),
            choices = list(
              "Analysis without expectation values" = 1,
              "Upload expectation table" = 2,
              "Enter expectation values manually" = 3
            ),
            selected = 3
          ),
          br(),
          conditionalPanel(
            "input.expmode == 1",
            em(span("- Without expectation values, no expectation-based analyses can be performed -",style = "color:blue")),
            br(),br()
          ),
          conditionalPanel(
            "input.expmode == 2",
            br(),br(),
            fileInput("expfile", label = h4("Expectation table")),
            h4("Additional file settings"),
            checkboxInput("exphead", label = "File contains headers", value = FALSE),
            selectInput(
              "expsep",
              label = h5("Column separator"),
              choices = list(
                "Space" = 1,
                "Tab" = 2,
                "Semicolon" = 3,
                "Comma" = 4
              ),
              selected = 1
            )
          ),
          uiOutput("expcontrols")
        ),
        conditionalPanel(
          "input.expmode!=1",
          plotOutput("expmatrix"),
          br()
        ),
        actionButton("back2", strong(span("Back", style = "color:blue"))),
        actionButton("next4", strong(span("Continue", style = "color:blue"))),
        textOutput("tbk2"),
        textOutput("tnx4"),
        br(),br()
      )
    )
  ),

  tabPanel(
    title = "Output",
    value = "4",
    sidebarLayout(
      creds,
      mainPanel(
        h3("Output settings"),
        br(),
        h4("Basic analysis"),
        em("These analyses do not take into account groups or expectation values, they are based 
           solely on the input data and the neighbourhood size. The output values are the values 
           of the selected visual property in the neighbourhood of each cell."),
        checkboxInput("avg", label = "Average (v_avg)", value = FALSE),
        checkboxInput("prom", label = "Prominence (v_prom)", value = FALSE),
        checkboxInput("min", label = "Minimum (v_min)", value = FALSE),
        checkboxInput("max", label = "Maximum (v_max)", value = FALSE),
        checkboxInput("range", label = "Range (v_range)", value = FALSE),
        br(),
        h4("Group-based analysis"),
        em("These analyses compare the different groups of the neighbourhood and return
           the group that contains the selected property value instead of the value itself."),
        checkboxInput("minavg", label = "Minimum average (g_minavg)", value = FALSE),
        checkboxInput("maxavg", label = "Maximum average (g_maxavg)", value = FALSE),
        checkboxInput("minval", label = "Minimum value (g_minval)", value = FALSE),
        checkboxInput("maxval", label = "Maximum value (g_maxval)", value = FALSE),
        checkboxInput("minrange", label = "Minimum range (g_minrange)", value = FALSE),
        checkboxInput("maxrange", label = "Maximum range (g_maxrange)", value = FALSE),
        br(),
        conditionalPanel(
          "input.expmode != 1",
          h4("Expectation-based analysis"),
          em("These analyses compare the input data to the specified expectation values.
             The output values indicate the error: a low value means the hypothesis fits well."),
          checkboxInput("genrmse", 
                        label = HTML("Global root-mean-square error (rmse_global) <br><em> each cell weighted equally </em>"), 
                        value = FALSE),
          checkboxInput("grouprmse", 
                        label = HTML("Grouped root-mean-square error (rmse_grouped) <br><em> each group weighted equally </em>"), 
                        value = FALSE)
        ),
        br(),
        h4("Edge effects"),
        em("Results for cells close to the edge are influenced by the fact that part of the neighbourhood
           falls outside of the study area. This option filters out the cells affected by this issue from 
           the output. If the filter is turned off, a value is computed for these cells based on the partial
           neighbourhood that is within the study area."),
        checkboxInput("edgefilter", label = "Remove edge effects", value = TRUE),
        br(),
        textInput("outdir",h4("Output directory"),value="./output/"),
        textOutput("dircheck"),
        checkboxInput("plotoutput", label = "Save a plot of the output in addition to the ASCII table", value = TRUE),
        br(),
        br(),
        actionButton("back3", strong(span("Back", style = "color:blue"))),
        actionButton("run", strong(span("Start analysis", style = "color:blue"))),
        br(),
        em("Depending on input size, neighbourhood size, and selected options, 
           the analysis can take up to several hours. To cancel a running analysis, 
           press the X at the top right corner of the window."),
        textOutput("tbk3"),
        textOutput("t"),
        br(),br()
      )
    )

  )
  
  
)
