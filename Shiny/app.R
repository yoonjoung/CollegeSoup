suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(tidycensus)))
suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(plyr)))
suppressWarnings(suppressMessages(library(readxl)))

suppressWarnings(suppressMessages(library(plotly)))
suppressWarnings(suppressMessages(library(RColorBrewer))) 

suppressWarnings(suppressMessages(library(DT))) 

date<-as.Date(Sys.time(	), format='%d%b%Y')

# This creates shiny app to display census and ACS data for Maryland 
# There are four parts in this document:
# 0. Database update 
# 1. USER INTERFACE 
# 2. SERVER
# 3. CREATE APP     

##### 0. Database update #####

#******************************
# 0. Database update 
#******************************

##### 0.1 load datasets #####

# Load and mutate ----
# Datasets are prepared using CDS_DataPrepForShiny.Rmd

#setwd("~/Dropbox/0Kids/CDS/Shiny/")
dta <- read_excel("college_data_shiny.xlsx", col_names = TRUE)%>%
    filter(year>=2017)

dtaall_latest <- read_excel("college_data_shiny_all_latest.xlsx", col_names = TRUE)

##### 0.2 Define input list #####

collegegrouplist<-as.vector(c("All", 
                              "Tiny (undergraduate <2000)", 
                              "Small (undergraduate 2K-5K)", 
                              "Medium (undergraduate 5K-10K)", 
                              "Large (undergraduate 10K-30K)", 
                              "Huge (undergraduate 30K+)", 
                              "Reach (Acceptance rate below 20%)",   
                              "Target (Acceptance rate 20% - 50%)",
                              "Safety (Acceptance rate 50% and more)", 
                              "Open curriculum colleges",
                              "Public universities",
                              "Women's colleges"))

grouplist<-as.vector(c("All", 
                              "Group by size", 
                              "Group by acceptance rate", 
                              "Group by other characteristics"))

grouplist_size<-as.vector(c("Tiny (undergraduate <2000)", 
                              "Small (undergraduate 2K-5K)", 
                              "Medium (undergraduate 5K-10K)", 
                              "Large (undergraduate 10K-30K)", 
                              "Huge (undergraduate 30K+)"))

grouplist_admit<-as.vector(c("Below 20%",   
                             "20% - 50%",
                             "50% and more"))

grouplist_other<-as.vector(c("Open curriculum colleges",
                            "Public universities",
                            "Women's colleges"))

collegelist_all<-as.vector(sort(unique(dta$college)))

temp<-dta%>%filter(tiny==1)
collegelist_tiny<-as.vector(sort(unique(temp$college)))
temp<-dta%>%filter(small==1)
collegelist_small<-as.vector(sort(unique(temp$college)))
temp<-dta%>%filter(medium==1)
collegelist_medium<-as.vector(sort(unique(temp$college)))
temp<-dta%>%filter(large==1)
collegelist_large<-as.vector(sort(unique(temp$college)))
temp<-dta%>%filter(huge==1)
collegelist_huge<-as.vector(sort(unique(temp$college)))

temp<-dta%>%filter(pct_admit_reach==1)
collegelist_reach<-as.vector(sort(unique(temp$college)))
temp<-dta%>%filter(pct_admit_target==1)
collegelist_target<-as.vector(sort(unique(temp$college)))
temp<-dta%>%filter(pct_admit_safety==1)
collegelist_safety<-as.vector(sort(unique(temp$college)))

temp<-dta%>%filter(open==1)
collegelist_open<-as.vector(sort(unique(temp$college)))
temp<-dta%>%filter(public==1)
collegelist_public<-as.vector(sort(unique(temp$college)))
temp<-dta%>%filter(women==1)
collegelist_women<-as.vector(sort(unique(temp$college)))

##### 0.3 Define color list #####
greycolors <- brewer.pal(7,"Greys")
bluecolors <- brewer.pal(7,"Blues")
greencolors <- brewer.pal(7,"Greens")
orangecolors <- brewer.pal(7,"Oranges")
redcolors <- brewer.pal(7,"Reds")
purplecolors <- brewer.pal(7,"Purples")
divcolors<-brewer.pal(7,"RdYlBu")
qualcolors<-brewer.pal(7,"Set3")

##### 0.4 Define functions for plot #####
hline <- function(y = 0, color = "#CCCCCC") {
    list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = y,
        y1 = y,
        line = list(color = color)
    )
}

vline <- function(x = 0, color = "#CCCCCC") {
    list(
        type = "line",
        x0 = x,
        x1 = x,
        y0 = 0,
        y1 = 1,
        yref = "paper",
        line = list(color = color)
    )
}

##### 1. USER INTERFACE #####

#******************************
# 1. USER INTERFACE 
#******************************

ui<-fluidPage(
    
    # Header panel 
    headerPanel("College Soup: information x insight"),
    #headerPanel("Data from American Community Survey and decennial censuses"),
    
    # Title panel 
    titlePanel(""),
    p("View on a monitor for best experience, and hover over the figures to explore further."),   
    p("Unless noted, vertical line is for 2020-2021 academic year."),  
    # Main page for output display 
    mainPanel(
        width = 12,

        tabsetPanel(id="demo", 
                    type = "tabs",
       
                    # TAB 1: financial aid ####
                    tabPanel("1. Financial aid overview",
                            # Top row ####
                            fluidRow(
                                selectInput("collegegroup_tab1", 
                                             "Select a group of colleges",
                                             choices = collegegrouplist), 
                                p("Note: It includes select colleges of interest.",
                                  actionLink("link_tab1_annex", "See Annex"),
                                  "for the list of colleges in each group.")
                            ),                               
                            # Rows ####
                            fluidRow(
                                h4("1.1. Need-based (NB) assistance", style = "color:blue"),   
                                p("NB assistance includes:", 
                                  "(1) NB scholarship or grant aid,", 
                                  "(2) NB self-help aid, and", 
                                  "(3) NNB scholarship or grant aid awarded to those who have need."
                                  , style = "color:blue"),
                                p("It exclude PLUS loans, unsubsidized loans, and private alternative loans.", style = "color:blue"), 
                                
                                column(4,
                                       h5("Percent of freshmen who receive any NBA (%)"),    
                                       plotlyOutput("plot_pct_nba",
                                                    width = 400, height = 600)
                                ),
                                column(4,
                                       h5("Percent of freshmen who receive grant/scholarship NB assistance (%)"),    
                                       plotlyOutput("plot_pct_nba_grant",
                                                    width = 400, height = 600)
                                )
                                                                 
                            ),   
                            fluidRow(
                                column(4,
                                       h5("Average amount of any NBA, among those who received it"),    
                                       plotlyOutput("plot_amount_nba",
                                                    width = 400, height = 600)
                                ),
                                column(4,
                                       h5("Average amount of grant/scholarship NB assistance, among those who received it"),    
                                       plotlyOutput("plot_amount_nba_grant",
                                                    width = 400, height = 600)
                                ),  
                                column(4,
                                       h5("Percent of need amount met, among those who received any NB assistance"),    
                                       plotlyOutput("plot_pct_neetmet",
                                                    width = 400, height = 600)
                                )
                            ),   
                            fluidRow(
                                h4("1.2. Non-need-based (NNB) assistance", style = "color:blue"),                           
                                p("NNB assistance refers to ", 
                                  "institutional non-need-based scholarship or grant aid given to students in line who have no financial need.", 
                                  "It excludes assistance given to those who were awarded athletic awards and tuition benefits." 
                                  , style = "color:blue"), 
                                #p("- Institutional non-need-based athletic scholarship or grant"), 
                                column(4,
                                       h5("Percent of freshmen who receive NNB assistance (%)"),    
                                       plotlyOutput("plot_pct_nonnba",
                                                    width = 400, height = 600) 
                                       
                                ),
                                column(4,
                                       h5("Average amount of NNB assistance, among those who received it"),    
                                       plotlyOutput("plot_amount_nonnba",
                                                    width = 400, height = 500),
                                       h6("Note"), 
                                       h6("1. Only including colleges where 5% or higher students received NNB assistance")
                                )
                            ),   
                            h6(strong("DEFINITIONS IN FINANCIAL AID")), 
                            h6(strong("- Financial aid applicant:"), "Any applicant who submits any one of the institutionally required financial aid applications/forms, such as the FAFSA."),
                            h6(strong("- Institutional scholarships and grants:"), "Endowed scholarships, annual gifts and tuition funded grants for which the institution determines the recipient."),
                            h6(strong("- Financial need:"), "As determined by your institution using the federal methodology and/or your institution's own standards."),
                            h6(strong("- Need-based aid:"), "College-funded or college-administered award from institutional, state, federal, or other sources for which a student must have financial need to qualify. This includes both institutional and non-institutional student aid (grants, jobs, and loans)."),
                            h6(strong("- Need-based scholarship or grant aid:"), "Scholarships and grants from institutional, state, federal, or other sources for which a student must have financial need to qualify."),
                            h6(strong("- Need-based self-help aid:"), "Loans and jobs from institutional, state, federal, or other sources for which a student must demonstrate financial need to qualify."),
                            h6(strong("- Non-need-based scholarship or grant aid:"), "Scholarships and grants, gifts, or merit-based aid from institutional, state, federal, or other sources (including unrestricted funds or gifts and endowment income) awarded solely on the basis of academic achievement, merit, or any other non-need-based reason. When reporting questions H1 and H2, non-need-based aid that is used to meet need should be counted as need-based aid."),
                            h6(strong("- Non-need-based self-help aid:"), "Loans and jobs from institutional, state, or other sources for which a student need not demonstrate financial need to qualify."),
                            h6(strong("- Private student loans:"), "A nonfederal loan made by a lender such as a bank, credit union or private lender used to pay for up to the annual cost of education, less any financial aid received."),
                            h6(strong("- External scholarships and grants:"), "Scholarships and grants received from outside (private) sources that students bring with them (e.g., Kiwanis, National Merit scholarships). The institution may process paperwork to receive the dollars, but it has no role in determining the recipient or the dollar amount awarded."),
                            h6(strong("- Work study and employment:"), "Federal and state work study aid, and any employment packaged by your institution in financial aid awards."),
                            
                            # End-note ####
                            hr(), 
                            h6("Application was created on October 20, 2023, and updated most recently on December 20, 2023."),
                            h6("See Annex for data sources and note."),
                            h6("For typos, errors, and questions:", 
                               a("contact YJ Choi at www.iSquared.global", href="https://www.isquared.global/YJ")), 
                            
                            hr() 
                    ),
                    # TAB 2: admission criteria ####
                    tabPanel("2. Admission criteria overview",
                             # Top row ####
                             fluidRow(
                                 selectInput("collegegroup_tab2", 
                                             "Select a group of colleges",
                                             choices = collegegrouplist), 
                                 p("Note: It includes select colleges of interest.",
                                   actionLink("link_tab2_annex", "See Annex"),
                                   "for the list of colleges in each group.")
                             ),  
                             fluidRow(
                                 h4("Admission criteria", style = "color:blue"),    
                                 p("Relative importance of academic and nonacademic factors in their", 
                                   "first-time, firstyear, degree-seeking general admissions decisions", 
                                   "(not including programs with specific criteria)."
                                   , style = "color:blue"),
                                 column(4,
                                        h5("Academic Criteria"),     
                                        plotlyOutput("plot_aca",
                                                     width = 400, height = 600),
                                        h6(strong("LEGEND")), 
                                        h6("Very important", style = "color:#006837"),  
                                        h6("Important", style = "color:#66bd63"),  
                                        h6("Considered (or considered if submitted)?", style = "color:#d9ef8b"),  
                                        h6("Not considered", style = "color:#fee08b"), 
                                        h6(""), 
                                        h6(strong("DESCRIPTION")), 
                                        h6("- essay: Application Essay"),
                                        h6("- gpa: Academic GPA"),
                                        h6("- rank: Class rank"),
                                        h6("- recommendation: Recommendation"),
                                        h6("- rigor: Rigor of secondary school record"),
                                        h6("- sts: Standardized test scores")
                                 ),
                                 column(8,
                                        h5("Non-academic criteria"),    
                                        plotlyOutput("plot_acna",
                                                     width = 800, height = 600),
                                        h6(strong("LEGEND")), 
                                        h6("Very important", style = "color:#006837"),  
                                        h6("Important", style = "color:#66bd63"),  
                                        h6("Considered (or considered if submitted)?", style = "color:#d9ef8b"),  
                                        h6("Not considered", style = "color:#fee08b"),
                                        h6(""), 
                                        h6(strong("DESCRIPTION")), 
                                        h6("- alum.relation: Alumni/ae relation"),
                                        h6("- character: Character/personal qualities"),
                                        h6("- extra.cur.activities: Extracurricular activities"),
                                        h6("- geo.residence: Geographical residence"),
                                        h6("- interest: Level of applicant’s interest"), 
                                        h6("- interview: Interview"),
                                        h6("- religious.aff: Religious affiliation/commitment"),
                                        h6("- state.residence: State residency"),
                                        h6("- talent: Talent/ability"),
                                        h6("- volunteer.work: Volunteer work"),
                                        h6("- work: Work experience"),
                                        h6("- first.gen: First generation")
                                 )
                            )
                    ),          
                    # TAB 3: admission statistics ####
                    tabPanel("3. Admission statistics overview",
                             # Top row ####
                             fluidRow(
                                 selectInput("collegegroup_tab3", 
                                             "Select a group of colleges", 
                                             choices = collegegrouplist), 
                                 p("Note: It includes select colleges of interest.",
                                   actionLink("link_tab3_annex", "See Annex"),
                                   "for the list of colleges in each group.")
                                 ),
                             # Row 1-2 ####
                             fluidRow(   
                                 h4("How has admission changed?", style = "color:blue"),   
                                 p("Is admission rate really decreasing? Is class size decreasing? Are there more applications?", style = "color:blue"),   
                                   
                                 column(4,
                                        h5("Admission rates over time (%)"),   
                                        plotlyOutput("plot_admit",
                                                     width = 400, height = 400)
                                 ),
                                 column(4,
                                        h5("Admission rates among ED over time (%)"),   
                                        plotlyOutput("plot_admit_ed",
                                                     width = 400, height = 400)
                                 ),
                                 column(4,
                                        h5("Percent of enrolled students who submitted an early decision application"),    
                                        plotlyOutput("plot_ed_among_enroll",
                                                     width = 400, height = 400)
                                 )
                             ),
                             fluidRow(
                                 column(4,
                                        h5("Relative number of applications over time, compared to a reference in the latest year"),    
                                        plotlyOutput("plot_relnumapp_total",
                                                     width = 400, height = 400)
                                 ), 
                                 column(4,
                                        h5("Relative number of addmitted over time, compared to a reference in the latest year"),    
                                        plotlyOutput("plot_relnumadmit_total",
                                                     width = 400, height = 400)
                                 )
                             ), 
                             # Row 3 ####
                             fluidRow(
                                 h4("Are admitted students more academically qualified than before?", style = "color:blue"), 
                                   
                                 column(4,
                                        h5("Percent of enrolled students who submitted SAT scores over time"),   
                                        plotlyOutput("plot_satsubmit",
                                                     width = 400, height = 400)
                                 ),                                 
                                 column(4,
                                        h5("25th percentile SAT composite score over time, among those enrolled and submitted SAT scores"),    
                                        plotlyOutput("plot_satscore25th",
                                                     width = 400, height = 400)
                                 ),
                                 column(4,
                                        h5("75th percentile SAT composite score over time, among those enrolled and submitted SAT scores"),    
                                        plotlyOutput("plot_satscore75th",
                                                     width = 400, height = 400)
                                 )
                             ),
                             # Row 4 ####
                             fluidRow(
                                   
                                 column(4,
                                        h5("Percent of enrolled students who were in top 10th percentile in their class over time"),    
                                        plotlyOutput("plot_top10th",
                                                     width = 400, height = 400)
                                 ) 
                             ),   
                             # Row 5 ####
                             fluidRow(
                                 h4("What is relationship between acceptance rates among all vs. ED?", style = "color:blue"),                                  
                                 column(4,
                                        h5("Association between yield rate and acceptance rate"),    
                                        plotlyOutput("plot_admit_all_ed",
                                                     width = 400, height = 400)
                                 )
                             ),
                             # Row 6 ####
                             fluidRow(
                                 h4("How do yield and retention rates correlated with acceptance rate? And, did retention rate bounce back after 2020?", style = "color:blue"),                                  
                                 column(4,
                                        h5("Association between yield rate and acceptance rate"),    
                                        plotlyOutput("plot_yield_admit",
                                                     width = 400, height = 400)
                                 ),
                                 column(4,
                                        h5("Association between retention rate and acceptance rate"),    
                                        plotlyOutput("plot_retention_admit",
                                                     width = 400, height = 400)
                                 ),
                                 column(4,
                                        h5("Retention rate over time"),    
                                        plotlyOutput("plot_retention_trend",
                                                     width = 400, height = 400)
                                 )
                             )
                    ),
                    # TAB 4: Student population and life ####
                    tabPanel("4. Student population and life overview",
                             # Top row ####
                             fluidRow(
                                 selectInput("collegegroup_tab4", 
                                            "Select a group of colleges", 
                                            choices = collegegrouplist), 
                                 p("Note: It includes select colleges of interest.",
                                   actionLink("link_tab4_annex", "See Annex"),
                                   "for the list of colleges in each group.") 
                             ),    
                               
                             # gender ####
                             fluidRow(
                                 h4("In terms of gender...", style = "color:blue"),  
                                 column(4,
                                        h5("Number of male students per 100 female students (full-time undergraduate students)"),    
                                        plotlyOutput("plot_ratiogender",
                                                     width = 400, height = 400)
                                 )
                             ),
                             # Residency ####
                             fluidRow(
                                 h4("In terms of residency...", style = "color:blue"),                  
                                 column(4,
                                        h5("Percent of US undergraduates who are out-of-state (%), latest year"),    
                                        plotlyOutput("plot_pct_us_oos_rank",
                                                     width = 400, height = 400)
                                 ),
                                 column(4,
                                        h5("Percent of US undergraduates who are out-of-state (%), trends"),    
                                        plotlyOutput("plot_pct_us_oos",
                                                     width = 400, height = 400)
                                 )
                             ),
                             fluidRow(
                                 column(4,
                                        h5("Percent of undergraduate students who are non-resident alien (%), latest year"),     
                                        plotlyOutput("plot_pct_under_nra_rank",
                                                     width = 400, height = 400)
                                 ),
                                 column(4,
                                        h5("Percent of undergraduate students who are non-resident alien (%), trends"),    
                                        plotlyOutput("plot_pct_under_nra",
                                                     width = 400, height = 400)
                                 )
                             ),                              
                             # Race ####
                             fluidRow(
                                 h4("In terms of race and ethnicity...", style = "color:blue"),  
                                 column(4,
                                        h5("Percent of undergraduate students who are Asian (%)"),    
                                        plotlyOutput("plot_pct_under_asian",
                                                     width = 400, height = 400)
                                 )                                 

                             ), 
                             
                             # Transfer ####
                             fluidRow(
                                 h4("How common are transfer students?", style = "color:blue"),
                                 column(4,
                                        h5("Ratio of transfer vs. freshmen enrollment, latest year"),    
                                        plotlyOutput("plot_ratio_admit_trans_rank",
                                                     width = 400, height = 400)
                                 ),
                                 column(4,
                                        h5("Ratio of transfer vs. freshmen enrollment,  trends"),    
                                        plotlyOutput("plot_ratio_admit_trans",
                                                     width = 400, height = 400)
                                 )
                             ),
                             fluidRow(
                                 h4("What are transfer trends, and would it affect admission?", style = "color:blue"), 
                                 p("Including only colleges where transfer students account for 5% or more of the undergraduate students.", style = "color:red"),  
                                 column(4,
                                        h5("Relative number of transfer applications over time, compared to a reference in the latest year"),    
                                        plotlyOutput("plot_relnumtransapp_total",
                                                     width = 400, height = 400)
                                 ),
                                 column(4,
                                        h5("Relative number of transfer admitted over time, compared to a reference in the latest year"),    
                                        plotlyOutput("plot_relnumtransadmit_total",
                                                     width = 400, height = 400)
                                 ),
                                 column(4,
                                        h5("Relative number of transfer enrolled over time, compared to a reference in the latest year"),    
                                        plotlyOutput("plot_relnumtransenroll_total",
                                                     width = 400, height = 400)
                                 )
                             ), 
                             
                             # Housing ####
                             fluidRow(
                                 h4("In terms of housing ...", style = "color:blue"),  
                                 column(4,
                                        h5("Percent of freshmen who live in university-affiliated housing(%), latest year"),    
                                        plotlyOutput("plot_pct_housing_freshmen_rank",
                                                     width = 400, height = 400)
                                 ),
                                 column(4,
                                        h5("Percent of freshmen who live in university-affiliated housing(%), trends"),    
                                        plotlyOutput("plot_pct_housing_freshmen",
                                                     width = 400, height = 400)
                                 ),
                             ),
                             fluidRow(
                                 column(4,
                                        h5("Percent of overall undergraduates live in university-affiliated housing(%), latest year"),    
                                        plotlyOutput("plot_pct_housing_rank",
                                                     width = 400, height = 400)
                                 ),
                                 column(4,
                                        h5("Percent of undergraduates who live in university-affiliated housing(%), trends"),    
                                        plotlyOutput("plot_pct_housing",
                                                     width = 400, height = 400)
                                 )
                             ),
                             
                             # Greek life ####
                             fluidRow(
                                 h4("In terms of Greek life ...", style = "color:blue"),  
                                 column(4,
                                        h5("Percent of undergraduate who are in fraternities (%), latest year"),    
                                        plotlyOutput("plot_pct_frat_rank",
                                                     width = 400, height = 400)
                                 ),
                                 column(4,
                                        h5("Percent of undergraduate who are in fraternities (%), trends"),  
                                        plotlyOutput("plot_pct_frat",
                                                     width = 400, height = 400)
                                 ),
                             ),
                             fluidRow(
                                 column(4,
                                        h5("Percent of undergraduate who are in sororities (%), latest year"),    
                                        plotlyOutput("plot_pct_soro_rank",
                                                     width = 400, height = 400)
                                 ),
                                 column(4,
                                        h5("Percent of undergraduate who are in sororities (%), trends"),  
                                        plotlyOutput("plot_pct_soro",
                                                     width = 400, height = 400)
                                 ),
                             )
                    ), 
                    # TAB 5: In-depth insight by college####
                    tabPanel("5. In-depth insight by college",                             
                             selectInput("collegegroup_tab5", 
                                         "Select a group of colleges", 
                                         choices = collegegrouplist), 
                             selectInput("college_tab5", 
                                         "Select a college", 
                                         choices = NULL), 
                             # Programs offered ####
                             h4("Does the school offer specific programs of interest?", style = "color:blue"),      
                             p("Data from the latest year"),
                             #textOutput("text_offer"),
                             #verbatimTextOutput("text_offer1"), 
                             tableOutput("table_offer"),
                             # Financial aid ####
                             h4("Does the school offer good financial assistance?", style = "color:blue"),
                             p("Data from the latest year"),
                             fluidRow(
                                 column(6,
                                        h5("Percent of enrolled freshmen who applied for NB assistance, determined to have need, and received NB assistance (%)"),    
                                        plotlyOutput("cplot_pct_assistance",
                                                     width = 600, height = 400)
                                 ),                                 
                                 column(6,
                                        h5("Amount of financial assistance, among those who received it: NB assistance and non-athletic NNB assistance"),    
                                        plotlyOutput("cplot_amount_assistance",
                                                     width = 600, height = 400)
                                 )
                             ), 

                             # Admission - criteria  ####
                             h4("What are admission criteria?", style = "color:blue"),
                             p("Relative importance of academic and nonacademic factors in their", 
                               "first-time, firstyear, degree-seeking general admissions decisions", 
                               "(not including programs with specific criteria)."),
                             fluidRow(
                                 column(4,
                                        h5("Academic Criteria"),     
                                        plotlyOutput("cplot_aca",
                                                     width = 400, height = 100),
                                        h6("LEGEND"), 
                                        h6("Very important", style = "color:#006837"),  
                                        h6("Important", style = "color:#66bd63"),  
                                        h6("Considered (or considered if submitted)?", style = "color:#d9ef8b"),  
                                        h6("Not considered", style = "color:#fee08b"), 
                                        h6(""), 
                                        h6(strong("DESCRIPTION")), 
                                        h6("- essay: Application Essay"),
                                        h6("- gpa: Academic GPA"),
                                        h6("- rank: Class rank"),
                                        h6("- recommendation: Recommendation"),
                                        h6("- rigor: Rigor of secondary school record"),
                                        h6("- sts: Standardized test scores")
                                 ),
                                 column(8,
                                        h5("Non-academic criteria"),    
                                        plotlyOutput("cplot_acna",
                                                     width = 800, height = 100),
                                        h6("LEGEND"), 
                                        h6("Very important", style = "color:#006837"),  
                                        h6("Important", style = "color:#66bd63"),  
                                        h6("Considered (or considered if submitted)?", style = "color:#d9ef8b"),  
                                        h6("Not considered", style = "color:#fee08b"),
                                        h6(""), 
                                        h6(strong("DESCRIPTION")), 
                                        h6("- alum.relation: Alumni/ae relation"),
                                        h6("- character: Character/personal qualities"),
                                        h6("- extra.cur.activities: Extracurricular activities"),
                                        h6("- geo.residence: Geographical residence"),
                                        h6("- interest: Level of applicant’s interest"), 
                                        h6("- interview: Interview"),
                                        h6("- religious.aff: Religious affiliation/commitment"),
                                        h6("- state.residence: State residency"),
                                        h6("- talent: Talent/ability"),
                                        h6("- volunteer.work: Volunteer work"),
                                        h6("- work: Work experience"),
                                        h6("- first.gen: First generation")
                                 )
                             ),
                             
                             # Admission - overview  ####
                             h4("What are admission trends?", style = "color:blue"),
                               
                             fluidRow(
                                 column(4,
                                        h5("Admission rates over time by group (%)"),    
                                        plotlyOutput("cplot_admit_trend_detail",
                                                     width = 400, height = 400)
                                 ),                                 
                                 column(4,
                                        h5("Percent of enrolled students who submitted an early decision application (%)"),    
                                        plotlyOutput("cplot_ed_among_enroll",
                                                     width = 400, height = 400)
                                 ),
                                 column(4,
                                        h5("Relative changes in the number of applications, admitted, and enrolled"),    
                                        plotlyOutput("cplot_relnum_trend",
                                                     width = 400, height = 400)
                                 )
                             ), 
                             # Admission - criteria and academic qualification ####
                             h4("What are academic characteristics among those enrolled?", style = "color:blue"),
                               
                             fluidRow(
                                 column(4,
                                        h5("Percent of who submitted SAT scores (%)"),    
                                        plotlyOutput("cplot_satsubmit",
                                                     width = 400, height = 400),
                                        h6("Note: Lighter colors are for years when standardized test scores were ", 
                                          strong("neither"), strong(em("important for admission decision")),
                                          strong("nor"), strong(em("required."))) 
                                 ),
                                 column(4,
                                        h5("SAT composite scores (%)"),    
                                        plotlyOutput("cplot_satscores",
                                                     width = 400, height = 400),
                                        h6("Note: Lighter colors are for years when standardized test scores were ", 
                                           strong("neither"), strong(em("important for admission decision")),
                                           strong("nor"), strong(em("required."))) 
                                 ),
                                 column(4,
                                        h5("Percent who were within top 10th percentile (%)"),    
                                        plotlyOutput("cplot_top10th",
                                                     width = 400, height = 400)
                                 )
                             ), 
                             
                             # Student population ####
                             h4("How does the student population look like?", style = "color:blue"),
                               
                             fluidRow(
                                 column(4,
                                        h5("Percent of undergraduate students who are Asian or non-resident alien (%)"),    
                                        plotlyOutput("cplot_pct_under_asian_nra",
                                                     width = 400, height = 400)
                                 ),                                 
                                 column(4,
                                        h5("Percent of out-of-state freshmen among US freshmen (%)"),    
                                        plotlyOutput("cplot_pct_us_oos",
                                                     width = 400, height = 400)
                                 ),
                                 column(4,
                                        h5("Number of male students per 100 female students (full-time undergraduate students)"),    
                                        plotlyOutput("cplot_ratio_gender",
                                                     width = 400, height = 400)
                                 )
                             ),
                             fluidRow(
                                 column(4,
                                        h5("Number of students: undergraduate vs. graduate"),    
                                        plotlyOutput("cplot_num_student",
                                                     width = 400, height = 400)
                                 )
                             ),
                             # Transfer ####
                             h4("How about trends in transfer?", style = "color:blue"),
                             p("Note: Data are presented only for colleges where transfer students account for 5% or more of the undergraduate students.", style = "color:red"),  
                             
                             fluidRow(
                                 column(4,
                                        h5("Number of transfer applications"),    
                                        plotlyOutput("cplot_numtransapp_total",
                                                     width = 400, height = 400)
                                 ),                                 
                                 column(4,
                                        h5("Number of transfer admitted"),    
                                        plotlyOutput("cplot_numtransadmit_total",
                                                     width = 400, height = 400)
                                 ),
                                 column(4,
                                        h5("Number of transfer students enrolled"),    
                                        plotlyOutput("cplot_numtransenroll_total",
                                                     width = 400, height = 400)
                                 )
                             )
                    ),         
                    # Annex ####
                    tabPanel("Annex",           
                             
                             p(strong("A. Data sources:")),
                             p("1.",
                               a("Common Data Set",
                                 href="file:///Users/yoonjoungchoi/Downloads/CDS_2022-2023.pdf"),
                               "annual submission by each college: focusing on 2017-2023"),
                             p("2.",
                               a("College Scorecard",
                                 href="https://collegescorecard.ed.gov/data/")),
                             
                             hr(), 
                             p(strong("B. List of colleges/universities included")),
                             h6("School-size categorization is based on the total number of full-time undergraduate students and follows:"),
                             
                             #tiny = latest_num_under < 2000,
                             #small = latest_num_under >= 2000 & latest_num_under < 5000,
                             #medium = latest_num_under >= 5000 & latest_num_under<10000,
                             #large = latest_num_under >= 10000 & latest_num_under<30000,
                             #huge = latest_num_under >= 30000, 
                             
                             h6("- Tiny: < 2K"),
                             h6("- Small: 2K-5K"),
                             h6("- Medium: 5K-10K"),
                             h6("- Large: 10K-30K"),
                             h6("- Huge: 30K+"),
                             h6(""),
                             #tableOutput("table_list"),
                             dataTableOutput("table_list_simple"), 
                             
                             hr(), 
                             h5("For typos, errors, and questions:", 
                                a("contact YJ Choi at www.iSquared.global", href="https://www.isquared.global/YJ")), 
                             hr()
                    )
                    # End of user interface ####
        )
    )
)

##### 2. SERVER #####

#******************************
# 2. SERVER
#******************************

server<-function(input, output, session) {

    observeEvent(input$link_tab1_annex, {
        updateTabsetPanel(session, "demo", "Annex")
    })
    
    observeEvent(input$link_tab2_annex, {
        updateTabsetPanel(session, "demo", "Annex")
    })
    
    observeEvent(input$link_tab3_annex, {
        updateTabsetPanel(session, "demo", "Annex")
    })
    
    observeEvent(input$link_tab4_annex, {
        updateTabsetPanel(session, "demo", "Annex")
    })    

    ##### output: Tab 1 ####

    output$plot_pct_nba <- renderPlotly({
        
        dtafig<-dtaall_latest%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab1))),
                         group))
        
        averagetuition<-mean(dtafig$tuition, na.rm = TRUE)
        
        dtafig%>%
            plot_ly(
                y = ~college,
                type="bar", 
                x = ~pct_nba_received, 
                name="Received any NBA",
                text = ~pct_nba_received, textposition = 'outside',
                line = list(color = bluecolors[5]), marker = list(color = bluecolors[5]),
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>% received any NBA: ", dtafig$pct_nba_received, "%"))%>%    
            layout(
                title = "", 
                xaxis = list(title = "Percent enrolled students",  
                             range=c(0, 100), 
                             showgrid = FALSE, showticklabels = TRUE),
                yaxis = list(title = "",
                             tickfont = list(size=9),
                             categoryorder = "total descending"),
                showlegend = FALSE
            )
    })
    
    output$plot_amount_nba <- renderPlotly({
        
        dtafig<-dtaall_latest%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab1))),
                         group))
        
        averagetuition<-mean(dtafig$tuition, na.rm = TRUE)    
        
        dtafig%>%
            plot_ly(
                y = ~college,
                type="bar", 
                x = ~amount_nba,
                text = ~amount_nba, textposition = 'outside',
                marker = list(color = bluecolors[3]),
                hovertemplate = paste0(
                    dtafig$college,
                    '<br>Percent who received of any NBA: ', dtafig$pct_nba_received, "%",
                    '<br>Average amount of any NBA: $', dtafig$amount_nba,
                    '<br>Tuition: $', dtafig$tuition))%>%
            layout(
                title = "", 
                xaxis = list(title = "Average any NBA ($)", range=c(0, 80000),  
                             showgrid = FALSE, showticklabels = TRUE),
                yaxis = list(title = "", 
                             tickfont = list(size=9),
                             categoryorder = "total descending"),
                #shapes = list(vline(averagetuition)),
                showlegend = FALSE
            )
    })

    output$plot_pct_nba_grant <- renderPlotly({
        
        dtafig<-dtaall_latest%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab1))),
                         group))
        
        averagetuition<-mean(dtafig$tuition, na.rm = TRUE)
        
        dtafig%>%
            plot_ly(
                y = ~college,
                type="bar", 
                x = ~pct_nba_received_grant, 
                text = ~pct_nba_received_grant,  textposition="outside",
                name="Received NB grant/scholarship",
                line = list(color = bluecolors[5]), marker = list(color = bluecolors[5]),
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>% received grant/scholarship NBA: ", dtafig$pct_nba_received_grant, "%"))%>%    
            layout(
                title = "", 
                xaxis = list(title = "Percent enrolled students",  
                             range=c(0, 100), 
                             showgrid = FALSE, showticklabels = TRUE),
                yaxis = list(title = "",
                             tickfont = list(size=9),
                             categoryorder = "total descending"),
                showlegend = FALSE
            )
    })
    
    output$plot_amount_nba_grant <- renderPlotly({
        
        dtafig<-dtaall_latest%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab1))),
                         group))
        
        averagetuition<-mean(dtafig$tuition, na.rm = TRUE)    
        
        dtafig%>%
            plot_ly(
                y = ~college,
                type="bar", 
                x = ~amount_nba_grant,
                text = ~amount_nba_grant, textposition = 'outside',
                marker = list(color = bluecolors[3]),
                hovertemplate = paste0(
                    dtafig$college,
                    '<br>Percent who received of scholarship/grant NBA: ', dtafig$pct_nba_received, "%",
                    '<br>Average amount of scholarship/grant NBA: $', dtafig$amount_nba,
                    '<br>Tuition: $', dtafig$tuition))%>%
            layout(
                title = "", 
                xaxis = list(title = "Average scholarship/grant NBA ($)", range=c(0, 80000),  
                             showgrid = FALSE, showticklabels = TRUE),
                yaxis = list(title = "", 
                             tickfont = list(size=9),
                             categoryorder = "total descending"),
                #shapes = list(vline(averagetuition)),
                showlegend = FALSE
            )
    })    

    output$plot_pct_neetmet <- renderPlotly({    
        
        dtafig<-dtaall_latest%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab1))),
                         group))
        
        dtafig%>%
            plot_ly(
                y = ~college,
                type="bar", 
                x = ~pct_needmet,
                text = ~pct_needmet, textposition = 'auto',
                line = list(color = greencolors[3]), marker = list(color = greencolors[3]),
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>% of need amount met: ", dtafig$pct_needmet, "%"))%>%  
            layout(
                title = "", 
                xaxis = list(title = "Percent of need amount met among those determined to have need",  
                             range=c(0, 100), 
                             showgrid = FALSE, showticklabels = TRUE),
                yaxis = list(title = "",
                             tickfont = list(size=9),
                             categoryorder = "total descending"),
                showlegend = FALSE
            )
    })    

    output$plot_pct_nonnba <- renderPlotly({
        
        dtafig<-dtaall_latest%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab1))),
                         group))
        
        dtafig%>%
            plot_ly(
                y = ~college,
                type="bar", 
                x = ~pct_nonnba_received, 
                name="Received any NNB assistance",
                text = ~pct_nonnba_received, textposition = 'outside',
                line = list(color = redcolors[5]), marker = list(color = redcolors[5]),
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>% received any NNB assistance: ", dtafig$pct_nonnba_received, "%"))%>%    
            layout(
                title = "", 
                xaxis = list(title = "Percent enrolled students",  
                             range=c(0, 100), 
                             showgrid = FALSE, showticklabels = TRUE),
                yaxis = list(title = "",
                             tickfont = list(size=9),
                             categoryorder = "total descending"),
                showlegend = FALSE
            )
    })
    
    output$plot_amount_nonnba <- renderPlotly({
        
        dtafig<-dtaall_latest%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab1))),
                         group))%>%
            filter(pct_nonnba_received>=5)
        
        averagetuition<-mean(dtafig$tuition, na.rm = TRUE)    
        
        dtafig%>%
            plot_ly(
                y = ~college,
                type="bar", 
                x = ~amount_nonnba,
                text = ~amount_nonnba, textposition = 'outside',
                marker = list(color = redcolors[3]),
                hovertemplate = paste0(
                    dtafig$college,
                    '<br>Percent who received of any NNB assistance: ', dtafig$pct_nonnba_received, "%",
                    '<br>Average amount of any NNB assistance: $', dtafig$amount_nonnba,
                    '<br>Tuition: $', dtafig$tuition))%>%
            layout(
                title = "", 
                xaxis = list(title = "Average any NNB assistance ($)", range=c(0, 80000),  
                             showgrid = FALSE, showticklabels = TRUE),
                yaxis = list(title = "", 
                             tickfont = list(size=9),
                             categoryorder = "total descending"),
                #shapes = list(vline(averagetuition)),
                showlegend = FALSE
            )
    })    
    
    ##### output: Tab 2 #####
    
    output$plot_aca <- renderPlotly({    

        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab2))),
                         group))%>%
            filter(year==yearlatest)%>%
            select(college, starts_with(c("num_aca_")))
    
        dtafig<-as.data.frame(dtafig)
        
        indicatorlist<-colnames(dtafig[2:ncol(dtafig)]) 
        
        dtafiglong<-reshape(dtafig, 
                            direction='long', 
                            varying=colnames(dtafig[2:ncol(dtafig)]), 
                            timevar="criteria",
                            times=indicatorlist,
                            v.names=c("value"),
                            idvar="college")%>%
            mutate(criteria = sub("num_aca_", "", criteria))
        
        dtafiglong%>%
            plot_ly(
                x = ~criteria, y = ~college, z = ~value, 
                #colors = colorRamp(c("red", "green")),
                zauto = F, zmin = 0, zmax = 100,
                colors = brewer.pal(20,"RdYlGn"),
                type = "heatmap",
                hoverinfo = 'text',
                text = ~paste(
                    "Criterion:", dtafiglong$criteria,
                    "<br>College:", dtafiglong$college
                    #,
                    #"<br>Value:",  dtafiglong$value
                )
            )%>%
            layout(
                xaxis=list(title = "", showgrid = FALSE), 
                yaxis=list(title = "", showgrid = FALSE, tickfont = list(size=9))
            )%>% 
            hide_colorbar()
    
})    
    
    output$plot_acna <- renderPlotly({    
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab2))),
                         group))%>%
            filter(year==yearlatest)%>%
            select(college, starts_with(c("num_acna_")))
        
        dtafig<-as.data.frame(dtafig)
        
        indicatorlist<-colnames(dtafig[2:ncol(dtafig)]) 
        
        dtafiglong<-reshape(dtafig, 
                            direction='long', 
                            varying=colnames(dtafig[2:ncol(dtafig)]), 
                            timevar="criteria",
                            times=indicatorlist,
                            v.names=c("value"),
                            idvar="college"
        )%>%
            mutate(criteria = sub("num_acna_", "", criteria))
        
        dtafiglong%>%
            plot_ly(
                x = ~criteria, y = ~college, z = ~value, 
                #colors = colorRamp(c("red", "green")),
                zauto = F, zmin = 0, zmax = 100,
                colors = brewer.pal(20,"RdYlGn"),
                type = "heatmap",
                hoverinfo = 'text',
                text = ~paste(
                    "Criterion:", dtafiglong$criteria,
                    "<br>College:", dtafiglong$college
                    #,
                    #"<br>Value:",  dtafiglong$value
                )
            )%>%
            layout(
                xaxis=list(title = "", showgrid = FALSE), 
                yaxis=list(title = "", showgrid = FALSE, tickfont = list(size=9))
            )%>% 
            hide_colorbar()
        
    })        
    
    ##### output: Tab 3 #####
    
    output$plot_admit <- renderPlotly({    
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab3))),
                          group))
        
        dtafig%>%
            plot_ly(
                x = ~year, 
                type="scatter", mode = 'lines+markers', 
                y = ~pct_admit_total, 
                color = ~college, 
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Year: ", dtafig$year, 
                    "<br>Admission rate: ", dtafig$pct_admit_total, " (%)"))%>%
            layout(
                title = "", showlegend = FALSE, 
                shapes = list(vline(2021)),
                yaxis = list(title = "Admission rate (%)",  
                             range=c(0, 100), 
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023))
            )
    })       
    
    output$plot_admit_ed <- renderPlotly({    
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab3))),
                         group))
        
        dtafig%>%
            plot_ly(
                x = ~year, 
                type="scatter", mode = 'lines+markers', 
                y = ~pct_admit_ed, 
                color = ~college, 
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Year: ", dtafig$year, 
                    "<br>Admission rate: ", dtafig$pct_admit_ed, " (%)"))%>%
            layout(
                title = "", showlegend = FALSE, 
                shapes = list(vline(2021)),
                yaxis = list(title = "Admission rate among ED (%)",  
                             range=c(0, 100), 
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023))
            )
    })           
    
    output$plot_relnumadmit_total <- renderPlotly({    
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab3))),
                         group))
        
        dtafig%>%
            plot_ly(
                x = ~year, 
                type="scatter", mode = 'lines+markers', 
                y = ~relnumadmit_total,
                color = ~college, 
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Year: ", dtafig$year, 
                    "<br>Ratio: ", dtafig$relnumadmit_total)
                )%>%
            layout(
                title = "", showlegend = FALSE, 
                yaxis = list(title = "Ratio (reference = value in the latest year)",  
                             range=c(0, 3), 
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                shapes = list(hline(1), vline(2021)),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023))
            )
        
    })  
    
    output$plot_relnumapp_total <- renderPlotly({    
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab3))),
                         group))
        
        dtafig%>%
            plot_ly(
                x = ~year, 
                type="scatter", mode = 'lines+markers', 
                y = ~relnumapp_total,
                color = ~college, 
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Year: ", dtafig$year, 
                    "<br>Ratio: ", dtafig$relnumapp_total))%>%
            layout(
                title = "", showlegend = FALSE, 
                shapes = list(hline(1), vline(2021)),
                yaxis = list(title = "Ratio (reference = value in the latest year)",  
                             range=c(0, 1.6), 
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023))
            )        
    }) 
    
    output$plot_admit_yield <- renderPlotly({    
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab3))),
                         group))
        
        dtafig%>%
            plot_ly(
                x = ~pct_admit_total, 
                type="scatter", mode = 'markers', 
                y = ~pct_yield_total,
                color = ~college, 
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Year: ", dtafig$year, 
                    "<br>Yield rate: ", dtafig$pct_yield_total, " (%)"))%>%
            layout(
                title = "", showlegend = FALSE, 
                yaxis = list(title = "Yield rate (%)",  
                             range=c(0, 100), 
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                shapes = list(hline(1), vline(2021)),
                xaxis = list(title = "Admission rate (%)", showgrid = FALSE , range=c(0, 100))
            )        
    })       
    
    output$plot_satsubmit <- renderPlotly({    
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab3))),
                         group))
        
        dtafig%>%
            plot_ly(
                x = ~year, y = ~pct_submitsat, 
                type="scatter", mode = 'lines+markers', 
                color = ~college,
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Year: ", dtafig$year, 
                    "<br>SAT score submission rate: ", dtafig$pct_submitsat, " (%)"))%>%
            layout(
                title = "", showlegend = FALSE, 
                shapes = list(vline(2021)),
                yaxis = list(title = "Percent who submitted SAT scores",  
                             range=c(0,100), 
                             showgrid = FALSE, 
                             showticklabels = TRUE, tickfont = list(size=9)),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023))
            )
    })     
    
    output$plot_satscore25th <- renderPlotly({    
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab3))),
                         group))
        
        dtafig%>%
            plot_ly(
                x = ~year, y = ~sat25, 
                type="scatter", mode = 'lines+markers', 
                color = ~college,
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Year: ", dtafig$year, 
                    "<br>SAT score, composite, 25th: ", dtafig$sat25))%>%
            layout(
                title = "", showlegend = FALSE, 
                shapes = list(vline(2021), hline(1400)),
                yaxis = list(title = "SAT score",  
                             range=c(1100, 1600), 
                             showgrid = FALSE, 
                             showticklabels = TRUE, tickfont = list(size=9)),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023))
            )
        
        
    })     
    
    output$plot_satscore75th <- renderPlotly({    
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab3))),
                         group))
        
        dtafig%>%
            plot_ly(
                x = ~year, y = ~sat75, 
                type="scatter", mode = 'lines+markers', 
                color = ~college,
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Year: ", dtafig$year, 
                    "<br>SAT score, composite, 75th: ", dtafig$sat75))%>%
            layout(
                title = "", showlegend = FALSE, 
                shapes = list(vline(2021), hline(1400)),
                yaxis = list(title = "SAT score",  
                             range=c(1100, 1600), 
                             showgrid = FALSE, 
                             showticklabels = TRUE, tickfont = list(size=9)),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023))
            )
        
        
    })         
    
    output$plot_top10th <- renderPlotly({    
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab3))),
                         group))
        
        dtafig%>%
            plot_ly(
                x = ~year, y = ~pct_top10pct, 
                type="scatter", mode = 'lines+markers', 
                color = ~college, 
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Year: ", dtafig$year, 
                    "<br>Class rank within top 10 percent: ", dtafig$pct_top10pct, " (%)"))%>%
            layout(
                title = "", showlegend = FALSE, 
                shapes = list(vline(2021), hline(80)),
                yaxis = list(title = "Percent of enrolled students",  
                             range=c(0,100), 
                             showgrid = FALSE, 
                             showticklabels = TRUE, tickfont = list(size=9)),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023))
            )
    }) 

    output$plot_ed_among_enroll <- renderPlotly({
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab3))),
                         group))%>%
            filter((earlydecision==1 & count>=2) | 
                       (is.na(numapp_ed)==FALSE & count>=2))
        
        dtafig%>%
            plot_ly(
                x = ~year, y = ~pct_ed_among_enroll,
                type="scatter", mode = 'lines+markers', 
                color = ~college,
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Year: ", dtafig$year, 
                    "<br>ED among enrolled: ", dtafig$pct_ed_among_enroll, " (%)"))%>%
            layout(
                title = "", showlegend = FALSE, 
                shapes = list(hline(50), vline(2021)),
                yaxis = list(title = "Percent of enrolled students",  
                             range=c(0, 100), 
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023))
            )
    })

    output$plot_admit_all_ed <- renderPlotly({
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab3))),
                         group))
        
        dtafig%>%
            plot_ly(
                x = ~pct_admit_total, y = ~pct_admit_ed,
                type="scatter", mode = 'markers', 
                color = ~college,
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Acceptance rate, total: ", dtafig$pct_admit_total, " (%)",
                    "<br>Acceptance rate, ED: ", dtafig$pct_admit_ed, " (%)",
                    "<br>Year: ", dtafig$year))%>%
            add_trace(
                y = ~pct_admit_total, 
                type="scatter", mode = 'lines', 
                marker = list(
                    color = 'darkgrey',
                    size = 1))%>%
            layout(
                title = "", showlegend = FALSE, 
                yaxis = list(title = "Admission rate, ED (%)",  
                             range=c(0, 100), 
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                #shapes = list(vline(2021)),
                xaxis = list(title = "Admission rate, total (%)", showgrid = FALSE , range=c(0, 100))
            )
    })
    
    output$plot_yield_admit <- renderPlotly({
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab3))),
                         group))
        
        dtafig%>%
            plot_ly(
                x = ~pct_admit_total, y = ~pct_yield_total,
                type="scatter", mode = 'markers', 
                color = ~college,
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Admission rate: ", dtafig$pct_admit_total, " (%)",
                    "<br>Yield rate: ", dtafig$pct_yield_total, " (%)",
                    "<br>Year: ", dtafig$year))%>%
            layout(
                title = "", showlegend = FALSE, 
                yaxis = list(title = "Yield rate (%)",  
                             range=c(0, 100), 
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                #shapes = list(vline(2021)),
                xaxis = list(title = "Admission rate (%)", showgrid = FALSE , range=c(0, 100))
            )
    })
    
    output$plot_retention_admit <- renderPlotly({
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab3))),
                         group))
        
        dtafig%>%
            plot_ly(
                x = ~pct_admit_total, y = ~pct_retention,
                type="scatter", mode = 'markers', 
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Admission rate: ", dtafig$pct_admit_total, " (%)",
                    "<br>Retention rate: ", dtafig$pct_retention, " (%)",
                    "<br>Year: ", dtafig$year),            
                color = ~college)%>%
            layout(
                title = "", showlegend = FALSE, 
                yaxis = list(title = "Retention rate (%)",  
                             range=c(60, 100), 
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                shapes = list(hline(90)),
                xaxis = list(title = "Admission rate (%)", showgrid = FALSE , range=c(0, 100))
            )
    })    
    
    output$plot_retention_trend <- renderPlotly({
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab3))),
                         group))
        
        dtafig%>%
            plot_ly(
                x = ~year, y = ~pct_retention,
                type="scatter", mode = 'lines+markers', 
                color = ~college,
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Retention rate: ", dtafig$pct_retention, " (%)",
                    "<br>Year: ", dtafig$year))%>%
            layout(
                title = "", showlegend = FALSE, 
                shapes = list(hline(90), vline(2021)),
                yaxis = list(title = "Percent",  
                             range=c(70, 100), 
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023))
            )
    })
    
    ##### output: Tab 4: student pop + transfer #####    
    output$plot_ratiogender <- renderPlotly({
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab4))),
                         group))
        
        dtafig%>%
            plot_ly(
                x = ~year, y = ~ratio_underft_mf,
                type="scatter", mode = 'lines+markers', 
                color = ~college,
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Year: ", dtafig$year, 
                    "<br>Ratio: ", dtafig$ratio_underft_mf))%>%
            layout(
                title = "", showlegend = FALSE, 
                shapes = list(hline(100), vline(2021)),
                yaxis = list(title = "Ratio", 
                             range=c(0, 150),
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023))
            )
    })    
    
    output$plot_pct_under_asian <- renderPlotly({
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab4))),
                         group))
        
        dtafig%>%
            plot_ly(
                x = ~year, y = ~pct_under_asian,
                type="scatter", mode = 'lines+markers', 
                name = "Asian",
                color = ~college,
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Year: ", dtafig$year, 
                    "<br>Asian: ", dtafig$pct_under_asian, " (%)"))%>%
            layout(
                title = "", showlegend = FALSE, 
                shapes = list(vline(2021), hline(20)),
                yaxis = list(title = "Percent of undergraduate students",  
                             range=c(0, 40), 
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023))
            )
    })
    
    output$plot_pct_under_nra <- renderPlotly({
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab4))),
                         group))
        
        dtafig%>%
            plot_ly(
                x = ~year, y = ~pct_under_nra,
                type="scatter", mode = 'lines+markers', 
                name = "Non resident alien",
                color = ~college,
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Year: ", dtafig$year, 
                    "<br>Non-resident Aliens: ", dtafig$pct_under_nra, " (%)"))%>%
            layout(
                title = "", showlegend = FALSE, 
                shapes = list(vline(2021), hline(20)),
                yaxis = list(title = "Percent of undergraduate students",  
                             range=c(0, 40), 
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023))
            )
    })    

    output$plot_pct_under_nra_rank <- renderPlotly({
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab4))),
                         group))%>%
            filter(year==yearlatest)
        
        dtafig%>%
            plot_ly(
                y = ~college,
                type="bar", 
                x = ~pct_under_nra, 
                text = ~pct_under_nra, textposition = 'outside',
                line = list(color = bluecolors[5]), 
                marker = list(color = bluecolors[5]),
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>% of undergraduate who are non-US residents: ", dtafig$pct_under_nra, "%"))%>%    
            layout(
                title = "", 
                xaxis = list(title = "Percent undergraduate students",  
                             range=c(0, 100), 
                             showgrid = FALSE, showticklabels = TRUE),
                yaxis = list(title = "",
                             tickfont = list(size=9),
                             categoryorder = "total descending"),
                showlegend = FALSE
            )
    })            

    output$plot_ratio_admit_trans <- renderPlotly({
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab4))),
                         group))
        
        dtafig%>%
            plot_ly(
                x = ~year, y = ~ratio_admit_trans,
                type="scatter", mode = 'lines+markers', 
                color = ~college,
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Year: ", dtafig$year, 
                    "<br>For every 100 freshmen enrolled, ",
                    "<br>there are ", dtafig$ratio_admit_trans, " students, who are enrolled via transfer"))%>%   
            layout(
                title = "", showlegend = FALSE, 
                shapes = list(vline(2021)),
                yaxis = list(title = "Ratio: transfer over freshmen",  
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023))
            )
    })      
    
    output$plot_ratio_admit_trans_rank <- renderPlotly({
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab4))),
                         group))%>%
            filter(year==yearlatest)
        
        dtafig%>%
            plot_ly(
                y = ~college,
                type="bar", 
                x = ~ratio_admit_trans, 
                text = ~ratio_admit_trans, textposition = 'auto',
                line = list(color = greencolors[3]), 
                marker = list(color = greencolors[3]),
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>For every 100 freshmen enrolled, ",
                    "<br>there are ", dtafig$ratio_admit_trans, " students, who are enrolled via transfer"))%>%   
            layout(
                title = "", 
                xaxis = list(title = "Ratio: transfer over freshmen",   
                             showgrid = FALSE, showticklabels = TRUE),
                yaxis = list(title = "",
                             tickfont = list(size=9),
                             categoryorder = "total descending"),
                showlegend = FALSE
            )
    })        
    
    output$plot_relnumtransapp_total <- renderPlotly({
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab4))),
                         group))
        
        dtafig%>%
            plot_ly(
                x = ~year, y = ~relnumtransapp_total,
                type="scatter", mode = 'lines+markers', 
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Year: ", dtafig$year, 
                    "<br>Number of applications: ", dtafig$numtransapp_total,
                    "<br>Relative number against the reference: ", dtafig$relnumtransapp_total),     
                color = ~college)%>%
            layout(
                title = "", showlegend = FALSE,
                yaxis = list(title = "Ratio (reference = value in the latest year)",  
                             range=c(0, 3), 
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                shapes = list(hline(1), vline(2021)),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023))
            )
    })
    
    output$plot_relnumtransadmit_total <- renderPlotly({
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab4))),
                         group))
        
        dtafig%>%
            plot_ly(
                x = ~year, y = ~relnumtransadmit_total,
                type="scatter", mode = 'lines+markers', 
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Year: ", dtafig$year, 
                    "<br>Number of admitted students: ", dtafig$numtransadmit_total,
                    "<br>Relative number against the reference: ", dtafig$relnumtransadmit_total),     
                color = ~college)%>%
            layout(
                title = "", showlegend = FALSE,
                yaxis = list(title = "Ratio (reference = value in the latest year)",  
                             range=c(0, 3), 
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                shapes = list(hline(1), vline(2021)),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023))
            )
    })
    
    output$plot_relnumtransenroll_total <- renderPlotly({
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab4))),
                         group))
        
        dtafig%>%
            plot_ly(
                x = ~year, y = ~relnumtransenroll_total,
                type="scatter", mode = 'lines+markers', 
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Year: ", dtafig$year, 
                    "<br>Number of enrolled students: ", dtafig$numtransenroll_total,
                    "<br>Relative number against the reference: ", dtafig$relnumtransenroll_total),     
                color = ~college)%>%
            layout(
                title = "", showlegend = FALSE,
                yaxis = list(title = "Ratio (reference = value in the latest year)",  
                             range=c(0, 3), 
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                shapes = list(hline(1), vline(2021)),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023))
            )
    })    
    
    ##### output: Tab 4: Student life #####    
    
    output$plot_pct_us_oos <- renderPlotly({
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab4))),
                         group))
        
        dtafig%>%
            plot_ly(
                x = ~year, y = ~pct_oos,
                type="scatter", mode = 'lines+markers', 
                name = "Out of State US students",
                color = ~college,
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Year: ", dtafig$year, 
                    "<br>Out of state: ", dtafig$pct_oos, " (%)"))%>%
            layout(
                title = "", showlegend = FALSE, 
                shapes = list(vline(2021)),
                yaxis = list(title = "Percent of enrolled US undergraduates",  
                             range=c(0, 100), 
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023))
            )
    })      

    output$plot_pct_us_oos_rank <- renderPlotly({
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab4))),
                         group))%>%
            filter(year==yearlatest)
        
        dtafig%>%
            plot_ly(
                y = ~college,
                type="bar", 
                x = ~pct_oos, 
                text = ~pct_oos, textposition = 'outside',
                line = list(color = bluecolors[5]), 
                marker = list(color = bluecolors[5]),
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>% of US undergraduate who are out-of-state: ", dtafig$pct_oos, "%"))%>%    
            layout(
                title = "", 
                xaxis = list(title = "Percent US undergraduate students",  
                             range=c(0, 100), 
                             showgrid = FALSE, showticklabels = TRUE),
                yaxis = list(title = "",
                             tickfont = list(size=9),
                             categoryorder = "total descending"),
                showlegend = FALSE
            )
    })    
    
    output$plot_pct_housing_freshmen <- renderPlotly({
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab4))),
                         group))
        
        dtafig%>%
            plot_ly(
                x = ~year, y = ~pct_colhouse_fresh,
                type="scatter", mode = 'lines+markers', 
                color = ~college,
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Year: ", dtafig$year, 
                    "<br>Percent of freshmen living in univerisity housing: ", dtafig$pct_colhouse_fresh, " (%)"))%>%
            layout(
                title = "", showlegend = FALSE, 
                shapes = list(vline(2021)),
                yaxis = list(title = "Percent of freshmen",  
                             range=c(0, 100), 
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023))
            )
    })      

    output$plot_pct_housing_freshmen_rank <- renderPlotly({
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab4))),
                         group))%>%
            filter(year==yearlatest)
        
        dtafig%>%
            plot_ly(
                y = ~college,
                type="bar", 
                x = ~pct_colhouse_fresh, 
                text = ~pct_colhouse_fresh, textposition = 'auto',
                line = list(color = bluecolors[3]), 
                marker = list(color = bluecolors[3]),
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>% of freshmen living in university housing: ", dtafig$pct_colhouse_fresh, "%"))%>%    
            layout(
                title = "", 
                xaxis = list(title = "Percent of freshmen",  
                             range=c(0, 100), 
                             showgrid = FALSE, showticklabels = TRUE),
                yaxis = list(title = "",
                             tickfont = list(size=9),
                             categoryorder = "total descending"),
                shapes = list(vline(90), vline(50)),
                showlegend = FALSE
            )
    })      
    
    output$plot_pct_housing <- renderPlotly({
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab4))),
                         group))
        
        dtafig%>%
            plot_ly(
                x = ~year, y = ~pct_colhouse,
                type="scatter", mode = 'lines+markers', 
                color = ~college,
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Year: ", dtafig$year, 
                    "<br>Percent of undergraduates living in univerisity housing: ", dtafig$pct_colhouse, " (%)"))%>%
            layout(
                title = "", showlegend = FALSE, 
                shapes = list(vline(2021)),
                yaxis = list(title = "Percent of undergraduates",  
                             range=c(0, 100), 
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023))
            )
    })      
    
    output$plot_pct_housing_rank <- renderPlotly({
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab4))),
                         group))%>%
            filter(year==yearlatest)
        
        dtafig%>%
            plot_ly(
                y = ~college,
                type="bar", 
                x = ~pct_colhouse, 
                text = ~pct_colhouse, textposition = 'auto',
                line = list(color = bluecolors[3]), 
                marker = list(color = bluecolors[3]),
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>% of undergraduates living in university housing: ", dtafig$pct_colhouse, "%"))%>%    
            layout(
                title = "", 
                xaxis = list(title = "Percent of undergraduates",  
                             range=c(0, 100), 
                             showgrid = FALSE, showticklabels = TRUE),
                yaxis = list(title = "",
                             tickfont = list(size=9),
                             categoryorder = "total descending"),
                shapes = list(vline(90), vline(50)),
                showlegend = FALSE
            )
    })        
    
    output$plot_pct_frat <- renderPlotly({
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab4))),
                         group))
        
        dtafig%>%
            plot_ly(
                x = ~year, y = ~pct_frat,
                type="scatter", mode = 'lines+markers', 
                color = ~college,
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Year: ", dtafig$year, 
                    "<br>Percent of undergraduate men in fraternities: ", dtafig$pct_frat, " (%)"))%>%
            layout(
                title = "", showlegend = FALSE, 
                shapes = list(vline(2021)),
                yaxis = list(title = "Percent of undergraduate men",  
                             range=c(0, 100), 
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023))
            )
    })      
    
    output$plot_pct_frat_rank <- renderPlotly({
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab4))),
                         group))%>%
            filter(year==yearlatest)
        
        dtafig%>%
            plot_ly(
                y = ~college,
                type="bar", 
                x = ~pct_frat, 
                text = ~pct_frat, textposition = 'auto',
                line = list(color = greencolors[3]), 
                marker = list(color = greencolors[3]),
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Percent of undergraduate men in fraternities: ", dtafig$pct_frat, " (%)"))%>%   
            layout(
                title = "", 
                xaxis = list(title = "Percent of undergraduate men",  
                             range=c(0, 100), 
                             showgrid = FALSE, showticklabels = TRUE),
                yaxis = list(title = "",
                             tickfont = list(size=9),
                             categoryorder = "total descending"),
                showlegend = FALSE
            )
    })        
    
    output$plot_pct_soro <- renderPlotly({
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab4))),
                         group))
        
        dtafig%>%
            plot_ly(
                x = ~year, y = ~pct_soro,
                type="scatter", mode = 'lines+markers', 
                color = ~college,
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Year: ", dtafig$year, 
                    "<br>Percent of undergraduate women in sororities: ", dtafig$pct_soro, " (%)"))%>%
            layout(
                title = "", showlegend = FALSE, 
                shapes = list(vline(2021)),
                yaxis = list(title = "Percent of undergraduate women",  
                             range=c(0, 100), 
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023))
            )
    })      
    
    output$plot_pct_soro_rank <- renderPlotly({
        
        dtafig<-dta%>%
            filter(grepl(sub("\\'.*", "", 
                             sub("\\s.*", "", 
                                 tolower(input$collegegroup_tab4))),
                         group))%>%
            filter(year==yearlatest)
        
        dtafig%>%
            plot_ly(
                y = ~college,
                type="bar", 
                x = ~pct_soro, 
                text = ~pct_soro, textposition = 'auto',
                line = list(color = greencolors[3]), 
                marker = list(color = greencolors[3]),
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Percent of undergraduate women in sororities: ", dtafig$pct_soro, " (%)"))%>%   
            layout(
                title = "", 
                xaxis = list(title = "Percent of undergraduate women",  
                             range=c(0, 100), 
                             showgrid = FALSE, showticklabels = TRUE),
                yaxis = list(title = "",
                             tickfont = list(size=9),
                             categoryorder = "total descending"),
                showlegend = FALSE
            )
    })        
    

    
    ##### output: Tab 5 #####

    observeEvent(input$collegegroup_tab5, {
        # Define choices for the second selectInput based on the first choice
        college_tab5 <- switch(input$collegegroup_tab5,
                               "All" = collegelist_all, 
                               "Size: Tiny (undergraduate <2000)" = collegelist_tiny, 
                               "Size: Small (undergraduate 2K-5K)" = collegelist_small, 
                               "Size: Medium (undergraduate 5K-10K)" = collegelist_medium, 
                               "Size: Large (undergraduate 10K-30K)" = collegelist_large, 
                               "Size: Huge (undergraduate 30K+)" = collegelist_huge, 
                               
                               "Acceptance rate: below 20%" = collegelist_reach,     
                               "Acceptance rate: 20% - 50%" = collegelist_target,
                               "Acceptance rate: 50% or higher" = collegelist_safety, 
                               
                               "Open curriculum colleges" = collegelist_open,
                               "Public universities" = collegelist_public,
                               "Women's colleges" = collegelist_women
                               )

        # Update the choices for the second selectInput
        updateSelectInput(session, "college_tab5", choices = college_tab5)
    })
    
    output$text_college <- renderText({
        paste(input$college_tab5) 
    })        

    output$text_offer <- renderText({
        paste("Does", input$college_tab5, "provide the following?") 
    })    
    
    output$text_offer1 <- renderText({
        
        dtafig<-dta%>%filter(college==input$college_tab5)%>%
            filter(year==yearlatest)
        
        paste("Cross-registration:", dtafig$offer_cross)
    })   
    
    output$table_offer <- renderTable({
        
        dtafig<-dta%>%filter(college==input$college_tab5)%>%
        #dtafig<-dta%>%filter(college=="Middlebury")%>%    
            filter(year==yearlatest)%>%
            select(starts_with("offer_"))
        
        temp<-data.frame(t(dtafig))
        temp = setNames(data.frame(t(dtafig[,-1])), dtafig[,1])
        
        temp<-temp%>%
            mutate(
                var = rownames(temp),
                #var = sub(".*_", "", var), 
                Program = ifelse(grepl("cross", var)==TRUE, "Cross_registration",
                                 ifelse(grepl("double", var)==TRUE, "Double major",
                                        ifelse(grepl("honor_size", var)==TRUE, "(HP freshmen size)",  
                                        ifelse(grepl("honor_web", var)==TRUE, "(HP website)",         
                                        ifelse(grepl("honor", var)==TRUE, "Honors program",         
                                               ifelse(grepl("intern", var)==TRUE, "Internships",
                                                      ifelse(grepl("career", var)==TRUE, "Liberal arts/career combination",                                            
                                                             ifelse(grepl("stdesign", var)==TRUE, "Student-designed major",
                                                                    ifelse(grepl("abroad", var)==TRUE, "Study abroad",      
                                                                           ifelse(grepl("teacher", var)==TRUE, "Teacher certification",      
                                                                           "?"))))))))))
            )%>%
            rename(Offer = colnames(temp)[1])%>%
            select(Program, Offer)

    })      

    output$cplot_pct_assistance <- renderPlotly({    
        
        dtafig<-dtaall_latest%>%filter(college==input$college_tab5) 
        
        dtafig%>%
            plot_ly(
                x = ~college, 
                type="bar", 
                y = ~pct_nba_applied, 
                name="Applied<br>for NBA",
                text = ~pct_nba_applied, textposition = 'outside',
                line = list(color = bluecolors[3]), marker = list(color = bluecolors[3]))%>%
            add_trace(
                y = ~pct_nba_determined, 
                name="Determined<br>to have need",
                text = ~pct_nba_determined, textposition = 'outside',
                line = list(color = bluecolors[4]), marker = list(color = bluecolors[4]))%>%
            add_trace(
                y = ~pct_nba_received, 
                name="Received NBA,<br>any",
                text = ~pct_nba_received, textposition = 'outside',
                line = list(color = bluecolors[5]), marker = list(color = bluecolors[5]))%>%
            add_trace(
                y = ~pct_nba_received_grant, 
                name="Received NBA,<br>grant/scholarship",
                text = ~pct_nba_received_grant, textposition = 'outside',
                line = list(color = bluecolors[6]), marker = list(color = bluecolors[6]))%>%
            add_trace(
                y = ~pct_nonnba_received, 
                name="Received<br>Non-NBA",
                text = ~pct_nonnba_received, textposition = 'outside',
                line = list(color = redcolors[5]), marker = list(color = redcolors[5]))%>%  
            
            layout(
                title = "", 
                yaxis = list(title = "Percent enrolled students",  
                             range=c(0, 100), 
                             showgrid = FALSE, showticklabels = TRUE),
                xaxis = list(title = "",
                             tickfont = list(size=9),
                             categoryorder = "total descending"),
                legend = list(orientation = "h", font = list(size=8), 
                              xanchor = "center", x = 0.5, 
                              yanchor = "top", y = -0.05)
            )
    })
    
    output$cplot_amount_assistance <- renderPlotly({
        
        dtafig<-dtaall_latest%>%filter(college==input$college_tab5) 
        
        dtafig%>%
            plot_ly(
                x = ~college, 
                type="bar", 
                y = ~amount_nba,
                text = ~amount_nba, textposition = 'outside',
                name = "NBA, any", 
                marker = list(color = bluecolors[5]),
                hovertemplate = paste0(
                    dtafig$college,
                    '<br>Percent who received any NBA: ', dtafig$pct_nba_received, "%",
                    '<br>Average amount of any NBA: $', dtafig$amount_nba))%>%
            add_trace(
                y = ~amount_nba_grant,
                text = ~amount_nba_grant, textposition = 'outside',
                name = "NBA, grant/scholarship", 
                marker = list(color = bluecolors[6]),
                hovertemplate = paste0(
                    dtafig$college,
                    '<br>Percent who received grant/scholarship NBA: ', dtafig$pct_nba_received, "%",
                    '<br>Average amount of grant/scholarship NBA: $', dtafig$amount_nba_grant))%>%
            add_trace(
                y = ~amount_nonnba,
                text = ~amount_nonnba, textposition = 'outside',
                name = "Non-NBA", 
                marker = list(color = redcolors[5]),
                hovertemplate = paste0(
                    dtafig$college,
                    '<br>Percent who received of NNBA: ', dtafig$pct_nonnba_received, "%",
                    '<br>Average amount of NNBA: $', dtafig$amount_nonnba))%>%            
            add_annotations(
                text = (paste0("Tuition: $", dtafig$tuition)),
                x = 0.9, xref = "paper", y = ~tuition, yref = "y",    
                xanchor = "center", yanchor = "bottom", showarrow = FALSE,
                font = list(size = 12)
            ) %>%  
            add_annotations(
                text = ("Tuition + Fees + R&B"),
                x = 0.9, xref = "paper", y = ~totalcost, yref = "y",    
                xanchor = "center", yanchor = "bottom", showarrow = FALSE,
                font = list(size = 12)
            ) %>%              
            layout(
                title = "", 
                yaxis = list(title = "Average amount ($)", range=c(0, 80000),  
                             showgrid = FALSE, showticklabels = TRUE),
                xaxis = list(title = "", 
                             tickfont = list(size=9),
                             categoryorder = "total descending"),
                shapes = list(hline(dtafig$tuition), hline(dtafig$totalcost)),
                legend = list(orientation = "h", font = list(size=8), 
                              xanchor = "center", x = 0.5, 
                              yanchor = "top", y = -0.05)
            )
    })    

    output$cplot_aca <- renderPlotly({    
        
        dtafig<-dta%>%
            filter(college==input$college_tab5)%>% 
            filter(year==yearlatest)%>%
            select(college, starts_with(c("num_aca_")))
        
        dtafig<-as.data.frame(dtafig)
        
        indicatorlist<-colnames(dtafig[2:ncol(dtafig)]) 
        
        dtafiglong<-reshape(dtafig, 
                            direction='long', 
                            varying=colnames(dtafig[2:ncol(dtafig)]), 
                            timevar="criteria",
                            times=indicatorlist,
                            v.names=c("value"),
                            idvar="college")%>%
            mutate(criteria = sub("num_aca_", "", criteria))
        
        dtafiglong%>%
            plot_ly(
                x = ~criteria, y = ~college, z = ~value, 
                #colors = colorRamp(c("red", "green")),
                zauto = F, zmin = 0, zmax = 100,
                colors = brewer.pal(20,"RdYlGn"),
                type = "heatmap",
                hoverinfo = 'text',
                text = ~paste(
                    "Criterion:", dtafiglong$criteria,
                    "<br>College:", dtafiglong$college
                    #,
                    #"<br>Value:",  dtafiglong$value
                )
            )%>%
            layout(
                xaxis=list(title = "", showgrid = FALSE), 
                yaxis=list(title = "", showgrid = FALSE, tickfont = list(size=9))
            )%>% 
            hide_colorbar()
        
    })    
    
    output$cplot_acna <- renderPlotly({    
        
        dtafig<-dta%>%
            filter(college==input$college_tab5)%>% 
            filter(year==yearlatest)%>%
            select(college, starts_with(c("num_acna_")))
        
        dtafig<-as.data.frame(dtafig)
        
        indicatorlist<-colnames(dtafig[2:ncol(dtafig)]) 
        
        dtafiglong<-reshape(dtafig, 
                            direction='long', 
                            varying=colnames(dtafig[2:ncol(dtafig)]), 
                            timevar="criteria",
                            times=indicatorlist,
                            v.names=c("value"),
                            idvar="college"
        )%>%
            mutate(criteria = sub("num_acna_", "", criteria))
        
        dtafiglong%>%
            plot_ly(
                x = ~criteria, y = ~college, z = ~value, 
                #colors = colorRamp(c("red", "green")),
                zauto = F, zmin = 0, zmax = 100,
                colors = brewer.pal(20,"RdYlGn"),
                type = "heatmap",
                hoverinfo = 'text',
                text = ~paste(
                    "Criterion:", dtafiglong$criteria,
                    "<br>College:", dtafiglong$college
                    #,
                    #"<br>Value:",  dtafiglong$value
                )
            )%>%
            layout(
                xaxis=list(title = "", showgrid = FALSE), 
                yaxis=list(title = "", showgrid = FALSE, tickfont = list(size=9))
            )%>% 
            hide_colorbar()
        
    })        
    
    output$cplot_pct_under_asian_nra <- renderPlotly({
        
        dtafig<-dta%>%filter(college==input$college_tab5) 
        
        dtafig%>%
            plot_ly(
                x = ~year, y = ~pct_under_asian,
                type="scatter", mode = 'lines+markers', 
                name = "Asian",
                line = list(color = bluecolors[5]),
                marker = list(color = bluecolors[5]))%>%
            add_trace(
                y = ~pct_under_nra,
                name = "Non resident alien",
                line = list(color = orangecolors[5]),
                marker = list(color = orangecolors[5]))%>%
            layout(
                title = "", 
                shapes = list(vline(2021), hline(20)),
                yaxis = list(title = "Percent of undergraduate students",  
                             range=c(0, 40), 
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023)),
                legend = list(orientation = "v", font = list(size=9), 
                              xanchor = "left", x = 0.05)
            )
    })
    
    output$cplot_pct_us_oos <- renderPlotly({
        
        dtafig<-dta%>%filter(college==input$college_tab5) 
        
        dtafig%>%
            plot_ly(
                x = ~year, y = ~pct_oos,
                type="scatter", mode = 'lines+markers', 
                name = "Out of State US students",
                line = list(color = greycolors[5]),
                marker = list(color = greycolors[5]))%>%
            layout(
                title = "", 
                shapes = list(vline(2021)),
                yaxis = list(title = "Percent of enrolled US freshmen",  
                             range=c(0, 100), 
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023)),
                showlegend = FALSE
            )
    })   

    output$cplot_ratio_gender <- renderPlotly({    
        
        dtafig<-dta%>%filter(college==input$college_tab5) 
        
        dtafig%>% 
            filter(women!=1)%>%    
            plot_ly(
                x = ~year, y = ~ratio_underft_mf,
                type="scatter", mode = 'lines+markers', 
                line = list(color = greycolors[5]),
                marker = list(color = greycolors[5]))%>%
            layout(
                title = "", 
                shapes = list(hline(100), vline(2021)),
                yaxis = list(title = "Ratio",  
                             range=c(0, 150),
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023)),
                showlegend = FALSE
            )
    })
    
    output$cplot_num_student <- renderPlotly({    
        
        dtafig<-dta%>%filter(college==input$college_tab5) 
        
        dtafig%>%
            plot_ly(x = ~year, 
                    type="scatter", mode = 'lines+markers', 
                    y = ~numenroll_total, 
                    name="Enrolled freshmen, total",
                    line = list(color = bluecolors[5]),
                    marker = list(color = bluecolors[5]))%>%        
            add_trace(y = ~num_under, 
                      name="Undergraduate students, total",
                      line = list(color = bluecolors[3]),
                      marker = list(color = bluecolors[3]))%>%        
            add_trace(y = ~num_graduate, 
                      name="Graduate students, total",
                      line = list(color = redcolors[5]),
                      marker = list(color = redcolors[5]))%>%       
            layout(
                title = "", 
                shapes = list(vline(2021)),
                yaxis = list(title = "Number of students",  
                             range=c(0, max(dtafig$num_total)), 
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023)),
                legend = list(orientation = "v", font = list(size=9), 
                              xanchor = "left", x = 0.05, 
                              xanchor = "center", y = 0.5 )
            )
    })    
    
    output$cplot_admit_trend_detail <- renderPlotly({    
        
        dtafig<-dta%>%filter(college==input$college_tab5) 
        
        dtafig%>%
            plot_ly(
                x = ~year, 
                type="scatter", mode = 'lines+markers', 
                y = ~pct_admit_total, 
                name = "Total",
                line = list(color = greycolors[5]),
                marker = list(color = greycolors[5]))%>%
            add_trace(y = ~pct_admit_female, 
                      name="Female",
                      line = list(color = redcolors[5]),
                      marker = list(color = redcolors[5]))%>%
            add_trace(y = ~pct_admit_male, 
                      name="male",
                      line = list(color = greencolors[5]),
                      marker = list(color = greencolors[5]))%>%      
            add_trace(y = ~pct_admit_ed, 
                      name="Early decision",
                      line = list(color = purplecolors[5]),
                      marker = list(color = purplecolors[5]))%>%             
            layout(
                title = "", 
                shapes = list(vline(2021)),
                yaxis = list(title = "(%)",
                             range=c(0, 100), 
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023)),
                legend = list(orientation = "v", font = list(size=9), 
                              xanchor = "left", x = 0.05)
                #legend = list(orientation = "h",   # show entries horizontally
                #              xanchor = "center", x = 0.5)
            )                
    })      

    output$cplot_ed_among_enroll <- renderPlotly({
        
        dtafig<-dta%>%filter(college==input$college_tab5) 
        
        dtafig%>%
            plot_ly(
                x = ~year, y = ~pct_ed_among_enroll,
                type="scatter", mode = 'lines+markers', 
                line = list(color = purplecolors[5]),
                marker = list(color = purplecolors[5]),
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Year: ", dtafig$year, 
                    "<br>ED among enrolled: ", dtafig$pct_ed_among_enroll, " (%)"))%>%
            layout(
                title = "", showlegend = FALSE, 
                shapes = list(hline(50), vline(2021)),
                yaxis = list(title = "Percent of enrolled students",  
                             range=c(0, 100), 
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023))
            )
    })
    
    output$cplot_relnum_trend <- renderPlotly({    
        
        dtafig<-dta%>%filter(college==input$college_tab5) 
        
        dtafig%>%
            plot_ly(
                x = ~year, 
                type="scatter", mode = 'lines+markers', 
                y = ~relnumapp_total, 
                name = "Applications, total",
                line = list(color = greencolors[6]),
                marker = list(color = greencolors[6]))%>%
            add_trace(y = ~relnumapp_ed, 
                      name="Applications, early decision",
                      line = list(color = purplecolors[5]),
                      marker = list(color = purplecolors[5]))%>%
            add_trace(y = ~relnumadmit_total, 
                      name="Accepted",
                      line = list(color = redcolors[5]),
                      marker = list(color = redcolors[5]))%>%
            add_trace(y = ~relnumenroll_total, 
                      name="Enrolled",
                      line = list(color = bluecolors[5]),
                      marker = list(color = bluecolors[5]))%>%    
            layout(
                title = "", 
                shapes = list(vline(2021), hline(1)),
                yaxis = list(title = "Relative change (reference = 2022)",
                             showgrid = FALSE, showticklabels = TRUE,
                             range=c(0, 2.5)), 
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023)),
                legend = list(orientation = "v", font = list(size=9), 
                              xanchor = "left", x = 0.05)
            )
    })          

    output$cplot_satsubmit <- renderPlotly({
        
        dtafig<-dta%>%filter(college==input$college_tab5) 
        
        dtafig%>%
            plot_ly(
                x = ~year, y = ~pct_submitsat, 
                type="scatter", mode = 'lines+markers', 
                line = list(color = 'darkgray'),
                marker = list(color = 'darkgray'))%>%
            add_trace(
                y = ~pct_submitsatoptional, 
                line = list(color = 'lightgray'),
                marker = list(color = 'lightgray'))%>%
            layout(
                title = "", 
                shapes = list(vline(2021)),
                yaxis = list(title = "Percent of enrolled students",  
                             range=c(0,100), 
                             showgrid = FALSE, 
                             showticklabels = TRUE, tickfont = list(size=9)),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023)),
                showlegend = FALSE
            )
    })

    output$cplot_satscores <- renderPlotly({    
        
        dtafig<-dta%>%filter(college==input$college_tab5) 
        
        dtafig%>%        
            plot_ly(
                x = ~year, 
                type="scatter", mode = 'lines+markers', 
                y = ~sat75, 
                name = "75th percentile", 
                line = list(color = orangecolors[5]),
                marker = list(color = orangecolors[5]))%>%
            add_trace(
                y = ~sat75optional, 
                name = " ", 
                line = list(color = '#fee090'),
                marker = list(color = '#fee090'))%>%                   
            add_trace(
                y = ~sat25, 
                name = "25th percentile", 
                line = list(color = bluecolors[5]),
                marker = list(color = bluecolors[5]))%>%    
            add_trace(
                y = ~sat25optional, 
                name = " ", 
                line = list(color = '#e0f3f8'),
                marker = list(color = '#e0f3f8'))%>%          
            layout(
                title = "", 
                shapes = list(vline(2021), hline(1400)),
                yaxis = list(title = "SAT score",  
                             range=c(1100, 1600), 
                             showgrid = FALSE, 
                             showticklabels = TRUE, tickfont = list(size=9)),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023)),
                legend = list(orientation = "v", font = list(size=9), 
                              xanchor = "right", x = 0.95,
                              yanchor = "bottom", y = 0.05)
            )
    })       

    output$cplot_top10th <- renderPlotly({    
        
        dtafig<-dta%>%filter(college==input$college_tab5) 
        
        dtafig%>%
            plot_ly(
                x = ~year, y = ~pct_top10pct, 
                type="scatter", mode = 'lines+markers', 
                name="pct_top10pct",
                line = list(color = greycolors[5]),
                marker = list(color = greycolors[5]))%>%
            layout(
                title = "", 
                shapes = list(vline(2021), hline(80)),
                yaxis = list(title = "Percent of enrolled students",  
                             range=c(0,100), 
                             showgrid = FALSE, 
                             showticklabels = TRUE, tickfont = list(size=9)),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023)),
                showlegend = FALSE
            )
    })    

    output$cplot_numtransapp_total <- renderPlotly({
        
        dtafig<-dta%>%filter(college==input$college_tab5) 
        
        dtafig%>%
            plot_ly(
                x = ~year, y = ~numtransapp_total,
                type="scatter", mode = 'lines+markers', 
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Year: ", dtafig$year, 
                    "<br>Number of applications: ", dtafig$numtransapp_total),    
                line = list(color = greycolors[5]),
                marker = list(color = greycolors[5]))%>%
            layout(
                title = "", showlegend = FALSE,
                yaxis = list(title = "Ratio (reference = value in the latest year)",  
                             range=c(0, 3), 
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                shapes = list(hline(1), vline(2021)),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023))
            )
    })
    
    output$cplot_numtransadmit_total <- renderPlotly({
        
        dtafig<-dta%>%filter(college==input$college_tab5) 
        
        dtafig%>%
            plot_ly(
                x = ~year, y = ~numtransadmit_total,
                type="scatter", mode = 'lines+markers', 
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Year: ", dtafig$year, 
                    "<br>Number of admitted students: ", dtafig$numtransadmit_total),     
                line = list(color = greycolors[5]),
                marker = list(color = greycolors[5]))%>%
            layout(
                title = "", showlegend = FALSE,
                yaxis = list(title = "Ratio (reference = value in the latest year)",  
                             range=c(0, 3), 
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                shapes = list(hline(1), vline(2021)),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023))
            )
    })
    
    output$cplot_numtransenroll_total <- renderPlotly({
        
        dtafig<-dta%>%filter(college==input$college_tab5) 
        
        dtafig%>%
            plot_ly(
                x = ~year, y = ~numtransenroll_total,
                type="scatter", mode = 'lines+markers', 
                hovertemplate = paste0(
                    dtafig$college,       
                    "<br>Year: ", dtafig$year, 
                    "<br>Number of enrolled students: ", dtafig$numtransenroll_total),     
                line = list(color = greycolors[5]),
                marker = list(color = greycolors[5]))%>%
            layout(
                title = "", showlegend = FALSE,
                yaxis = list(title = "Ratio (reference = value in the latest year)",  
                             range=c(0, 3), 
                             showgrid = FALSE, 
                             showticklabels = TRUE),
                shapes = list(hline(1), vline(2021)),
                xaxis = list(title = "Year", showgrid = FALSE , range=c(2016, 2023))
            )
    })
    
    ##### output: Annex #####
    
    output$table_list <- renderTable({
        
        temp<-dta%>%filter(year==yearlatest)%>%
            select(college, 
                   tiny, small, medium, large, huge, 
                   pct_admit_reach, pct_admit_target, pct_admit_safety, 
                   open, public, women)%>%
            filter(is.na(tiny)==FALSE)%>%
            arrange(college)
            
        
        names(temp) <- toupper(names(temp))
        
        temp<-temp%>%
            mutate_at(colnames(temp)[2: ncol(temp)], 
                      funs(as.character(.)) )%>%
            mutate_at(colnames(temp)[2: ncol(temp)], 
                      funs(ifelse(.=="1" | .=="TRUE" , "Yes", 
                                  ifelse(.=="0" | .=="FALSE" , "", .))))
        
    })         
    
    #https://rstudio.github.io/DT/options.html
    output$table_list_simple <- renderDataTable({
        
        temp<-dta%>%filter(year==yearlatest)%>%
            select(college, latest_num_under, 
                   groupsize, groupadmit, open, public, women)%>%
            filter(is.na(groupsize)==FALSE)%>%
            rename(
                undergraduate.size = latest_num_under, 
                admission.rate = groupadmit, 
                size.group = groupsize       )%>%
            mutate(
                admission.rate = ifelse(admission.rate=="reach", "Below 20%",
                                        ifelse(admission.rate=="target", "20-50%",
                                               ifelse(admission.rate=="safety", "50% or higher",
                                                      "-"))))%>%
            arrange(college)
        
        names(temp) <- toupper(names(temp))
        
        temp<-temp%>%
            mutate_at(colnames(temp)[5: ncol(temp)], 
                      funs(as.character(.)) )%>%
            mutate_at(colnames(temp)[5: ncol(temp)], 
                      funs(ifelse(.=="1" | .=="TRUE" , "Yes", 
                                  ifelse(.=="0" | .=="FALSE" , "", .))))
        
        datatable(head(temp, nrow(temp)),
                  rownames = FALSE, 
                  options = list(
                      columnDefs = list(list(className = 'dt-center', 
                                             targets = 1:5)), 
                      order = list(list(0, 'asc')),  
                      pageLength = 20,
                      lengthMenu = c(20, 40, 60, 80, 100)
                  )
        )
        
    })             
    
}    
    
##### 3. CREATE APP #####

#******************************
# 3. CREATE APP 
#******************************

shinyApp(ui, server)