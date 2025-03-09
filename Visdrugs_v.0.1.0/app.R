library(shiny)
library(bslib)
library(stringr)
library(stringi)
library(shinycssloaders)
library(shinyjs)
library(plotly)
library(echarts4r)
library(DT)
# library(random)
source("makefigures.R")
source("run.R")
# install.packages("shinycssloaders",dependencies = T)


# Define UI ----
my_theme <- bs_theme(bootswatch="cerulean")
# 定义 UI
ui <- fluidPage(
  #loading---------------------------
  useShinyjs(),  # 启用 shinyjs
  
  # 自定义CSS样式
  tags$head(
    tags$style(HTML("
                    body {
        min-width: 1500px;
      }
      .container-fluid {
        max-width: 1500px;
        margin: 0 auto;
      }
      /* 全屏遮罩层样式 */
      #loading-overlay {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background-color: rgba(255, 255, 255, 0.9);
        z-index: 10000;
        display: flex;
        justify-content: center;
        align-items: center;
        flex-direction: column;
      }
        /* 提示文本样式 */
        .loading-text {
         font-size: 18px;
          color: #333;
          margin-bottom: 20px;
          text-align: center;
          font-weight: bold;
       }
      /* 进度条容器样式 */
      .progress-container {
        width: 50%;
        height: 30px;
        background-color: #f3f3f3;
        border: 2px solid #ddd;
        border-radius: 5px;
        overflow: hidden;
      }
      /* 进度条样式 */
      .progress-bar {
        height: 100%;
        background-color: #4caf50;
        width: 0;
        text-align: center;
        line-height: 30px;
        color: white;
        font-weight: bold;
      }
      /* 完成文本样式 */
      .complete-text {
        display: none;
        margin-top: 20px;
        font-size: 20px;
        font-weight: bold;
        color: #4caf50;
      }
    "))
  ),
  
  # 加载遮罩层
  div(id = "loading-overlay",
      div(class = "loading-text",
               span("Due to the large dataset, loading may take some time. Please be patient, Thanks!"),
              br(),             
                span("数据集较大，载入需要些时间，请耐心等待，感谢您的使用！")
      ),
      div(class = "progress-container",
          div(id = "progress-bar", class = "progress-bar", "0%")  # 进度条
      ),
      div(id = "complete-text", class = "complete-text", "Loading Complete!")  # 完成文本
  ),
  
  
  #main progranm---------------------------------------
  theme = my_theme,

  # 标题栏
  navbarPage(
    title = div(
      img(src = paste0("LOGO/SCT LOGO2.png?", Sys.time()), height = "80px", style = "vertical-align: middle;"),
      "FAERS Analysis", style = "display: inline-block; vertical-align: middle;"
    ),
  #1st tab----------------------------------
    tabPanel("Most Potential Reactions of Target Drugs",
             fluidRow(
               column(12,
                      div(style = "display: flex; align-items: center;",#add red *
                        tags$span(style ="color: red; margin-right: 5px;", "*"),
                        selectizeInput(
                        inputId = "Drugs_mpt1",
                        label = "Choose Drugs to Show Their Most Potential Reactions:",
                        choices = indexdrug_prof,
                        selected = NULL,
                        multiple = TRUE,
                        options = list(
                          placeholder = 'Start typing to search...',
                          maxOptions = 10,
                          create = FALSE  # 禁止用户输入自定义选项
                        ),
                        width = "800px"
                      ),
                      tags$span(style = "margin-right: 15px;"),
                      tags$a(href = "INFO/INFO_EN.htm",
                             "Click here for user guide",
                             target = "_blank",style="margin-top: 10px;")
                      ),
                      div(style = "display: flex; align-items: center;",#add red *
                          tags$span(style ="color: white; margin-right: 5px;", "*"),
                      textInput("druggroupname1", "Give Chosen drugs a name:",width = "800px"),
                      tags$span(style = "margin-right: 20px;"),
                      tags$a(href = "INFO/INFO_CN.htm",
                             "中文版使用说明请点这里",
                             target = "_blank",style="margin-top: 10px;")
                      ),
                      tags$span(style = "margin-right: 10px;"),
                      actionButton("submit1", "Submit"),
                      # downloadButton("downloadImage_pie","Download Pie Plot"),
                      downloadButton("downloaddata_pie","Download Pie Data"),
                      downloadButton("downloadImage_forest","Download Forest Plot"),
                      downloadButton("downloaddata_forest","Download Forest Data")
               )
             ),
             tags$hr(),
             fluidRow(
               column(12,align = "center",
               # textOutput("select_drugs"),
               withSpinner(echarts4rOutput("sh_pie")),
               br(),
               withSpinner(DTOutput("dataTable")),
               br(),
               withSpinner(uiOutput("drug_mpt_forest_plot"))
               # withSpinner(uiOutput("drug_mpt_pie_plot")) 
             )
           )
    ), 
  #2nd tab----------------------------------
  tabPanel("Reaction Comparison Between Drugs",
           fluidRow(
             column(12,
                    div(style = "display: flex; align-items: center;",#add red *
                        tags$span(style ="color: red; margin-right: 5px;", "*"),
                    selectizeInput(
                      inputId = "Drugs_dvd1",
                      label = "Target Drugs:",
                      choices = indexdrug_prof,
                      selected = NULL,
                      multiple = TRUE,
                      options = list(
                        placeholder = 'Start typing to search drugs...',
                        maxOptions = 10,
                        create = FALSE  # 禁止用户输入自定义选项
                      ),
                      width = "800px"
                    ),
                    tags$span(style = "margin-right: 15px;"),
                    tags$a(href = "INFO/INFO_EN.htm",
                           "Click here for user guide",
                           target = "_blank",style="margin-top: 10px;")
                    ),
                    div(style = "display: flex; align-items: center;",#add red *
                        tags$span(style ="color: red; margin-right: 5px;", "*"),
                    selectizeInput(
                      inputId = "Drugs_dvd2",
                      label = "Control Drugs:",
                      choices = c(indexdrug_prof,"OTHER DRUGS"),
                      selected = "OTHER DRUGS",
                      multiple = TRUE,
                      options = list(
                        placeholder = 'Start typing to search drugs...',
                        maxOptions = 10,
                        create = FALSE  # 禁止用户输入自定义选项
                      ),
                      width = "800px"
                    )),
                    div(style = "display: flex; align-items: center;",#add red *
                        tags$span(style ="color: red; margin-right: 5px;", "*"),
                    selectizeInput(
                      inputId = "targetreac",
                      label = "Target Reactions:",
                      choices = indexpt_prof,
                      selected = NULL,
                      multiple = TRUE,
                      options = list(
                        placeholder = 'Start typing to search reactions...',
                        maxOptions = 10,
                        create = FALSE  # 禁止用户输入自定义选项
                      ),
                      width = "800px"
                    ),
                    tags$span(style = "margin-right: 20px;"),
                    tags$a(href = "INFO/INFO_CN.htm",
                           "中文版使用说明请点这里",
                           target = "_blank",style="margin-top: 10px;")
                    ),
                    radioButtons(
                      inputId = "subgroup",
                      label = "Whether use subgroup?",
                      choices = list("By Age" = "age", "By Gender" = "gender", "No" = "no"),
                      selected = "no"
                    ),
                    conditionalPanel(
                      condition = "input.subgroup == 'age'",
                      radioButtons(
                        inputId = "age_unit",
                        label = "Select age unit:",
                        choices = list("Year" = "year", "Day" = "day", "Hour" = "hour"),
                        selected = NULL
                      )
                    ),
                    conditionalPanel(
                      condition = "input.subgroup == 'age' && input.age_unit != null",
                      div(style = "display: flex; align-items: center;",#add red *
                          tags$span(style ="color: red; margin-right: 5px;", "*"),
                      numericInput(
                        inputId = "age_value",
                        label = "Enter age value:",
                        value = 0,
                        min = 0
                      ))
                    ),
                    div(style = "display: flex; align-items: center;",#add red *
                        tags$span(style ="color: white; margin-right: 5px;", "*"),
                    textInput("druggroupname_dvd_1", "Give Target Group a Name:")),
                    div(style = "display: flex; align-items: center;",#add red *
                        tags$span(style ="color: white; margin-right: 5px;", "*"),
                    textInput("druggroupname_dvd_2", "Give Control Group a Name:")),
                    actionButton("submit2", "Submit"),
                    downloadButton("DL_dvd_forest_plot","Download Forest Plot"),
                    downloadButton("DL_dvd_forest_data","Download Forest Data"),
                    downloadButton("DL_dvd_areac_data","Download Target Drugs' All Reaction Data")
             )
           ),
           tags$hr(),
           fluidRow(
             column(12,align = "center",
                    # textOutput("select_drugs2"),
                    withSpinner(uiOutput("drug_dvd_forest_plot")),
                    br(),
                    withSpinner(DTOutput("dataTable2"))
             )
           )
  )
  )

)




# Define server logic ----
server <- function(input, output,session) {

  # 数据加载完成后，隐藏加载遮罩层并显示主内容------------------
  # 模拟数据预加载并更新进度条
  for (i in 1:100) {
    Sys.sleep(0.03)  # 模拟数据加载的时间
    # 更新进度条的宽度和百分比
    shinyjs::runjs(sprintf("$('#progress-bar').css('width', '%d%%').text('%d%%');", i, i))
  }
  
  # 显示“加载完成”文本
  shinyjs::runjs("$('#complete-text').show();")
  
  # 短暂延迟后隐藏加载遮罩层并显示主内容
  shinyjs::delay(1000, {
    shinyjs::runjs("$('#loading-overlay').hide();")
  })
  
  
  USRID<-stri_rand_strings(1, 10, pattern = "[A-Za-z0-9]")
  #1st server--------------------------------
  
  # output$select_drugs <- renderText({
  #   paste("Most Potential Reactions for", input$druggroupname1)
  # })
  #按按钮才进行计算
  reactive_sh_pie <- eventReactive(input$submit1, {
    fdapieplot(drugs1=input$Drugs_mpt1,druggroupname1=input$druggroupname1,USRID=USRID)
  })
  
  reactive_img_path_pie <- eventReactive(input$submit1, {
    paste("temp/",USRID,"_pie2.png?", Sys.time(),sep = "")
    })
  
  reactive_img_path_forest <- eventReactive(input$submit1, {
  paste("temp/",USRID,"_mpt_forest2.png?", Sys.time(),sep = "")
  })
  
  reactive_name <- eventReactive(input$submit1, {
    input$druggroupname1
  })
  
  # 可交互饼图
  output$sh_pie <- renderEcharts4r({
    reactive_sh_pie() %>%
      e_charts(Reaction) %>%
      e_pie(Frequency, radius = "50%") %>%
      e_title(paste("Reaction Distribution of", reactive_name()),left = "center") %>%
      e_tooltip(trigger = "item", 
                formatter = htmlwidgets::JS("
                  function(params) {
                    return params.name + ': ' + params.value + ' occurrences';
                  }
                ")) %>%
      e_legend(orient = "vertical", right = "10%")%>%
      e_on(
        list(seriesName = "Frequency"),
        htmlwidgets::JS("
          function(params) {
            Shiny.setInputValue('pieChart_click', params.dataIndex, {priority: 'event'});
          }
        ")
      )
  })
  # 可交互表格
  output$dataTable <- renderDT({
    req(input$pieChart_click)  # 等待点击事件
    res<-reactive_sh_pie()[,1:3]
    res$Percentage<-round(res$Percentage,2)%>%paste0("%")
    rownames(res)<-1:nrow(res)
    res# +1 由于数据索引从0开始
  }, options = list(pageLength = 5))
  
  # 渲染 UI 输出
  output$drug_mpt_pie_plot <- renderUI({
    img_path_pie <- reactive_img_path_pie()
    tags$img(src = img_path_pie)
  })
  
  output$drug_mpt_forest_plot <- renderUI({
    img_path_for <- reactive_img_path_forest()
    tags$img(src = img_path_for)
  })
  
  output$downloadImage_pie<-downloadHandler(
    filename = function() {
      paste("high_res_image for ",input$druggroupname1," ", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      file.copy(paste("WWW/temp/",USRID,"_pie.png",sep=""), file)
    }
  )
  
  output$downloadImage_forest<-downloadHandler(
    filename = function() {
      paste("high_res_image for ",input$druggroupname1," ", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      file.copy(paste("WWW/temp/",USRID,"_mpt_forest.png",sep=""), file)
    }
  )

  output$downloaddata_forest<-downloadHandler(
    filename = function() {
      paste("Forest DATA for ",input$druggroupname1," ", Sys.Date(), ".xls", sep = "")
    },
    content = function(file) {
      file.copy(paste("WWW/temp/",USRID,"_mpt_forest.xls",sep=""), file)
    }
  )
  
  output$downloaddata_pie<-downloadHandler(
    filename = function() {
      paste("Pie DATA for ",input$druggroupname1," ", Sys.Date(), ".xls", sep = "")
    },
    content = function(file) {
      file.copy(paste("WWW/temp/",USRID,"_pie.xls",sep=""), file)
    }
  )
  #2nd server--------------------------------
  # output$select_drugs2 <- renderText({
  #   paste("Reactions for", input$druggroupname_dvd_1,"v.s." ,input$druggroupname_dvd_2)
  # })
  #按按钮才进行计算
  reactive_img_path_dvd_forest <- eventReactive(input$submit2, {
    forest_data<-ffplot_dvd(drugs1=input$Drugs_dvd1, drugs2=input$Drugs_dvd2, USRID=USRID,
                    targetreac=input$targetreac, druggroupname1=input$druggroupname_dvd_1,
                    druggroupname2=input$druggroupname_dvd_2, subgroup=input$subgroup,age_unit=input$age_unit,age=input$age_value)
    forest_path<-paste("temp/",USRID,"_dvd_forest2.png?", Sys.time(),sep = "")
    return(list(forest_data=forest_data,forest_path=forest_path))
  })
  
  # 渲染 UI 输出

  output$drug_dvd_forest_plot <- renderUI({
    img_path_for <- reactive_img_path_dvd_forest()$forest_path
    tags$img(src = img_path_for)
  })
  
  output$dataTable2 <- renderDT({
    res<-reactive_img_path_dvd_forest()$forest_data
    rownames(res)<-1:nrow(res)
    res# +1 由于数据索引从0开始
    
  }, options = list(pageLength = 5))
  
  output$DL_dvd_forest_plot<-downloadHandler(
    filename = function() {
      paste("high_res_image for ",input$druggroupname1," ", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      file.copy(paste("WWW/temp/",USRID,"_dvd_forest.png",sep=""), file)
    }
  )
  
  output$DL_dvd_forest_data<-downloadHandler(
    filename = function() {
      paste("high_res_image for ",input$druggroupname1," ", Sys.Date(), ".xls", sep = "")
    },
    content = function(file) {
      file.copy(paste("WWW/temp/",USRID,"_dvd_forest.xls",sep=""), file)
    }
  )
  
  output$DL_dvd_areac_data<-downloadHandler(
    filename = function() {
      paste("Forest DATA for ",input$druggroupname1," ", Sys.Date(), ".xls", sep = "")
    },
    content = function(file) {
      file.copy(paste("WWW/temp/",USRID,"_dvd_allreac.xls",sep=""), file)
    }
  )
  #允许自动重连
  session$allowReconnect(TRUE)
  # 当会话结束时删除图片文件
  session$onSessionEnded(function() {
    image_path<-c(paste("WWW/temp/",USRID,"_pie.png",sep=""),
                  paste("WWW/temp/",USRID,"_pie2.png",sep=""),
                  paste("WWW/temp/",USRID,"_pie.xls",sep=""),
                  paste("WWW/temp/",USRID,"_mpt_forest.png",sep=""),
                  paste("WWW/temp/",USRID,"_mpt_forest2.png",sep=""),
                  paste("WWW/temp/",USRID,"_mpt_forest.xls",sep=""),
                  paste("WWW/temp/",USRID,"_dvd_forest.png",sep=""),
                  paste("WWW/temp/",USRID,"_dvd_forest2.png",sep=""),
                  paste("WWW/temp/",USRID,"_dvd_forest.xls",sep=""),
                  paste("WWW/temp/",USRID,"_dvd_allreac.xls",sep=""))
    lapply(image_path, function(x){
      if (file.exists(x)) {
        file.remove(x)
        message("Deleted file: ", x)
      }  
    })
  })
}
# Run the app ----
shinyApp(ui = ui, server = server)