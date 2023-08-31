#' Shiny app for use with MCOTEA Analysis Sychronization Matrix (ASM)
#'
#' \code{asm_app} provides a Shiny application for importing and viewing data
#'   from the MCOTEA ASM datFormTable.xlsx and qryProgramsMilestoneInfo.xlsx output.
#'
#' @return A Shiny Application.
#'
#' @seealso 
#'
#' @examples
#' # asm_app(
#' #   filedir = "directory containing datFormProgram.xlsx and qryProgramsMilestoneInfo.xlsx",
#' #   launch.browser = TRUE
#' # )
#'
#' @export
# Run the application 
asm_app <- function(filedir, launch.browser = TRUE){

#library(shiny)
#library(shinyFiles)
#library(shinyjs)
#library(jsonlite)
#library(DT)
#library(data.table)
#library(readxl)
#library(ggplot2)
#library(plotly)
#library(mcotear)

fp1 <- file.path(filedir, "datFormProgram.xlsx")
fp2 <- file.path(filedir, "qryProgramsMilestonesInfo.xlsx")

f1 <- read_xlsx(
  fp1,
  sheet = 1,
  na = c("NA", "N/A", "", "na", "Na", "n/a", "N/a", "NaN", "nA"))

f2 <- read_xlsx(
  fp2,
  sheet = 1,
  na = c("NA", "N/A", "", "na", "Na", "n/a", "N/a", "NaN", "nA"))

# Make start date a Sunday
start_date_sunday <- function(x){
  x <- as.Date(x)
  if(weekdays(x) == "Sunday"){ NULL } else
    if(weekdays(x) == "Monday"){ x <- x - 1} else
      if(weekdays(x) == "Tuesday"){ x <- x - 2 } else
        if(weekdays(x) == "Wednesday"){ x <- x - 3 } else
          if(weekdays(x) == "Thursday"){ x <- x - 4 } else
            if(weekdays(x) == "Friday"){ x <- x - 5 } else
              if(weekdays(x) == "Saturday"){ x <- x - 6 }
  return(x)
}

# Make stop date a Sunday
stop_date_sunday <- function(x){
  x <- as.Date(x)
  if(weekdays(x) == "Sunday"){ NULL } else
    if(weekdays(x) == "Monday"){ x <- x + 6} else
      if(weekdays(x) == "Tuesday"){ x <- x + 5 } else
        if(weekdays(x) == "Wednesday"){ x <- x + 4 } else
          if(weekdays(x) == "Thursday"){ x <- x + 3 } else
            if(weekdays(x) == "Friday"){ x <- x + 2 } else
              if(weekdays(x) == "Saturday"){ x <- x + 1 }
  return(x)
}


# See 24 months in the future, but start on the first date of current month
# and go to to the end of the 24th month
start_stop_month <- function(x, months){
  months <- paste(months, "months")
  start.date <- as.Date(trunc(as.POSIXlt(x), "month"))
  stop.date <- seq(start.date, length = 2, by = months)[2]
  stop.date <- as.Date(trunc(as.POSIXlt(stop.date), "month"))
  return(list(start = start.date, stop = stop.date))
}
# start_stop_month(x = Sys.Date(), months = 1)
# start_stop_month(x = Sys.Date(), months = 2)
# start_stop_month(x = Sys.Date(), months = 24)


# Identify programs in dt_ss that are missing from dt
dt_add_miss_progs <- function(x = dt_ss, y = dt){
  # identify programs missing from y
  missing_programs <- unique(x[, Program])[
    !(unique(x[, Program]) %in% unique(y[, Program]))
  ]

  # add missing programs into dt so their
  # vertical plotting position is accounted for
  dt <- rbindlist(
    list(
      y,
      x[Program %in% missing_programs]
    ), fill = TRUE
  )
  return(dt)
}


# So programs are sorted based on earliest review date
dt_early_order <- function(dt){
  # So programs are sorted based on earliest review date
  # Find the earliest review date per program
  dt[, Earliest := min(Date), by = Program]
  
  # Order the earliest review dates per program
  ord <- unique(dt[, .(Earliest, Program)])[, .(Order = order(Earliest), Program)]
  
  # Reverse Order
  ord[, ReverseOrd := (max(Order)+1)-Order]
  
  # Expand so each program takes up 2 units on the y-axis and is centered on
  # an odd value
  ord[, ReverseOrd := (ReverseOrd*2) - 1]
  
  # Merge Order and ReverseOrd into dt
  dt <- merge(dt, ord, by = "Program")
  return(dt)
}


# Create the shift within a program row
dt_pswg_prog_row_shift <- function(dt){
  (progs <- dt[, .N, Program])
  for(i in 1:nrow(progs)){
    N <- progs[i, N]
    if(N < 4){
      dt[
        Program == progs[i, Program],
        Shift := rev(seq( -(N-1)/N, (N-1)/N, length.out = N))
      ]
    } else {
      dt[
        Program == progs[i, Program],
        Shift := rev(rep(seq( -(3-1)/3, (3-1)/3, length.out = 3), times = ceiling(N/3)))[1:N]
      ]
    }
  }
  dt[, ReverseOrdShift := ReverseOrd + Shift]
  return(dt)
}


# Shift steps so steps do not overlap
dt_step_shift <- function(dt){
  shifts <- seq( 1/6, 2+1/6, length.out = 7)-1
  dt[Step == "Evaluation Planning", Shift := shifts[6]]
  dt[Step == "Concept Planning", Shift := shifts[5]]
  dt[Step == "Detailed Planning", Shift := shifts[4]]
  dt[Step == "Event Execution", Shift := shifts[3]]
  dt[Step == "Test Reporting", Shift := shifts[2]]
  dt[Step == "Evaluation Reporting", Shift := shifts[1]]
  dt[, ReverseOrdShift := ReverseOrd + Shift]
  return(dt)
}





plot_fun_pswg <- function(dt, start = Sys.Date(), days = 90){
  
  today <- Sys.Date()

  start.date <- start_date_sunday(x = start)
  stop.date <- stop_date_sunday(x = start.date + days)
  
  # Base plot
  my_plot <- dt %>%
    ggplot()
  
  # color every other row to help indicate distinctions between programs
  my_seq <- seq(dt[,min(ReverseOrd)]-1, dt[, max(ReverseOrd)]-1, by = 2)
  for(i in seq_along(my_seq)){
    my_plot <- my_plot + geom_ribbon(
      data = data.frame(
        x = c(start.date-7*10, stop.date+7*10),
        ymin = c(my_seq[i],my_seq[i]),
        ymax = c(my_seq[i]+2,my_seq[i]+2)
      ),
      aes(x = x, ymin = ymin, ymax = ymax),
      fill = ifelse(i %% 2 == 0, "grey70", "transparent"),
      alpha = ifelse(i %% 2 == 0, .2, 0)
    )
  }
  
  
  keep <- c("CSSD", "ED", "GD", "MD", "LFD", "CD") %in% dt[, .N, Division][, Division]
  #keep <- c("CSSD", "ED", "GD", "MD") %in% dt[, .N, Division][, Division]
  
  # Add review points
  my_plot <- my_plot +
    geom_point(aes(x = Date, y = ReverseOrdShift, colour = Division,
                   text = paste(
                     'Division:', Division,
                     '</br></br>Lead OTA:', LeadOTA,
                     '</br>Program:', Program,
                     '</br>Product:', Product,
                     '</br>Review:', Review,
                     '</br>Date:', Date,
                     '</br>OTPO:', OTPO,
                     '</br>ORSA:', ORSA,
                     '</br>MS:', MS,
                     '</br>TM:', TM,
                     '</br>DM:', DM,
                     '</br>Cy:', Cy,
                     '</br>LF:', LF
                     )
                   )
               )
  
  # Add color by Division
  my_plot <- my_plot +
    scale_colour_manual(
      # name = paste(
      #   paste0("Total Reviews = ", dt[, .N]),
      #   "\nDivision\n(# Programs, # Reviews)", sep = "\n"
      # )
      name = paste(
        paste0("Total Reviews = ", dt[, .N]),
        "\nDivision (# Programs, # Reviews)",
        
        paste(
          merge(
            dt[, .N, keyby = .(Division,Program)][, .(Reviews = sum(N)),Division],
            dt[, .N, keyby = .(Division,Program)][, .(Programs = .N),Division]
          )[, paste0(Division, " (", Programs, ", ", Reviews, ")\n")],
          collapse = ""
        ),    
        sep = "\n"
      ),
      values = c("CSSD" = cbbPalette[1], "ED" = cbbPalette[3],
                 "GD" = cbbPalette[4], "MD" = cbbPalette[2],
                 "LF" = cbbPalette[8], "CD" = cbbPalette[7])[keep], #
      #labels = merge(
      #  dt[, .N, keyby = .(Division,Program)][, .(Reviews = sum(N)),Division],
      #  dt[, .N, keyby = .(Division,Program)][, .(Programs = .N),Division]
      #)[, paste0(Division, " (", Programs, ", ", Reviews, ")\n")]
    )
  
  # Add text label
  my_plot <- my_plot +
    geom_text(
      aes(x = Date, y = ReverseOrdShift, label = Label),
      hjust = 0.0, size = 2.5, nudge_x = .5
    )
    
  # Add labels
  my_plot <- my_plot + labs(
      y = "Program", x = "", #"Date",
      title = paste0("Number of Programs = ", dt[,.N,Program][,.N])
    )
  
  # Add x-axis
  my_plot <- my_plot +
    scale_x_date(
      breaks = seq(start.date-7*10, stop.date+7*10, by = "week"),
      date_labels = "%d\n%b",
      expand = c(0,0)
    )
  
  # Add theme
  my_plot <- my_plot +
    theme(
      panel.grid.minor = element_blank(),
      plot.caption = element_text(colour = "darkgreen", size = 10),
      legend.title.align = 0.5,
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8),
      panel.grid.major.y = element_blank(),
      plot.title = element_text(size = 10)
    )
  
  # Add y-axis
  my_plot <- my_plot +
    scale_y_continuous(
      breaks = sort(unique(dt[, ReverseOrd])),
      labels = (unique(dt[, Program, keyby = ReverseOrd])[,Program]),
      expand = c(0.01,0.01)
    )
  
  my_plot
}






plot_fun_cub <- function(dt, dt_ss, dt_prog, start = Sys.Date(), days = 90, add_steps = TRUE){
  
  today <- Sys.Date()

  start.date <- start_date_sunday(x = start)
  stop.date <- stop_date_sunday(x = start.date + days)
  
  # Base plot
  my_plot <- dt %>%
    ggplot()
  
  # color every other row to help indicate distinctions between programs
  my_seq <- seq(dt[, min(ReverseOrd)]-1, dt[, max(ReverseOrd)]-1, by = 2)
  for(i in seq_along(my_seq)){
    my_plot <- my_plot + geom_ribbon(
      data = data.frame(
        # x = c(start.date, stop.date+14),
        x = c(start.date-7*10, stop.date+7*10),
        ymin = c(my_seq[i],my_seq[i]),
        ymax = c(my_seq[i]+2,my_seq[i]+2)
      ),
      aes(x = x, ymin = ymin, ymax = ymax),
      fill = ifelse(i %% 2 == 0, "grey70", "transparent"),
      alpha = ifelse(i %% 2 == 0, .2, 0)
    )
  }
  
  # Add labels
  my_plot <- my_plot +
    labs(
      y = "Program", x = "", #"Date",
      title = paste0("Number of Programs = ", dt[,.N,Program][,.N])
    )
  
  # Add x-axis formatting
  my_plot <- my_plot +
    scale_x_date(
      #limits = c(start.date, stop.date+14),
      breaks = seq(start.date-7*10, stop.date+7*10, by = "week"),
      #breaks = "1 week",
      date_labels = "%d\n%b"
    )
  
  # Add theme
  my_plot <- my_plot +
    theme(
      panel.grid.minor = element_blank(),
      plot.caption = element_text(colour = "darkgreen", size = 10),
      legend.title.align = 0.5,
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8),
      panel.grid.major.y = element_blank(),
      plot.title = element_text(size = 10)
    )
  
  # Add y-axis
  my_plot <- my_plot +
    scale_y_continuous(
      breaks = sort(unique(dt[, ReverseOrd])),
      labels = (unique(dt[, Program, keyby = ReverseOrd])[,Program]),
      expand = c(0.01,0.01)
    )
  
  
  if(add_steps){
    dt_ss_long <- melt.data.table(
      dt_ss,
      id.vars = c("ID", "LeadOTA", "Division", "Program", "ReverseOrdShiftBottom", "ReverseOrdShiftTop", "Step",
      "Product", "Event", "EventName", "DaysToStart", "Duration", "DaysToEnd",
      "OTPO", "ORSA", "MS", "TM", "DM", "Cy", "LF"),
        #c("ID", "Division", "Program", "ReverseOrdShiftBottom", "ReverseOrdShiftTop", "Step"),
      measure.vars = c("StartDate", "StopDate"),
      variable.name = "SS",
      value.name = "Date"
    )
    
    # Add step start/stop ribbon
    my_plot <- my_plot +
      geom_ribbon(
        data = dt_ss_long,
        mapping = aes(
          x = Date,
          ymin = ReverseOrdShiftBottom,
          ymax = ReverseOrdShiftTop,
          fill = Step, group = interaction(ID, Program, Step),
          text = paste(
            'Division:', Division,
            '</br></br>Lead OTA:', LeadOTA,
            '</br>Program:', Program,
            '</br>Step:', Step,
            '</br>Date:', Date,
            '</br>DaysToStart:', DaysToStart,
            '</br>Duration (days):', Duration,
            '</br>DaysToEnd:', DaysToEnd,
            '</br>Event:', Event,
            '</br>Event Name:', EventName,
            '</br>OTPO:', OTPO,
            '</br>ORSA:', ORSA,
            '</br>MS:', MS,
            '</br>TM:', TM,
            '</br>DM:', DM,
            '</br>Cy:', Cy,
            '</br>LF:', LF
          )
        ), alpha = .5
      )
    
    # Add ribbon fill color
    my_plot <- my_plot +
      scale_fill_manual(
        name = paste(
          "\nStep", sep = "\n\n"
        ),
        values = c(
          "Evaluation Planning" = cbbPalette[1],
          "Concept Planning" = cbbPalette[2],
          "Detailed Planning" = cbbPalette[3],
          "Event Execution" = cbbPalette[4],
          "Test Reporting" = cbbPalette[5],
          "Evaluation Reporting" = cbbPalette[8]
        )
      )
  }

    
  # Add TEMP/Report signatures
  my_plot <- my_plot +
    geom_point(
      data = dt_prog,
      mapping = aes(x = StopDate, y = ReverseOrd,
                    text = paste(
                      'Program:', Program,
                      '</br></br>Division:', Division,
                      '</br>Product:', Product,
                      '</br>Date:', StopDate
                    )), size = 1.25, shape = 5
    ) +
    geom_text(
      data = dt_prog,
      mapping = aes(x = StopDate, y = ReverseOrd, label = Product),
      hjust = 0.0,
      size = 3,     # 4.5,
      nudge_x = 3   # .5
    )
  
  my_plot
}






# Function for filterable chart
plot_fun_plotly <- function(
    dt, dt_ss, dt_prog, dt_miles,
    steps = TRUE, temps_reports = TRUE, milestones = TRUE, reviews = TRUE, quarters = FALSE, xaxis = "month"
    ){
  
  my_plot <- dt %>%
    ggplot()
  
  # color every other row to help indicate distinctions between programs
  my_seq <- seq(dt[, min(ReverseOrd)]-1, dt[, max(ReverseOrd)]-1, by = 2)
  for(i in seq_along(my_seq)){
    my_plot <- my_plot + geom_ribbon(
      data = data.frame(
        x = c(mindate, maxdate),
        ymin = c(my_seq[i],my_seq[i]),
        ymax = c(my_seq[i]+2,my_seq[i]+2)
      ),
      aes(x = x, ymin = ymin, ymax = ymax),
      fill = ifelse(i %% 2 == 0, "grey70", "transparent"),
      alpha = ifelse(i %% 2 == 0, .2, 0)
    )
  }
  
  if(quarters){
    quarter_ribbons <- data.table(
      Date = seq.Date( as.Date("2000-10-01"), as.Date("2060-07-01"), by = "3 months"),
      Quarter = rep(1:4, times = 60)
    )
    
    #i <- 1
    for(i in seq_along(1:nrow(quarter_ribbons))){
      my_plot <- my_plot +
        geom_ribbon(
          data = quarter_ribbons[, .(Date = Date[c(i, i+1)], Quarter = Quarter[i])],
          mapping = aes(
            x = Date,
            ymin = dt[, min(ReverseOrd)]-1.1,
            ymax = dt[, max(ReverseOrd)]+1.1
          ),
          fill = ifelse(quarter_ribbons[i, Quarter] %% 2 == 0, "grey70", "transparent"),
          alpha = ifelse(quarter_ribbons[i, Quarter] %% 2 == 0, .2, 0)
        )
    }
  }
  
  # Add axis labels and theme
  my_plot <- my_plot +
    labs(
      y = "Program", x = "", #x = "Date"
    ) +
    theme(
      panel.grid.minor = element_blank(),
      plot.caption = element_text(colour = "darkgreen", size = 10),
      legend.title.align = 0.5,
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8),
      panel.grid.major.y = element_blank(),
      axis.text.x.top = element_text(size = 6)
    ) +
    scale_y_continuous(
      breaks = sort(unique(dt[, ReverseOrd])),
      labels = (unique(dt[, Program, keyby = ReverseOrd])[,Program]),
      limits = c(dt[, min(ReverseOrd)]-1.1, dt[, max(ReverseOrd)]+1.1),
      expand = c(0.00,0.00)
    )
  
  
  # Customize x-axis
  if(xaxis == "month"){
    my_plot <- my_plot +
      scale_x_date(
        #limits = c(start.date, stop.date+14),
        breaks = seq(mindate, maxdate, by = "month"),
        #breaks = "1 week",
        date_labels = "%b\n%y",
        expand = c(0,0)
      )
  } else
  if(xaxis == "quarter"){
    quarter_ribbons <- data.table(
      Date = seq.Date( as.Date("2000-10-01"), as.Date("2060-07-01"), by = "3 months"),
      Quarter = rep(1:4, times = 60)
    )
    
    mind <- quarter_ribbons[Date <= mindate, max(Date)]
    maxd <- quarter_ribbons[Date >= maxdate, min(Date)]
    
    q1 <- quarter_ribbons[between(Date, mind, maxd), paste0("Q", Quarter)]
    q2 <- quarter_ribbons[between(Date, mind, maxd), format(Date, "%b\n%y")]
    q <- paste0(q1, "\n", q2)
    
    q3 <- rep(" ", length(q))
    
    labels <- head(c( rbind(q, q3, q3) ), -2)
    
    my_plot <- my_plot +
      scale_x_date(
        breaks = seq(mind, maxd, by = "month"),
        #breaks = seq(mind, maxd, by = "3 months"),
        #minor_breaks = seq(mind, maxd, by = "month"),
        date_labels = labels, #paste0(q1, "\n%b\n%y"),
        expand = c(0,0)
      )
  } else
  if(xaxis == "week"){
    
    mind <- start_date_sunday(x = mindate)
    maxd <- stop_date_sunday(x = maxdate)
    
      my_plot <- my_plot +
        scale_x_date(
          breaks = seq(mind, maxd, by = "week"),
          date_labels = "%d\n%b",
          expand = c(0,0)
        )
    }
  
  
  
  dt_ss_long <- melt.data.table(
    dt_ss,
    id.vars = c("ID", "LeadOTA", "Division", "Program", "ReverseOrdShiftBottom", "ReverseOrdShiftTop", "Step",
                "Product", "Event", "EventName", "DaysToStart", "Duration", "DaysToEnd",
                "OTPO", "ORSA", "MS", "TM", "DM", "Cy", "LF"),
    measure.vars = c("StartDate", "StopDate"),
    variable.name = "SS",
    value.name = "Date"
  )
  
  # Plot Ribbons
  if(steps){
  my_plot <- my_plot +
    geom_ribbon(
      data = dt_ss_long,
      mapping = aes(
        x = Date,
        ymin = ReverseOrdShiftBottom,
        ymax = ReverseOrdShiftTop,
        fill = Step, group = interaction(ID, Program, Step),
        text = paste(
          'Division:', Division,
          '</br></br>Lead OTA:', LeadOTA,
          '</br>Program:', Program,
          '</br>Step:', Step,
          '</br>Date:', Date,
          '</br>DaysToStart:', DaysToStart,
          '</br>Duration (days):', Duration,
          '</br>DaysToEnd:', DaysToEnd,
          '</br>Event:', Event,
          '</br>Event Name:', EventName,
          '</br>OTPO:', OTPO,
          '</br>ORSA:', ORSA,
          '</br>MS:', MS,
          '</br>TM:', TM,
          '</br>DM:', DM,
          '</br>Cy:', Cy,
          '</br>LF:', LF
        ), alpha = .5
      )
    ) +
    scale_fill_manual(
      name = paste(
        paste0("Number of Programs = ", dt[,.N,Program][,.N]),
        "\nStep", sep = "\n\n"
      ),
      values = c(
        "Evaluation Planning" = cbbPalette[1],
        "Concept Planning" = cbbPalette[2],
        "Detailed Planning" = cbbPalette[3],
        "Event Execution" = cbbPalette[4],
        "Test Reporting" = cbbPalette[5],
        "Evaluation Reporting" = cbbPalette[8]
        )
    )
  
    
    my_plot <- my_plot +
      geom_text(
        data = dt_ss[Step %in% c("Event Execution", "Evaluation Reporting")],
        mapping = aes(x = StopDate, y = ReverseOrdShift, label = EventName, text = NULL),
        hjust = 0.0, size = 2.5, nudge_x = 0
      )
  }
  
  

  
  # Plot CRB/IPR Reviews
  if(reviews == TRUE & nrow(dt) > 0){ 
    my_plot <- my_plot +
      geom_point(data = dt, 
                 aes(x = Date, y = ReverseOrdShift, fill = Step,
                     text = paste(
                       'Division:', Division,
                       '</br></br>Lead OTA:', LeadOTA,
                       '</br>Program:', Program,
                       '</br>Product:', Product,
                       '</br>Review:', Review,
                       '</br>Date:', Date,
                       '</br>OTPO:', OTPO,
                       '</br>ORSA:', ORSA,
                       '</br>MS:', MS,
                       '</br>TM:', TM,
                       '</br>DM:', DM,
                       '</br>Cy:', Cy,
                       '</br>LF:', LF
                     )),
                 shape = 21, size = 1.25, stroke = .25, show.legend = FALSE,
                 colour = "black"
      )
  }
  
  # Plot External Signatures
  if(temps_reports == TRUE & nrow(dt_prog) > 0){
    my_plot <- my_plot +
      geom_point(
        data = dt_prog,
        mapping = aes(x = StopDate, y = ReverseOrd,
                      text = paste(
                        'Product:', Product,
                        '</br></br>Event:', Event,
                        '</br>Event Name:', EventName,
                        '</br>Date:', StopDate
                      )
        ), size = 1.25, shape = 5
      ) +
      geom_text(
        data = dt_prog,
        mapping = aes(x = StopDate, y = ReverseOrd, label = Product, text = NULL),
        size = 3.0, nudge_x = ifelse( len_days > 300, 3, 1)
      )
  }
  
  # Plot Milestones Signatures
  if(milestones == TRUE & nrow(dt_miles) > 0){
    my_plot <- my_plot +
      geom_point(
        data = dt_miles,
        mapping = aes(x = Date, y = ReverseOrd,
                      text = paste(
                        'Program:', Program,
                        '</br></br>Milestone:', Milestone,
                        '</br>Date:', Date
                      )
        ), size = 1.25, shape = 8, colour = "red", stroke = 0
      ) +
      geom_text(
        data = dt_miles,
        mapping = aes(x = Date, y = ReverseOrd, label = Milestone, text = NULL),
        size = 3.0, nudge_x = ifelse( len_days > 300, 3, 1), colour = "red", family = "serif"
      )
  }
  
  my_plot <- my_plot +
    guides(alpha = "none", colour = "none")
  
  my_plot
}
















# Define UI for application that draws a histogram
ui <- fluidPage(

  tags$head(
    tags$style(HTML("

    #loadmessage {
      /*position: fixed;
      top: 0px;
      left: 0px;*/
      width: 100%;
      padding: 5px 0px 5px 0px;
      text-align: center;
      font-weight: bold;
      font-size: 14pt;
      color: #000000;
      background-color: #CCFF66;
      z-index: 105;
    }"
    )
    )
  ),
    
  useShinyjs(),
  navbarPage(
    title = "MCOTEA", windowTitle = "MCOTEA PSWG and CUB Charts",
    
    
    # Start of PSWG Chart Panel ==========================================================================================================
    tabPanel(
      title = "PSWG Chart - Reviews",
      div(
        id = "PSWG_Sidebar",
        sidebarPanel(
          
          #HTML("<p style = 'font-size: 8pt; margin: 0 0 0px;'>Choose Excel File Containing 'datFormProgram' table data from the ASM</p>"),
          #shinyFilesButton("dataFile", "File select", "Please select an Excel .xlsx file", multiple = FALSE),
          dateInput("pswg_start_date", "Start Date", value = Sys.Date()),
          numericInput("pswg_days", "Number of Days to Display", value = 90, min = 0),
          textInput("verticals", "Display Vertical Lines at...", value = "0,30,60,90", placeholder = "0,30"),
          HTML("<p style = 'font-size: 8pt; margin: 0 0 0px;'>Input numeric values (days) separated by commas.</p>"),
          width = 2
        )
      ),
      
      mainPanel(
        actionButton("toggle_PSWG_Sidebar", "Toggle Sidebar"),
        textOutput("message"),
        textOutput("filemessage"),
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         tags$div("Loading... Please wait.",id="loadmessage")),
        br(),
        #downloadButton("download_PSWG_Plot", "Download Plot"),
        plotlyOutput("PSWG_Plot", width = 1400, height = 750),
        br(),
        hr(),
        fluidRow(
          DTOutput("pswg_table")
        )
      )
    ), # End of PSWG Chart Panel ==========================================================================================================
    
    
    
    # Start of CUB Chart Panel ============================================================================================================
    tabPanel(
      title = "CUB Chart - TEMP Docs & Reports",
      div(
        id = "CUB_Sidebar",
        sidebarPanel(
          dateInput("cub_start_date", "Start Date", value = Sys.Date()),
          numericInput("cub_days", "Number of Days to Display", value = 180, min = 0),
          textInput("cub_verticals", "Display Vertical Lines at...", value = "0,30,60,90", placeholder = "0,30"),
          HTML("<p style = 'font-size: 8pt; margin: 0 0 0px;'>Input numeric values (days) separated by commas.</p>"),
          width = 2
        )
      ),
      
      mainPanel(
        actionButton("toggle_CUB_Sidebar", "Toggle Sidebar"),
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         tags$div("Loading... Please wait.",id="loadmessage")),
        br(),
        plotlyOutput("CUB_Plot", width = 1400, height = 750)
      )
    ), # End of CUB Chart Panel ===========================================================================================================
    
    
    
    # Start of Filterable Chart Panel =====================================================================================================
    tabPanel(
      title = "Filterable Chart",
      div(
        id = "Filter_Sidebar",
        sidebarPanel(
          dateInput("f_start_date", "Start Date", value = Sys.Date()),
          numericInput("f_months", "Number of Months to Display", value = 24, min = 0),
          selectInput("f_division", label = "Select Divisions to Display", choices="No Divisions to display.", multiple = TRUE),
          selectInput("f_program", label = "Select Programs to Display", choices = "No Programs to display.", multiple = TRUE),
          selectInput("show", label = "Select data to display",
                      choices = c("Steps", "TEMPs/Reports", "Milestones", "Reviews"),
                      selected = c("Steps", "TEMPs/Reports", "Milestones", "Reviews"),
                      multiple = TRUE),
          #selectInput("quarters", label = "Shade quarters?", choices = c(TRUE, FALSE), selected = FALSE, multiple = FALSE),
          checkboxInput("quarters", label = "Shade quarters?", value = FALSE),
          selectInput("xaxis", label = "x-axis by...", choices = c("week", "month", "quarter"), selected = "month", multiple = FALSE),
          selectInput("filterby", label = "Filter People by Position",
                      choices = c("NA", "OTPO", "ORSA", "MS", "TM", "DM", "Cyber", "Live Fire"),
                      selected = "NA", multiple = FALSE),
          selectInput("people", label = "Select People to Display",choices = "No People to display.", multiple = TRUE),
          textInput("f_verticals", "Display Vertical Lines at...", value = "0", placeholder = "0,30"),
          HTML("<p style = 'font-size: 8pt; margin: 0 0 0px;'>Input numeric values (days) separated by commas.</p>"),
          width = 2
        )
      ),
      
      mainPanel(
        actionButton("toggle_Filter_Sidebar", "Toggle Sidebar"),
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         tags$div("Loading... Please wait.",id="loadmessage")),
        br(),
        #plotOutput("f_Plot", width = 1400, height = 750),
        plotlyOutput("f_Plot", width = 1400, height = 750)#,
        # br(),
        # hr(),
        # fluidRow(
        #   DTOutput("f_table")
        # )
      )
    ), # End of Filterable Chart Panel =====================================================================================================
    
    
    # Start of DCD Chart Panel =====================================================================================================
    tabPanel(
      title = "DCDs Chart",
      div(
        id = "DCD_Sidebar",
        sidebarPanel(
          dateInput("dcd_start_date", "Start Date", value = Sys.Date()),
          numericInput("dcd_months", "Number of Months to Display", value = 24, min = 0),
          selectInput("device_type", label = "Select Device Types to Display",
                      choices = c("DCD", "Mesa Tablet" , "Getac Tablet", "Getac Laptop"),
                      selected = c("Mesa Tablet" , "Getac Tablet", "Getac Laptop"), multiple = TRUE),
          checkboxInput("show_only", label = "Only show programs/events with a request for one of the above device types?", value = FALSE),
          textInput("dcd_verticals", "Display Vertical Lines at...", value = "0", placeholder = "0,30"),
          HTML("<p style = 'font-size: 8pt; margin: 0 0 0px;'>Input numeric values (days) separated by commas.</p>"),
          width = 2
        )
      ),

      mainPanel(
        actionButton("toggle_DCD_Sidebar", "Toggle Sidebar"),
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         tags$div("Loading... Please wait.",id="loadmessage")),
        br(),
        plotlyOutput("dcd_Plot", width = 1400, height = 750)
      )
    ), # End of DCD Chart Panel =====================================================================================================
    
    
    # Start of DCD Chart Panel =====================================================================================================
    tabPanel(
      title = "DCDs Chart2",
      div(
        id = "DCD_Sidebar2",
        sidebarPanel(
          dateInput("dcd_start_date2", "Start Date", value = Sys.Date()),
          numericInput("dcd_months2", "Number of Months to Display", value = 12, min = 0),
          textInput("dcd_verticals2", "Display Vertical Lines at...", value = "0", placeholder = "0,30"),
          HTML("<p style = 'font-size: 8pt; margin: 0 0 0px;'>Input numeric values (days) separated by commas.</p>"),
          width = 2
        )
      ),
      
      mainPanel(
        actionButton("toggle_DCD_Sidebar2", "Toggle Sidebar"),
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         tags$div("Loading... Please wait.",id="loadmessage")),
        br(),
        plotlyOutput("dcd_Plot2", width = 1400, height = 750)
      )
    ) # End of DCD Chart Panel =====================================================================================================
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  #######################################
  session$onSessionEnded(function() {   #
    stopApp()                           #
  })                                    #
  #######################################    
  
  
  # Message to select Excel file if no file is yet selected.
  # output$message <- renderText({
  #   if(class(input$dataFile) != "list"){
  #     "Please select an Excel .xlsx file using the 'File select' button."
  #   } else{
  #     NULL
  #   }
  # })
  
  # Select file
  # dataFile <- reactive({
  #   roots <- c(wd = file.path(filedir))
  #   shinyFileChoose(input, "dataFile", roots = roots, 
  #                   session = session, filetypes = c("", "xlsx"))
  #   req(input$dataFile)
  #   if (is.null(input$dataFile)) 
  #     return(NULL)
  #   return(parseFilePaths(roots = roots, input$dataFile)$datapath)
  # })
  # 
  # output$filemessage <- renderText({
  #   dataFile()
  # })
  
  # Read in file
  dt <- reactive({
    #req(dataFile())
    
    #inFile <- dataFile()
    
    #if(is.null(inFile)){
    if(is.null(f1)){
      return(NULL)
    }
    else{
      # dt <- read_xlsx(
      #   inFile, sheet = 1,
      #   na = c("NA", "N/A", "", "na", "Na", "n/a", "N/a", "NaN", "nA"))
      dt <- f1
      setDT(dt)
      
      
      dt[, StopDate := as.Date(StopDate)]
      dt[, StartDate := as.Date(StartDate)]
      
      dt[, OTPO := paste0(OTPOPrimary, ", ", OTPOSecondary)]
      dt[, OTPOSecondary := NULL]
      
      dt[, ORSA := paste0(ORSAPrimary, ", ", ORSASecondary)]
      dt[, ORSASecondary := NULL]
      
      dt[, MS := paste0(MSPrimary, ", ", MSSecondary)]
      dt[, MSSecondary := NULL]
      
      dt[, TM := paste0(TMPrimary, ", ", TMSecondary)]
      dt[, TMSecondary := NULL]
      
      dt[, DM := paste0(DMPrimary, ", ", DMSecondary)]
      dt[, DMSecondary := NULL]
      
      dt[, Cy := paste0(LCA, ", ", CA)]
      dt[, LCA := NULL]
      dt[, CA := NULL]
      
      dt[, LF := LFAnalyst]
      dt[, LFAnalyst := NULL]
    }
  })
  
  # Filter file
  dt_f1 <- reactive({
    # inFile <- dataFile()
    
    # if(is.null(inFile)){
    if(is.null(f1)){
      return(NULL)
    }
    else{
      dt <- dt()
      
      dt <- dt[
        StepStatus == "Active",
        .(
          ID, LeadOTA, Program, Division, Step, Product, Event, EventName,
          CRBIPRDate, IPR1Date, IPR2Date, IPR3Date, IPR4aDate, IPR4bDate, IPR5Date, TEMPDate, OTRBDate, OTRRDate, FDSCDate, FOSDueDate,
          OTPO, ORSA, MS, TM, DM, Cy, LF
        )
      ]
      
      dt
    }
  })
  
  # Filter file
  dt_f1_long <- reactive({
    # inFile <- dataFile()
    
    # if(is.null(inFile)){
    if(is.null(f1)){
      return(NULL)
    }
    else{
      dt <- dt_f1()
      
      dt <- melt.data.table(
        dt,
        id.vars = c("ID", "LeadOTA", "Program", "Division", "Step", "Product", "Event", "EventName",
                    "OTPO", "ORSA", "MS", "TM", "DM", "Cy", "LF"),
        variable.name = "Review", value.name = "Date"
      )
      
      # Remove the word "Date" from the Review column
      # Keep only complete cases
      # Make sure it's formatted as a date
      dt[, Review := gsub("Date", "", Review)]
      dt <- dt[!is.na(Date)]
      dt[, Date := as.Date(Date)]
      
      dt
    }
  })
  
  
  
  
  # start and stop events
  dt_ss1 <- reactive({
    if(is.null(f1)){
      return(NULL)
    }
    else{
      dt <- dt()
      
      dt <- dt[
        StepStatus == "Active",
        .(ID, LeadOTA, Program, Division, Step, Product, Event, EventName, StartDate, Duration,
          OTPO, ORSA, MS, TM, DM, Cy, LF
        ),
        keyby = .(StopDate)
      ]
      
      start.date <- start_date_sunday(x = cub_start_date())
      stop.date <- stop_date_sunday(x = start.date + cub_days())
      
      date_range <- seq(start.date, stop.date, by = 1)
      date_range <- as.Date(date_range)
      
      # keep start and stop events that occur over the course of time
      # we're looking at
      btwn <- array(NA, dim = c(nrow(dt), length(date_range)))
      for(i in seq_along(date_range)){
        btwn[,i] <- dt[, between(date_range[i], StartDate, StopDate) ]
      }
      keep_rows <- which(apply(btwn, MARGIN = 1, FUN = function(x) any(x == TRUE)))
      dt <- dt[keep_rows]
      
      # Remove Monitor/Transition
      dt <- dt[Step != "Monitor/Transition"]
      
      dt
    }
  })
  
  
  # start and stop events
  dt_milestones <- reactive({
    # inFile <- dataFile()
    
    # if(is.null(inFile)){
    if(is.null(f1)){
      return(NULL)
    }
    else{
      dt <- f2
      setDT(dt)
      dt[, Program := ProgramName]
      dt[, ProgramName := NULL]
      
      dt <- melt.data.table(
        dt,
        id.vars = c("Division", "Program"),
        measure.vars = c(
          "MSA", "MSB", "MSC", "TempInput", "TempSign", "PDR", "CDR",
          "RFPRelease", "ContractAward", "SystemDelivery",
          "CDD", "CPD", "IOC", "GOBoD CAR", "FRP"),
        variable.name = "Milestone",
        value.name = "Date"
      )
      
      dt[, Date := as.Date(Date)]
      
      dt
    }
  })
  

  


  
  ###############################################################################################################################
  # Start PSWG Server Logic
  
  ###############################################
  observeEvent(input$toggle_PSWG_Sidebar, {     #
    shinyjs::toggle(id = "PSWG_Sidebar")        #
  })                                            #
  ###############################################
  
  ###############################################
  observeEvent(input$toggle_CUB_Sidebar, {      #
    shinyjs::toggle(id = "CUB_Sidebar")         #
  })                                            #
  ###############################################
  
  ###############################################
  observeEvent(input$toggle_Filter_Sidebar, {   #
    shinyjs::toggle(id = "Filter_Sidebar")      #
  })                                            #
  ###############################################
  
  ###############################################
  observeEvent(input$toggle_DCD_Sidebar, {      #
    shinyjs::toggle(id = "DCD_Sidebar")         #
  })                                            #
  ###############################################
  
  ###############################################
  observeEvent(input$toggle_DCD_Sidebar, {      #
    shinyjs::toggle(id = "DCD_Sidebar2")         #
  })                                            #
  ###############################################
  
  
  days_pswg <- reactive({
    input$pswg_days
  })
  pswg_days <- debounce(days_pswg, 2000)
  
  
  start_pswg <- reactive({
    input$pswg_start_date
  })
  pswg_start_date <- debounce(start_pswg, 2000)
  
  

    dt_pswg <- reactive({
    # inFile <- dataFile()
    
    # if(is.null(inFile)){
    if(is.null(f1)){
      return(NULL)
    }
    else{
      dt <- dt_f1_long()

      start.date <- start_date_sunday(x = pswg_start_date())
      stop.date <- stop_date_sunday(x = start.date + pswg_days())
      
      # Only keep data in our range
      dt <- dt[between(Date, start.date, stop.date)]
      
      dt[, Label := trimws(paste(trimws(Program), Product, Review, EventName, sep = ", "))]

      # Set to sort by date and program by default
      setkey(dt, Date, Program)
      
      # So programs are sorted based on earliest review date
      dt <- dt_early_order(dt)
      
      # Create the shift within a program row
      dt <- dt_pswg_prog_row_shift(dt)
      
      return(dt)
    }
  })
  
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    pswg_v <- reactive({
      input$verticals
    })
    verticals <- debounce(pswg_v, 2000)
    
  output$PSWG_Plot <- renderPlotly({
    dt <- dt_pswg()
    
    miny <- floor(min(dt$ReverseOrdShift))-5
    maxy <- ceiling(max(dt$ReverseOrdShift))+5
    
    today <- Sys.Date()
    start.date <- start_date_sunday(x = pswg_start_date())
    stop.date <- stop_date_sunday(x = start.date + pswg_days())
    
    p <- plot_fun_pswg(dt = dt, start = start.date, days = pswg_days()) %>%
      ggplotly(tooltip = "text") %>%
      style(textposition = "right") %>%
      # layout(xaxis = list(
      #   range = c(as.numeric(start.date), as.numeric(stop.date))
      # )
      # ) %>%
      config(
        toImageButtonOptions = list(
          format = "png", height = 750, width = 1400, scale = 3
        )
      )
    
    # Remove the hover from the geom_text
    p_json <- plotly_json(p, FALSE)
    # p_json <- plotly_json(p)
    # print(paste0(fromJSON(p_json$x$data)$data$type, ": ",
    #              fromJSON(p_json$x$data)$data$name))
    
    
    dont_trace <- is.na(fromJSON(p_json)$data$name)
    # dont_trace <- rep(FALSE, length(dont_trace))
    
    p <- plot_fun_pswg(dt, start = start.date, days = pswg_days()) %>%
      ggplotly(tooltip = c("text")) %>%
      style(textposition = "right", hoverinfo = "none", traces = dont_trace) %>%
      layout(xaxis = list(
        range = as.numeric(c(start.date, stop.date+14)),
        restrict = "domain"
      )
      ) %>%
      config(
        toImageButtonOptions = list(
          format = "png", height = 750, width = 1400, scale = 3
        )
      )
    
    
    verticals <- strsplit(verticals(), ",") %>% unlist %>% as.numeric
    nans <- which(is.na(verticals))
    if( !identical(nans, integer(0)) ){
      verticals <- verticals[-nans]
    }
    
    for(i in seq_along(verticals)){
      if(verticals[i] == 0){
        name <- paste0("today: ", format(today, "%d %b %y"))
      } else
      if(verticals[i] > 0){
        name <- paste0("today+", verticals[i], ": ", format(today+verticals[i], "%d %b %y"))
      } else
      if(verticals[i] < 0){
          name <- paste0("today", verticals[i], ": ", format(today-verticals[i], "%d %b %y"))
      }
      
      p <- p %>%
        add_lines(x = as.numeric(today+verticals[i]), y = c(miny,maxy), text = NULL,
                  name = name,
                  color = I("black"), opacity = .5)
    }
    
    # # Add vertical lines
    # p <- p %>%
    #   add_lines(x = as.numeric(today), y = c(miny,maxy), text = NULL, name = paste0("today: ", format(today, "%d %b %y")), color = I("black"), opacity = .5) %>%
    #   add_lines(x = as.numeric(today)+30, y = c(miny,maxy), text = NULL, name = paste0("today+30: ", format(today+30, "%d %b %y")), color = I("black"), opacity = .5) %>%
    #   add_lines(x = as.numeric(today)+60, y = c(miny,maxy), text = NULL, name = paste0("today+60: ", format(today+60, "%d %b %y")), color = I("black"), opacity = .5) %>%
    #   add_lines(x = as.numeric(today)+90, y = c(miny,maxy), text = NULL, name = paste0("today+90: ", format(today+90, "%d %b %y")), color = I("black"), opacity = .5)
    
    p
  })
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  output$pswg_table <- renderDT(server = FALSE, {                                      #
    dt <- dt_pswg()
    setDT(dt)
    dt <- dt[, .(ID, LeadOTA, Program, Division, Step, Product, Event, EventName, Review, Date, Order, ReverseOrdShift)]
    
    datatable(                                                                         #
      dt,                                                                              #
      filter = list(position = "top", clear = TRUE, plain = TRUE),                     #
      rownames = FALSE,                                                                #
      style = "bootstrap",                                                             #
      extensions = "Buttons",                                                          #
      selection = "multiple",                                                          #
      class = "compact hover cell-border stripe order-column",                         #
      options = list(                                                                  #
        buttons = list(                                                                #
          list(extend = "excel", exportOptions = list(modifier = list(page = "all"))), #
          list(extend = "csv", exportOptions = list(modifier = list(page = "all"))),   #
          list(                                                                        #
            extend = "pdf",                                                            #
            orientation = "landscape",                                                 #
            exportOptions = list(modifier = list(page = "all"))                        #
          ),                                                                           #
          list(extend = "copy", exportOptions = list(modifier = list(page = "all"))),  #
          list(extend = "print", exportOptions = list(modifier = list(page = "all")))  #
        ),                                                                             #
        dom = "Bfrtip",                                                                #
        autowidth = TRUE,                                                              #
        pageLength = 50,                                                               #
        order = list(list(10,"dsc"))                                                   #
      )                                                                                #
    ) }                                                                                #
  )                                                                                    #
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  # End PSWG Server Logic
  ###############################################################################################################################
  
  
  
  
  
  
  
  
  ###############################################################################################################################
  # Start CUB Server Logic
  
  days_cub <- reactive({
    input$cub_days
  })
  cub_days <- debounce(days_cub, 2000)
  
  
  start_cub <- reactive({
    input$cub_start_date
  })
  cub_start_date <- debounce(start_cub, 2000)
  
  cub_dt <- reactive({    #cub_dt_ss
    # inFile <- dataFile()
    
    # if(is.null(inFile)){
    if(is.null(f1)){
      return(NULL)
    }
    else{
      start.date <- start_date_sunday(x = cub_start_date())
      stop.date <- stop_date_sunday(x = start.date + cub_days())
      
      # start and stop events
      dt_ss <- dt_ss1()
      
      # Wide to long
      dt <- dt_f1_long()

      # Only keep data in our range
      dt <- dt[between(Date, start.date, stop.date)]
      
      # Set to sort by date and program by default
      setkey(dt, Date, Program)
      
      dt[, Label := trimws(paste(trimws(Program), Product, Review, EventName, sep = ", "))]
      
      # identify programs in dt_ss missing from dt and add them in
      dt <- dt_add_miss_progs(x = dt_ss, y = dt)
      
      # So programs are sorted based on earliest review date
      dt <- dt_early_order(dt)
      
      #==========================================================
      # Make Step a factor and order the factor levels
      dt[, Step := factor(Step,
        levels = c(
          "Evaluation Planning", "Concept Planning", "Detailed Planning",
          "Event Execution", "Test Reporting", "Evaluation Reporting")
        )
      ]
      #==========================================================
      
      
      dt_prog <- dt_ss[
        Step %in% "TempSign" | #c("MS C", "TempSign") |
          Product %in% c(
            "MTS", "TEMP", "TEMPSign",                       #TEMP like docs
            "IAR", "OMAR", "SAR", "OAR", "OER", "Report"     #Reports
            )]

      
      dt_prog <- merge(
        dt_prog,
        unique(dt[, .(LeadOTA, Program, Division, ReverseOrd)]),
        by = c("Program", "Division"), all.x = T)
      
      # return(dt_prog)
      
      #==========================================================
      # Make Step a factor and order the factor levels
      dt_ss <- dt_ss[!(Step %in% c("MS C", "TempSign"))]
      
      dt_ss[, Step := factor(Step,
        levels = c(
          "Evaluation Planning", "Concept Planning", "Detailed Planning",
          "Event Execution", "Test Reporting", "Evaluation Reporting")
        )
      ]
      #==========================================================
      
      #==========================================================
      # Add ReverseOrd by Program so we have plotting positions
      dt_ss <- merge(
        dt_ss,
        unique(dt[, .(Program, ReverseOrd)]),
        by = c("Program"), all.x = TRUE
      )
      
      # Shift steps so steps do not overlap
      dt_ss <- dt_step_shift(dt = dt_ss)
      
      dt_ss[, ReverseOrdShiftTop := ReverseOrdShift+1/6]
      dt_ss[, ReverseOrdShiftBottom := ReverseOrdShift-1/6]
      
      # dt_ss[StartDate < start.date,  StartDate := start.date]
      # dt_ss[StopDate > stop.date+14,  StopDate := stop.date+14]
      
      # return(dt_ss)
      
      # Shift steps so steps do not overlap
      dt <- dt_step_shift(dt = dt)
      
      dt[, ReverseOrdShiftTop := ReverseOrdShift+1/6]
      dt[, ReverseOrdShiftBottom := ReverseOrdShift-1/6]
      
      # return(dt)
      today <- Sys.Date()
      dt_ss[, StartDate := as.Date(StartDate)]
      dt_ss[, StopDate := as.Date(StopDate)]
      dt_ss[, DaysToStart := difftime(StartDate, today, units = "days")]
      dt_ss[, DaysToEnd := difftime(StopDate, today, units = "days")]
      
      out <- list(dt = dt, dt_ss = dt_ss, dt_prog = dt_prog)
      return(out)
    }
  })
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  cub_v <- reactive({
    input$cub_verticals
  })
  cub_verticals <- debounce(cub_v, 2000)
  
  output$CUB_Plot <- renderPlotly({
    
    dt_all <- cub_dt()
    dt <- dt_all$dt
    dt_ss <- dt_all$dt_ss
    dt_prog <- dt_all$dt_prog
    
    miny <- floor(min(dt$ReverseOrd))-5
    maxy <- ceiling(max(dt$ReverseOrd))+5
    
    today <- Sys.Date()
    
    start.date <- start_date_sunday(x = cub_start_date())
    stop.date <- stop_date_sunday(x = start.date + cub_days())
    
    p <- plot_fun_cub(dt = dt, dt_ss = dt_ss, dt_prog = dt_prog,
                      start = start.date, days = cub_days(), add_steps = TRUE) %>%
      ggplotly(tooltip = "text") %>%
      style(textposition = "right") %>%
      # layout(xaxis = list(
      #   range = c(as.numeric(start.date), as.numeric(stop.date))
      # )
      # ) %>%
      config(
        toImageButtonOptions = list(
          format = "png", height = 750, width = 1400, scale = 3
        )
      )
    
    # Remove the hover from the geom_text
    p_json <- plotly_json(p, FALSE)
    # p_json <- plotly_json(p)
    # print(paste0(fromJSON(p_json$x$data)$data$type, ": ",
    #              fromJSON(p_json$x$data)$data$name))
    
    
    dont_trace <- fromJSON(p_json)$data$mode=="text" #is.na(fromJSON(p_json)$data$name)
    # dont_trace <- rep(FALSE, length(dont_trace))
    
    
    p <- plot_fun_cub(dt = dt, dt_ss = dt_ss, dt_prog = dt_prog,
                      start = start.date, days = cub_days(), add_steps = TRUE) %>%
      ggplotly(tooltip = c("text")) %>%
      style(textposition = "right", hoverinfo = "none", traces = dont_trace
            ) %>%
      layout(xaxis = list(
        range = as.numeric(c(start.date, stop.date+14)),
        restrict = "domain"
      )
      ) %>%
      config(
        toImageButtonOptions = list(
          format = "png", height = 750, width = 1400, scale = 3
        )
      )
    
    
    # Add vertical lines
    verticals <- strsplit(cub_verticals(), ",") %>% unlist %>% as.numeric
    nans <- which(is.na(verticals))
    if( !identical(nans, integer(0)) ){
      verticals <- verticals[-nans]
    }
    
    for(i in seq_along(verticals)){
      if(verticals[i] == 0){
        name <- paste0("today: ", format(today, "%d %b %y"))
      } else
        if(verticals[i] > 0){
          name <- paste0("today+", verticals[i], ": ", format(today+verticals[i], "%d %b %y"))
        } else
          if(verticals[i] < 0){
            name <- paste0("today", verticals[i], ": ", format(today-verticals[i], "%d %b %y"))
          }
      
      p <- p %>%
        add_lines(x = as.numeric(today+verticals[i]), y = c(miny,maxy), text = NULL,
                  name = name,
                  color = I("black"), opacity = .5)
    }
    
    # Add vertical lines
    # p <- p %>%
    #   add_lines(x = as.numeric(today), y = c(miny,maxy), text = NULL, name = paste0("today: ", format(today, "%d %b %y")), color = I("black"), opacity = .5) %>%
    #   add_lines(x = as.numeric(today)+30, y = c(miny,maxy), text = NULL, name = paste0("today+30: ", format(today+30, "%d %b %y")), color = I("black"), opacity = .5) %>%
    #   add_lines(x = as.numeric(today)+60, y = c(miny,maxy), text = NULL, name = paste0("today+60: ", format(today+60, "%d %b %y")), color = I("black"), opacity = .5) %>%
    #   add_lines(x = as.numeric(today)+90, y = c(miny,maxy), text = NULL, name = paste0("today+90: ", format(today+90, "%d %b %y")), color = I("black"), opacity = .5)
    
    p
  })
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  
  # End CUB Server Logic
  ###############################################################################################################################
  
  
  
  
  
  ###############################################################################################################################
  # Start Filterable Server Logic
  
  f_start <- reactive({
    input$f_start_date
  })
  f_start_date <- debounce(f_start, 2000)
  
  months <- reactive({
    input$f_months
  })
  d_months <- debounce(months, 2000)
  
  
  # So if a new division is selected, it waits 2 secs before updating
  division <- reactive({
    input$f_division
  })
  d_division <- debounce(division, 2000)
  
  divisions <- reactive({
    dt <- dt()
    return(sort(unique(dt$Division)))
  })
  observe({
    updateSelectInput(
      session,
      "f_division",
      label = "Select Divisions to Display",
      choices = c("All", divisions()),
      selected = "All"
    )
  })
  
  
  program <- reactive({
    input$f_program
  })
  d_program <- debounce(program, 2000)
  
  programs <- reactive({
    # input$dataFile
    dt <- dt()
    
    if("All" %in% d_division()){
      divisions <- divisions()
    } else {
      divisions <- d_division()
    }
    
    return(sort(unique(dt[Division %in% divisions, Program])))
  })
  observe({
    updateSelectInput(
      session,
      "f_program",
      label = "Select Programs to Display",
      choices = c("All", programs()),
      selected = "All"
    )
  })
  
  
  
  
  people <- reactive({
    dt <- dt()
    
    if("All" %in% d_division()){
      divisions <- divisions()
    } else {
      divisions <- d_division()
    }
    
    if(input$filterby != "NA"){
      filter_column <- switch(
        input$filterby,
        "OTPO" = "OTPO",
        "MS" = "MS",
        "ORSA" = "ORSA",
        "TM" = "TM",
        "DM" = "DM",
        "Cyber" = "Cy",
        "Live Fire" = "LF"
      )
      filter_column <- which(names(dt) == filter_column)
      
      people <- dt[Division %in% divisions, ..filter_column] %>%
        unlist %>% strsplit(split = ", ") %>% unlist %>% unique %>% sort
      
      return(people)
    } else {
      return(NULL)
    }
  })
  
  observe({
    updateSelectInput(
      session,
      "people",
      label = "Select People to Display",
      choices = c("All", people()),
      selected = "All"
    )
  })
  
  # If changing filterby then reset to "All"
  observeEvent(input$filterby,{
    updateSelectInput(
      session,
      "people",
      label = "Select People to Display",
      choices = c("All", people()),
      selected = "All"
    )
  })
  
  selected_people <- reactive({
    input$people
  })
  d_people <- debounce(selected_people, 2000)
  
  
  
  f_show <- reactive({
    input$show
  })
  d_show <- debounce(f_show, 2000)
  
  
  
  f_out <- reactive({
    dt <- dt()
    
    today <- Sys.Date()
    
    start.date <- start_stop_month(x = f_start_date(), d_months())$start
    stop.date <- start_stop_month(x = f_start_date(), d_months())$stop

    
    setDT(dt)
    mindate <- dt[, min(StartDate, na.rm = TRUE)]
    mindate <- as.Date(trunc(as.POSIXct(mindate), "month"))
    maxdate <- dt[, max(StopDate, na.rm = TRUE)]
    maxdate <- as.Date(trunc(as.POSIXct(maxdate), "month"))
    maxdate <- seq(maxdate, length = 2, by = "1 month")[2]
    mindate <<- min(c(mindate, start.date))
    maxdate <<- max(c(maxdate, stop.date))
    
    
    if("All" %in% d_division()){
      divisions <- divisions()
    } else {
      divisions <- d_division()
    }
    if("All" %in% d_program()){
      programs <- programs()
    } else {
      programs <- d_program()
    }
    
    dt[, StopDate := as.Date(StopDate)]
    dt[, StartDate := as.Date(StartDate)]
    dt <- dt[Division %in% divisions & Program %in% programs]
    
    
    if("All" %in% d_people()){
      NULL
    } else{
      filter_column <- switch(
        input$filterby,
        "OTPO" = "OTPO",
        "MS" = "MS",
        "ORSA" = "ORSA",
        "TM" = "TM",
        "DM" = "DM",
        "Cyber" = "Cy",
        "Live Fire" = "LF"
      )

      selected <- unique(unlist(sapply(X = d_people(), function(x = X){
        grep(x, dt[,eval(parse(text = filter_column))])
      })))

      dt <- dt[ selected ]
      # dt <- dt[grep(d_people(), eval(parse(text = filter_column)))]
    }
    
    dt[is.na(EventName), EventName := Event]
    
    dt_sub <- dt[StepStatus == "Active",
                 .(ID, LeadOTA, Program, Division, Step, Product, Event, EventName, CRBIPRDate, IPR1Date,
                   IPR2Date, IPR3Date, IPR4aDate, IPR4bDate, IPR5Date, TEMPDate, FDSCDate, FOSDueDate,
                   OTRBDate, OTRRDate,
                   OTPO, ORSA, MS, TM, DM, Cy, LF
                 )]
    # dt[, .N, Step]
    
    
    #=============================================================================
    # start and stop events
    dt_ss <- dt[StepStatus == "Active",
                .(ID, LeadOTA, Program, Division, Step, Product, Event, EventName, StartDate, Duration,
                  OTPO, ORSA, MS, TM, DM, Cy, LF),
                keyby = .(StopDate)]
    date_range <- seq(start.date, stop.date, by = 1)
    date_range <- as.Date(date_range)
    
    # keep start and stop events that occur over the course of time
    # we're looking at
    btwn <- array(NA, dim = c(nrow(dt_ss), length(date_range)))
    for(i in seq_along(date_range)){
      btwn[,i] <- dt_ss[, between(date_range[i], StartDate, StopDate) ]
    }
    keep_rows <- which(apply(btwn, MARGIN = 1, FUN = function(x) any(x == TRUE)))
    dt_ss <- dt_ss[keep_rows]
    # dt_ss[, .N, Step]
    
    
    # Remove Monitor/Transition
    dt_ss <- dt_ss[Step != "Monitor/Transition"]
    #=============================================================================
    
    
    dt_sub_long <- melt(
      dt_sub,
      id.vars = c(
        "ID", "LeadOTA", "Program", "Division", "Step", "Product", "Event", "EventName",
        "OTPO", "ORSA", "MS", "TM", "DM", "Cy", "LF"
      ),
      measure.vars = c(
        "CRBIPRDate", "IPR1Date", "IPR2Date",
        "IPR3Date", "IPR4aDate", "IPR4bDate", "IPR5Date", "TEMPDate", "OTRBDate", "OTRRDate", "FDSCDate", "FOSDueDate",
      ),
      variable.name = "Review",
      value.name = "Date"
    )
    
    # Remove the word "Date" from the Review column
    dt_sub_long[, Review := gsub("Date", "", Review)]
    dt_sub_long_complete_cases <- dt_sub_long[!is.na(Date)]
    dt_sub_long_complete_cases[, Date := as.Date(Date)]
    
    # Set to sort by date and program by default
    setkey(dt_sub_long_complete_cases, Date, Program)
    
    dt <- dt_sub_long_complete_cases
    rm(list = c("dt_sub", "dt_sub_long", "dt_sub_long_complete_cases",
                "date_range", "keep_rows", "i", "btwn"))
    
    #=========================================================================
    # Only keep data in our range
    dt <- dt[between(Date, start.date, stop.date)]
    
    # Set to sort by date and program by default
    setkey(dt, Date, Program)
    
    # Create Label
    dt[, Label := trimws(paste(trimws(Program), Product, Review, EventName, sep = ", "))]
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # identify programs missing from dt
    missing_programs <- unique(dt_ss[, Program])[
      !(unique(dt_ss[, Program]) %in% unique(dt[, Program]))
    ]
    
    # add missing programs into dt so their
    # vertical plotting position is accounted for
    dt <- rbindlist(
      list(
        dt,
        dt_ss[Program %in% missing_programs]
      ), fill = TRUE
    )
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    #=============================================================================
    # So programs are sorted based on earliest review date 
    # Find the earliest review date per program
    dt[, Order := as.numeric(as.factor(Program))]
    dt[, ReverseOrd := (max(Order)+1)-Order]
    dt[, ReverseOrd := (ReverseOrd*2) - 1]
    #=============================================================================
    

    #==========================================================
    # Shift Steps so they do not overplot
    dt <- dt_step_shift(dt)
    dt[, ReverseOrdShift := ReverseOrd + Shift]
    #==========================================================
    
    dt[, ReverseOrdShiftTop := ReverseOrdShift+1/6]
    dt[, ReverseOrdShiftBottom := ReverseOrdShift-1/6]
    
    
    #==========================================================
    # Make Step a factor and order the factor levels
    dt[, .N, keyby = Step]
    dt_ss[, .N, keyby = Step]
    dt[, Step := factor(
      Step,
      levels = c(
        "Evaluation Planning", "Concept Planning", "Detailed Planning",
        "Event Execution", "Test Reporting", "Evaluation Reporting")
      )
    ]
    
    dt_ss[,.N,Step]
    dt_ss[,.N,Product]
    dt_ss[,.N,.(Step,Product)]
    dt_ss[Step == "Evaluation Reporting" & is.na(Product)]
    dt_prog <- dt_ss[
      Step %in% "TempSign" | #c("MS C", "TempSign") |
        Product %in% c(
          "MTS", "TEMP", "TEMPSign",                       #TEMP like docs
          "IAR", "OMAR", "SAR", "OAR", "OER", "Report"     #Reports
        )]
    
    dt_ss <- dt_ss[!(Step %in% c("MS C", "TempSign"))]
    
    dt_ss[,
          Step := factor(
            Step,
            levels = c(
              "Evaluation Planning", "Concept Planning", "Detailed Planning",
              "Event Execution", "Test Reporting", "Evaluation Reporting")
          )
    ]
    #==========================================================
    
    
    
    #===========================================================================
    # Add ReverseOrd by Program so we have plotting positions
    dt_ss <- merge(
      dt_ss,
      unique(dt[, .(Program, ReverseOrd)]),
      by = c("Program"), all.x = TRUE
    )
    
    # Shift steps so they do not overplot
    dt_ss <- dt_step_shift(dt_ss)
    dt_ss[, ReverseOrdShift := ReverseOrd + Shift]
    dt_ss[, ReverseOrdShiftTop := ReverseOrdShift+1/6]
    dt_ss[, ReverseOrdShiftBottom := ReverseOrdShift-1/6]
    

    dt_prog <- merge(
      dt_prog,
      unique(dt[, .(Program, Division, ReverseOrd)]),
      by = c("Program", "Division"), all.x = T)
    dt_ss[, DaysToStart := difftime(StartDate, today, units = "days")]
    dt_ss[, DaysToEnd := difftime(StopDate, today, units = "days")]
    
    
    dt_miles <- merge(
      dt_milestones(),
      unique(dt[, .(Program, Division, ReverseOrd)]),
      by = c("Program", "Division"), all.x = F)
    dt_miles <- dt_miles[!is.na(Date)]
    setkey(dt_miles, Program, Date)
    dt_miles[, DiffDays := as.numeric(difftime(Date, shift(Date,1), units = "days" )) , by = Program]
    dt_miles[, ReverseOrd := ReverseOrd + .8]
    
    # To avoid overplotting of the milestone events
    for(i in 1:4){
      dt_miles[, ReverseOrd := ifelse(
        !is.na(shift(Program)) & Program == shift(Program) & DiffDays < 60,
        shift(ReverseOrd-.4),
        ReverseOrd
        )
      ]
    }
    
    out <- list(dt = dt, dt_prog = dt_prog, dt_ss = dt_ss, dt_miles = dt_miles)
    return(out)
  })

  
  
  
  f_v <- reactive({
    input$f_verticals
  })
  f_verticals <- debounce(f_v, 2000)
  
  output$f_Plot <- renderPlotly({
    
    dt_all <- f_out()
    dt <- dt_all$dt
    dt_ss <- dt_all$dt_ss
    dt_prog <- dt_all$dt_prog
    dt_miles <- dt_all$dt_miles
    
    today <- Sys.Date()

    miny <- floor(min(dt$ReverseOrdShift))-5
    maxy <- ceiling(max(dt$ReverseOrdShift))+5

    start.date <- start_stop_month(x = f_start_date(), d_months())$start
    stop.date <- start_stop_month(x = f_start_date(), d_months())$stop
    
    len_days <<- difftime(stop.date, start.date, "days")[[1]]
    
    show_data <- d_show()
    steps <- ifelse("Steps" %in% show_data, TRUE, FALSE)
    temps_reports <- ifelse("TEMPs/Reports" %in% show_data, TRUE, FALSE)
    milestones <- ifelse("Milestones" %in% show_data, TRUE, FALSE)
    reviews <- ifelse("Reviews" %in% show_data, TRUE, FALSE)
    
    p <- plot_fun_plotly(
      dt, dt_ss, dt_prog, dt_miles,
      steps = steps, temps_reports = temps_reports, milestones = milestones,
      reviews = reviews, quarters = input$quarters, xaxis = input$xaxis) %>%
      ggplotly(tooltip = c("text")) %>%
      style(textposition = "right") %>%
      layout(xaxis = list(
        range = c(as.numeric(start.date), as.numeric(stop.date))
      )
      ) %>%
      config(
        toImageButtonOptions = list(
          format = "png", height = 750, width = 1400, scale = 3
        )
      )

    # Remove the hover from the geom_text
    p_json <- plotly_json(p, FALSE)
    # p_json <- plotly_json(p)
    # print(paste0(fromJSON(p_json$x$data)$data$type, ": ",
    #              fromJSON(p_json$x$data)$data$name))


    traces <- fromJSON(p_json)$data$mode
    if("text" %in% traces){
      #dont_trace <- fromJSON(p_json)$data$mode == "text"
      dont_trace <- is.na(fromJSON(p_json)$data$mode)

      p <- plot_fun_plotly(
        dt, dt_ss, dt_prog, dt_miles,
        steps = steps, temps_reports = temps_reports, milestones = milestones,
        reviews = reviews, quarters = input$quarters, xaxis = input$xaxis) %>%
        ggplotly(tooltip = c("text")) %>%
        style(textposition = "right", hoverinfo = "none", traces = dont_trace) %>%
        layout(xaxis = list(
          range = c(as.numeric(start.date), as.numeric(stop.date))
        )
        ) %>%
        config(
          toImageButtonOptions = list(
            format = "png", height = 750, width = 1400, scale = 3
          )
        )
    }

    
    # Add vertical lines
    verticals <- strsplit(f_verticals(), ",") %>% unlist %>% as.numeric
    nans <- which(is.na(verticals))
    if( !identical(nans, integer(0)) ){
      verticals <- verticals[-nans]
    }
    
    for(i in seq_along(verticals)){
      if(verticals[i] == 0){
        name <- paste0("today: ", format(today, "%d %b %y"))
      } else
        if(verticals[i] > 0){
          name <- paste0("today+", verticals[i], ": ", format(today+verticals[i], "%d %b %y"))
        } else
          if(verticals[i] < 0){
            name <- paste0("today", verticals[i], ": ", format(today-verticals[i], "%d %b %y"))
          }
      
      p <- p %>%
        add_lines(x = as.numeric(today+verticals[i]), y = c(miny,maxy), text = NULL,
                  name = name,
                  color = I("black"), opacity = .5)
    }
    p
  })
  
  # #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # output$f_table <- renderDT(server = FALSE, {                                      #
  #   dt <- f_out()$dt
  #   setDT(dt)
  #   #dt <- #dt[, .(ID, Program, Division, Step, Product, Event, EventName, Review, Date, Order, ReverseOrdShift)]
  #   
  #   datatable(                                                                         #
  #     dt,                                                                              #
  #     filter = list(position = "top", clear = TRUE, plain = TRUE),                     #
  #     rownames = FALSE,                                                                #
  #     style = "bootstrap",                                                             #
  #     extensions = "Buttons",                                                          #
  #     selection = "multiple",                                                          #
  #     class = "compact hover cell-border stripe order-column",                         #
  #     options = list(                                                                  #
  #       buttons = list(                                                                #
  #         list(extend = "excel", exportOptions = list(modifier = list(page = "all"))), #
  #         list(extend = "csv", exportOptions = list(modifier = list(page = "all"))),   #
  #         list(                                                                        #
  #           extend = "pdf",                                                            #
  #           orientation = "landscape",                                                 #
  #           exportOptions = list(modifier = list(page = "all"))                        #
  #         ),                                                                           #
  #         list(extend = "copy", exportOptions = list(modifier = list(page = "all"))),  #
  #         list(extend = "print", exportOptions = list(modifier = list(page = "all")))  #
  #       ),                                                                             #
  #       dom = "Bfrtip",                                                                #
  #       autowidth = TRUE,                                                              #
  #       pageLength = 50,                                                               #
  #       order = list(list(10,"dsc"))                                                   #
  #     )                                                                                #
  #   ) }                                                                                #
  # )                                                                                    #
  # #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # End Filterable Server Logic
  ###############################################################################################################################
  
  
  
  
  
  
  ###############################################################################################################################
  # Start DCD Server Logic
  
  start_dcd <- reactive({
    input$dcd_start_date
  })
  dcd_start_date <- debounce(start_dcd, 2000)  
  
  
  months_dcd <- reactive({
    input$dcd_months
  })
  dcd_months <- debounce(months_dcd, 2000)
  
  device_type <- reactive({
    input$device_type
  })
  device_type_d <- debounce(device_type, 2000)


  dcd_out <- reactive({
  dt <- dt()

  start.date <- start_stop_month(x = dcd_start_date(), dcd_months())$start
  stop.date <- start_stop_month(x = dcd_start_date(), dcd_months())$stop


  dt_sub2 <- dt[
    StepStatus == "Active" & Step == "Event Execution" &
      #(Event %in% c("OA", "OT", "Special Project")) &
      StartDate <= (seq(start.date, length = 2, by = paste0(dcd_months(), " months") )[2]),
    .(
      ID, LeadOTA, Program, Division, Event, EventName,
      DMPrimary, TMPrimary,
      NumDCDRequested, NumOfMesaTablet, NumOfGetacTablet, NumOfGetacLaptop
    ),
    keyby = .(Start = StartDate, Stop = StopDate)]
  
  
  # Add data
  dt_sub2[, `DCD Prep, V&V Start` := Start-45]
  dt_sub2[, `DCD Prep, V&V Stop` := `DCD Prep, V&V Start`+30]
  
  dt_sub2[, `Ship DCD to Test Site Start` := `DCD Prep, V&V Stop`]
  dt_sub2[, `Ship DCD to Test Site Stop` := `Ship DCD to Test Site Start`+15]
  
  dt_sub2[, `Ship DCD to MCOTEA Start` := Stop]
  dt_sub2[, `Ship DCD to MCOTEA Stop` := `Ship DCD to MCOTEA Start`+15]
  
  dt_sub2[, `DCD Reconfiguration Start` := `Ship DCD to MCOTEA Stop`]
  dt_sub2[, `DCD Reconfiguration Stop` := `DCD Reconfiguration Start`+7]
  
  dt_sub2[, `Event Execution Start` := Start]
  dt_sub2[, `Event Execution Stop` := Stop]
  
  dt_sub2[, Start := NULL]
  dt_sub2[, Stop := NULL]
  
  # turn data into long form
  # names(dt_sub2)
  dt_long2 <- melt.data.table(
    dt_sub2,
    id.vars = c(
      "ID", "LeadOTA", "Division", "Program", "Event", "EventName",
      "DMPrimary", "TMPrimary",
      "NumDCDRequested", "NumOfMesaTablet", "NumOfGetacTablet", "NumOfGetacLaptop"),
    variable.name = "Action", value.name = "Date"
  )
  dt_long2[, StartStop := "Start"]
  dt_long2[grepl("Stop", Action), StartStop := "Stop"]
  dt_long2[, Action := gsub("Start", "", Action)]
  dt_long2[, Action := gsub("Stop", "", Action)]
  
  # Separate Start and Stops
  dt_final2 <- dcast.data.table(
    dt_long2,
    ID + LeadOTA + Division + Program + Event + EventName +
      DMPrimary + TMPrimary +
      NumDCDRequested + NumOfMesaTablet + NumOfGetacTablet + NumOfGetacLaptop + Action ~ StartStop,
    value.var = "Date")
  
  # Make Action a factor and order the factor levels
  #dt_final2[, .N, Action]
  dt_final2[, Action := trimws(Action)]
  # dt_final2[, Action]
  dt_final2[, Action := factor(
    Action,
    levels = c(
      "DCD Prep, V&V", "Ship DCD to Test Site", "Event Execution",
      "Ship DCD to MCOTEA", "DCD Reconfiguration"
    )
  )
  ]
  
  dt_final2[is.na(NumDCDRequested), NumDCDRequested := 0]
  dt_final2[is.na(NumOfMesaTablet), NumOfMesaTablet := 0]
  dt_final2[is.na(NumOfGetacTablet), NumOfGetacTablet := 0]
  dt_final2[is.na(NumOfGetacLaptop), NumOfGetacLaptop := 0]
  
  # Keep only the program that have some DCD request
  devices <- device_type_d()
  if(input$show_only == TRUE){
    # dt_final2 <- dt_final2[
    #   NumDCDRequested > 0 | NumOfMesaTablet > 0 | NumOfGetacTablet > 0 | NumOfGetacLaptop > 0
    # ]
    
    dt_final2 <-
      dt_final2[
        eval(parse(text = paste(
          ifelse("DCD" %in% devices, "NumDCDRequested > 0" , "NA"),
          ifelse("Mesa Tablet" %in% devices, "NumOfMesaTablet > 0", "NA"),
          ifelse("Getac Tablet" %in% devices, "NumOfGetacTablet > 0", "NA"),
          ifelse("Getac Laptop" %in% devices, "NumOfGetacLaptop > 0", "NA"), sep = "|"
          ))
        )
      ]
    
  }
  
  # Order based on the earlist Start
  dt_final2[, Earliest := min(Start), by = ID]
  setkey(dt_final2, Earliest, Action)
  dt_final2[, Row := .I]
  dt_final2[, Order := min(Row), by = ID]
  dt_ord2 <- unique(dt_final2[, .(ID, Order)])[, .(ID, OrderU = order(Order))]
  dt_final2 <- merge(dt_final2, dt_ord2, by = "ID")
  setkey(dt_final2, Earliest, Action)
  
  # Reverse Order
  dt_final2[, ReverseOrd := (max(OrderU)+1)-OrderU]
  
  # Expand so each program takes up 2 units on the y-axis and is centered on
  # an odd value
  dt_final2[, ReverseOrd := (ReverseOrd*2) - 1]
  
  
  dcd_request <- unique(
    dt_final2[
      Action %in% c("DCD Prep, V&V", "DCD Reconfiguration"),
      .(ID, LeadOTA, Division, Program, Event, EventName, Action,
        Start, Stop, TMPrimary, DMPrimary,
        NumDCDRequested, NumOfMesaTablet, NumOfGetacTablet, NumOfGetacLaptop)
    ]
  )

  dcd_request <- melt.data.table(
    dcd_request,
    id.vars = c(
      "ID", "LeadOTA", "Division", "Program", "Event", "EventName",
      "Action", "TMPrimary", "DMPrimary",
      "NumDCDRequested", "NumOfMesaTablet", "NumOfGetacTablet", "NumOfGetacLaptop"),
    variable.name = "SS", value.name = "Date")

  dcd_request <- dcd_request[
    (Action == "DCD Prep, V&V" & SS == "Start") |
      (Action == "DCD Reconfiguration" & SS == "Stop"), ]

  setkey(dcd_request, Date)
  
    
  dcd_request[, NumDCDRequested := ifelse(SS == "Start", NumDCDRequested, -1*abs(NumDCDRequested))]
  dcd_request[, NumOfMesaTablet := ifelse(SS == "Start", NumOfMesaTablet, -1*abs(NumOfMesaTablet))]
  dcd_request[, NumOfGetacTablet := ifelse(SS == "Start", NumOfGetacTablet, -1*abs(NumOfGetacTablet))]
  dcd_request[, NumOfGetacLaptop := ifelse(SS == "Start", NumOfGetacLaptop, -1*abs(NumOfGetacLaptop))]
    
  dcd_request[, DCDsInUse := 0]
  dcd_request[, DCDsInUse := cumsum(NumDCDRequested)]
  dcd_request[, MesasInUse := 0]
  dcd_request[, MesasInUse := cumsum(NumOfMesaTablet)]
  dcd_request[, GetacTabsInUse := 0]
  dcd_request[, GetacTabsInUse := cumsum(NumOfGetacTablet)]
  dcd_request[, GetacLapsInUse := 0]
  dcd_request[, GetacLapsInUse := cumsum(NumOfGetacLaptop)]

  
  
  dcd_request[, ProgramsUsing := ifelse(SS == "Start", 1, -1)]
  dcd_request[, ProgramsUsing := cumsum(ProgramsUsing)]

  uids <- unique(dcd_request$ID)
  for(i in seq_along( uids )){
    plot_positions <- 1:length(uids)

    if(i == 1) {
      id <- uids[i]
      dcd_request[ID == id, PlotPosition := 1]
      next
    }

    id <- uids[i]
    t1 <- dcd_request[ID == id & SS == "Start", Date]
    t2 <- dcd_request[ID == id & SS == "Stop", Date]

    ids <- uids[( !(uids %in% id) )]

    overlap <- rep(FALSE, length(ids))
    for(j in seq_along( ids )){
      jid <- ids[j]
      tt1 <- dcd_request[ID == jid & SS == "Start", Date]
      tt2 <- dcd_request[ID == jid & SS == "Stop", Date]

      overlap[j] <- int_overlaps(
        interval(t1, t2),
        interval(tt1, tt2)
      )
    }

    # dcd_request[ID %in% ids[overlap]]
    used_plot_positions <- dcd_request[ID %in% ids[overlap], PlotPosition]

    pp <- min(plot_positions[!(plot_positions %in% used_plot_positions)])
    dcd_request[ID == id, PlotPosition := pp]
  }
  # dcd_request
  
  
  
  dcd_request[,
    DCDs := max(
      ifelse("DCD" %in% devices, NumDCDRequested, 0) +
        ifelse("Mesa Tablet" %in% devices, NumOfMesaTablet, 0) +
        ifelse("Getac Tablet" %in% devices, NumOfGetacTablet, 0) +
        ifelse("Getac Laptop" %in% devices, NumOfGetacLaptop, 0)
      ),
    by = ID]
  dcd_request[, DCDs := factor(DCDs, levels = sort(unique(DCDs)))]

  dcd_request_wide <- dcast.data.table(dcd_request,
    ID + LeadOTA + Division + Program + Event + EventName +
      TMPrimary + DMPrimary + DCDs + PlotPosition ~ SS, value.var = "Date"
  )
  dcd_request_wide[, x := mean(c(Start, Stop)), by = ID]
  dcd_request_wide[is.na(EventName), EventName := Event]

  
  dcd_request_long <- melt.data.table(
    dcd_request,
    id.vars = c(
      "ID", "LeadOTA", "Division", "Program", "Event", "EventName", "PlotPosition",
      "TMPrimary", "DMPrimary", "SS", "Date", "DCDs"),
    measure.vars = c(
      "DCDsInUse", "MesasInUse", "GetacTabsInUse", "GetacLapsInUse"),
    variable.name = "Device", value.name = "Qty"
  )
  dcd_request_long[Device == "DCDsInUse", Device := "DCD"]
  dcd_request_long[Device == "MesasInUse", Device := "Mesa Tablet"]
  dcd_request_long[Device == "GetacTabsInUse", Device := "Getac Tablet"]
  dcd_request_long[Device == "GetacLapsInUse", Device := "Getac Laptop"]
  
  dcd_request_long <- dcd_request_long[Device %in% devices]
  
  return(
    list(
      dcd_request_wide = dcd_request_wide,
      dcd_request = dcd_request_long
    )
  )
  
  })
  
  #======================================
  
  dcd_v <- reactive({
    input$dcd_verticals
  })
  dcd_verticals <- debounce(dcd_v, 2000)
  
  output$dcd_Plot <- renderPlotly({

    start.date <- start_stop_month(x = dcd_start_date(), dcd_months())$start
    stop.date <- start_stop_month(x = dcd_start_date(), dcd_months())$stop
    
    start.datex <- start_stop_month(x = dcd_start_date()-100, dcd_months()+7)$start
    stop.datex <- start_stop_month(x = dcd_start_date()-100, dcd_months()+7)$stop

    dcd_request <- dcd_out()$dcd_request
    dcd_request_wide <- dcd_out()$dcd_request_wide
    
  cum_dcd_plot <- ggplot(dcd_request) +
    geom_point(
      aes(x = Date, y = Qty,
          text = paste('Device:', Device)), size = 0.1, colour = "transparent"
    ) +
    geom_step(
      aes(x = Date, y = Qty, colour = Device, linetype = Device,
          text = paste('Device:', Device))
    ) +
    labs(
      y = "DCDs in Use", x = ""
    ) +
    scale_x_date(
      breaks = seq(start.datex, stop.datex, by = "month"),
      date_labels = "%d\n%b\n%y",
      expand = c(0,0)
    ) +
    theme(
      panel.grid.minor = element_blank(),
      plot.caption = element_text(colour = "darkgreen", size = 10),
      legend.title.align = 0.5,
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8),
      panel.grid.major.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.margin = margin(t = 5.5, r = 5.5, b = -10, l = 5.5, unit = "pt")
    ) +
    scale_y_continuous(
      breaks = seq(0,500,5),
      expand = expansion(add = c(0, 5))
    ) +
    scale_color_manual(values = cbbPalette)
    #coord_cartesian(xlim = c(start.date, stop.date))

  ymax <- ggplot_build(cum_dcd_plot)$layout$panel_params[[1]]$y.range[2]
  pwr <- nchar(ymax)-1
  pp_nchar <- nchar(dcd_request[, max(PlotPosition)])-1
  pwr <- pwr-pp_nchar
  
  events_plot <- ggplot(dcd_request) +
    geom_ribbon(
      aes(
        x = Date,
        ymin = PlotPosition*10^(pwr)-.33*10^(pwr),
        ymax = PlotPosition*10^(pwr)+.33*10^(pwr),
        fill = DCDs, group = interaction(ID, Program)
      )
    ) +
    scale_fill_brewer("", palette = "Spectral", direction = -1) +
    #scale_fill_brewer( type = "seq", palette = 7,  direction = 1,  aesthetics = "fill") +
    geom_text(
      data = dcd_request_wide,
      mapping = aes(x = x, y = PlotPosition*10^(pwr), label = paste0(Program, ", ", Event, ", ", DCDs),
                    text = paste(
                      'Division:', Division,
                      '</br></br>Lead OTA:', LeadOTA,
                      '</br>Program:', Program,
                      '</br>EventName:', EventName,
                      '</br>DMPrimary:', DMPrimary,
                      '</br>StartDate:', Start,
                      '</br>StopDate:', Stop,
                      '</br>DCDs:', DCDs
                    )),
      fontface = "bold", size = 2.5
    ) +
    labs(
      y = "Plotting Position", x = "", #"Date"#,
      #caption = 
      #  paste0("Data pulled from ASM datFormProgram on ", as.Date(dt_mtime), ".")
    ) +
    scale_x_date(
      breaks = seq(start.datex, stop.datex, by = "month"),
      date_labels = "%b\n%y",
      expand = c(0,0)
    ) +
    theme(
      panel.grid.minor = element_blank(),
      plot.caption = element_text(colour = "darkgreen", size = 10),
      legend.title.align = 0.5,
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8),
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(colour = "transparent"),
      axis.title.y = element_text(colour = "transparent"),
      axis.ticks.y = element_line(colour = "transparent"),
      legend.position = "bottom",
      plot.margin = margin(t = 0, r = 5.5, b = 5.5, l = 5.5, unit = "pt")
    ) #+
    #coord_cartesian(xlim = c(start.date, stop.date))
  
  # grid.arrange(
  #   cum_dcd_plot,
  #   events_plot,
  #   nrow = 2,
  #   ncol = 1,
  #   heights = c(2, 1.5)
  # )
  
  p1 <- ggplotly(cum_dcd_plot, tooltip = c("x", "y", "text"))
  p2 <- ggplotly(events_plot, tooltip = "text")
  
  miny <- -10
  maxy <- 200
  
  # Add vertical lines
  verticals <- strsplit(dcd_verticals(), ",") %>% unlist %>% as.numeric
  nans <- which(is.na(verticals))
  if( !identical(nans, integer(0)) ){
    verticals <- verticals[-nans]

  }
  
  today <- Sys.Date()
  for(i in seq_along(verticals)){
    if(verticals[i] == 0){
      name <- paste0("today: ", format(today, "%d %b %y"))
    } else
    if(verticals[i] > 0){
      name <- paste0("today+", verticals[i], ": ", format(today+verticals[i], "%d %b %y"))
    } else
    if(verticals[i] < 0){
      name <- paste0("today", verticals[i], ": ", format(today-verticals[i], "%d %b %y"))
    }
  
    p1 <- p1 %>%
      add_lines(x = as.numeric(today+verticals[i]), y = c(miny,maxy), text = NULL,
                name = name,
                color = I("black"),
                opacity = .5,
                fill = I("black"))
    
    p2 <- p2 %>%
       add_lines(x = as.numeric(today+verticals[i]), y = c(miny,maxy), text = NULL,
                 name = name,
                 color = I("black"), opacity = .5, fill = I("black"))
  }
  
  p1 <- p1 %>%
    layout(
      xaxis = list(
        range = as.numeric(c(start.date, stop.date)),
        constrain="domain"
      )
    )

  p2 <- p2 %>%
    layout(
      xaxis = list(
        range = as.numeric(c(start.date, stop.date)),
        constrain="domain"
      )
    )
  
  p3 <- subplot(
    p1,
    p2,
    nrows = 2,
    heights = c(2, 1.5)/sum(c(2, 1.5))
  )
  p3
  
  })
  # End DCD Server Logic
  ###############################################################################################################################
  
  
  
  
  
  ###############################################################################################################################
  # Start DCD2 Server Logic
  
  start_dcd2 <- reactive({
    input$dcd_start_date2
  })
  dcd_start_date2 <- debounce(start_dcd2, 2000)  
  
  
  months_dcd2 <- reactive({
    input$dcd_months2
  })
  dcd_months2 <- debounce(months_dcd2, 2000)
  
  
  dcd_out2 <- reactive({
    dt <- dt()
    
    start.date <- start_stop_month(x = dcd_start_date2(), dcd_months2())$start
    stop.date <- start_stop_month(x = dcd_start_date2(), dcd_months2())$stop
    
    
    dt_sub2 <- dt[
      StepStatus == "Active" & Step == "Event Execution" &
        #(Event %in% c("OA", "OT", "Special Project")) &
        # StartDate <= (seq(start.date, length = 2, by = paste0(dcd_months2(), " months") )[2])
      (between(StartDate, start.date, stop.date) | between(StopDate, start.date, stop.date))
      ,
      .(ID, LeadOTA, Program, Division, Event, EventName, DMPrimary, TMPrimary,
        #NumDCDRequested,
        NumOfMesaTablet, NumOfGetacTablet, NumOfGetacLaptop
      ),
      keyby = .(Start = StartDate, Stop = StopDate)]
    
    
    # Add data
    dt_sub2[, `DCD Prep, V&V Start` := Start-45]
    dt_sub2[, `DCD Prep, V&V Stop` := `DCD Prep, V&V Start`+30]
    
    dt_sub2[, `Ship DCD to Test Site Start` := `DCD Prep, V&V Stop`]
    dt_sub2[, `Ship DCD to Test Site Stop` := `Ship DCD to Test Site Start`+15]
    
    dt_sub2[, `Ship DCD to MCOTEA Start` := Stop]
    dt_sub2[, `Ship DCD to MCOTEA Stop` := `Ship DCD to MCOTEA Start`+15]
    
    dt_sub2[, `DCD Reconfiguration Start` := `Ship DCD to MCOTEA Stop`]
    dt_sub2[, `DCD Reconfiguration Stop` := `DCD Reconfiguration Start`+7]
    
    dt_sub2[, `Event Execution Start` := Start]
    dt_sub2[, `Event Execution Stop` := Stop]
    
    dt_sub2[, Start := NULL]
    dt_sub2[, Stop := NULL]
    
    # turn data into long form
    dt_long2 <- melt.data.table(
      dt_sub2,
      id.vars = c(
        "ID", "LeadOTA", "Division", "Program", "Event", "EventName",
        "DMPrimary", "TMPrimary",
        "NumOfMesaTablet", "NumOfGetacTablet", "NumOfGetacLaptop"),
      variable.name = "Action", value.name = "Date"
    )
    dt_long2[, StartStop := "Start"]
    dt_long2[grepl("Stop", Action), StartStop := "Stop"]
    dt_long2[, Action := gsub("Start", "", Action)]
    dt_long2[, Action := gsub("Stop", "", Action)]
    
    # Separate Start and Stops
    dt_final2 <- dcast.data.table(
      dt_long2,
      ID + LeadOTA + Division + Program + Event + EventName + DMPrimary + TMPrimary +
      NumOfMesaTablet + NumOfGetacTablet + NumOfGetacLaptop + Action ~ StartStop,
      value.var = "Date")
    
    # Make Action a factor and order the factor levels
    #dt_final2[, .N, Action]
    dt_final2[, Action := trimws(Action)]

    dt_final2[, Action := factor(
      Action,
      levels = c(
        "DCD Prep, V&V", "Ship DCD to Test Site", "Event Execution",
        "Ship DCD to MCOTEA", "DCD Reconfiguration"
      )
    )
    ]
    
    
    #dt_final2[is.na(NumDCDRequested), NumDCDRequested := 0]
    dt_final2[is.na(NumOfMesaTablet), NumOfMesaTablet := 0]
    dt_final2[is.na(NumOfGetacTablet), NumOfGetacTablet := 0]
    dt_final2[is.na(NumOfGetacLaptop), NumOfGetacLaptop := 0]
    
    # Keep only the programs that have some DCD request
    dt_final2 <- dt_final2[
      # NumDCDRequested > 0 |
      NumOfMesaTablet > 0 | NumOfGetacTablet > 0 | NumOfGetacLaptop > 0]
    
    # Order based on the earliest Start
    dt_final2[, Earliest := min(Start), by = ID]
    setkey(dt_final2, Earliest, Action)
    dt_final2[, Row := .I]
    dt_final2[, Order := min(Row), by = ID]
    dt_ord2 <- unique(dt_final2[, .(ID, Order)])[, .(ID, OrderU = order(Order))]
    dt_final2 <- merge(dt_final2, dt_ord2, by = "ID")
    setkey(dt_final2, Earliest, Action)
    
    # Reverse Order
    dt_final2[, ReverseOrd := (max(OrderU)+1)-OrderU]
    
    # Expand so each program takes up 2 units on the y-axis and is centered on
    # an odd value
    dt_final2[, ReverseOrd := (ReverseOrd*2) - 1]
    
    
    dcd_request <- unique(
      dt_final2[
        #(between(Start, start.date, stop.date) | between(Stop, start.date, stop.date))
        ,
        .(ID, LeadOTA, Division, Program, Event, EventName, Action, Start, Stop,
          NumOfMesaTablet, NumOfGetacTablet, NumOfGetacLaptop,
          TMPrimary, DMPrimary)
      ]
    )
    
    dcd_request <- melt.data.table(
      dcd_request,
      id.vars = c(
        "ID", "LeadOTA", "Division", "Program", "Event", "EventName", "Action",
        "NumOfMesaTablet", "NumOfGetacTablet", "NumOfGetacLaptop",
        "TMPrimary", "DMPrimary"),
      variable.name = "SS", value.name = "Date")
    
    # dcd_request <- dcd_request[
    #   (Action == "DCD Prep, V&V" & SS == "Start") |
    #     (Action == "DCD Reconfiguration" & SS == "Stop"), ]
    
    setkey(dcd_request, Date)

    dcd_request[, NumOfMesaTablet := ifelse(SS == "Start", NumOfMesaTablet, -1*abs(NumOfMesaTablet))]
    dcd_request[, NumOfGetacTablet := ifelse(SS == "Start", NumOfGetacTablet, -1*abs(NumOfGetacTablet))]
    dcd_request[, NumOfGetacLaptop := ifelse(SS == "Start", NumOfGetacLaptop, -1*abs(NumOfGetacLaptop))]
    
    dcd_request[, MesasInUse := 0]
    dcd_request[, MesasInUse := cumsum(NumOfMesaTablet)]
    dcd_request[, GetacTabsInUse := 0]
    dcd_request[, GetacTabsInUse := cumsum(NumOfGetacTablet)]
    dcd_request[, GetacLapsInUse := 0]
    dcd_request[, GetacLapsInUse := cumsum(NumOfGetacLaptop)]
    
    dcd_request_long2 <- melt.data.table(
      dcd_request,
      id.vars = c(
        "ID", "LeadOTA", "Division", "Program", "Event", "EventName", "Action",
        "TMPrimary", "DMPrimary", "SS", "Date"),
      measure.vars = c(
        "NumOfMesaTablet", "NumOfGetacTablet", "NumOfGetacLaptop"),
      variable.name = "Device", value.name = "Qty"
    )
    
    
    dcd_request_long2 <- dt_early_order(dcd_request_long2)
    
    # Rename columns
    dcd_request_long2[Device == "NumOfMesaTablet", Device := "Mesa Tablet"]
    dcd_request_long2[Device == "NumOfGetacTablet", Device := "Getac Tablet"]
    dcd_request_long2[Device == "NumOfGetacLaptop", Device := "Getac Laptop"]

    setkey(dcd_request_long2, ReverseOrd)
    
    # Add label to include device name and quantity
    dt_temp <- dcd_request_long2[
      abs(Qty) > 0,
      .(MaxDate = max(Date), Label = paste0(Device, ": ", abs(Qty) ) ),
      .(Program, ID, LeadOTA, Division, Event, EventName, TMPrimary, DMPrimary, ReverseOrd)
    ] %>% unique
    
    
    # Keep ID and add "\n" to end of each label
    dt_labs <- dt_temp[, .(ID, V1 = paste0(Label, "\n"))]
    
    # For each unique ID, collapse them together and remove the last "\n"
    u_labs <- unique(dt_labs$ID )
    for(i in seq_along( u_labs ) ){
      temp <- paste(dt_labs[ID == u_labs[i]]$V1, collapse = "")
      dt_labs[ID == u_labs[i] , Label := substr( temp, 1,  nchar(temp)-1)]
    }
    dt_labs <- dt_labs[, V1 := NULL]
    dt_labs <- unique(dt_labs)

    # Merge so we have labels
    dt_label <- merge(
      dt_temp[, .(Program, ID, LeadOTA, Division, Event, EventName,
                  TMPrimary, DMPrimary, ReverseOrd, MaxDate)] %>% unique,
      dt_labs, by = "ID"
    )
    
    dt_ribbon <- dcd_request_long2[
      abs(Qty)>0,
      .(ID, LeadOTA, Division, Program, Action, Event, EventName, Date, ReverseOrd)
    ] %>% unique
    
    dt_ribbon <- merge(
      dt_ribbon,
      dt_label[, .(ID, Label)],
      by = "ID"
    )
    dt_ribbon[, Label := gsub("\n", "</br> ", Label)]
    
    return(
      list(
        dt_ribbon = dt_ribbon,
        dt_label = dt_label
      )
    )
    
  })
  
  
  
  
  #======================================================
  
  
  dcd_v2 <- reactive({
    input$dcd_verticals2
  })
  dcd_verticals2 <- debounce(dcd_v2, 2000)
  
  output$dcd_Plot2 <- renderPlotly({
    
    dt_ribbon <- dcd_out2()$dt_ribbon
    dt_label <- dcd_out2()$dt_label
    
    start.date <- start_stop_month(x = dcd_start_date2(), dcd_months2())$start
    stop.date <- start_stop_month(x = dcd_start_date2(), dcd_months2())$stop
    
    start.datex <- start_stop_month(x = dcd_start_date2()-100, dcd_months2()+7)$start
    stop.datex <- start_stop_month(x = dcd_start_date2()-100, dcd_months2()+7)$stop
    
  device_plot2 <- function(dt, dt2, start.date = start.date, stop.date = stop.date){
    setkey(dt, ReverseOrd)
    
    program_id_count <- dt[, .N, .(ID, Program)][, .N, Program]
    dt <- merge(dt, program_id_count, by = "Program")

    if( dt[, max(N)] > 1 ){
      dt_n <- unique(dt[N > 1, .(MinDate = min(Date)), .(ID, Program, N)])
      dt_n[, Ord := order(MinDate), Program]
      dt <- merge(
        dt, dt_n,
        by = c("ID", "Program", "N"), all = TRUE)
      dt[is.na(Ord), Ord := 0]
    
      seq2 <- Vectorize(seq.default, vectorize.args = c("from", "to", "length.out"))
      dt[N > 1, ROS := ReverseOrd + ((seq2( (N-1), -(N-1), length.out = N))[Ord])/N]
      dt[is.na(ROS), ROS := ReverseOrd]
    } else {
      dt[, ROS := ReverseOrd]
    }
    
    dt[, ROST := ROS + 1/(N+1)]
    dt[, ROSB := ROS - 1/(N+1)]
    
    dt2 <- unique(merge(dt2, dt[, .(ID, ROS)], by = "ID", all.x = TRUE, all.y = FALSE))

    # Base plot
    my_plot <- dt %>%
      ggplot()
    
    # color every other row to help indicate distinctions between programs
    my_seq <- seq(dt[,min(ReverseOrd)]-1, dt[, max(ReverseOrd)]-1, by = 2)
    for(i in seq_along(my_seq)){
      my_plot <- my_plot + geom_ribbon(
        data = data.frame(
          x = c(start.datex, stop.datex),
          ymin = c(my_seq[i],my_seq[i]),
          ymax = c(my_seq[i]+2,my_seq[i]+2)
        ),
        aes(x = x, ymin = ymin, ymax = ymax),
        fill = ifelse(i %% 2 == 0, "grey70", "transparent"),
        alpha = ifelse(i %% 2 == 0, .2, 0)
      )
    }
    
    my_plot <- my_plot +
      scale_y_continuous(
        breaks = dt[, sort(unique(ReverseOrd))],
        labels = (unique(dt[, Program, keyby = ReverseOrd])[,Program]),
        expand = c(0.01,0.01)
      ) +
      scale_x_date(
        breaks = seq(start.datex, stop.datex, by = "month"),
        date_labels = "%b\n%y",
        expand = c(0,0)
      )
    
    my_plot <- my_plot +
      geom_ribbon(
        data = dt,
        aes(
          x = Date,
          # ymin = ReverseOrd-.5, ymax = ReverseOrd+.5,
          ymin = ROSB, ymax = ROST,
          #alpha = Action,
          fill = Action,
          group = interaction(ID, Program,  Action),
          text = paste(
            'Division:', Division,
            '</br></br>Lead OTA:', LeadOTA,
            '</br>Program:', Program,
            '</br>Date:', Date,
            '</br>Event:', Event,
            '</br>Event Name:', EventName,
            '</br>Action:', Action,
            '</br></br>Devices:</br>', Label
          )
        ), alpha = .7
      ) +
      scale_fill_manual(values = c(
        "DCD Prep, V&V" = cbPalette[1],
        "Ship DCD to Test Site" = cbbPalette[1],
        "Event Execution" = cbbPalette[2],
        "Ship DCD to MCOTEA" = cbbPalette[3],
        "DCD Reconfiguration" = cbbPalette[4]
      )
      )
    
    my_plot <- my_plot +
      geom_text(
        data = dt2,
        aes(x = MaxDate,
            # y = ReverseOrd,
            y = ROS,
            label = Label,
             text = paste(
               'Division:', Division,
               '</br></br>Lead OTA:', LeadOTA,
               '</br>Program:', Program,
               '</br>EndDate:', MaxDate,
               '</br>Event:', Event,
               '</br>Event Name:', EventName,
               '</br></br>Devices:</br>', Label
               )
        ), size = 3.0, hjust = 0
      )
    
    my_plot <- my_plot +
      #coord_cartesian(xlim = c(start.date, stop.date)) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
      ) +
      labs(x = "", y = "Program")
    
    my_plot
  }
  
  p <- device_plot2(
    dt = dt_ribbon,
    dt2 = dt_label,
    start.date = start.date,
    stop.date = stop.date
  ) %>%
    ggplotly(tooltip = "text") %>%
    style(textposition = "right") %>%
    layout(
      xaxis = list(
        range = as.numeric(c(start.date, stop.date)),
        constrain="domain"
      )
    )
    
  
  
  miny <- dt_ribbon[, min(ReverseOrd)-2]
  maxy <- dt_ribbon[, max(ReverseOrd)+2]
  
  # Add vertical lines
  verticals <- strsplit(dcd_verticals2(), ",") %>% unlist %>% as.numeric
  nans <- which(is.na(verticals))
  if( !identical(nans, integer(0)) ){
    verticals <- verticals[-nans]
  }
  
  today <- Sys.Date()
  for(i in seq_along(verticals)){
    if(verticals[i] == 0){
      name <- paste0("today: ", format(today, "%d %b %y"))
    } else
      if(verticals[i] > 0){
        name <- paste0("today+", verticals[i], ": ", format(today+verticals[i], "%d %b %y"))
      } else
        if(verticals[i] < 0){
          name <- paste0("today", verticals[i], ": ", format(today-verticals[i], "%d %b %y"))
        }
    
    p <- p %>%
      add_lines(x = as.numeric(today+verticals[i]), y = c(miny,maxy), text = NULL,
                name = name,
                color = I("black"), opacity = .5, fill = I("black"))
  }
  
  p
  })
}

# Run the application 
shinyApp(ui = ui, server = server, options = list(launch.browser = launch.browser))
}