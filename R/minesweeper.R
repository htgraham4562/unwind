#' Unwind with Minesweeper
#'
#' @export minesweeper
#' @param gamelog An optional specification of existing Minesweeper game log.
#' @returns Launches a shiny Gadget to play Minesweeper under user specifications and returns a game log upon closing the shiny Gadget.
#' @examples
#' minesweeper()
#'
#' gamelog_examp <- data.frame(StartTime = Sys.time(),
#'                             DiffLev = 'Unknown',
#'                             NumRows = 7,
#'                             NumCols = 14,
#'                             NumMines = 10,
#'                             RandSeed = 123456,
#'                             GameResult = 'Loss',
#'                             GameTime = round(difftime(Sys.time()+10, Sys.time(), units = 'secs'), 0))
#' minesweeper(gamelog_examp)

# Function for Shiny Gadget
minesweeper <- function(gamelog = NULL) {

  ui <- miniPage(
    miniContentPanel(
      plotOutput("plot", click="flag", dblclick = "uncover"),
      textOutput('timer_val'),
      textOutput('seed_val')
    ),
    # user inputs
    tags$head(tags$style(type = 'text/css',".shiny-input-panel{padding: 0px 0px !important;}")),
    inputPanel(
      radioButtons("difflev", label = "",
                   choices = c("Beginner", 'Intermediate', 'Expert', 'Custom')),
      conditionalPanel(
        condition = "input.difflev=='Custom'",
        tags$style(type = 'text/css', 'label{ display: table-cell; text-align: left;vertical-align: top; } .form-group { display: table-row;}'),
        numericInput('in_numrow', label = "Rows:", value = 1, min = 1, max = 30),
        numericInput('in_numcol', label = "Columns:", value = 1, min = 1, max = 50),
        numericInput('in_nummine', label = "Mines:", value = 1, min = 1, max = 2401),
      ),
      panel(
        textInput('randseed', label = "Seed:", width = 100),
        actionButton("newgame", label = "START GAME", style = "background-color: #b1a29b"),
        style = "background-color: #a9b6bc; color: #63666A"
      ),
      style = "background-color: #a9b6bc; color: #63666A"),
    setBackgroundColor('#a9b6bc')
  )


  server <- function(input, output, session) {

    # turn on thematic
    thematic::thematic_on()

    # create win and loss messages to be displayed when needed
    win_mess <- modalDialog("Congratulations! You've won the game!",
                            easyClose = T, size = 'l')
    loss_mess <- modalDialog("Oh No! You've uncovered a mine!",
                             easyClose = T, size = 'l')

    # initialize game log
    if (is.null(gamelog)){
      game_log_out <- reactiveVal(data.frame(StartTime = NA,
                                             DiffLev = NA,
                                             NumRows = NA,
                                             NumCols = NA,
                                             NumMines = NA,
                                             RandSeed = NA,
                                             GameResult = NA,
                                             GameTime = NA)%>%
                                    filter(!is.na(StartTime)))
    } else {
      # check if supplied log is in the right format (warning message if not)
      if(sum(c('StartTime', 'DiffLev', 'NumRows',
               'NumCols', 'NumMines', 'RandSeed',
               'GameResult', 'GameTime') %in% colnames(gamelog)) != 8){
        warning("Warning: User-supplied game log does not contain all necessary columns.
                User-supplied game log will be ignored and a new game log will be started.")
        game_log_out <- reactiveVal(data.frame(StartTime = NA,
                                               DiffLev = NA,
                                               NumRows = NA,
                                               NumCols = NA,
                                               NumMines = NA,
                                               RandSeed = NA,
                                               GameResult = NA,
                                               GameTime = NA) %>%
                                      filter(!is.na(StartTime)))
      } else if (ncol(gamelog) > 8){
        warning("Warning: User-supplied game log contains more columns than expected.
                Additional columns will be removed from game log.")
        game_log_out <- reactiveVal(gamelog[c('StartTime', 'DiffLev', 'NumRows',
                                              'NumCols', 'NumMines', 'RandSeed',
                                              'GameResult', 'GameTime')])
      } else {
        game_log_out <- reactiveVal(gamelog)
      }
    }


    # initialize minesweeper grid based on user inputs
    mode_game <- reactiveVal()
    grid_r <- reactiveVal()
    grid_c <- reactiveVal()
    seed_val_use <- reactiveVal()
    mine_new <- reactiveVal()
    timer <- reactiveVal()
    time_init <- reactiveVal()
    n_mines_init <- reactiveVal()
    n_mines_left <- reactiveVal()
    game_comp <- reactiveVal()

    observeEvent(input$newgame, {
      game_comp(FALSE)

      # determine which seed to use (user specified or random)
      if(isolate(input$randseed) != ""){
        seed_val_use(as.numeric(isolate(input$randseed)))} else {
          seed_val_use(round(as.numeric(Sys.time()), 0))
        }

      mode_game(isolate(input$difflev))

      if (mode_game() == "Beginner"){
        n_mines_init(10)
        n_mines_left(10)
        grid_r(8)
        grid_c(8)
      } else if (mode_game() == "Intermediate"){
        n_mines_init(40)
        n_mines_left(40)
        grid_r(16)
        grid_c(16)
      } else if (mode_game() == "Expert"){
        n_mines_init(99)
        n_mines_left(99)
        grid_r(16)
        grid_c(30)
      } else {
        n_mines_init(as.numeric(isolate(input$in_nummine)))
        n_mines_left(as.numeric(isolate(input$in_nummine)))
        grid_r(as.numeric(isolate(input$in_numrow)))
        grid_c(as.numeric(isolate(input$in_numcol)))
      }

      # get new grid
      mine_new(create_new_game_mine(grid_r(), grid_c(), n_mines_init(), seed_val_use()))

      # start timer
      timer(0)
      time_init(Sys.time())
      # update timer every second
      observe({
        invalidateLater(1000, session)
        isolate({timer(round(difftime(Sys.time(), time_init(), units = 'secs'), 0))})
      })

    }) # when start game button is pushed, get a new board

    # update grid based on user input (flag/uncover)
    observeEvent(input$uncover, {
      check0(mine_new(), ceiling(as.numeric(isolate(input$uncover$y))),
             ceiling(as.numeric(isolate(input$uncover$x)))) %>%
        mine_new()
      # check none of the uncovered cells were mines
      if(nrow(filter(mine_new(), uncovered == T, mine == T)) > 0){
        # display loss message
        showModal(loss_mess)
        # output record to game log
        game_log_out(rbind(isolate(game_log_out()),
                           data.frame(StartTime = isolate(time_init()),
                                      DiffLev = isolate(mode_game()),
                                      NumRows = isolate(grid_r()),
                                      NumCols = isolate(grid_c()),
                                      NumMines = isolate(n_mines_init()),
                                      RandSeed = isolate(seed_val_use()),
                                      GameResult = "Loss",
                                      GameTime = isolate(timer()))))
        assign('gamelog_mine', game_log_out(), envir=.GlobalEnv)
        time_init(NA)
        game_comp(TRUE)
      }
      # check that all non-mine cells are uncovered
      if(nrow(filter(mine_new(), mine == F, uncovered == F)) == 0){
        # display win message
        showModal(win_mess)
        # output record to game log
        game_log_out(rbind(isolate(game_log_out()),
                           data.frame(StartTime = isolate(time_init()),
                                      DiffLev = isolate(mode_game()),
                                      NumRows = isolate(grid_r()),
                                      NumCols = isolate(grid_c()),
                                      NumMines = isolate(n_mines_init()),
                                      RandSeed = isolate(seed_val_use()),
                                      GameResult = "Win",
                                      GameTime = isolate(timer()))))
        assign('gamelog_mine', game_log_out(), envir=.GlobalEnv)
        time_init(NA)
        game_comp(TRUE)
      }
    })
    observeEvent(input$flag, {
      mine_new() %>%
        mutate(flagged = ifelse(rowy == ceiling(as.numeric(isolate(input$flag$y))) &
                                  colx == ceiling(as.numeric(isolate(input$flag$x))) &
                                  uncovered == FALSE, # can't flag already uncovered cell
                                as.logical(abs(flagged - 1)), flagged)) %>% # flag or unflag based on current cell status
        # update data value
        mine_new()
      # reduce number of mines left to flag by 1
      n_mines_left(n_mines_init() - nrow(filter(mine_new(), flagged == T)))
    })

    # update game board
    output$plot <- renderPlot({
      plt_mine(mine_new(), mode_game(), n_mines_left(), grid_r(), grid_c(), game_comp())
    })

    # display timer
    output$timer_val <- renderText({
      paste0('Time: ', seconds_to_period(timer()))
    })
    # display seed
    output$seed_val <- renderText({
      paste0('Seed: ', seed_val_use())
    })
    # Let user know that game log is saved in global environment
    onStop(function() {
      cat("Your game log is saved as a dataframe in the global environment named 'gamelog_mine'.\nTo save your next game(s) to this log, supply the log object as the argument to the minesweeper function on startup.\nCome back to unwind soon!")
      })

  }
  suppressWarnings(runGadget(ui, server, viewer = dialogViewer('Unwind with Minesweeper', width = 700, height = 630)))
}
