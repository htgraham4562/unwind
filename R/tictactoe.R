#' Unwind with one-player or two-player Tic-Tac-Toe
#'
#' @export tictactoe
#' @param gamelog An optional specification of existing Tic-Tac-Toe game log.
#' @returns Launches a shiny Gadget to play Tic-Tac-Toe under user specifications and returns a game log upon closing the shiny Gadget.
#' @examples
#' tictactoe()
#'
#' gamelog_examp <- data.frame(StartTime = Sys.time(),
#'                             GameType = 'Unknown',
#'                             RandSeed = 123456,
#'                             GameResult = 'Loss',
#'                             GameTime = round(difftime(Sys.time()+10, Sys.time(), units = 'secs'), 0),
#'                             NumMoves_X = 0,
#'                             NumMoves_O = 0)
#' tictactoe(gamelog_examp)

# Function for Shiny Gadget
tictactoe <- function(gamelog = NULL) {

  ui <- miniPage(
    miniContentPanel(
      plotOutput("plot", click="playhere"),
      textOutput('timer_val'),
      textOutput('seed_val')
    ),
    # user inputs
    tags$head(tags$style(type = 'text/css',".shiny-input-panel{padding: 0px 0px !important;}")),
    inputPanel(
      switchInput('numplayer', value = TRUE, onLabel = 'One Player', offLabel = 'Two Players',
                  onStatus = '#63666A', offStatus = '#63666A', label = 'Game Type'),
      conditionalPanel(
        condition = "input.numplayer",
        tags$style(type = 'text/css', 'label{ display: table-cell; text-align: left;vertical-align: top; } .form-group { display: table-row;}'),
        switchInput('xos', value = TRUE, onLabel = "X (1st)", offLabel = "O (2nd)", label = 'Play Symbol',
                    onStatus = '#63666A', offStatus = '#63666A')
      ),
      conditionalPanel(
        condition = "input.numplayer",
        tags$style(type = 'text/css', 'label{ display: table-cell; text-align: left;vertical-align: top; } .form-group { display: table-row;}'),
        radioButtons("difflev", label = "",
                     choices = c("Easy", 'Normal', 'Hard'))
      ),
      conditionalPanel(
        condition = "input.numplayer",
        tags$style(type = 'text/css', 'label{ display: table-cell; text-align: left;vertical-align: top; } .form-group { display: table-row;}'),
        textInput('randseed', label = "Seed:", width = 100)
      ),
      panel(
        actionButton("newgame", label = "START GAME", style = "background-color: #b1a29b"),
        style = "background-color: #a9b6bc; color: #63666A"
      ),
      style = "background-color: #a9b6bc; color: #63666A"),
    setBackgroundColor('#a9b6bc')
  )


  server <- function(input, output, session) {

    # turn on thematic
    thematic_on()

    # initialize game log
    if (is.null(gamelog)){
      game_log_out <- reactiveVal(data.frame(StartTime = NA,
                                             GameType = NA,
                                             RandSeed = NA,
                                             GameResult = NA,
                                             GameTime = NA,
                                             NumMoves_X = NA,
                                             NumMoves_O = NA)%>%
                                    dplyr::filter(!is.na(StartTime)))
    } else {
      # check if supplied log is in the right format (warning message if not)
      if(sum(c('StartTime', 'GameType',
               'RandSeed', 'GameResult', 'GameTime',
               'NumMoves_X', 'NumMoves_O') %in% colnames(gamelog)) != 7){
        warning("Warning: User-supplied game log does not contain all necessary columns.
                User-supplied game log will be ignored and a new game log will be started.")
        game_log_out <- reactiveVal(data.frame(StartTime = NA,
                                               GameType = NA,
                                               RandSeed = NA,
                                               GameResult = NA,
                                               GameTime = NA,
                                               NumMoves_X = NA,
                                               NumMoves_O = NA) %>%
                                      dplyr::filter(!is.na(StartTime)))
      } else if (ncol(gamelog) > 7){
        warning("Warning: User-supplied game log contains more columns than expected.
                Additional columns will be removed from game log.")
        game_log_out <- reactiveVal(gamelog[c('StartTime', 'GameType',
                                              'RandSeed', 'GameResult', 'GameTime',
                                              'NumMoves_X', 'NumMoves_O')])
      } else {
        game_log_out <- reactiveVal(gamelog)
      }
    }

    # initialize empty game board
    ttt_init <- data.frame(colx = rep(c(1:3),3),
                           rowy = c(rep(3,3), rep(2,3), rep(1,3)),
                           vals = "")

    # initialize possible win combinations
    ttt_wins <- data.frame(cell1 = c(1,4,7,1,2,3,1,3),
                           cell2 = c(2,5,8,4,5,6,5,5),
                           cell3 = c(3,6,9,7,8,9,9,7),
                           seqval = "---",
                           win_0 = 0, win_1 = 0)

    # initialize
    mode_game <- reactiveVal()
    seed_val_use <- reactiveVal()
    timer <- reactiveVal()
    time_init <- reactiveVal()
    ttt_new <- reactiveVal()
    ttt_wins_new <- reactiveVal()
    player_turn <- reactiveVal()
    compval_init <- reactiveVal()
    compval_flg <- reactiveVal()

    observeEvent(input$newgame, {

      # determine if one or two player game for setup
      if(isolate(input$numplayer)){ # if one player game selected

        # determine which seed to use (user specified or random)
        if(isolate(input$randseed) != ""){
          seed_val_use(as.numeric(isolate(input$randseed)))} else {
            seed_val_use(round(as.numeric(Sys.time()), 0))
          }

        mode_game(paste0("One Player: ", isolate(input$difflev)))

        # set seed
        set.seed(seed_val_use())

        # get new grid
        if (isolate(input$xos)){ # if player wants to go first
          ttt_new(ttt_init)
          ttt_wins_new(ttt_wins)
          compval_flg(FALSE)
        } else { # if player wants computer to go first
          compval_init(ifelse(isolate(input$difflev) == 'Easy', sample(c(2,4,6,8), 1),
                              ifelse(isolate(input$difflev) == 'Normal', sample(c(1:9), 1),
                                     5)))
          ttt_new(mutate(ttt_init, vals = ifelse(row_number() == compval_init(), 'X', '')))
          # update wins grid
          compval_flg(TRUE)
          mutate(ttt_wins,
                 seqval = ifelse(cell1 == compval_init(), paste0(ifelse(compval_flg(), 'X', 'O'), substr(seqval, 2,3)),
                                 ifelse(cell2 == compval_init(), paste0(substr(seqval, 1,1), ifelse(compval_flg(), 'X', 'O'), substr(seqval, 3,3)),
                                        ifelse(cell3 == compval_init(), paste0(substr(seqval, 1,2), ifelse(compval_flg(), 'X', 'O')), seqval))),
                 win_0 = str_count(seqval, 'X'),
                 win_1 = str_count(seqval, 'O')) %>%
            #dplyr::filter(!(win_0 > 0 & win_1 > 0)) %>%
            ttt_wins_new()
        }

      } else { # if two player game selected
        seed_val_use("Not Applicable")
        mode_game(paste0("Two Player"))
        # get new grid
        ttt_new(ttt_init)
        ttt_wins_new(ttt_wins)
      }


      # start timer
      timer(0)
      time_init(Sys.time())
      # update timer every second
      observe({
        invalidateLater(1000, session)
        isolate({timer(round(difftime(Sys.time(), time_init(), units = 'secs'), 0))})
      })
      # X's go first
      player_turn(0)
    }) # when start game button is pushed, get a new board

   # update grid based on user input
    observeEvent(input$playhere, {

      if(grepl('One', mode_game())){ # if one player game, update the grid for both player and computer
        # change display grid
        if(nrow(dplyr::filter(ttt_new(), rowy == ceiling(as.numeric(isolate(input$playhere$y))) &
                       colx == ceiling(as.numeric(isolate(input$playhere$x))) & !(vals %in% c('X', 'O')))) > 0){

          # update grid
          ttt_grid_update(ttt_new(), ceiling(as.numeric(isolate(input$playhere$y))),
                          ceiling(as.numeric(isolate(input$playhere$x))), abs(compval_flg())) %>%
            ttt_new()

          # update win possibilities
          ttt_win_update(ttt_wins_new(), ceiling(as.numeric(isolate(input$playhere$y))),
                         ceiling(as.numeric(isolate(input$playhere$x))),
                         abs(compval_flg())) %>%
            ttt_wins_new()

           # check for win / draw / ongoing and also play computer's turn
           if(ttt_check(ttt_wins_new()) == 'draw'){
             # display message
             showModal(modalDialog("The game has completed without a winner.",
                                   easyClose = T, size = 'l'))
             # output log
             game_log_out(rbind(isolate(game_log_out()),
                                data.frame(StartTime = isolate(time_init()),
                                           GameType = isolate(mode_game()),
                                           RandSeed = isolate(seed_val_use()),
                                           GameResult = 'Draw',
                                           GameTime = isolate(timer()),
                                           NumMoves_X = nrow(dplyr::filter(isolate(ttt_new()), vals == 'X')),
                                           NumMoves_O = nrow(dplyr::filter(isolate(ttt_new()), vals == 'O')))))
             assign('gamelog_ttt', game_log_out(), envir=.GlobalEnv)
             time_init(NA)
             # wipe grid dataset
             ttt_new(NULL)
           } else if (ttt_check(ttt_wins_new()) == 'win') {

             # display message
             showModal(modalDialog(paste0("Tic-Tac-Toe, three-in-a-row! You've won the game!"),
                                   easyClose = T, size = 'l'))
             # output log
             game_log_out(rbind(isolate(game_log_out()),
                                data.frame(StartTime = isolate(time_init()),
                                           GameType = isolate(mode_game()),
                                           RandSeed = isolate(seed_val_use()),
                                           GameResult = "Win",
                                           GameTime = isolate(timer()),
                                           NumMoves_X = nrow(dplyr::filter(isolate(ttt_new()), vals == 'X')),
                                           NumMoves_O = nrow(dplyr::filter(isolate(ttt_new()), vals == 'O')))))
             assign('gamelog_ttt', game_log_out(), envir=.GlobalEnv)
             time_init(NA)
             # wipe grid dataset
             ttt_new(NULL)

           } else if(ttt_check(ttt_wins_new()) == 'ongoing'){

             # play for computer and re-check for a computer win
             valplay <- ttt_comp_move(ttt_new(), ttt_wins_new(), mode_game(), compval_flg())

             # update grid
             mutate(ttt_new(), vals = ifelse(row_number() == valplay,
                                             ifelse(compval_flg(), 'X', 'O'), vals)) %>%
              ttt_new()

            # update win possibilities
            mutate(ttt_wins_new(),
                   seqval = ifelse(cell1 == valplay, paste0(ifelse(compval_flg(), 'X', 'O'), substr(seqval, 2,3)),
                                   ifelse(cell2 == valplay, paste0(substr(seqval, 1,1), ifelse(compval_flg(), 'X', 'O'), substr(seqval, 3,3)),
                                          ifelse(cell3 == valplay, paste0(substr(seqval, 1,2), ifelse(compval_flg(), 'X', 'O')), seqval))),
                   win_0 = str_count(seqval, 'X'),
                   win_1 = str_count(seqval, 'O')) %>%
              #dplyr::filter(!(win_0 > 0 & win_1 > 0)) %>%
              ttt_wins_new()

            # check for computer win
            if(ttt_check(ttt_wins_new()) == 'draw'){
              # display message
              showModal(modalDialog("The game has completed without a winner.",
                                    easyClose = T, size = 'l'))
              # output log
              game_log_out(rbind(isolate(game_log_out()),
                                 data.frame(StartTime = isolate(time_init()),
                                            GameType = isolate(mode_game()),
                                            RandSeed = isolate(seed_val_use()),
                                            GameResult = 'Draw',
                                            GameTime = isolate(timer()),
                                            NumMoves_X = nrow(dplyr::filter(isolate(ttt_new()), vals == 'X')),
                                            NumMoves_O = nrow(dplyr::filter(isolate(ttt_new()), vals == 'O')))))
              assign('gamelog_ttt', game_log_out(), envir=.GlobalEnv)
              time_init(NA)
              # wipe grid dataset
              ttt_new(NULL)
            } else if (ttt_check(ttt_wins_new()) == 'win') {
              # display message
              showModal(modalDialog(paste0("Oh No! You've lost the game!"),
                                    easyClose = T, size = 'l'))
              # output log
              game_log_out(rbind(isolate(game_log_out()),
                                 data.frame(StartTime = isolate(time_init()),
                                            GameType = isolate(mode_game()),
                                            RandSeed = isolate(seed_val_use()),
                                            GameResult = "Loss",
                                            GameTime = isolate(timer()),
                                            NumMoves_X = nrow(dplyr::filter(isolate(ttt_new()), vals == 'X')),
                                            NumMoves_O = nrow(dplyr::filter(isolate(ttt_new()), vals == 'O')))))
              assign('gamelog_ttt', game_log_out(), envir=.GlobalEnv)
              time_init(NA)
              # wipe grid dataset
              ttt_new(NULL)
            }

          }
        }
      } else { # if two player, just make move where clicked
        # change display grid
        if(nrow(dplyr::filter(ttt_new(), rowy == ceiling(as.numeric(isolate(input$playhere$y))) &
                  colx == ceiling(as.numeric(isolate(input$playhere$x))) & !(vals %in% c('X', 'O')))) > 0){
        # update grid
        ttt_grid_update(ttt_new(), ceiling(as.numeric(isolate(input$playhere$y))),
                        ceiling(as.numeric(isolate(input$playhere$x))), player_turn()) %>%
          ttt_new()

        # update win possiblities
        ttt_win_update(ttt_wins_new(), ceiling(as.numeric(isolate(input$playhere$y))),
                       ceiling(as.numeric(isolate(input$playhere$x))),
                       player_turn()) %>%
        ttt_wins_new()

        # check for a win / draw / ongoing
        if(ttt_check(ttt_wins_new()) == 'draw'){
          # display message
          showModal(modalDialog("The game has completed without a winner.",
                                            easyClose = T, size = 'l'))
          # output log
          game_log_out(rbind(isolate(game_log_out()),
                             data.frame(StartTime = isolate(time_init()),
                                        GameType = 'Two Player',
                                        RandSeed = 'Not Applicable',
                                        GameResult = 'Draw',
                                        GameTime = isolate(timer()),
                                        NumMoves_X = nrow(dplyr::filter(isolate(ttt_new()), vals == 'X')),
                                        NumMoves_O = nrow(dplyr::filter(isolate(ttt_new()), vals == 'O')))))
          assign('gamelog_ttt', game_log_out(), envir=.GlobalEnv)
          time_init(NA)
          # wipe grid dataset
          ttt_new(NULL)
        } else if(ttt_check(ttt_wins_new()) == 'win'){
          # display message
          showModal(modalDialog(paste0("Tic-Tac-Toe, three-in-a-row! Player ", player_turn() +1, ' has won the game!'),
                                easyClose = T, size = 'l'))
          # output log
          game_log_out(rbind(isolate(game_log_out()),
                             data.frame(StartTime = isolate(time_init()),
                                        GameType = 'Two Player',
                                        RandSeed = 'Not Applicable',
                                        GameResult = paste0('Player ', isolate(player_turn()) +1, ' Won'),
                                        GameTime = isolate(timer()),
                                        NumMoves_X = nrow(dplyr::filter(isolate(ttt_new()), vals == 'X')),
                                        NumMoves_O = nrow(dplyr::filter(isolate(ttt_new()), vals == 'O')))))
          assign('gamelog_ttt', game_log_out(), envir=.GlobalEnv)
          time_init(NA)
          # wipe grid dataset
          ttt_new(NULL)
        } else {
          # switch player turn
          player_turn(abs(player_turn() -1))
        }
        }

      }
    })

    # update game board
    output$plot <- renderPlot({
      plt_ttt(ttt_new(), mode_game(), player_turn()+1)
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
      cat("Your game log is saved as a dataframe in the global environment named 'gamelog_ttt'.\nTo save your next game(s) to this log, supply the log object as the argument to the tictactoe function on startup.\nCome back to unwind soon!")
    })

  }
  suppressWarnings(runGadget(ui, server, viewer = dialogViewer('Unwind with Tic-Tac-Toe', width = 900, height = 650)))
}
