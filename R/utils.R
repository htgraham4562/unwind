# function to safely sample one integer
resample <- function(x, ...) x[sample.int(length(x), ...)]

# function for plotting tic-tac-toe grid
plt_ttt <- function(dat_use, mode_game, player_use){
  ggplot(data = dat_use, aes(x=colx, y=rowy)) +
    geom_point(size = 0) +
    geom_text(label = dat_use$vals, nudge_x = -0.5, nudge_y = -0.5,
              color = 'black', size = 30,
              alpha = ifelse(dat_use$vals %in% c('X', 'O'), 1, 0.06)) +
    geom_hline(yintercept = c(1,2), size = 4, color = '#63666A') +
    geom_vline(xintercept = c(1,2), size = 4, color = '#63666A') +
    scale_x_continuous(limits = c(0,3)) +
    scale_y_continuous(limits = c(0,3)) +
    coord_fixed(expand = F) +
    ggtitle(ifelse(grepl('One', mode_game),
                   paste0("TIC-TAC-TOE\n", mode_game),
                   paste0("TIC-TAC-TOE\n", mode_game, "\nPlayer ", player_use, "'s Turn"))) +
    theme(axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 30))
}

# function for updating ttt grid dataset
ttt_grid_update <- function(datuse, grid_r, grid_c, playturn){
  tmp <- mutate(datuse,
                vals = ifelse(rowy == grid_r & colx == grid_c,
                              ifelse(playturn == 0, 'X', 'O'),
                              vals))
  return(tmp)
}

# function for updating win possibilities dataset
ttt_win_update <- function(datuse, grid_r, grid_c, playturn){
  val_play <- ifelse(grid_r == 1 & grid_c == 1, 7,
                     ifelse(grid_r == 1 & grid_c == 2, 8,
                            ifelse(grid_r == 1 & grid_c == 3, 9,
                                   ifelse(grid_r == 2 & grid_c == 1, 4,
                                          ifelse(grid_r == 2 & grid_c == 2, 5,
                                                 ifelse(grid_r == 2 & grid_c == 3, 6,
                                                        ifelse(grid_r == 3 & grid_c == 1, 1,
                                                               ifelse(grid_r == 3 & grid_c == 2, 2, 3))))))))

  tmp <- mutate(datuse,
                seqval = ifelse(cell1 == val_play, paste0(ifelse(playturn == 0, 'X', 'O'), substr(seqval, 2,3)),
                                ifelse(cell2 == val_play, paste0(substr(seqval, 1,1), ifelse(playturn == 0, 'X', 'O'), substr(seqval, 3,3)),
                                       ifelse(cell3 == val_play, paste0(substr(seqval, 1,2), ifelse(playturn == 0, 'X', 'O')), seqval))),
                win_0 = str_count(seqval, 'X'),
                win_1 = str_count(seqval, 'O')) #%>%
  #filter(!(win_0 > 0 & win_1 > 0))

  return(tmp)
}

# function to check for win / draw
ttt_check <- function(datuse){
  retval <- ifelse(nrow(filter(datuse, win_0 > 0, win_1 > 0)) == 8, 'draw',
                   ifelse((nrow(filter(datuse, win_0 == 3 | win_1 == 3)) > 0),
                          'win', 'ongoing'))
  return(retval)
}

# function to find next computer move
ttt_comp_move <- function(dat_grid, dat_wins, mode, comp_val){
  if (comp_val) {
    col_comp <- 'win_0'
    col_play <- 'win_1'
  } else {
    col_comp <- 'win_1'
    col_play <- 'win_0'
  }

  if(str_count(mode, 'Easy') > 0) {

    tmp <- dat_wins[which(dat_wins[[col_comp]] == min(dat_wins[[col_comp]])),]
    tmp2 <- tmp[which(tmp[[col_play]] == min(tmp[[col_play]])),]

    tmp3 <- table(c(tmp2$cell1[which(substr(tmp2$seqval, 1,1) == '-')],
                    tmp2$cell2[which(substr(tmp2$seqval, 2,2) == '-')],
                    tmp2$cell3[which(substr(tmp2$seqval, 3,3) == '-')]))
    samp_vec <- as.numeric(names(tmp3)[which(tmp3 == min(tmp3))])

    return(resample(samp_vec, 1))

  } else if(str_count(mode, 'Normal') > 0){

    return(resample(which(dat_grid$vals == ''), 1))

  } else {

    # win if available
    tmp1 <- dat_wins[which(dat_wins[[col_comp]] == 2 & dat_wins[[col_play]] == 0),]
    # block if available
    tmp2 <- dat_wins[which(dat_wins[[col_play]] == 2& dat_wins[[col_comp]] == 0),]
    # else pick a reasonable move
    tmp0 <- dat_wins[which(dat_wins$win_0 + dat_wins$win_1 != 3),]
    tmp <- tmp0[which(tmp0[[col_comp]] == max(tmp0[[col_comp]])),]
    tmp3 <- tmp[which(tmp[[col_play]] == max(tmp[[col_play]])),]

    if (nrow(tmp1) > 0){
      tmp4 <- table(c(tmp1$cell1[which(substr(tmp1$seqval, 1,1) == '-')],
                      tmp1$cell2[which(substr(tmp1$seqval, 2,2) == '-')],
                      tmp1$cell3[which(substr(tmp1$seqval, 3,3) == '-')]))
      samp_vec <- as.numeric(names(tmp4)[which(tmp4 == max(tmp4))])

      return(resample(samp_vec, 1))
    } else if (nrow(tmp2) > 0){
      tmp4 <- table(c(tmp2$cell1[which(substr(tmp2$seqval, 1,1) == '-')],
                      tmp2$cell2[which(substr(tmp2$seqval, 2,2) == '-')],
                      tmp2$cell3[which(substr(tmp2$seqval, 3,3) == '-')]))
      samp_vec <- as.numeric(names(tmp4)[which(tmp4 == max(tmp4))])

      return(resample(samp_vec, 1))
    } else {
      tmp4 <- table(c(tmp3$cell1[which(substr(tmp3$seqval, 1,1) == '-')],
                      tmp3$cell2[which(substr(tmp3$seqval, 2,2) == '-')],
                      tmp3$cell3[which(substr(tmp3$seqval, 3,3) == '-')]))
      samp_vec <- as.numeric(names(tmp4)[which(tmp4 == max(tmp4))])

      return(resample(samp_vec, 1))
    }



  }
}

# define function for checking surrounding cells and assigning value of the cell
count_mine <- function(datuse, rowid, colid){
  if(filter(datuse, colx == colid, rowy == rowid)$mine){ # if cell is mine return "*"
    return("*")
  } else {
    # create mini dataframe of surrounding cell
    mini_dat <- filter(datuse, abs(colx - colid) <= 1, abs(rowy - rowid) <= 1)
    return(sum(mini_dat$mine))
  }
}

# function for creating random grid
create_new_game_mine <- function(grid_r, grid_c, num_mines, rand_seed_use){

  set.seed(rand_seed_use)

  mine_rand <- sample.int(n=grid_r*grid_c, size = num_mines)
  mine_ind <- vector(length = grid_r*grid_c)
  mine_ind[mine_rand] <- TRUE

  # random grid dataset
  mine_init <- data.frame(colx = rep(c(1:grid_c), each = grid_r),
                          rowy = rep(c(1:grid_r), grid_c),
                          mine = mine_ind, cell_val = '',
                          uncovered = F, flagged = F)
  for (i in 1:nrow(mine_init)){
    mine_init$cell_val[i] <- count_mine(mine_init, mine_init$rowy[i], mine_init$colx[i])
  }
  # Note: slow - may be better to call the function each time a user uncovers a cell
  # rather than calculate all cell values up front?

  # Display "0" cells as missing
  mine_init <- mutate(mine_init, cell_val = ifelse(cell_val == "0", "", cell_val))

  return(mine_init)
}

# function for uncovering all cells around 0's and iterating if another 0
# updated to uncover all surrounding cells when all available mines have been flagged
check0 <- function(datuse, uncov_r, uncov_c){
  if (!(filter(datuse, rowy == uncov_r, colx == uncov_c)$flagged)){
    #if (filter(datuse, colx == uncov_c, rowy == uncov_r)$cell_val != ""){
    # check if number of surrounding mines is equal to number of surrounding flags
    checkdat <- filter(datuse, abs(rowy - uncov_r) <= 1, abs(colx - uncov_c) <= 1)
    if (nrow(filter(checkdat, mine == TRUE)) != nrow(filter(checkdat, flagged == TRUE))) {
      # if not all mines have been flagged
      datret <- mutate(datuse,
                       uncovered = ifelse(rowy == uncov_r & colx == uncov_c, TRUE, uncovered))
      return(datret)
    } else {
      # if all mines have been flagged
      tocheck_cells <- filter(checkdat, uncovered == FALSE) %>%#, mine == FALSE) %>%
        transmute(colx, rowy, cind = NA)
      checked <- data.frame(colx = NULL,
                            rowy = NULL,
                            cind = NULL)
      datret1 <- datuse
      while (nrow(tocheck_cells) > 0){
        tmpsubset <- filter(datret1, abs(rowy - tocheck_cells$rowy[1]) <= 1, abs(colx - tocheck_cells$colx[1]) <= 1)
        if(nrow(filter(tmpsubset, mine == TRUE)) != nrow(filter(tmpsubset, flagged == TRUE))){
          # all mines have been not been flagged - move cell to checked file to be uncovered
          checked <- rbind(checked, tocheck_cells[1,]) %>%
            mutate(cind = 'Y')
          tocheck_cells <- tocheck_cells[-1,]
        } else {
          # all mines have been flagged - move cell to checked file and add any more to be checked
          checked <- rbind(checked, tocheck_cells[1,]) %>%
            mutate(cind = 'Y')
          new_checks <- filter(tmpsubset, uncovered == FALSE) %>% #, mine == FALSE) %>%
            transmute(colx, rowy, cind = NA)
          tocheck_cells <- rbind(tocheck_cells[-1,], new_checks) %>%
            select(-cind) %>%
            left_join(checked, by = c('colx', 'rowy')) %>%
            filter(is.na(cind)) %>%
            group_by(colx, rowy) %>%
            unique() %>%
            ungroup()
        }
      }
      datret1 <- left_join(datret1, checked, by = c('colx', 'rowy')) %>%
        mutate(uncovered = ifelse(!is.na(cind) & !(flagged), TRUE, uncovered)) %>%
        select(-cind)
      return(datret1)
    }
  } else {
    return(datuse)
  }
}

# function for plotting minesweeper grid
plt_mine <- function(dat_use, mode_game, n_mines_left, grid_r, grid_c, blank){
  if (!blank){
    ggplot(data = dat_use, aes(x=colx, y=rowy)) +
      geom_point(size = 0) +
      geom_text(label = dat_use$cell_val, nudge_x = -0.5, nudge_y = -0.5,
                color = '#63666A', size = 6,
                alpha = ifelse(dat_use$uncovered, 1, 0)) +
      annotate('rect', xmin = dat_use$colx[!dat_use$uncovered] -1,
               xmax = dat_use$colx[!dat_use$uncovered],
               ymin = dat_use$rowy[!dat_use$uncovered] -1,
               ymax = dat_use$rowy[!dat_use$uncovered],
               fill = 'lightgrey') +
      geom_text(label = ifelse(dat_use$flagged == T, "*", ""),
                nudge_x = -0.5, nudge_y = -0.9,
                color = '#b1a29b', size = 15) +
      geom_hline(yintercept = c(-1:grid_r+1), size = 1, color = '#63666A') +
      geom_vline(xintercept = c(-1:grid_c+1), size = 1, color = '#63666A') +
      scale_x_continuous(limits = c(0, grid_c)) +
      scale_y_continuous(limits = c(0, grid_r)) +
      coord_fixed(expand = F) +
      ggtitle(paste0(mode_game, " Mode: ", n_mines_left, " Mines Left")) +
      theme(axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 20, color = '#63666A'))
  } else {
    ggplot() + theme_void()
  }
}
