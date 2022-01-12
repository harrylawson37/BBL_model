### Harry Lawson ###

source('bbl_decimal_odds.R')

create_excel_file <- function(team_one_name, team_two_name, team_one_b1, team_two_b2, team_one_header_style, 
                              team_one_cell_style, team_two_header_style, team_two_cell_style){
  
  # This function creates a formatted excel file.
  
  # return: An excel workbook
  # parameter team_one_b1: A list of data frames created by calling get_decimal_odds
  # parameter team_two_b2: A list of data frames created by calling get_decimal_odds
  # parameter team_one_header_style: A Createstyle for excel 
  # parameter team_two_header_style: A Createstyle for excel 
  # parameter team_one_cell_style: A Createstyle for excel
  # parameter team_two_cell_style: A Createstyle for excel
  # complexity: O(1)
  
  grey_header_style <- createStyle(fgFill = "#d8dcd6",  borderStyle = "none", textDecoration = "bold")
  
  wb <- createWorkbook()
  
  addWorksheet(wb, paste(team_one_name, "Batting First", sep=" ") , gridLines = FALSE)
  
  writeData(wb, paste(team_one_name, "Batting First"), team_one_b1$odds_df, startCol = 2, startRow = 2)
  
  writeData(wb, paste(team_one_name, "Batting First"), team_one_b1$t1_batting_plus, startCol = 2, startRow = 6)
  
  writeData(wb, paste(team_one_name, "Batting First"), team_one_b1$t2_batting_plus, startCol = 2, startRow = 19)
  
  writeData(wb, paste(team_one_name, "Batting First"), team_one_b1$t1_batting_boundaries, startCol = 12, startRow = 6)
  
  writeData(wb, paste(team_one_name, "Batting First"), team_one_b1$t2_batting_boundaries, startCol = 12, startRow = 19)
  
  writeData(wb, paste(team_one_name, "Batting First"), team_one_b1$t1_batting_runs_line_over, startCol = 2, startRow = 32)
  
  writeData(wb, paste(team_one_name, "Batting First"), team_one_b1$t1_batting_runs_line_under, startCol = 2, startRow = 45)
  
  writeData(wb, paste(team_one_name, "Batting First"), team_one_b1$t2_batting_runs_line_over, startCol = 2, startRow = 58)
  
  writeData(wb, paste(team_one_name, "Batting First"), team_one_b1$t2_batting_runs_line_under, startCol = 2, startRow = 71)
  
  writeData(wb, paste(team_one_name, "Batting First"), team_one_b1$bowling_team_one, startCol = 2, startRow = 84)
  
  writeData(wb, paste(team_one_name, "Batting First"), team_one_b1$bowling_team_two, startCol = 8, startRow = 84)
  
  addStyle(wb, sheet=paste(team_one_name, "Batting First"), style=grey_header_style, rows=2, cols=2:3)
  addStyle(wb, sheet=paste(team_one_name, "Batting First"), style=team_one_header_style, rows=3, cols=2:3)
  addStyle(wb, sheet=paste(team_one_name, "Batting First"), style=team_two_header_style, rows=4, cols=2:3)
  
  addStyle(wb, sheet=paste(team_one_name, "Batting First"), style=team_one_header_style, rows=6, cols=2:10)
  addStyle(wb, sheet=paste(team_one_name, "Batting First"), style=team_one_cell_style, rows=7:17, cols=2:10, gridExpand = T)
  
  addStyle(wb, sheet=paste(team_one_name, "Batting First"), style=team_one_header_style, rows=6, cols=12:18)
  addStyle(wb, sheet=paste(team_one_name, "Batting First"), style=team_one_cell_style, rows=7:17, cols=12:18, gridExpand = T)
  
  addStyle(wb, sheet=paste(team_one_name, "Batting First"), style=team_two_header_style, rows=19, cols=2:10)
  addStyle(wb, sheet=paste(team_one_name, "Batting First"), style=team_two_cell_style, rows=20:30, cols=2:10, gridExpand = T)
  
  addStyle(wb, sheet=paste(team_one_name, "Batting First"), style=team_two_header_style, rows=19, cols=12:18)
  addStyle(wb, sheet=paste(team_one_name, "Batting First"), style=team_two_cell_style, rows=20:30, cols=12:18, gridExpand = T)
  
  addStyle(wb, sheet=paste(team_one_name, "Batting First"), style=team_one_header_style, rows=32, cols=2:26)
  addStyle(wb, sheet=paste(team_one_name, "Batting First"), style=team_one_cell_style, rows=33:43, cols=2:26, gridExpand = T)
  
  addStyle(wb, sheet=paste(team_one_name, "Batting First"), style=team_one_header_style, rows=45, cols=2:26)
  addStyle(wb, sheet=paste(team_one_name, "Batting First"), style=team_one_cell_style, rows=46:56, cols=2:26, gridExpand = T)
  
  addStyle(wb, sheet=paste(team_one_name, "Batting First"), style=team_two_header_style, rows=58, cols=2:26)
  addStyle(wb, sheet=paste(team_one_name, "Batting First"), style=team_two_cell_style, rows=59:69, cols=2:26, gridExpand = T)
  
  addStyle(wb, sheet=paste(team_one_name, "Batting First"), style=team_two_header_style, rows=71, cols=2:26)
  addStyle(wb, sheet=paste(team_one_name, "Batting First"), style=team_two_cell_style, rows=72:82, cols=2:26, gridExpand = T)
  
  addStyle(wb, sheet=paste(team_one_name, "Batting First"), style=team_one_header_style, rows=84, cols=2:6)
  addStyle(wb, sheet=paste(team_one_name, "Batting First"), style=team_one_cell_style, rows=85:95, cols=2:6, gridExpand = T)
  
  addStyle(wb, sheet=paste(team_one_name, "Batting First"), style=team_two_header_style, rows=84, cols=8:12)
  addStyle(wb, sheet=paste(team_one_name, "Batting First"), style=team_two_cell_style, rows=85:95, cols=8:12, gridExpand = T)
  
  
  addWorksheet(wb, paste(team_two_name, "Batting First", sep=" ") , gridLines = FALSE)
  
  writeData(wb, paste(team_two_name, "Batting First"), team_two_b2$odds_df, startCol = 2, startRow = 2)
  
  writeData(wb, paste(team_two_name, "Batting First"), team_two_b2$t1_batting_plus, startCol = 2, startRow = 6)
  
  writeData(wb, paste(team_two_name, "Batting First"), team_two_b2$t2_batting_plus, startCol = 2, startRow = 19)
  
  writeData(wb, paste(team_two_name, "Batting First"), team_two_b2$t1_batting_boundaries, startCol = 12, startRow = 6)
  
  writeData(wb, paste(team_two_name, "Batting First"), team_two_b2$t2_batting_boundaries, startCol = 12, startRow = 19)
  
  writeData(wb, paste(team_two_name, "Batting First"), team_two_b2$t1_batting_runs_line_over, startCol = 2, startRow = 32)
  
  writeData(wb, paste(team_two_name, "Batting First"), team_two_b2$t1_batting_runs_line_under, startCol = 2, startRow = 45)
  
  writeData(wb, paste(team_two_name, "Batting First"), team_two_b2$t2_batting_runs_line_over, startCol = 2, startRow = 58)
  
  writeData(wb, paste(team_two_name, "Batting First"), team_two_b2$t2_batting_runs_line_under, startCol = 2, startRow = 71)
  
  writeData(wb, paste(team_two_name, "Batting First"), team_two_b2$bowling_team_one, startCol = 2, startRow = 84)
  
  writeData(wb, paste(team_two_name, "Batting First"), team_two_b2$bowling_team_two, startCol = 8, startRow = 84)
  
  addStyle(wb, sheet=paste(team_two_name, "Batting First"), style=grey_header_style, rows=2, cols=2:3)
  addStyle(wb, sheet=paste(team_two_name, "Batting First"), style=team_two_header_style, rows=3, cols=2:3)
  addStyle(wb, sheet=paste(team_two_name, "Batting First"), style=team_one_header_style, rows=4, cols=2:3)
  
  addStyle(wb, sheet=paste(team_two_name, "Batting First"), style=team_two_header_style, rows=6, cols=2:10)
  addStyle(wb, sheet=paste(team_two_name, "Batting First"), style=team_two_cell_style, rows=7:17, cols=2:10, gridExpand = T)
  
  addStyle(wb, sheet=paste(team_two_name, "Batting First"), style=team_two_header_style, rows=6, cols=12:18)
  addStyle(wb, sheet=paste(team_two_name, "Batting First"), style=team_two_cell_style, rows=7:17, cols=12:18, gridExpand = T)
  
  addStyle(wb, sheet=paste(team_two_name, "Batting First"), style=team_one_header_style, rows=19, cols=2:10)
  addStyle(wb, sheet=paste(team_two_name, "Batting First"), style=team_one_cell_style, rows=20:30, cols=2:10, gridExpand = T)
  
  addStyle(wb, sheet=paste(team_two_name, "Batting First"), style=team_one_header_style, rows=19, cols=12:18)
  addStyle(wb, sheet=paste(team_two_name, "Batting First"), style=team_one_cell_style, rows=20:30, cols=12:18, gridExpand = T)
  
  addStyle(wb, sheet=paste(team_two_name, "Batting First"), style=team_two_header_style, rows=32, cols=2:26)
  addStyle(wb, sheet=paste(team_two_name, "Batting First"), style=team_two_cell_style, rows=33:43, cols=2:26, gridExpand = T)
  
  addStyle(wb, sheet=paste(team_two_name, "Batting First"), style=team_two_header_style, rows=45, cols=2:26)
  addStyle(wb, sheet=paste(team_two_name, "Batting First"), style=team_two_cell_style, rows=46:56, cols=2:26, gridExpand = T)
  
  addStyle(wb, sheet=paste(team_two_name, "Batting First"), style=team_one_header_style, rows=58, cols=2:26)
  addStyle(wb, sheet=paste(team_two_name, "Batting First"), style=team_one_cell_style, rows=59:69, cols=2:26, gridExpand = T)
  
  addStyle(wb, sheet=paste(team_two_name, "Batting First"), style=team_one_header_style, rows=71, cols=2:26)
  addStyle(wb, sheet=paste(team_two_name, "Batting First"), style=team_one_cell_style, rows=72:82, cols=2:26, gridExpand = T)
  
  addStyle(wb, sheet=paste(team_two_name, "Batting First"), style=team_two_header_style, rows=84, cols=2:6)
  addStyle(wb, sheet=paste(team_two_name, "Batting First"), style=team_two_cell_style, rows=85:95, cols=2:6, gridExpand = T)
  
  addStyle(wb, sheet=paste(team_two_name, "Batting First"), style=team_one_header_style, rows=84, cols=8:12)
  addStyle(wb, sheet=paste(team_two_name, "Batting First"), style=team_one_cell_style, rows=85:95, cols=8:12, gridExpand = T)
  
  return(wb)
  
}
