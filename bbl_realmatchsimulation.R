### Harry Lawson ###

library(openxlsx)
source('bbl_create_excel.R')

bbl_bbb = read.csv('bbl_bbb_final.csv')

bbl_match_odds <- function(team_one, team_two, batting_order_t1, batting_order_t2, bowling_order_t1, bowling_order_t2, simulations, stadium = FALSE, batting_first = FALSE){
  
  # This function is used to call a match easily. The teams have to be altered depending
  # on the game. 
  
  # precondition: The batting and bowling orders are correct
  
  if (team_one == "Scorchers"){
    t1_header_style = createStyle(fgFill = "#f0833a", textDecoration = "bold",  borderStyle = "thin")
    t1_cell_style = createStyle(fgFill = "#ffd8b1", borderStyle = "none")
    p_scorchers <- c("NR Hobson", "KR Patterson", 
                     "LJ Evans", "AJ Turner", "AM Hardie", "CJM Sabburg", "AC Agar", "AJ Tye",
                     "M Kelly", "JP Behrendorff", "P Hatzoglou")
    team_one_batting_lineup <- character(11)
    for (i in 1:length(batting_order_t1)){
      team_one_batting_lineup[i] = p_scorchers[batting_order_t1[i]]
    }
  } else if (team_one == "Strikers"){
    t1_header_style = createStyle(fgFill = "#0d75f8", textDecoration = "bold",  borderStyle = "thin", fontColour = "#ffffff")
    t1_cell_style = createStyle(fgFill = "#a2cffe", borderStyle = "none")
    a_strikers <- c("HJ Hunt", "MW Short", "J Weatherald", "JW Wells", 
                     "T Kelly", "RJ Gibson", "HJ Nielsen", "Rashid Khan", 
                    "HNA Conway", "PM Siddle", "Fawad Ahmed")
    a_strikers <- c("HJ Hunt", "MW Short", "J Weatherald", "JW Wells", 
                    "T Kelly", "L Scott", "Rashid Khan",  "HJ Nielsen", "HTRY Thornton",
                    "HNA Conway", "PM Siddle")
    team_one_batting_lineup <- character(11)
    for (i in 1:length(batting_order_t1)){
      team_one_batting_lineup[i] = a_strikers[batting_order_t1[i]]
    }
  } else if (team_one == "Thunder"){
    t1_header_style = createStyle(fgFill = "#aaff32", textDecoration = "bold",  borderStyle = "thin")
    t1_cell_style = createStyle(fgFill = "#d1ffbd", borderStyle = "none")
    s_thunder <- c("M Gilkes", "AD Hales", "JJS Sangha", "DR Sams",  
                   "O Davies", "BCJ Cutting", "BJ Holt", 
                   "GS Sandhu", "N McAndrew", "T Sangha", "Mohammad Hasnain")
    team_one_batting_lineup <- character(11)
    for (i in 1:length(batting_order_t1)){
      team_one_batting_lineup[i] = s_thunder[batting_order_t1[i]]
    }
  } else if (team_one == "Sixers"){
    t1_header_style = createStyle(fgFill = "#f504c9", textDecoration = "bold",  borderStyle = "thin", fontColour = "#ffffff")
    t1_cell_style = createStyle(fgFill = "#ffd1df",  borderStyle = "none")
    s_sixers <- c("JR Philippe", "J Avendano", "DP Hughes", "MC Henriques",
                  "JC Silk", "DT Christian", "SA Abbott", "H Kerr", "Shadab Khan",
                  "BJ Dwarshuis", "JM Bird")
    s_sixers <- c("JR Philippe", "J Edwards", "DP Hughes", "MC Henriques", 
                  "Shadab Khan", "DT Christian", "SA Abbott", "H Kerr", 
                  "BJ Dwarshuis", "JM Bird", "T Murphy")
    team_one_batting_lineup <- character(11)
    for (i in 1:length(batting_order_t1)){
      team_one_batting_lineup[i] = s_sixers[batting_order_t1[i]]
    }
  } else if (team_one == "Renegades"){
    t1_header_style = createStyle(fgFill = "#e50000", textDecoration = "bold",  borderStyle = "thin", fontColour = "#ffffff")
    t1_cell_style = createStyle(fgFill = "#f29e8e",  borderStyle = "none")
    m_renegades <- c("J Fraser-McGurk", "AJ Finch", "SE Marsh", "NJ Maddinson", 
                     "MW Harvey", "SB Harper", "W Sutherland", "CJ Boyce",
                     "KW Richardson", "JK Lalor", "Zahir Khan")
    team_one_batting_lineup <- character(11)
    for (i in 1:length(batting_order_t1)){
      team_one_batting_lineup[i] = m_renegades[batting_order_t1[i]]
    }
  } else if (team_one == "Stars"){
    t1_header_style = createStyle(fgFill = "#39ad48", textDecoration = "bold",  borderStyle = "thin", fontColour = "#ffffff")
    t1_cell_style = createStyle(fgFill = "#b2fba5",  borderStyle = "none")
    m_stars <- c("NCR Larkin", "JM Clarke", "GJ Maxwell", "JA Burns",  
                 "HWR Cartwright", "BJ Webster", "SL Rainbird", 
                 "Qais Ahmad", "A Zampa", "BL Couch", "Haris Rauf")
    team_one_batting_lineup <- character(11)
    for (i in 1:length(batting_order_t1)){
      team_one_batting_lineup[i] = m_stars[batting_order_t1[i]]
    }
  } else if (team_one == "Hurricanes"){
    t1_header_style = createStyle(fgFill = "#9900fa", textDecoration = "bold",  borderStyle = "thin", fontColour = "#ffffff")
    t1_cell_style = createStyle(fgFill = "#e4cbff",  borderStyle = "none")
    h_hurricanes <- c("CP Jewell",  "BR McDermott", "PSP Handscomb", "HC Brook", 
                      "DJM Short", "TH David", "JA Thompson", "MJ Owen", 
                      "TS Rogers", "RP Meredith", "S Lamichhane")
    team_one_batting_lineup <- character(11)
    for (i in 1:length(batting_order_t1)){
      team_one_batting_lineup[i] = h_hurricanes[batting_order_t1[i]]
    }
  } else if (team_one == "Heat"){
    t1_header_style = createStyle(fgFill = "#06c2ac", textDecoration = "bold",  borderStyle = "thin", fontColour = "#ffffff")
    t1_cell_style = createStyle(fgFill = "#b8ffeb",  borderStyle = "none")
    b_heat <- c("JS Lehmann", "J Clayton", "Fakhar Zaman", "SD Heazlett",  
                "TLW Cooper", "LD Pfeffer", "S McGiffin",
                "W Prestwidge", "RH McDonald", "DMK Grant", "Mujeeb Ur Rahman")
    b_heat <- c("CA Lynn", "NA McSweeney", "LD Pfeffer", "BM Duckett",
                "SD Heazlett", "JS Lehmann", "XC Bartlett", "W Prestwidge",
                "MP Kuhnemann", "L Guthrie", "Mujeeb Ur Rahman")
    team_one_batting_lineup <- character(11)
    for (i in 1:length(batting_order_t1)){
      team_one_batting_lineup[i] = b_heat[batting_order_t1[i]]
    }
  }
  
  if (team_two == "Scorchers"){
    t2_header_style = createStyle(fgFill = "#f0833a", textDecoration = "bold",  borderStyle = "thin")
    t2_cell_style = createStyle(fgFill = "#ffd8b1", borderStyle = "none")
    p_scorchers <- c("NR Hobson", "KR Patterson", 
                     "LJ Evans", "AJ Turner", "AM Hardie", "CJM Sabburg", "AC Agar", "AJ Tye",
                     "M Kelly", "JP Behrendorff", "P Hatzoglou")
    team_two_batting_lineup <- character(11)
    for (i in 1:length(batting_order_t2)){
      team_two_batting_lineup[i] = p_scorchers[batting_order_t2[i]]
    }
  } else if (team_two == "Strikers"){
    t2_header_style = createStyle(fgFill = "#0d75f8", textDecoration = "bold",  borderStyle = "thin", fontColour = "#ffffff")
    t2_cell_style = createStyle(fgFill = "#a2cffe", borderStyle = "none")
    a_strikers <- c("HJ Hunt", "MW Short", "J Weatherald", "JW Wells", 
                    "T Kelly", "RJ Gibson", "HJ Nielsen", "Rashid Khan", 
                    "HNA Conway", "PM Siddle", "Fawad Ahmed")
    a_strikers <- c("HJ Hunt", "MW Short", "J Weatherald", "JW Wells", 
                    "T Kelly", "L Scott", "Rashid Khan",  "HJ Nielsen", "HTRY Thornton",
                    "HNA Conway", "PM Siddle")
    team_two_batting_lineup <- character(11)
    for (i in 1:length(batting_order_t2)){
      team_two_batting_lineup[i] = a_strikers[batting_order_t2[i]]
    }
  } else if (team_two == "Thunder"){
    t2_header_style = createStyle(fgFill = "#aaff32", textDecoration = "bold",  borderStyle = "thin")
    t2_cell_style = createStyle(fgFill = "#d1ffbd", borderStyle = "none")
    s_thunder <- c("M Gilkes", "AD Hales", "JJS Sangha", "DR Sams",  
                    "O Davies", "BCJ Cutting", "BJ Holt", 
                   "GS Sandhu", "N McAndrew", "T Sangha", "Mohammad Hasnain")
    team_two_batting_lineup <- character(11)
    for (i in 1:length(batting_order_t2)){
      team_two_batting_lineup[i] = s_thunder[batting_order_t2[i]]
    }
  } else if (team_two == "Sixers"){
    t2_header_style = createStyle(fgFill = "#f504c9", textDecoration = "bold",  borderStyle = "thin", fontColour = "#ffffff")
    t2_cell_style = createStyle(fgFill = "#ffd1df",  borderStyle = "none")
    s_sixers <- c("JR Philippe", "J Avendano", "DP Hughes", "MC Henriques",
                  "JC Silk", "DT Christian", "H Kerr", "Shadab Khan",
                  "BJ Dwarshuis", "JM Bird")
    s_sixers <- c("JR Philippe", "J Edwards", "DP Hughes", "MC Henriques", 
                  "Shadab Khan", "DT Christian", "SA Abbott", "H Kerr", 
                  "BJ Dwarshuis", "JM Bird", "T Murphy")
    team_two_batting_lineup <- character(11)
    for (i in 1:length(batting_order_t2)){
      team_two_batting_lineup[i] = s_sixers[batting_order_t2[i]]
    }
  } else if (team_two == "Renegades"){
    t2_header_style = createStyle(fgFill = "#e50000", textDecoration = "bold",  borderStyle = "thin", fontColour = "#ffffff")
    t2_cell_style = createStyle(fgFill = "#f29e8e",  borderStyle = "none")
    m_renegades <- c("J Fraser-McGurk", "AJ Finch", "SE Marsh", "NJ Maddinson", 
                     "MW Harvey", "SB Harper", "W Sutherland", "CJ Boyce",
                     "KW Richardson", "JK Lalor", "Zahir Khan")
    team_two_batting_lineup <- character(11)
    for (i in 1:length(batting_order_t2)){
      team_two_batting_lineup[i] = m_renegades[batting_order_t2[i]]
    }
  } else if (team_two == "Stars"){
    t2_header_style = createStyle(fgFill = "#39ad48", textDecoration = "bold",  borderStyle = "thin", fontColour = "#ffffff")
    t2_cell_style = createStyle(fgFill = "#b2fba5",  borderStyle = "none")
    m_stars <- c("NCR Larkin", "JM Clarke", "GJ Maxwell", "JA Burns",  
                 "HWR Cartwright", "BJ Webster", "SL Rainbird", 
                 "Qais Ahmad", "A Zampa", "BL Couch", "Haris Rauf")
    team_two_batting_lineup <- character(11)
    for (i in 1:length(batting_order_t2)){
      team_two_batting_lineup[i] = m_stars[batting_order_t2[i]]
    }
  } else if (team_two == "Hurricanes"){
    t2_header_style = createStyle(fgFill = "#9900fa", textDecoration = "bold",  borderStyle = "thin", fontColour = "#ffffff")
    t2_cell_style = createStyle(fgFill = "#e4cbff",  borderStyle = "none")
    h_hurricanes <- c("CP Jewell",  "BR McDermott", "PSP Handscomb", "HC Brook", 
                      "DJM Short", "TH David", "JA Thompson", "MJ Owen", 
                      "TS Rogers", "RP Meredith", "S Lamichhane")
    team_two_batting_lineup <- character(11)
    for (i in 1:length(batting_order_t2)){
      team_two_batting_lineup[i] = h_hurricanes[batting_order_t2[i]]
    }
  } else if (team_two == "Heat"){
    t2_header_style = createStyle(fgFill = "#06c2ac", textDecoration = "bold",  borderStyle = "thin", fontColour = "#ffffff")
    t2_cell_style = createStyle(fgFill = "#b8ffeb",  borderStyle = "none")
    b_heat <- c("JS Lehmann", "J Clayton", "Fakhar Zaman", "SD Heazlett",  
                "TLW Cooper", "LD Pfeffer", "S McGiffin",
                "W Prestwidge", "RH McDonald", "DMK Grant", "Mujeeb Ur Rahman")
    b_heat <- c("CA Lynn", "NA McSweeney", "LD Pfeffer", "BM Duckett",
                "SD Heazlett", "JS Lehmann", "XC Bartlett", "W Prestwidge",
                "MP Kuhnemann", "L Guthrie", "Mujeeb Ur Rahman")
    team_two_batting_lineup <- character(11)
    for (i in 1:length(batting_order_t2)){
      team_two_batting_lineup[i] = b_heat[batting_order_t2[i]]
    }
  }
  
  if (batting_first == 'T1'){
    team_one_bat_first = get_decimal_odds(bbl_bbb, team_one, team_two, team_one_batting_lineup, team_two_batting_lineup, bowling_order_t1, bowling_order_t2, simulations, stadium)
    team_two_bat_first = get_decimal_odds(bbl_bbb, team_two, team_one, team_two_batting_lineup, team_one_batting_lineup, bowling_order_t2, bowling_order_t1, 1, stadium)
  } else if (batting_first == 'T2'){
    team_two_bat_first = get_decimal_odds(bbl_bbb, team_two, team_one, team_two_batting_lineup, team_one_batting_lineup, bowling_order_t2, bowling_order_t1, simulations, stadium)
    team_one_bat_first = get_decimal_odds(bbl_bbb, team_one, team_two, team_one_batting_lineup, team_two_batting_lineup, bowling_order_t1, bowling_order_t2, 1,  stadium)
  } else {
    team_one_bat_first = get_decimal_odds(bbl_bbb, team_one, team_two, team_one_batting_lineup, team_two_batting_lineup, bowling_order_t1, bowling_order_t2, simulations, stadium)
    team_two_bat_first = get_decimal_odds(bbl_bbb, team_two, team_one, team_two_batting_lineup, team_one_batting_lineup, bowling_order_t2, bowling_order_t1, simulations, stadium)
  }
  
  newwb = create_excel_file(team_one, team_two, team_one_bat_first, team_two_bat_first, t1_header_style, t1_cell_style, t2_header_style, t2_cell_style)
  
  excel_file_name = paste(team_one, team_two, Sys.Date(), ".xlsx", sep = "_")
  saveWorkbook(newwb, excel_file_name, TRUE)
}

# bbl_match_odds("Heat", "Strikers", c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), 
#                c(10, 7, 10, 8, 11, 11, 11, 11, 10, 10, 9, 9, 9, 8, 8, 8, 9, 7, 7, 7),
#                c(2, 10, 9, 11, 11, 11, 11, 2, 2, 2, 9, 9, 9, 10, 10, 10, 7, 7, 7, 7), 
#                4000, stadium = "Brisbane Cricket Ground", batting_first = 'T2')
