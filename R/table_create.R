# Libraries
library(gt)
# remotes::install_github("jthomasmock/gtExtras")
library(gtExtras)

# Data
source("R/data_fake.R")

head(df_routes_jozi)

j1 <-
  df_routes_jozi %>%
  select(city.y,
         dest,
         flight_no,
         terminal,
         gate,
         depart_time,
         status,
         time_before_depart) %>%
  arrange(city.y)

j1 %>% 
  mutate(page = ntile(city.y, 3)) %>% 
  gt(groupname_col = "page") %>% 
  tab_header(title = "O. R. Tambo International Airport",
             subtitle = html("<font size=6><strong>Departures</strong></font>")) %>% 
  cols_label(
    city.y = "To",
    dest = "Code",
    flight_no = "Flight",
    terminal = "Terminal",
    gate = "Gate",
    depart_time = "Original Time",
    status = "Status",
    time_before_depart = "Time Before Departure"
  ) %>% 
  tab_spanner(label = "You are in Terminal A",
                columns = 4:last_col()) %>% 
  tab_source_note("Thursday, October 27th, 2021 6:00:00 UTC + 2") %>% 
  gt_plt_bar_pct(column = time_before_depart,
                 scaled = FALSE,
                 fill = "#D90707",
                 background = "#DABD8F") %>%
  cols_align(align = "right",
             columns = "depart_time") %>%
  tab_style(
    style = list(
      cell_text(weight = "bold",
                color = "red")
    ),
    locations = cells_body(
      columns = status,
      rows = status == "Canceled"
    )) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold",
                color = "orange")
    ),
    locations = cells_body(
      columns = status,
      rows = str_detect(status, "Delayed")
    )) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold",
                color = "green")
    ),
    locations = cells_body(
      columns = status,
      rows = str_detect(status, "On Time")
    )) %>% 
  gt_merge_stack(col1 = city.y, col2 = dest) %>% 
  # Code from https://aniruhil.org/posts/2021-02-08-more-with-gt-tables/
  tab_options(
    table.width = px(800),
    heading.background.color = "#2A326D",
    column_labels.background.color = "#F4CF0D",
    column_labels.font.weight = "bold",
    stub.background.color = "#F4CF0D",
    stub.font.weight = "bold"
  ) %>% 
  tab_style(
    style = list(
      cell_fill(color = "#fff8e7")
    ),
    locations = cells_body(
    )    
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "left",
        color = "white",
        weight = px(6)
      )
    ),
    locations = list(
      cells_body(
        columns = 'flight_no'
      )
    )
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "white",
        weight = px(6)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  )

