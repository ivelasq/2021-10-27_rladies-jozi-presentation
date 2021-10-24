
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Around the World in a Single Table 🎈

## 2021-10-27 R-Ladies Johannesburg Presentation

I will be discussing tables as a format for communicating information
and walking through creating a table in R using the {gt} package.

📆 [Event
Page](https://www.meetup.com/rladies-johannesburg/events/281522272/)

📝
[Presentation](https://ivelasq.github.io/2021-10-27_rladies-jozi-presentation/slides.html)

💻 [RStudio Cloud (requires
login)](https://rstudio.cloud/project/3093601)

🔗 Links mentioned in talk:

-   [Isabella’s Twitter](https://twitter.com/ivelasq3)
-   [2021 RStudio Table
    Contest](https://blog.rstudio.com/2021/09/30/rstudio-table-contest-2021/)
-   [Ryan Estrellado’s website](https://ryanestrellado.com/)
-   [Visual Thinking Graphic Discoveries by Michael
    Friendly](https://medium.com/@michael.friendly/visual-thinking-graphic-discoveries-128468677592)
-   [Francisco Requena’s tutorial on Exploring World Airline
    Network](https://franciscorequena.com/post/exploring-world-airline-network/)

📣 More on gt:

-   [Package website](https://gt.rstudio.com/index.html)
-   [More on
    tab\_style](https://gt.rstudio.com/reference/tab_style.html)
-   [ALL the table
    options!](https://gt.rstudio.com/reference/tab_options.html)
-   [GT Cookbook by Thomas
    Mock](https://themockup.blog/static/gt-cookbook.html#Introduction)
-   [Advanced Usage by Thomas
    Mock](https://themockup.blog/static/gt-cookbook-advanced.html)
-   [Beautiful tables in
    R](https://themockup.blog/static/slides/intro-tables.html#1)
-   [gtExtra](https://jthomasmock.github.io/gtExtras/)
-   [gtsummary](https://cran.r-project.org/web/packages/gtsummary/vignettes/rmarkdown.html)

<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8"/>
<style>body{background-color:white;}</style>


</head>
<body>
<div id="mreovxulay" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  <style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#mreovxulay .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: 800px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#mreovxulay .gt_heading {
  background-color: #2A326D;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#mreovxulay .gt_title {
  color: #FFFFFF;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#mreovxulay .gt_subtitle {
  color: #FFFFFF;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#mreovxulay .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mreovxulay .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#mreovxulay .gt_col_heading {
  color: #333333;
  background-color: #F4CF0D;
  font-size: 100%;
  font-weight: bold;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#mreovxulay .gt_column_spanner_outer {
  color: #333333;
  background-color: #F4CF0D;
  font-size: 100%;
  font-weight: bold;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#mreovxulay .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#mreovxulay .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#mreovxulay .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#mreovxulay .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#mreovxulay .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#mreovxulay .gt_from_md > :first-child {
  margin-top: 0;
}

#mreovxulay .gt_from_md > :last-child {
  margin-bottom: 0;
}

#mreovxulay .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#mreovxulay .gt_stub {
  color: #333333;
  background-color: #F4CF0D;
  font-size: 100%;
  font-weight: bold;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#mreovxulay .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#mreovxulay .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#mreovxulay .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#mreovxulay .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#mreovxulay .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#mreovxulay .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mreovxulay .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#mreovxulay .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#mreovxulay .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#mreovxulay .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#mreovxulay .gt_left {
  text-align: left;
}

#mreovxulay .gt_center {
  text-align: center;
}

#mreovxulay .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#mreovxulay .gt_font_normal {
  font-weight: normal;
}

#mreovxulay .gt_font_bold {
  font-weight: bold;
}

#mreovxulay .gt_font_italic {
  font-style: italic;
}

#mreovxulay .gt_super {
  font-size: 65%;
}

#mreovxulay .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
  <table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="7" class="gt_heading gt_title gt_font_normal" style>O. R. Tambo International Airport</th>
    </tr>
    <tr>
      <th colspan="7" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style><font size=6><strong>Departures</strong></font></th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" style="border-bottom-width: 6px; border-bottom-style: solid; border-bottom-color: white;">To</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" style="border-bottom-width: 6px; border-bottom-style: solid; border-bottom-color: white;">Flight</th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="5">
        <span class="gt_column_spanner">You are in Terminal A</span>
      </th>
    </tr>
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="border-bottom-width: 6px; border-bottom-style: solid; border-bottom-color: white;">Terminal</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="border-bottom-width: 6px; border-bottom-style: solid; border-bottom-color: white;">Gate</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="border-bottom-width: 6px; border-bottom-style: solid; border-bottom-color: white;">Original Time</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="border-bottom-width: 6px; border-bottom-style: solid; border-bottom-color: white;">Status</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="border-bottom-width: 6px; border-bottom-style: solid; border-bottom-color: white;">Time Before Departure</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <td colspan="7" class="gt_group_heading">1</td>
    </tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Abu Dhabi</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>AUH</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">C512</td>
<td class="gt_row gt_right" style="color: #D55E00; font-weight: bold; background-color: #FFF8E7;">A</td>
<td class="gt_row gt_center" style="color: #0072B2; font-weight: bold; background-color: #FFF8E7;">10</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">16:58</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Canceled</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:0%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Accra</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>ACC</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">A882</td>
<td class="gt_row gt_right" style="color: #D55E00; font-weight: bold; background-color: #FFF8E7;">A</td>
<td class="gt_row gt_center" style="color: #0072B2; font-weight: bold; background-color: #FFF8E7;">2</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">10:12</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:81.8023277855708%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Addis Ababa</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>ADD</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">F595</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">B</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">12</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">12:06</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Delayed</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>Now 20:16</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:92.1901656746408%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Amsterdam</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>AMS</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">B450</td>
<td class="gt_row gt_right" style="color: #D55E00; font-weight: bold; background-color: #FFF8E7;">A</td>
<td class="gt_row gt_center" style="color: #0072B2; font-weight: bold; background-color: #FFF8E7;">12</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">14:51</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:99.2417636577321%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Antananarivo</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>TNR</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">E787</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">C</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">15</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">17:27</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Delayed</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>Now 20:45</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:94.3890510672177%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Atlanta</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>ATL</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">B501</td>
<td class="gt_row gt_right" style="color: #D55E00; font-weight: bold; background-color: #FFF8E7;">A</td>
<td class="gt_row gt_center" style="color: #0072B2; font-weight: bold; background-color: #FFF8E7;">12</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">01:39</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Canceled</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:0%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Bangkok</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>BKK</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">A587</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">B</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">2</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">16:10</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Delayed</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>Now 19:06</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:86.8825112787656%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Beira</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>BEW</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">D533</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">D</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">15</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">11:08</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:86.5792167418584%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Blantyre</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>BLZ</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">F529</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">C</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">17</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">11:10</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:85.0627440573227%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Bloemfontein</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>BFN</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">C699</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">C</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">19</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">17:49</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:78.9968533191796%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Brazzaville</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>BZV</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">F743</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">B</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">3</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">14:25</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:77.783675171551%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Bulawayo</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>BUQ</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">C646</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">B</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">12</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">01:51</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:87.0341585472192%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Cairo</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>CAI</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">C813</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">B</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">5</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">17:28</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:79.1485005876332%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Cape Town</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>CPT</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">E454</td>
<td class="gt_row gt_right" style="color: #D55E00; font-weight: bold; background-color: #FFF8E7;">A</td>
<td class="gt_row gt_center" style="color: #0072B2; font-weight: bold; background-color: #FFF8E7;">4</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">13:07</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:81.5748568828904%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Dakar</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>DKR</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">F770</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">C</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">15</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">01:20</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:81.4990332486636%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Dar Es Salaam</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>DAR</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">A795</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">D</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">4</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">12:10</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:91.9626947719604%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Dubai</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>DXB</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">E769</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">D</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">10</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">13:22</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:9.54240436744133%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Durban</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>DUR</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">F491</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">D</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">14</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">01:17</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Delayed</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>Now 02:43</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:12.3478788338325%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>East London</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>ELS</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">B803</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">D</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">12</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">11:21</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Canceled</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:0%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Entebbe</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>EBB</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">A696</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">D</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">10</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">17:14</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:99.0901163892785%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Francistown</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>FRW</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">C487</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">C</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">6</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">13:19</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:91.2044584296925%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Frankfurt</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>FRA</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">F489</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">B</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">9</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">11:51</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:92.6451074800015%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Gaberone</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>GBE</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">A671</td>
<td class="gt_row gt_right" style="color: #D55E00; font-weight: bold; background-color: #FFF8E7;">A</td>
<td class="gt_row gt_center" style="color: #0072B2; font-weight: bold; background-color: #FFF8E7;">13</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">14:54</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Canceled</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:0%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>George</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>GRJ</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">D888</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">C</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">9</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">15:31</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:86.8825112787656%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Harare</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>HRE</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">E556</td>
<td class="gt_row gt_right" style="color: #D55E00; font-weight: bold; background-color: #FFF8E7;">A</td>
<td class="gt_row gt_center" style="color: #0072B2; font-weight: bold; background-color: #FFF8E7;">9</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">10:46</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:96.6637600940213%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Hoedspruit</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>HDS</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">B776</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">D</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">12</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">14:40</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Delayed</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>Now 20:37</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:93.7824619934033%;height:16px;'></div></div></td></tr>
    <tr class="gt_group_heading_row">
      <td colspan="7" class="gt_group_heading">2</td>
    </tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Hong Kong</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>HKG</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">E999</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">D</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">6</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">15:27</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:10.4522879781628%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Inhambane</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>INH</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">D684</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">D</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">2</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">12:53</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:83.0155059331994%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Jeddah</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>JED</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">D590</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">D</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">15</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">13:03</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:99.2417636577321%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Kasane</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>BBK</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">D705</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">B</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">16</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">01:10</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:77.6320279030974%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Kigali</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>KGL</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">D770</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">B</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">13</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">15:19</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:98.8626454865982%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Kimberley</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>KIM</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">D795</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">C</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">7</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">15:44</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:88.5506312317549%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Kinshasa</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>FIH</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">B689</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">C</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">7</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">14:41</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:95.9813473859802%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Lagos</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>LOS</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">F627</td>
<td class="gt_row gt_right" style="color: #D55E00; font-weight: bold; background-color: #FFF8E7;">A</td>
<td class="gt_row gt_center" style="color: #0072B2; font-weight: bold; background-color: #FFF8E7;">15</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">16:16</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:85.8209803995906%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Libreville</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>LBV</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">E449</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">D</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">4</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">13:48</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:84.3045077150548%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Lilongwe</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>LLW</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">D555</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">B</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">2</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">13:13</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Canceled</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:0%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Livingstone</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>LVI</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">A540</td>
<td class="gt_row gt_right" style="color: #D55E00; font-weight: bold; background-color: #FFF8E7;">A</td>
<td class="gt_row gt_center" style="color: #0072B2; font-weight: bold; background-color: #FFF8E7;">19</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">01:42</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:13.5610569814611%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>London</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>LHR</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">F979</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">D</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">12</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">13:55</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Delayed</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>Now 17:42</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:80.5133260037154%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Luanda</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>LAD</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">A719</td>
<td class="gt_row gt_right" style="color: #D55E00; font-weight: bold; background-color: #FFF8E7;">A</td>
<td class="gt_row gt_center" style="color: #0072B2; font-weight: bold; background-color: #FFF8E7;">17</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">14:11</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:93.9341092618569%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Lubumashi</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>FBM</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">D889</td>
<td class="gt_row gt_right" style="color: #D55E00; font-weight: bold; background-color: #FFF8E7;">A</td>
<td class="gt_row gt_center" style="color: #0072B2; font-weight: bold; background-color: #FFF8E7;">17</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">11:27</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:89.3088675740228%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Lusaka</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>LUN</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">F766</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">B</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">20</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">13:12</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:88.8539257686621%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Mahe</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>SEZ</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">C521</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">C</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">13</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">13:40</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:9.23910983053418%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Manzini</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>MTS</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">A696</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">C</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">5</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">15:57</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:81.4232096144368%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Maputo</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>MPM</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">A716</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">B</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">3</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">16:12</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:84.4561549835084%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Margate</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>MGH</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">D962</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">B</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">4</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">17:05</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:89.8396330136103%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Maseru</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>MSU</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">D949</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">C</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">2</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">13:28</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:81.5748568828904%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Maun</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>MUB</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">B963</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">D</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">8</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">12:49</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:81.1199150775297%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Mpumalanga</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>MQP</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">C684</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">D</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">5</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">13:48</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Delayed</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>Now 19:35</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:89.0813966713425%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Mumbai</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>BOM</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">F429</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">C</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">2</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">10:09</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Canceled</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:0%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Munich</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>MUC</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">C425</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">D</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">20</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">01:47</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Delayed</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>Now 19:00</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:86.4275694734049%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Nairobi</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>NBO</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">E945</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">B</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">18</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">01:52</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:98.8626454865982%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Nampula</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>APL</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">C748</td>
<td class="gt_row gt_right" style="color: #D55E00; font-weight: bold; background-color: #FFF8E7;">A</td>
<td class="gt_row gt_center" style="color: #0072B2; font-weight: bold; background-color: #FFF8E7;">14</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">11:00</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:87.8682185237138%;height:16px;'></div></div></td></tr>
    <tr class="gt_group_heading_row">
      <td colspan="7" class="gt_group_heading">3</td>
    </tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Ndola</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>NLA</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">C473</td>
<td class="gt_row gt_right" style="color: #D55E00; font-weight: bold; background-color: #FFF8E7;">A</td>
<td class="gt_row gt_center" style="color: #0072B2; font-weight: bold; background-color: #FFF8E7;">13</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">14:24</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:87.2616294498995%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>New York</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>JFK</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">E710</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">C</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">19</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">01:14</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:79.6034423929939%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Paris</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>CDG</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">C902</td>
<td class="gt_row gt_right" style="color: #D55E00; font-weight: bold; background-color: #FFF8E7;">A</td>
<td class="gt_row gt_center" style="color: #0072B2; font-weight: bold; background-color: #FFF8E7;">8</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">01:56</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:77.5562042688706%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Pemba</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>POL</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">D861</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">B</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">20</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">15:51</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Canceled</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:0%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Perth</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>PER</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">E610</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">B</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">1</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">15:56</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:100%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Phalaborwa</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>PHW</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">D411</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">D</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">9</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">10:57</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:11.817113394245%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Pietermaritzburg</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>PZB</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">A901</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">C</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">3</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">13:46</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:87.7165712552603%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Plaisance</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>MRU</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">D426</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">B</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">10</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">12:03</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:10.4522879781628%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Pointe-noire</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>PNR</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">C516</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">C</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">14</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">15:33</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:94.1615801645373%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Port Elizabeth</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>PLZ</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">E749</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">B</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">15</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">14:42</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:99.8483527315464%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Potgietersrus</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>PTG</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">E851</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">B</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">4</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">17:59</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:9.0874625620806%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Richard's Bay</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>RCB</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">A681</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">D</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">12</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">12:24</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:80.4375023694886%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Sao Paulo</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>GRU</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">C568</td>
<td class="gt_row gt_right" style="color: #D55E00; font-weight: bold; background-color: #FFF8E7;">A</td>
<td class="gt_row gt_center" style="color: #0072B2; font-weight: bold; background-color: #FFF8E7;">5</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">01:00</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:77.4803806346438%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Singapore</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>SIN</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">B946</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">C</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">19</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">14:00</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:83.5462713727869%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>St.-denis</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>RUN</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">B912</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">D</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">9</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">01:03</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Canceled</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:0%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Sydney</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>SYD</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">A441</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">C</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">18</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">16:20</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:79.906736929901%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Tel-aviv</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>TLV</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">B402</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">D</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">9</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">13:22</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:11.2105243204307%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Tete</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>TET</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">D854</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">D</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">7</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">15:32</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Delayed</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>Now 17:28</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:79.4517951245403%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Umtata</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>UTT</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">B639</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">C</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">15</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">16:08</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Delayed</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>Now 18:06</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:82.3330932251583%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Upington</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>UTN</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">E918</td>
<td class="gt_row gt_right" style="color: #D55E00; font-weight: bold; background-color: #FFF8E7;">A</td>
<td class="gt_row gt_center" style="color: #0072B2; font-weight: bold; background-color: #FFF8E7;">11</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">16:58</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:79.8309132956743%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Victoria Falls</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>VFA</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">C443</td>
<td class="gt_row gt_right" style="color: #D55E00; font-weight: bold; background-color: #FFF8E7;">A</td>
<td class="gt_row gt_center" style="color: #0072B2; font-weight: bold; background-color: #FFF8E7;">2</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">11:07</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:96.2088182886606%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Vilankulu</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>VNX</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">A783</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">C</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">15</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">17:06</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:87.2616294498995%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Walvis Bay</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>WVB</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">D432</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">C</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">7</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">10:08</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:99.5450581946393%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Windhoek</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>WDH</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">B641</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">B</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">11</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">01:18</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:82.7122113962922%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Zanzibar</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>ZNZ</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">C933</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">B</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">3</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">10:12</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:78.5419115138189%;height:16px;'></div></div></td></tr>
    <tr><td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>Zurich</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>ZRH</span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7; border-left-width: 6px; border-left-style: solid; border-left-color: white;">F446</td>
<td class="gt_row gt_right" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">B</td>
<td class="gt_row gt_center" style="color: #3A3B3C; font-weight: bold; background-color: #FFF8E7;">10</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;">14:47</td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;color:black;font-size:14px'>On Time</div>
<div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'></span></div></td>
<td class="gt_row gt_left" style="background-color: #FFF8E7;"><div style='flex-grow:1;margin-left:8px;background:#DABD8F;'><div style='background:#D90707;width:79.6034423929939%;height:16px;'></div></div></td></tr>
  </tbody>
  <tfoot class="gt_sourcenotes">
    <tr>
      <td class="gt_sourcenote" colspan="7">Thursday, October 27th, 2021 6:00:00 UTC + 2</td>
    </tr>
  </tfoot>
  
</table>
</div>
</body>
</html>
