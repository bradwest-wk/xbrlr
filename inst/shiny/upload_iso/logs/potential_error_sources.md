2017-09-19T15:09:36.456554+00:00 shinyapps[208972]: Warning: Error in gs_lookup: "ASC 225 Income" doesn't match sheet_title of any sheet returned by gs_ls() (which should reflect user's Google Sheets home screen)

2017-09-19T19:43:33.898937+00:00 shinyapps[208972]: Warning: Error in gs_lookup: "consolidated taxonomy" doesn't match sheet_title of any sheet returned by gs_ls() (which should reflect user's Google Sheets home screen).

Error from incorrect name.

2017-09-19T19:48:40.714352+00:00 shinyapps[208972]: Warning: Error in : Worksheet Pivotable Table not found.
2017-09-19T19:48:40.720239+00:00 shinyapps[208972]: Stack trace (innermost first):
2017-09-19T19:48:40.720240+00:00 shinyapps[208972]:     97: spf
2017-09-19T19:48:40.720242+00:00 shinyapps[208972]:     95: gs_read_csv
2017-09-19T19:48:40.720242+00:00 shinyapps[208972]:     94: googlesheets::gs_read
2017-09-19T19:48:40.720243+00:00 shinyapps[208972]:     93: get_df_tx
2017-09-19T19:48:40.720243+00:00 shinyapps[208972]:     92: eval
2017-09-19T19:48:40.720243+00:00 shinyapps[208972]:     91: eval
2017-09-19T19:48:40.720244+00:00 shinyapps[208972]:     90: withProgress
2017-09-19T19:48:40.720244+00:00 shinyapps[208972]:     89: <reactive>
2017-09-19T19:48:40.720244+00:00 shinyapps[208972]:     78: get_df_reactive
2017-09-19T19:48:40.720245+00:00 shinyapps[208972]:     77: observeEventHandler [/srv/connect/apps/upload_iso/app.R#155]
2017-09-19T19:48:40.720241+00:00 shinyapps[208972]:     96: gs_ws
2017-09-19T19:48:40.720246+00:00 shinyapps[208972]:     12: fn
2017-09-19T19:48:40.720250+00:00 shinyapps[208972]:      3: eval
2017-09-19T19:48:40.720246+00:00 shinyapps[208972]:     11: doTryCatch
2017-09-19T19:48:40.720250+00:00 shinyapps[208972]:      2: eval.parent
2017-09-19T19:48:40.720248+00:00 shinyapps[208972]:      7: connect$retry
2017-09-19T19:48:40.720250+00:00 shinyapps[208972]:      1: local
2017-09-19T19:48:40.720249+00:00 shinyapps[208972]:      6: eval
2017-09-19T19:48:40.720249+00:00 shinyapps[208972]:      4: eval
2017-09-19T19:48:40.720245+00:00 shinyapps[208972]:     13: runApp
2017-09-19T19:48:40.720249+00:00 shinyapps[208972]:      5: eval
2017-09-19T19:48:40.720247+00:00 shinyapps[208972]:     10: tryCatchOne
2017-09-19T19:48:40.720248+00:00 shinyapps[208972]:      8: tryCatch
2017-09-19T19:48:40.720247+00:00 shinyapps[208972]:      9: tryCatchList
2017-09-19T19:48:48.823177+00:00 shinyapps[208972]: Stack trace (innermost first):
2017-09-19T19:48:48.813756+00:00 shinyapps[208972]: Warning: Error in function_list[[k]]: Unauthorized (HTTP 401).

Error from incorrect sheet name. Creates a stack trace?
Seems like incorrect sheet name creates Warning: Error in function_list[[k]]: Unauthorized (HTTP 401) each time

2017-09-25T18:57:46.061316+00:00 shinyapps[208972]: Warning: Error in function_list[[k]]: Unauthorized (HTTP 401).
2017-09-25T18:57:46.067818+00:00 shinyapps[208972]: Stack trace (innermost first):
2017-09-25T18:57:46.067821+00:00 shinyapps[208972]:     121: httr::stop_for_status

Need to investigate what the httr::stop_for_status file is.
