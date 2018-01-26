# Test GoogleAuthR
library(googleAuthR)

# scopes
options("googleAuthR.scopes.selected" = "https://www.googleapis.com/auth/spreadsheets.readonly")
# client id and secret
options("googleAuthR.client_id" = "422259808797-e8gvgvq0ifiuluu5d4aevqe7h8884d3a.apps.googleusercontent.com")
options("googleAuthR.client_secret" = "DbxgAd6XhalwQRUzeFQ8-fIH")
options(googleAuthR.verbose = 1)

# request
get_data <- function(spreadsheetId, range) {

    l <- googleAuthR::gar_api_generator(
        baseURI = "https://sheets.googleapis.com/v4/",
        http_header = 'GET',
        path_args = list(spreadsheets = spreadsheetId,
                         values = URLencode(range)),
        pars_args = list(majorDimension = 'ROWS',
                         valueRenderOption = 'UNFORMATTED_VALUE'),
        data_parse_function = function(x) x)

    req <- l()

    req
}

# authenticate
gar_auth(new_user = TRUE)

# input
spreadsheet_id <- "1No6vz_JZACW6wAPuRfHMPiT0okeZtMoNwzBPISDko4c"
range <- URLencode("Sheet1!A:B")

# get data
df <- get_data(spreadsheet_id, range)

# above does not work because of space in the range variable. Eventually failing
# because of a json call:

# Token exists.
# Valid local token
# Request: https://sheets.googleapis.com/v4/spreadsheets/1No6vz_JZACW6wAPuRfHMPiT0okeZtMoNwzBPISDko4c/values/'Sheet 1'!A:B?majorDimension=ROWS&valueRenderOption=UNFORMATTED_VALUE
# -> GET /v4/spreadsheets/1No6vz_JZACW6wAPuRfHMPiT0okeZtMoNwzBPISDko4c/values/'Sheet 1'!A:B?majorDimension=ROWS&valueRenderOption=UNFORMATTED_VALUE HTTP/1.1
# -> Host: sheets.googleapis.com
# -> User-Agent: googleAuthR/0.5.1 (gzip)
# -> Accept: application/json, text/xml, application/xml, */*
#     -> Accept-Encoding: gzip
# -> Authorization: Bearer ya29.GlvmBDKaXGMH2k5JNA0YPFPjqrHgBMMscJIjnSFWRVLNZonHZ7NMFfZM6E8Vb26V8c4GHeuRfrz15ub3bjxX5IyIgpTxqm4NUgAkBA9pk9XAvwjga2nR6EYEw2YP
# ->
#     <- HTTP/1.0 400 Bad Request
# <- Content-Type: text/html; charset=UTF-8
# <- Referrer-Policy: no-referrer
# <- Content-Length: 1555
# <- Date: Mon, 16 Oct 2017 14:58:37 GMT
# <-
#     Request Status Code: 400
# Error : lexical error: invalid char in json text.
# <!DOCTYPE html> <html lang=en>
#     (right here) ------^
#
#     JSON fetch error: Could not fetch response
# No retry attempted: Could not fetch response
# Error: lexical error: invalid char in json text.
# <!DOCTYPE html> <html lang=en>
#     (right here) ------^
#     In addition: Warning message:
#     Show Traceback
#
# Rerun with Debug
# Error: lexical error: invalid char in json text.
# <!DOCTYPE html> <html lang=en>
#     (right here) ------^

# ====
# Stack trace
# ====
# gar_api_generator
# need to URLencode the value
