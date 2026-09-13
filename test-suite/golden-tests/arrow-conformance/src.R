# Canonical Arrow table fixture and describer, R side. Mirrors src.py:
# same columns, same values, same ASCII description. Integer columns are
# built from strings and cast so 64-bit extremes survive R's 32-bit ints.

.spec <- function() {
  s <- function(...) c(...)
  list(
    list("b",   arrow::bool(),                  c(TRUE, NA, FALSE, TRUE, FALSE)),
    list("i8",  arrow::int8(),                  s("-128", NA, "0", "127", "1")),
    list("i16", arrow::int16(),                 s("-32768", NA, "0", "32767", "2")),
    list("i32", arrow::int32(),                 s("-2147483648", NA, "0", "2147483647", "3")),
    list("i64", arrow::int64(),                 s("-9223372036854775808", NA, "0", "9223372036854775807", "4")),
    list("u8",  arrow::uint8(),                 s("0", NA, "255", "7", "5")),
    list("u16", arrow::uint16(),                s("0", NA, "65535", "7", "6")),
    list("u32", arrow::uint32(),                s("0", NA, "4294967295", "7", "7")),
    list("u64", arrow::uint64(),                s("0", NA, "18446744073709551615", "7", "8")),
    list("f32", arrow::float32(),               c(0.5, NA, -1.5, 100.125, 3.0)),
    list("f64", arrow::float64(),               c(0.5, NA, -1.5, 100.125, 0.001)),
    list("s",   arrow::utf8(),                  c("", NA, "abc", "h\u00e9llo", "x")),
    list("ls",  arrow::large_utf8(),            c("", NA, "abc", "\u65e5\u672c", "y")),
    list("bin", arrow::binary(),                list(raw(0), NULL, as.raw(c(0, 1)), charToRaw("ab"), as.raw(255))),
    list("d",   arrow::date32(),                s("0", NA, "18262", "-1", "1")),
    list("ts",  arrow::timestamp("us", "UTC"),  s("0", NA, "1577836800000000", "-1", "1")),
    list("dur", arrow::duration("us"),          s("0", NA, "86400000000", "-1", "1")),
    list("li",  arrow::list_of(arrow::int64()), list(integer(0), NULL, c(1L, 2L, 3L), 4L, c(5L, 6L)))
  )
}

.storage <- function(type) {
  switch(type$ToString(),
    "date32[day]" = arrow::int32(),
    "timestamp[us, tz=UTC]" = arrow::int64(),
    "duration[us]" = arrow::int64(),
    type)
}

# Numeric and temporal values arrive as strings (64-bit extremes do not fit
# an R integer); strings are cast through the storage type. Text columns
# are built directly.
.build_col <- function(type, vals) {
  t <- type$ToString()
  if (is.character(vals) && !(t %in% c("string", "large_string"))) {
    a <- arrow::Array$create(vals, type = arrow::utf8())$cast(.storage(type))
    if (.storage(type)$ToString() != t) a <- a$cast(type)
    return(a)
  }
  arrow::Array$create(vals, type = type)
}

mk <- function(case) {
  cols <- list()
  for (c in .spec()) {
    name <- c[[1]]; type <- c[[2]]; vals <- c[[3]]
    v <- if (case == "full") vals
         else if (case == "one") vals[1]
         else if (case == "empty") vals[0]
         else if (case == "allnull") { if (is.list(vals)) rep(list(NULL), 5) else rep(NA, 5) }
         else stop(paste("unknown case", case))
    if (case == "allnull") {
      cols[[name]] <- if (is.list(vals)) arrow::Array$create(rep(list(NULL), 5), type = type)
                      else arrow::Array$create(rep(NA_character_, 5), type = arrow::utf8())$cast(.storage(type))$cast(type)
    } else if (case == "empty" && is.list(vals)) {
      cols[[name]] <- arrow::Array$create(list(), type = type)
    } else {
      cols[[name]] <- .build_col(type, v)
    }
  }
  do.call(arrow::record_batch, cols)
}

.esc <- function(bytes) {
  if (length(bytes) == 0) return("")
  paste(vapply(as.integer(bytes), function(b) {
    if (b >= 0x20 && b <= 0x7e && b != 0x5c) rawToChar(as.raw(b)) else sprintf("\\x%02x", b)
  }, character(1)), collapse = "")
}

.tname <- function(type) {
  switch(type$ToString(),
    "bool" = "bool", "int8" = "i8", "int16" = "i16", "int32" = "i32", "int64" = "i64",
    "uint8" = "u8", "uint16" = "u16", "uint32" = "u32", "uint64" = "u64",
    "float" = "f32", "double" = "f64", "string" = "utf8", "large_string" = "large_utf8",
    "binary" = "binary", "date32[day]" = "date32",
    "timestamp[us, tz=UTC]" = "ts_us_UTC", "duration[us]" = "dur_us",
    "list<item: int64>" = "list_i64",
    paste0("?", type$ToString()))
}

.fmt_col <- function(col) {
  t <- col$type$ToString()
  n <- length(col)
  if (n == 0) return(character(0))
  isnull <- vapply(seq_len(n) - 1L, function(i) col$IsNull(i), logical(1))
  out <- character(n)
  if (t == "bool") {
    v <- as.vector(col); out <- ifelse(v, "true", "false")
  } else if (t %in% c("float", "double")) {
    v <- as.vector(col); out <- sprintf("%g", v)
  } else if (t %in% c("string", "large_string")) {
    v <- as.vector(col$cast(arrow::utf8()))
    out <- vapply(v, function(x) if (is.na(x)) "" else .esc(charToRaw(enc2utf8(x))), character(1))
  } else if (t == "binary") {
    v <- as.vector(col)
    out <- vapply(v, function(x) if (is.null(x)) "" else .esc(x), character(1))
  } else if (t == "list<item: int64>") {
    v <- as.vector(col)
    out <- vapply(v, function(x) {
      if (is.null(x)) return("")
      paste0("[", paste(as.vector(arrow::Array$create(x)$cast(arrow::int64())$cast(arrow::utf8())), collapse = ","), "]")
    }, character(1))
  } else {
    storage <- switch(t, "date32[day]" = arrow::int32(),
                         "timestamp[us, tz=UTC]" = arrow::int64(),
                         "duration[us]" = arrow::int64(), col$type)
    out <- as.vector(col$cast(storage)$cast(arrow::utf8()))
  }
  out[isnull] <- "null"
  out
}

describe <- function(rb) {
  lines <- sprintf("rows=%d cols=%d", rb$num_rows, rb$num_columns)
  for (name in names(rb)) {
    col <- rb[[name]]
    lines <- c(lines, sprintf("%s %s n=%d nulls=%d [%s]", name, .tname(col$type),
                              length(col), col$null_count, paste(.fmt_col(col), collapse = ",")))
  }
  paste(lines, collapse = "\n")
}
