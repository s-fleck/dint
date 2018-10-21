if (requireNamespace("scales", quietly = TRUE)){
  date_yq_trans <- scales::trans_new(
    name = "date_yq",
    transform = as_yearqtr,
    inverse   = function(x){
      x <- as.numeric(x)
      y <- trunc(x)
      q <- ceiling((x %% 1 + 1/4) * 4)

      assert(all(q %in% 1:5 | is.na(q)))

      y[which(q == 5)] <- y[which(q == 5)] + 1L
      q[which(q == 5)] <- q[which(q == 5)] - 1L

      date_yq(y, q)
    },
    breaks = date_yq_breaks(padded = 1L),
    format = function(x){
      if (all(get_quarter(x) == 1L | is.na(x))){
        as.character(get_year(x))
      } else {
        format_yq_short(x)
      }
    }
  )



  date_ym_trans <- scales::trans_new(
    name = "date_ym",
    transform = as_yearmon,
    inverse   = function(x){
      x <- as.numeric(x)
      y <- trunc(x)
      m <- ceiling((x %% 1 + 1/12) * 12)

      assert(all(m %in% 1:13 | is.na(m)))

      y[which(m == 13)] <- y[which(m == 13)] + 1L
      m[which(m == 13)] <- m[which(m == 13)] - 1L

      date_ym(y, m)
    },
    breaks = date_ym_breaks(padded = 1L),
    format = function(x){
      if (all(get_month(x) == 1L | is.na(x))){
        as.character(get_year(x))
      } else {
        format_ym_short(x)
      }
    }
  )

}





