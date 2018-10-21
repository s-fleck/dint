if (requireNamespace("scales", quietly = TRUE)){
  date_yq_trans <- scales::trans_new(
    name = "date_yq",
    transform = as_yearqtr,
    inverse   = function(x){
      y <- trunc(x)
      q <- ceiling((x %% 1 + 0.25) * 4)

      assert(all(q %in% 1:5))

      y[q == 5] <- y[q == 5] + 1L
      q[q == 5] <- q[q == 5] - 1L

      date_yq(y, q)
    },
    breaks = date_yq_breaks(padded = 1L),
    format = function(x){
      if (all(get_quarter(x) == 1L)){
        as.character(get_year(x))
      } else {
        format_yq_short(x)
      }
    }
  )
}





