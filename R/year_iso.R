year_iso <- function(inputyear, boundary=50){
    data.table::fifelse(inputyear > boundary,
                        1900+inputyear,
                        2000+inputyear,
                        na = NA)
}
