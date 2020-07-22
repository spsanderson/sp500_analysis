
# Lib Load ----------------------------------------------------------------

if(!require(pacman)) install.packages("pacman")
pacman::p_load(
    "tidyquant"
    , "modeltime"
    , "timetk"
    , "anomalize"
    , "tidyverse"
    , "lubridate"
)


# Get SP500 Index ---------------------------------------------------------

sp500_co_data_tbl <- tq_index("SP500")
sp500_prices_tbl  <- tq_get(x = sp500_co_data_tbl$symbol)
sp500_tbl <- sp500_prices_tbl %>%
    left_join(sp500_co_data_tbl, by = c("symbol"="symbol"))

sp500_tbl

asset_returns_tbl <- sp500_tbl %>%
    select(symbol, date, adjusted) %>%
    group_by(symbol) %>%
    tq_transmute(
        mutate_fun = periodReturn
        , period = "monthly"
        , type = "log"
        , col_rename = "returns"
    ) %>%
    pivot_wider(
        names_from = symbol
        , values_from = returns
    ) %>%
    slice(-1)
asset_returns_tbl

asset_returns_long <- pivot_longer(
        data = asset_returns_tbl
        , cols = -date
        , names_to = "symbol"
        , values_to = "returns"
    ) %>%
    drop_na()

asset_returns_long %>%
    ggplot(
        mapping = aes(
            x = date
            , y = returns
            , group = symbol
        )
    ) +
    geom_line()
