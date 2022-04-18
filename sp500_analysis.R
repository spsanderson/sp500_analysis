
# Lib Load ----------------------------------------------------------------

if(!require(pacman)) install.packages("pacman")
pacman::p_load(
    "tidyquant"
    , "modeltime"
    , "timetk"
    , "anomalize"
    , "tidyverse"
    , "lubridate"
    , "fs"
    , "fst"
    , "arrow"
)


# Get SP500 Index ---------------------------------------------------------

sp500_co_data_tbl <- tq_index("SP500")
symbol_sector <- sp500_co_data_tbl %>%
    distinct(symbol, sector)
sectors <- unique(sp500_co_data_tbl$sector) %>% as_tibble()

# Load data if exists
if(fs::file_exists("asset_history.rds")) {
    asset_history <- read_rds("asset_history.rds")
    
    # Add data if needed
    td <- Sys.Date() %>% lubridate::as_date()
    md <- max(asset_history$date)
    if(md < td) {
        asset_tmp <- tq_get(
            x = sp500_co_data_tbl$symbol
            , from = md + ddays(1)
        ) %>%
            left_join(sp500_co_data_tbl, by = c("symbol"="symbol"))
        
        # Union results
        asset_history %>%
            union(asset_tmp) %>%
            # Write out file
            write_rds("asset_history.rds")
        
        # Get Returns in Long format
        asset_tmp_ret <- asset_history %>%
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
        
        asset_tmp_long <- pivot_longer(
            data = asset_tmp_ret
            , cols = -date
            , names_to = "symbol"
            , values_to = "returns"
        ) %>%
            drop_na()
        
        # Write file out
        write_rds(x = asset_tmp_long, "asset_returns_long.rds")
        # Load asset returns long
        asset_returns_long <- read_rds("asset_returns_long.rds")
    }

} else {
    
    # if file dne then get
    sp500_prices_tbl <- tq_get(x = sp500_co_data_tbl$symbol)
    
    # Join co data tbl
    sp500_tbl <- sp500_prices_tbl %>%
        left_join(sp500_co_data_tbl, by = c("symbol"="symbol")) %>%
        write_rds("asset_history.rds")
    
    # Get returns
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
    
    # Make data long
    asset_returns_long <- pivot_longer(
        data = asset_returns_tbl
        , cols = -date
        , names_to = "symbol"
        , values_to = "returns"
    ) %>%
        drop_na()
    
    # Write file out
    write_rds(x = asset_returns_long, "asset_returns_long.rds")
    # Load asset returns long
    asset_returns_long <- read_rds("asset_returns_long.rds")
}

# Initial Vizualizations --------------------------------------------------


asset_returns_long_sector_tbl <- asset_returns_long %>%
    filter(!symbol == "TT") %>%
    left_join(symbol_sector, by = c("symbol" = "symbol"))

asset_returns_long_sector_tbl %>%
    ggplot(
        mapping = aes(
            x = date
            , y = returns
            , group = symbol
            , color = sector
        )
    ) +
    geom_line() +
    facet_wrap(~ sector, scales = "free_y") +
    theme_tq() +
    scale_color_tq() +
    labs(
        title = "Monthly Log Returns for Assets by Sector"
    ) +
    theme(legend.position = "none")

asset_returns_long_sector_tbl %>%
    ggplot(
        mapping = aes(
            x = returns
            , group = symbol
            , color = sector
        )
    ) +
    geom_density() +
    facet_wrap(~ sector, scales = "free_y") +
    theme_tq() +
    scale_color_tq() +
    labs(
        title = "Monthly Log Returns for Assets by Sector"
    ) +
    theme(legend.position = "none")

timetk::plot_time_series(
    .data = asset_returns_long %>%
        filter(!symbol == "TT") %>%
        left_join(symbol_sector, by = c("symbol" = "symbol"))
    , .date_var = date
    , .value = returns
    , .color_var = sector
    , .facet_vars = sector
    , .facet_ncol = 3
)


# Mean Returns by Sector --------------------------------------------------

mean_ret_by_sector_tbl <- asset_returns_long_sector_tbl %>%
    filter(!symbol == "TT") %>%
    select(date, sector, returns) %>%
    group_by(date, sector) %>%
    summarise(returns = mean(returns, na.rm = TRUE)) %>%
    ungroup()

timetk::plot_time_series(
    .data = mean_ret_by_sector_tbl
    , .date_var = date
    , .value = returns 
    , .color_var = sector
    , .facet_vars = sector
    , .facet_ncol = 3
    , .title = "Monthly Log Returns by Sector S&P 500"
)

timetk::plot_anomaly_diagnostics(
    .data = mean_ret_by_sector_tbl
    , .date_var = date
    , .value = returns
    , .facet_vars = sector
    , .facet_ncol = 3
)


# Build a portfolio -------------------------------------------------------

w <- c(
    rep(
        1/length(unique(sp500_co_data_tbl$sector))
        , length(unique(sp500_co_data_tbl$sector))
        )
    )

tibble(w, sectors) 

tibble(w, sectors) %>%
    summarise(total_weight = sum(w))

portfolio_returns_tq_rebalanced_monthly <- mean_ret_by_sector_tbl %>%
    tq_portfolio(assets_col = sector,
                 returns_col = returns,
                 weights = w,
                 col_rename = "returns",
                 rebalance_on = "months")

mean_ret_by_sector_tbl %>%
    ggplot(
        mapping = aes(
            x = returns
            , fill = sector
            )
        ) +
    geom_histogram(
        alpha = 0.15
        , binwidth = .01
        ) +
    geom_histogram(
        data = portfolio_returns_tq_rebalanced_monthly
        , fill = "cornflowerblue"
        , binwidth = .01
        ) +
    ggtitle("Portfolio and Asset Monthly Returns") +
    theme_update(plot.title = element_text(hjust = 0.5)) +
    theme_tq() +
    scale_color_tq()


# Risk --------------------------------------------------------------------

portfolio_sd_tidyquant_builtin_percent <-
    portfolio_returns_tq_rebalanced_monthly %>%
    tq_performance(Ra = returns,
                   Rb = NULL,
                   performance_fun = table.Stats) %>%
    select(Stdev) %>%
    mutate(tq_sd = round(Stdev, 4) * 100)

