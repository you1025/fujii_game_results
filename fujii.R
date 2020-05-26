library(tidyverse)
library(rstan)

# 事前分布
a <- 2
b <- 2
tibble(theta = seq(0, 1, 0.01), density = dbeta(theta, a, b)) %>%
  ggplot(aes(theta, density)) +
    geom_line()

df.results <- readr::read_csv(
  file = "https://raw.githubusercontent.com/you1025/fujii_game_results/master/data/fujii.csv",
  col_types = cols(
    date = col_date(format = ""),
    first_or_second = col_character(),
    result = col_character()
  )
) %>%

  dplyr::mutate(
    # 対戦番号の付与
    game = dplyr::row_number(),

    # 先攻フラグ
    flg_first = (first_or_second == "先"),

    # 勝利フラグ
    flg_win = (result == "○"),

    # 通算勝利数
    cumsum_win = cumsum(flg_win),

    # 推定値
    MLE = cumsum_win / game,
    EAP = (a + cumsum_win) / (a + b + game),
    MAP = (a + cumsum_win - 1) / (a + b + game - 2)
  ) %>%

  # 対戦前のレコードを追加
  dplyr::bind_rows(tibble(game = 0, cumsum_win = 0, EAP = a / (a + b))) %>%
  dplyr::arrange(game) %>%

  # Beta 分布の 2.5%, 97.5% タイルを算出
  dplyr::mutate(
    q.025 = qbeta(p = 0.025, shape1 = a + cumsum_win, shape2 = b + game - cumsum_win),
    q.975 = qbeta(p = 0.975, shape1 = a + cumsum_win, shape2 = b + game - cumsum_win)
  ) %>%

  dplyr::select(
    game,
    flg_first,
    flg_win,
    cumsum_win,
    MLE,
    EAP,
    MAP,
    q.025,
    q.975
  )

# 先行/後攻 ごとの勝率
df.results %>%
  dplyr::filter(!is.na(flg_first)) %>%
  dplyr::group_by(flg_first) %>%
  dplyr::summarise(avg_win = mean(flg_win))

# 勝率の推移
df.results %>%
  ggplot(aes(game)) +
    geom_line(aes(y = MLE), linetype = 2, alpha = 1/2) +
    geom_line(aes(y = EAP), linetype = 1, colour = "tomato") +
    geom_line(aes(y = MAP), linetype = 1, colour = "blue", alpha = 1/3) +
    geom_ribbon(aes(ymin = q.025, ymax = q.975), alpha = 1/7) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    labs(
      x = "Games",
      y = NULL
    )


# 対戦ごとの事後分布の一覧
df.results %>%

  dplyr::mutate(
    # 対戦が進む度に事後分布を推定する
    data = map2(game, cumsum_win, function(game, cumsum_win) {
      tibble(
        x = seq(0, 1, 0.001),
        d = dbeta(x, shape1 = a + cumsum_win, b + game - cumsum_win)
      )
    })
  ) %>%
  tidyr::unnest(data) %>%

  ggplot(aes(x, d)) +
    geom_line(aes(group = game, colour = game), alpha = 1/3) +
    scale_colour_gradient(low = "blue", high = "tomato") +
    labs(
      x = "勝率",
      y = NULL
    ) +
    scale_x_continuous(labels = scales::percent) +
    theme_gray(base_family = "Osaka")



# Stan --------------------------------------------------------------------

stan_code <- "
data {
  int N;
  int Y[N];
  int X[N];
}

parameters {
  real<lower=0, upper=1> q0;
  real<lower=0, upper=1> q1;
}

model {
  q0 ~ beta(4, 2);
  q1 ~ beta(8, 2);
  for(n in 1:N) {
    Y[n] ~ bernoulli(X[n] * q1 + (1-X[n]) * q0);
  }
}
"

fit <- rstan::stan(
  model_code = stan_code,
  data = list(
    N = nrow(df.results[-1,]),
    Y = as.integer(df.results[-1,]$flg_win),
    X = as.integer(df.results[-1,]$flg_first)
  ),
  iter = 6000,
  chain = 4,
  warmup = 1000,
  seed = 1025
)

q0 <- rstan::extract(fit, "q0")[[1]]
q1 <- rstan::extract(fit, "q1")[[1]]
tibble(
  q0 = q0,
  q1 = q1
) %>%
  tidyr::pivot_longer(cols = c(q0, q1), names_to = "parameter", values_to = "value") %>%
  ggplot(aes(value)) +
    geom_density(aes(fill = parameter), alpha = 1/3) +
    scale_x_continuous(limit = c(0, 1)) +
    facet_grid(parameter ~ .)

rstan::stan_trace(fit)
rstan::stan_hist(fit)
rstan::stan_dens(fit, separate_chains = T)
rstan::stan_plot(fit)
