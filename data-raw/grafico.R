## code to prepare `grafico` dataset goes here

# figura 1 ----------------------------------------------------------------

totais <- list(
  ano = 1999:2011,
  voluntario_n = c(4845, 4932, 5802, 4840, 4828, 4605, 5847, 5888, 4399, 4483, 5054, 3706, 7062),
  voluntario_p = c(49, 48, 56, 55, 46, 45, 43, 47, 48, 48, 41, 45, 41),
  oficio_n = c(376, 371, 376, 320, 346, 343, 381, 314, 268, 169, 104, 72, 134),
  oficio_p = c(92, 93, 94, 91, 93, 88, 87, 90, 89, 80, 80, 89, 86)
) |>
  tibble::as_tibble() |>
  tidyr::pivot_longer(-ano) |>
  tidyr::separate("name", c("tipo_recurso", "variavel"), sep = "_") |>
  tidyr::pivot_wider(names_from = variavel, values_from = value) |>
  dplyr::mutate(
    p = p/100,
    ic_min =  p - qnorm(.975) * sqrt(p * (1 - p) / n),
    ic_max =  p + qnorm(.975) * sqrt(p * (1 - p) / n),
    tipo_recurso = dplyr::case_match(
      tipo_recurso, "oficio" ~ "Governo", "voluntario" ~ "Contribuinte"
    )
  )

p <- totais |>
  ggplot2::ggplot(ggplot2::aes(x = ano, y = p, colour = tipo_recurso)) +
  ggplot2::geom_rect(
    xmin = 2008, xmax = 2010, ymin = -1, ymax = 2,
    colour = "transparent",
    fill = "blue", alpha = .005
  ) +
  ggplot2::geom_line(linewidth = .8) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = ic_min, ymax = ic_max),
    width = 0.2, linewidth = .8
  ) +
  ggplot2::geom_hline(yintercept = 0.5) +
  ggplot2::geom_text(
    ggplot2::aes(label = n, y = 1.05),
    data = dplyr::filter(totais, tipo_recurso == "Contribuinte"),
    family = "Times New Roman", size = 5.8
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = n, y = 1.1),
    data = dplyr::filter(totais, tipo_recurso == "Governo"),
    family = "Times New Roman", size = 5.8
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 1.1), breaks = 0:10/10,
    labels = scales::percent
  ) +
  ggplot2::scale_x_continuous(breaks = 1999:2011) +
  ggplot2::scale_colour_viridis_d(begin = .2, end = .6) +
  ggplot2::theme_bw(17) +
  ggplot2::theme(
    text = ggplot2::element_text(family = "Times"),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(colour = "gray85", linewidth = .1),
    panel.border = ggplot2::element_rect(colour = "gray85", linewidth = .1),
    axis.ticks = ggplot2::element_line(colour = "black", linewidth = .1),
    legend.position = c(.5, .2),
    legend.background = ggplot2::element_rect(fill = "#ffffffaa")
  ) +
  ggplot2::labs(
    x = "Ano",
    y = "Proporção de decisões favoráveis ao contribuinte",
    colour = "Autor do recurso"
  )

ggplot2::ggsave("data-raw/figura1.pdf", p, width = 9, height = 6)


# figura 2 ----------------------------------------------------------------

datas <- seq.Date(
  from = as.Date("1999-01-01"), to = as.Date("2011-12-31"),
  by = "quarter"
)
datas <- glue::glue("{format(datas, '%Y')} T{lubridate::quarter(datas)}")

totais <- tibble::tibble(
  label = datas,
  x = c(
    NA, 46.5, 46, 48.2,  # 1999
    48.5, 49, 48.2, 45, # 2000
    53, 59, 63, 51, # 2001
    54.5, 52, 57, 52.1, # 2002
    47.4,44, 44, 47.5, # 2003
    48, 45, 44.5, 42, # 2004
    41.5, 44, 41.5, 43, # 2005
    43.5, 45, 53.5, 45.8, # 2006
    48.5, 48, 45.5, 48, # 2007
    48, 50, 48.1, 47, # 2008
    39.1, 39.5, 42.1, 44.1, # 2009
    49, 40.8, 49.2, 42, # 2010
    43.9, 41.1, 42, 41 # 2011
  ),
  y = c(
    NA, 1100, 1090, 1095,   # 1999
    1500, 1900, 1250, 1090, # 2000
    1120, 1100, 1080, 1050, # 2001
    1550, 1600, 1880, 1300, # 2002
    1700, 1500, 2050, 1520, # 2003
    1450, 1550, 1895, 1000, # 2004
    1930, 1800, 1200, 1380, # 2005
    1330, 1500, 1550, 1080, # 2006
    2200, 2990, 2450, 1070, # 2007
    1150, 1320, 1020, 0600, # 2008
    1020, 0580, 0590, 0300, # 2009
    0330, 0400, 0310, 0100, # 2010
    0090, 0090, 0070, 0050  # 2011
  )
)

p <- totais |>
  ggplot2::ggplot() +
  ggplot2::aes(x = x/100, y = y, label = label) +
  ggplot2::geom_text(family = "Times") +
  ggplot2::geom_smooth(method = "lm", se = FALSE) +
  ggplot2::labs(
    x = "Proporção de decisões favoráveis ao contribuinte",
    y = "Número de casos novos (no trimestre seguinte)",
  ) +
  ggplot2::scale_x_continuous(labels = scales::percent) +
  ggplot2::theme_bw(17) +
  ggplot2::theme(
    text = ggplot2::element_text(family = "Times"),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(colour = "gray85", linewidth = .1),
    panel.border = ggplot2::element_rect(colour = "gray85", linewidth = .1),
    axis.ticks = ggplot2::element_line(colour = "black", linewidth = .1),
    legend.position = c(.5, .2),
    legend.background = ggplot2::element_rect(fill = "#ffffffaa")
  )

p

ggplot2::ggsave("data-raw/figura2.pdf", p, width = 9, height = 6)

# figura 3 ----------------------------------------------------------------

totais <- list(
  ano = 1999:2011,
  ltda_n = c(2971, 2968, 3155, 2720, 2420, 2400, 3205, 3251, 2541, 2641, 2567, 2036, 3632),
  ltda_p = c(49, 51, 53, 50.5, 49, 47, 46, 52, 52.2, 51, 49, 46, 45),
  nltda_n = c(1874, 1964, 2647, 2120, 2408, 2205, 2642, 2637, 1858, 1842, 2487, 1670, 3433),
  nltda_p = c(51, 42, 60, 58, 43, 43, 39.8, 39.8, 43, 44.5, 34.5, 44, 38)
) |>
  tibble::as_tibble() |>
  tidyr::pivot_longer(-ano) |>
  tidyr::separate("name", c("tipo_empresa", "variavel"), sep = "_") |>
  tidyr::pivot_wider(names_from = variavel, values_from = value) |>
  dplyr::mutate(
    p = p/100,
    tipo_empresa = dplyr::case_match(
      tipo_empresa, "ltda" ~ "Limitada", "nltda" ~ "Não Limitada"
    )
  )

p <- totais |>
  ggplot2::ggplot(ggplot2::aes(x = ano, y = p, colour = tipo_empresa)) +
  ggplot2::geom_rect(
    xmin = 2008, xmax = 2010, ymin = -1, ymax = 2,
    colour = "transparent",
    fill = "blue", alpha = .005
  ) +
  ggplot2::geom_line(linewidth = .8) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0.5) +
  ggrepel::geom_text_repel(
    ggplot2::aes(label = n),
    family = "Times New Roman", size = 4
  ) +
  ggplot2::scale_y_continuous(
    limits = c(.3, .7), breaks = 0:10/10,
    labels = scales::percent
  ) +
  ggplot2::scale_x_continuous(breaks = 1999:2011) +
  ggplot2::scale_colour_viridis_d(begin = .2, end = .6) +
  ggplot2::theme_bw(15) +
  ggplot2::theme(
    text = ggplot2::element_text(family = "Times"),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(colour = "gray85", linewidth = .1),
    panel.border = ggplot2::element_rect(colour = "gray85", linewidth = .1),
    axis.ticks = ggplot2::element_line(colour = "black", linewidth = .1),
    legend.position = c(.5, .87),
    legend.background = ggplot2::element_rect(fill = "#ffffffaa")
  ) +
  ggplot2::labs(
    x = "Ano",
    y = "Proporção de decisões favoráveis ao contribuinte",
    colour = "Autor do recurso"
  )

p

ggplot2::ggsave("data-raw/figura3.pdf", p, width = 9, height = 6)


