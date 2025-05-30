# https://github.com/joelnitta/japan_ferns_spatial_phy

# Captions ----

# - First define the "full" version, which would include a caption
# (except I never use the caption in the function, and instead replace with 'blank')

fig_full <- captioner::captioner(prefix = "Fig.")
table_full <- captioner::captioner(prefix = "Table")
eq_full <- captioner::captioner(prefix = "Eq.")
s_fig_full <- captioner::captioner(prefix = "Fig. S", auto_space = FALSE)
s_table_full <- captioner::captioner(prefix = "Table S", auto_space = FALSE)
s_eq_full <- captioner::captioner(prefix = "Eq. S", auto_space = FALSE)
appendix_full <- captioner::captioner(prefix = "Supplementary Section ", auto_space = FALSE)

# - Make a short function that prints only the object type and number, e.g., "Figure 1"
fig <- purrr::partial(fig_full, display = "cite", caption = "blank")
table <- purrr::partial(table_full, display = "cite", caption = "blank")
eq <- purrr::partial(eq_full, display = "cite", caption = "blank")
s_fig <- purrr::partial(s_fig_full, display = "cite", caption = "blank")
s_table <- purrr::partial(s_table_full, display = "cite", caption = "blank")
s_eq <- purrr::partial(s_eq_full, display = "cite", caption = "blank")
appendix <- purrr::partial(appendix_full, display = "cite", caption = "blank")

# - Make a short function that prints only the number (e.g., "1")
# |> doen't work
fig_num <- function (name) {fig(name) %>% str_remove("Fig. ")}
table_num <- function (name) {table(name) %>% str_remove("Table ")}
eq_num <- function (name) {eq(name) %>% str_remove("Eq. ")}
s_fig_num <- function (name) {s_fig(name) %>% str_remove("Fig. ")}
s_table_num <- function (name) {s_table(name) %>% str_remove("Table ")}
s_eq_num <- function (name) {s_eq(name) %>% str_remove("Eq. ")}
appendix_num <- function (name) {appendix(name) %>% str_remove("Supplementary Section ")}
