# https://github.com/joelnitta/japan_ferns_spatial_phy

# Captions ----

# Follow Journal of Plant Research (JPR) style:
# - All figures are to be numbered using Arabic numerals.
# - Figures should always be cited in text in consecutive numerical order.
# - Figure parts should be denoted by lowercase letters (a, b, c, etc.).
# - If an appendix appears in your article and it contains one or more figures,
#   continue the consecutive numbering of the main text.
# - Do not number the appendix figures, "A1, A2, A3, etc."
# - Figures in online appendices (Electronic Supplementary Material) should,
#   however, be numbered separately

# - First define the "full" version, which would include a caption
# (except I never use the caption in the function, and instead replace with 'blank')
figure_full <- captioner::captioner(prefix = "Fig.")
table_full <- captioner::captioner(prefix = "Table")
s_figure_full <- captioner::captioner(prefix = "Figure S", auto_space = FALSE)
s_table_full <- captioner::captioner(prefix = "Table S", auto_space = FALSE)

# - Make a short function that prints only the object type and number, e.g., "Fig. 1"
figure <- pryr::partial(figure_full, display = "cite", caption = "blank")
table <- pryr::partial(table_full, display = "cite", caption = "blank")
s_figure <- pryr::partial(s_figure_full, display = "cite", caption = "blank")
s_table <- pryr::partial(s_table_full, display = "cite", caption = "blank")

# - Make a short function that prints only the number (e.g., "1")
figure_num <- function (name) {figure(name) %>% str_remove("Fig. ")}
table_num <- function (name) {table(name) %>% str_remove("Table ")}
s_figure_num <- function (name) {s_figure(name) %>% str_remove("Fig. ")}
s_table_num <- function (name) {s_table(name) %>% str_remove("Table ")}
