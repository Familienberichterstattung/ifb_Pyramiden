# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Bayerische Bevölkerungspyramiden: Familienstand  ----
#            2000    -   2010    -   2021
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Dieses Skript nutzt frei zugängliche Daten des bayerischen Landesamts für 
# Statistik sowie des statistischen Bundesamts um nach Familienstand 
# differenzierte Bevölkerungspyramiden für die bayerische Bevölkerung in den 
# Jahren 2000, 2010 und 2021 zu erstellen.

# Das Skript basiert zu erheblichen Teilen auf den Skripten zur Erstellung der
# Grafiken des BiB-Berichts "Demografischen Wandel neu entdecken"
# (https://github.com/z3tt/BiB-population-pyramids). Wir bedanken uns beim BiB
# und Cédric Scherer für gelebte Open Science und die hervorragende Vorlage.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Benötigte R-Pakete (installieren und) laden ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Liste der verwendeten Pakete
pkgs <-
  c(
    "tidyverse",  # Diverse Funktionen zur Aufbereitung der Daten
    "readxl",     # Excel-Dateien einlesen
    "glue",       # Strings zusammenkleben
    "here",       # relative Pfadangaben
    "ggh4x",      # zur Feinjustierung der Ticks
    "ggtext",     # Formatierung von Text im Plot
    "showtext",   # zum Laden der Schriftart Roboto
    "colorspace", # für die desaturierten Konturen der Segmente im Plot
    "patchwork",  # zum Arrangieren der Plots
    "pdftools",   # zum Speichern des Plots
    "magick"      # zum Hinzufügen des ifb-Logos
  )

  
## Installieren uninstallierter Pakete
lapply(pkgs[!(pkgs %in% installed.packages())], install.packages)

## Laden der Pakete
lapply(pkgs, library, character.only = TRUE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Einlesen und Aufbereiten der Altersverteilungen für über 84-Jährige ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Erforderlich da das Landesamt nur mis zum Alter von 84 Jahren
# nach Familienstand differenzierte Daten liefert
# Daten kommen vom Statistischen Bundesamt


# Ausprägungen für Familienstand (inklusive Extra-Katergorie "fehlend")
famstd_levels <- c("ledig", "verheiratet",
                   "geschieden", "verwitwet", "fehlend")

# Jahreskürzel für das automatisierte Einlesen
yrs <- c("00", "10", "21")

# Daten einlesen und in Form bringen
Bev_BY_YR_tidy <- 
  map(glue("BEV_BY_{yrs}_BY.xlsx"),
      ~ read_excel(here("Daten", .x), range = "A5:C97") |>
        slice((n() - 5):n()) |>
        mutate(Alter = 85:90, .before = 1) |>
        select(-2) |>
        mutate(group = factor("fehlend",levels = famstd_levels))
      )

# Separate Datensätze für Männer und Frauen

# ... Männer
Bev_BY_YR_M <- 
  map(Bev_BY_YR_tidy,
      ~ .x |>
        mutate(value = round(`männlich`/1000,1)) |>
        select(-(2:3))
      ) 

# ... Frauen
Bev_BY_YR_F <- map(Bev_BY_YR_tidy,
                   ~ .x |>
                     mutate(value = round(`weiblich`/1000,1)) |>
                     select(-(2:3))
                   )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Einlesen/Aufbereiten der nach Familienstand differenzierten Altersvert. ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Daten kommen vom Landesamt für Statistik Bayern
# Tabelle "12411-008s"

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Einlesen
Famst_BY_YR_raw <- 
  map(glue("Famst_BY_{yrs}_BY.xlsx"),
      ~read_excel(here("Daten", .x), range = "A7:P96")
      )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Extrahieren der relevanten Zeilen und Spalten
Famst_BY_YR_tidy <- 
  map(Famst_BY_YR_raw,
      ~ .x |> 
        slice(1:(n() - 1)) |> 
        mutate(Alter = row_number() - 1, .before = 1) |>
        select(-c(2:7))
  )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Erstellen eines Vektors mit neuen Variablennamen auf Basis der 
# ursprünglichen Variablennamen
newnames <- str_replace(names(Famst_BY_YR_tidy[[1]]), 
                        "[\\.]{3}([7-9]|1[0-1])", "_M")
newnames <- str_replace(newnames, 
                        "[\\.]{3}1[2-6]", "_F")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Variablen umbenennen und Strings numerische Variablen umwandeln
Famst_BY_YR_tidy <- 
  map(Famst_BY_YR_tidy,
      ~ .x |>
        set_names(newnames) |>
        mutate(across(where(is.character), ~ifelse(.x == "-", 0, .x)),
               across(where(is.character), ~as.numeric(.x)))
      )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Separate Datensätze für Männer und Frauen
  
    # - Eingrenzen auf Beobachtungen mit unaggregierten Altersdaten
    # - Extrahieren relevanter Spalten
    # - Einheit: 1 = 1000 Einwohner
    # - Daten ins lange Format bringen: 
    #     - 1 Zeile je Familienstand-Alter-Kombination
    #     - Die Zellen enthalten kumulierte Häufigkeiten 
    #       um "gestaplete" Plots zu erstellen 
    
    

# ... Männer

Famst_BY_YR_M <- 
  map(Famst_BY_YR_tidy,
      ~ .x |>
        
        filter(Alter < 85) |> 
        select(Alter, ends_with("_M")) |> 
        select(-2) |>
        pivot_longer(cols = -c("Alter"), 
                     names_to = "group", 
                     values_to = "value") |>
        mutate(value = round(value/1000,1),
               group = str_remove(group, "_M"),
               group = factor(group, levels = famstd_levels)) |> 
        arrange(Alter, group) |>
        group_by(Alter) |>  
        mutate(value = cumsum(value)) |>  
        ungroup() |>
        mutate(group = fct_rev(group))
      )


# ... Frauen

Famst_BY_YR_F <- 
  map(Famst_BY_YR_tidy,
      ~ .x |>
        filter(Alter < 85) |> 
        select(Alter, ends_with("_F")) |> 
        select(-2) |>
        pivot_longer(cols = -c("Alter"), 
                     names_to = "group", 
                     values_to = "value")  |>  
        mutate(value = round(value/1000,1),
               group = str_remove(group, "_F"),
               group = factor(group, levels = famstd_levels)) |>
        arrange(Alter, group) |>
        group_by(Alter) |>  
        mutate(value = cumsum(value)) |>  
        ungroup() |>
        mutate(group = fct_rev(group))
      )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Bevölkerungszahl für hohe Altersgruppen ergänzen ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Nach Geschlecht getrennt; in 3 Schritten
# Schritt 1: 
# Letzte jahresgenaue Bevölkerungszahl (84) der bayerischen Daten extrahieren

# Schritt 2: 
# Datensatz für den Sprung von 84 auf 85 mit "imputierten" Werten erstellen;
# es werden 9 Zwischenschritte ergänzt die den Unterschied in der Bevölkerungsgröße
# in gleich große Intervalle unterteilen; das Ganze ist erforderlich da 
# der Übergang zwischen den beiden Alterstufen nicht korrekt dargestellt wird

# Schritt 3:
# Bayerische Daten (bis 84) und DESTATIS Daten (ab 85; inklusive der imputierten
# Daten) zusammenbringen 


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ... Männer

# Schritt 1
lastobs_M <- 
  map(Famst_BY_YR_M,
      ~ .x |>
        filter(Alter == 84) |> 
        pull(value) |> 
        max()
      )

# Schritt 2
fillgap_M <- 
  map2(Bev_BY_YR_M, lastobs_M,
       ~.x |>
         slice(rep(1, 9)) |> 
         mutate(Alter = Alter - 1 + row_number()/10,
                value = .y + 
                  ((value - .y)*(row_number()/10)))
       )

# Schritt 3
Famst_BY_YR_M <- 
  map(1:3,
      ~Famst_BY_YR_M[[.x]] |>
        bind_rows(fillgap_M[[.x]], Bev_BY_YR_M[[.x]])
      )


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ... Frauen

# Schritt 1
lastobs_F <-
  map(Famst_BY_YR_F,
      ~ .x |>
        filter(Alter == 84) |> 
        pull(value) |> 
        max()
      )

# Schritt 2
fillgap_F <- 
  map2(Bev_BY_YR_F, lastobs_F,
       ~.x |>
         slice(rep(1, 9)) |> 
         mutate(Alter = Alter - 1 + row_number()/10,
                value = .y + 
                  ((value - .y)*(row_number()/10)))
       )

# Schritt 3
Famst_BY_YR_F <- 
  map(1:3,
      ~Famst_BY_YR_F[[.x]] |>
        bind_rows(fillgap_F[[.x]], Bev_BY_YR_F[[.x]])
      )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Vorbereitungen für den Plot ----
# Definition einiger Parameter 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Laden der verwendeten Schriftart
font_add_google("Roboto", "Roboto")
showtext_auto()

# Farben für Familienstand (inklusive eines hellen grau für "fehlende Werte")
colors <- c("#E1E1E1", "#A6A6A6", "#F7AF7A", "#F27279", "#72B2C3") 

# Parameter für die Plots (vornehmlich für den mittleren Achsenlabel-Plot)
limit_age <- 90
steps_age <- 5
title_x_centre_pos = .65

ybrks <- seq(0, limit_age, by = steps_age)
ylbls <- ybrks
ylbls[length(ylbls)] <- paste0(ylbls[length(ylbls)],"\nJahre") 

vjust <- c(rep(.5, limit_age / steps_age), .8)

limit_count <- 130

# Achsentitel
title_x_m <- sprintf("\u2190  **Männer** in Tausend")
title_x_f <- sprintf("**Frauen** in Tausend  \u2192")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plots: Pyramidenhälften ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ... Männer
p_Famst_BY_YR_M <-
  map(Famst_BY_YR_M,
      ~.x |> 
        ggplot(aes(x = Alter, ymin = 0, ymax = value)) +
        coord_flip(clip = "off") +
        geom_ribbon(aes(fill = group,
                        color = after_scale(desaturate(lighten(fill, .25), .15))), 
                    position = "identity") +
        scale_fill_manual(values = colors, name = NULL) +
        scale_y_continuous(expand = c(0, 0),
                           limits = c(130, 0),
                           breaks = seq(0, 120, by = 20),
                           labels = seq(0, 120, by = 20),
                           trans = "reverse") +
        scale_x_continuous(
          position = "top",
          expand = c(0, 0),
          breaks = seq(0, limit_age, by = 5),
          minor_breaks = seq(0, limit_age, by = 1),
          limits = c(0, limit_age * 1.0015),
          labels = function(breaks) {rep_along(breaks, "")}
        )  +
        labs(x = NULL, y = NULL) +
        guides(y = "axis_minor") +
        theme_classic() +
        theme(legend.position = "none",
              axis.title.x = element_markdown(hjust = 1),
              axis.text.x = element_text(family = "Roboto",
                                         size = 11),
              axis.line.y = element_blank(),
              axis.ticks.length.y = unit(0.25, "cm"),
              axis.ticks.length.x = unit(0.15, "cm"),
              ggh4x.axis.ticks.length.minor = rel(0.5))
  )


# ... Frauen
p_Famst_BY_YR_F <-
  map(Famst_BY_YR_F,
      ~.x |>
        ggplot(aes(x = Alter, ymin = 0, ymax = value)) +
        coord_flip(clip = "off") +
        geom_ribbon(aes(fill = group,
                        color = after_scale(desaturate(lighten(fill, .25), .15))), 
                    position = "identity") +
        scale_fill_manual(values = colors, name = NULL) +
        scale_y_continuous(expand = c(0, 0),
                           limits = c(0, 130),
                           breaks = seq(0, 120, by = 20),
                           labels = seq(0, 120, by = 20)) +
        scale_x_continuous(
          expand = c(0, 0),
          breaks = seq(0, limit_age, by = 5),
          minor_breaks = seq(0, limit_age, by = 1),
          limits = c(0, limit_age * 1.0015),
          labels = function(breaks) {rep_along(breaks, "")}
        ) +
        labs(x = NULL, y = NULL) +
        guides(y = "axis_minor") +
        theme_classic() +
        theme(legend.position = "none",
              axis.title.x = element_markdown(hjust = 0),
              axis.text.x = element_text(family = "Roboto",
                                         size = 11),
              axis.line.y = element_blank(),
              axis.ticks.length.y = unit(0.25, "cm"),
              ggh4x.axis.ticks.length.minor = rel(0.5))
      )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot: Achsenbeschriftung ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

p_axis <- Famst_BY_YR_F[[1]] |> 
  ggplot(aes(y = Alter)) +
  geom_blank() +
  
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0, limit_age, by = steps_age),
    labels = ylbls,
    limits = c(0, limit_age)) +
  labs(x = NULL, y = NULL, title = NULL) +
  theme_void() +
  theme(
    axis.text.y = element_text(
      family = "Roboto", size = 11, color = "grey25", lineheight = .9,
      margin = margin(0, 2.5, 0, 2.5), hjust = .5, vjust = vjust
    ),
    plot.title.position = "plot"
  )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot-Labelei: Achsentitel und "Legende" ---- 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Für den ersten Teilplot (Jahr 2000) der späteren Zusammenstellung
# ergänzen wir Achsentitel und Labels für den Familienstand in der Abbildung


# Männer 2000: Achsentitel ergänzen
p_Famst_BY_YR_M[[1]]  <-
  p_Famst_BY_YR_M[[1]] +
  labs(y = title_x_m)


# Koordinaten für die Wertelabels
fmstd_labels <- p_Famst_BY_YR_F[[1]]$data |>
  slice(1:4) |> 
  transmute(group = group,
            x = c(7,56,46,83),
            y = c(85,100,108,70))

# Frauen 2000: Labels und Linien und Achsentitel ergänzen
p_Famst_BY_YR_F[[1]]  <-
  p_Famst_BY_YR_F[[1]] +
  labs(y = title_x_f) +
  geom_text(data = fmstd_labels, aes(x = x, y = y, 
                                     label = group, 
                                     colour = group,
                                     ymax =130),
            fontface = "bold", family = "Roboto", size = 4.9
            ) +
  scale_color_manual(values = colors[2:5], name = NULL) +
  geom_curve(
    aes(y = 45, x = 72, yend = 70, xend = 82),
    size = 0.8, color = colors[2], curvature = 0.2
  ) +
  geom_curve(
    aes(y = 45, x = 45, yend = 100, xend = 55),
    size = 0.8, color = colors[4], curvature = 0.3
  ) +
  geom_curve(
    aes(y = 90, x = 40, yend = 108, xend = 45),
    size = 0.8, color = colors[3], curvature = 0.2
  ) +
  geom_curve(
    aes(y = 60, x = 12, yend = 85, xend = 8),
    size = 0.8, color = colors[5], curvature = -0.3
  ) +
  theme(legend.position = "none")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Finaler Plot für die Jahre 2000 - 2010 - 2021 ---- 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Plots mit patchwork zusammenstellen
pyramiden <- 
  wrap_plots(p_Famst_BY_YR_M[[1]] +
               annotate("text", x=85, y = 100, label = "2000", 
                        size = 18, family = "Roboto"),
             p_axis, 
             p_Famst_BY_YR_F[[1]],
             p_Famst_BY_YR_M[[2]] + labs(y = NULL) +
               annotate("text", x=85, y = 100, label = "2010", 
                        size = 18, family = "Roboto"), 
             p_axis, 
             p_Famst_BY_YR_F[[2]] + labs(y = NULL),
             p_Famst_BY_YR_M[[3]] + labs(y = NULL) +
               annotate("text", x=85, y = 100, label = "2021", 
                        size = 18, family = "Roboto"), 
             p_axis, 
             p_Famst_BY_YR_F[[3]] + labs(y = NULL)
             ) +
  plot_layout(nrow = 1, 
              widths = rep(c(limit_count, 0, limit_count), 3)
              ) +
  plot_annotation(caption = 
                    glue("**Quelle**: Raab/ifb 2022; 
                         **Daten**: Bayerisches Landesamt für Statistik & DESTATIS")
                  ) &
  theme(plot.caption = element_markdown('Roboto', size = 11))


# Plots speichern
ggsave(plot = pyramiden,
       here("Abbildungen", "Pyramiden_00-10-21.pdf"),
       width = 20, height = 11, device = cairo_pdf)

pdf_convert(here("Abbildungen", "Pyramiden_00-10-21.pdf"),
            format = "png", dpi = 300, pages = 1,
            here("Abbildungen", "Pyramiden_00-10-21.png"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~
# ifb-Logo ergänzen ----
# ~~~~~~~~~~~~~~~~~~~~~~

# Logo einlesen und Größe anpassen
img <- image_read(here("Logos", "ifb_ZahlenFakten.png"))
img_inset <- image_scale(img, "75%x") 

# Pyramide ohne Logo einlesen
img <- image_read(here("Abbildungen", "Pyramiden_00-10-21.png"))

# Logo als zusätzliche Ebene einfügen
img_with_inset <- img |>  
  image_composite(
    img_inset,
    operator = "Atop",
    gravity = "SouthWest",
    offset = "+20+20"
    )


# Pyramide mit Logo speichern
# ... als png
image_write(img_with_inset, 
            here("Abbildungen", "Pyramiden_00-10-21.png"))

# ... als pdf
img <- image_read(here("Abbildungen", "Pyramiden_00-10-21.png"))

image_convert(img, "pdf") |> 
  image_write(here("Abbildungen", "Pyramiden_00-10-21.pdf"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~