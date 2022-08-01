# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Alters- und geschlechtsspezifische Familienstandsgliederung ----
#       2000    -   2010    -   2021              
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Dieses Skript nutzt frei zugängliche Daten des bayerischen Landesamts für 
# Statistik nach alters-/geschlechtsspezifische Familienstandsverteilungen für  
# die bayerische Bevölkerung in den Jahren 2000, 2010 und 2021 zu erstellen


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
    "pdftools",   # zum Speichern des Plots
    "magick"      # zum Hinzufügen des ifb-Logos
  )


## Installieren uninstallierter Pakete
lapply(pkgs[!(pkgs %in% installed.packages())], install.packages)

## Laden der Pakete
lapply(pkgs, library, character.only = TRUE)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Einlesen/Aufbereiten der nach Familienstand differenzierten Altersvert. ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Daten kommen vom Landesamt für Statistik Bayern
# Tabelle "12411-008s"

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Wertelabels für Familienstand
famstd_levels <- c("ledig", "verheiratet",
                   "geschieden", "verwitwet")

# Jahreskürzel für das automatisierte Einlesen
yrs <- c("00", "10", "21")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Einlesen der Excel-Datein
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
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Daten für Plots: Komplette Verteilung und Verteilung der Modalwerte ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Datensatz mit alters-, jahres- und geschlechtsspezifischen 
# relativen Häufigkeiten für jeden Familienstand

shares_fmstd <-
  map2(Famst_BY_YR_tidy, c(2000,2010,2021),
       ~.x |>
         filter(Alter < 85) |> 
         pivot_longer(cols = -c("Alter"),
                      names_pattern = "(.*)_(M|F)$", 
                      names_to = c(".value","sex")) |> 
         mutate(sex = ifelse(sex == "M", "Männer", "Frauen"),
                sex = factor(sex, levels = c("Männer", "Frauen"))) |> 
         mutate(across(ledig:geschieden, ~ .x/Insgesamt)) |> 
         select(-Insgesamt) |> 
         pivot_longer(cols = -c("Alter", "sex"),
                      names_to = "Familienstand",
                      values_to = "share") |> 
         mutate(Familienstand = factor(Familienstand,
                                       levels = famstd_levels)) |> 
         mutate(Jahr = .y , .before = 1)
  ) |> 
  bind_rows()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Datensatz mit alters-, jahres- und geschlechtsspezifischen 
# relativen Häufigkeiten der Modalkategorie

modal_fmstd <- shares_fmstd |> 
  group_by(Jahr, Alter, sex) |> 
  filter(share == max(share)) |> 
  ungroup()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Hilfsdatensätze für die Plots:

# Position der Jahreslabel
yrs_label <- 
  tibble(Jahr = c(2000,2010,2021),
         sex = rep(factor("Männer",levels = c("Männer", "Frauen")), 3),
         Alter = 5, 
         share = .8, 
         Familienstand = NA
  )

# Position der Trennlinien beim Wechsel der Modalkategorie
vline_fmstd <-
  modal_fmstd |> 
  group_by(Jahr, sex) |> 
  filter(Familienstand != lead(Familienstand)) |> 
  transmute(Alter = Alter +.5)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot: Vollständige Verteilung ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Farbvektor
colors <- c("#767676", "#94AF53", "#E95C80", "#56B3EF")
names(colors) <- rev(famstd_levels)

# Laden der verwendeten Schriftart
font_add_google("Roboto", "Roboto")
showtext_auto()


# Plot erstellen
p_shares_fmstd <- shares_fmstd |>
  ggplot(aes(x = Alter, fill = Familienstand)) +
  geom_col(aes(y= share), width = 1) +
  geom_text(data = yrs_label,
            aes(x = Alter, y = share, label = Jahr),
            fontface = "bold", family = "Roboto", size = 6) +
  scale_x_continuous(breaks = c(seq(0,84,5),84),
                     minor_breaks = seq(0, 84, 1),
                     expand = expansion(add = c(.5, .5))) + # 
  scale_y_continuous(breaks = seq(0,1,.5),
                     labels = c("0%", "50%", "100%"),
                     minor_breaks = seq(0, 1, .1)) +
  guides(y = "axis_minor", x = "axis_minor") +
  labs(caption = glue("**Quelle**: Raab/ifb 2022;
                   **Daten**: Bayerisches Landesamt für Statistik"),
       y = NULL) +
  facet_grid(vars(fct_rev(factor(Jahr))), vars(sex)) +
  scale_fill_manual(values = rev(colors)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        plot.caption = element_markdown('Roboto', size = 9),
        strip.text.y = element_blank(),
        strip.text.x = element_text(size=18),
        axis.title.x = element_text(size = 14,
                                   margin = margin(5, 0, 2.5, 0)),
        panel.spacing = unit(1, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = .3),
        axis.ticks.length = unit(0.15, "cm"),
        axis.line.x = element_line(),
        ggh4x.axis.ticks.length.minor = rel(0.5))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Plots speichern + ifb Logo ergänzen

# Speichern ohne Logo
ggsave(plot = p_shares_fmstd,
       here("Abbildungen", "Anteilswerte_00-10-21.pdf"),
       width = 15, height = 7, device = cairo_pdf)

# pdf in png konvertieren
pdf_convert(here("Abbildungen", "Anteilswerte_00-10-21.pdf"),
            format = "png", dpi = 301, pages = 1,
            here("Abbildungen", "Anteilswerte_00-10-21.png"))


# ifb-Logo ergänzen 
# Logo einlesen und Größe anpassen
img <- image_read(here("Logos", "ifb_ZahlenFakten.png"))
img_inset <- image_scale(img, "75%x") 

# Bild ohne Logo einlesen
img <- image_read(here("Abbildungen", "Anteilswerte_00-10-21.png"))

# Logo als zusätzliche Ebene einfügen
img_with_inset <- img |>  
  image_composite(
    img_inset,
    operator = "Atop",
    gravity = "SouthWest",
    offset = "+20+20"
  )

# Bild mit Logo speichern
# ... als png
image_write(img_with_inset, 
            here("Abbildungen", "Anteilswerte_00-10-21.png"))

# ... als pdf
img <- image_read(here("Abbildungen", "Anteilswerte_00-10-21.png"))

image_convert(img, "pdf") |> 
  image_write(here("Abbildungen", "Anteilswerte_00-10-21.pdf"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot: Verteilung der Modalwerte ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Reduzierter Farbvektor (nur drei Modalkategorien)
colors <- c("#767676","#E95C80", "#56B3EF")


# Plot erstellen
p_modal_fmstd <- modal_fmstd |>  
  ggplot(aes(x = Alter, fill = Familienstand)) +
  geom_col(aes(y= share), width = 1) +
  geom_text(data = yrs_label,
            aes(x = Alter, y = share, label = Jahr),
            fontface = "bold", family = "Roboto", size = 6) +
  geom_vline(data = vline_fmstd, 
             aes(xintercept  = Alter)) +
  scale_x_continuous(breaks = c(seq(0,84,5),84),
                     minor_breaks = seq(0, 84, 1),
                     expand = expansion(add = c(.5, .5))) + # 
  scale_y_continuous(breaks = seq(0,1,.5),
                     labels = c("0%", "50%", "100%"),
                     minor_breaks = seq(0, 1, .1)) +
  guides(y = "axis_minor", x = "axis_minor") +
  labs(caption = glue("**Quelle**: Raab/ifb 2022;
                   **Daten**: Bayerisches Landesamt für Statistik"),
       y = NULL) +
  facet_grid(vars(fct_rev(factor(Jahr))), vars(sex)) +
  scale_fill_manual(values = rev(colors)) +
  theme_minimal(base_family = "Roboto") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        plot.caption = element_markdown('Roboto', size = 9),
        strip.text.y = element_blank(),
        strip.text.x = element_text(size=18),
        axis.title.x = element_text(size = 14,
                                    margin = margin(5, 0, 2.5, 0)),
        panel.spacing = unit(1, "lines"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = .3),
        axis.ticks.length = unit(0.15, "cm"),
        axis.line.x = element_line(),
        ggh4x.axis.ticks.length.minor = rel(0.5))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Plots speichern + ifb Logo ergänzen

# Speichern ohne Logo
ggsave(plot = p_modal_fmstd,
       here("Abbildungen", "Modalwerte_00-10-21.pdf"),
       width = 15, height = 7, device = cairo_pdf)

# pdf in png konvertieren
pdf_convert(here("Abbildungen", "Modalwerte_00-10-21.pdf"),
            format = "png", dpi = 301, pages = 1,
            here("Abbildungen", "Modalwerte_00-10-21.png"))


# ifb-Logo ergänzen 
# Logo einlesen und Größe anpassen
img <- image_read(here("Logos", "ifb_ZahlenFakten.png"))
img_inset <- image_scale(img, "75%x") 

# Bild ohne Logo einlesen
img <- image_read(here("Abbildungen", "Modalwerte_00-10-21.png"))

# Logo als zusätzliche Ebene einfügen
img_with_inset <- img |>  
  image_composite(
    img_inset,
    operator = "Atop",
    gravity = "SouthWest",
    offset = "+20+20"
  )

# Bild mit Logo speichern
# ... als png
image_write(img_with_inset, 
            here("Abbildungen", "Modalwerte_00-10-21.png"))

# ... als pdf
img <- image_read(here("Abbildungen", "Modalwerte_00-10-21.png"))

image_convert(img, "pdf") |> 
  image_write(here("Abbildungen", "Modalwerte_00-10-21.pdf"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Durchschnittliche relative Häufigkeit des Familienstands "verheiratet" ----
# für Männer und Frauen in den Dreißigern in den Jahren 2000 und 2021
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

shares_fmstd |> 
  bind_rows() |> 
  filter(Familienstand == "verheiratet" & 
           Jahr != 2010 & 
           Alter %in% c(30:39)) |> 
  group_by(sex, Jahr) |> 
  summarise(mean = mean(share)) 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



shares_fmstd |> 
  bind_rows() |> 
  filter(Familienstand == "verheiratet" & 
           #Jahr != 2010 & 
           Alter >= 80) |> 
  group_by(sex, Jahr) |> 
  summarise(min = min(share)) 
