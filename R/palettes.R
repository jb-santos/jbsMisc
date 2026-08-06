# color palettes

pal_jbor3 <- c(
  "#0C9EBD", # dark blue
  "#DF4540", # dark red
  "#B3BBBA"  # grey
)

pal_jbor3r <- c(
  "#DF4540", # dark red
  "#0C9EBD", # dark blue
  "#B3BBBA"  # grey
)

pal_jbor4 <- c(
  "#0C9EBD", # dark blue
  "#A4E4EC", # light blue
  "#DF4540", # dark red
  "#B3BBBA"  # grey
)

pal_jbor4r <- c(
  "#DF4540", # dark red
  "#F4B2B3", # light red
  "#A4E4EC", # light blue
  "#0C9EBD", # dark blue
  "#B3BBBA"  # grey
)

pal_jbor5 <- c(
  "#0C9EBD", # dark blue
  "#A4E4EC", # light blue
  "#F4B2B3", # light red
  "#DF4540", # dark red
  "#B3BBBA"  # grey
)

pal_jbor5r <- c(
  "#B3BBBA",  # grey
  "#DF4540", # dark red
  "#F4B2B3", # light red
  "#A4E4EC", # light blue
  "#0C9EBD" # dark blue

)


pal_jbor5m <- c(
  "#0C9EBD", # dark blue
  "#A4E4EC", # light blue
  "#B3BBBA",  # grey
  "#F4B2B3", # light red
  "#DF4540" # dark red
)

pal_abvotegrp <- c(
  "#D71920",   # Other left = light red
           "#f37221",   # NDP = orange
           "#3D9B35",   # Others = green
           "#184484",   # UCP = navy blue
           "#bbbbbb",   # Und = dark grey
           "#E76BF3"    # Other right = pink
)
pal_abvote <- c(
  "#184484",   # UCP = navy blue
  "#f37221",   # NDP = orange
  "#bbbbbb",    # unspecified = light-mid grey
  "#333333"    # Others = light-mid grey
  #"#009cde",   # AB Party = light blue
  #"#e23b22",   # Liberal = orange-red
  #"#E76BF3",   # WIP/IPA

)

pal_fedvote <- c(
  "#000000",   # None = mid grey
           "#33B2CC",   # BQ = sky blue
           "#1A4782",   # CPC = dark blue
           "#3D9B35",   # Green = green
           "#D71920",   # Liberal = bright red
           "#F37021",   # NDP = orange
           "#bbbbbb",   # Other = grey
           "#442d7b"    # PPC = purple
)

pal_fedpid <- c(
  "#33B2CC",   # BQ = sky blue
           "#1A4782",   # CPC = dark blue
           "#3D9B35",   # Green = green
           "#D71920",   # Liberal = bright red
           "#F37021",   # NDP = orange
           "#bbbbbb",   # Other = grey
           "#442d7b"    # PPC = purple
)


jbspal <- list(
  pal_jbor3 = pal_jbor3,
  pal_jbor3r = pal_jbor3r,
  pal_jbor4 = pal_jbor4,
  pal_jbor4r = pal_jbor4r,
  pal_jbor5 = pal_jbor5,
  pal_jbor5r = pal_jbor5r,
  pal_jbor5m = pal_jbor5m,
  pal_abvote = pal_abvote,
  pal_abvotegrp = pal_abvotegrp,
  pal_fedvote = pal_fedvote,
  pal_fedp = pal_fedpid)


save(jbspal, file = "Data/jbspal.rda")
