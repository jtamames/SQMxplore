  # ===========================================================================
  # MAG MAP TAB — metabolic completeness diagram with interactive overlays
  # ===========================================================================

  # ReactiveVals for the per-pathway pathview view inside MAG Map
  magmap_pw_status  <- reactiveVal("idle")   # idle | generating | ready | error
  magmap_pw_pid     <- reactiveVal(NULL)     # pathway ID being shown
  magmap_pw_name    <- reactiveVal(NULL)     # pathway display name
  magmap_pw_img     <- reactiveVal(NULL)     # path to rendered PNG
  magmap_pw_nodes   <- reactiveVal(NULL)     # node df for hover/overlay
  magmap_view_mode  <- reactiveVal("map")    # "map" | "pathway"

  # ---- Helper: extract KOs for a bin --------------------------------------
  get_bin_kos <- function(proj, bin_name) {
    # Extract KO identifiers for a given bin.
    #
    # SQM stores KOs in proj$orfs$table[["KEGG ID"]] (one KO per ORF row).
    # Values may carry a trailing asterisk (e.g. "K21573*") which must be
    # stripped before validation.  Bins are not in the ORF table directly;
    # the link is:  ORF -> "Contig ID"  and  contig -> "Bin ID" in contigs$table.

    orf_tbl <- tryCatch(proj$orfs$table,     error = function(e) NULL)
    ctg_tbl <- tryCatch(proj$contigs$table,  error = function(e) NULL)
    if (is.null(orf_tbl) || is.null(ctg_tbl)) return(character(0))

    # -- 1. Find which contigs belong to this bin ------------------------------
    # "Bin ID" column in contigs$table (exact name from SQM)
    bin_col <- grep("^Bin ID$", colnames(ctg_tbl), value = TRUE)[1]
    if (is.na(bin_col)) bin_col <- grep("\\bbin\\b", colnames(ctg_tbl),
                                        ignore.case = TRUE, value = TRUE)[1]
    if (is.na(bin_col) || is.null(bin_col)) return(character(0))

    bin_ids  <- as.character(ctg_tbl[[bin_col]])
    mask_ctg <- bin_ids == bin_name
    # also try stripping common suffixes (.fa.contigs, .fa.sub.contigs)
    if (!any(mask_ctg, na.rm = TRUE)) {
      stripped <- sub("\\.fa(\\.sub)?\\.contigs$", "", bin_name)
      mask_ctg <- bin_ids == stripped
    }
    bin_contigs <- rownames(ctg_tbl)[mask_ctg & !is.na(mask_ctg)]
    if (length(bin_contigs) == 0) return(character(0))

    # -- 2. Find ORFs on those contigs -----------------------------------------
    ctg_col <- grep("^Contig ID$", colnames(orf_tbl), value = TRUE)[1]
    if (is.na(ctg_col)) ctg_col <- grep("\\bcontig\\b", colnames(orf_tbl),
                                         ignore.case = TRUE, value = TRUE)[1]
    if (is.na(ctg_col) || is.null(ctg_col)) return(character(0))

    orf_rows <- orf_tbl[as.character(orf_tbl[[ctg_col]]) %in% bin_contigs, ,
                        drop = FALSE]
    if (nrow(orf_rows) == 0) return(character(0))

    # -- 3. Extract KO identifiers ---------------------------------------------
    # Column is "KEGG ID"; values may be "K00001", "K00001*", NA, "-", etc.
    ko_col <- grep("^KEGG ID$", colnames(orf_tbl), value = TRUE)[1]
    if (is.na(ko_col)) ko_col <- grep("^KEGG$", colnames(orf_tbl),
                                       ignore.case = TRUE, value = TRUE)[1]
    if (is.na(ko_col) || is.null(ko_col)) return(character(0))

    kos_raw <- as.character(orf_rows[[ko_col]])
    kos <- unlist(strsplit(kos_raw, "[,;[:space:]]+"))
    kos <- sub("\\*+$", "", kos)        # strip trailing asterisk(s)
    kos <- unique(kos[grepl("^K\\d{5}$", kos)])
    kos
  }

  # ---- Category definitions: explicit curated KO lists + rect coords --------
  # rect = c(x0, y0, x1, y1) as fractions [0,1] of image (1536x1024 px).
  # kos  = explicit curated list of KEGG KOs for that functional category.
  # Completeness = (KOs present in MAG) / (total KOs in list) * 100.
  CORE_KOS <- list(
    "Glycolysis" = c("K00016", "K00131", "K00134", "K00149", "K00150", "K00844", "K00845", "K00850", "K00873", "K00918", "K00927", "K01596", "K01610", "K01623", "K01624", "K01689", "K01803", "K01810", "K01834", "K06859", "K11645", "K12406", "K12407", "K13810", "K15633", "K15634", "K15635", "K15916", "K16305", "K16306", "K16370", "K21071", "K25026"),
    "Pentose Phosphate Pathway" = c("K00033", "K00036", "K00615", "K00616", "K01057", "K01783", "K01807", "K01808", "K07404"),
    "Entner-Doudoroff Pathway" = c("K00033", "K00036", "K01057", "K01625", "K01690"),
    "TCA Cycle" = c("K00024", "K00025", "K00026", "K00030", "K00031", "K00161", "K00162", "K00163", "K00164", "K00239", "K00240", "K00241", "K00242", "K00382", "K00627", "K00658", "K01595", "K01637", "K01638", "K01647", "K01648", "K01676", "K01677", "K01678", "K01681", "K01682", "K01902", "K01903", "K01958", "K01959", "K01960"),
    "CO2 Fixation" = c("K00169", "K00170", "K00171", "K00172", "K00174", "K00175", "K00176", "K00177", "K00194", "K00197", "K00297", "K00855", "K00925", "K01491", "K01601", "K01602", "K01647", "K01938", "K14138", "K14139", "K14140", "K14141", "K15022", "K15038", "K15039", "K18556"),
    "Fermentation" = c("K00001", "K00004", "K00016", "K00114", "K00121", "K00169", "K00170", "K00171", "K00172", "K00625", "K00634", "K00925", "K00929", "K01034", "K01035", "K01067", "K01512", "K01568", "K01613", "K01659", "K03777", "K04072"),
    "Nitrogen Fixation" = c("K00531", "K02584", "K02585", "K02586", "K02587", "K02588", "K02589", "K02590", "K02591", "K02592", "K02593", "K02594", "K02595", "K02596", "K02597", "K02598", "K02599", "K02600", "K02601", "K22896", "K22897", "K22898", "K22899"),
    "Assimilatory N" = c("K00261", "K00262", "K00263", "K00264", "K00265", "K00266", "K00362", "K00363", "K00366", "K00367", "K01914", "K01915", "K02575", "K02576", "K02577", "K02578", "K10534"),
    "Denitrification" = c("K00368", "K00370", "K00371", "K00374", "K00376", "K02305", "K02567", "K02568", "K04561", "K15864"),
    "Sulfur Cycle" = c("K00380", "K00381", "K00385", "K00390", "K00394", "K00395", "K00956", "K00957", "K00958", "K01738", "K11180", "K11181", "K16950", "K16951", "K17224", "K17225", "K17226", "K17227", "K17228", "K17229", "K17725"),
    "Nitrification" = c("K00370", "K00371", "K10535", "K10944", "K10945", "K10946", "K20932", "K20935"),
    "Methane Metabolism" = c(
      # Methanogenesis (CO2 → CH4 hydrogenotrophic / acetoclastic / methylotrophic)
      "K00200", "K00201", "K00202", "K00203", "K00205",        # Fwd/Fmd formyl-MF dehydrogenase
      "K00672",                                                  # Ftr formylmethanofuran-H4MPT formyltransferase
      "K01499",                                                  # Mch methenyl-H4MPT cyclohydrolase
      "K00319", "K13942",                                        # Mtd / Hmd F420-dependent methylene-H4MPT dehydrogenase
      "K00320",                                                  # Mer methylene-H4MPT reductase
      "K00577", "K00578", "K00579", "K00580", "K00581",          # MtrA-E
      "K00582", "K00583", "K00584",                              # MtrF-H methyl-H4MPT:CoM methyltransferase
      "K00399", "K00401", "K00402",                              # McrA/B/G methyl-CoM reductase (diagnostic)
      "K03388", "K03389", "K03390",                              # HdrA/B/C heterodisulfide reductase
      "K14080", "K14081", "K14082", "K14083", "K14084",          # MtaA/MtaB/MtaC / MtbA / MtsA methanol-CoM
      # Methanotrophy (CH4 → CH3OH → HCHO → ...)
      "K10944", "K10945", "K10946",                              # pMMO (pmoA/B/C)
      "K16157", "K16158", "K16159", "K16160", "K16161", "K16162",# sMMO (mmoX/Y/B/Z/C/D)
      "K17066",                                                  # MxaF/XoxF methanol dehydrogenase (PQQ)
      "K14028", "K14029",                                        # MDH2 (NAD-dependent methanol DH)
      "K00148",                                                  # Fae formaldehyde activating enzyme
      "K08685"                                                   # MtdB methylene-H4MPT dehydrogenase (NADP)
    ),
    "Amino Acids" = c("K00003", "K00013", "K00053", "K00133", "K00145", "K00261", "K00262", "K00265", "K00266", "K00548", "K00549", "K00600", "K00601", "K00604", "K00605", "K00609", "K00618", "K00620", "K00765", "K00766", "K00811", "K00812", "K00813", "K00817", "K00820", "K00826", "K00831", "K00872", "K00928", "K00930", "K01079", "K01438", "K01501", "K01513", "K01652", "K01653", "K01687", "K01695", "K01696", "K01697", "K01714", "K01733", "K01738", "K01739", "K01755", "K01760", "K01778", "K01814", "K01817", "K01823", "K01915", "K01940", "K02501", "K02502", "K04518", "K14170", "K14172"),
    "Nucleotides" = c("K00088", "K00226", "K00525", "K00526", "K00549", "K00602", "K00609", "K00611", "K00764", "K00940", "K00942", "K01465", "K01495", "K01537", "K01756", "K01923", "K01924", "K01944", "K01945", "K01951", "K02844", "K13800"),
    "Vitamins / Cofactors" = c("K00297", "K00552", "K00568", "K00599", "K00606", "K00652", "K00667", "K00763", "K00788", "K00793", "K00794", "K00796", "K00798", "K00815", "K00833", "K00859", "K00939", "K00941", "K00946", "K00949", "K01012", "K01071", "K01479", "K01492", "K01497", "K01556", "K01580", "K01661", "K01698", "K01737", "K01744", "K01770", "K01772", "K01773", "K01890", "K01935", "K02224", "K02225", "K02226", "K02227", "K02228", "K02229", "K02230", "K02232", "K02548", "K02549", "K02550", "K02551", "K02793", "K02794", "K03148", "K03149", "K03151", "K03183", "K03187", "K06214", "K06215"),
    "Fatty Acids" = c("K00019", "K00022", "K00059", "K00208", "K00232", "K00249", "K00626", "K00632", "K00645", "K00647", "K00648", "K00965", "K00981", "K01448", "K01692", "K01703", "K01704", "K01715", "K01961", "K01962", "K01963", "K01964", "K02517", "K07508", "K09458", "K11533"),
    "Cell Wall" = c("K00075", "K00286", "K00640", "K00666", "K00699", "K00748", "K00754", "K00974", "K01067", "K01093", "K01425", "K01448", "K01498", "K01778", "K01914", "K01919", "K01920", "K01921", "K02517", "K02519", "K02526", "K02527", "K02528", "K02535", "K02536", "K02842", "K02843", "K02848", "K02849", "K02850", "K03280", "K03469", "K03587", "K03588", "K03589", "K05363", "K06078", "K06079", "K14327", "K14328"),
    "ETC" = c("K00239", "K00240", "K00241", "K00242", "K00330", "K00331", "K00332", "K00333", "K00334", "K00335", "K00336", "K00337", "K00338", "K00339", "K00340", "K00341", "K00342", "K00343", "K00411", "K00412", "K00413", "K00414", "K00415", "K00425", "K00426", "K02256", "K02262", "K02264", "K02265", "K02276", "K02277", "K02548", "K03186", "K03187"),
    "ATP Synthase" = c("K02108", "K02109", "K02110", "K02111", "K02112", "K02113", "K02114", "K02115", "K02116", "K02132", "K02133", "K02134"),
    "Oxidative Phosphorylation" = c(
      # Comprehensive OxPhos: ETC + ATP synthase combined (canonical OXPHOS pathway)
      # Complex I (NADH dehydrogenase, NuoA-N)
      "K00330", "K00331", "K00332", "K00333", "K00334", "K00335", "K00336",
      "K00337", "K00338", "K00339", "K00340", "K00341", "K00342", "K00343",
      # Complex II (succinate dehydrogenase / fumarate reductase)
      "K00239", "K00240", "K00241", "K00242",
      # Complex III (cytochrome bc1)
      "K00411", "K00412", "K00413", "K00414", "K00415",
      # Complex IV (cytochrome c oxidase: aa3 and cbb3)
      "K02256", "K02261", "K02262", "K02263", "K02264", "K02265",
      "K02274", "K02275", "K02276", "K02277",
      # Quinol oxidases (bd, bo3)
      "K00425", "K00426", "K02297", "K02298", "K02299", "K02300",
      # F-type ATP synthase
      "K02108", "K02109", "K02110", "K02111", "K02112", "K02113", "K02114",
      "K02115", "K02116",
      # V/A-type ATP synthase (archaea + some bacteria)
      "K02117", "K02118", "K02119", "K02120", "K02121", "K02122", "K02123",
      "K02124", "K02125", "K02126"
    ),
    "Anaerobic Respiration" = c("K00244", "K00245", "K00246", "K00247", "K00368", "K00370", "K00371", "K00374", "K00376", "K00394", "K00395", "K00958", "K02305", "K02567", "K02568", "K02639", "K03603", "K04561", "K04755", "K07305", "K07306", "K11180", "K11181", "K15864"),
    "Photosynthesis" = c(
      # Photosystem II (PsbA-Z)
      "K02703", "K02706", "K02705", "K02704", "K02707", "K02708", "K02709",
      "K02710", "K02711", "K02712", "K02713", "K02714", "K02716", "K02717",
      "K02718", "K02719", "K02720", "K02721", "K02722", "K02723", "K02724",
      # Photosystem I (PsaA-N)
      "K02689", "K02690", "K02691", "K02692", "K02693", "K02694", "K02696",
      "K02697", "K02698", "K02699", "K02700", "K02701", "K02702",
      # Cytochrome b6f
      "K02635", "K02636", "K02637", "K02642", "K02643", "K02640",
      # ATP synthase (chloroplast / cyanobacterial)
      "K02105", "K02106", "K02107", "K02108", "K02109", "K02110", "K02111", "K02112",
      "K02113", "K02114", "K02115",
      # Plastocyanin / Ferredoxin / FNR
      "K02638", "K02639", "K02641",
      # Bacterial photosynthetic reaction centre (purple bacteria)
      "K08928", "K08929", "K13991", "K13992",
      # Bacterial photosynthetic reaction centre (green-sulphur, type I)
      "K08940", "K08941", "K08942", "K08943",
      # Antenna proteins (LH1, LH2)
      "K08926", "K08927", "K13993", "K13994"
    ),
    "ABC Transporters" = c("K01990", "K01991", "K01992", "K01998", "K01999", "K02000", "K02001", "K02002", "K02010", "K02011", "K02012", "K02013", "K02014", "K02015", "K02016", "K02026", "K02027", "K02028", "K02036", "K02037", "K02038", "K02039", "K02040", "K02065", "K02066", "K02067", "K06147", "K06148", "K06149", "K09969", "K10009", "K10013", "K10118", "K10119", "K10120", "K10440", "K10441", "K10442", "K10542", "K10543", "K10544", "K10545", "K10546", "K10551", "K10552", "K10553", "K10820", "K10821", "K10822", "K12340", "K12341", "K23227"),
    "Sec / Tat Systems" = c("K02453", "K02454", "K02455", "K02456", "K02457", "K02458", "K02459", "K02460", "K02461", "K02462", "K02463", "K02464", "K03070", "K03071", "K03072", "K03073", "K03074", "K03075", "K03076", "K03077", "K03106", "K03110", "K03116", "K03117", "K03118", "K03286", "K03425", "K03522", "K03523", "K13771"),
    "Efflux Pumps" = c("K03327", "K03380", "K03543", "K03585", "K06147", "K07672", "K08139", "K08140", "K09687", "K09688", "K13655", "K18138", "K18139", "K18143", "K18144", "K19415"),
    "Motility" = c("K02387", "K02388", "K02389", "K02390", "K02391", "K02392", "K02393", "K02394", "K02395", "K02396", "K02397", "K02398", "K02399", "K02400", "K02401", "K02402", "K02403", "K02404", "K02405", "K02406", "K02407", "K02408", "K02409", "K02410", "K02411", "K02412", "K02413", "K02414", "K02415", "K02416", "K02417", "K02418", "K02419", "K02420", "K02421", "K02422", "K02423", "K02424", "K02425", "K02426", "K02427", "K02428", "K02429", "K02430", "K02431", "K02432", "K02556", "K02557", "K02558", "K02658", "K02659", "K03406", "K03407", "K03408", "K03409", "K03410", "K03412", "K03413"),
    "CRISPR" = c("K07016", "K19078", "K19079", "K19080", "K19081", "K19082", "K19083", "K19084", "K19085", "K19086", "K19087", "K19088", "K19089", "K19090", "K19091", "K19473", "K19611", "K21319", "K21572"),
    "Stress Response" = c("K00386", "K00428", "K00432", "K00562", "K01151", "K02083", "K02313", "K02314", "K03111", "K03217", "K03553", "K03593", "K03610", "K03654", "K03671", "K03686", "K03687", "K03695", "K03704", "K03705", "K03706", "K03707", "K03708", "K03709", "K03781", "K03782", "K03799", "K04077", "K04078", "K04083", "K04564", "K04565", "K04764", "K05056", "K06203", "K06204", "K11931")
  )

  MAG_MAP_CATEGORIES <- list(

    # Central Carbon Metabolism
    "Glycolysis" = list(
      rect  = c(0.1190, 0.1478, 0.2584, 0.2259),
      paths = c("Glycolysis / Gluconeogenesis")
    ),
    "Pentose Phosphate\nPathway" = list(
      rect  = c(0.1190, 0.2440, 0.2584, 0.3290),
      paths = c("Pentose phosphate pathway",
                "Pentose and glucuronate interconversions")
    ),
    "Entner-Doudoroff\nPathway" = list(
      rect  = c(0.1190, 0.3457, 0.2584, 0.4310),
      paths = c("Glycolysis / Gluconeogenesis",
                "Pentose phosphate pathway")
    ),
    "TCA Cycle" = list(
      rect  = c(0.2950, 0.2189, 0.3829, 0.3639),
      paths = c("Citrate cycle (TCA cycle)",
                "Glyoxylate and dicarboxylate metabolism")
    ),
    "CO2 Fixation" = list(
      rect  = c(0.3968, 0.1506, 0.5139, 0.2286),
      paths = c("Carbon fixation by Calvin cycle",
                "Other carbon fixation pathways")
    ),
    "Fermentation" = list(
      rect  = c(0.3968, 0.3471, 0.5149, 0.4294),
      paths = c("Pyruvate metabolism",
                "Propanoate metabolism",
                "Butanoate metabolism",
                "C5-Branched dibasic acid metabolism")
    ),

    # N, S and CH4 Metabolism
    "Nitrogen\nFixation" = list(
      rect  = c(0.5511, 0.1520, 0.6980, 0.2300),
      paths = c("Nitrogen metabolism")
    ),
    "Assimilatory N" = list(
      rect  = c(0.7333, 0.1520, 0.8625, 0.2300),
      paths = c("Nitrogen metabolism")
    ),
    "Denitrification" = list(
      rect  = c(0.5500, 0.2496, 0.6980, 0.3262),
      paths = c("Nitrogen metabolism")
    ),
    "Sulfur Cycle" = list(
      rect  = c(0.7342, 0.2496, 0.8625, 0.3262),
      paths = c("Sulfur metabolism")
    ),
    "Nitrification" = list(
      rect  = c(0.5493, 0.3457, 0.6980, 0.4255),
      paths = c("Nitrogen metabolism")
    ),
    "Methane\nMetabolism" = list(
      rect  = c(0.7342, 0.3471, 0.8699, 0.4255),
      paths = c("Methane metabolism")
    ),

    # Biosynthesis / Anabolism
    "Amino Acids" = list(
      rect  = c(0.1301, 0.5115, 0.2444, 0.5897),
      paths = c("Alanine, aspartate and glutamate metabolism",
                "Glycine, serine and threonine metabolism",
                "Cysteine and methionine metabolism",
                "Valine, leucine and isoleucine biosynthesis",
                "Lysine biosynthesis",
                "Arginine biosynthesis",
                "Histidine metabolism",
                "Phenylalanine, tyrosine and tryptophan biosynthesis")
    ),
    "Nucleotides" = list(
      rect  = c(0.2704, 0.5115, 0.3950, 0.5897),
      paths = c("Purine metabolism",
                "Pyrimidine metabolism")
    ),
    "Vitamins /\nCofactors" = list(
      rect  = c(0.4165, 0.5563, 0.4926, 0.6483),
      paths = c("Thiamine metabolism",
                "Riboflavin metabolism",
                "Vitamin B6 metabolism",
                "Nicotinate and nicotinamide metabolism",
                "Pantothenate and CoA biosynthesis",
                "Biotin metabolism",
                "Lipoic acid metabolism",
                "Folate biosynthesis",
                "Porphyrin metabolism",
                "Ubiquinone and other terpenoid-quinone biosynthesis")
    ),
    "Fatty Acids" = list(
      rect  = c(0.1301, 0.6120, 0.2444, 0.6901),
      paths = c("Fatty acid biosynthesis",
                "Fatty acid degradation",
                "Glycerophospholipid metabolism")
    ),
    "Cell Wall" = list(
      rect  = c(0.2704, 0.6120, 0.3950, 0.6901),
      paths = c("Peptidoglycan biosynthesis",
                "Lipopolysaccharide biosynthesis",
                "Teichoic acid biosynthesis",
                "Amino sugar and nucleotide sugar metabolism")
    ),

    # Respiration / Energy
    # ETC, ATP Synthase and Oxidative Phosphorylation all map to KEGG 00190.
    "ETC" = list(
      rect  = c(0.5288, 0.5019, 0.6952, 0.5780),
      paths = c("Oxidative phosphorylation")
    ),
    "ATP Synthase" = list(
      rect  = c(0.7296, 0.5019, 0.8625, 0.5780),
      paths = c("Oxidative phosphorylation")
    ),
    "Oxidative\nPhosphorylation" = list(
      rect  = c(0.5288, 0.5995, 0.6952, 0.6761),
      paths = c("Oxidative phosphorylation")
    ),
    "Anaerobic\nRespiration" = list(
      rect  = c(0.7296, 0.5981, 0.8625, 0.6761),
      paths = c("Nitrogen metabolism",
                "Sulfur metabolism",
                "Oxidative phosphorylation")
    ),
    "Photosynthesis" = list(
      rect  = c(0.5288, 0.6943, 0.8625, 0.7473),
      paths = c("Photosynthesis",
                "Photosynthesis - antenna proteins")
    ),

    # Transporters / Systems
    "ABC\nTransporters" = list(
      rect  = c(0.1425, 0.8058, 0.2565, 0.8705),
      paths = c("ABC transporters")
    ),
    "Sec / Tat\nSystems" = list(
      rect  = c(0.2723, 0.8072, 0.3783, 0.8705),
      paths = c("Protein export",
                "Bacterial secretion system")
    ),
    "Efflux\nPumps" = list(
      rect  = c(0.3935, 0.8072, 0.4963, 0.8705),
      paths = c("ABC transporters",
                "Phosphotransferase system (PTS)",
                "Two-component system")
    ),
    "Motility" = list(
      rect  = c(0.5112, 0.8058, 0.6013, 0.8705),
      paths = c("Flagellar assembly",
                "Bacterial chemotaxis")
    ),
    "CRISPR" = list(
      rect  = c(0.6171, 0.8072, 0.7156, 0.8705),
      paths = c("DNA replication",
                "Homologous recombination")
    ),
    "Stress\nResponse" = list(
      rect  = c(0.7296, 0.8072, 0.8476, 0.8705),
      paths = c("Mismatch repair",
                "Two-component system")
    )
  )

  # Build KO lists for each category from KEGG_CATEGORIES (the master KEGG
  # pathway database loaded in global.R). This guarantees every KO listed in
  # a category actually belongs to one of the L3 pathways assigned to it.
  if (exists("KEGG_CATEGORIES") && !is.null(KEGG_CATEGORIES)) {
    for (cn in names(MAG_MAP_CATEGORIES)) {
      paths <- MAG_MAP_CATEGORIES[[cn]]$paths
      kos <- unique(KEGG_CATEGORIES$id[
        !is.na(KEGG_CATEGORIES$l3) &
        KEGG_CATEGORIES$l3 %in% paths
      ])
      MAG_MAP_CATEGORIES[[cn]]$kos_extended <- kos
      MAG_MAP_CATEGORIES[[cn]]$kos          <- kos  # default = extended
    }
  } else {
    for (cn in names(MAG_MAP_CATEGORIES)) {
      MAG_MAP_CATEGORIES[[cn]]$kos_extended <- character(0)
      MAG_MAP_CATEGORIES[[cn]]$kos          <- character(0)
    }
  }
  # Attach curated central KO lists (key matching uses the category name with
  # newlines stripped, so "Pentose Phosphate\nPathway" matches "Pentose Phosphate Pathway")
  for (cn in names(MAG_MAP_CATEGORIES)) {
    key <- gsub("[\n\\s]+", " ", cn, perl = TRUE)
    MAG_MAP_CATEGORIES[[cn]]$kos_central <-
      if (!is.null(CORE_KOS[[key]])) CORE_KOS[[key]] else character(0)
  }

  # Helper: given a category and the active mode ("extended" or "central"),
  # return the active KO list.
  magmap_active_kos <- function(cat_info, mode) {
    if (identical(mode, "central")) cat_info$kos_central else cat_info$kos_extended
  }

  magmap_selected_bin <- reactiveVal(NULL)

  # Completeness: present KOs / curated KO list size * 100
  magmap_completeness <- reactive({
    proj    <- sqm_data(); req(proj)
    bin     <- magmap_selected_bin(); req(bin)
    mode    <- input$magmap_ko_mode %||% "extended"
    mag_kos <- tryCatch(get_bin_kos(proj, bin), error = function(e) character(0))
    lapply(MAG_MAP_CATEGORIES, function(cat_info) {
      cat_kos <- unique(magmap_active_kos(cat_info, mode))
      if (length(cat_kos) == 0) return(list(pct = NA_real_, present = 0L, total = 0L))
      present <- sum(cat_kos %in% mag_kos)
      list(pct     = round(100 * present / length(cat_kos), 1),
           present = present,
           total   = length(cat_kos))
    })
  })

  # MAG selector UI
  output$magmap_bin_select_ui <- renderUI({
    proj <- sqm_data()
    if (is.null(proj))
      return(tags$div(style = "font-size:0.8rem; color:var(--muted);",
        "Load a SQM project with binning first."))
    bins <- tryCatch(rownames(proj$bins$table), error = function(e) NULL)
    if (is.null(bins) || length(bins) == 0)
      return(tags$div(style = "font-size:0.8rem; color:var(--muted);",
        "No MAGs found in this project."))
    selectInput("magmap_bin", NULL, choices = c("— select a MAG —" = "", bins),
                selected = isolate(magmap_selected_bin()) %||% "")
  })

  observeEvent(input$magmap_bin, {
    v <- input$magmap_bin
    magmap_selected_bin(if (nzchar(v)) v else NULL)
    # Keep the keggmap-mode selector in sync
    if (!identical(isolate(input$magmap_bin2), v))
      updateSelectInput(session, "magmap_bin2", selected = v)
  })

  # Sidebar info panel — shared by diagram & keggmap modes
  magmap_selected_panel <- function() {
    bin <- magmap_selected_bin()
    if (is.null(bin))
      return(tags$div(style = "font-size:0.8rem; color:var(--muted);",
        "Select a MAG above to see completeness overlay."))
    proj    <- sqm_data()
    mag_kos <- tryCatch(get_bin_kos(proj, bin), error = function(e) character(0))

    # ── Pull MAG stats from proj$bins$table (loaded by SqueezeMeta from bintable) ──
    bt  <- tryCatch(proj$bins$table, error = function(e) NULL)
    row <- NULL
    if (!is.null(bt) && bin %in% rownames(bt)) row <- bt[bin, , drop = FALSE]

    stat_row <- function(label, value, unit = "") {
      if (is.null(value) || length(value) == 0 ||
          (is.numeric(value) && is.na(value)) ||
          (is.character(value) && !nzchar(trimws(value))))
        return(NULL)
      val_txt <- if (is.numeric(value)) {
        if (value >= 1e6)      sprintf("%.2f Mb", value / 1e6)
        else if (value >= 1e3) sprintf("%.1f kb", value / 1e3)
        else                   format(value, big.mark = ",", scientific = FALSE)
      } else as.character(value)
      tags$div(style = "display:flex; justify-content:space-between; gap:8px; font-size:0.78rem; padding:2px 0;",
        tags$span(style = "color:var(--muted);", label),
        tags$span(style = "font-weight:600; text-align:right; word-break:break-word;",
                  paste0(val_txt, if (nzchar(unit)) paste0(" ", unit) else ""))
      )
    }

    get_val <- function(row, col_pattern, exact = FALSE) {
      if (is.null(row)) return(NULL)
      cn  <- colnames(row)
      idx <- if (exact) match(col_pattern, cn) else grep(col_pattern, cn, ignore.case = TRUE)[1]
      if (is.na(idx) || length(idx) == 0) return(NULL)
      v <- row[[idx]]
      if (is.factor(v)) as.character(v) else v
    }

    completeness  <- get_val(row, "^Completeness$")
    contamination <- get_val(row, "^Contamination$")
    size_bp       <- get_val(row, "^(Length|Size)$|^Length \\(bp\\)$")
    num_contigs   <- get_val(row, "^Num.*contigs?$|^Number of contigs$|^Contigs$")

    # ── Read directly from 18.<project>.bintable file: Tax, Length, Num contigs ──
    taxonomy_raw <- NULL
    proj_dir <- tryCatch(path_project(), error = function(e) NULL)
    if (!is.null(proj_dir) && nzchar(proj_dir)) {
      results_dir <- file.path(proj_dir, "results")
      bt_files <- list.files(
        c(results_dir, proj_dir),
        pattern = "^18\\..*\\.bintable$", full.names = TRUE
      )
      if (length(bt_files) > 0) {
        bt_path <- bt_files[1]
        bt_df <- tryCatch(
          read.table(bt_path, sep = "\t", header = TRUE, comment.char = "#",
                     quote = "", stringsAsFactors = FALSE, check.names = FALSE,
                     fill = TRUE, na.strings = c("", "NA")),
          error = function(e) NULL
        )
        if (!is.null(bt_df) && nrow(bt_df) > 0) {
          bin_col <- colnames(bt_df)[1]
          match_idx <- which(bt_df[[bin_col]] == bin)
          if (length(match_idx) > 0) {
            r_bt <- bt_df[match_idx[1], , drop = FALSE]
            if ("Tax" %in% colnames(r_bt)) {
              v <- r_bt[["Tax"]]
              if (!is.na(v) && nzchar(trimws(as.character(v))))
                taxonomy_raw <- trimws(as.character(v))
            }
            # Length (size) — try several common column names
            for (lc in c("Length", "Size", "Length (bp)")) {
              if (lc %in% colnames(r_bt)) {
                v <- suppressWarnings(as.numeric(r_bt[[lc]]))
                if (!is.na(v) && v > 0) { size_bp <- v; break }
              }
            }
            # Number of contigs
            for (nc in c("Num contigs", "Number of contigs", "Contigs")) {
              if (nc %in% colnames(r_bt)) {
                v <- suppressWarnings(as.numeric(r_bt[[nc]]))
                if (!is.na(v) && v > 0) { num_contigs <- v; break }
              }
            }
          }
        }
      }
    }

    # ── Number of genes: count ORFs whose contig belongs to this bin ──────────
    num_genes <- NULL
    tryCatch({
      ct <- proj$contigs$table
      ot <- proj$orfs$table
      if (!is.null(ct) && !is.null(ot)) {
        # Find the bin-assignment column in contigs
        bin_col_ct <- NA
        for (cand in c("Bin ID", "Bin", "DAS", "bin", "Bin Name")) {
          if (cand %in% colnames(ct)) { bin_col_ct <- cand; break }
        }
        if (!is.na(bin_col_ct)) {
          # Bin names in contigs table may lack the .fa.contigs / .fa_sub.contigs suffix
          # Try the full name first, then progressively stripped variants
          bin_variants <- unique(c(
            bin,
            sub("\\.fa\\.contigs$",     "", bin),
            sub("\\.fa_sub\\.contigs$", "", bin),
            sub("\\.contigs$",          "", bin),
            sub("\\.fa$",               "", bin)
          ))
          bin_contigs <- rownames(ct)[ct[[bin_col_ct]] %in% bin_variants]
          if (length(bin_contigs) > 0) {
            # Prefer the "Num genes" column already present in the contigs table
            if ("Num genes" %in% colnames(ct)) {
              num_genes <- sum(suppressWarnings(as.numeric(
                ct[bin_contigs, "Num genes"])), na.rm = TRUE)
            } else {
              # Fall back to counting ORFs by contig
              contig_col_ot <- NA
              for (cand in c("Contig ID", "Contig", "contig")) {
                if (cand %in% colnames(ot)) { contig_col_ot <- cand; break }
              }
              if (!is.na(contig_col_ot))
                num_genes <- sum(ot[[contig_col_ot]] %in% bin_contigs)
              else
                num_genes <- sum(sub("_[0-9]+$", "", rownames(ot)) %in% bin_contigs)
            }
          }
        }
      }
    }, error = function(e) NULL)

    # ── Coverage: columnas "Coverage <sample>" de proj$bins$table ────────────
    cov_block <- NULL
    if (!is.null(row)) {
      cov_cols <- grep("^Coverage ", colnames(row), value = TRUE)
      if (length(cov_cols) > 0) {
        cov_rows <- lapply(cov_cols, function(cc) {
          sample_name <- sub("^Coverage ", "", cc)
          val <- suppressWarnings(as.numeric(row[[cc]]))
          if (is.na(val)) return(NULL)
          tags$div(
            style = "display:flex; justify-content:space-between; gap:8px; font-size:0.76rem; padding:1px 0;",
            tags$span(style = "color:var(--muted); word-break:break-all;", sample_name),
            tags$span(style = "font-weight:600;", sprintf("%.2f\u00d7", val))
          )
        })
        cov_rows <- Filter(Negate(is.null), cov_rows)
        if (length(cov_rows) > 0)
          cov_block <- tags$div(
            style = "margin-top:8px;",
            tags$div(
              style = "font-size:0.72rem; color:var(--muted); text-transform:uppercase; letter-spacing:0.04em; margin-bottom:2px;",
              "Coverage per sample"
            ),
            do.call(tags$div, cov_rows)
          )
      }
    }

    # ── Taxonomy: parsear string separado por ";" en niveles independientes ───
    # Soporta "k__Bacteria;p__Proteobacteria;..." o "Bacteria;Proteobacteria;..."
    # Soporta prefijos con _ simple (k_) o doble (k__), y "rank:value" (Tax 16S)
    RANK_LABELS <- c(
      k = "Kingdom", d = "Domain", p = "Phylum", c = "Class",
      o = "Order",   f = "Family", g = "Genus",  s = "Species"
    )
    PLAIN_RANKS <- c("Domain", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")

    taxonomy_block <- NULL
    if (!is.null(taxonomy_raw) && nzchar(trimws(as.character(taxonomy_raw)))) {
      tax_str    <- trimws(as.character(taxonomy_raw))
      tax_levels <- trimws(unlist(strsplit(tax_str, ";")))
      tax_levels <- tax_levels[nzchar(tax_levels)]

      # Detectar formato: "k_X" / "k__X",  "rank:value", o sin prefijo
      has_underscore <- any(grepl("^[a-z]_+", tax_levels))
      has_colon      <- any(grepl("^[a-z]+:", tax_levels))

      tax_rows <- if (has_underscore) {
        lapply(tax_levels, function(lvl) {
          # Captura la letra de rango y el valor: "k_Bacteria" o "k__Bacteria"
          m <- regmatches(lvl, regexec("^([a-z])_+(.+)$", lvl))[[1]]
          if (length(m) < 3) return(NULL)
          letter <- m[2]; value <- trimws(m[3])
          label  <- RANK_LABELS[[letter]]
          if (is.null(label)) label <- toupper(letter)
          if (!nzchar(value) || value %in% c("NA", "Unknown", "unclassified")) return(NULL)
          tags$div(
            style = "display:flex; justify-content:space-between; gap:8px; font-size:0.76rem; padding:1px 0;",
            tags$span(style = "color:var(--muted); flex-shrink:0;", label),
            tags$span(style = "font-weight:600; text-align:right; word-break:break-word;",
                      if (letter == "s") tags$em(value) else value)
          )
        })
      } else if (has_colon) {
        # Formato "rank:value" del Tax 16S
        lapply(tax_levels, function(lvl) {
          m <- regmatches(lvl, regexec("^([^:]+):(.+)$", lvl))[[1]]
          if (length(m) < 3) return(NULL)
          label <- trimws(m[2]); value <- trimws(m[3])
          # Capitalizar label
          label <- paste0(toupper(substr(label, 1, 1)), substr(label, 2, nchar(label)))
          if (!nzchar(value) || value %in% c("NA", "Unknown", "unclassified")) return(NULL)
          tags$div(
            style = "display:flex; justify-content:space-between; gap:8px; font-size:0.76rem; padding:1px 0;",
            tags$span(style = "color:var(--muted); flex-shrink:0;", label),
            tags$span(style = "font-weight:600; text-align:right; word-break:break-word;", value)
          )
        })
      } else {
        # Sin prefijos: asignar rangos en orden
        mapply(function(value, label) {
          if (!nzchar(trimws(value)) || value %in% c("NA", "Unknown")) return(NULL)
          tags$div(
            style = "display:flex; justify-content:space-between; gap:8px; font-size:0.76rem; padding:1px 0;",
            tags$span(style = "color:var(--muted); flex-shrink:0;", label),
            tags$span(style = "font-weight:600; text-align:right; word-break:break-word;",
                      if (label == "Species") tags$em(value) else value)
          )
        }, tax_levels, PLAIN_RANKS[seq_along(tax_levels)], SIMPLIFY = FALSE)
      }
      tax_rows <- Filter(Negate(is.null), tax_rows)

      if (length(tax_rows) > 0)
        taxonomy_block <- tags$div(
          style = "margin-top:8px;",
          tags$div(
            style = "font-size:0.72rem; color:var(--muted); text-transform:uppercase; letter-spacing:0.04em; margin-bottom:2px;",
            "Taxonomy"
          ),
          do.call(tags$div, tax_rows)
        )
    }

    tags$div(
      tags$div(class = "form-label", "Selected MAG"),
      tags$div(style = "font-size:0.82rem; word-break:break-all; font-weight:600;", bin),
      tags$div(style = "font-size:0.74rem; color:var(--muted); margin-top:2px; margin-bottom:8px;",
        paste0(length(mag_kos), " unique KEGG KOs")),

      stat_row("Completeness",  if (!is.null(completeness))  sprintf("%.1f%%", as.numeric(completeness))),
      stat_row("Contamination", if (!is.null(contamination)) sprintf("%.1f%%", as.numeric(contamination))),
      stat_row("Size",          if (!is.null(size_bp))       sprintf("%.2f Mb", as.numeric(size_bp) / 1e6)),
      stat_row("Contigs",       if (!is.null(num_contigs))   as.character(as.integer(num_contigs))),
      stat_row("Genes",         if (!is.null(num_genes))     as.character(as.integer(num_genes))),

      taxonomy_block,
      cov_block
    )
  }
  output$magmap_selected_ui  <- renderUI({ magmap_selected_panel() })
  output$magmap_selected_ui2 <- renderUI({ magmap_selected_panel() })


  # Reusable renderer for the pathview pathway view (used by both the
  # single-MAG view and the KEGG-map browser). Returns a tagList; the
  # caller adds a Back button if needed.
  magmap_render_pathway_view <- function() {
          img_path  <- magmap_pw_img();  req(img_path, file.exists(img_path))
          nodes     <- magmap_pw_nodes()
          res_name  <- paste0("magmap_pw_", magmap_pw_pid())
          addResourcePath(res_name, dirname(img_path))
          img_src   <- paste0(res_name, "/", basename(img_path))
          bin       <- magmap_selected_bin() %||% ""
          mag_kos   <- tryCatch(get_bin_kos(sqm_data(), bin), error = function(e) character(0))
          kegg_names <- tryCatch(sqm_data()$misc$KEGG_names, error = function(e) NULL)
  
          # Tooltip CSS + div (reuse pw- id so same JS works)
          tooltip_css <- tags$style(HTML("
            #pw-tooltip {
              position:fixed; pointer-events:none; z-index:9999;
              background:rgba(20,30,50,0.92); color:#f0f4f8;
              padding:5px 9px; border-radius:5px; font-size:0.75rem;
              max-width:320px; line-height:1.4; display:none;
              box-shadow:0 2px 8px rgba(0,0,0,0.3); white-space:pre-wrap; word-break:break-word;
            }
          "))
          tooltip_div <- tags$div(id = "pw-tooltip")
          tooltip_js  <- tags$script(HTML("
            (function() {
              var tip = document.getElementById('pw-tooltip');
              if (!tip) return;
              document.addEventListener('mousemove', function(e) {
                tip.style.left = (e.clientX + 14) + 'px';
                tip.style.top  = (e.clientY + 14) + 'px';
              });
            })();
            function pwShowTip(el) {
              var tip = document.getElementById('pw-tooltip');
              if (tip) { tip.textContent = el.getAttribute('data-tip'); tip.style.display='block'; }
            }
            function pwHideTip() {
              var tip = document.getElementById('pw-tooltip');
              if (tip) tip.style.display='none';
            }
          "))
  
          # Build node JSON — same structure as server_pathways.R
          # Resolve KO function names from KEGG_NAMES (master DB, has all KOs)
          # with fallback to the project's own KEGG_names.
          node_json <- if (!is.null(nodes) && nrow(nodes) > 0) {
            name_for_ko <- function(ko) {
              if (exists("KEGG_NAMES") && !is.null(KEGG_NAMES) && ko %in% names(KEGG_NAMES)) {
                nm <- as.character(KEGG_NAMES[[ko]])
                if (length(nm) && !is.na(nm) && nzchar(nm)) return(nm)
              }
              if (!is.null(kegg_names) && ko %in% names(kegg_names)) {
                nm <- as.character(kegg_names[[ko]])
                if (length(nm) && !is.na(nm) && nzchar(nm)) return(nm)
              }
              NA_character_
            }
            node_list <- lapply(seq_len(nrow(nodes)), function(i) {
              r      <- nodes[i, ]
              ko_ids <- unique(sub("^ko:", "", trimws(unlist(strsplit(r$ko_names, "[[:space:]]+")))))
              ko_ids <- ko_ids[grepl("^K[0-9]{5}$", ko_ids)]
              nms    <- unique(na.omit(vapply(ko_ids, name_for_ko, character(1))))
              ko_str   <- paste(ko_ids, collapse = ", ")
              name_str <- if (length(nms) > 0) paste(nms, collapse = " / ")
                          else "(no annotation available)"
              present  <- as.integer(any(ko_ids %in% mag_kos))
              tip <- paste0(ko_str, "\n", name_str, "\n\u2014 ",
                            if (present == 1L) "PRESENT" else "absent")
              list(x = r$x, y = r$y, w = r$w, h = r$h, tip = tip, present = present)
            })
            jsonlite::toJSON(node_list, auto_unbox = TRUE)
          } else "[]"
  
          map_id <- "magmap_pwmap"
          img_tag <- tags$div(
            id = paste0(map_id, "_wrap"),
            style = "position:relative; display:inline-block; width:100%;",
            tags$img(src = img_src, id = map_id,
              style = "max-width:100%; display:block; border:1px solid var(--border); border-radius:6px;",
              alt = "KEGG pathway")
          )
          overlay_js <- tags$script(HTML(sprintf('
            (function() {
              var nodes  = %s;
              var img    = document.getElementById("%s");
              var wrap   = document.getElementById("%s_wrap");
              function placeBoxes() {
                // Remove old boxes
                wrap.querySelectorAll(".magmap-box").forEach(function(el) { el.remove(); });
                var scaleX = img.offsetWidth  / img.naturalWidth;
                var scaleY = img.offsetHeight / img.naturalHeight;
                for (var i = 0; i < nodes.length; i++) {
                  var n = nodes[i];
                  var x = (n.x - n.w / 2) * scaleX;
                  var y = (n.y - n.h / 2) * scaleY;
                  var w = n.w * scaleX;
                  var h = n.h * scaleY;
                  var box = document.createElement("div");
                  box.className = "magmap-box";
                  box.style.position   = "absolute";
                  box.style.left       = x + "px";
                  box.style.top        = y + "px";
                  box.style.width      = w + "px";
                  box.style.height     = h + "px";
                  box.style.background = n.present === 1 ? "rgba(220,80,70,0.40)" : "rgba(200,200,200,0.30)";
                  box.style.border     = n.present === 1 ? "1px solid rgba(192,57,43,0.7)" : "1px solid rgba(160,160,160,0.6)";
                  box.style.boxSizing  = "border-box";
                  box.style.cursor     = "crosshair";
                  (function(tip) {
                    box.addEventListener("mouseenter", function() {
                      var el = document.getElementById("pw-tooltip");
                      if (el) { el.textContent = tip; el.style.display = "block"; }
                    });
                    box.addEventListener("mouseleave", function() {
                      var el = document.getElementById("pw-tooltip");
                      if (el) el.style.display = "none";
                    });
                  })(n.tip);
                  wrap.appendChild(box);
                }
              }
              if (img.complete && img.naturalWidth > 0) { placeBoxes(); }
              else { img.addEventListener("load", placeBoxes); }
              window.addEventListener("resize", placeBoxes);
              if (window.ResizeObserver) {
                new ResizeObserver(placeBoxes).observe(img);
              }
            })();
          ', node_json, map_id, map_id)))
  
          return(tagList(
            tags$div(style = "font-size:0.78rem; color:var(--muted); margin-bottom:6px;",
              tags$span(style = "display:inline-block; width:12px; height:12px; background:#c0392b; border-radius:2px; margin-right:4px; vertical-align:middle;"),
              "Present in MAG  ",
              tags$span(style = "display:inline-block; width:12px; height:12px; background:#e8e8e8; border:1px solid #ccc; border-radius:2px; margin-right:4px; margin-left:10px; vertical-align:middle;"),
              "Absent"
            ),
            tooltip_css, tooltip_div, tooltip_js,
            img_tag, overlay_js
          ))
  }

  # Main view: BacMet image + SVG overlay (coloured rects + % labels)
  # OR pathview pathway diagram when magmap_view_mode() == "pathway"
  output$magmap_view_ui <- renderUI({

    # ── PATHWAY MODE ────────────────────────────────────────────────────────────
    if (magmap_view_mode() == "pathway") {
      s <- magmap_pw_status()
      back_btn <- actionButton("magmap_back", "\u2190 Back",
                               class = "btn-default",
                               style = "margin-bottom:10px; font-size:0.82rem;")
      if (s == "generating") {
        return(tagList(back_btn,
          tags$div(style = "color:var(--muted); font-size:0.85rem; padding:2rem; text-align:center;",
            tags$div(style = "font-size:1.5rem; margin-bottom:8px;", "\u25cc"),
            tags$div("Generating pathway map for ",
                     tags$strong(magmap_pw_name() %||% magmap_pw_pid()), "\u2026"),
            tags$div(style = "margin-top:6px; font-size:0.78rem;",
              "Red = present in MAG, grey = absent"))))
      }
      if (s == "error") {
        return(tagList(back_btn,
          tags$div(style = "color:#c0392b; font-size:0.85rem; padding:2rem; text-align:center;",
            tags$div(style = "font-size:1.5rem;", "\u2715"),
            tags$div("Pathway generation failed."))))
      }
      if (s == "ready") {
        return(tagList(back_btn, magmap_render_pathway_view()))
      }
      return(tagList(back_btn))
    }

    # ── MAP MODE (default) ──────────────────────────────────────────────────────
    proj <- sqm_data()

    # Resolve image (www/ folder preferred; fall back to base64 or copy)
    img_src   <- "BacMet.png"
    www_path  <- file.path(app_dir, "www", "BacMet.png")
    root_path <- file.path(app_dir, "BacMet.png")
    if (!file.exists(www_path) && !file.exists(root_path))
      return(tags$div(style = "padding:2rem; color:var(--muted);",
        "BacMet.png not found. Place it in the app's www/ folder or the app directory."))
    if (!file.exists(www_path) && file.exists(root_path)) {
      if (requireNamespace("base64enc", quietly = TRUE)) {
        img_src <- paste0("data:image/png;base64,",
                          base64enc::base64encode(root_path))
      } else {
        dir.create(file.path(app_dir, "www"), showWarnings = FALSE)
        file.copy(root_path, www_path)
      }
    }

    bin  <- magmap_selected_bin()
    comp <- if (!is.null(bin) && !is.null(proj))
              tryCatch(magmap_completeness(), error = function(e) NULL)
            else NULL

    # SVG defs: drop-shadow for text legibility
    svg_defs <- '<defs></defs>'

    # Build SVG elements: progress-bar fill + black bold % text at top
    svg_elements <- ""
    if (!is.null(comp)) {
      svg_elements <- paste(mapply(function(cat_name, cat_info, cat_comp) {
        r    <- cat_info$rect
        pct  <- cat_comp$pct
        pres <- cat_comp$present
        tot  <- cat_comp$total

        fill_col <- "220,40,40"   # red overlay for all bars

        label    <- if (is.na(pct)) "N/A" else paste0(round(pct), "%")
        tip      <- htmltools::htmlEscape(
                      paste0(gsub("\n", " ", cat_name), "\n",
                             label, "  (", pres, "/", tot, " KOs)"),
                      attribute = TRUE)

        # Coordinates in viewBox [0,100] units
        x0 <- r[1] * 100; y0 <- r[2] * 100
        w  <- (r[3] - r[1]) * 100; h <- (r[4] - r[2]) * 100
        cx <- (r[1] + r[3]) / 2 * 100

        # Progress bar width proportional to pct
        fill_w <- if (is.na(pct)) 0 else w * (pct / 100)

        # clip-path id (alphanumeric only)
        cp_id <- paste0("cp_", gsub("[^A-Za-z0-9]", "_", cat_name))

        # Label position: default = above top-left corner of the box
        # Exceptions:
        #   Motility, ATP Synthase  → top-right (avoid overlap with other legends)
        #   TCA Cycle               → top-center
        label_pos <- "TL"
        if (cat_name %in% c("Motility", "ATP Synthase")) label_pos <- "TR"
        if (cat_name == "TCA Cycle")                    label_pos <- "TC"

        if (label_pos == "TR") {
          lx <- x0 + w
          ty <- y0 - 0.3
          anchor <- "end"
        } else if (label_pos == "TC") {
          lx <- cx
          ty <- y0 - 0.3
          anchor <- "middle"
        } else {
          lx <- x0
          ty <- y0 - 0.3
          anchor <- "start"
        }

        paste0(
          # Clip path so progress bar respects rounded corners
          if (fill_w > 0) sprintf(
            '<clipPath id="%s"><rect x="%.3f" y="%.3f" width="%.3f" height="%.3f" rx="0.8" ry="0.8"/></clipPath>\n',
            cp_id, x0, y0, w, h) else "",

          # Progress bar fill
          if (fill_w > 0) sprintf(
            '<rect x="%.3f" y="%.3f" width="%.3f" height="%.3f" fill="rgba(%s,0.45)" clip-path="url(#%s)" data-cat="%s" data-tip="%s"/>\n',
            x0, y0, fill_w, h, fill_col, cp_id, htmltools::htmlEscape(cat_name, attribute=TRUE), tip) else "",

          # Box outline removed (no stroke)
          sprintf('<rect class="magmap-cat" data-cat="%s"', htmltools::htmlEscape(cat_name, attribute=TRUE)),
          sprintf(' x="%.3f" y="%.3f" width="%.3f" height="%.3f"', x0, y0, w, h),
          ' fill="transparent"'  ,
          ' stroke="none"',
          ' rx="0.8" ry="0.8"',
          sprintf(' data-tip="%s"/>\n', tip),

          # % label: black, bold, positioned outside the top edge of the box
          sprintf('<text class="magmap-label" data-cat="%s"', htmltools::htmlEscape(cat_name, attribute=TRUE)),
          sprintf(' x="%.3f" y="%.3f"', lx, ty),
          sprintf(' dominant-baseline="auto" text-anchor="%s"', anchor),
          ' font-family="Arial,sans-serif" font-weight="bold"',
          ' font-size="1.7"',
          ' fill="black"',
          sprintf(' data-tip="%s">', tip),
          label,
          '</text>\n'
        )
      }, names(MAG_MAP_CATEGORIES), MAG_MAP_CATEGORIES, comp, SIMPLIFY = FALSE),
      collapse = "")
    }

    # Tooltip + click JS
    tooltip_js <- "
(function() {
  var tip = document.getElementById('magmap-tooltip');
  if (!tip) {
    tip = document.createElement('div');
    tip.id = 'magmap-tooltip';
    tip.style.cssText =
      'position:fixed;pointer-events:none;display:none;' +
      'background:rgba(15,15,15,0.93);color:#fff;font-size:0.8rem;' +
      'padding:7px 13px;border-radius:7px;z-index:9999;white-space:pre;' +
      'box-shadow:0 3px 12px rgba(0,0,0,0.5);line-height:1.55;';
    document.body.appendChild(tip);
  }
  // Clean up any leftover calibration UI from earlier sessions
  var oldCoord = document.getElementById('magmap-coord-tip');
  if (oldCoord) oldCoord.remove();
  var oldPanel = document.getElementById('magmap-calib-panel');
  if (oldPanel) oldPanel.remove();

  var svg = document.getElementById('magmap-svg-overlay');
  if (!svg) return;
  svg.querySelectorAll('[data-cat]').forEach(function(el) {
    el.style.cursor = 'pointer';
    el.addEventListener('mouseenter', function() {
      tip.textContent = el.getAttribute('data-tip'); tip.style.display = 'block';
    });
    el.addEventListener('mousemove', function(e) {
      tip.style.left = (e.clientX + 16) + 'px';
      tip.style.top  = (e.clientY - 36) + 'px';
    });
    el.addEventListener('mouseleave', function() { tip.style.display = 'none'; });
    el.addEventListener('click', function() {
      var cat = el.getAttribute('data-cat');
      if (cat && typeof Shiny !== 'undefined')
        Shiny.setInputValue('magmap_clicked_cat', cat, {priority: 'event'});
    });
  });
})();
"

    tagList(
      tags$div(
        style = "position:relative; display:inline-block; width:100%; max-width:100%;",
        tags$img(src = img_src, style = "width:100%; height:auto; display:block;",
                 alt = "Metabolic diagram"),
        tags$svg(
          id                  = "magmap-svg-overlay",
          xmlns               = "http://www.w3.org/2000/svg",
          viewBox             = "0 0 100 100",
          preserveAspectRatio = "none",
          style = paste0("position:absolute; top:0; left:0;",
                         " width:100%; height:100%;",
                         " pointer-events:auto; overflow:visible;"),
          HTML(paste0(svg_defs, svg_elements))
        )
      ),
      tags$script(HTML(tooltip_js))
    )
  })

  # ── MAG Map: clicked category → KO detail table ─────────────────────────────

  magmap_clicked_cat <- reactiveVal(NULL)

  observeEvent(input$magmap_clicked_cat, {
    v <- input$magmap_clicked_cat
    magmap_clicked_cat(if (!is.null(v) && nzchar(v)) v else NULL)
  })

  output$magmap_detail_ui <- renderUI({
    cat_name <- magmap_clicked_cat()
    if (is.null(cat_name)) return(NULL)

    bin <- magmap_selected_bin()
    if (is.null(bin)) return(NULL)

    proj <- sqm_data()
    if (is.null(proj)) return(NULL)

    # KOs present in this MAG
    mag_kos <- tryCatch(get_bin_kos(proj, bin), error = function(e) character(0))

    # Find the matching category key (cat_name from JS has \n replaced by space)
    cat_key <- names(MAG_MAP_CATEGORIES)[
      vapply(names(MAG_MAP_CATEGORIES), function(n) {
        identical(n, cat_name) ||
        identical(gsub("\n", " ", n), cat_name) ||
        identical(gsub("\n", "\\n", n), cat_name)
      }, logical(1))
    ]
    if (length(cat_key) == 0) return(NULL)
    mode    <- input$magmap_ko_mode %||% "extended"
    cat_kos <- unique(magmap_active_kos(MAG_MAP_CATEGORIES[[cat_key[1]]], mode))

    # KO name + path lookup
    # Source 0: KEGG_NAMES global (master KEGG database, loaded once in global.R)
    # Source 1: proj$functions$KEGG$names
    # Source 2: proj$misc$KEGG_names
    # Source 3: orfs table KEGGFUN

    ko_names_df <- tryCatch({
      df <- proj$functions$KEGG$names
      if (is.data.frame(df) && nrow(df) > 0) df else NULL
    }, error = function(e) NULL)

    misc_names <- tryCatch(proj$misc$KEGG_names, error = function(e) NULL)

    # Build KO -> name dictionary from ORF table as a fallback
    orf_ko_names <- tryCatch({
      ot <- proj$orfs$table
      if (!is.null(ot) && all(c("KEGG ID", "KEGGFUN") %in% colnames(ot))) {
        kos_raw  <- as.character(ot[, "KEGG ID"])
        funs_raw <- as.character(ot[, "KEGGFUN"])
        kos_clean <- sub("\\*+$", "", kos_raw)
        keep <- !is.na(kos_clean) & nzchar(kos_clean) &
                !is.na(funs_raw)  & nzchar(funs_raw)
        if (any(keep)) {
          tapply(funs_raw[keep], kos_clean[keep],
                 function(v) v[which(nzchar(v))[1]])
        } else NULL
      } else NULL
    }, error = function(e) NULL)

    # Helper: get name for a KO
    ko_name_fn <- function(ko) {
      if (exists("KEGG_NAMES") && !is.null(KEGG_NAMES) && ko %in% names(KEGG_NAMES)) {
        nm <- as.character(KEGG_NAMES[[ko]])
        if (!is.na(nm) && nzchar(nm)) return(nm)
      }
      if (!is.null(ko_names_df) && ko %in% rownames(ko_names_df)) {
        nm <- as.character(ko_names_df[ko, "Name"])
        if (!is.na(nm) && nzchar(nm)) return(nm)
      }
      if (!is.null(misc_names) && ko %in% names(misc_names)) {
        nm <- as.character(misc_names[[ko]])
        if (!is.na(nm) && nzchar(nm)) return(nm)
      }
      if (!is.null(orf_ko_names) && ko %in% names(orf_ko_names)) {
        nm <- as.character(orf_ko_names[[ko]])
        if (!is.na(nm) && nzchar(nm)) return(nm)
      }
      "(no annotation available)"
    }

    # Build KO -> KEGGPATH dictionary from ORF table as a fallback for L3
    orf_ko_paths <- tryCatch({
      ot <- proj$orfs$table
      if (!is.null(ot) && all(c("KEGG ID", "KEGGPATH") %in% colnames(ot))) {
        kos_raw   <- as.character(ot[, "KEGG ID"])
        paths_raw <- as.character(ot[, "KEGGPATH"])
        kos_clean <- sub("\\*+$", "", kos_raw)
        keep <- !is.na(kos_clean)   & nzchar(kos_clean) &
                !is.na(paths_raw)   & nzchar(paths_raw)
        if (any(keep)) {
          tapply(paths_raw[keep], kos_clean[keep],
                 function(v) v[which(nzchar(v))[1]])
        } else NULL
      } else NULL
    }, error = function(e) NULL)

    # Helper: get L3 pathways for a KO
    # Path column format: "L1; L2; L3 | L1; L2; L3 | ..."
    # Sources (in priority order):
    #   1. KEGG_CATEGORIES global (complete KEGG database, covers all KOs)
    #   2. proj$functions$KEGG$names (only KOs in project)
    #   3. proj$orfs$table KEGGPATH (only KOs in project)
    ko_l3_fn <- function(ko) {
      # Source 1: global KEGG_CATEGORIES (complete database)
      if (exists("KEGG_CATEGORIES") && !is.null(KEGG_CATEGORIES)) {
        kc_rows <- KEGG_CATEGORIES[KEGG_CATEGORIES$id == ko & !is.na(KEGG_CATEGORIES$l3), , drop = FALSE]
        if (nrow(kc_rows) > 0) {
          l3 <- unique(as.character(kc_rows$l3))
          l3 <- l3[!is.na(l3) & nzchar(l3)]
          if (length(l3) > 0) return(l3)
        }
      }
      # Source 2: project KEGG names table
      path_raw <- NA_character_
      if (!is.null(ko_names_df) && ko %in% rownames(ko_names_df))
        path_raw <- as.character(ko_names_df[ko, "Path"])
      # Source 3: ORF table KEGGPATH
      if ((is.na(path_raw) || !nzchar(path_raw)) &&
          !is.null(orf_ko_paths) && ko %in% names(orf_ko_paths))
        path_raw <- as.character(orf_ko_paths[[ko]])
      if (is.na(path_raw) || !nzchar(path_raw)) return(character(0))
      blocks <- strsplit(path_raw, " | ", fixed = TRUE)[[1]]
      l3 <- unique(vapply(blocks, function(b) {
        parts <- strsplit(trimws(b), "; ", fixed = TRUE)[[1]]
        if (length(parts) >= 3) trimws(parts[[3]]) else NA_character_
      }, character(1)))
      l3[!is.na(l3) & nzchar(l3)]
    }

    # Build per-KO rows — l3 strictly filtered to the paths of this category.
    # KOs with no matching pathway are grouped under the category name itself,
    # NEVER under unrelated KEGG pathways.
    cat_paths   <- MAG_MAP_CATEGORIES[[cat_key[1]]]$paths
    cat_label   <- gsub("\n", " ", cat_key[1])
    rows <- lapply(cat_kos, function(ko) {
      all_l3 <- ko_l3_fn(ko)
      # Keep ONLY pathways that belong to this category
      l3 <- all_l3[all_l3 %in% cat_paths]
      list(
        ko      = ko,
        name    = ko_name_fn(ko),
        l3      = l3,
        present = ko %in% mag_kos
      )
    })

    # Collect all L3s, sort (real pathways first)
    all_l3 <- sort(unique(unlist(lapply(rows, `[[`, "l3"))))
    no_path <- vapply(rows, function(r) length(r$l3) == 0, logical(1))
    has_unassigned <- any(no_path)

    cat_display <- gsub("\n", " ", cat_key[1])
    n_present   <- sum(vapply(rows, `[[`, logical(1), "present"))
    n_total     <- length(rows)

    # Build grouped HTML
    make_ko_row <- function(r) {
      icon  <- if (r$present) "✓" else "✗"
      color <- if (r$present) "#1a7a3a" else "#c0392b"
      bg    <- if (r$present) "#f0fff4" else "#fff5f5"
      sprintf(
        '<tr style="background:%s;">
          <td style="padding:3px 8px 3px 22px;font-family:monospace;font-size:0.8rem;white-space:nowrap;width:80px;">%s</td>
          <td style="padding:3px 8px;font-size:0.79rem;">%s</td>
          <td style="padding:3px 12px;text-align:center;font-size:1rem;color:%s;width:40px;">%s</td>
        </tr>',
        bg,
        htmltools::htmlEscape(r$ko),
        htmltools::htmlEscape(r$name),
        color, icon)
    }

    # Lookup pathway ID from name via KEGG_HIERARCHY
    find_pid <- function(path_name) {
      tryCatch({
        for (l1 in KEGG_HIERARCHY) for (l2 in l1) for (pw in l2)
          if (identical(pw$name, path_name)) return(pw$id)
        NULL
      }, error = function(e) NULL)
    }

    make_l3_header <- function(label) {
      pid_val <- find_pid(label)
      if (!is.null(pid_val)) {
        # Clickable: sends pid to Shiny via magmap_pw_clicked
        sprintf(
          '<tr style="background:#e8edf5; cursor:pointer;" onclick="Shiny.setInputValue(\'magmap_pw_clicked\',{pid:\'%s\',name:\'%s\',ts:Date.now()},{priority:\'event\'})">
            <td colspan="3" style="padding:5px 8px;font-weight:600;font-size:0.81rem;color:#1a5598;letter-spacing:0.01em;text-decoration:underline dotted;">%s &#x2197;</td>
          </tr>',
          htmltools::htmlEscape(pid_val, attribute = TRUE),
          htmltools::htmlEscape(label, attribute = TRUE),
          htmltools::htmlEscape(label))
      } else {
        sprintf(
          '<tr style="background:#e8edf5;"><td colspan="3" style="padding:5px 8px;font-weight:600;font-size:0.81rem;color:#1a3a6b;letter-spacing:0.01em;">%s</td></tr>',
          htmltools::htmlEscape(label))
      }
    }

    tbl_html <- ""
    for (l3 in all_l3) {
      grp <- Filter(function(r) l3 %in% r$l3, rows)
      if (length(grp) == 0) next
      tbl_html <- paste0(tbl_html, make_l3_header(l3),
                         paste(vapply(grp, make_ko_row, character(1)), collapse = ""))
    }
    if (has_unassigned) {
      grp <- rows[no_path]
      tbl_html <- paste0(tbl_html,
                         make_l3_header(paste0(cat_label, " (other KOs)")),
                         paste(vapply(grp, make_ko_row, character(1)), collapse = ""))
    }

    card(
      style = "margin-top:12px;",
      card_header(
        tags$span(style = "font-weight:600;", cat_display),
        tags$span(style = "margin-left:12px; font-size:0.82rem; color:var(--muted);",
          sprintf("%d / %d KOs present (%.0f%%)", n_present, n_total,
                  100 * n_present / max(n_total, 1)))
      ),
      card_body(class = "p-0",
        tags$div(
          style = "max-height:380px; overflow-y:auto;",
          tags$table(
            style = "width:100%; border-collapse:collapse;",
            tags$thead(
              tags$tr(style = "background:#d0d8ea; position:sticky; top:0; z-index:1;",
                tags$th(style = "padding:5px 8px 5px 22px; text-align:left; font-size:0.81rem;", "KO"),
                tags$th(style = "padding:5px 8px; text-align:left; font-size:0.81rem;", "Name"),
                tags$th(style = "padding:5px 12px; text-align:center; font-size:0.81rem; width:40px;", "Present")
              )
            ),
            tags$tbody(HTML(tbl_html))
          )
        )
      )
    )
  })

  # Category selector for the comparative view — grouped like the cell figure
  output$magmap_comp_category_ui <- renderUI({
    # Section → category names (must match keys in MAG_MAP_CATEGORIES)
    sections <- list(
      "Central Carbon Metabolism" = c("Glycolysis", "Pentose Phosphate\nPathway",
        "Entner-Doudoroff\nPathway", "TCA Cycle", "CO2 Fixation", "Fermentation"),
      "N, S and CH4 Metabolism"   = c("Nitrogen\nFixation", "Assimilatory N",
        "Denitrification", "Sulfur Cycle", "Nitrification", "Methane\nMetabolism"),
      "Biosynthesis / Anabolism"  = c("Amino Acids", "Nucleotides",
        "Vitamins /\nCofactors", "Fatty Acids", "Cell Wall"),
      "Respiration / Energy"      = c("ETC", "ATP Synthase",
        "Oxidative\nPhosphorylation", "Anaerobic\nRespiration", "Photosynthesis"),
      "Transporters / Systems"    = c("ABC\nTransporters", "Sec / Tat\nSystems",
        "Efflux\nPumps", "Motility", "CRISPR", "Stress\nResponse")
    )
    # Build a nested choices list: only keep categories that exist and have a
    # central KO list. selectInput renders nested lists as <optgroup>s.
    grouped <- list()
    for (sec in names(sections)) {
      cats <- sections[[sec]]
      cats <- cats[vapply(cats, function(cn)
        !is.null(MAG_MAP_CATEGORIES[[cn]]) &&
        length(MAG_MAP_CATEGORIES[[cn]]$kos_central) > 0, logical(1))]
      if (length(cats) == 0) next
      grouped[[sec]] <- setNames(as.list(cats), gsub("\n", " ", cats))
    }
    first_cat <- if (length(grouped) > 0) grouped[[1]][[1]] else NULL
    selectInput("magmap_comp_category", NULL, choices = grouped,
                selected = first_cat)
  })

  # ── KEGG pathway tree for the comparative view ──────────────────────────────
  output$magmap_comp_kegg_ui <- renderUI({
    if (is.null(sqm_data()))
      return(tags$div(style = "font-size:0.78rem; color:var(--muted); padding:4px 0;",
        "Load a project to browse pathways."))
    search_box <- tags$input(
      id = "magmap_comp_search", type = "text",
      placeholder = "Search pathway\u2026",
      oninput = "filterMagmapCompTree(this.value)",
      style = paste0(
        "width:100%; box-sizing:border-box; padding:3px 6px;",
        "font-size:0.78rem; border:1px solid var(--border);",
        "border-radius:4px; margin-bottom:6px;",
        "background:var(--surface); color:var(--text);"))
    KEGG_HIERARCHY_EXCL_L2 <- "1.0 Global and overview maps"
    tree_items <- lapply(names(KEGG_HIERARCHY), function(l1) {
      l2_items <- lapply(names(KEGG_HIERARCHY[[l1]]), function(l2) {
        if (l2 %in% KEGG_HIERARCHY_EXCL_L2) return(NULL)
        pathways <- KEGG_HIERARCHY[[l1]][[l2]]
        pw_links <- lapply(pathways, function(pw) {
          tags$div(
            class = "mcpw-item",
            "data-name" = tolower(paste(pw$name, pw$id)),
            style = "padding:2px 4px 2px 8px; cursor:pointer; font-size:0.75rem; border-radius:3px;",
            onclick = sprintf(
              "event.stopPropagation(); Shiny.setInputValue('magmap_comp_pw',{pid:'%s',name:'%s',ts:Date.now()},{priority:'event'}); document.querySelectorAll('.mcpw-item').forEach(function(el){el.style.background=''}); this.style.background='var(--accent-light)';",
              pw$id, gsub("'", "\\\\'", pw$name)),
            tags$span(style="color:var(--muted); margin-right:4px; font-family:monospace;", pw$id),
            pw$name
          )
        })
        tags$details(style = "margin-left:8px;",
          tags$summary(
            style = "font-size:0.75rem; font-weight:600; color:var(--muted); cursor:pointer; padding:2px 2px; list-style:none; display:flex; align-items:center; gap:4px;",
            tags$span(class="mcpw-chevron", style="font-size:0.6rem;", "\u25b6"), l2),
          pw_links)
      })
      l2_items <- Filter(Negate(is.null), l2_items)
      tags$details(open = NA, style = "margin-bottom:2px;",
        tags$summary(
          style = "font-size:0.8rem; font-weight:700; color:var(--text); cursor:pointer; padding:3px 2px; list-style:none; display:flex; align-items:center; gap:4px; border-bottom:1px solid var(--border);",
          tags$span(class="mcpw-chevron", style="font-size:0.65rem;", "\u25b6"), l1),
        l2_items)
    })
    tags$div(
      tags$style(HTML(
        "details[open] > summary .mcpw-chevron { transform: rotate(90deg); }
         .mcpw-item:hover { background: var(--accent-light) !important; }")),
      tags$script(HTML(
        "function filterMagmapCompTree(q) {
          q = q.toLowerCase().trim();
          document.querySelectorAll('.mcpw-item').forEach(function(el) {
            var m = !q || el.getAttribute('data-name').includes(q);
            el.style.display = m ? '' : 'none';
          });
          document.querySelectorAll('#magmap_comp_tree details').forEach(function(d) {
            var vis = Array.from(d.querySelectorAll('.mcpw-item')).some(function(el) {
              return el.style.display !== 'none'; });
            d.style.display = vis ? '' : 'none';
            if (q && vis) d.open = true;
          });
        }")),
      search_box,
      tags$div(id = "magmap_comp_tree",
        style = "max-height:320px; overflow-y:auto; border:1px solid var(--border); border-radius:4px; padding:4px;",
        tree_items)
    )
  })

  # Helper: KOs of a KEGG pathway, from KEGG_CATEGORIES (matched by L3 name)
  magmap_pathway_kos <- function(pw_name) {
    if (!exists("KEGG_CATEGORIES") || is.null(KEGG_CATEGORIES)) return(character(0))
    unique(KEGG_CATEGORIES$id[
      !is.na(KEGG_CATEGORIES$l3) &
      KEGG_CATEGORIES$l3 == pw_name &
      grepl("^K[0-9]{5}$", KEGG_CATEGORIES$id)
    ])
  }

  # ── Comparative table: presence/absence across all MAGs ─────────────────────
  # Rows = genes (KOs), Columns = MAGs
  output$magmap_comparative_ui <- renderUI({
    req(input$magmap_view_select == "comparative")
    proj <- sqm_data()
    if (is.null(proj))
      return(tags$div(style = "padding:2rem; color:var(--muted); font-size:0.85rem;",
        "Load a SQM project with binning first."))
    bins <- tryCatch(rownames(proj$bins$table), error = function(e) NULL)
    if (is.null(bins) || length(bins) == 0)
      return(tags$div(style = "padding:2rem; color:var(--muted); font-size:0.85rem;",
        "No MAGs found in this project."))

    comp_mode <- input$magmap_comp_mode %||% "category"

    # Resolve the KO set + label depending on the mode
    if (comp_mode == "pathway") {
      pw <- input$magmap_comp_pw
      if (is.null(pw) || is.null(pw$pid))
        return(tags$div(style = "padding:2rem; color:var(--muted); font-size:0.85rem;",
          "Pick a KEGG pathway from the tree on the left."))
      cat_kos   <- sort(magmap_pathway_kos(pw$name))
      cat_label <- paste0(pw$name, " [", pw$pid, "]")
      if (length(cat_kos) == 0)
        return(tags$div(style = "padding:2rem; color:var(--muted); font-size:0.85rem;",
          paste0("No KOs found for pathway ", pw$pid, ".")))
    } else {
      cat_name <- input$magmap_comp_category
      req(cat_name, cat_name %in% names(MAG_MAP_CATEGORIES))
      cat_kos   <- sort(unique(MAG_MAP_CATEGORIES[[cat_name]]$kos_central))
      cat_label <- gsub("\n", " ", cat_name)
      if (length(cat_kos) == 0)
        return(tags$div(style = "padding:2rem; color:var(--muted); font-size:0.85rem;",
          "No central KO list for this category."))
    }

    # KO sets per MAG (computed once)
    bin_kos <- lapply(bins, function(b)
      tryCatch(get_bin_kos(proj, b), error = function(e) character(0)))
    names(bin_kos) <- bins

    short_bin <- function(b) sub("\\.fa.*$", "", b)

    # KO function-name lookup
    ko_label <- function(ko) {
      if (exists("KEGG_NAMES") && !is.null(KEGG_NAMES) && ko %in% names(KEGG_NAMES)) {
        nm <- as.character(KEGG_NAMES[[ko]])
        if (length(nm) && !is.na(nm) && nzchar(nm)) return(nm)
      }
      ""
    }

    # Header: one column per MAG (vertical labels), fixed width to match cells
    header_cells <- paste0(
      '<th title="', vapply(bins, function(b) htmltools::htmlEscape(b, attribute = TRUE), character(1)),
      '" style="padding:0; width:22px; min-width:22px; max-width:22px; font-size:0.66rem; ',
      'writing-mode:vertical-rl; text-orientation:mixed; white-space:nowrap; ',
      'border-bottom:1px solid var(--border); height:120px; vertical-align:bottom; text-align:center;">',
      vapply(bins, function(b) htmltools::htmlEscape(short_bin(b)), character(1)),
      '</th>', collapse = "")

    # One row per gene (KO)
    body_rows <- vapply(cat_kos, function(ko) {
      fn_name  <- ko_label(ko)
      fn_full  <- if (nzchar(fn_name)) fn_name else "(no annotation available)"
      ko_tip   <- htmltools::htmlEscape(paste0(ko, " — ", fn_full), attribute = TRUE)
      cells <- paste0(
        vapply(bins, function(b) {
          is_pres <- ko %in% bin_kos[[b]]
          status  <- if (is_pres) "present" else "absent"
          tip <- htmltools::htmlEscape(
            paste0(ko, " — ", fn_full, "\n", short_bin(b), "\n", status),
            attribute = TRUE)
          bg  <- if (is_pres) "#c0392b" else "#f0f0f0"
          sprintf('<td title="%s" style="background:%s; border:1px solid #fff; width:22px; min-width:22px; max-width:22px; height:18px; padding:0;"></td>',
                  tip, bg)
        }, character(1)),
        collapse = "")
      n_pres  <- sum(vapply(bins, function(b) ko %in% bin_kos[[b]], logical(1)))
      sprintf(
        '<tr><td title="%s" style="padding:2px 6px; font-size:0.72rem; font-family:monospace; white-space:nowrap;">%s</td><td title="%s" style="padding:2px 8px; font-size:0.72rem; white-space:nowrap; max-width:240px; overflow:hidden; text-overflow:ellipsis;">%s</td>%s<td style="padding:2px 8px; font-size:0.72rem; color:var(--muted); text-align:center;">%d/%d</td></tr>',
        ko_tip, htmltools::htmlEscape(ko),
        ko_tip, htmltools::htmlEscape(fn_name),
        cells, n_pres, length(bins))
    }, character(1))

    tags$div(style = "padding:8px 4px;",
      tags$div(style = "font-weight:600; font-size:1rem; margin-bottom:4px; color:#1a3a6b;",
        cat_label,
        tags$span(style = "font-weight:400; color:var(--muted); font-size:0.82rem; margin-left:8px;",
          sprintf("%d genes \u00d7 %d MAGs (%s)", length(cat_kos), length(bins),
                  if (comp_mode == "pathway") "all pathway KOs" else "central KO set"))),
      tags$div(style = "font-size:0.8rem; color:var(--muted); margin-bottom:12px;",
        tags$span(style = "display:inline-block; width:12px; height:12px; background:#c0392b; border-radius:2px; margin-right:4px; vertical-align:middle;"),
        "Gene present  ",
        tags$span(style = "display:inline-block; width:12px; height:12px; background:#f0f0f0; border:1px solid #ccc; border-radius:2px; margin-right:4px; margin-left:10px; vertical-align:middle;"),
        "Gene absent"),
      tags$div(style = "overflow-x:auto; width:100%;",
        HTML(paste0(
          '<table style="border-collapse:collapse; table-layout:fixed; width:max-content;">',
          '<thead><tr>',
          '<th style="border-bottom:1px solid var(--border); text-align:left; padding:2px 6px;">KO</th>',
          '<th style="border-bottom:1px solid var(--border); text-align:left; padding:2px 8px;">Function</th>',
          header_cells,
          '<th style="border-bottom:1px solid var(--border); padding:2px 8px;">Present</th>',
          '</tr></thead>',
          '<tbody>', paste(body_rows, collapse = ""), '</tbody>',
          '</table>'))
      )
    )
  })

  # ── KEGG map mode: MAG selector (writes to the shared magmap_selected_bin) ──
  output$magmap_bin_select_ui2 <- renderUI({
    proj <- sqm_data()
    if (is.null(proj))
      return(tags$div(style = "font-size:0.8rem; color:var(--muted);",
        "Load a SQM project with binning first."))
    bins <- tryCatch(rownames(proj$bins$table), error = function(e) NULL)
    if (is.null(bins) || length(bins) == 0)
      return(tags$div(style = "font-size:0.8rem; color:var(--muted);",
        "No MAGs found in this project."))
    selectInput("magmap_bin2", NULL, choices = c("— select a MAG —" = "", bins),
                selected = isolate(magmap_selected_bin()) %||% "")
  })

  observeEvent(input$magmap_bin2, {
    v <- input$magmap_bin2
    magmap_selected_bin(if (nzchar(v)) v else NULL)
    # Keep the diagram-mode selector in sync
    if (!identical(isolate(input$magmap_bin), v))
      updateSelectInput(session, "magmap_bin", selected = v)
  })

  # Switching the single-MAG sub-mode resets the pathway view back to the
  # cell diagram, so returning to "Metabolism diagram" shows it fresh.
  observeEvent(input$magmap_single_mode, {
    magmap_view_mode("map")
    magmap_pw_status("idle")
  }, ignoreInit = TRUE)

  # ── KEGG map mode: collapsible KEGG pathway tree (same style as Pathways) ──
  output$magmap_kegg_select_ui <- renderUI({
    if (is.null(sqm_data()))
      return(tags$div(style = "font-size:0.78rem; color:var(--muted); padding:4px 0;",
        "Load a project to browse pathways."))

    search_box <- tags$input(
      id = "magmap_kegg_search", type = "text",
      placeholder = "Search pathway\u2026",
      oninput = "filterMagmapKeggTree(this.value)",
      style = paste0(
        "width:100%; box-sizing:border-box; padding:3px 6px;",
        "font-size:0.78rem; border:1px solid var(--border);",
        "border-radius:4px; margin-bottom:6px;",
        "background:var(--surface); color:var(--text);"))

    KEGG_HIERARCHY_EXCL_L2 <- "1.0 Global and overview maps"
    tree_items <- lapply(names(KEGG_HIERARCHY), function(l1) {
      l2_items <- lapply(names(KEGG_HIERARCHY[[l1]]), function(l2) {
        if (l2 %in% KEGG_HIERARCHY_EXCL_L2) return(NULL)
        pathways <- KEGG_HIERARCHY[[l1]][[l2]]
        pw_links <- lapply(pathways, function(pw) {
          tags$div(
            class = "mkpw-item",
            "data-name" = tolower(paste(pw$name, pw$id)),
            style = "padding:2px 4px 2px 8px; cursor:pointer; font-size:0.75rem; border-radius:3px;",
            onclick = sprintf(
              "event.stopPropagation(); Shiny.setInputValue('magmap_pw_clicked',{pid:'%s',name:'%s',ts:Date.now()},{priority:'event'}); document.querySelectorAll('.mkpw-item').forEach(function(el){el.style.background=''}); this.style.background='var(--accent-light)';",
              pw$id, gsub("'", "\\\\'", pw$name)),
            tags$span(style="color:var(--muted); margin-right:4px; font-family:monospace;", pw$id),
            pw$name
          )
        })
        tags$details(
          style = "margin-left:8px;",
          tags$summary(
            style = paste0(
              "font-size:0.75rem; font-weight:600; color:var(--muted);",
              "cursor:pointer; padding:2px 2px; list-style:none;",
              "display:flex; align-items:center; gap:4px;"),
            tags$span(class="mkpw-chevron", style="font-size:0.6rem;", "\u25b6"),
            l2
          ),
          pw_links
        )
      })
      l2_items <- Filter(Negate(is.null), l2_items)
      tags$details(
        open = NA,
        style = "margin-bottom:2px;",
        tags$summary(
          style = paste0(
            "font-size:0.8rem; font-weight:700; color:var(--text);",
            "cursor:pointer; padding:3px 2px; list-style:none;",
            "display:flex; align-items:center; gap:4px;",
            "border-bottom:1px solid var(--border);"),
          tags$span(class="mkpw-chevron", style="font-size:0.65rem;", "\u25b6"),
          l1
        ),
        l2_items
      )
    })

    tree_css <- tags$style(HTML(
      "details[open] > summary .mkpw-chevron { transform: rotate(90deg); }
       .mkpw-item:hover { background: var(--accent-light) !important; }"
    ))
    search_js <- tags$script(HTML(
      "function filterMagmapKeggTree(q) {
        q = q.toLowerCase().trim();
        document.querySelectorAll('.mkpw-item').forEach(function(el) {
          var match = !q || el.getAttribute('data-name').includes(q);
          el.style.display = match ? '' : 'none';
        });
        document.querySelectorAll('#magmap_kegg_tree details').forEach(function(d) {
          var vis = Array.from(d.querySelectorAll('.mkpw-item')).some(function(el) {
            return el.style.display !== 'none';
          });
          d.style.display = vis ? '' : 'none';
          if (q && vis) d.open = true;
        });
      }"
    ))

    tags$div(
      tree_css, search_js, search_box,
      tags$div(id = "magmap_kegg_tree",
        style = "max-height:340px; overflow-y:auto; border:1px solid var(--border); border-radius:4px; padding:4px;",
        tree_items)
    )
  })

  # ── KEGG map mode: the rendered pathway view (reuses the pathview machinery) ──
  output$magmap_keggmap_view_ui <- renderUI({
    bin <- magmap_selected_bin()
    if (is.null(bin))
      return(tags$div(style = "padding:2rem; color:var(--muted); font-size:0.85rem; text-align:center;",
        "Select a MAG and a KEGG pathway from the panel on the left."))
    s <- magmap_pw_status()
    if (s == "idle" || is.null(magmap_pw_pid()))
      return(tags$div(style = "padding:2rem; color:var(--muted); font-size:0.85rem; text-align:center;",
        "Pick a KEGG pathway from the tree on the left to render it."))
    if (s == "generating")
      return(tags$div(style = "color:var(--muted); font-size:0.85rem; padding:2rem; text-align:center;",
        tags$div(style = "font-size:1.5rem; margin-bottom:8px;", "\u25cc"),
        tags$div("Generating pathway map for ",
                 tags$strong(magmap_pw_name() %||% magmap_pw_pid()), "\u2026")))
    if (s == "error")
      return(tags$div(style = "color:#c0392b; font-size:0.85rem; padding:2rem; text-align:center;",
        tags$div(style = "font-size:1.5rem;", "\u2715"),
        tags$div("Pathway generation failed.")))
    # s == "ready": reuse the exact same renderer as the single-MAG pathway view
    magmap_render_pathway_view()
  })

  observeEvent(input$magmap_pw_clicked, {
    v <- input$magmap_pw_clicked
    req(!is.null(v), nzchar(v$pid))
    pid  <- v$pid
    name <- v$name
    bin  <- magmap_selected_bin(); req(bin)
    proj <- sqm_data(); req(proj)

    if (!requireNamespace("pathview", quietly = TRUE)) {
      showNotification("pathview not installed. Run: BiocManager::install(\"pathview\")",
                       type = "error", duration = 10)
      return()
    }

    magmap_pw_status("generating")
    magmap_pw_pid(pid)
    magmap_pw_name(name)
    magmap_pw_img(NULL)
    magmap_pw_nodes(NULL)
    magmap_view_mode("pathway")

    shinyjs::delay(50, tryCatch({
      mag_kos <- tryCatch(get_bin_kos(proj, bin), error = function(e) character(0))

      dir.create(pw_kegg_cache, showWarnings = FALSE, recursive = TRUE)

      # Download KEGG PNG and XML if not cached
      png_cached <- file.path(pw_kegg_cache, paste0("ko", pid, ".png"))
      xml_cached <- file.path(pw_kegg_cache, paste0("ko", pid, ".xml"))
      if (!file.exists(png_cached))
        tryCatch(pathview::download.kegg(pathway.id = pid, species = "ko",
                                         kegg.dir = pw_kegg_cache, file.type = "png"),
                 error = function(e) message("PNG download failed: ", e$message))
      if (!file.exists(xml_cached))
        .ensure_valid_xml(pid, pw_kegg_cache)

      if (!file.exists(png_cached)) {
        showNotification(paste("Could not download KEGG image for", pid),
                         type = "error", duration = 8)
        magmap_pw_status("error"); return()
      }

      # Parse KGML for node positions — same approach as server_pathways.R
      # Coordinates are pre-scaled in R so JS only needs offsetWidth/naturalWidth
      xml_nodes <- tryCatch({
        req(file.exists(xml_cached))
        doc <- xml2::read_xml(xml_cached)
        # scale_x/y = 1 because we show the original KEGG PNG (no pathview re-render)
        scale_x <- 1; scale_y <- 1
        entries <- xml2::xml_find_all(doc, ".//entry[@type='ortholog']")
        all_rows <- Filter(Negate(is.null), lapply(entries, function(e) {
          ko_names <- trimws(xml2::xml_attr(e, "name"))
          g <- xml2::xml_find_first(e, "graphics")
          if (is.na(xml2::xml_attr(g, "x"))) return(NULL)
          x <- as.numeric(xml2::xml_attr(g, "x")) * scale_x
          y <- as.numeric(xml2::xml_attr(g, "y")) * scale_y
          w <- as.numeric(xml2::xml_attr(g, "width"))  * scale_x
          h <- as.numeric(xml2::xml_attr(g, "height")) * scale_y
          if (anyNA(c(x, y, w, h))) return(NULL)
          label <- xml2::xml_attr(g, "name")
          list(ko_names = ko_names, x = x, y = y, w = w, h = h, label = label)
        }))
        if (length(all_rows) == 0) return(NULL)
        df <- data.frame(
          ko_names = sapply(all_rows, `[[`, "ko_names"),
          x = sapply(all_rows, `[[`, "x"), y = sapply(all_rows, `[[`, "y"),
          w = sapply(all_rows, `[[`, "w"), h = sapply(all_rows, `[[`, "h"),
          label = sapply(all_rows, `[[`, "label"),
          stringsAsFactors = FALSE
        )
        df[!duplicated(paste(round(df$x), round(df$y), sep = ",")), ]
      }, error = function(e) { message("MAG map XML parse error: ", e$message); NULL })

      magmap_pw_nodes(xml_nodes)
      magmap_pw_img(png_cached)
      magmap_pw_status("ready")

    }, error = function(e) {
      message("MAG map pathview error: ", e$message)
      magmap_pw_status("error")
      showNotification(paste("Pathway error:", e$message), type = "error", duration = 10)
    }))
  })

  # ── Back button: return to map view ─────────────────────────────────────────
  observeEvent(input$magmap_back, {
    magmap_view_mode("map")
    magmap_pw_status("idle")
    magmap_pw_img(NULL)
    magmap_pw_nodes(NULL)
  })




