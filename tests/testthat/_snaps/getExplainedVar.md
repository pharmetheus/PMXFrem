# getExplainedVar works

    structure(list(AGE = c(1, 1, -99, -99, -99, -99, -99, -99, -99, 
    -99, -99, -99, -99, -99, -99, -99, -99), ALT = c(1, -99, 1, -99, 
    -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99
    ), AST = c(1, -99, -99, 1, -99, -99, -99, -99, -99, -99, -99, 
    -99, -99, -99, -99, -99, -99), BILI = c(1, -99, -99, -99, 1, 
    -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99), 
        BMI = c(1, -99, -99, -99, -99, 1, -99, -99, -99, -99, -99, 
        -99, -99, -99, -99, -99, -99), BSA = c(1, -99, -99, -99, 
        -99, -99, 1, -99, -99, -99, -99, -99, -99, -99, -99, -99, 
        -99), CRCL = c(1, -99, -99, -99, -99, -99, -99, 1, -99, -99, 
        -99, -99, -99, -99, -99, -99, -99), ETHNIC = c(1, -99, -99, 
        -99, -99, -99, -99, -99, 1, -99, -99, -99, -99, -99, -99, 
        -99, -99), GENO2 = c(1, -99, -99, -99, -99, -99, -99, -99, 
        -99, 1, -99, -99, -99, -99, -99, -99, -99), HT = c(1, -99, 
        -99, -99, -99, -99, -99, -99, -99, -99, 1, -99, -99, -99, 
        -99, -99, -99), LBWT = c(1, -99, -99, -99, -99, -99, -99, 
        -99, -99, -99, -99, 1, -99, -99, -99, -99, -99), NCIL = c(1, 
        -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, 1, 
        -99, -99, -99, -99), RACEL = c(1, -99, -99, -99, -99, -99, 
        -99, -99, -99, -99, -99, -99, -99, 1, -99, -99, -99), SEX = c(1, 
        -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, 
        -99, 1, -99, -99), SMOK = c(1, -99, -99, -99, -99, -99, -99, 
        -99, -99, -99, -99, -99, -99, -99, -99, 1, -99), WT = c(1, 
        -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, 
        -99, -99, -99, 1)), row.names = c(NA, -17L), class = "data.frame")

---

    structure(list(COVNUM = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 
    10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 1L, 2L, 3L, 4L, 5L, 6L, 
    7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L), COVNAME = c("All", 
    "AGE", "ALT", "AST", "BILI", "BMI", "BSA", "CRCL", "ETHNIC", 
    "GENO2", "HT", "LBWT", "NCIL", "RACEL", "SEX", "SMOK", "WT", 
    "All", "AGE", "ALT", "AST", "BILI", "BMI", "BSA", "CRCL", "ETHNIC", 
    "GENO2", "HT", "LBWT", "NCIL", "RACEL", "SEX", "SMOK", "WT"), 
        PARAMETER = c("CL", "CL", "CL", "CL", "CL", "CL", "CL", "CL", 
        "CL", "CL", "CL", "CL", "CL", "CL", "CL", "CL", "CL", "V", 
        "V", "V", "V", "V", "V", "V", "V", "V", "V", "V", "V", "V", 
        "V", "V", "V", "V"), TOTVAR = c(6.92700924262584, 6.92700924262584, 
        6.92700924262584, 6.92700924262584, 6.92700924262584, 6.92700924262584, 
        6.92700924262584, 6.92700924262584, 6.92700924262584, 6.92700924262584, 
        6.92700924262584, 6.92700924262584, 6.92700924262584, 6.92700924262584, 
        6.92700924262584, 6.92700924262584, 6.92700924262584, 2953.42969380271, 
        2953.42969380271, 2953.42969380271, 2953.42969380271, 2953.42969380271, 
        2953.42969380271, 2953.42969380271, 2953.42969380271, 2953.42969380271, 
        2953.42969380271, 2953.42969380271, 2953.42969380271, 2953.42969380271, 
        2953.42969380271, 2953.42969380271, 2953.42969380271, 2953.42969380271
        ), TOTCOVVAR = c(4.46552680806801, 4.46552680806801, 4.46552680806801, 
        4.46552680806801, 4.46552680806801, 4.46552680806801, 4.46552680806801, 
        4.46552680806801, 4.46552680806801, 4.46552680806801, 4.46552680806801, 
        4.46552680806801, 4.46552680806801, 4.46552680806801, 4.46552680806801, 
        4.46552680806801, 4.46552680806801, 2171.68896842711, 2171.68896842711, 
        2171.68896842711, 2171.68896842711, 2171.68896842711, 2171.68896842711, 
        2171.68896842711, 2171.68896842711, 2171.68896842711, 2171.68896842711, 
        2171.68896842711, 2171.68896842711, 2171.68896842711, 2171.68896842711, 
        2171.68896842711, 2171.68896842711, 2171.68896842711), COVVAR = c(4.46552680806801, 
        0.579913135779043, 0.577160622123797, 0.507972917668682, 
        0.502834744801922, 1.00897095554055, 0.960372511068275, 0.605584730241323, 
        0.544541183892976, 3.65661770807731, 0.53063848900904, 0.719750128945979, 
        0.491050219267062, 0.491050219267062, 0.523514245961374, 
        0.706477099421947, 1.01981077937666, 2171.68896842711, 121.174618174663, 
        66.2631636244046, 62.7039138960645, 66.5835943180437, 725.551910743545, 
        467.831532952432, 230.547623532791, 131.194370523011, 1312.9583935233, 
        47.0004755547948, 131.703607303715, 46.8275750018961, 46.8275750018961, 
        85.9858643504231, 116.10508996189, 574.157038787521)), row.names = c(NA, 
    -34L), class = "data.frame")

---

    structure(list(COVNUM = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 
    10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 1L, 2L, 3L, 4L, 5L, 6L, 
    7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L), COVNAME = c("All", 
    "AGE", "ALT", "AST", "BILI", "BMI", "BSA", "CRCL", "ETHNIC", 
    "GENO2", "HT", "LBWT", "NCIL", "RACEL", "SEX", "SMOK", "WT", 
    "All", "AGE", "ALT", "AST", "BILI", "BMI", "BSA", "CRCL", "ETHNIC", 
    "GENO2", "HT", "LBWT", "NCIL", "RACEL", "SEX", "SMOK", "WT"), 
        PARAMETER = c("CL", "CL", "CL", "CL", "CL", "CL", "CL", "CL", 
        "CL", "CL", "CL", "CL", "CL", "CL", "CL", "CL", "CL", "V", 
        "V", "V", "V", "V", "V", "V", "V", "V", "V", "V", "V", "V", 
        "V", "V", "V", "V"), TOTVAR = c(6.58566102630933, 6.58566102630933, 
        6.58566102630933, 6.58566102630933, 6.58566102630933, 6.58566102630933, 
        6.58566102630933, 6.58566102630933, 6.58566102630933, 6.58566102630933, 
        6.58566102630933, 6.58566102630933, 6.58566102630933, 6.58566102630933, 
        6.58566102630933, 6.58566102630933, 6.58566102630933, 2790.18949388472, 
        2790.18949388472, 2790.18949388472, 2790.18949388472, 2790.18949388472, 
        2790.18949388472, 2790.18949388472, 2790.18949388472, 2790.18949388472, 
        2790.18949388472, 2790.18949388472, 2790.18949388472, 2790.18949388472, 
        2790.18949388472, 2790.18949388472, 2790.18949388472, 2790.18949388472
        ), TOTCOVVAR = c(2.77073639010358, 2.77073639010358, 2.77073639010358, 
        2.77073639010358, 2.77073639010358, 2.77073639010358, 2.77073639010358, 
        2.77073639010358, 2.77073639010358, 2.77073639010358, 2.77073639010358, 
        2.77073639010358, 2.77073639010358, 2.77073639010358, 2.77073639010358, 
        2.77073639010358, 2.77073639010358, 1606.46729688097, 1606.46729688097, 
        1606.46729688097, 1606.46729688097, 1606.46729688097, 1606.46729688097, 
        1606.46729688097, 1606.46729688097, 1606.46729688097, 1606.46729688097, 
        1606.46729688097, 1606.46729688097, 1606.46729688097, 1606.46729688097, 
        1606.46729688097, 1606.46729688097, 1606.46729688097), COVVAR = c(2.77073639010358, 
        0.43175735746296, 0.402996437882061, 0.377694396153942, 0.377490369726996, 
        0.91307921167808, 0.834208656169661, 0.452984669981506, 0.411955156850903, 
        1.71938897063243, 0.405681484495717, 0.583060693622307, 0.364104606173144, 
        0.364104606173144, 0.397599615119587, 0.70261152232371, 0.917486751268712, 
        1606.46729688097, 90.7542075362117, 47.441407215965, 47.846154379001, 
        48.4418380621582, 808.358324909027, 478.174079727226, 176.234504799733, 
        113.937994882729, 577.65716689941, 36.8565134139018, 117.733652976804, 
        36.5736508923994, 36.5736508923994, 78.0154283079582, 137.036480502138, 
        632.68262856638)), row.names = c(NA, -34L), class = "data.frame")

---

    structure(list(COVNUM = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 
    10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 1L, 2L, 3L, 4L, 5L, 6L, 
    7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L), COVNAME = c("All", 
    "AGE", "ALT", "AST", "BILI", "BMI", "BSA", "CRCL", "ETHNIC", 
    "GENO2", "HT", "LBWT", "NCIL", "RACEL", "SEX", "SMOK", "WT", 
    "All", "AGE", "ALT", "AST", "BILI", "BMI", "BSA", "CRCL", "ETHNIC", 
    "GENO2", "HT", "LBWT", "NCIL", "RACEL", "SEX", "SMOK", "WT"), 
        PARAMETER = c("CL", "CL", "CL", "CL", "CL", "CL", "CL", "CL", 
        "CL", "CL", "CL", "CL", "CL", "CL", "CL", "CL", "CL", "V", 
        "V", "V", "V", "V", "V", "V", "V", "V", "V", "V", "V", "V", 
        "V", "V", "V", "V"), TOTVAR = c(8.32800241233519, 8.32800241233519, 
        8.32800241233519, 8.32800241233519, 8.32800241233519, 8.32800241233519, 
        8.32800241233519, 8.32800241233519, 8.32800241233519, 8.32800241233519, 
        8.32800241233519, 8.32800241233519, 8.32800241233519, 8.32800241233519, 
        8.32800241233519, 8.32800241233519, 8.32800241233519, 2869.3128215101, 
        2869.3128215101, 2869.3128215101, 2869.3128215101, 2869.3128215101, 
        2869.3128215101, 2869.3128215101, 2869.3128215101, 2869.3128215101, 
        2869.3128215101, 2869.3128215101, 2869.3128215101, 2869.3128215101, 
        2869.3128215101, 2869.3128215101, 2869.3128215101, 2869.3128215101
        ), TOTCOVVAR = c(2.77073639010358, 2.77073639010358, 2.77073639010358, 
        2.77073639010358, 2.77073639010358, 2.77073639010358, 2.77073639010358, 
        2.77073639010358, 2.77073639010358, 2.77073639010358, 2.77073639010358, 
        2.77073639010358, 2.77073639010358, 2.77073639010358, 2.77073639010358, 
        2.77073639010358, 2.77073639010358, 1606.46729688097, 1606.46729688097, 
        1606.46729688097, 1606.46729688097, 1606.46729688097, 1606.46729688097, 
        1606.46729688097, 1606.46729688097, 1606.46729688097, 1606.46729688097, 
        1606.46729688097, 1606.46729688097, 1606.46729688097, 1606.46729688097, 
        1606.46729688097, 1606.46729688097, 1606.46729688097), COVVAR = c(2.77073639010358, 
        0.43175735746296, 0.402996437882061, 0.377694396153942, 0.377490369726996, 
        0.91307921167808, 0.834208656169661, 0.452984669981506, 0.411955156850903, 
        1.71938897063243, 0.405681484495717, 0.583060693622307, 0.364104606173144, 
        0.364104606173144, 0.397599615119587, 0.70261152232371, 0.917486751268712, 
        1606.46729688097, 90.7542075362117, 47.441407215965, 47.846154379001, 
        48.4418380621582, 808.358324909027, 478.174079727226, 176.234504799733, 
        113.937994882729, 577.65716689941, 36.8565134139018, 117.733652976804, 
        36.5736508923994, 36.5736508923994, 78.0154283079582, 137.036480502138, 
        632.68262856638)), row.names = c(NA, -34L), class = "data.frame")

---

    structure(list(COVNUM = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 
    10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 1L, 2L, 3L, 4L, 5L, 6L, 
    7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L), COVNAME = c("All", 
    "AGE", "ALT", "AST", "BILI", "BMI", "BSA", "CRCL", "ETHNIC", 
    "GENO2", "HT", "LBWT", "NCIL", "RACEL", "SEX", "SMOK", "WT", 
    "All", "AGE", "ALT", "AST", "BILI", "BMI", "BSA", "CRCL", "ETHNIC", 
    "GENO2", "HT", "LBWT", "NCIL", "RACEL", "SEX", "SMOK", "WT"), 
        PARAMETER = c("CL", "CL", "CL", "CL", "CL", "CL", "CL", "CL", 
        "CL", "CL", "CL", "CL", "CL", "CL", "CL", "CL", "CL", "V", 
        "V", "V", "V", "V", "V", "V", "V", "V", "V", "V", "V", "V", 
        "V", "V", "V", "V"), TOTVAR = c(8.32800241233519, 8.32800241233519, 
        8.32800241233519, 8.32800241233519, 8.32800241233519, 8.32800241233519, 
        8.32800241233519, 8.32800241233519, 8.32800241233519, 8.32800241233519, 
        8.32800241233519, 8.32800241233519, 8.32800241233519, 8.32800241233519, 
        8.32800241233519, 8.32800241233519, 8.32800241233519, 2869.3128215101, 
        2869.3128215101, 2869.3128215101, 2869.3128215101, 2869.3128215101, 
        2869.3128215101, 2869.3128215101, 2869.3128215101, 2869.3128215101, 
        2869.3128215101, 2869.3128215101, 2869.3128215101, 2869.3128215101, 
        2869.3128215101, 2869.3128215101, 2869.3128215101, 2869.3128215101
        ), TOTCOVVAR = c(2.77073639010358, 2.77073639010358, 2.77073639010358, 
        2.77073639010358, 2.77073639010358, 2.77073639010358, 2.77073639010358, 
        2.77073639010358, 2.77073639010358, 2.77073639010358, 2.77073639010358, 
        2.77073639010358, 2.77073639010358, 2.77073639010358, 2.77073639010358, 
        2.77073639010358, 2.77073639010358, 1606.46729688097, 1606.46729688097, 
        1606.46729688097, 1606.46729688097, 1606.46729688097, 1606.46729688097, 
        1606.46729688097, 1606.46729688097, 1606.46729688097, 1606.46729688097, 
        1606.46729688097, 1606.46729688097, 1606.46729688097, 1606.46729688097, 
        1606.46729688097, 1606.46729688097, 1606.46729688097), COVVAR = c(2.77073639010358, 
        0.43175735746296, 0.402996437882061, 0.377694396153942, 0.377490369726996, 
        0.91307921167808, 0.834208656169661, 0.452984669981506, 0.411955156850903, 
        1.71938897063243, 0.405681484495717, 0.583060693622307, 0.364104606173144, 
        0.364104606173144, 0.397599615119587, 0.70261152232371, 0.917486751268712, 
        1606.46729688097, 90.7542075362117, 47.441407215965, 47.846154379001, 
        48.4418380621582, 808.358324909027, 478.174079727226, 176.234504799733, 
        113.937994882729, 577.65716689941, 36.8565134139018, 117.733652976804, 
        36.5736508923994, 36.5736508923994, 78.0154283079582, 137.036480502138, 
        632.68262856638)), row.names = c(NA, -34L), class = "data.frame")
