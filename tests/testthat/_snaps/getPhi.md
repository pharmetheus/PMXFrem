# getPhi handles all logic branches

    Code
      getPhi(file_1_table)
    Output
        ID ETA1
      1  1  1.1

---

    Code
      getPhi(file_2_tables, set = 1)
    Output
        ID ETA1
      1  1  1.1

---

    Code
      getPhi(file_3_tables, set = 2)
    Output
        ID ETA1
      1  1  2.2

---

    Code
      getPhi(file_4_tables, set = 4)
    Output
        ID ETA1
      1  1  4.4

---

    Code
      getPhi(file_4_tables)
    Output
        ID ETA1
      1  1  4.4

