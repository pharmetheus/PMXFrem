# getExt handles all logic branches

    Code
      getExt(file_1_table)
    Output
          ITERATION TVAL
      1 -1000000000  1.1

---

    Code
      getExt(file_2_tables, set = 1)
    Output
          ITERATION TVAL
      1 -1000000000  1.1

---

    Code
      getExt(file_2_tables, set = 2)
    Output
          ITERATION TVAL
      1 -1000000000  2.2

---

    Code
      getExt(file_3_tables, set = 2)
    Output
          ITERATION TVAL
      1 -1000000000  2.2

---

    Code
      getExt(file_3_tables, set = 3)
    Output
          ITERATION TVAL
      1 -1000000000  3.3

---

    Code
      getExt(file_4_tables, set = 4)
    Output
          ITERATION TVAL
      1 -1000000000  4.4

---

    Code
      getExt(file_4_tables)
    Output
          ITERATION TVAL
      1 -1000000000  4.4

