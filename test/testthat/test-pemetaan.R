test_that("pemetaan wajib dinilai sesuai jenis KLB", {
  kosong <- periksa_pemetaan("keracunan_pangan", list())
  expect_false(kosong$lengkap)
  expect_true("makanan" %in% kosong$wajib_kurang$key)

  contoh <- contoh_keracunan_pangan(n = 20)
  lengkap <- periksa_pemetaan("keracunan_pangan", contoh$pemetaan)
  expect_true(lengkap$lengkap)
  expect_true("Kurva epidemik" %in% lengkap$analisis_tersedia)
})

test_that("pangan wajib untuk keracunan pangan tetapi tidak untuk pertusis", {
  expect_true("makanan" %in% peran_wajib("keracunan_pangan")$key)
  expect_false("makanan" %in% peran_wajib("pd3i")$key)
  expect_true("status_imunisasi" %in% peran_wajib("pd3i")$key)
})

test_that("nilai positif yang belum ditentukan menghasilkan peringatan", {
  pemetaan <- list(status_sakit = list(kolom = "sakit"))
  hasil <- periksa_pemetaan("penyakit_menular", pemetaan)
  expect_true(any(grepl("berarti sakit", hasil$peringatan)))
})

test_that("waktu diurai dari beberapa format", {
  hasil <- urai_waktu(c("2024-05-16T17:00:00", "16/05/2024", "2024-05-16"),
                      c(NA, "18.30", NA))
  expect_length(hasil, 3)
  expect_equal(format(hasil[1], "%H:%M"), "17:00")
  expect_equal(format(hasil[2], "%H:%M"), "18:30")
})

test_that("katalog model diterima dalam bentuk data.frame maupun daftar", {
  # Shiny menyederhanakan larik objek JSON menjadi data.frame
  df <- normalkan_katalog(data.frame(id = c("A", "B"), vram = c(1024, 2048),
                                     stringsAsFactors = FALSE))
  expect_equal(nrow(df), 2)
  expect_equal(df$id, c("A", "B"))

  # Bentuk daftar tetap harus diterima
  lst <- normalkan_katalog(list(list(id = "A", vram = 1024), list(id = "B")))
  expect_equal(lst$id, c("A", "B"))
  expect_true(is.na(lst$vram[2]))

  # Bentuk yang benar-benar dikirim jembatan: dua larik sejajar
  sejajar <- normalkan_katalog(list(id = c("A-MLC", "B-MLC"), vram = c(879, 2264)))
  expect_equal(sejajar$id, c("A-MLC", "B-MLC"))
  expect_equal(sejajar$vram, c(879, 2264))

  # Bentuk lama yang pernah muncul: vektor rata berselang-seling
  rata <- normalkan_katalog(c("A-MLC", "879.04", "B-MLC", "2263.69"))
  expect_equal(rata$id, c("A-MLC", "B-MLC"))
  expect_equal(round(rata$vram), c(879, 2264))

  expect_equal(nrow(normalkan_katalog(NULL)), 0)
  expect_equal(nrow(normalkan_katalog(list())), 0)
})
