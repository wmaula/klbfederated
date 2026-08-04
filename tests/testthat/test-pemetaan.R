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
