test_that("payload agregat tidak memuat data individu", {
  contoh <- contoh_keracunan_pangan(n = 80, seed = 21)
  hasil <- analisis_klb(contoh$data, contoh$pemetaan, contoh$konfigurasi)
  payload <- susun_agregat(hasil, contoh$keterangan,
    node = list(kode = "KAB-01", nama = "Dinkes", kabupaten = "Kulon Progo",
                provinsi = "D.I. Yogyakarta"),
    tambahan = list(patogen_diduga = "Proteus mirabilis"))

  teks <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  expect_false(grepl("id_kasus", teks))
  expect_false(grepl("no_responden", teks))
  expect_false(grepl("mkn_rendang", teks))
  expect_equal(payload$angka$total_kasus, hasil$ringkasan$total_kasus)
  expect_true(length(payload$distribusi$kurva_epidemik) > 0)
})

test_that("basis data node menyimpan dan membaca kembali investigasi", {
  berkas <- file.path(tempdir(), paste0("uji-", as.integer(runif(1, 1, 1e6)), ".sqlite"))
  con <- buka_db("kabupaten", berkas = berkas)
  on.exit({ DBI::dbDisconnect(con); unlink(berkas) })

  ids <- muat_data_contoh(con, tambah_pengguna = FALSE)
  expect_length(ids, 2)

  data <- baca_submission(con, ids[1])
  expect_equal(nrow(data), 209)

  pm <- baca_pemetaan(con, ids[1])
  expect_true(periksa_pemetaan("keracunan_pangan", pm$pemetaan)$lengkap)
  expect_equal(pm$konfigurasi$satuan_waktu, "jam")
})

test_that("autentikasi memakai hash dan menolak sandi salah", {
  berkas <- file.path(tempdir(), paste0("uji-auth-", as.integer(runif(1, 1, 1e6)), ".sqlite"))
  con <- buka_db("kabupaten", berkas = berkas)
  on.exit({ DBI::dbDisconnect(con); unlink(berkas) })

  buat_pengguna(con, "petugas", "sandirahasia", "Petugas Uji", "analis")
  hash <- DBI::dbGetQuery(con, "SELECT sandi_hash FROM pengguna WHERE username = 'petugas'")$sandi_hash
  expect_false(grepl("sandirahasia", hash, fixed = TRUE))

  expect_null(periksa_login(con, "petugas", "salah"))
  sesi <- periksa_login(con, "petugas", "sandirahasia")
  expect_equal(sesi$peran, "analis")
  expect_true(boleh_ubah(sesi$peran))
  expect_false(boleh_ubah("viewer"))
})

test_that("kriteria KLB dinilai dari data pembanding", {
  bins <- data.frame(mulai = Sys.time() + c(0, 3600, 7200),
                     selesai = Sys.time() + c(3600, 7200, 10800),
                     kasus = c(1, 3, 6))
  pembanding <- utils::modifyList(pembanding_kosong(),
    list(kasus_periode_sebelumnya = 2, cfr_periode_sebelumnya = 1))
  hasil <- nilai_kriteria_klb(pembanding, list(total_kasus = 10, cfr = 2), bins)

  expect_equal(nrow(hasil), 7)
  expect_true(hasil$terpenuhi[hasil$kode == "b"])
  expect_true(hasil$terpenuhi[hasil$kode == "c"])
  expect_true(hasil$terpenuhi[hasil$kode == "f"])
  expect_true(is.na(hasil$terpenuhi[hasil$kode == "g"]))
})
