test_that("draf laporan memuat seluruh bab dan angka hasil analisis", {
  contoh <- contoh_keracunan_pangan(n = 100, seed = 3)
  hasil <- analisis_klb(contoh$data, contoh$pemetaan, contoh$konfigurasi)
  naskah <- susun_draf_laporan(hasil, contoh$keterangan, metadata_laporan())

  expect_true(all(bagian_laporan()$kunci %in% names(naskah)))
  expect_match(naskah$hasil, as.character(hasil$ringkasan$total_kasus), fixed = TRUE)
  expect_match(naskah$intisari, "Latar belakang")
  expect_match(naskah$intisari, "Kesimpulan")
})

test_that("tabel laporan bernomor urut dan memuat tabel wajib", {
  contoh <- contoh_keracunan_pangan(n = 100, seed = 5)
  hasil <- analisis_klb(contoh$data, contoh$pemetaan, contoh$konfigurasi)
  tabel <- tabel_laporan(hasil, contoh$keterangan)

  expect_equal(vapply(tabel, function(t) t$nomor, numeric(1)), seq_along(tabel))
  judul <- vapply(tabel, function(t) t$judul, character(1))
  expect_true(any(grepl("tanda dan gejala", judul)))
  expect_true(any(grepl("jenis kelamin", judul)))
  expect_true(any(grepl("kelompok umur", judul)))
  expect_true(any(grepl("attack rate ratio", judul)))
  expect_true(any(grepl("Diagnosis banding", judul)))
  expect_true(any(grepl("kriteria KLB", judul)))
})

test_that("ekspor Word menghasilkan berkas", {
  skip_if_not_installed("officer")
  contoh <- contoh_keracunan_pangan(n = 60, seed = 9)
  hasil <- analisis_klb(contoh$data, contoh$pemetaan, contoh$konfigurasi)
  naskah <- susun_draf_laporan(hasil, contoh$keterangan)
  berkas <- file.path(tempdir(), "uji-laporan.docx")
  ekspor_docx(hasil, contoh$keterangan, naskah, berkas)
  expect_true(file.exists(berkas))
  expect_gt(file.size(berkas), 10000)
})

test_that("pemeriksa narasi menandai angka asing dan klaim kepastian", {
  fakta <- "- Attack rate: 78,9 persen\n- Jumlah kasus: 165"
  bersih <- periksa_narasi("Attack rate mencapai 78,9 persen pada 165 kasus.", fakta)
  expect_true(bersih$bersih)

  kotor <- periksa_narasi(
    "Attack rate 91,4 persen dan dapat dipastikan penyebabnya Proteus mirabilis.", fakta)
  expect_false(kotor$bersih)
  expect_true(91.4 %in% kotor$angka_asing)
  expect_length(kotor$kalimat_terlalu_pasti, 1)
})
