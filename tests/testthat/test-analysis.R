test_that("analisis keracunan pangan menghasilkan struktur lengkap", {
  contoh <- contoh_keracunan_pangan(n = 120, seed = 42)
  hasil <- analisis_klb(contoh$data, contoh$pemetaan, contoh$konfigurasi,
                        jenis = "keracunan_pangan")

  expect_s3_class(hasil, "klb_hasil")
  expect_gt(hasil$ringkasan$total_kasus, 0)
  expect_equal(hasil$ringkasan$total_diinvestigasi, 120)
  expect_true(hasil$ringkasan$attack_rate > 0 && hasil$ringkasan$attack_rate <= 100)
  expect_true(nrow(hasil$gejala) > 0)
  expect_true(nrow(hasil$ar_umur) > 0)
  expect_true(nrow(hasil$paparan) == length(contoh$pemetaan$makanan$kolom))
  expect_true(nrow(hasil$kriteria_klb) == 7)
  expect_true(nrow(hasil$diagnosis_banding) > 0)
})

test_that("attack rate dihitung sesuai definisi", {
  df <- data.frame(
    sakit = c(1, 1, 0, 0, 1, NA),
    meninggal = c(0, 1, 0, 0, 0, 0),
    jk = c("L", "L", "P", "P", "P", "L"),
    stringsAsFactors = FALSE
  )
  tab <- tabel_attack_rate(df, "jk")
  laki <- tab[tab$kategori == "L", ]
  expect_equal(laki$populasi, 2)
  expect_equal(laki$kasus, 2)
  expect_equal(laki$ar, 100)
  expect_equal(laki$cfr, 50)
})

test_that("risk ratio dan selang kepercayaan sesuai rumus Katz", {
  # a = 40, b = 10, c = 20, d = 30
  paparan <- c(rep(1, 50), rep(0, 50))
  sakit <- c(rep(1, 40), rep(0, 10), rep(1, 20), rep(0, 30))
  hasil <- analisis_2x2(paparan, sakit, "kohort")

  rr_harapan <- (40 / 50) / (20 / 50)
  se <- sqrt(1 / 40 - 1 / 50 + 1 / 20 - 1 / 50)
  expect_equal(hasil$estimasi, rr_harapan, tolerance = 1e-8)
  expect_equal(hasil$ci_bawah, exp(log(rr_harapan) - 1.96 * se), tolerance = 1e-8)
  expect_equal(hasil$ukuran, "RR")
  expect_equal(hasil$uji, "chi-square")
})

test_that("odds ratio dipakai pada desain kasus kontrol", {
  paparan <- c(rep(1, 50), rep(0, 50))
  sakit <- c(rep(1, 40), rep(0, 10), rep(1, 20), rep(0, 30))
  hasil <- analisis_2x2(paparan, sakit, "kasus_kontrol")
  expect_equal(hasil$ukuran, "OR")
  expect_equal(hasil$estimasi, (40 * 30) / (10 * 20), tolerance = 1e-8)
})

test_that("sel nol memicu koreksi Haldane-Anscombe", {
  paparan <- c(rep(1, 20), rep(0, 20))
  sakit <- c(rep(1, 20), rep(0, 20))
  hasil <- analisis_2x2(paparan, sakit, "kohort")
  expect_true(hasil$koreksi)
  expect_true(is.finite(hasil$estimasi))
  expect_true(is.finite(hasil$ci_bawah))
})

test_that("frekuensi harapan kecil memakai uji Fisher exact", {
  paparan <- c(rep(1, 8), rep(0, 8))
  sakit <- c(rep(1, 7), 0, rep(0, 7), 1)
  hasil <- analisis_2x2(paparan, sakit, "kohort")
  expect_equal(hasil$uji, "Fisher exact")
})

test_that("interval kurva epidemik mengikuti seperempat masa inkubasi", {
  contoh <- contoh_keracunan_pangan(n = 100, seed = 7)
  hasil <- analisis_klb(contoh$data, contoh$pemetaan, contoh$konfigurasi)
  expect_equal(hasil$kurva$interval_jam, hasil$inkubasi$rata_jam / 4, tolerance = 1e-8)
  expect_equal(hasil$kurva$tipe, "common_source")
})

test_that("kurva propagated source dikenali pada KLB pertusis", {
  contoh <- contoh_pertusis(seed = 11)
  hasil <- analisis_klb(contoh$data, contoh$pemetaan, contoh$konfigurasi, jenis = "pd3i")
  expect_equal(hasil$kurva$tipe, "propagated_source")
  expect_null(hasil$diagnosis_banding)
  expect_true("Status imunisasi" %in% names(hasil$ar_lain))
})

test_that("penyingkiran diagnosis banding mengikuti aturan masa inkubasi", {
  inkubasi <- list(n = 10, min_jam = 1, maks_jam = 8, rata_jam = 4, median_jam = 4)
  gejala <- data.frame(kategori = c("Mual", "Muntah"), n = c(9, 8), persen = c(90, 80),
                       stringsAsFactors = FALSE)
  hasil <- diagnosis_banding(gejala, inkubasi, periode_klb_jam = 7)

  klebsiella <- hasil[hasil$agen == "Klebsiella pneumoniae", ]
  expect_equal(klebsiella$status, "disingkirkan")
  expect_match(klebsiella$alasan, "lebih pendek")
})
