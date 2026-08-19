#' Data contoh KLB keracunan pangan
#'
#' Membangkitkan data sintetis berbentuk submission KoboToolbox untuk KLB
#' keracunan pangan pada suatu hajatan, lengkap dengan pemetaan variabel dan
#' konfigurasi analisis. Sumber keracunan disetel pada satu menu sehingga
#' analisis kohort menghasilkan risk ratio tinggi pada menu tersebut.
#' Data ini fiktif dan hanya untuk pengujian serta pelatihan.
#'
#' @param n Jumlah responden.
#' @param seed Bibit pembangkit acak agar hasil dapat direproduksi.
#' @return Daftar berisi `data`, `pemetaan`, `konfigurasi`, dan `keterangan`.
#' @export
#' @examples
#' contoh <- contoh_keracunan_pangan(n = 50)
#' nrow(contoh$data)
contoh_keracunan_pangan <- function(n = 209, seed = 20180410) {
  set.seed(seed)
  menu <- data.frame(
    kunci = c("nasi", "rendang", "telur_puyuh", "acar", "semangka", "es_krim", "air_mineral", "teh"),
    label = c("Nasi", "Rendang daging sapi", "Oseng telur puyuh", "Acar", "Semangka",
              "Es krim", "Air mineral", "Teh manis"),
    p = c(0.75, 0.72, 0.57, 0.45, 0.35, 0.20, 0.05, 0.55),
    stringsAsFactors = FALSE
  )
  gejala <- data.frame(
    kunci = c("diare", "sakit_perut", "mual", "muntah", "pusing", "demam"),
    label = c("Diare cair", "Sakit perut", "Mual", "Muntah", "Pusing", "Demam"),
    p = c(0.97, 0.97, 0.52, 0.38, 0.37, 0.12),
    stringsAsFactors = FALSE
  )

  laki <- stats::runif(n) < 0.565
  roll <- stats::runif(n)
  umur <- ifelse(roll < 0.034, floor(stats::runif(n) * 5),
          ifelse(roll < 0.12, 5 + floor(stats::runif(n) * 8),
          ifelse(roll < 0.16, 13 + floor(stats::runif(n) * 6),
          ifelse(roll < 0.61, 19 + floor(stats::runif(n) * 27),
          ifelse(roll < 0.93, 46 + floor(stats::runif(n) * 19),
                 65 + floor(stats::runif(n) * 20))))))
  tempat <- ifelse(stats::runif(n) < 0.66, "di_tempat", "dibungkus")

  konsumsi <- vapply(seq_len(nrow(menu)), function(i) {
    p <- ifelse(tempat == "di_tempat", pmin(0.98, menu$p[i] + 0.15), pmax(0.02, menu$p[i] - 0.10))
    as.integer(stats::runif(n) < p)
  }, integer(n))
  colnames(konsumsi) <- paste0("mkn_", menu$kunci)

  # Rendang kloter dua menjadi sumber keracunan.
  p_sakit <- ifelse(konsumsi[, "mkn_rendang"] == 1, 0.94, 0.05)
  sakit <- stats::runif(n) < p_sakit

  waktu_makan <- as.POSIXct("2018-04-10 12:00:00", tz = "") +
    ifelse(stats::runif(n) < 0.5, 0, 1800)
  # Masa inkubasi 1 sampai 9,5 jam dengan modus sekitar 5 jam.
  inkubasi <- pmin(9.5, 1 + abs(stats::runif(n) + stats::runif(n) + stats::runif(n) - 1.5) * 5.6)
  onset <- waktu_makan + inkubasi * 3600
  onset[!sakit] <- NA

  data <- data.frame(
    no_responden = sprintf("R-%03d", seq_len(n)),
    jk = ifelse(laki, "L", "P"),
    umur = umur,
    dusun = ifelse(stats::runif(n) < 0.6, "Kalinongko", "Ngramang"),
    tempat_makan = tempat,
    tgl_makan = format(waktu_makan, "%Y-%m-%dT%H:%M:%S"),
    status_sakit = ifelse(sakit, "sakit", "tidak_sakit"),
    tgl_onset = ifelse(is.na(onset), NA_character_, format(onset, "%Y-%m-%dT%H:%M:%S")),
    meninggal = "tidak",
    berobat = ifelse(!sakit, "tidak_berobat",
                     sample(c("puskesmas", "rumah_sakit", "tidak_berobat"), n, TRUE, c(0.45, 0.25, 0.3))),
    lat = -7.85 + stats::runif(n) * 0.01,
    lon = 110.14 + stats::runif(n) * 0.01,
    stringsAsFactors = FALSE
  )
  data <- cbind(data, as.data.frame(konsumsi))
  for (i in seq_len(nrow(gejala))) {
    data[[paste0("gjl_", gejala$kunci[i])]] <- as.integer(sakit & stats::runif(n) < gejala$p[i])
  }

  pemetaan <- list(
    id_kasus = list(kolom = "no_responden"),
    status_sakit = list(kolom = "status_sakit", nilai_positif = "sakit"),
    tanggal_onset = list(kolom = "tgl_onset"),
    tanggal_paparan = list(kolom = "tgl_makan"),
    umur = list(kolom = "umur"),
    jenis_kelamin = list(kolom = "jk", peta_nilai = list(L = "Laki-laki", P = "Perempuan")),
    wilayah = list(kolom = "dusun"),
    lokasi_paparan = list(kolom = "tempat_makan",
                          peta_nilai = list(di_tempat = "Makan di tempat acara",
                                            dibungkus = "Dibungkus atau hantaran")),
    gejala = list(kolom = paste0("gjl_", gejala$kunci), nilai_positif = "1",
                  label = stats::setNames(as.list(gejala$label), paste0("gjl_", gejala$kunci))),
    makanan = list(kolom = paste0("mkn_", menu$kunci), nilai_positif = "1",
                   label = stats::setNames(as.list(menu$label), paste0("mkn_", menu$kunci))),
    meninggal = list(kolom = "meninggal", nilai_positif = "ya"),
    tempat_berobat = list(kolom = "berobat"),
    latitude = list(kolom = "lat"),
    longitude = list(kolom = "lon")
  )

  list(
    data = data,
    pemetaan = pemetaan,
    konfigurasi = konfigurasi_analisis(
      kelompok_umur = c(0, 5, 13, 19, 46, 65),
      satuan_waktu = "jam", populasi_berisiko = 240, desain = "kohort"
    ),
    keterangan = list(
      nama = "KLB Keracunan Pangan Hajatan Desa Kedungsari (data contoh)",
      jenis = "keracunan_pangan", penyakit = "Keracunan pangan",
      provinsi = "D.I. Yogyakarta", kabupaten = "Kulon Progo",
      kecamatan = "Pengasih", desa = "Kedungsari",
      latitude = -7.8512, longitude = 110.1445,
      tanggal_lapor = "2018-04-11", status = "berakhir"
    )
  )
}

#' Data contoh KLB pertusis
#'
#' Membangkitkan data sintetis KLB pertusis di satu kalurahan dengan pola
#' propagated source, variabel status imunisasi, klasifikasi kasus, dan faktor
#' risiko penularan. Data ini fiktif dan hanya untuk pengujian serta pelatihan.
#'
#' @inheritParams contoh_keracunan_pangan
#' @return Daftar berisi `data`, `pemetaan`, `konfigurasi`, dan `keterangan`.
#' @export
contoh_pertusis <- function(n = 53, seed = 20241109) {
  set.seed(seed)
  gejala <- data.frame(
    kunci = c("batuk", "pilek", "muntah_batuk", "demam", "whooping", "sakit_tenggorokan", "mata_berair"),
    label = c("Batuk", "Pilek", "Muntah setelah batuk", "Demam",
              "Whooping atau tarikan napas panjang", "Sakit tenggorokan", "Mata berair"),
    p = c(1, 0.42, 0.42, 0.25, 0.17, 0.17, 0.17),
    stringsAsFactors = FALSE
  )
  faktor <- data.frame(
    kunci = c("kontak_serumah", "sekolah_sama", "mengaji_sama", "kamar_padat"),
    label = c("Kontak serumah dengan kasus", "Satu sekolah dengan kasus",
              "Satu tempat mengaji dengan kasus", "Kepadatan hunian tinggi"),
    p = c(0.40, 0.55, 0.35, 0.30),
    rr = c(2.2, 1.5, 1.4, 1.3),
    stringsAsFactors = FALSE
  )
  imunisasi <- data.frame(
    nilai = c("lengkap", "tidak_imunisasi", "tidak_tahu"),
    label = c("Lengkap (Penta I, II, III dan booster)", "Tidak imunisasi", "Tidak tahu atau lupa"),
    p = c(0.70, 0.02, 0.28), rr = c(1, 1.6, 1.35),
    stringsAsFactors = FALSE
  )

  laki <- stats::runif(n) < 0.547
  roll <- stats::runif(n)
  umur <- ifelse(roll < 0.11, floor(stats::runif(n) * 5),
          ifelse(roll < 0.79, 5 + floor(stats::runif(n) * 5),
          ifelse(roll < 0.83, 10 + floor(stats::runif(n) * 5),
                 20 + floor(stats::runif(n) * 30))))
  imun_idx <- sample(seq_len(nrow(imunisasi)), n, TRUE, prob = imunisasi$p)

  fr <- vapply(seq_len(nrow(faktor)), function(i) as.integer(stats::runif(n) < faktor$p[i]), integer(n))
  colnames(fr) <- paste0("fr_", faktor$kunci)

  risiko <- 0.16 * imunisasi$rr[imun_idx]
  for (i in seq_len(nrow(faktor))) risiko <- risiko * ifelse(fr[, i] == 1, faktor$rr[i], 1)
  risiko <- risiko * ifelse(umur >= 20, 1.8, 1)
  sakit <- stats::runif(n) < pmin(0.85, risiko)

  mulai <- as.Date("2024-03-03")
  roll2 <- stats::runif(n)
  hari <- ifelse(roll2 < 0.15, floor(stats::runif(n) * 60),
          ifelse(roll2 < 0.70, 140 + floor(stats::runif(n) * 25),
                 60 + floor(stats::runif(n) * 180)))
  onset <- mulai + hari
  onset[!sakit] <- NA

  bergejala <- sakit & stats::runif(n) > 0.2
  spesimen <- sakit | stats::runif(n) < 0.2
  lab_positif <- spesimen & sakit & ((bergejala & stats::runif(n) < 0.13) |
                                       (!bergejala & stats::runif(n) < 0.5))

  data <- data.frame(
    no_responden = sprintf("P-%03d", seq_len(n)),
    jk = ifelse(laki, "L", "P"),
    umur = umur,
    dusun = ifelse(stats::runif(n) < 0.7, "Ngropoh", "Soropadan"),
    status_sakit = ifelse(sakit, "kasus", "bukan_kasus"),
    klasifikasi = ifelse(!sakit, "bukan_kasus",
                  ifelse(lab_positif & !bergejala, "carrier",
                  ifelse(lab_positif, "konfirmasi", "probable"))),
    tgl_onset = ifelse(is.na(onset), NA_character_, format(onset, "%Y-%m-%d")),
    status_imunisasi = imunisasi$nilai[imun_idx],
    spesimen = ifelse(spesimen, "usap_nasofaring", NA_character_),
    hasil_lab = ifelse(!spesimen, NA_character_, ifelse(lab_positif, "positif", "negatif")),
    meninggal = "tidak",
    lat = -7.755 + stats::runif(n) * 0.01,
    lon = 110.400 + stats::runif(n) * 0.01,
    stringsAsFactors = FALSE
  )
  data <- cbind(data, as.data.frame(fr))
  for (i in seq_len(nrow(gejala))) {
    data[[paste0("gjl_", gejala$kunci[i])]] <- as.integer(bergejala & stats::runif(n) < gejala$p[i])
  }

  pemetaan <- list(
    id_kasus = list(kolom = "no_responden"),
    status_sakit = list(kolom = "status_sakit", nilai_positif = "kasus"),
    klasifikasi_kasus = list(kolom = "klasifikasi"),
    tanggal_onset = list(kolom = "tgl_onset"),
    umur = list(kolom = "umur"),
    jenis_kelamin = list(kolom = "jk", peta_nilai = list(L = "Laki-laki", P = "Perempuan")),
    wilayah = list(kolom = "dusun"),
    status_imunisasi = list(kolom = "status_imunisasi",
                            peta_nilai = stats::setNames(as.list(imunisasi$label), imunisasi$nilai)),
    gejala = list(kolom = paste0("gjl_", gejala$kunci), nilai_positif = "1",
                  label = stats::setNames(as.list(gejala$label), paste0("gjl_", gejala$kunci))),
    faktor_risiko = list(kolom = paste0("fr_", faktor$kunci), nilai_positif = "1",
                         label = stats::setNames(as.list(faktor$label), paste0("fr_", faktor$kunci))),
    hasil_lab = list(kolom = "hasil_lab"),
    jenis_spesimen = list(kolom = "spesimen"),
    meninggal = list(kolom = "meninggal", nilai_positif = "ya"),
    latitude = list(kolom = "lat"),
    longitude = list(kolom = "lon")
  )

  list(
    data = data,
    pemetaan = pemetaan,
    konfigurasi = konfigurasi_analisis(
      kelompok_umur = c(0, 5, 10, 15, 20), satuan_waktu = "hari",
      interval_kurva = 7, populasi_berisiko = n, desain = "kohort",
      inkubasi_acuan = list(min = 6 * 24, maks = 20 * 24)
    ),
    keterangan = list(
      nama = "KLB Pertusis Kalurahan Condongcatur (data contoh)",
      jenis = "pd3i", penyakit = "Pertusis",
      provinsi = "D.I. Yogyakarta", kabupaten = "Sleman",
      kecamatan = "Depok", desa = "Condongcatur",
      latitude = -7.7562, longitude = 110.4053,
      tanggal_lapor = "2024-11-09", status = "berlangsung"
    )
  )
}
