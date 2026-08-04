#' Konfigurasi analisis bawaan
#'
#' @param ... Nilai yang ingin diganti, misalnya `satuan_waktu = "hari"`.
#' @return Daftar konfigurasi analisis.
#' @export
#' @examples
#' konfigurasi_analisis(satuan_waktu = "hari", interval_kurva = 7)
konfigurasi_analisis <- function(...) {
  bawaan <- list(
    kelompok_umur = c(0, 5, 13, 19, 46, 65),
    satuan_waktu = "jam",
    interval_kurva = NULL,
    desain = "kohort",
    ambang_kandidat = 0.25,
    populasi_berisiko = NULL,
    inkubasi_acuan = NULL
  )
  utils::modifyList(bawaan, list(...))
}

nilai_teks <- function(x) {
  s <- trimws(as.character(x))
  s[s %in% c("", "NA", "na", "NULL", "null")] <- NA_character_
  s
}

TRUEISH <- c("1", "ya", "yes", "true", "y", "positif", "ada", "sakit")

nilai_positif <- function(x, nilai = NULL) {
  s <- tolower(nilai_teks(x))
  if (!is.null(nilai) && length(nilai) > 0) {
    hasil <- ifelse(is.na(s), NA, s %in% tolower(nilai))
  } else {
    hasil <- ifelse(is.na(s), NA, s %in% TRUEISH)
  }
  as.integer(hasil)
}

#' Mengurai tanggal atau tanggal dan jam menjadi POSIXct
#'
#' Menerima format ISO (`2024-05-16T17:00:00`), tanggal ISO, serta
#' `dd/mm/yyyy` dan `dd-mm-yyyy`. Kolom jam terpisah digabungkan bila diberikan.
#'
#' @param tanggal Vektor tanggal.
#' @param jam Vektor jam opsional, misalnya `"17:30"` atau `"17.30"`.
#' @return Vektor `POSIXct`.
#' @export
urai_waktu <- function(tanggal, jam = NULL) {
  s <- nilai_teks(tanggal)
  dmy <- grepl("^\\d{1,2}[/-]\\d{1,2}[/-]\\d{4}", s) & !is.na(s)
  if (any(dmy)) {
    bagian <- regmatches(s[dmy], regexpr("^(\\d{1,2})[/-](\\d{1,2})[/-](\\d{4})", s[dmy]))
    urai <- do.call(rbind, strsplit(bagian, "[/-]"))
    s[dmy] <- sprintf("%s-%02d-%02d", urai[, 3], as.integer(urai[, 2]), as.integer(urai[, 1]))
  }
  s <- sub("T", " ", s)
  punya_jam <- grepl("\\d{1,2}:\\d{2}", s)
  if (!is.null(jam)) {
    j <- nilai_teks(jam)
    cocok <- regmatches(j, regexpr("\\d{1,2}[.:]\\d{2}", j))
    tambah <- !punya_jam & !is.na(j) & nzchar(j) & !is.na(s)
    if (any(tambah)) {
      jj <- rep(NA_character_, length(s))
      idx <- which(!is.na(j) & nzchar(j))
      cc <- regmatches(j[idx], regexpr("\\d{1,2}[.:]\\d{2}", j[idx]))
      jj[idx[nzchar(cc)]] <- gsub("\\.", ":", cc[nzchar(cc)])
      pakai <- tambah & !is.na(jj)
      s[pakai] <- paste(substr(s[pakai], 1, 10), jj[pakai])
    }
  }
  # Vektor campuran tanggal dan tanggal-jam harus diseragamkan lebih dahulu,
  # karena as.POSIXct memilih satu format untuk seluruh vektor sehingga jam
  # dapat terbuang bila ada satu elemen yang hanya berisi tanggal.
  punya_jam <- grepl("\\d{1,2}:\\d{2}", s)
  s[!punya_jam & !is.na(s)] <- paste0(substr(s[!punya_jam & !is.na(s)], 1, 10), " 00:00:00")
  s[punya_jam & !grepl("\\d{1,2}:\\d{2}:\\d{2}", s)] <-
    paste0(s[punya_jam & !grepl("\\d{1,2}:\\d{2}:\\d{2}", s)], ":00")
  as.POSIXct(s, tz = "", format = "%Y-%m-%d %H:%M:%S")
}

kolom_peran <- function(pemetaan, key) {
  m <- pemetaan[[key]]
  if (is.null(m)) return(character(0))
  kolom <- m$kolom
  kolom[!is.na(kolom) & nzchar(kolom)]
}

label_kolom <- function(pemetaan, key, kolom) {
  lab <- pemetaan[[key]]$label
  if (!is.null(lab) && !is.null(lab[[kolom]])) return(lab[[kolom]])
  kolom
}

#' Membangun tabel analisis dari data KoboToolbox
#'
#' Mengubah submission mentah menjadi tabel siap analisis berdasarkan pemetaan
#' peran variabel yang dipilih pengguna.
#'
#' @param data `data.frame` submission KoboToolbox.
#' @param pemetaan Daftar pemetaan peran variabel.
#' @param konfigurasi Konfigurasi analisis dari [konfigurasi_analisis()].
#' @return Daftar berisi `df`, `gejala`, `paparan`, `kategorik`, `peringatan`,
#'   dan penanda ketersediaan variabel.
#' @export
bangun_dataset <- function(data, pemetaan, konfigurasi = konfigurasi_analisis()) {
  stopifnot(is.data.frame(data))
  n <- nrow(data)
  peringatan <- character(0)
  amb <- function(kolom) if (length(kolom) && kolom %in% names(data)) data[[kolom]] else rep(NA, n)

  kol_status <- kolom_peran(pemetaan, "status_sakit")[1]
  sakit <- if (!is.na(kol_status)) {
    nilai_positif(amb(kol_status), pemetaan$status_sakit$nilai_positif)
  } else {
    rep(NA_integer_, n)
  }
  if (sum(is.na(sakit)) > 0) {
    peringatan <- c(peringatan, sprintf(
      "%d responden tidak memiliki nilai status kasus yang dapat dibaca dan dikeluarkan dari perhitungan attack rate.",
      sum(is.na(sakit))
    ))
  }

  kol_meninggal <- kolom_peran(pemetaan, "meninggal")[1]
  meninggal <- if (!is.na(kol_meninggal)) {
    nilai_positif(amb(kol_meninggal), pemetaan$meninggal$nilai_positif)
  } else {
    rep(NA_integer_, n)
  }

  kol_onset <- kolom_peran(pemetaan, "tanggal_onset")[1]
  kol_jam_onset <- kolom_peran(pemetaan, "jam_onset")[1]
  onset <- if (!is.na(kol_onset)) {
    urai_waktu(amb(kol_onset), if (!is.na(kol_jam_onset)) amb(kol_jam_onset) else NULL)
  } else {
    as.POSIXct(rep(NA_real_, n), origin = "1970-01-01")
  }

  kol_paparan <- kolom_peran(pemetaan, "tanggal_paparan")[1]
  kol_jam_paparan <- kolom_peran(pemetaan, "jam_paparan")[1]
  paparan_waktu <- if (!is.na(kol_paparan)) {
    urai_waktu(amb(kol_paparan), if (!is.na(kol_jam_paparan)) amb(kol_jam_paparan) else NULL)
  } else {
    as.POSIXct(rep(NA_real_, n), origin = "1970-01-01")
  }

  kol_umur <- kolom_peran(pemetaan, "umur")[1]
  kol_lahir <- kolom_peran(pemetaan, "tanggal_lahir")[1]
  umur <- rep(NA_real_, n)
  if (!is.na(kol_umur)) {
    umur <- suppressWarnings(as.numeric(gsub(",", ".", nilai_teks(amb(kol_umur)))))
  }
  if (!is.na(kol_lahir)) {
    lahir <- urai_waktu(amb(kol_lahir))
    acuan <- onset
    acuan[is.na(acuan)] <- Sys.time()
    hitung <- is.na(umur) & !is.na(lahir)
    umur[hitung] <- as.numeric(difftime(acuan[hitung], lahir[hitung], units = "days")) / 365.25
  }

  df <- data.frame(sakit = sakit, meninggal = meninggal, umur = umur,
                   stringsAsFactors = FALSE)
  df$onset <- onset
  df$paparan <- paparan_waktu

  kategorik_spec <- list(
    list(peran = "jenis_kelamin", kol = "jk", label = "Jenis kelamin", mode = "ar"),
    list(peran = "wilayah", kol = "wilayah", label = "Wilayah", mode = "ar"),
    list(peran = "lokasi_paparan", kol = "lokasi", label = "Lokasi atau cara paparan", mode = "ar"),
    list(peran = "status_imunisasi", kol = "imunisasi", label = "Status imunisasi", mode = "ar"),
    list(peran = "pekerjaan_status", kol = "status_orang", label = "Status atau pekerjaan", mode = "ar"),
    list(peran = "kontak_kasus", kol = "kontak", label = "Hubungan epidemiologi", mode = "ar"),
    # Variabel berikut hanya terisi pada kasus sehingga disajikan sebagai
    # distribusi frekuensi kasus, bukan attack rate.
    list(peran = "klasifikasi_kasus", kol = "klasifikasi", label = "Klasifikasi kasus", mode = "frekuensi"),
    list(peran = "tempat_berobat", kol = "berobat", label = "Tempat berobat", mode = "frekuensi"),
    list(peran = "hasil_lab", kol = "lab", label = "Hasil laboratorium", mode = "frekuensi"),
    list(peran = "jenis_spesimen", kol = "spesimen", label = "Jenis spesimen", mode = "frekuensi")
  )
  kategorik <- data.frame(kolom = character(0), label = character(0),
                          mode = character(0), stringsAsFactors = FALSE)
  for (spec in kategorik_spec) {
    sumber <- kolom_peran(pemetaan, spec$peran)[1]
    if (is.na(sumber)) next
    nilai <- nilai_teks(amb(sumber))
    peta <- pemetaan[[spec$peran]]$peta_nilai
    if (!is.null(peta)) {
      cocok <- !is.na(nilai) & nilai %in% names(peta)
      nilai[cocok] <- unlist(peta[nilai[cocok]], use.names = FALSE)
    }
    df[[spec$kol]] <- nilai
    kategorik <- rbind(kategorik, data.frame(kolom = spec$kol, label = spec$label,
                                             mode = spec$mode, stringsAsFactors = FALSE))
  }

  gejala <- data.frame(kolom = character(0), label = character(0), stringsAsFactors = FALSE)
  kolom_gejala <- kolom_peran(pemetaan, "gejala")
  for (i in seq_along(kolom_gejala)) {
    nama <- paste0("g_", i)
    df[[nama]] <- nilai_positif(amb(kolom_gejala[i]), pemetaan$gejala$nilai_positif)
    gejala <- rbind(gejala, data.frame(
      kolom = nama, label = label_kolom(pemetaan, "gejala", kolom_gejala[i]),
      stringsAsFactors = FALSE
    ))
  }

  paparan <- data.frame(kolom = character(0), label = character(0),
                        sumber = character(0), stringsAsFactors = FALSE)
  idx <- 0
  for (peran in c("makanan", "faktor_risiko")) {
    for (kol in kolom_peran(pemetaan, peran)) {
      idx <- idx + 1
      nama <- paste0("e_", idx)
      df[[nama]] <- nilai_positif(amb(kol), pemetaan[[peran]]$nilai_positif)
      paparan <- rbind(paparan, data.frame(
        kolom = nama, label = label_kolom(pemetaan, peran, kol), sumber = peran,
        stringsAsFactors = FALSE
      ))
    }
  }

  kol_lat <- kolom_peran(pemetaan, "latitude")[1]
  kol_lon <- kolom_peran(pemetaan, "longitude")[1]
  kol_geo <- kolom_peran(pemetaan, "geopoint")[1]
  lat <- rep(NA_real_, n); lon <- rep(NA_real_, n)
  if (!is.na(kol_lat) && !is.na(kol_lon)) {
    lat <- suppressWarnings(as.numeric(nilai_teks(amb(kol_lat))))
    lon <- suppressWarnings(as.numeric(nilai_teks(amb(kol_lon))))
  } else if (!is.na(kol_geo)) {
    bagian <- strsplit(nilai_teks(amb(kol_geo)), "\\s+")
    lat <- suppressWarnings(as.numeric(vapply(bagian, function(x) if (length(x) >= 1) x[1] else NA_character_, character(1))))
    lon <- suppressWarnings(as.numeric(vapply(bagian, function(x) if (length(x) >= 2) x[2] else NA_character_, character(1))))
  }
  df$lat <- lat
  df$lon <- lon

  kol_id <- kolom_peran(pemetaan, "id_kasus")[1]
  df$id_kasus <- if (!is.na(kol_id)) nilai_teks(amb(kol_id)) else paste0("#", seq_len(n))

  pop <- konfigurasi$populasi_berisiko
  if (!is.null(pop) && !is.na(pop) && pop < n) {
    peringatan <- c(peringatan, sprintf(
      paste("Populasi berisiko yang diisi (%s) lebih kecil dari jumlah responden terinvestigasi (%s).",
            "Attack rate memakai jumlah responden sebagai penyebut."),
      pop, n
    ))
  }

  list(
    df = df,
    gejala = gejala,
    paparan = paparan,
    kategorik = kategorik,
    peringatan = peringatan,
    ada_onset = any(!is.na(df$onset)),
    ada_paparan_waktu = any(!is.na(df$paparan)),
    ada_meninggal = !is.na(kol_meninggal),
    ada_koordinat = any(!is.na(df$lat))
  )
}
