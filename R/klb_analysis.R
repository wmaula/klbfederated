#' Menjalankan seluruh analisis KLB
#'
#' Fungsi utama yang menjalankan analisis deskriptif dan analitik sesuai
#' Pedoman Penyelidikan dan Penanggulangan KLB Kementerian Kesehatan edisi
#' revisi III tahun 2020. Dapat dipanggil langsung dari konsol R maupun dari
#' aplikasi Shiny.
#'
#' @param data `data.frame` submission KoboToolbox atau data line listing lain.
#' @param pemetaan Daftar pemetaan peran variabel, lihat [peran_variabel()].
#' @param konfigurasi Konfigurasi analisis, lihat [konfigurasi_analisis()].
#' @param pembanding Data pembanding surveilans, lihat [pembanding_kosong()].
#' @param jenis Jenis KLB. Diagnosis banding otomatis hanya dijalankan untuk
#'   `"keracunan_pangan"` karena tabel rujukan bawaan memuat agen keracunan pangan.
#' @param rujukan Tabel rujukan agen penyebab.
#'
#' @return Objek `klb_hasil`, sebuah daftar berisi ringkasan, tabel deskriptif,
#'   kurva epidemik, masa inkubasi, analisis paparan, model multivariabel,
#'   diagnosis banding, penilaian kriteria KLB, dan daftar peringatan.
#' @export
#' @examples
#' contoh <- contoh_keracunan_pangan()
#' hasil <- analisis_klb(contoh$data, contoh$pemetaan, contoh$konfigurasi,
#'                       jenis = "keracunan_pangan")
#' hasil$ringkasan$attack_rate
analisis_klb <- function(data,
                         pemetaan,
                         konfigurasi = konfigurasi_analisis(),
                         pembanding = pembanding_kosong(),
                         jenis = c("keracunan_pangan", "penyakit_menular", "pd3i"),
                         rujukan = rujukan_patogen()) {
  jenis <- match.arg(jenis)
  ds <- bangun_dataset(data, pemetaan, konfigurasi)
  df <- ds$df
  peringatan <- ds$peringatan

  n_inv <- sum(!is.na(df$sakit))
  n_kasus <- sum(df$sakit == 1, na.rm = TRUE)
  n_mati <- sum(df$meninggal == 1 & df$sakit == 1, na.rm = TRUE)
  pop_estimasi <- konfigurasi$populasi_berisiko
  if (is.null(pop_estimasi) || !is.finite(pop_estimasi)) pop_estimasi <- NA_real_

  onset_kasus <- df$onset[!is.na(df$sakit) & df$sakit == 1 & !is.na(df$onset)]
  kasus_pertama <- if (length(onset_kasus)) min(onset_kasus) else as.POSIXct(NA)
  kasus_terakhir <- if (length(onset_kasus)) max(onset_kasus) else as.POSIXct(NA)

  ringkasan <- list(
    populasi_berisiko = if (is.na(pop_estimasi)) n_inv else pop_estimasi,
    total_diinvestigasi = n_inv,
    total_kasus = n_kasus,
    total_meninggal = n_mati,
    attack_rate = if (n_inv > 0) n_kasus / n_inv * 100 else NA_real_,
    cfr = if (ds$ada_meninggal && n_kasus > 0) n_mati / n_kasus * 100 else NA_real_,
    tanggal_kasus_pertama = kasus_pertama,
    tanggal_kasus_terakhir = kasus_terakhir
  )

  ## Distribusi gejala pada kasus
  gejala <- NULL
  if (nrow(ds$gejala) > 0 && n_kasus > 0) {
    idx <- which(!is.na(df$sakit) & df$sakit == 1)
    n <- vapply(ds$gejala$kolom, function(k) sum(df[[k]][idx] == 1, na.rm = TRUE), numeric(1))
    urut <- order(-n)
    gejala <- data.frame(
      kategori = ds$gejala$label[urut], n = as.integer(n[urut]),
      persen = as.numeric(n[urut]) / n_kasus * 100, stringsAsFactors = FALSE
    )
  }

  ## Attack rate menurut variabel kategorik
  ar_jenis_kelamin <- tabel_attack_rate(df, "jk")
  ar_tempat <- tabel_attack_rate(df, "wilayah")

  ar_umur <- NULL
  if (any(!is.na(df$umur))) {
    df$kelompok_umur <- as.character(kelompokkan_umur(df$umur, konfigurasi$kelompok_umur))
    ar_umur <- tabel_attack_rate(df, "kelompok_umur")
    if (!is.null(ar_umur)) {
      urutan <- unique(as.character(kelompokkan_umur(sort(konfigurasi$kelompok_umur), konfigurasi$kelompok_umur)))
      ar_umur <- ar_umur[order(match(ar_umur$kategori, urutan)), ]
    }
  }

  ar_lain <- list()
  freq_kasus <- list()
  for (i in seq_len(nrow(ds$kategorik))) {
    kol <- ds$kategorik$kolom[i]
    label <- ds$kategorik$label[i]
    if (kol %in% c("jk", "wilayah")) next
    if (ds$kategorik$mode[i] == "ar") {
      tab <- tabel_attack_rate(df, kol)
      if (!is.null(tab)) ar_lain[[label]] <- tab
    } else {
      tab <- tabel_frekuensi_kasus(df, kol)
      if (!is.null(tab)) freq_kasus[[label]] <- tab
    }
  }

  ## Waktu
  inkubasi <- ringkasan_inkubasi(df)
  kurva <- kurva_epidemik(df, konfigurasi, inkubasi)
  klasifikasi <- klasifikasi_kurva(kurva$bins, inkubasi, ds$ada_paparan_waktu,
                                   konfigurasi$inkubasi_acuan)
  inkubasi$periode_paparan <- periode_paparan(kasus_pertama, kasus_terakhir,
                                              inkubasi, konfigurasi$inkubasi_acuan)

  ## Analitik
  paparan <- analisis_paparan(df, ds$paparan, konfigurasi$desain)
  multivariabel <- analisis_multivariabel(df, ds$paparan, paparan, konfigurasi)

  if (length(multivariabel$peringatan) > 0) {
    peringatan <- c(peringatan, multivariabel$peringatan)
  }

  if (!is.null(paparan) && any(paparan$koreksi)) {
    peringatan <- c(peringatan, paste(
      "Sebagian tabel 2x2 memiliki sel bernilai nol sehingga estimasi memakai koreksi 0,5",
      "(Haldane-Anscombe). Tafsirkan selang kepercayaan dengan hati-hati."
    ))
  }

  ## Etiologi dan kriteria KLB
  periode_klb_jam <- if (!is.na(kasus_pertama) && !is.na(kasus_terakhir)) {
    as.numeric(difftime(kasus_terakhir, kasus_pertama, units = "hours"))
  } else NA_real_

  banding <- NULL
  if (identical(jenis, "keracunan_pangan")) {
    banding <- diagnosis_banding(gejala, inkubasi, periode_klb_jam, rujukan)
  } else {
    peringatan <- c(peringatan, paste(
      "Diagnosis banding otomatis tidak dijalankan karena tabel rujukan bawaan hanya memuat agen",
      "keracunan pangan. Susun diagnosis banding secara manual atau sediakan tabel rujukan penyakit terkait."
    ))
  }

  kriteria <- nilai_kriteria_klb(pembanding, ringkasan, kurva$bins)

  ## Spot map
  spot <- df[!is.na(df$lat) & !is.na(df$lon), c("lat", "lon", "sakit", "id_kasus")]
  if (nrow(spot) > 0) spot$sakit <- !is.na(spot$sakit) & spot$sakit == 1

  if (!ds$ada_onset) {
    peringatan <- c(peringatan,
      "Tanggal onset belum tersedia sehingga kurva epidemik dan periode KLB tidak dapat dihitung.")
  }
  if (inkubasi$n == 0 && ds$ada_paparan_waktu) {
    peringatan <- c(peringatan,
      "Masa inkubasi tidak dapat dihitung karena pasangan waktu paparan dan onset tidak lengkap.")
  }
  if (!is.na(pop_estimasi) && pop_estimasi > n_inv) {
    peringatan <- c(peringatan, sprintf(
      paste("Attack rate dihitung dengan penyebut responden terinvestigasi (%d).",
            "Populasi berisiko yang diestimasi %g orang dilaporkan terpisah."),
      n_inv, pop_estimasi))
  }

  struktur <- list(
    dibuat_pada = Sys.time(),
    jenis = jenis,
    ringkasan = ringkasan,
    gejala = gejala,
    ar_jenis_kelamin = ar_jenis_kelamin,
    ar_umur = ar_umur,
    ar_tempat = ar_tempat,
    ar_lain = ar_lain,
    freq_kasus = freq_kasus,
    inkubasi = inkubasi,
    kurva = list(bins = kurva$bins, interval_jam = kurva$interval_jam,
                 satuan = konfigurasi$satuan_waktu, tipe = klasifikasi$tipe,
                 alasan_tipe = klasifikasi$alasan),
    paparan = paparan,
    multivariabel = multivariabel,
    diagnosis_banding = banding,
    kriteria_klb = kriteria,
    spot_map = spot,
    peringatan = peringatan,
    konfigurasi = konfigurasi,
    dataset = ds
  )
  class(struktur) <- c("klb_hasil", "list")
  struktur
}

#' @export
print.klb_hasil <- function(x, ...) {
  r <- x$ringkasan
  cat("Hasil analisis KLB\n")
  cat(sprintf("  Populasi berisiko   : %s\n", r$populasi_berisiko))
  cat(sprintf("  Diinvestigasi       : %s\n", r$total_diinvestigasi))
  cat(sprintf("  Kasus               : %s (attack rate %s persen)\n",
              r$total_kasus, angka_id(r$attack_rate)))
  cat(sprintf("  Meninggal           : %s (CFR %s persen)\n",
              r$total_meninggal, angka_id(r$cfr)))
  if (is.finite(x$inkubasi$min_jam)) {
    cat(sprintf("  Masa inkubasi       : %s sampai %s jam, rata-rata %s jam\n",
                angka_id(x$inkubasi$min_jam, 2), angka_id(x$inkubasi$maks_jam, 2),
                angka_id(x$inkubasi$rata_jam, 2)))
  }
  cat(sprintf("  Kurva epidemik      : %s, interval %s jam\n",
              gsub("_", " ", x$kurva$tipe), angka_id(x$kurva$interval_jam, 2)))
  if (!is.null(x$paparan)) {
    bermakna <- x$paparan[is.finite(x$paparan$p_value) & x$paparan$p_value < 0.05 &
                            x$paparan$estimasi > 1, ]
    if (nrow(bermakna) > 0) {
      cat("  Paparan bermakna    :\n")
      for (i in seq_len(nrow(bermakna))) {
        cat(sprintf("    %s: %s %s (95%% CI %s sampai %s), p %s\n",
                    bermakna$label[i], bermakna$ukuran[i], angka_id(bermakna$estimasi[i], 2),
                    angka_id(bermakna$ci_bawah[i], 2), angka_id(bermakna$ci_atas[i], 2),
                    format.pval(bermakna$p_value[i], digits = 3, eps = 1e-4)))
      }
    }
  }
  if (length(x$peringatan)) {
    cat("  Peringatan          :\n")
    for (p in x$peringatan) cat(sprintf("    - %s\n", p))
  }
  invisible(x)
}
