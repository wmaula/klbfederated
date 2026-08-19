#' Tabel attack rate menurut satu variabel kategorik
#'
#' @param df Tabel hasil [bangun_dataset()].
#' @param kolom Nama kolom kategorik pada `df`.
#' @return `data.frame` dengan kolom `kategori`, `populasi`, `kasus`,
#'   `meninggal`, `ar`, dan `cfr`.
#' @export
tabel_attack_rate <- function(df, kolom) {
  if (!kolom %in% names(df)) return(NULL)
  nilai <- df[[kolom]]
  pakai <- !is.na(nilai) & nzchar(nilai) & !is.na(df$sakit)
  if (!any(pakai)) return(NULL)
  ada_mati <- any(!is.na(df$meninggal))
  kategori <- sort(unique(nilai[pakai]))
  hasil <- lapply(kategori, function(k) {
    sel <- pakai & nilai == k
    kasus <- sum(df$sakit[sel] == 1, na.rm = TRUE)
    mati <- sum(df$meninggal[sel] == 1 & df$sakit[sel] == 1, na.rm = TRUE)
    data.frame(
      kategori = k, populasi = sum(sel), kasus = kasus, meninggal = mati,
      ar = if (sum(sel) > 0) kasus / sum(sel) * 100 else NA_real_,
      cfr = if (ada_mati && kasus > 0) mati / kasus * 100 else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, hasil)
}

#' Tabel distribusi frekuensi pada kasus
#' @param df Tabel hasil [bangun_dataset()].
#' @param kolom Nama kolom kategorik.
#' @return `data.frame` dengan kolom `kategori`, `n`, dan `persen`.
#' @export
tabel_frekuensi_kasus <- function(df, kolom) {
  if (!kolom %in% names(df)) return(NULL)
  sel <- !is.na(df$sakit) & df$sakit == 1 & !is.na(df[[kolom]]) & nzchar(df[[kolom]])
  if (!any(sel)) return(NULL)
  tb <- table(df[[kolom]][sel])
  data.frame(kategori = names(tb), n = as.integer(tb),
             persen = as.numeric(tb) / sum(tb) * 100, stringsAsFactors = FALSE)
}

#' Kelompok umur menurut batas yang ditentukan
#' @param umur Vektor umur dalam tahun.
#' @param batas Vektor batas bawah kelompok, misalnya `c(0, 5, 13, 19, 46, 65)`.
#' @return Faktor kelompok umur.
#' @export
kelompokkan_umur <- function(umur, batas = c(0, 5, 13, 19, 46, 65)) {
  batas <- sort(unique(batas))
  tepi <- c(batas, Inf)
  label <- vapply(seq_len(length(tepi) - 1), function(i) {
    if (is.infinite(tepi[i + 1])) sprintf("\u2265%g tahun", tepi[i])
    else sprintf("%g-%g tahun", tepi[i], tepi[i + 1] - 1)
  }, character(1))
  cut(umur, breaks = tepi, labels = label, right = FALSE, include.lowest = TRUE)
}

#' Ringkasan masa inkubasi
#' @param df Tabel hasil [bangun_dataset()].
#' @return Daftar berisi `n`, `min_jam`, `maks_jam`, `rata_jam`, dan `median_jam`.
#' @export
ringkasan_inkubasi <- function(df) {
  jam <- as.numeric(difftime(df$onset, df$paparan, units = "hours"))
  jam <- jam[!is.na(df$sakit) & df$sakit == 1 & is.finite(jam) & jam >= 0]
  if (length(jam) == 0) {
    return(list(n = 0L, min_jam = NA_real_, maks_jam = NA_real_,
                rata_jam = NA_real_, median_jam = NA_real_))
  }
  list(n = length(jam), min_jam = min(jam), maks_jam = max(jam),
       rata_jam = mean(jam), median_jam = stats::median(jam))
}

pilih_interval <- function(rentang_jam, rata_inkubasi) {
  if (is.finite(rata_inkubasi) && rata_inkubasi > 0) return(rata_inkubasi / 4)
  if (!is.finite(rentang_jam) || rentang_jam <= 0) return(1)
  kandidat <- c(0.25, 0.5, 1, 2, 3, 6, 12, 24, 48, 72, 168, 336, 720)
  kandidat[which.min(abs(kandidat - rentang_jam / 20))]
}

#' Kurva epidemik
#'
#' Interval bawaan mengikuti Pedoman KLB 2020, yaitu seperempat masa inkubasi
#' rata-rata. Interval dapat ditetapkan manual melalui konfigurasi.
#'
#' @param df Tabel hasil [bangun_dataset()].
#' @param konfigurasi Konfigurasi analisis.
#' @param inkubasi Hasil [ringkasan_inkubasi()].
#' @return Daftar berisi `bins` (`data.frame`) dan `interval_jam`.
#' @export
kurva_epidemik <- function(df, konfigurasi = konfigurasi_analisis(), inkubasi = NULL) {
  onset <- df$onset[!is.na(df$sakit) & df$sakit == 1 & !is.na(df$onset)]
  if (length(onset) == 0) {
    return(list(bins = data.frame(mulai = as.POSIXct(character(0)),
                                  selesai = as.POSIXct(character(0)),
                                  kasus = integer(0)),
                interval_jam = NA_real_))
  }
  if (is.null(inkubasi)) inkubasi <- ringkasan_inkubasi(df)
  rentang <- as.numeric(difftime(max(onset), min(onset), units = "hours"))
  interval <- konfigurasi$interval_kurva
  faktor <- c(menit = 1 / 60, jam = 1, hari = 24, minggu = 168)
  interval_jam <- if (!is.null(interval) && is.finite(interval) && interval > 0) {
    interval * unname(faktor[konfigurasi$satuan_waktu])
  } else {
    pilih_interval(rentang, inkubasi$rata_jam)
  }
  if (!is.finite(interval_jam) || interval_jam <= 0) interval_jam <- 1

  lebar <- interval_jam * 3600
  awal <- as.numeric(min(onset))
  akhir <- as.numeric(max(onset))
  tepi_num <- seq(floor(awal / lebar) * lebar, ceiling((akhir + 1) / lebar) * lebar, by = lebar)
  if (length(tepi_num) < 2) tepi_num <- c(tepi_num, tepi_num + lebar)
  cnt <- as.integer(table(cut(as.numeric(onset), breaks = tepi_num, right = FALSE, include.lowest = TRUE)))
  bins <- data.frame(
    mulai = as.POSIXct(utils::head(tepi_num, -1), origin = "1970-01-01"),
    selesai = as.POSIXct(utils::tail(tepi_num, -1), origin = "1970-01-01"),
    kasus = cnt
  )
  list(bins = bins, interval_jam = interval_jam)
}

#' Format angka gaya Indonesia
#'
#' Memakai koma sebagai pemisah desimal dan mengembalikan tanda hubung untuk
#' nilai yang tidak terhingga atau hilang. Fungsi ini tervektorisasi sehingga
#' dapat dipakai langsung di dalam `ggplot2::aes()`.
#'
#' @param x Vektor numerik.
#' @param digit Jumlah angka di belakang koma.
#' @return Vektor karakter.
#' @export
#' @examples
#' angka_id(c(78.912, NA, 1.5))
angka_id <- function(x, digit = 1) {
  if (length(x) == 0) return("-")
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.finite(x), sub("\\.", ",", formatC(x, format = "f", digits = digit)), "-")
}

#' Klasifikasi bentuk kurva epidemik
#'
#' Aturan mengikuti Pedoman KLB 2020: bila periode KLB tidak melebihi selisih
#' masa inkubasi terpanjang dan terpendek, pola sesuai sumber tunggal.
#'
#' @param bins Bins kurva epidemik.
#' @param inkubasi Hasil [ringkasan_inkubasi()].
#' @param ada_waktu_paparan Apakah waktu paparan tersedia.
#' @param inkubasi_acuan Daftar `list(min, maks)` masa inkubasi rujukan penyakit.
#' @return Daftar berisi `tipe` dan `alasan`.
#' @export
klasifikasi_kurva <- function(bins, inkubasi, ada_waktu_paparan, inkubasi_acuan = NULL) {
  total <- sum(bins$kasus)
  if (total < 3) {
    return(list(tipe = "tidak_dapat_ditentukan",
                alasan = "Jumlah kasus dengan tanggal onset terlalu sedikit untuk menilai bentuk kurva."))
  }
  terisi <- which(bins$kasus > 0)
  durasi <- as.numeric(difftime(bins$selesai[max(terisi)], bins$mulai[min(terisi)], units = "hours"))
  selisih <- if (!is.null(inkubasi_acuan)) {
    inkubasi_acuan$maks - inkubasi_acuan$min
  } else if (is.finite(inkubasi$min_jam) && is.finite(inkubasi$maks_jam)) {
    inkubasi$maks_jam - inkubasi$min_jam
  } else {
    NA_real_
  }

  if (ada_waktu_paparan && is.finite(selisih) && is.finite(durasi)) {
    if (durasi <= selisih * 1.5) {
      return(list(tipe = "common_source", alasan = sprintf(
        paste("Seluruh kasus muncul dalam %s jam, tidak melebihi selisih masa inkubasi terpanjang",
              "dan terpendek (%s jam). Pola ini sesuai sumber tunggal (common source) sebagaimana",
              "lazim pada KLB keracunan pangan."),
        angka_id(durasi), angka_id(selisih))))
    }
    return(list(tipe = "propagated_source", alasan = sprintf(
      paste("Periode KLB (%s jam) melebihi selisih masa inkubasi terpanjang dan terpendek (%s jam),",
            "sehingga penularan berkelanjutan dari orang ke orang perlu dipertimbangkan."),
      angka_id(durasi), angka_id(selisih))))
  }

  kasus <- bins$kasus
  sebelum <- c(0, utils::head(kasus, -1))
  sesudah <- c(utils::tail(kasus, -1), 0)
  puncak <- sum(kasus > 0 & kasus >= sebelum & kasus >= sesudah)
  bin_terisi <- sum(kasus > 0)
  if (bin_terisi <= 3) {
    return(list(tipe = "common_source", alasan = sprintf(
      paste("Kasus terkonsentrasi pada %d interval waktu sehingga menyerupai paparan sumber tunggal.",
            "Konfirmasi dengan data waktu paparan bila tersedia."), bin_terisi)))
  }
  if (puncak >= 2) {
    return(list(tipe = "propagated_source", alasan = sprintf(
      "Kurva menunjukkan %d puncak yang terpisah dan kasus tersebar pada %d interval waktu, pola yang sesuai penularan orang ke orang.",
      puncak, bin_terisi)))
  }
  list(tipe = "tidak_dapat_ditentukan", alasan = paste(
    "Bentuk kurva belum dapat ditentukan secara otomatis karena waktu paparan tidak tersedia",
    "dan pola puncak tidak khas. Penilaian akhir diserahkan pada penyelidik."))
}

#' Perkiraan periode paparan
#'
#' Menghitung mundur dari kasus pertama dengan masa inkubasi terpendek dan dari
#' kasus terakhir dengan masa inkubasi terpanjang, sesuai Pedoman KLB 2020.
#'
#' @param kasus_pertama,kasus_terakhir Waktu onset kasus pertama dan terakhir.
#' @param inkubasi Hasil [ringkasan_inkubasi()].
#' @param inkubasi_acuan Masa inkubasi rujukan penyakit bila tersedia.
#' @return Daftar `list(mulai, selesai)` atau `NULL`.
#' @export
periode_paparan <- function(kasus_pertama, kasus_terakhir, inkubasi, inkubasi_acuan = NULL) {
  mn <- if (!is.null(inkubasi_acuan)) inkubasi_acuan$min else inkubasi$min_jam
  mx <- if (!is.null(inkubasi_acuan)) inkubasi_acuan$maks else inkubasi$maks_jam
  if (is.na(kasus_pertama) || is.na(kasus_terakhir) || !is.finite(mn) || !is.finite(mx)) return(NULL)
  list(mulai = kasus_pertama - mn * 3600, selesai = kasus_terakhir - mx * 3600)
}
