#' Analisis 2x2 satu variabel paparan
#'
#' Menghitung attack rate kelompok terpapar dan tidak terpapar, attack rate
#' ratio, serta RR (desain kohort) atau OR (desain kasus kontrol) dengan selang
#' kepercayaan 95 persen. Uji chi-square dipakai bila seluruh frekuensi harapan
#' minimal 5, selain itu dipakai uji Fisher exact. Bila terdapat sel bernilai
#' nol, estimasi memakai koreksi 0,5 (Haldane-Anscombe).
#'
#' @param paparan Vektor 0 atau 1 status paparan.
#' @param sakit Vektor 0 atau 1 status kasus.
#' @param desain `"kohort"` atau `"kasus_kontrol"`.
#' @return `data.frame` satu baris hasil analisis.
#' @export
#' @examples
#' analisis_2x2(c(1,1,1,0,0,0), c(1,1,0,0,0,1))
analisis_2x2 <- function(paparan, sakit, desain = c("kohort", "kasus_kontrol")) {
  desain <- match.arg(desain)
  a <- sum(paparan == 1 & sakit == 1, na.rm = TRUE)
  b <- sum(paparan == 1 & sakit == 0, na.rm = TRUE)
  cc <- sum(paparan == 0 & sakit == 1, na.rm = TRUE)
  d <- sum(paparan == 0 & sakit == 0, na.rm = TRUE)

  tab <- matrix(c(a, b, cc, d), nrow = 2, byrow = TRUE)
  p <- NA_real_
  uji <- "tidak dapat dihitung"
  if (sum(tab) > 0 && all(rowSums(tab) > 0) && all(colSums(tab) > 0)) {
    harapan <- outer(rowSums(tab), colSums(tab)) / sum(tab)
    if (all(harapan >= 5)) {
      p <- suppressWarnings(stats::chisq.test(tab, correct = FALSE)$p.value)
      uji <- "chi-square"
    } else {
      p <- stats::fisher.test(tab)$p.value
      uji <- "Fisher exact"
    }
  }

  koreksi <- any(c(a, b, cc, d) == 0)
  a2 <- a; b2 <- b; c2 <- cc; d2 <- d
  if (koreksi) {
    a2 <- a + 0.5; b2 <- b + 0.5; c2 <- cc + 0.5; d2 <- d + 0.5
  }

  ar1 <- if (a + b > 0) a / (a + b) * 100 else NA_real_
  ar0 <- if (cc + d > 0) cc / (cc + d) * 100 else NA_real_
  arr <- if (is.finite(ar0) && ar0 > 0) ar1 / ar0 else NA_real_

  if (desain == "kasus_kontrol") {
    est <- (a2 * d2) / (b2 * c2)
    se <- sqrt(1 / a2 + 1 / b2 + 1 / c2 + 1 / d2)
    ukuran <- "OR"
  } else {
    r1 <- a2 / (a2 + b2)
    r0 <- c2 / (c2 + d2)
    est <- r1 / r0
    se <- sqrt(1 / a2 - 1 / (a2 + b2) + 1 / c2 - 1 / (c2 + d2))
    ukuran <- "RR"
  }

  data.frame(
    terpapar_sakit = a, terpapar_tidak_sakit = b, terpapar_total = a + b,
    ar_terpapar = ar1,
    tak_terpapar_sakit = cc, tak_terpapar_tidak_sakit = d, tak_terpapar_total = cc + d,
    ar_tak_terpapar = ar0, arr = arr,
    ukuran = ukuran, estimasi = est,
    ci_bawah = exp(log(est) - 1.96 * se), ci_atas = exp(log(est) + 1.96 * se),
    p_value = p, uji = uji, koreksi = koreksi,
    stringsAsFactors = FALSE
  )
}

#' Analisis bivariat seluruh variabel paparan
#' @param df Tabel hasil [bangun_dataset()].
#' @param paparan `data.frame` kolom dan label paparan.
#' @param desain Desain studi.
#' @return `data.frame` hasil analisis per variabel.
#' @export
analisis_paparan <- function(df, paparan, desain = "kohort") {
  if (nrow(paparan) == 0) return(NULL)
  hasil <- lapply(seq_len(nrow(paparan)), function(i) {
    baris <- analisis_2x2(df[[paparan$kolom[i]]], df$sakit, desain)
    cbind(data.frame(variabel = paparan$kolom[i], label = paparan$label[i],
                     stringsAsFactors = FALSE), baris)
  })
  do.call(rbind, hasil)
}

#' Analisis multivariabel variabel paparan
#'
#' Desain kohort memakai regresi Poisson dengan galat baku robust sehingga
#' estimasi dilaporkan sebagai adjusted risk ratio. Desain kasus kontrol
#' memakai regresi logistik dan menghasilkan adjusted odds ratio. Kandidat
#' variabel diambil dari analisis bivariat dengan nilai p di bawah ambang.
#'
#' @param df Tabel hasil [bangun_dataset()].
#' @param paparan `data.frame` kolom dan label paparan.
#' @param bivariat Hasil [analisis_paparan()].
#' @param konfigurasi Konfigurasi analisis.
#' @return Daftar berisi `model`, `tabel`, dan `catatan`.
#' @export
analisis_multivariabel <- function(df, paparan, bivariat, konfigurasi = konfigurasi_analisis()) {
  kosong <- list(model = "tidak_dijalankan", tabel = NULL, catatan = "", peringatan = character(0))
  if (is.null(bivariat) || nrow(bivariat) == 0) return(kosong)

  n_kasus <- sum(df$sakit == 1, na.rm = TRUE)
  ambang <- konfigurasi$ambang_kandidat
  kandidat <- bivariat$variabel[is.finite(bivariat$p_value) & bivariat$p_value < ambang]
  kandidat <- kandidat[vapply(kandidat, function(k) {
    v <- df[[k]]
    length(unique(v[!is.na(v)])) > 1
  }, logical(1))]

  if (length(kandidat) == 0) {
    kosong$catatan <- sprintf(
      "Tidak ada variabel dengan nilai p di bawah %s sehingga model multivariabel tidak dijalankan.",
      angka_id(ambang, 2))
    return(kosong)
  }
  if (n_kasus < 10) {
    kosong$catatan <- "Jumlah kasus kurang dari 10 sehingga model multivariabel tidak dijalankan."
    return(kosong)
  }

  dat <- df[, c("sakit", kandidat), drop = FALSE]
  dat <- dat[stats::complete.cases(dat), , drop = FALSE]
  rumus <- stats::as.formula(paste("sakit ~", paste(kandidat, collapse = " + ")))

  # Peringatan model dikumpulkan, bukan dibungkam, karena masalah seperti
  # kovarians robust yang mendekati singular perlu diketahui analis.
  peringatan_model <- character(0)
  kumpulkan <- function(w) {
    peringatan_model <<- c(peringatan_model, conditionMessage(w))
    invokeRestart("muffleWarning")
  }

  hasil <- tryCatch(
    withCallingHandlers({
      if (identical(konfigurasi$desain, "kasus_kontrol")) {
        fit <- stats::glm(rumus, family = stats::binomial(), data = dat)
        list(fit = fit, vcov = stats::vcov(fit), model = "logistik")
      } else {
        fit <- stats::glm(rumus, family = stats::poisson(link = "log"), data = dat)
        list(fit = fit, vcov = sandwich::vcovHC(fit, type = "HC0"), model = "poisson_robust")
      }
    }, warning = kumpulkan),
    error = function(e) list(galat = conditionMessage(e)))

  if (!is.null(hasil$galat)) {
    kosong$catatan <- paste("Model gagal dijalankan:", hasil$galat)
    return(kosong)
  }

  uji <- lmtest::coeftest(hasil$fit, vcov. = hasil$vcov)
  nama <- rownames(uji)
  pilih <- nama != "(Intercept)"
  est <- uji[pilih, "Estimate"]
  se <- uji[pilih, "Std. Error"]
  label <- paparan$label[match(nama[pilih], paparan$kolom)]

  tabel <- data.frame(
    variabel = nama[pilih],
    label = ifelse(is.na(label), nama[pilih], label),
    estimasi = exp(est),
    ci_bawah = exp(est - 1.96 * se),
    ci_atas = exp(est + 1.96 * se),
    p_value = uji[pilih, 4],
    stringsAsFactors = FALSE
  )

  catatan <- if (identical(hasil$model, "poisson_robust")) {
    sprintf(paste("Regresi Poisson dengan galat baku robust (sandwich HC0); estimasi dilaporkan",
                  "sebagai adjusted risk ratio. Kandidat variabel: nilai p di bawah %s pada analisis bivariat."),
            angka_id(ambang, 2))
  } else {
    sprintf(paste("Regresi logistik; estimasi dilaporkan sebagai adjusted odds ratio.",
                  "Kandidat variabel: nilai p di bawah %s pada analisis bivariat."),
            angka_id(ambang, 2))
  }

  peringatan <- character(0)
  if (any(grepl("singular", peringatan_model, ignore.case = TRUE))) {
    peringatan <- c(peringatan, paste(
      "Matriks kovarians robust pada model multivariabel mendekati singular karena sebagian",
      "pengamatan memiliki leverage mendekati satu. Selang kepercayaan pada model ini dapat",
      "terlalu sempit atau terlalu lebar, sehingga hasil bivariat lebih dapat diandalkan."))
  }
  if (any(grepl("did not converge|fitted rates numerically 0|fitted probabilities numerically 0",
                peringatan_model, ignore.case = TRUE))) {
    peringatan <- c(peringatan, paste(
      "Model multivariabel tidak konvergen sempurna atau menunjukkan pemisahan sempurna",
      "(separation). Estimasi dan selang kepercayaannya perlu ditafsirkan dengan sangat hati-hati."))
  }

  list(model = hasil$model, tabel = tabel, catatan = catatan, peringatan = peringatan)
}

#' Diagnosis banding etiologi
#'
#' Penyingkiran mengikuti Pedoman KLB 2020 Bab Keracunan Pangan:
#' masa inkubasi KLB terpendek yang lebih pendek dari batas bawah agen,
#' masa inkubasi KLB terpanjang yang melebihi batas atas agen, atau periode KLB
#' yang melebihi selisih masa inkubasi agen.
#'
#' @param gejala `data.frame` distribusi gejala hasil analisis.
#' @param inkubasi Hasil [ringkasan_inkubasi()].
#' @param periode_klb_jam Panjang periode KLB dalam jam.
#' @param rujukan Tabel rujukan agen, bawaan [rujukan_patogen()].
#' @return `data.frame` hasil diagnosis banding.
#' @export
diagnosis_banding <- function(gejala, inkubasi, periode_klb_jam, rujukan = rujukan_patogen()) {
  utama <- character(0)
  if (!is.null(gejala) && nrow(gejala) > 0) {
    kandidat <- gejala$kategori[gejala$persen >= 20]
    utama <- stats::na.omit(vapply(kandidat, normalkan_gejala, character(1)))
  }

  baris <- lapply(seq_len(nrow(rujukan)), function(i) {
    ref <- rujukan[i, ]
    alasan <- character(0)
    disingkirkan <- FALSE
    if (is.finite(inkubasi$min_jam) && inkubasi$min_jam < ref$inkubasi_min_jam) {
      disingkirkan <- TRUE
      alasan <- c(alasan, sprintf(
        "Masa inkubasi KLB terpendek (%s jam) lebih pendek dari masa inkubasi terpendek agen (%s jam).",
        angka_id(inkubasi$min_jam), angka_id(ref$inkubasi_min_jam, 2)))
    }
    if (is.finite(inkubasi$maks_jam) && inkubasi$maks_jam > ref$inkubasi_maks_jam) {
      disingkirkan <- TRUE
      alasan <- c(alasan, sprintf(
        "Masa inkubasi KLB terpanjang (%s jam) melebihi masa inkubasi terpanjang agen (%s jam).",
        angka_id(inkubasi$maks_jam), angka_id(ref$inkubasi_maks_jam, 2)))
    }
    if (is.finite(periode_klb_jam) && periode_klb_jam > (ref$inkubasi_maks_jam - ref$inkubasi_min_jam)) {
      disingkirkan <- TRUE
      alasan <- c(alasan, sprintf(
        "Periode KLB (%s jam) melebihi selisih masa inkubasi agen (%s jam).",
        angka_id(periode_klb_jam), angka_id(ref$inkubasi_maks_jam - ref$inkubasi_min_jam)))
    }
    gejala_ref <- strsplit(ref$gejala, "|", fixed = TRUE)[[1]]
    cocok <- sum(utama %in% gejala_ref)
    if (!disingkirkan && length(alasan) == 0) {
      alasan <- "Masa inkubasi KLB masih berada dalam rentang masa inkubasi agen ini."
    }
    data.frame(
      agen = ref$nama,
      inkubasi_min_jam = ref$inkubasi_min_jam,
      inkubasi_maks_jam = ref$inkubasi_maks_jam,
      cocok_gejala = cocok,
      total_gejala = length(gejala_ref),
      status = if (disingkirkan) "disingkirkan" else "belum disingkirkan",
      alasan = paste(alasan, collapse = " "),
      sumber = ref$sumber,
      perlu_verifikasi = ref$perlu_verifikasi,
      stringsAsFactors = FALSE
    )
  })
  hasil <- do.call(rbind, baris)
  hasil[order(hasil$status != "belum disingkirkan", -hasil$cocok_gejala), ]
}

#' Penilaian tujuh kriteria KLB
#'
#' @param pembanding Data pembanding surveilans, lihat [pembanding_kosong()].
#' @param ringkasan Daftar berisi `total_kasus` dan `cfr`.
#' @param bins Bins kurva epidemik.
#' @return `data.frame` hasil penilaian tiap kriteria.
#' @export
nilai_kriteria_klb <- function(pembanding, ringkasan, bins) {
  kr <- kriteria_klb()
  # Nilai pembanding dapat berasal dari JSON sehingga perlu dipaksa ke numerik.
  ambil <- function(x) {
    if (is.null(x) || length(x) == 0) return(NULL)
    if (is.logical(x)) return(if (is.na(x[1])) NULL else x[1])
    nilai <- suppressWarnings(as.numeric(x[1]))
    if (!is.finite(nilai)) NULL else nilai
  }

  hasil <- lapply(seq_len(nrow(kr)), function(i) {
    kode <- kr$kode[i]
    terpenuhi <- NA
    ket <- "Data pembanding belum diisi."

    if (kode == "a") {
      baru <- ambil(pembanding$penyakit_baru)
      if (!is.null(baru)) {
        terpenuhi <- isTRUE(baru)
        ket <- if (terpenuhi) {
          "Penyakit dinyatakan tidak pernah ada atau tidak dikenal sebelumnya di wilayah ini."
        } else "Penyakit sudah pernah ada di wilayah ini."
      }
    } else if (kode == "b") {
      if (nrow(bins) >= 3) {
        k <- bins$kasus
        naik <- any(vapply(seq(3, length(k)), function(j) k[j] > k[j - 1] && k[j - 1] > k[j - 2], logical(1)))
        terpenuhi <- naik
        ket <- if (naik) {
          "Terdapat peningkatan kasus pada tiga interval waktu berturut-turut pada kurva epidemik."
        } else "Tidak ditemukan peningkatan kasus tiga interval berturut-turut pada kurva epidemik."
      } else {
        ket <- "Interval kurva epidemik kurang dari tiga sehingga kriteria tidak dapat dinilai."
      }
    } else if (kode == "c") {
      v <- ambil(pembanding$kasus_periode_sebelumnya)
      if (!is.null(v)) {
        terpenuhi <- if (v == 0) ringkasan$total_kasus > 0 else ringkasan$total_kasus >= 2 * v
        ket <- sprintf("Kasus periode berjalan %s dibanding %s pada periode sebelumnya.",
                       ringkasan$total_kasus, v)
      }
    } else if (kode == "d") {
      v <- ambil(pembanding$rata_bulan_tahun_lalu)
      if (!is.null(v)) {
        terpenuhi <- if (v == 0) ringkasan$total_kasus > 0 else ringkasan$total_kasus >= 2 * v
        ket <- sprintf("Penderita baru %s dibanding rata-rata %s kasus per bulan pada tahun sebelumnya.",
                       ringkasan$total_kasus, v)
      }
    } else if (kode == "e") {
      ini <- ambil(pembanding$rata_bulan_tahun_ini)
      lalu <- ambil(pembanding$rata_bulan_tahun_lalu)
      if (!is.null(ini) && !is.null(lalu)) {
        terpenuhi <- if (lalu == 0) ini > 0 else ini >= 2 * lalu
        ket <- sprintf("Rata-rata %s kasus per bulan tahun ini dibanding %s kasus per bulan tahun sebelumnya.",
                       ini, lalu)
      }
    } else if (kode == "f") {
      lalu <- ambil(pembanding$cfr_periode_sebelumnya)
      if (!is.null(lalu) && is.finite(ringkasan$cfr)) {
        terpenuhi <- if (lalu == 0) ringkasan$cfr > 0 else ringkasan$cfr >= 1.5 * lalu
        ket <- sprintf("CFR periode ini %s persen dibanding %s persen pada periode sebelumnya.",
                       angka_id(ringkasan$cfr), lalu)
      } else if (!is.finite(ringkasan$cfr)) {
        ket <- "CFR periode berjalan belum dapat dihitung karena variabel kematian belum dipetakan."
      }
    } else if (kode == "g") {
      ini <- ambil(pembanding$proporsi_periode_ini)
      lalu <- ambil(pembanding$proporsi_periode_sebelumnya)
      if (!is.null(ini) && !is.null(lalu)) {
        terpenuhi <- if (lalu == 0) ini > 0 else ini >= 2 * lalu
        ket <- sprintf("Proporsi penyakit periode ini %s persen dibanding %s persen pada periode sebelumnya.",
                       ini, lalu)
      }
    }

    data.frame(kode = kode, uraian = kr$uraian[i], terpenuhi = terpenuhi,
               keterangan = ket, stringsAsFactors = FALSE)
  })
  do.call(rbind, hasil)
}
