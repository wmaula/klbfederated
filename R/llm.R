#' Aturan sistem untuk model bahasa
#'
#' Model hanya boleh menyusun narasi dari daftar fakta hasil analisis. Aturan
#' ini dikirim sebagai pesan sistem kepada WebLLM di peramban.
#'
#' @return Teks aturan sistem.
#' @export
sistem_llm <- function() {
  paste(
    "Anda adalah epidemiolog lapangan yang menulis laporan penyelidikan kejadian luar biasa dalam bahasa Indonesia baku dan formal.",
    "Aturan yang tidak boleh dilanggar:",
    "1. Gunakan hanya angka, nama, tanggal dan istilah yang tercantum pada DAFTAR FAKTA. Dilarang keras menambah, membulatkan ulang, atau mengarang angka.",
    "2. Jangan menambahkan sitasi, nama jurnal, atau referensi apa pun.",
    "3. Bila sebuah informasi tidak ada pada DAFTAR FAKTA, tulis penanda dalam kurung siku agar diisi penyelidik, contoh [perlu dilengkapi].",
    "4. Tulis dalam paragraf mengalir, bukan daftar butir, kecuali diminta.",
    "5. Jangan memakai tanda pisah panjang dalam kalimat.",
    "6. Investigasi KLB hanya menghasilkan dugaan. Gunakan kata diduga atau kemungkinan, dan jangan menulis bahwa penyebab sudah dapat dipastikan kecuali daftar fakta menyebut konfirmasi laboratorium.",
    "7. Jangan memberi komentar tentang tugas ini, langsung tulis naskahnya.",
    sep = "\n"
  )
}

#' Daftar fakta hasil analisis untuk model bahasa
#'
#' Menyusun seluruh angka hasil analisis menjadi daftar fakta. Model bahasa
#' hanya boleh memakai angka yang ada pada daftar ini.
#'
#' @param hasil Objek `klb_hasil`.
#' @param keterangan Identitas investigasi.
#' @param meta Metadata laporan.
#' @return Teks daftar fakta.
#' @export
daftar_fakta <- function(hasil, keterangan, meta = metadata_laporan()) {
  r <- hasil$ringkasan
  baris <- c(
    sprintf("Judul investigasi: %s", keterangan$nama),
    sprintf("Jenis KLB: %s", keterangan$jenis),
    sprintf("Penyakit atau sindrom: %s", if (!is.null(keterangan$penyakit)) keterangan$penyakit else "belum ditetapkan"),
    sprintf("Lokasi: %s", lokasi_teks(keterangan)),
    sprintf("Tanggal laporan diterima: %s", if (!is.null(keterangan$tanggal_lapor)) keterangan$tanggal_lapor else "belum tersedia"),
    sprintf("Populasi berisiko: %s", r$populasi_berisiko),
    sprintf("Jumlah diinvestigasi: %s", r$total_diinvestigasi),
    sprintf("Jumlah kasus: %s", r$total_kasus),
    sprintf("Jumlah meninggal: %s", r$total_meninggal),
    sprintf("Attack rate: %s persen", angka_id(r$attack_rate)),
    sprintf("Case fatality rate: %s", if (is.finite(r$cfr)) paste(angka_id(r$cfr), "persen") else "tidak dihitung"),
    sprintf("Onset kasus pertama: %s", format(r$tanggal_kasus_pertama, "%Y-%m-%d %H:%M")),
    sprintf("Onset kasus terakhir: %s", format(r$tanggal_kasus_terakhir, "%Y-%m-%d %H:%M"))
  )

  if (!is.null(hasil$gejala)) {
    baris <- c(baris, sprintf("Distribusi gejala pada kasus: %s",
      paste(sprintf("%s %s kasus (%s persen)", hasil$gejala$kategori, hasil$gejala$n,
                    angka_id(hasil$gejala$persen)), collapse = "; ")))
  }
  ar_teks <- function(tab, nama) {
    if (is.null(tab)) return(NULL)
    sprintf("Attack rate menurut %s: %s", nama,
            paste(sprintf("%s %s/%s (%s persen)", tab$kategori, tab$kasus, tab$populasi,
                          angka_id(tab$ar)), collapse = "; "))
  }
  baris <- c(baris, ar_teks(hasil$ar_jenis_kelamin, "jenis kelamin"),
             ar_teks(hasil$ar_umur, "kelompok umur"), ar_teks(hasil$ar_tempat, "tempat"))
  for (nama in names(hasil$ar_lain)) baris <- c(baris, ar_teks(hasil$ar_lain[[nama]], tolower(nama)))
  for (nama in names(hasil$freq_kasus)) {
    tb <- hasil$freq_kasus[[nama]]
    baris <- c(baris, sprintf("Distribusi kasus menurut %s: %s", tolower(nama),
      paste(sprintf("%s %s (%s persen)", tb$kategori, tb$n, angka_id(tb$persen)), collapse = "; ")))
  }

  if (is.finite(hasil$inkubasi$min_jam)) {
    baris <- c(baris, sprintf(
      "Masa inkubasi: terpendek %s jam, terpanjang %s jam, rata-rata %s jam, median %s jam, dihitung dari %s kasus",
      angka_id(hasil$inkubasi$min_jam, 2), angka_id(hasil$inkubasi$maks_jam, 2),
      angka_id(hasil$inkubasi$rata_jam, 2), angka_id(hasil$inkubasi$median_jam, 2), hasil$inkubasi$n))
  }
  if (!is.null(hasil$inkubasi$periode_paparan)) {
    baris <- c(baris, sprintf("Perkiraan periode paparan: %s sampai %s",
      format(hasil$inkubasi$periode_paparan$mulai, "%Y-%m-%d %H:%M"),
      format(hasil$inkubasi$periode_paparan$selesai, "%Y-%m-%d %H:%M")))
  }
  baris <- c(baris, sprintf("Kurva epidemik: interval %s jam, tipe %s, alasan: %s",
                            angka_id(hasil$kurva$interval_jam, 2), hasil$kurva$tipe, hasil$kurva$alasan_tipe))
  if (nrow(hasil$kurva$bins) > 0) {
    puncak <- hasil$kurva$bins[which.max(hasil$kurva$bins$kasus), ]
    baris <- c(baris, sprintf("Puncak kurva epidemik: interval mulai %s dengan %s kasus",
                              format(puncak$mulai, "%Y-%m-%d %H:%M"), puncak$kasus))
  }

  if (!is.null(hasil$paparan)) {
    p <- hasil$paparan
    baris <- c(baris, sprintf("Analisis paparan: %s", paste(sprintf(
      "%s: AR terpapar %s persen, AR tidak terpapar %s persen, ARR %s, %s %s (95 persen CI %s sampai %s), p %s, uji %s",
      p$label, angka_id(p$ar_terpapar), angka_id(p$ar_tak_terpapar), angka_id(p$arr, 2),
      p$ukuran, angka_id(p$estimasi, 2), angka_id(p$ci_bawah, 2), angka_id(p$ci_atas, 2),
      vapply(p$p_value, p_teks, character(1)), p$uji), collapse = "; ")))
  }
  if (!is.null(hasil$multivariabel$tabel)) {
    m <- hasil$multivariabel$tabel
    baris <- c(baris, sprintf("Analisis multivariabel (%s): %s", hasil$multivariabel$model,
      paste(sprintf("%s %s (95 persen CI %s sampai %s), p %s", m$label, angka_id(m$estimasi, 2),
                    angka_id(m$ci_bawah, 2), angka_id(m$ci_atas, 2),
                    vapply(m$p_value, p_teks, character(1))), collapse = "; ")))
  }
  if (!is.null(hasil$diagnosis_banding)) {
    d <- hasil$diagnosis_banding
    belum <- d$agen[d$status == "belum disingkirkan"]
    baris <- c(baris, sprintf("Diagnosis banding, agen belum disingkirkan: %s. Agen disingkirkan: %s",
                              if (length(belum)) paste(belum, collapse = ", ") else "tidak ada",
                              paste(d$agen[d$status == "disingkirkan"], collapse = ", ")))
  }
  kr <- hasil$kriteria_klb
  terpenuhi <- kr[!is.na(kr$terpenuhi) & kr$terpenuhi, ]
  baris <- c(baris, sprintf("Kriteria KLB Permenkes 1501 Tahun 2010 yang terpenuhi: %s",
    if (nrow(terpenuhi) > 0) paste(sprintf("%s (%s)", terpenuhi$kode, terpenuhi$keterangan), collapse = "; ")
    else "belum ada yang dinilai terpenuhi"))

  tambahan <- c(
    if (nzchar(meta$patogen_diduga)) sprintf("Patogen atau agen yang diduga: %s", meta$patogen_diduga),
    if (nzchar(meta$sumber_diduga)) sprintf("Sumber penularan yang diduga: %s", meta$sumber_diduga),
    if (nzchar(meta$hasil_laboratorium)) sprintf("Hasil laboratorium: %s", meta$hasil_laboratorium),
    if (nzchar(meta$studi_lingkungan)) sprintf("Temuan studi lingkungan: %s", meta$studi_lingkungan),
    if (nzchar(meta$upaya_pengendalian)) sprintf("Upaya pengendalian: %s", meta$upaya_pengendalian),
    if (nzchar(meta$definisi_kasus)) sprintf("Definisi kasus: %s", meta$definisi_kasus),
    if (length(hasil$peringatan)) sprintf("Catatan keterbatasan analisis: %s", paste(hasil$peringatan, collapse = " "))
  )
  baris <- c(baris, tambahan)
  paste(paste0("- ", baris[!is.na(baris)]), collapse = "\n")
}

#' Perintah penulisan satu bab laporan
#'
#' @param bagian Kunci bab, misalnya `"intisari"` atau `"hasil"`.
#' @param fakta Daftar fakta dari [daftar_fakta()].
#' @param draf Draf template bab yang bersangkutan.
#' @return Teks perintah untuk model bahasa.
#' @export
perintah_bagian <- function(bagian, fakta, draf) {
  instruksi <- switch(
    bagian,
    intisari = "Tulis INTISARI laporan KLB dengan empat alinea berlabel Latar Belakang, Metode, Hasil, dan Kesimpulan. Panjang total 250 sampai 300 kata.",
    abstract = "Write the English ABSTRACT of this outbreak investigation with four labelled paragraphs: Background, Methods, Results, Conclusion. Total length 250 to 300 words. Use only the numbers listed in the fact list. Do not invent references.",
    pendahuluan = "Tulis bab PENDAHULUAN yang memuat kronologi awal laporan, besaran masalah, dan alasan penyelidikan dilakukan. Tiga alinea.",
    metode = "Tulis bab METODOLOGI yang memuat desain penyelidikan, batas wilayah, populasi berisiko, definisi kasus, cara pengumpulan data, dan cara analisis.",
    hasil = "Tulis bab HASIL PENYELIDIKAN yang menguraikan temuan menurut orang, tempat dan waktu, diikuti analisis paparan dan multivariabel, lalu diagnosis banding dan penetapan kriteria KLB. Rujuk tabel dengan penomoran Tabel 1 dan seterusnya serta kurva epidemik sebagai Gambar 1. Sampaikan temuan tanpa menafsirkan berlebihan.",
    pembahasan = "Tulis bab PEMBAHASAN yang menafsirkan temuan: agen dan sumber yang diduga, kesesuaian masa inkubasi dan bentuk kurva epidemik, kelompok berisiko, serta keterbatasan investigasi. Beri penanda dalam kurung siku pada tempat yang memerlukan perbandingan dengan literatur.",
    kesimpulan = "Tulis KESIMPULAN dalam bentuk daftar bernomor yang menjawab tujuan khusus penyelidikan.",
    rekomendasi = "Tulis REKOMENDASI dalam daftar bernomor yang operasional dan dapat dikerjakan dinas kesehatan serta pihak terkait.",
    sprintf("Tulis bagian %s laporan KLB.", bagian)
  )
  paste(instruksi, "",
        "DAFTAR FAKTA (satu-satunya sumber angka yang boleh dipakai):", fakta, "",
        "DRAF AWAL (perbaiki gaya bahasanya, pertahankan seluruh angka apa adanya):", draf,
        sep = "\n")
}

#' Memeriksa naskah keluaran model bahasa
#'
#' Memeriksa dua risiko: angka yang tidak ada pada daftar fakta, dan kalimat
#' yang menyatakan kepastian penyebab padahal investigasi hanya menghasilkan
#' dugaan.
#'
#' @param teks Naskah keluaran model.
#' @param fakta Daftar fakta yang diberikan kepada model.
#' @return Daftar berisi `angka_asing`, `kalimat_terlalu_pasti`, dan `bersih`.
#' @export
#' @examples
#' periksa_narasi("Attack rate 78,9 persen pada 165 kasus.",
#'                "- Attack rate: 78,9 persen\n- Jumlah kasus: 165")
periksa_narasi <- function(teks, fakta) {
  ambil_angka <- function(x) {
    cocok <- unlist(regmatches(x, gregexpr("\\d+(?:[.,]\\d+)*", x)))
    cocok <- gsub("\\.(?=\\d{3}\\b)", "", cocok, perl = TRUE)
    suppressWarnings(as.numeric(gsub(",", ".", cocok)))
  }
  fakta_angka <- stats::na.omit(ambil_angka(fakta))
  teks_angka <- unique(stats::na.omit(ambil_angka(teks)))

  asing <- teks_angka[vapply(teks_angka, function(a) {
    if (a %% 1 == 0 && a <= 12) return(FALSE)
    !any(abs(fakta_angka - a) < 0.051)
  }, logical(1))]

  frasa <- c("dapat dipastikan", "terbukti secara pasti", "sudah pasti",
             "jelas disebabkan", "penyebabnya adalah", "terkonfirmasi sebagai penyebab",
             "tidak diragukan")
  kalimat <- unlist(strsplit(teks, "(?<=[.!?])\\s+", perl = TRUE))
  terlalu <- kalimat[vapply(kalimat, function(k) {
    any(vapply(frasa, function(f) grepl(f, tolower(k), fixed = TRUE), logical(1)))
  }, logical(1))]

  list(
    angka_asing = sort(asing),
    kalimat_terlalu_pasti = unname(terlalu),
    bersih = length(asing) == 0 && length(terlalu) == 0
  )
}
