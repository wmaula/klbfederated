#' Menyusun tabel laporan bernomor
#'
#' Susunan tabel mengikuti Checklist Laporan KLB Keracunan Makanan.
#'
#' @param hasil Objek `klb_hasil`.
#' @param keterangan Identitas investigasi.
#' @return Daftar tabel, masing-masing berisi `nomor`, `judul`, `data`, dan `catatan`.
#' @export
tabel_laporan <- function(hasil, keterangan) {
  lokasi <- lokasi_teks(keterangan)
  tabel <- list()
  tambah <- function(judul, data, catatan = NULL) {
    if (is.null(data) || nrow(data) == 0) return(invisible(NULL))
    tabel[[length(tabel) + 1]] <<- list(nomor = length(tabel) + 1, judul = judul,
                                        data = data, catatan = catatan)
  }
  fmt <- function(x, d = 2) ifelse(is.finite(x), sub("\\.", ",", formatC(x, format = "f", digits = d)), "-")

  if (!is.null(hasil$gejala)) {
    tambah(sprintf("Distribusi kasus menurut tanda dan gejala di %s", lokasi),
           data.frame(`Tanda dan gejala` = hasil$gejala$kategori,
                      `Jumlah kasus` = hasil$gejala$n,
                      `Persentase` = fmt(hasil$gejala$persen),
                      check.names = FALSE, stringsAsFactors = FALSE))
  }
  ar_tab <- function(tab, kolom1) {
    data.frame(setNames(list(tab$kategori), kolom1),
               `Populasi berisiko` = tab$populasi, `Jumlah kasus` = tab$kasus,
               `Attack rate` = fmt(tab$ar), check.names = FALSE, stringsAsFactors = FALSE)
  }
  if (!is.null(hasil$ar_jenis_kelamin)) {
    tambah(sprintf("Distribusi kasus dan attack rate menurut jenis kelamin di %s", lokasi),
           ar_tab(hasil$ar_jenis_kelamin, "Jenis kelamin"))
  }
  if (!is.null(hasil$ar_umur)) {
    tambah(sprintf("Distribusi kasus dan attack rate menurut kelompok umur di %s", lokasi),
           ar_tab(hasil$ar_umur, "Kelompok umur"))
  }
  if (!is.null(hasil$ar_tempat)) {
    tambah(sprintf("Distribusi kasus dan attack rate menurut tempat di %s", lokasi),
           ar_tab(hasil$ar_tempat, "Tempat"))
  }
  for (nama in names(hasil$ar_lain)) {
    tambah(sprintf("Distribusi kasus dan attack rate menurut %s di %s", tolower(nama), lokasi),
           ar_tab(hasil$ar_lain[[nama]], nama))
  }
  for (nama in names(hasil$freq_kasus)) {
    tb <- hasil$freq_kasus[[nama]]
    tambah(sprintf("Distribusi kasus menurut %s di %s", tolower(nama), lokasi),
           data.frame(setNames(list(tb$kategori), nama), `Jumlah kasus` = tb$n,
                      `Persentase` = fmt(tb$persen), check.names = FALSE, stringsAsFactors = FALSE))
  }

  if (!is.null(hasil$paparan)) {
    p <- hasil$paparan
    tambah(sprintf("Attack rate menurut riwayat paparan dan attack rate ratio di %s", lokasi),
           data.frame(`Variabel paparan` = p$label,
                      `Terpapar sakit` = p$terpapar_sakit,
                      `Terpapar tidak sakit` = p$terpapar_tidak_sakit,
                      `Terpapar total` = p$terpapar_total,
                      `AR terpapar` = fmt(p$ar_terpapar, 1),
                      `Tidak terpapar sakit` = p$tak_terpapar_sakit,
                      `Tidak terpapar total` = p$tak_terpapar_total,
                      `AR tidak terpapar` = fmt(p$ar_tak_terpapar, 1),
                      ARR = fmt(p$arr),
                      check.names = FALSE, stringsAsFactors = FALSE))
    tambah(sprintf("Analisis bivariat variabel paparan dengan kejadian sakit di %s", lokasi),
           data.frame(`Variabel paparan` = p$label,
                      setNames(list(fmt(p$estimasi)), p$ukuran[1]),
                      `95 persen CI` = paste(fmt(p$ci_bawah), "sampai", fmt(p$ci_atas)),
                      `Nilai p` = vapply(p$p_value, p_teks, character(1)),
                      Uji = p$uji, check.names = FALSE, stringsAsFactors = FALSE),
           "Uji chi-square dipakai bila seluruh frekuensi harapan minimal 5, selain itu dipakai uji Fisher exact.")
  }

  if (!is.null(hasil$multivariabel$tabel)) {
    m <- hasil$multivariabel$tabel
    ukuran <- if (identical(hasil$multivariabel$model, "logistik")) "aOR" else "aRR"
    tambah(sprintf("Analisis multivariabel variabel paparan dengan kejadian sakit di %s", lokasi),
           data.frame(Variabel = m$label, setNames(list(fmt(m$estimasi)), ukuran),
                      `95 persen CI` = paste(fmt(m$ci_bawah), "sampai", fmt(m$ci_atas)),
                      `Nilai p` = vapply(m$p_value, p_teks, character(1)),
                      check.names = FALSE, stringsAsFactors = FALSE),
           hasil$multivariabel$catatan)
  }

  if (!is.null(hasil$diagnosis_banding)) {
    d <- hasil$diagnosis_banding
    tambah("Diagnosis banding etiologi berdasarkan masa inkubasi dan gejala",
           data.frame(Agen = d$agen,
                      `Masa inkubasi rujukan (jam)` = paste(fmt(d$inkubasi_min_jam), "sampai", fmt(d$inkubasi_maks_jam)),
                      `Kecocokan gejala` = paste0(d$cocok_gejala, "/", d$total_gejala),
                      Status = d$status, `Dasar penilaian` = d$alasan, `Sumber rujukan` = d$sumber,
                      check.names = FALSE, stringsAsFactors = FALSE),
           paste("Aturan penyingkiran mengikuti Pedoman KLB 2020 Bab Keracunan Pangan.",
                 "Nilai rujukan yang berasal dari compendium umum wajib diverifikasi penyelidik."))
  }

  k <- hasil$kriteria_klb
  tambah("Penilaian kriteria KLB menurut Permenkes 1501 Tahun 2010",
         data.frame(Kriteria = k$kode, Uraian = k$uraian,
                    Status = ifelse(is.na(k$terpenuhi), "Belum dinilai",
                                    ifelse(k$terpenuhi, "Terpenuhi", "Tidak terpenuhi")),
                    Keterangan = k$keterangan, check.names = FALSE, stringsAsFactors = FALSE))
  tabel
}

#' Mengekspor laporan ke berkas Word
#'
#' Menyusun dokumen `.docx` berisi seluruh bab laporan, tabel bernomor, serta
#' gambar kurva epidemik dan attack rate.
#'
#' @param hasil Objek `klb_hasil`.
#' @param keterangan Identitas investigasi.
#' @param naskah Daftar naskah bab dari [susun_draf_laporan()].
#' @param berkas Lokasi berkas keluaran.
#' @return Jalur berkas yang dihasilkan, tidak terlihat.
#' @export
#' @examples
#' contoh <- contoh_keracunan_pangan(n = 60)
#' hasil <- analisis_klb(contoh$data, contoh$pemetaan, contoh$konfigurasi)
#' naskah <- susun_draf_laporan(hasil, contoh$keterangan)
#' berkas <- file.path(tempdir(), "laporan-klb.docx")
#' ekspor_docx(hasil, contoh$keterangan, naskah, berkas)
ekspor_docx <- function(hasil, keterangan, naskah, berkas = "laporan-klb.docx") {
  tabel <- tabel_laporan(hasil, keterangan)
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, keterangan$nama, style = "heading 1")

  simpan_gambar <- function(plot, tinggi = 3.6) {
    tmp <- tempfile(fileext = ".png")
    suppressWarnings(ggplot2::ggsave(tmp, plot, width = 6.5, height = tinggi, dpi = 200))
    tmp
  }

  for (i in seq_len(nrow(bagian_laporan()))) {
    kunci <- bagian_laporan()$kunci[i]
    isi <- naskah[[kunci]]
    if (is.null(isi) || !nzchar(trimws(isi))) next
    doc <- officer::body_add_par(doc, bagian_laporan()$judul[i], style = "heading 2")
    for (baris in strsplit(isi, "\n")[[1]]) {
      if (nzchar(trimws(baris))) doc <- officer::body_add_par(doc, baris, style = "Normal")
    }

    if (identical(kunci, "hasil")) {
      g1 <- simpan_gambar(plot_kurva_epidemik(hasil))
      doc <- officer::body_add_img(doc, g1, width = 6.5, height = 3.6)
      doc <- officer::body_add_par(doc, sprintf(
        "Gambar 1. Kurva epidemik KLB dengan interval %s jam.", angka_id(hasil$kurva$interval_jam, 2)),
        style = "Normal")
      if (!is.null(hasil$ar_umur)) {
        g2 <- simpan_gambar(plot_attack_rate(hasil$ar_umur, "Attack rate menurut kelompok umur"), 3.2)
        doc <- officer::body_add_img(doc, g2, width = 6.5, height = 3.2)
        doc <- officer::body_add_par(doc, "Gambar 2. Attack rate menurut kelompok umur.", style = "Normal")
      }
      if (!is.null(hasil$paparan) && nrow(hasil$paparan) > 0) {
        g3 <- simpan_gambar(plot_paparan(hasil$paparan), 3.4)
        doc <- officer::body_add_img(doc, g3, width = 6.5, height = 3.4)
        doc <- officer::body_add_par(doc, "Gambar 3. Estimasi ukuran asosiasi variabel paparan beserta selang kepercayaan 95 persen.",
                                     style = "Normal")
      }
      for (t in tabel) {
        doc <- officer::body_add_par(doc, sprintf("Tabel %d. %s", t$nomor, t$judul), style = "Normal")
        doc <- officer::body_add_table(doc, t$data, style = "table_template")
        if (!is.null(t$catatan)) doc <- officer::body_add_par(doc, t$catatan, style = "Normal")
        doc <- officer::body_add_par(doc, "", style = "Normal")
      }
    }
  }

  doc <- officer::body_add_par(doc, paste(
    "Disusun dengan package klbfederated. Seluruh angka berasal dari analisis R atas data",
    "investigasi yang tersimpan di node kabupaten atau kota."), style = "Normal")
  print(doc, target = berkas)
  invisible(berkas)
}

#' Mengekspor laporan ke Markdown
#' @inheritParams ekspor_docx
#' @return Teks Markdown, tidak terlihat bila `berkas` diberikan.
#' @export
ekspor_markdown <- function(hasil, keterangan, naskah, berkas = NULL) {
  tabel <- tabel_laporan(hasil, keterangan)
  md_tabel <- function(t) {
    kolom <- names(t$data)
    baris <- apply(t$data, 1, function(r) paste("|", paste(r, collapse = " | "), "|"))
    paste(c(sprintf("**Tabel %d.** %s", t$nomor, t$judul), "",
            paste("|", paste(kolom, collapse = " | "), "|"),
            paste("|", paste(rep("---", length(kolom)), collapse = " | "), "|"),
            baris,
            if (!is.null(t$catatan)) paste0("\n_", t$catatan, "_") else NULL),
          collapse = "\n")
  }
  bagian <- character(0)
  bl <- bagian_laporan()
  for (i in seq_len(nrow(bl))) {
    isi <- naskah[[bl$kunci[i]]]
    if (is.null(isi) || !nzchar(trimws(isi))) next
    bagian <- c(bagian, sprintf("## %s\n\n%s", bl$judul[i], isi))
    if (identical(bl$kunci[i], "hasil")) {
      bagian <- c(bagian, sprintf("**Gambar 1.** Kurva epidemik KLB (interval %s jam)",
                                  angka_id(hasil$kurva$interval_jam, 2)))
      bagian <- c(bagian, vapply(tabel, md_tabel, character(1)))
    }
  }
  teks <- paste(c(sprintf("# %s", keterangan$nama), "", bagian), collapse = "\n\n")
  if (!is.null(berkas)) {
    writeLines(teks, berkas)
    return(invisible(berkas))
  }
  teks
}
