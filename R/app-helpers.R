#' @keywords internal
#' @noRd
kartu_stat <- function(label, nilai, catatan = NULL) {
  htmltools::div(
    class = "klb-stat",
    htmltools::div(class = "label", label),
    htmltools::div(class = "value", nilai),
    if (!is.null(catatan)) htmltools::div(class = "note", catatan)
  )
}

#' @keywords internal
#' @noRd
pil <- function(teks, jenis = "mute") {
  htmltools::span(class = paste("klb-pill", jenis), teks)
}

#' @keywords internal
#' @noRd
tema_app <- function() {
  bslib::bs_theme(
    version = 5, primary = "#16556b", base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter"), font_scale = 0.92
  )
}

#' @keywords internal
#' @noRd
tabel_dt <- function(data, ...) {
  if (is.null(data) || nrow(data) == 0) {
    return(DT::datatable(data.frame(Keterangan = "Belum ada data"), rownames = FALSE,
                         options = list(dom = "t")))
  }
  DT::datatable(data, rownames = FALSE, options = list(dom = "tip", pageLength = 15,
                                                       scrollX = TRUE), ...)
}

#' @keywords internal
#' @noRd
bulat <- function(x, digit = 2) {
  if (is.data.frame(x)) {
    for (nm in names(x)) if (is.numeric(x[[nm]])) x[[nm]] <- round(x[[nm]], digit)
    return(x)
  }
  round(x, digit)
}

#' @keywords internal
#' @noRd
notifikasi <- function(pesan, jenis = "message") {
  shiny::showNotification(pesan, type = jenis, duration = 6)
}

#' @keywords internal
#' @noRd
nilai_unik_kolom <- function(data, kolom, batas = 40) {
  if (is.null(kolom) || !nzchar(kolom) || !kolom %in% names(data)) return(character(0))
  v <- unique(trimws(as.character(data[[kolom]])))
  v <- v[!is.na(v) & nzchar(v)]
  utils::head(sort(v), batas)
}

#' @keywords internal
#' @noRd
daftar_kolom <- function(data, skema = NULL) {
  kolom <- names(data)
  kolom <- kolom[!startsWith(kolom, "_")]
  label <- stats::setNames(kolom, kolom)
  if (!is.null(skema) && nrow(skema) > 0) {
    cocok <- match(kolom, skema$nama)
    ada <- !is.na(cocok)
    label[ada] <- sprintf("%s [%s]", skema$label[cocok[ada]], kolom[ada])
  }
  stats::setNames(kolom, label)
}

#' @keywords internal
#' @noRd
status_checklist <- function(hasil, naskah, meta, jenis, tipe) {
  cl <- checklist_laporan()
  cl <- cl[vapply(strsplit(cl$jenis, ","), function(x) jenis %in% x, logical(1)), , drop = FALSE]
  cl <- cl[cl$berlaku == "keduanya" | cl$berlaku == tipe, , drop = FALSE]

  cl$terpenuhi <- vapply(cl$kunci, function(kunci) {
    bagian <- strsplit(kunci, ".", fixed = TRUE)[[1]]
    if (bagian[1] == "analisis") {
      if (is.null(hasil)) return(FALSE)
      switch(bagian[2],
        ringkasan = TRUE,
        gejala = !is.null(hasil$gejala) && nrow(hasil$gejala) > 0,
        ar_jenis_kelamin = !is.null(hasil$ar_jenis_kelamin),
        ar_umur = !is.null(hasil$ar_umur),
        ar_tempat = !is.null(hasil$ar_tempat),
        kurva = nrow(hasil$kurva$bins) > 0,
        paparan = !is.null(hasil$paparan),
        multivariabel = !is.null(hasil$multivariabel$tabel),
        diagnosis_banding = !is.null(hasil$diagnosis_banding),
        kriteria = any(!is.na(hasil$kriteria_klb$terpenuhi)),
        FALSE)
    } else {
      if (bagian[2] == "definisi_kasus") return(nchar(trimws(meta$definisi_kasus)) > 20)
      isi <- naskah[[bagian[2]]]
      !is.null(isi) && nchar(trimws(isi)) > 40 && !startsWith(trimws(isi), "[")
    }
  }, logical(1))
  cl
}

#' @keywords internal
#' @noRd
ui_checklist <- function(cl) {
  htmltools::tagList(
    lapply(seq_len(nrow(cl)), function(i) {
      htmltools::div(
        class = "klb-checklist",
        htmltools::span(class = paste("tanda", if (cl$terpenuhi[i]) "ok" else "no"),
                        if (cl$terpenuhi[i]) "\u2713" else "\u00b7"),
        htmltools::div(
          htmltools::strong(cl$judul[i]), " ", pil(cl$bagian[i]),
          htmltools::div(class = "klb-kecil", cl$uraian[i])
        )
      )
    })
  )
}
