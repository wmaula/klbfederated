#' Penanda versi aset antarmuka
#'
#' Ditempelkan pada URL berkas CSS dan JavaScript agar peramban mengambil versi
#' terbaru setelah package diperbarui.
#'
#' @return Teks versi package.
#' @export
versi_aset <- function() {
  as.character(utils::packageVersion("klbfederated"))
}

#' Menormalkan katalog model dari peramban
#'
#' Shiny menyederhanakan larik objek JSON menjadi `data.frame`, tetapi larik
#' berisi satu objek atau objek dengan kunci berbeda tetap menjadi daftar.
#' Fungsi ini menerima kedua bentuk tersebut dan selalu mengembalikan
#' `data.frame` dengan kolom `id` dan `vram`.
#'
#' @param katalog Nilai `input$webllm_katalog` dari peramban.
#' @return `data.frame` dengan kolom `id` dan `vram`.
#' @export
#' @examples
#' normalkan_katalog(list(list(id = "Model-A", vram = 1024)))
#' normalkan_katalog(data.frame(id = "Model-A", vram = 1024))
normalkan_katalog <- function(katalog) {
  kosong <- data.frame(id = character(0), vram = numeric(0), stringsAsFactors = FALSE)
  if (is.null(katalog) || length(katalog) == 0) return(kosong)

  if (is.data.frame(katalog)) {
    id <- as.character(katalog$id)
    vram <- if ("vram" %in% names(katalog)) suppressWarnings(as.numeric(katalog$vram))
            else rep(NA_real_, length(id))
  } else if (is.list(katalog) && !is.null(katalog$id) && length(katalog$id) > 0 &&
             !is.list(katalog$id)) {
    # Bentuk yang dikirim jembatan: dua larik sejajar id dan vram.
    id <- as.character(unlist(katalog$id))
    vram <- if (is.null(katalog$vram)) rep(NA_real_, length(id))
            else suppressWarnings(as.numeric(unlist(katalog$vram)))
    if (length(vram) != length(id)) vram <- rep(NA_real_, length(id))
    vram[!is.finite(vram) | vram <= 0] <- NA_real_
  } else if (is.atomic(katalog) && length(katalog) %% 2 == 0 &&
             all(grepl("-MLC$", katalog[seq(1, length(katalog), by = 2)]))) {
    # Bentuk lama: vektor rata berselang-seling id dan vram.
    id <- as.character(katalog[seq(1, length(katalog), by = 2)])
    vram <- suppressWarnings(as.numeric(katalog[seq(2, length(katalog), by = 2)]))
  } else {
    id <- vapply(katalog, function(m) {
      nilai <- if (is.list(m)) m[["id"]] else m
      if (is.null(nilai)) NA_character_ else as.character(nilai)[1]
    }, character(1))
    vram <- vapply(katalog, function(m) {
      nilai <- if (is.list(m)) m[["vram"]] else NULL
      if (is.null(nilai) || length(nilai) == 0) NA_real_ else suppressWarnings(as.numeric(nilai)[1])
    }, numeric(1))
  }
  hasil <- data.frame(id = id, vram = vram, stringsAsFactors = FALSE)
  hasil[!is.na(hasil$id) & nzchar(hasil$id), , drop = FALSE]
}

#' Daftar model WebLLM bawaan
#'
#' Dipakai untuk mengisi pilihan model sejak awal, sehingga daftar tidak pernah
#' kosong meski katalog dari peramban belum sempat dikirim.
#'
#' @return Vektor bernama berisi id model.
#' @export
model_bawaan <- function() {
  c(
    "Qwen2.5 3B (perkiraan 2,4 GB VRAM)" = "Qwen2.5-3B-Instruct-q4f16_1-MLC",
    "Llama 3.2 3B (perkiraan 2,2 GB VRAM)" = "Llama-3.2-3B-Instruct-q4f16_1-MLC",
    "Qwen2.5 1.5B (perkiraan 1,6 GB VRAM)" = "Qwen2.5-1.5B-Instruct-q4f16_1-MLC",
    "Llama 3.2 1B (perkiraan 0,9 GB VRAM)" = "Llama-3.2-1B-Instruct-q4f16_1-MLC"
  )
}

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
