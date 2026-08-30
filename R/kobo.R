#' Menguji koneksi ke KoboToolbox
#'
#' @param base_url URL server KoboToolbox, misalnya `https://kf.kobotoolbox.org`.
#' @param token Token API pengguna.
#' @return Daftar berisi `username` dan `server`.
#' @export
kobo_uji_koneksi <- function(base_url, token) {
  resp <- kobo_get(base_url, token, "/me/?format=json")
  list(username = resp$username %||% "(tidak diketahui)", server = sub("/+$", "", base_url))
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

#' Pesan galat KoboToolbox yang dapat dibaca petugas
#'
#' Halaman galat KoboToolbox sering berupa HTML panjang. Fungsi ini
#' menerjemahkan kode status menjadi penjelasan singkat beserta langkah
#' yang perlu dilakukan.
#'
#' @param resp Objek respons httr2.
#' @return Teks pesan galat.
#' @keywords internal
#' @noRd
pesan_galat_kobo <- function(resp) {
  status <- httr2::resp_status(resp)
  isi <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
  # Buang tanda HTML agar pesan tetap ringkas dan terbaca
  ringkas <- trimws(gsub("\\s+", " ", gsub("<[^>]*>", " ", substr(isi, 1, 4000))))
  ringkas <- substr(ringkas, 1, 200)

  penjelasan <- switch(
    as.character(status),
    "401" = "Token API ditolak. Perbarui token pada menu Pengaturan.",
    "403" = paste("Akun pemilik token tidak memiliki akses ke sumber daya ini.",
                  "Pastikan proyek dibagikan ke akun tersebut."),
    "404" = paste("Proyek tidak ditemukan pada server ini. Periksa apakah proyek masih ada,",
                  "sudah di-deploy, dan berada pada server yang sama dengan URL di Pengaturan."),
    "429" = "Permintaan terlalu sering. Tunggu sejenak lalu coba lagi.",
    if (status >= 500) "Server KoboToolbox sedang bermasalah. Coba lagi beberapa saat lagi." else ""
  )
  sprintf("KoboToolbox membalas %s. %s%s", status, penjelasan,
          if (nzchar(ringkas)) paste0(" Rincian: ", ringkas) else "")
}

#' @keywords internal
#' @noRd
kobo_get <- function(base_url, token, jalur) {
  url <- if (grepl("^https?://", jalur)) jalur else paste0(sub("/+$", "", base_url), jalur)
  resp <- httr2::request(url) |>
    httr2::req_headers(Authorization = paste("Token", token), Accept = "application/json") |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform()
  if (httr2::resp_status(resp) >= 400) stop(pesan_galat_kobo(resp), call. = FALSE)
  httr2::resp_body_json(resp)
}

#' Daftar proyek survei pada akun KoboToolbox
#'
#' @inheritParams kobo_uji_koneksi
#' @return `data.frame` proyek dengan kolom `uid`, `nama`, `jumlah_submission`,
#'   dan `diubah`.
#' @export
kobo_daftar_proyek <- function(base_url, token) {
  hasil <- kobo_get(base_url, token, "/api/v2/assets/?format=json&limit=200")
  aset <- Filter(function(a) identical(a$asset_type, "survey"), hasil$results)
  if (length(aset) == 0) {
    return(data.frame(uid = character(0), nama = character(0),
                      jumlah_submission = integer(0), diubah = character(0),
                      stringsAsFactors = FALSE))
  }
  data.frame(
    uid = vapply(aset, function(a) a$uid %||% "", character(1)),
    nama = vapply(aset, function(a) a$name %||% "(tanpa nama)", character(1)),
    jumlah_submission = vapply(aset, function(a) as.integer(a$deployment__submission_count %||% 0), integer(1)),
    diubah = vapply(aset, function(a) substr(a$date_modified %||% "", 1, 10), character(1)),
    stringsAsFactors = FALSE
  )
}

label_pertama <- function(label, cadangan) {
  if (is.null(label)) return(cadangan)
  if (is.list(label)) label <- unlist(label)
  label <- label[nzchar(label)]
  if (length(label) == 0) cadangan else label[1]
}

#' Definisi pertanyaan pada formulir KoboToolbox
#'
#' Pertanyaan bertipe `select_multiple` disertai daftar kolom hasil pemekaran
#' agar dapat dipetakan sebagai variabel biner.
#'
#' @inheritParams kobo_uji_koneksi
#' @param uid UID proyek KoboToolbox.
#' @return Daftar berisi `nama` proyek dan `fields` (`data.frame`).
#' @export
kobo_definisi_form <- function(base_url, token, uid) {
  aset <- kobo_get(base_url, token, sprintf("/api/v2/assets/%s/?format=json", uid))
  survey <- aset$content$survey %||% list()
  pilihan <- aset$content$choices %||% list()

  daftar_pilihan <- list()
  for (c in pilihan) {
    lst <- c$list_name %||% ""
    nilai <- c$`$autovalue` %||% c$name %||% ""
    if (!nzchar(lst) || !nzchar(nilai)) next
    daftar_pilihan[[lst]] <- rbind(daftar_pilihan[[lst]],
      data.frame(nilai = nilai, label = label_pertama(c$label, nilai), stringsAsFactors = FALSE))
  }

  baris <- list()
  lewati <- c("note", "start", "end", "audit", "calculate", "begin_group", "end_group",
              "begin_repeat", "end_repeat")
  for (q in survey) {
    tipe <- q$type %||% ""
    if (tipe %in% lewati) next
    nama <- q$`$autoname` %||% q$name %||% ""
    if (!nzchar(nama)) next
    xpath <- q$`$xpath` %||% nama
    label <- label_pertama(q$label, nama)
    lst <- q$select_from_list_name %||% ""
    opsi <- if (nzchar(lst) && !is.null(daftar_pilihan[[lst]])) daftar_pilihan[[lst]] else NULL

    baris[[length(baris) + 1]] <- data.frame(
      nama = xpath, tipe = tipe, label = label,
      pilihan = if (is.null(opsi)) "" else paste(opsi$nilai, opsi$label, sep = "=", collapse = "|"),
      stringsAsFactors = FALSE)

    if (identical(tipe, "select_multiple") && !is.null(opsi)) {
      for (i in seq_len(nrow(opsi))) {
        baris[[length(baris) + 1]] <- data.frame(
          nama = paste0(xpath, "/", opsi$nilai[i]), tipe = "select_multiple_item",
          label = sprintf("%s: %s", label, opsi$label[i]), pilihan = "",
          stringsAsFactors = FALSE)
      }
    }
  }
  list(nama = aset$name %||% uid, fields = do.call(rbind, baris))
}

#' Mengunduh seluruh submission satu proyek
#'
#' Kolom `select_multiple` dimekarkan menjadi kolom biner sesuai definisi
#' formulir agar mudah dipetakan sebagai variabel gejala atau pangan.
#'
#' @inheritParams kobo_definisi_form
#' @param fields Definisi pertanyaan dari [kobo_definisi_form()].
#' @return `data.frame` submission.
#' @export
kobo_ambil_data <- function(base_url, token, uid, fields = NULL) {
  jalur <- sprintf("/api/v2/assets/%s/data/?format=json&limit=500", uid)
  kumpul <- list()
  repeat {
    hal <- kobo_get(base_url, token, jalur)
    kumpul <- c(kumpul, hal$results)
    lanjut <- hal$`next`
    if (is.null(lanjut) || is.na(lanjut) || !nzchar(lanjut)) break
    jalur <- lanjut
  }
  if (length(kumpul) == 0) return(data.frame())

  ratakan <- function(x) {
    x <- x[!vapply(x, function(v) is.list(v) && length(v) > 0 && is.list(v[[1]]), logical(1))]
    lapply(x, function(v) if (length(v) == 0) NA else paste(unlist(v), collapse = " "))
  }
  baris <- lapply(kumpul, ratakan)
  kolom <- unique(unlist(lapply(baris, names)))
  data <- as.data.frame(do.call(rbind, lapply(baris, function(b) {
    v <- b[kolom]
    names(v) <- kolom
    vapply(v, function(x) if (is.null(x)) NA_character_ else as.character(x), character(1))
  })), stringsAsFactors = FALSE)

  if (!is.null(fields)) {
    multi <- fields[fields$tipe == "select_multiple_item", , drop = FALSE]
    for (i in seq_len(nrow(multi))) {
      bagian <- strsplit(multi$nama[i], "/", fixed = TRUE)[[1]]
      induk <- paste(utils::head(bagian, -1), collapse = "/")
      nilai <- utils::tail(bagian, 1)
      if (!induk %in% names(data)) next
      dipilih <- strsplit(as.character(data[[induk]]), "\\s+")
      data[[multi$nama[i]]] <- vapply(dipilih, function(x) as.character(as.integer(nilai %in% x)), character(1))
    }
  }
  data
}
