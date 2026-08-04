#' Menyusun payload agregat untuk provinsi
#'
#' Payload dibentuk dari hasil analisis, bukan dari data individu. Tidak ada
#' baris responden, identitas, maupun koordinat rumah tangga di dalamnya.
#'
#' @param hasil Objek `klb_hasil`.
#' @param keterangan Identitas investigasi.
#' @param node Daftar identitas node: `kode`, `nama`, `kabupaten`, `provinsi`.
#' @param tambahan Daftar isian tambahan: `patogen_diduga`, `sumber_diduga`,
#'   `konfirmasi_lab`, `ringkasan_naratif`, `rekomendasi`.
#' @param dikirim_oleh Nama pengirim.
#' @return Daftar payload agregat.
#' @export
#' @examples
#' contoh <- contoh_keracunan_pangan(n = 60)
#' hasil <- analisis_klb(contoh$data, contoh$pemetaan, contoh$konfigurasi)
#' payload <- susun_agregat(hasil, contoh$keterangan,
#'   node = list(kode = "KAB-01", nama = "Dinkes", kabupaten = "Kulon Progo",
#'               provinsi = "D.I. Yogyakarta"))
#' names(payload)
susun_agregat <- function(hasil, keterangan, node, tambahan = list(), dikirim_oleh = "") {
  ambil <- function(nama, bawaan = NULL) if (is.null(tambahan[[nama]])) bawaan else tambahan[[nama]]
  r <- hasil$ringkasan

  faktor <- NULL
  if (!is.null(hasil$paparan)) {
    p <- hasil$paparan[is.finite(hasil$paparan$estimasi), ]
    p <- p[order(-p$estimasi), ]
    p <- utils::head(p, 5)
    faktor <- lapply(seq_len(nrow(p)), function(i) list(
      label = p$label[i], ukuran = p$ukuran[i], estimasi = p$estimasi[i],
      ci_bawah = p$ci_bawah[i], ci_atas = p$ci_atas[i], p_value = p$p_value[i]))
  }

  tabel_ke_list <- function(tab) {
    if (is.null(tab)) return(list())
    lapply(seq_len(nrow(tab)), function(i) as.list(tab[i, ]))
  }

  list(
    uid = paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-",
                 paste(sample(c(letters, 0:9), 8, TRUE), collapse = "")),
    node = node,
    klb = list(
      nama = keterangan$nama, jenis = keterangan$jenis, penyakit = keterangan$penyakit,
      patogen_diduga = ambil("patogen_diduga"), sumber_diduga = ambil("sumber_diduga"),
      status = keterangan$status, kecamatan = keterangan$kecamatan, desa = keterangan$desa,
      latitude = keterangan$latitude, longitude = keterangan$longitude,
      tanggal_mulai = format(r$tanggal_kasus_pertama, "%Y-%m-%d"),
      tanggal_akhir = format(r$tanggal_kasus_terakhir, "%Y-%m-%d"),
      tanggal_lapor = keterangan$tanggal_lapor
    ),
    angka = list(
      populasi_berisiko = r$populasi_berisiko, total_kasus = r$total_kasus,
      total_meninggal = r$total_meninggal, attack_rate = r$attack_rate,
      cfr = if (is.finite(r$cfr)) r$cfr else NULL,
      konfirmasi_lab = ambil("konfirmasi_lab")
    ),
    distribusi = list(
      jenis_kelamin = tabel_ke_list(hasil$ar_jenis_kelamin),
      kelompok_umur = tabel_ke_list(hasil$ar_umur),
      gejala_utama = tabel_ke_list(utils::head(hasil$gejala, 10)),
      kurva_epidemik = lapply(seq_len(nrow(hasil$kurva$bins)), function(i) list(
        mulai = format(hasil$kurva$bins$mulai[i], "%Y-%m-%dT%H:%M:%S"),
        selesai = format(hasil$kurva$bins$selesai[i], "%Y-%m-%dT%H:%M:%S"),
        kasus = hasil$kurva$bins$kasus[i]))
    ),
    faktor_risiko_teratas = faktor,
    kriteria_klb = tabel_ke_list(hasil$kriteria_klb),
    ringkasan_naratif = ambil("ringkasan_naratif", ""),
    rekomendasi = ambil("rekomendasi", character(0)),
    dikirim_oleh = dikirim_oleh,
    dikirim_pada = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    versi_aplikasi = as.character(utils::packageVersion("klbfederated"))
  )
}

#' Mengirim laporan agregat ke webapp provinsi
#'
#' @param payload Payload dari [susun_agregat()].
#' @param url URL webapp provinsi.
#' @param api_key Kunci API node yang diterbitkan webapp provinsi.
#' @return Daftar berisi `status` dan `respons`.
#' @export
kirim_agregat <- function(payload, url, api_key) {
  if (!nzchar(url) || !nzchar(api_key)) {
    stop("URL dan kunci API webapp provinsi wajib diisi pada menu Pengaturan.", call. = FALSE)
  }
  resp <- tryCatch({
    httr2::request(paste0(sub("/+$", "", url), "/api/ingest")) |>
      httr2::req_headers(`Content-Type` = "application/json", `X-API-Key` = api_key) |>
      httr2::req_body_raw(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform()
  }, error = function(e) e)

  if (inherits(resp, "error")) {
    return(list(status = "gagal", respons = conditionMessage(resp)))
  }
  isi <- httr2::resp_body_string(resp)
  list(status = if (httr2::resp_status(resp) < 300) "terkirim" else "gagal",
       respons = substr(isi, 1, 1000))
}

#' Menjalankan penerima laporan agregat di webapp provinsi
#'
#' Membuka endpoint `POST /api/ingest` memakai httpuv pada proses yang sama
#' dengan aplikasi Shiny provinsi. Kiriman harus menyertakan tajuk `X-API-Key`
#' yang cocok dengan salah satu node aktif.
#'
#' @param con Koneksi basis data provinsi.
#' @param port Port endpoint penerimaan.
#' @return Objek server httpuv, tidak terlihat.
#' @export
jalankan_penerima <- function(con, port = 4002) {
  tangani <- function(req) {
    if (!identical(req$REQUEST_METHOD, "POST") || !identical(req$PATH_INFO, "/api/ingest")) {
      return(list(status = 404L, headers = list("Content-Type" = "application/json"),
                  body = '{"error":"Endpoint tidak ditemukan"}'))
    }
    kunci <- req$HTTP_X_API_KEY
    if (is.null(kunci)) {
      return(list(status = 401L, headers = list("Content-Type" = "application/json"),
                  body = '{"error":"Kunci API tidak disertakan"}'))
    }
    node <- DBI::dbGetQuery(con, "SELECT id, kode, api_key_hash FROM node WHERE aktif = 1")
    cocok <- which(vapply(node$api_key_hash, function(h) {
      tryCatch(sodium::password_verify(h, kunci), error = function(e) FALSE)
    }, logical(1)))
    if (length(cocok) == 0) {
      return(list(status = 401L, headers = list("Content-Type" = "application/json"),
                  body = '{"error":"Kunci API tidak dikenali"}'))
    }

    isi <- rawToChar(req$rook.input$read())
    payload <- tryCatch(jsonlite::fromJSON(isi, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(payload) || is.null(payload$uid) || is.null(payload$klb)) {
      return(list(status = 400L, headers = list("Content-Type" = "application/json"),
                  body = '{"error":"Format laporan agregat tidak valid"}'))
    }

    nilai <- function(x, bawaan = NA) if (is.null(x)) bawaan else x
    DBI::dbExecute(con,
      "INSERT INTO laporan_agregat (uid, node_kode, kabupaten, provinsi, nama_klb, jenis,
         penyakit, patogen, sumber, status, kecamatan, desa, latitude, longitude,
         tanggal_mulai, tanggal_akhir, tanggal_lapor, populasi_berisiko, total_kasus,
         total_meninggal, attack_rate, cfr, konfirmasi_lab, payload)
       VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
       ON CONFLICT(uid) DO UPDATE SET payload = excluded.payload, status = excluded.status,
         total_kasus = excluded.total_kasus, total_meninggal = excluded.total_meninggal,
         attack_rate = excluded.attack_rate, cfr = excluded.cfr,
         diterima_pada = datetime('now')",
      list(payload$uid, nilai(payload$node$kode), nilai(payload$node$kabupaten),
           nilai(payload$node$provinsi), nilai(payload$klb$nama), nilai(payload$klb$jenis),
           nilai(payload$klb$penyakit), nilai(payload$klb$patogen_diduga),
           nilai(payload$klb$sumber_diduga), nilai(payload$klb$status),
           nilai(payload$klb$kecamatan), nilai(payload$klb$desa),
           nilai(payload$klb$latitude), nilai(payload$klb$longitude),
           nilai(payload$klb$tanggal_mulai), nilai(payload$klb$tanggal_akhir),
           nilai(payload$klb$tanggal_lapor), nilai(payload$angka$populasi_berisiko),
           nilai(payload$angka$total_kasus), nilai(payload$angka$total_meninggal),
           nilai(payload$angka$attack_rate), nilai(payload$angka$cfr),
           nilai(payload$angka$konfirmasi_lab), isi))
    DBI::dbExecute(con, "UPDATE node SET terakhir_kirim = datetime('now') WHERE id = ?",
                   list(node$id[cocok[1]]))

    list(status = 200L, headers = list("Content-Type" = "application/json"),
         body = jsonlite::toJSON(list(ok = TRUE, uid = payload$uid), auto_unbox = TRUE))
  }

  server <- httpuv::startServer("0.0.0.0", port, list(call = tangani))
  message(sprintf("[klbfederated] penerima laporan agregat aktif di http://localhost:%d/api/ingest", port))
  invisible(server)
}

#' Mendaftarkan node kabupaten pada webapp provinsi
#'
#' Kunci API dibuat acak, disimpan sebagai hash, dan hanya dikembalikan satu
#' kali pada saat pembuatan.
#'
#' @param con Koneksi basis data provinsi.
#' @param kode Kode node.
#' @param nama Nama node.
#' @param kabupaten Nama kabupaten atau kota.
#' @return Kunci API dalam bentuk teks.
#' @export
daftarkan_node <- function(con, kode, nama, kabupaten) {
  kunci <- paste(c(sample(c(letters, LETTERS, 0:9), 48, TRUE)), collapse = "")
  DBI::dbExecute(con,
    "INSERT INTO node (kode, nama, kabupaten, api_key_hash) VALUES (?, ?, ?, ?)",
    list(kode, nama, kabupaten, sodium::password_store(kunci)))
  kunci
}

#' Memutar ulang kunci API satu node
#' @param con Koneksi basis data provinsi.
#' @param node_id Id node.
#' @return Kunci API baru.
#' @export
putar_kunci_node <- function(con, node_id) {
  kunci <- paste(c(sample(c(letters, LETTERS, 0:9), 48, TRUE)), collapse = "")
  DBI::dbExecute(con, "UPDATE node SET api_key_hash = ? WHERE id = ?",
                 list(sodium::password_store(kunci), node_id))
  kunci
}
