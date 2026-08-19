#' Lokasi penyimpanan data aplikasi
#'
#' Bawaan memakai direktori data pengguna sesuai kebiasaan sistem operasi,
#' dan dapat diganti melalui variabel lingkungan `KLB_DATA_DIR`.
#'
#' @param sub Subdirektori, misalnya `"kabupaten"` atau `"provinsi"`.
#' @return Jalur direktori data.
#' @export
direktori_data <- function(sub = "") {
  akar <- Sys.getenv("KLB_DATA_DIR", unset = "")
  if (!nzchar(akar)) akar <- file.path(path.expand("~"), ".klbfederated")
  jalur <- if (nzchar(sub)) file.path(akar, sub) else akar
  dir.create(jalur, recursive = TRUE, showWarnings = FALSE)
  jalur
}

#' Membuka koneksi basis data node
#'
#' @param peran `"kabupaten"` atau `"provinsi"`.
#' @param berkas Lokasi berkas SQLite. Bila `NULL` memakai [direktori_data()].
#' @return Koneksi DBI.
#' @export
buka_db <- function(peran = c("kabupaten", "provinsi"), berkas = NULL) {
  peran <- match.arg(peran)
  if (is.null(berkas)) berkas <- file.path(direktori_data(peran), sprintf("%s.sqlite", peran))
  con <- DBI::dbConnect(RSQLite::SQLite(), berkas)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  siapkan_skema(con, peran)
  con
}

#' @keywords internal
#' @noRd
siapkan_skema <- function(con, peran) {
  jalankan <- function(sql) DBI::dbExecute(con, sql)

  jalankan("CREATE TABLE IF NOT EXISTS pengguna (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    username TEXT NOT NULL UNIQUE,
    sandi_hash TEXT NOT NULL,
    nama TEXT NOT NULL,
    peran TEXT NOT NULL CHECK (peran IN ('admin','analis','viewer')),
    aktif INTEGER NOT NULL DEFAULT 1,
    harus_ganti_sandi INTEGER NOT NULL DEFAULT 0,
    dibuat_pada TEXT NOT NULL DEFAULT (datetime('now')))")

  jalankan("CREATE TABLE IF NOT EXISTS pengaturan (
    kunci TEXT PRIMARY KEY, nilai TEXT)")

  jalankan("CREATE TABLE IF NOT EXISTS audit (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    pengguna_id INTEGER, aksi TEXT NOT NULL, detail TEXT,
    pada TEXT NOT NULL DEFAULT (datetime('now')))")

  if (identical(peran, "kabupaten")) {
    jalankan("CREATE TABLE IF NOT EXISTS investigasi (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      nama TEXT NOT NULL, jenis TEXT NOT NULL, penyakit TEXT,
      provinsi TEXT NOT NULL, kabupaten TEXT NOT NULL, kecamatan TEXT, desa TEXT,
      latitude REAL, longitude REAL, tanggal_lapor TEXT,
      status TEXT NOT NULL DEFAULT 'berlangsung',
      kobo_uid TEXT, kobo_nama TEXT, populasi_berisiko INTEGER, catatan TEXT,
      dibuat_oleh INTEGER, dibuat_pada TEXT NOT NULL DEFAULT (datetime('now')),
      diubah_pada TEXT NOT NULL DEFAULT (datetime('now')))")

    jalankan("CREATE TABLE IF NOT EXISTS pemetaan (
      investigasi_id INTEGER PRIMARY KEY,
      spec TEXT NOT NULL, konfigurasi TEXT NOT NULL, pembanding TEXT,
      diubah_pada TEXT NOT NULL DEFAULT (datetime('now')))")

    jalankan("CREATE TABLE IF NOT EXISTS skema_form (
      investigasi_id INTEGER PRIMARY KEY, konten TEXT NOT NULL,
      diambil_pada TEXT NOT NULL DEFAULT (datetime('now')))")

    jalankan("CREATE TABLE IF NOT EXISTS submission (
      investigasi_id INTEGER NOT NULL, kobo_id TEXT NOT NULL, data TEXT NOT NULL,
      diimpor_pada TEXT NOT NULL DEFAULT (datetime('now')),
      PRIMARY KEY (investigasi_id, kobo_id))")

    jalankan("CREATE TABLE IF NOT EXISTS analisis (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      investigasi_id INTEGER NOT NULL, hasil TEXT NOT NULL,
      dibuat_pada TEXT NOT NULL DEFAULT (datetime('now')), dibuat_oleh INTEGER)")

    jalankan("CREATE TABLE IF NOT EXISTS laporan (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      investigasi_id INTEGER NOT NULL, judul TEXT NOT NULL, tipe TEXT NOT NULL,
      naskah TEXT NOT NULL, meta TEXT NOT NULL, sumber_narasi TEXT DEFAULT 'template',
      dibuat_pada TEXT NOT NULL DEFAULT (datetime('now')),
      diubah_pada TEXT NOT NULL DEFAULT (datetime('now')))")

    jalankan("CREATE TABLE IF NOT EXISTS pengiriman (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      investigasi_id INTEGER NOT NULL, uid TEXT NOT NULL, payload TEXT NOT NULL,
      status TEXT NOT NULL, respons TEXT,
      dikirim_pada TEXT NOT NULL DEFAULT (datetime('now')), dikirim_oleh INTEGER)")

    bawaan <- list(node_kode = "KAB-DEMO", node_nama = "Dinas Kesehatan Kabupaten Demo",
                   kabupaten = "Kabupaten Demo", provinsi = "D.I. Yogyakarta",
                   kobo_base_url = "https://kf.kobotoolbox.org",
                   provinsi_url = "http://localhost:4002", provinsi_api_key = "",
                   kobo_token = "")
  } else {
    jalankan("CREATE TABLE IF NOT EXISTS node (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      kode TEXT NOT NULL UNIQUE, nama TEXT NOT NULL, kabupaten TEXT NOT NULL,
      api_key_hash TEXT NOT NULL, aktif INTEGER NOT NULL DEFAULT 1,
      terakhir_kirim TEXT, dibuat_pada TEXT NOT NULL DEFAULT (datetime('now')))")

    jalankan("CREATE TABLE IF NOT EXISTS laporan_agregat (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      uid TEXT NOT NULL UNIQUE, node_kode TEXT NOT NULL, kabupaten TEXT NOT NULL,
      provinsi TEXT NOT NULL, nama_klb TEXT NOT NULL, jenis TEXT NOT NULL,
      penyakit TEXT, patogen TEXT, sumber TEXT, status TEXT NOT NULL,
      kecamatan TEXT, desa TEXT, latitude REAL, longitude REAL,
      tanggal_mulai TEXT, tanggal_akhir TEXT, tanggal_lapor TEXT,
      populasi_berisiko INTEGER, total_kasus INTEGER, total_meninggal INTEGER,
      attack_rate REAL, cfr REAL, konfirmasi_lab INTEGER,
      payload TEXT NOT NULL, diterima_pada TEXT NOT NULL DEFAULT (datetime('now')))")

    bawaan <- list(provinsi = "D.I. Yogyakarta", port_ingest = "4002")
  }

  for (kunci in names(bawaan)) {
    ada <- DBI::dbGetQuery(con, "SELECT 1 FROM pengaturan WHERE kunci = ?", list(kunci))
    if (nrow(ada) == 0) {
      DBI::dbExecute(con, "INSERT INTO pengaturan (kunci, nilai) VALUES (?, ?)",
                     list(kunci, bawaan[[kunci]]))
    }
  }

  jumlah <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM pengguna")$n
  if (jumlah == 0) {
    buat_pengguna(con, Sys.getenv("KLB_ADMIN_USER", "admin"),
                  Sys.getenv("KLB_ADMIN_PASSWORD", "admin123"),
                  "Administrator", "admin", harus_ganti = TRUE)
  }
  invisible(TRUE)
}

#' Menyimpan atau membaca pengaturan
#' @param con Koneksi basis data.
#' @param kunci Nama pengaturan.
#' @param nilai Nilai baru. Bila `NULL`, fungsi membaca nilai.
#' @return Nilai pengaturan.
#' @export
pengaturan <- function(con, kunci, nilai = NULL) {
  if (is.null(nilai)) {
    hasil <- DBI::dbGetQuery(con, "SELECT nilai FROM pengaturan WHERE kunci = ?", list(kunci))
    return(if (nrow(hasil) == 0) NA_character_ else hasil$nilai[1])
  }
  DBI::dbExecute(con,
    "INSERT INTO pengaturan (kunci, nilai) VALUES (?, ?)
     ON CONFLICT(kunci) DO UPDATE SET nilai = excluded.nilai", list(kunci, nilai))
  invisible(nilai)
}

#' Mencatat aktivitas pengguna
#' @param con Koneksi basis data.
#' @param pengguna_id Id pengguna.
#' @param aksi Nama aksi.
#' @param detail Detail tambahan.
#' @export
catat_audit <- function(con, pengguna_id, aksi, detail = NULL) {
  DBI::dbExecute(con, "INSERT INTO audit (pengguna_id, aksi, detail) VALUES (?, ?, ?)",
                 list(pengguna_id, aksi,
                      if (is.null(detail)) NA_character_ else jsonlite::toJSON(detail, auto_unbox = TRUE)))
  invisible(TRUE)
}

#' Membuat pengguna baru
#'
#' Kata sandi disimpan sebagai hash argon2 melalui paket sodium.
#'
#' @param con Koneksi basis data.
#' @param username Nama pengguna.
#' @param sandi Kata sandi.
#' @param nama Nama lengkap.
#' @param peran `"admin"`, `"analis"`, atau `"viewer"`.
#' @param harus_ganti Apakah kata sandi wajib diganti saat login pertama.
#' @return Id pengguna baru, tidak terlihat.
#' @export
buat_pengguna <- function(con, username, sandi, nama, peran = "analis", harus_ganti = TRUE) {
  stopifnot(nchar(sandi) >= 8 || identical(sandi, "admin123") || identical(sandi, "analis123"))
  hash <- sodium::password_store(sandi)
  DBI::dbExecute(con,
    "INSERT INTO pengguna (username, sandi_hash, nama, peran, harus_ganti_sandi)
     VALUES (?, ?, ?, ?, ?)",
    list(username, hash, nama, peran, as.integer(harus_ganti)))
  invisible(DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id)
}

#' Memeriksa kredensial pengguna
#' @param con Koneksi basis data.
#' @param username Nama pengguna.
#' @param sandi Kata sandi.
#' @return Daftar data pengguna bila berhasil, `NULL` bila gagal.
#' @export
periksa_login <- function(con, username, sandi) {
  baris <- DBI::dbGetQuery(con,
    "SELECT id, username, sandi_hash, nama, peran, aktif, harus_ganti_sandi
     FROM pengguna WHERE username = ?", list(username))
  if (nrow(baris) == 0 || baris$aktif[1] != 1) return(NULL)
  ok <- tryCatch(sodium::password_verify(baris$sandi_hash[1], sandi), error = function(e) FALSE)
  if (!ok) {
    catat_audit(con, NA, "login_gagal", list(username = username))
    return(NULL)
  }
  catat_audit(con, baris$id[1], "login")
  list(id = baris$id[1], username = baris$username[1], nama = baris$nama[1],
       peran = baris$peran[1], harus_ganti_sandi = baris$harus_ganti_sandi[1] == 1)
}

#' Mengganti kata sandi pengguna
#' @param con Koneksi basis data.
#' @param pengguna_id Id pengguna.
#' @param sandi_baru Kata sandi baru, minimal 8 karakter.
#' @export
ganti_sandi <- function(con, pengguna_id, sandi_baru) {
  if (nchar(sandi_baru) < 8) stop("Kata sandi baru minimal 8 karakter", call. = FALSE)
  DBI::dbExecute(con, "UPDATE pengguna SET sandi_hash = ?, harus_ganti_sandi = 0 WHERE id = ?",
                 list(sodium::password_store(sandi_baru), pengguna_id))
  catat_audit(con, pengguna_id, "ganti_sandi")
  invisible(TRUE)
}

#' Apakah peran pengguna boleh mengubah data
#' @param peran Peran pengguna.
#' @return `TRUE` untuk admin dan analis.
#' @export
boleh_ubah <- function(peran) {
  isTRUE(peran %in% c("admin", "analis"))
}
