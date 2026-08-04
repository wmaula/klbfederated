#' Memuat data contoh ke dalam basis data node kabupaten
#'
#' Membuat dua investigasi berisi data sintetis: KLB keracunan pangan pada
#' hajatan dan KLB pertusis di satu kalurahan, lengkap dengan pemetaan variabel
#' dan konfigurasi analisis. Data ini fiktif dan hanya untuk pengujian serta
#' pelatihan.
#'
#' @param con Koneksi basis data node kabupaten dari [buka_db()].
#' @param tambah_pengguna Apakah pengguna contoh analis dan viewer dibuat.
#' @return Vektor id investigasi yang dibuat, tidak terlihat.
#' @export
#' @examples
#' con <- buka_db("kabupaten", berkas = file.path(tempdir(), "contoh.sqlite"))
#' muat_data_contoh(con)
#' DBI::dbDisconnect(con)
muat_data_contoh <- function(con, tambah_pengguna = TRUE) {
  if (tambah_pengguna) {
    ada <- DBI::dbGetQuery(con, "SELECT username FROM pengguna")$username
    if (!"analis" %in% ada) {
      buat_pengguna(con, "analis", "analis123", "Petugas Surveilans Kabupaten", "analis")
    }
    if (!"viewer" %in% ada) {
      buat_pengguna(con, "viewer", "viewer123", "Kepala Bidang P2P", "viewer")
    }
  }

  ids <- integer(0)
  for (contoh in list(contoh_keracunan_pangan(), contoh_pertusis())) {
    ket <- contoh$keterangan
    lama <- DBI::dbGetQuery(con, "SELECT id FROM investigasi WHERE nama = ?", list(ket$nama))
    if (nrow(lama) > 0) {
      id <- lama$id[1]
      DBI::dbExecute(con, "DELETE FROM submission WHERE investigasi_id = ?", list(id))
    } else {
      DBI::dbExecute(con,
        "INSERT INTO investigasi (nama, jenis, penyakit, provinsi, kabupaten, kecamatan, desa,
           latitude, longitude, tanggal_lapor, status, kobo_nama, populasi_berisiko, catatan)
         VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
        list(ket$nama, ket$jenis, ket$penyakit, ket$provinsi, ket$kabupaten, ket$kecamatan,
             ket$desa, ket$latitude, ket$longitude, ket$tanggal_lapor, ket$status,
             "Formulir contoh (data sintetis)", contoh$konfigurasi$populasi_berisiko,
             "Data contoh untuk pengujian aplikasi. Bukan data investigasi sebenarnya."))
      id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id
    }

    data <- contoh$data
    DBI::dbExecute(con, "BEGIN")
    for (i in seq_len(nrow(data))) {
      DBI::dbExecute(con,
        "INSERT INTO submission (investigasi_id, kobo_id, data) VALUES (?, ?, ?)
         ON CONFLICT(investigasi_id, kobo_id) DO UPDATE SET data = excluded.data",
        list(id, as.character(i), jsonlite::toJSON(as.list(data[i, ]), auto_unbox = TRUE, na = "null")))
    }
    DBI::dbExecute(con, "COMMIT")

    simpan_pemetaan(con, id, contoh$pemetaan, contoh$konfigurasi, pembanding_kosong())
    ids <- c(ids, id)
  }
  invisible(ids)
}

#' Menyimpan pemetaan variabel dan konfigurasi analisis
#' @param con Koneksi basis data.
#' @param investigasi_id Id investigasi.
#' @param pemetaan Daftar pemetaan peran variabel.
#' @param konfigurasi Konfigurasi analisis.
#' @param pembanding Data pembanding kriteria KLB.
#' @export
simpan_pemetaan <- function(con, investigasi_id, pemetaan, konfigurasi, pembanding = pembanding_kosong()) {
  DBI::dbExecute(con,
    "INSERT INTO pemetaan (investigasi_id, spec, konfigurasi, pembanding)
     VALUES (?, ?, ?, ?)
     ON CONFLICT(investigasi_id) DO UPDATE SET spec = excluded.spec,
       konfigurasi = excluded.konfigurasi, pembanding = excluded.pembanding,
       diubah_pada = datetime('now')",
    list(investigasi_id,
         jsonlite::toJSON(pemetaan, auto_unbox = TRUE, null = "null"),
         jsonlite::toJSON(konfigurasi, auto_unbox = TRUE, null = "null"),
         jsonlite::toJSON(pembanding, auto_unbox = TRUE, null = "null")))
  invisible(TRUE)
}

#' Membaca pemetaan variabel satu investigasi
#' @param con Koneksi basis data.
#' @param investigasi_id Id investigasi.
#' @return Daftar berisi `pemetaan`, `konfigurasi`, dan `pembanding`.
#' @export
baca_pemetaan <- function(con, investigasi_id) {
  baris <- DBI::dbGetQuery(con,
    "SELECT spec, konfigurasi, pembanding FROM pemetaan WHERE investigasi_id = ?",
    list(investigasi_id))
  if (nrow(baris) == 0) {
    return(list(pemetaan = list(), konfigurasi = konfigurasi_analisis(),
                pembanding = pembanding_kosong()))
  }
  konf <- jsonlite::fromJSON(baris$konfigurasi[1], simplifyVector = TRUE)
  konf <- utils::modifyList(konfigurasi_analisis(), konf)
  if (!is.null(konf$inkubasi_acuan) && length(konf$inkubasi_acuan) == 0) konf$inkubasi_acuan <- NULL
  # Nilai null pada JSON harus kembali menjadi NA, bukan menghapus elemen.
  pemb <- pembanding_kosong()
  if (!is.na(baris$pembanding[1])) {
    tersimpan <- jsonlite::fromJSON(baris$pembanding[1], simplifyVector = TRUE)
    for (nm in names(pemb)) {
      nilai <- tersimpan[[nm]]
      if (!is.null(nilai) && length(nilai) > 0) pemb[[nm]] <- nilai[1]
    }
  }
  list(
    pemetaan = jsonlite::fromJSON(baris$spec[1], simplifyVector = TRUE, simplifyDataFrame = FALSE),
    konfigurasi = konf,
    pembanding = pemb
  )
}

#' Membaca seluruh submission satu investigasi
#' @param con Koneksi basis data.
#' @param investigasi_id Id investigasi.
#' @return `data.frame` submission.
#' @export
baca_submission <- function(con, investigasi_id) {
  baris <- DBI::dbGetQuery(con,
    "SELECT data FROM submission WHERE investigasi_id = ? ORDER BY rowid", list(investigasi_id))
  if (nrow(baris) == 0) return(data.frame())
  daftar <- lapply(baris$data, function(x) jsonlite::fromJSON(x, simplifyVector = TRUE))
  kolom <- unique(unlist(lapply(daftar, names)))
  as.data.frame(do.call(rbind, lapply(daftar, function(d) {
    v <- d[kolom]
    names(v) <- kolom
    vapply(v, function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)[1],
           character(1))
  })), stringsAsFactors = FALSE)
}
