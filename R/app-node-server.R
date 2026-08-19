#' @keywords internal
#' @noRd
server_node <- function(con) {
  function(input, output, session) {
    rv <- shiny::reactiveValues(
      pengguna = NULL, inv_id = NULL, hasil = NULL, naskah = NULL,
      meta = metadata_laporan(), aset_kobo = NULL, payload = NULL,
      llm_status = list(tahap = "belum", pesan = "", progres = 0),
      llm_periksa = NULL, muat_ulang = 0
    )

    node_info <- function() {
      list(kode = pengaturan(con, "node_kode"), nama = pengaturan(con, "node_nama"),
           kabupaten = pengaturan(con, "kabupaten"), provinsi = pengaturan(con, "provinsi"))
    }

    ## ---------------- Login ----------------
    output$kerangka <- shiny::renderUI({
      if (is.null(rv$pengguna)) ui_login_node() else ui_utama_node(rv$pengguna, node_info())
    })

    shiny::observeEvent(input$login_kirim, {
      pengguna <- periksa_login(con, input$login_user, input$login_sandi)
      if (is.null(pengguna)) {
        output$login_pesan <- shiny::renderUI(
          shiny::div(class = "alert alert-danger mt-2 py-2", "Username atau kata sandi salah."))
        return(invisible())
      }
      rv$pengguna <- pengguna
      if (isTRUE(pengguna$harus_ganti_sandi)) {
        notifikasi("Kata sandi bawaan masih dipakai. Ganti kata sandi pada menu Pengaturan.", "warning")
      }
      session$sendCustomMessage("webllm_periksa", list())
    })

    shiny::observeEvent(input$nav, {
      if (identical(input$nav, "keluar")) rv$pengguna <- NULL
      # Permintaan katalog diulang saat tab Laporan dibuka, karena pesan yang
      # dikirim tepat saat login dapat mendahului tampilnya antarmuka utama.
      if (identical(input$nav, "Laporan")) session$sendCustomMessage("webllm_periksa", list())
    })

    boleh <- shiny::reactive(!is.null(rv$pengguna) && boleh_ubah(rv$pengguna$peran))

    ## ---------------- Investigasi ----------------
    daftar_investigasi <- shiny::reactive({
      rv$muat_ulang
      DBI::dbGetQuery(con, "SELECT i.id, i.nama, i.jenis, i.penyakit, i.kabupaten, i.kecamatan,
          i.desa, i.status, i.kobo_nama,
          (SELECT COUNT(*) FROM submission s WHERE s.investigasi_id = i.id) AS responden,
          (SELECT COUNT(*) FROM analisis a WHERE a.investigasi_id = i.id) AS analisis,
          (SELECT COUNT(*) FROM pengiriman p WHERE p.investigasi_id = i.id AND p.status = 'terkirim') AS terkirim
        FROM investigasi i ORDER BY i.dibuat_pada DESC")
    })

    output$tabel_investigasi <- DT::renderDT({
      d <- daftar_investigasi()
      if (nrow(d) == 0) return(tabel_dt(NULL))
      tampil <- data.frame(
        Investigasi = d$nama, Jenis = gsub("_", " ", d$jenis),
        Lokasi = paste(d$desa, d$kecamatan, d$kabupaten, sep = ", "),
        Status = d$status, Responden = d$responden, Analisis = d$analisis,
        Terkirim = d$terkirim, stringsAsFactors = FALSE)
      DT::datatable(tampil, rownames = FALSE, selection = "single",
                    options = list(dom = "tp", pageLength = 8, scrollX = TRUE))
    })

    shiny::observeEvent(input$tabel_investigasi_rows_selected, {
      d <- daftar_investigasi()
      baris <- input$tabel_investigasi_rows_selected
      if (length(baris) == 1) {
        rv$inv_id <- d$id[baris]
        rv$hasil <- NULL
        rv$naskah <- NULL
        muat_pemetaan_ke_ui()
        muat_analisis_tersimpan()
      }
    })

    investigasi_aktif <- shiny::reactive({
      shiny::req(rv$inv_id)
      DBI::dbGetQuery(con, "SELECT * FROM investigasi WHERE id = ?", list(rv$inv_id))
    })

    keterangan_aktif <- shiny::reactive({
      inv <- investigasi_aktif()
      list(nama = inv$nama, jenis = inv$jenis, penyakit = inv$penyakit,
           provinsi = inv$provinsi, kabupaten = inv$kabupaten, kecamatan = inv$kecamatan,
           desa = inv$desa, latitude = inv$latitude, longitude = inv$longitude,
           tanggal_lapor = inv$tanggal_lapor, status = inv$status)
    })

    output$investigasi_terpilih <- shiny::renderUI({
      if (is.null(rv$inv_id)) {
        return(shiny::div(class = "alert alert-warning mt-2 py-2",
                          "Pilih satu baris investigasi untuk mulai bekerja."))
      }
      inv <- investigasi_aktif()
      shiny::div(class = "mt-2",
        shiny::strong(inv$nama), " ", pil(inv$status, if (inv$status == "berlangsung") "warn" else "ok"),
        shiny::div(class = "klb-kecil",
                   sprintf("Jenis %s, penyakit %s, proyek Kobo: %s",
                           gsub("_", " ", inv$jenis),
                           if (is.na(inv$penyakit)) "belum diisi" else inv$penyakit,
                           if (is.na(inv$kobo_nama)) "belum ditautkan" else inv$kobo_nama)),
        if (boleh()) shiny::actionButton("inv_status", sprintf("Tandai KLB %s",
          if (inv$status == "berlangsung") "berakhir" else "masih berlangsung"),
          class = "btn-sm btn-outline-secondary mt-2")
      )
    })

    shiny::observeEvent(input$inv_status, {
      inv <- investigasi_aktif()
      baru <- if (inv$status == "berlangsung") "berakhir" else "berlangsung"
      DBI::dbExecute(con, "UPDATE investigasi SET status = ?, diubah_pada = datetime('now') WHERE id = ?",
                     list(baru, rv$inv_id))
      catat_audit(con, rv$pengguna$id, "ubah_status_investigasi", list(id = rv$inv_id, status = baru))
      rv$muat_ulang <- rv$muat_ulang + 1
      notifikasi(sprintf("Status investigasi diubah menjadi %s.", baru))
    })

    shiny::observeEvent(input$inv_simpan, {
      if (!boleh()) return(notifikasi("Peran Anda tidak dapat membuat investigasi.", "error"))
      if (!nzchar(input$inv_nama)) return(notifikasi("Nama investigasi wajib diisi.", "error"))
      DBI::dbExecute(con,
        "INSERT INTO investigasi (nama, jenis, penyakit, provinsi, kabupaten, kecamatan, desa,
           latitude, longitude, tanggal_lapor, populasi_berisiko, dibuat_oleh)
         VALUES (?,?,?,?,?,?,?,?,?,?,?,?)",
        list(input$inv_nama, input$inv_jenis, input$inv_penyakit, input$inv_provinsi,
             input$inv_kabupaten, input$inv_kecamatan, input$inv_desa,
             if (is.na(input$inv_lat)) NA else input$inv_lat,
             if (is.na(input$inv_lon)) NA else input$inv_lon,
             as.character(input$inv_tanggal),
             if (is.na(input$inv_populasi)) NA else input$inv_populasi, rv$pengguna$id))
      catat_audit(con, rv$pengguna$id, "buat_investigasi", list(nama = input$inv_nama))
      rv$muat_ulang <- rv$muat_ulang + 1
      notifikasi("Investigasi baru tersimpan.")
    })

    shiny::observeEvent(input$inv_contoh, {
      if (!boleh()) return(notifikasi("Peran Anda tidak dapat memuat data contoh.", "error"))
      muat_data_contoh(con, tambah_pengguna = FALSE)
      rv$muat_ulang <- rv$muat_ulang + 1
      notifikasi("Dua investigasi contoh dimuat beserta data sintetisnya.")
    })

    ## ---------------- KoboToolbox ----------------
    output$kobo_status <- shiny::renderUI({
      inv <- if (is.null(rv$inv_id)) NULL else investigasi_aktif()
      token <- pengaturan(con, "kobo_token")
      shiny::tagList(
        shiny::p(class = "klb-kecil",
          sprintf("Token KoboToolbox: %s. Proyek tertaut: %s",
                  if (is.na(token) || !nzchar(token)) "belum diatur pada menu Pengaturan" else "tersimpan",
                  if (is.null(inv) || is.na(inv$kobo_nama)) "belum ada" else inv$kobo_nama))
      )
    })

    shiny::observeEvent(input$kobo_muat, {
      token <- pengaturan(con, "kobo_token")
      if (is.na(token) || !nzchar(token)) return(notifikasi("Token KoboToolbox belum diatur.", "error"))
      hasil <- tryCatch(kobo_daftar_proyek(pengaturan(con, "kobo_base_url"), token),
                        error = function(e) e)
      if (inherits(hasil, "error")) return(notifikasi(conditionMessage(hasil), "error"))
      rv$aset_kobo <- hasil
      notifikasi(sprintf("%d proyek survei ditemukan.", nrow(hasil)))
    })

    output$tabel_kobo <- DT::renderDT({
      if (is.null(rv$aset_kobo)) return(tabel_dt(NULL))
      DT::datatable(rv$aset_kobo, rownames = FALSE, selection = "single",
                    options = list(dom = "tp", pageLength = 8))
    })

    shiny::observeEvent(input$kobo_tautkan, {
      shiny::req(rv$inv_id, rv$aset_kobo)
      baris <- input$tabel_kobo_rows_selected
      if (length(baris) != 1) return(notifikasi("Pilih satu proyek KoboToolbox terlebih dahulu.", "error"))
      uid <- rv$aset_kobo$uid[baris]
      hasil <- tryCatch(kobo_definisi_form(pengaturan(con, "kobo_base_url"),
                                           pengaturan(con, "kobo_token"), uid),
                        error = function(e) e)
      if (inherits(hasil, "error")) return(notifikasi(conditionMessage(hasil), "error"))
      DBI::dbExecute(con, "UPDATE investigasi SET kobo_uid = ?, kobo_nama = ? WHERE id = ?",
                     list(uid, hasil$nama, rv$inv_id))
      DBI::dbExecute(con,
        "INSERT INTO skema_form (investigasi_id, konten) VALUES (?, ?)
         ON CONFLICT(investigasi_id) DO UPDATE SET konten = excluded.konten,
           diambil_pada = datetime('now')",
        list(rv$inv_id, jsonlite::toJSON(hasil$fields, auto_unbox = TRUE)))
      catat_audit(con, rv$pengguna$id, "tautkan_kobo", list(uid = uid))
      rv$muat_ulang <- rv$muat_ulang + 1
      notifikasi(sprintf("Proyek %s ditautkan, %d pertanyaan terbaca.", hasil$nama, nrow(hasil$fields)))
    })

    shiny::observeEvent(input$kobo_impor, {
      shiny::req(rv$inv_id)
      inv <- investigasi_aktif()
      if (is.na(inv$kobo_uid)) return(notifikasi("Investigasi belum ditautkan ke proyek KoboToolbox.", "error"))
      skema <- skema_form()
      hasil <- tryCatch(kobo_ambil_data(pengaturan(con, "kobo_base_url"), pengaturan(con, "kobo_token"),
                                        inv$kobo_uid, skema), error = function(e) e)
      if (inherits(hasil, "error")) return(notifikasi(conditionMessage(hasil), "error"))
      if (nrow(hasil) == 0) return(notifikasi("Proyek belum memiliki submission.", "warning"))
      DBI::dbExecute(con, "BEGIN")
      for (i in seq_len(nrow(hasil))) {
        id_kobo <- if ("_id" %in% names(hasil)) as.character(hasil[["_id"]][i]) else as.character(i)
        DBI::dbExecute(con,
          "INSERT INTO submission (investigasi_id, kobo_id, data) VALUES (?, ?, ?)
           ON CONFLICT(investigasi_id, kobo_id) DO UPDATE SET data = excluded.data,
             diimpor_pada = datetime('now')",
          list(rv$inv_id, id_kobo, jsonlite::toJSON(as.list(hasil[i, ]), auto_unbox = TRUE, na = "null")))
      }
      DBI::dbExecute(con, "COMMIT")
      catat_audit(con, rv$pengguna$id, "impor_kobo", list(jumlah = nrow(hasil)))
      rv$muat_ulang <- rv$muat_ulang + 1
      notifikasi(sprintf("%d submission diunduh dan disimpan di node ini.", nrow(hasil)))
    })

    submission_aktif <- shiny::reactive({
      shiny::req(rv$inv_id)
      rv$muat_ulang
      baca_submission(con, rv$inv_id)
    })

    skema_form <- shiny::reactive({
      shiny::req(rv$inv_id)
      baris <- DBI::dbGetQuery(con, "SELECT konten FROM skema_form WHERE investigasi_id = ?",
                               list(rv$inv_id))
      if (nrow(baris) == 0) return(NULL)
      jsonlite::fromJSON(baris$konten[1], simplifyDataFrame = TRUE)
    })

    output$tabel_pratinjau <- DT::renderDT({
      d <- submission_aktif()
      if (nrow(d) == 0) return(tabel_dt(NULL))
      tabel_dt(utils::head(d, 200))
    })

    ## ---------------- Pemetaan variabel ----------------
    pemetaan_aktif <- shiny::reactiveVal(list())

    muat_pemetaan_ke_ui <- function() {
      shiny::req(rv$inv_id)
      pm <- baca_pemetaan(con, rv$inv_id)
      pemetaan_aktif(pm$pemetaan)
      k <- pm$konfigurasi
      shiny::updateTextInput(session, "cfg_umur", value = paste(k$kelompok_umur, collapse = ", "))
      shiny::updateSelectInput(session, "cfg_satuan", selected = k$satuan_waktu)
      shiny::updateNumericInput(session, "cfg_interval",
                                value = if (is.null(k$interval_kurva) || length(k$interval_kurva) == 0)
                                  NA else k$interval_kurva[1])
      shiny::updateSelectInput(session, "cfg_desain", selected = k$desain)
      shiny::updateNumericInput(session, "cfg_ambang", value = k$ambang_kandidat)
      shiny::updateNumericInput(session, "cfg_populasi",
                                value = if (is.null(k$populasi_berisiko) || length(k$populasi_berisiko) == 0)
                                  NA else k$populasi_berisiko[1])
      shiny::updateNumericInput(session, "cfg_ink_min",
                                value = if (is.null(k$inkubasi_acuan)) NA else k$inkubasi_acuan$min)
      shiny::updateNumericInput(session, "cfg_ink_maks",
                                value = if (is.null(k$inkubasi_acuan)) NA else k$inkubasi_acuan$maks)
      p <- pm$pembanding
      angka <- function(x) if (is.null(x) || length(x) == 0) NA_real_ else suppressWarnings(as.numeric(x[1]))
      shiny::updateNumericInput(session, "pb_kasus", value = angka(p$kasus_periode_sebelumnya))
      shiny::updateNumericInput(session, "pb_rata_lalu", value = angka(p$rata_bulan_tahun_lalu))
      shiny::updateNumericInput(session, "pb_rata_ini", value = angka(p$rata_bulan_tahun_ini))
      shiny::updateNumericInput(session, "pb_cfr", value = angka(p$cfr_periode_sebelumnya))
      shiny::updateNumericInput(session, "pb_prop_lalu", value = angka(p$proporsi_periode_sebelumnya))
      shiny::updateNumericInput(session, "pb_prop_ini", value = angka(p$proporsi_periode_ini))
      baru <- p$penyakit_baru
      shiny::updateSelectInput(session, "pb_baru",
        selected = if (is.null(baru) || length(baru) == 0 || is.na(baru[1])) ""
                   else if (isTRUE(baru[1])) "ya" else "tidak")
    }

    output$ui_pemetaan <- shiny::renderUI({
      shiny::req(rv$inv_id)
      data <- submission_aktif()
      if (nrow(data) == 0) {
        return(shiny::div(class = "alert alert-warning py-2",
                          "Belum ada data. Tarik data dari KoboToolbox atau muat data contoh."))
      }
      inv <- investigasi_aktif()
      kolom <- daftar_kolom(data, skema_form())
      pm <- pemetaan_aktif()
      katalog <- peran_variabel()
      wajib <- peran_wajib(inv$jenis)$key

      blok <- lapply(seq_len(nrow(katalog)), function(i) {
        peran <- katalog[i, ]
        terpilih <- pm[[peran$key]]$kolom
        htmltools::div(
          class = "klb-peran",
          htmltools::div(
            htmltools::strong(peran$label), " ",
            if (peran$key %in% wajib) pil("wajib", "err") else pil("opsional"),
            htmltools::div(class = "klb-kecil", peran$deskripsi,
                           if (!is.na(peran$acuan)) htmltools::em(sprintf(" Acuan: %s.", peran$acuan)))
          ),
          if (peran$ganda) {
            shiny::selectizeInput(paste0("map_", peran$key), NULL, choices = kolom,
                                  selected = terpilih, multiple = TRUE, width = "100%",
                                  options = list(placeholder = "pilih kolom"))
          } else {
            shiny::selectInput(paste0("map_", peran$key), NULL,
                               choices = c("(belum dipetakan)" = "", kolom),
                               selected = if (is.null(terpilih)) "" else terpilih[1], width = "100%")
          },
          if (peran$key == "status_sakit" || peran$tipe == "biner") {
            nilai <- if (peran$ganda) c("1", "ya", "Ya", "true") else
              nilai_unik_kolom(data, if (length(terpilih)) terpilih[1] else "")
            shiny::selectizeInput(paste0("pos_", peran$key), "Nilai yang berarti kasus atau positif",
                                  choices = unique(c(nilai, pm[[peran$key]]$nilai_positif)),
                                  selected = pm[[peran$key]]$nilai_positif,
                                  multiple = TRUE, width = "100%",
                                  options = list(create = TRUE, placeholder = "misalnya sakit atau 1"))
          }
        )
      })
      htmltools::tagList(blok)
    })

    kumpulkan_pemetaan <- function() {
      data <- submission_aktif()
      katalog <- peran_variabel()
      lama <- pemetaan_aktif()
      hasil <- list()
      for (i in seq_len(nrow(katalog))) {
        peran <- katalog[i, ]
        nilai <- input[[paste0("map_", peran$key)]]
        nilai <- nilai[!is.null(nilai) & nzchar(nilai)]
        if (length(nilai) == 0) next
        entri <- list(kolom = nilai)
        pos <- input[[paste0("pos_", peran$key)]]
        if (!is.null(pos) && length(pos) > 0) entri$nilai_positif <- pos
        if (!is.null(lama[[peran$key]]$peta_nilai)) entri$peta_nilai <- lama[[peran$key]]$peta_nilai
        if (!is.null(lama[[peran$key]]$label)) {
          entri$label <- lama[[peran$key]]$label
        } else if (peran$ganda) {
          skema <- skema_form()
          label <- stats::setNames(as.list(nilai), nilai)
          if (!is.null(skema)) {
            cocok <- match(nilai, skema$nama)
            ada <- !is.na(cocok)
            label[ada] <- as.list(skema$label[cocok[ada]])
          }
          entri$label <- label
        }
        hasil[[peran$key]] <- entri
      }
      hasil
    }

    kumpulkan_konfigurasi <- function() {
      konfigurasi_analisis(
        kelompok_umur = as.numeric(trimws(strsplit(input$cfg_umur, ",")[[1]])),
        satuan_waktu = input$cfg_satuan,
        interval_kurva = if (is.na(input$cfg_interval)) NULL else input$cfg_interval,
        desain = input$cfg_desain,
        ambang_kandidat = input$cfg_ambang,
        populasi_berisiko = if (is.na(input$cfg_populasi)) NULL else input$cfg_populasi,
        inkubasi_acuan = if (is.na(input$cfg_ink_min) || is.na(input$cfg_ink_maks)) NULL
          else list(min = input$cfg_ink_min, maks = input$cfg_ink_maks)
      )
    }

    kumpulkan_pembanding <- function() {
      angka <- function(x) if (is.null(x) || is.na(x)) NA_real_ else as.numeric(x)
      list(
        kasus_periode_sebelumnya = angka(input$pb_kasus),
        rata_bulan_tahun_lalu = angka(input$pb_rata_lalu),
        rata_bulan_tahun_ini = angka(input$pb_rata_ini),
        cfr_periode_sebelumnya = angka(input$pb_cfr),
        proporsi_periode_sebelumnya = angka(input$pb_prop_lalu),
        proporsi_periode_ini = angka(input$pb_prop_ini),
        penyakit_baru = if (!nzchar(input$pb_baru)) NA else identical(input$pb_baru, "ya"),
        catatan = ""
      )
    }

    shiny::observeEvent(input$pemetaan_simpan, {
      shiny::req(rv$inv_id)
      if (!boleh()) return(notifikasi("Peran Anda tidak dapat mengubah pemetaan.", "error"))
      spec <- kumpulkan_pemetaan()
      simpan_pemetaan(con, rv$inv_id, spec, kumpulkan_konfigurasi(), kumpulkan_pembanding())
      pemetaan_aktif(spec)
      catat_audit(con, rv$pengguna$id, "simpan_pemetaan", list(investigasi = rv$inv_id))
      notifikasi("Pemetaan variabel dan pengaturan analisis tersimpan.")
    })

    output$validasi_pemetaan <- shiny::renderUI({
      shiny::req(rv$inv_id)
      inv <- investigasi_aktif()
      v <- periksa_pemetaan(inv$jenis, pemetaan_aktif())
      shiny::tagList(
        if (v$lengkap) {
          shiny::div(class = "alert alert-success py-2",
                     "Seluruh variabel wajib sudah dipetakan. Analisis dapat dijalankan.")
        } else {
          shiny::div(class = "alert alert-warning py-2",
            shiny::strong("Variabel wajib yang belum dipetakan:"),
            shiny::tags$ul(lapply(seq_len(nrow(v$wajib_kurang)), function(i)
              shiny::tags$li(sprintf("%s (dibutuhkan oleh: %s)",
                                     v$wajib_kurang$label[i], v$wajib_kurang$dibutuhkan_oleh[i])))))
        },
        if (length(v$peringatan)) {
          shiny::div(class = "alert alert-info py-2",
                     shiny::tags$ul(lapply(v$peringatan, shiny::tags$li)))
        },
        shiny::fluidRow(
          shiny::column(6, shiny::h6("Analisis tersedia"),
                        shiny::tags$ul(class = "klb-kecil", lapply(v$analisis_tersedia, shiny::tags$li))),
          shiny::column(6, shiny::h6("Analisis belum dapat dijalankan"),
                        shiny::tags$ul(class = "klb-kecil",
                          lapply(v$analisis_terkunci, function(a)
                            shiny::tags$li(sprintf("%s: %s", a$analisis, a$alasan)))))
        )
      )
    })

    ## ---------------- Analisis ----------------
    muat_analisis_tersimpan <- function() {
      baris <- DBI::dbGetQuery(con,
        "SELECT hasil FROM analisis WHERE investigasi_id = ? ORDER BY id DESC LIMIT 1",
        list(rv$inv_id))
      if (nrow(baris) == 0) return(invisible())
      rv$hasil <- tryCatch(unserialize(jsonlite::base64_dec(baris$hasil[1])), error = function(e) NULL)
    }

    shiny::observeEvent(input$analisis_jalankan, {
      shiny::req(rv$inv_id)
      inv <- investigasi_aktif()
      data <- submission_aktif()
      if (nrow(data) == 0) return(notifikasi("Belum ada data responden.", "error"))
      spec <- kumpulkan_pemetaan()
      if (length(spec) == 0) spec <- pemetaan_aktif()
      v <- periksa_pemetaan(inv$jenis, spec)
      if (!v$lengkap) return(notifikasi("Pemetaan variabel wajib belum lengkap.", "error"))

      shiny::withProgress(message = "Menjalankan analisis", value = 0.4, {
        hasil <- tryCatch(
          analisis_klb(data, spec, kumpulkan_konfigurasi(), kumpulkan_pembanding(), jenis = inv$jenis),
          error = function(e) e)
      })
      if (inherits(hasil, "error")) return(notifikasi(conditionMessage(hasil), "error"))
      rv$hasil <- hasil
      if (boleh()) {
        DBI::dbExecute(con, "INSERT INTO analisis (investigasi_id, hasil, dibuat_oleh) VALUES (?, ?, ?)",
                       list(rv$inv_id, jsonlite::base64_enc(serialize(hasil, NULL)), rv$pengguna$id))
        catat_audit(con, rv$pengguna$id, "jalankan_analisis", list(investigasi = rv$inv_id))
      }
      rv$muat_ulang <- rv$muat_ulang + 1
      notifikasi("Analisis selesai dan hasilnya tersimpan.")
    })

    output$analisis_catatan <- shiny::renderUI({
      if (is.null(rv$hasil)) return(shiny::p(class = "klb-kecil", "Belum ada hasil analisis."))
      shiny::tagList(
        shiny::p(class = "klb-kecil", sprintf("Hasil analisis terakhir: %s",
                                              format(rv$hasil$dibuat_pada, "%d/%m/%Y %H:%M"))),
        if (length(rv$hasil$peringatan)) {
          shiny::div(class = "alert alert-info py-2",
                     shiny::strong("Catatan analisis:"),
                     shiny::tags$ul(lapply(rv$hasil$peringatan, shiny::tags$li)))
        }
      )
    })

    output$analisis_isi <- shiny::renderUI({
      shiny::req(rv$hasil)
      h <- rv$hasil
      r <- h$ringkasan
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(3, kartu_stat("Populasi berisiko", r$populasi_berisiko,
                                      sprintf("%s diinvestigasi", r$total_diinvestigasi))),
          shiny::column(3, kartu_stat("Jumlah kasus", r$total_kasus)),
          shiny::column(3, kartu_stat("Attack rate", sprintf("%s persen", angka_id(r$attack_rate)))),
          shiny::column(3, kartu_stat("Case fatality rate",
                                      if (is.finite(r$cfr)) sprintf("%s persen", angka_id(r$cfr)) else "tidak dihitung",
                                      sprintf("%s kematian", r$total_meninggal)))
        ),
        shiny::br(),
        shiny::fluidRow(
          shiny::column(6, shiny::div(class = "card mb-3",
            shiny::div(class = "card-header", "Distribusi tanda dan gejala"),
            shiny::div(class = "card-body", shiny::plotOutput("plot_gejala", height = "300px"),
                       DT::DTOutput("tabel_gejala")))),
          shiny::column(6, shiny::div(class = "card mb-3",
            shiny::div(class = "card-header", "Attack rate menurut kelompok umur"),
            shiny::div(class = "card-body", shiny::plotOutput("plot_umur", height = "300px"),
                       DT::DTOutput("tabel_umur"))))
        ),
        shiny::fluidRow(
          shiny::column(6, shiny::div(class = "card mb-3",
            shiny::div(class = "card-header", "Attack rate menurut jenis kelamin"),
            shiny::div(class = "card-body", DT::DTOutput("tabel_jk")))),
          shiny::column(6, shiny::div(class = "card mb-3",
            shiny::div(class = "card-header", "Attack rate menurut tempat"),
            shiny::div(class = "card-body", DT::DTOutput("tabel_tempat"))))
        ),
        shiny::uiOutput("kartu_variabel_lain"),
        shiny::div(class = "card mb-3",
          shiny::div(class = "card-header", "Distribusi menurut waktu"),
          shiny::div(class = "card-body",
            shiny::plotOutput("plot_kurva", height = "360px"),
            shiny::fluidRow(
              shiny::column(3, kartu_stat("Inkubasi terpendek", jam_teks(h$inkubasi$min_jam))),
              shiny::column(3, kartu_stat("Inkubasi terpanjang", jam_teks(h$inkubasi$maks_jam))),
              shiny::column(3, kartu_stat("Inkubasi rata-rata", jam_teks(h$inkubasi$rata_jam))),
              shiny::column(3, kartu_stat("Median inkubasi", jam_teks(h$inkubasi$median_jam),
                                          sprintf("n = %s", h$inkubasi$n)))
            ),
            if (!is.null(h$inkubasi$periode_paparan)) shiny::p(class = "klb-kecil mt-2",
              sprintf("Perkiraan periode paparan: %s sampai %s",
                      format(h$inkubasi$periode_paparan$mulai, "%d/%m/%Y %H:%M"),
                      format(h$inkubasi$periode_paparan$selesai, "%d/%m/%Y %H:%M")))
          )
        ),
        if (!is.null(h$paparan)) shiny::div(class = "card mb-3",
          shiny::div(class = "card-header", "Analisis paparan"),
          shiny::div(class = "card-body",
            shiny::plotOutput("plot_paparan", height = "340px"),
            shiny::h6("Tabel makan dan tidak makan serta attack rate ratio"),
            DT::DTOutput("tabel_paparan"),
            shiny::h6("Analisis bivariat", class = "mt-3"),
            DT::DTOutput("tabel_bivariat"))),
        shiny::div(class = "card mb-3",
          shiny::div(class = "card-header", "Analisis multivariabel"),
          shiny::div(class = "card-body",
            shiny::p(class = "klb-kecil", h$multivariabel$catatan),
            DT::DTOutput("tabel_multivariabel"))),
        if (!is.null(h$diagnosis_banding)) shiny::div(class = "card mb-3",
          shiny::div(class = "card-header", "Diagnosis banding etiologi"),
          shiny::div(class = "card-body",
            shiny::p(class = "klb-kecil",
              "Penyingkiran mengikuti aturan masa inkubasi Pedoman KLB 2020. Nilai rujukan dari compendium umum wajib diverifikasi penyelidik."),
            DT::DTOutput("tabel_banding"))),
        shiny::div(class = "card mb-3",
          shiny::div(class = "card-header", "Penetapan KLB menurut Permenkes 1501 Tahun 2010"),
          shiny::div(class = "card-body", DT::DTOutput("tabel_kriteria"))),
        if (nrow(h$spot_map) > 0) shiny::div(class = "card mb-3",
          shiny::div(class = "card-header", "Sebaran kasus"),
          shiny::div(class = "card-body", leaflet::leafletOutput("peta", height = "420px")))
      )
    })

    output$plot_gejala <- shiny::renderPlot(plot_gejala(rv$hasil$gejala))
    output$plot_umur <- shiny::renderPlot(plot_attack_rate(rv$hasil$ar_umur, "Attack rate menurut kelompok umur"))
    output$plot_kurva <- shiny::renderPlot(plot_kurva_epidemik(rv$hasil))
    output$plot_paparan <- shiny::renderPlot(plot_paparan(rv$hasil$paparan))
    output$peta <- leaflet::renderLeaflet(peta_kasus(rv$hasil))

    output$tabel_gejala <- DT::renderDT(tabel_dt(bulat(rv$hasil$gejala)))
    output$tabel_umur <- DT::renderDT(tabel_dt(bulat(rv$hasil$ar_umur)))
    output$tabel_jk <- DT::renderDT(tabel_dt(bulat(rv$hasil$ar_jenis_kelamin)))
    output$tabel_tempat <- DT::renderDT(tabel_dt(bulat(rv$hasil$ar_tempat)))
    output$tabel_kriteria <- DT::renderDT(tabel_dt(rv$hasil$kriteria_klb))
    output$tabel_banding <- DT::renderDT(tabel_dt(bulat(rv$hasil$diagnosis_banding)))
    output$tabel_multivariabel <- DT::renderDT(tabel_dt(bulat(rv$hasil$multivariabel$tabel, 3)))

    output$tabel_paparan <- DT::renderDT({
      p <- rv$hasil$paparan
      shiny::req(p)
      tabel_dt(bulat(p[, c("label", "terpapar_sakit", "terpapar_tidak_sakit", "terpapar_total",
                           "ar_terpapar", "tak_terpapar_sakit", "tak_terpapar_total",
                           "ar_tak_terpapar", "arr")]))
    })

    output$tabel_bivariat <- DT::renderDT({
      p <- rv$hasil$paparan
      shiny::req(p)
      tabel_dt(data.frame(
        Variabel = p$label, Ukuran = p$ukuran, Estimasi = round(p$estimasi, 2),
        `95 persen CI` = sprintf("%s sampai %s", round(p$ci_bawah, 2), round(p$ci_atas, 2)),
        `Nilai p` = vapply(p$p_value, p_teks, character(1)), Uji = p$uji,
        check.names = FALSE, stringsAsFactors = FALSE))
    })

    output$kartu_variabel_lain <- shiny::renderUI({
      h <- rv$hasil
      kartu <- c(
        lapply(names(h$ar_lain), function(nama) {
          shiny::column(6, shiny::div(class = "card mb-3",
            shiny::div(class = "card-header", sprintf("Attack rate menurut %s", tolower(nama))),
            shiny::div(class = "card-body",
              DT::renderDT(tabel_dt(bulat(h$ar_lain[[nama]]))))))
        }),
        lapply(names(h$freq_kasus), function(nama) {
          shiny::column(6, shiny::div(class = "card mb-3",
            shiny::div(class = "card-header", sprintf("Distribusi kasus menurut %s", tolower(nama))),
            shiny::div(class = "card-body",
              DT::renderDT(tabel_dt(bulat(h$freq_kasus[[nama]]))))))
        })
      )
      if (length(kartu) == 0) return(NULL)
      shiny::fluidRow(kartu)
    })

    ## ---------------- Laporan ----------------
    kumpulkan_meta <- function() {
      utils::modifyList(metadata_laporan(), list(
        principal_investigator = input$lap_pi %||% "",
        co_principal_investigator = input$lap_copi %||% "",
        institusi = input$lap_institusi %||% "",
        tipe = input$lap_tipe %||% "akhir",
        sumber_laporan = input$lap_kronologi %||% "",
        definisi_kasus = input$lap_definisi %||% "",
        patogen_diduga = input$lap_patogen %||% "",
        sumber_diduga = input$lap_sumber %||% "",
        hasil_laboratorium = input$lap_lab %||% "",
        studi_lingkungan = input$lap_lingkungan %||% "",
        upaya_pengendalian = input$lap_upaya %||% "",
        rekomendasi = input$lap_rekomendasi %||% ""
      ))
    }

    shiny::observeEvent(input$lap_susun, {
      shiny::req(rv$hasil, rv$inv_id)
      rv$meta <- kumpulkan_meta()
      rv$naskah <- susun_draf_laporan(rv$hasil, keterangan_aktif(), rv$meta)
      shiny::updateTextAreaInput(session, "lap_naskah", value = rv$naskah[[input$lap_bagian]])
      notifikasi("Draf template tersusun dari hasil analisis.")
    })

    shiny::observeEvent(input$lap_bagian, {
      if (!is.null(rv$naskah)) {
        shiny::updateTextAreaInput(session, "lap_naskah", value = rv$naskah[[input$lap_bagian]] %||% "")
      }
      rv$llm_periksa <- NULL
    })

    shiny::observeEvent(input$lap_naskah, {
      if (!is.null(rv$naskah) && !is.null(input$lap_bagian)) {
        rv$naskah[[input$lap_bagian]] <- input$lap_naskah
      }
    })

    shiny::observeEvent(input$lap_kembalikan, {
      shiny::req(rv$hasil)
      draf <- susun_draf_laporan(rv$hasil, keterangan_aktif(), kumpulkan_meta())
      rv$naskah[[input$lap_bagian]] <- draf[[input$lap_bagian]]
      shiny::updateTextAreaInput(session, "lap_naskah", value = draf[[input$lap_bagian]])
    })

    shiny::observeEvent(input$lap_simpan, {
      shiny::req(rv$naskah, rv$inv_id)
      if (!boleh()) return(notifikasi("Peran Anda tidak dapat menyimpan laporan.", "error"))
      inv <- investigasi_aktif()
      DBI::dbExecute(con,
        "INSERT INTO laporan (investigasi_id, judul, tipe, naskah, meta) VALUES (?, ?, ?, ?, ?)",
        list(rv$inv_id, inv$nama, input$lap_tipe,
             jsonlite::toJSON(rv$naskah, auto_unbox = TRUE),
             jsonlite::toJSON(kumpulkan_meta(), auto_unbox = TRUE)))
      catat_audit(con, rv$pengguna$id, "simpan_laporan", list(investigasi = rv$inv_id))
      notifikasi("Laporan tersimpan di node kabupaten atau kota.")
    })

    output$ui_checklist <- shiny::renderUI({
      shiny::req(rv$inv_id)
      inv <- investigasi_aktif()
      cl <- status_checklist(rv$hasil, rv$naskah %||% list(), kumpulkan_meta(), inv$jenis,
                             input$lap_tipe %||% "akhir")
      shiny::tagList(
        shiny::p(class = "klb-kecil", sprintf("%d dari %d butir terpenuhi.",
                                              sum(cl$terpenuhi), nrow(cl))),
        ui_checklist(cl)
      )
    })

    ## ---------------- WebLLM ----------------
    shiny::observeEvent(input$webllm_katalog, {
      katalog <- normalkan_katalog(input$webllm_katalog)
      if (nrow(katalog) == 0) return(invisible())
      terpilih <- shiny::isolate(input$llm_model)
      label <- ifelse(is.na(katalog$vram), katalog$id,
                      sprintf("%s (perkiraan %s GB VRAM)", katalog$id,
                              angka_id(katalog$vram / 1024, 1)))
      shiny::updateSelectInput(
        session, "llm_model", choices = stats::setNames(katalog$id, label),
        selected = if (!is.null(terpilih) && terpilih %in% katalog$id) terpilih else katalog$id[1])
    })

    shiny::observeEvent(input$webllm_status, {
      rv$llm_status <- input$webllm_status
    })

    output$llm_status <- shiny::renderUI({
      s <- rv$llm_status
      webgpu <- input$webllm_webgpu
      shiny::tagList(
        if (identical(webgpu, FALSE)) {
          shiny::div(class = "alert alert-warning py-2",
            "Peramban ini tidak mendukung WebGPU sehingga model bahasa tidak dapat dijalankan. Draf template deterministik tetap memuat seluruh angka hasil analisis.")
        },
        if (!is.null(s$pesan) && nzchar(s$pesan)) shiny::p(class = "klb-kecil", s$pesan),
        if (identical(s$tahap, "memuat")) shiny::tags$progress(value = s$progres, max = 1, style = "width:100%")
      )
    })

    shiny::observeEvent(input$llm_muat, {
      shiny::req(input$llm_model)
      session$sendCustomMessage("webllm_muat", list(model = input$llm_model))
    })

    shiny::observeEvent(input$llm_lepas, session$sendCustomMessage("webllm_lepas", list()))

    shiny::observeEvent(input$llm_tulis, {
      shiny::req(rv$hasil, input$llm_model, input$lap_bagian)
      if (is.null(rv$naskah)) {
        rv$naskah <- susun_draf_laporan(rv$hasil, keterangan_aktif(), kumpulkan_meta())
      }
      fakta <- daftar_fakta(rv$hasil, keterangan_aktif(), kumpulkan_meta())
      session$sendCustomMessage("webllm_tulis", list(
        model = input$llm_model, bagian = input$lap_bagian, sistem = sistem_llm(),
        perintah = perintah_bagian(input$lap_bagian, fakta, rv$naskah[[input$lap_bagian]] %||% "")
      ))
    })

    shiny::observeEvent(input$webllm_hasil, {
      hasil <- input$webllm_hasil
      shiny::req(hasil$teks)
      rv$naskah[[hasil$bagian]] <- hasil$teks
      if (identical(hasil$bagian, input$lap_bagian)) {
        shiny::updateTextAreaInput(session, "lap_naskah", value = hasil$teks)
      }
      fakta <- daftar_fakta(rv$hasil, keterangan_aktif(), kumpulkan_meta())
      rv$llm_periksa <- periksa_narasi(hasil$teks, fakta)
    })

    output$llm_periksa <- shiny::renderUI({
      p <- rv$llm_periksa
      if (is.null(p)) return(NULL)
      if (p$bersih) {
        return(shiny::div(class = "alert alert-success py-2",
          "Pemeriksaan otomatis: seluruh angka pada naskah cocok dengan hasil analisis dan tidak ditemukan pernyataan kepastian penyebab."))
      }
      shiny::div(class = "alert alert-warning py-2",
        shiny::strong("Pemeriksaan otomatis terhadap naskah model:"),
        shiny::tags$ul(
          if (length(p$angka_asing)) shiny::tags$li(
            "Angka berikut tidak ditemukan pada hasil analisis dan perlu diperiksa: ",
            shiny::code(paste(p$angka_asing, collapse = ", "))),
          lapply(p$kalimat_terlalu_pasti, function(k)
            shiny::tags$li(sprintf("Kalimat menyatakan kepastian penyebab: %s", k)))
        ))
    })

    ## ---------------- Ekspor ----------------
    output$unduh_docx <- shiny::downloadHandler(
      filename = function() sprintf("laporan-klb-%s.docx", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        shiny::req(rv$hasil, rv$naskah)
        ekspor_docx(rv$hasil, keterangan_aktif(), rv$naskah, file)
      }
    )

    output$unduh_md <- shiny::downloadHandler(
      filename = function() sprintf("laporan-klb-%s.md", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        shiny::req(rv$hasil, rv$naskah)
        ekspor_markdown(rv$hasil, keterangan_aktif(), rv$naskah, file)
      }
    )

    ## ---------------- Kirim ke provinsi ----------------
    kumpulkan_tambahan <- function() {
      list(
        patogen_diduga = if (nzchar(input$kirim_patogen %||% "")) input$kirim_patogen else NULL,
        sumber_diduga = if (nzchar(input$kirim_sumber %||% "")) input$kirim_sumber else NULL,
        konfirmasi_lab = if (is.na(input$kirim_lab)) NULL else input$kirim_lab,
        ringkasan_naratif = input$kirim_ringkasan %||% "",
        rekomendasi = Filter(nzchar, trimws(strsplit(input$kirim_rekomendasi %||% "", "\n")[[1]]))
      )
    }

    shiny::observeEvent(input$kirim_pratinjau, {
      shiny::req(rv$hasil)
      rv$payload <- susun_agregat(rv$hasil, keterangan_aktif(), node_info(),
                                  kumpulkan_tambahan(), rv$pengguna$nama)
      notifikasi("Pratinjau kiriman dibuat. Periksa isinya sebelum mengirim.")
    })

    output$kirim_pratinjau_ui <- shiny::renderUI({
      if (is.null(rv$payload)) return(NULL)
      p <- rv$payload
      shiny::div(class = "card mb-3",
        shiny::div(class = "card-header", "Pratinjau data yang akan dikirim"),
        shiny::div(class = "card-body",
          shiny::p(class = "klb-kecil", "Payload berikut tidak memuat baris data individu maupun identitas responden."),
          shiny::fluidRow(
            shiny::column(4, kartu_stat("Kasus", p$angka$total_kasus,
                                        sprintf("populasi berisiko %s", p$angka$populasi_berisiko))),
            shiny::column(4, kartu_stat("Attack rate", sprintf("%s persen", angka_id(p$angka$attack_rate)))),
            shiny::column(4, kartu_stat("Patogen diduga", p$klb$patogen_diduga %||% "belum ditetapkan"))
          ),
          shiny::br(),
          shiny::tags$details(
            shiny::tags$summary(class = "klb-kecil", "Lihat payload JSON lengkap"),
            shiny::tags$pre(style = "max-height:320px;overflow:auto;font-size:11px",
              jsonlite::toJSON(p, auto_unbox = TRUE, pretty = TRUE, null = "null"))
          )
        ))
    })

    shiny::observeEvent(input$kirim_kirim, {
      shiny::req(rv$hasil)
      if (!boleh()) return(notifikasi("Peran Anda tidak dapat mengirim laporan.", "error"))
      if (is.null(rv$payload)) return(notifikasi("Buat pratinjau kiriman terlebih dahulu.", "error"))
      shiny::showModal(shiny::modalDialog(
        title = "Konfirmasi pengiriman",
        shiny::p("Laporan agregat akan dikirim ke webapp provinsi dan tampil pada dashboard provinsi. Lanjutkan?"),
        footer = shiny::tagList(
          shiny::modalButton("Batal"),
          shiny::actionButton("kirim_konfirmasi", "Ya, kirim sekarang", class = "btn-primary"))
      ))
    })

    shiny::observeEvent(input$kirim_konfirmasi, {
      shiny::removeModal()
      hasil <- tryCatch(kirim_agregat(rv$payload, pengaturan(con, "provinsi_url"),
                                      pengaturan(con, "provinsi_api_key")),
                        error = function(e) list(status = "gagal", respons = conditionMessage(e)))
      DBI::dbExecute(con,
        "INSERT INTO pengiriman (investigasi_id, uid, payload, status, respons, dikirim_oleh)
         VALUES (?, ?, ?, ?, ?, ?)",
        list(rv$inv_id, rv$payload$uid,
             jsonlite::toJSON(rv$payload, auto_unbox = TRUE, null = "null"),
             hasil$status, hasil$respons, rv$pengguna$id))
      catat_audit(con, rv$pengguna$id, "kirim_agregat", list(status = hasil$status))
      rv$muat_ulang <- rv$muat_ulang + 1
      notifikasi(if (identical(hasil$status, "terkirim"))
        sprintf("Laporan agregat terkirim dengan nomor %s.", rv$payload$uid)
        else sprintf("Pengiriman gagal: %s", hasil$respons),
        if (identical(hasil$status, "terkirim")) "message" else "error")
    })

    output$tabel_pengiriman <- DT::renderDT({
      shiny::req(rv$inv_id)
      rv$muat_ulang
      d <- DBI::dbGetQuery(con,
        "SELECT dikirim_pada AS waktu, uid, status, substr(respons, 1, 120) AS respons
         FROM pengiriman WHERE investigasi_id = ? ORDER BY id DESC", list(rv$inv_id))
      tabel_dt(d)
    })

    ## ---------------- Pengaturan ----------------
    shiny::observe({
      shiny::req(rv$pengguna)
      shiny::updateTextInput(session, "set_kobo_url", value = pengaturan(con, "kobo_base_url"))
      shiny::updateTextInput(session, "set_node_kode", value = pengaturan(con, "node_kode"))
      shiny::updateTextInput(session, "set_node_nama", value = pengaturan(con, "node_nama"))
      shiny::updateTextInput(session, "set_kabupaten", value = pengaturan(con, "kabupaten"))
      shiny::updateTextInput(session, "set_provinsi", value = pengaturan(con, "provinsi"))
      shiny::updateTextInput(session, "set_prov_url", value = pengaturan(con, "provinsi_url"))
    })

    shiny::observeEvent(input$set_kobo_simpan, {
      if (!boleh()) return(notifikasi("Peran Anda tidak dapat mengubah pengaturan.", "error"))
      if (!nzchar(input$set_kobo_token)) return(notifikasi("Token API wajib diisi.", "error"))
      hasil <- tryCatch(kobo_uji_koneksi(input$set_kobo_url, input$set_kobo_token),
                        error = function(e) e)
      if (inherits(hasil, "error")) return(notifikasi(conditionMessage(hasil), "error"))
      pengaturan(con, "kobo_base_url", input$set_kobo_url)
      pengaturan(con, "kobo_token", input$set_kobo_token)
      catat_audit(con, rv$pengguna$id, "simpan_kredensial_kobo")
      notifikasi(sprintf("Token tersimpan dan terverifikasi untuk pengguna %s.", hasil$username))
    })

    shiny::observeEvent(input$set_simpan, {
      if (!identical(rv$pengguna$peran, "admin")) {
        return(notifikasi("Hanya admin yang dapat mengubah pengaturan node.", "error"))
      }
      pengaturan(con, "node_kode", input$set_node_kode)
      pengaturan(con, "node_nama", input$set_node_nama)
      pengaturan(con, "kabupaten", input$set_kabupaten)
      pengaturan(con, "provinsi", input$set_provinsi)
      pengaturan(con, "provinsi_url", input$set_prov_url)
      if (nzchar(input$set_prov_key)) pengaturan(con, "provinsi_api_key", input$set_prov_key)
      catat_audit(con, rv$pengguna$id, "simpan_pengaturan_node")
      notifikasi("Pengaturan node tersimpan.")
    })

    shiny::observeEvent(input$sandi_simpan, {
      hasil <- tryCatch(ganti_sandi(con, rv$pengguna$id, input$sandi_baru), error = function(e) e)
      if (inherits(hasil, "error")) return(notifikasi(conditionMessage(hasil), "error"))
      notifikasi("Kata sandi berhasil diganti.")
    })

    output$ui_pengguna <- shiny::renderUI({
      shiny::req(rv$pengguna)
      if (!identical(rv$pengguna$peran, "admin")) return(NULL)
      shiny::div(class = "card",
        shiny::div(class = "card-header", "Manajemen pengguna"),
        shiny::div(class = "card-body",
          shiny::fluidRow(
            shiny::column(6, shiny::textInput("pg_username", "Nama pengguna")),
            shiny::column(6, shiny::textInput("pg_nama", "Nama lengkap"))),
          shiny::fluidRow(
            shiny::column(6, shiny::passwordInput("pg_sandi", "Kata sandi awal")),
            shiny::column(6, shiny::selectInput("pg_peran", "Peran",
              c("Admin" = "admin", "Analis" = "analis", "Viewer" = "viewer"), "analis"))),
          shiny::actionButton("pg_tambah", "Tambah pengguna", class = "btn-primary"),
          shiny::br(), shiny::br(),
          DT::DTOutput("tabel_pengguna")))
    })

    shiny::observeEvent(input$pg_tambah, {
      hasil <- tryCatch(
        buat_pengguna(con, input$pg_username, input$pg_sandi, input$pg_nama, input$pg_peran),
        error = function(e) e)
      if (inherits(hasil, "error")) return(notifikasi(conditionMessage(hasil), "error"))
      catat_audit(con, rv$pengguna$id, "buat_pengguna", list(username = input$pg_username))
      rv$muat_ulang <- rv$muat_ulang + 1
      notifikasi("Pengguna baru dibuat. Kata sandi wajib diganti saat login pertama.")
    })

    output$tabel_pengguna <- DT::renderDT({
      rv$muat_ulang
      tabel_dt(DBI::dbGetQuery(con,
        "SELECT username, nama, peran, aktif, dibuat_pada FROM pengguna ORDER BY id"))
    })

    session$onSessionEnded(function() invisible(NULL))
  }
}
