#' @keywords internal
#' @noRd
ui_login_node <- function() {
  shiny::div(
    class = "klb-login",
    shiny::div(
      class = "card",
      shiny::div(class = "card-body",
        shiny::h4("Analisis KLB Federated", class = "mb-1"),
        shiny::p(class = "klb-kecil", "Node kabupaten atau kota. Masuk dengan akun dinas kesehatan Anda."),
        shiny::textInput("login_user", "Nama pengguna"),
        shiny::passwordInput("login_sandi", "Kata sandi"),
        shiny::actionButton("login_kirim", "Masuk", class = "btn-primary w-100"),
        shiny::uiOutput("login_pesan")
      )
    )
  )
}

#' @keywords internal
#' @noRd
ui_node <- function() {
  shiny::fluidPage(
    theme = tema_app(),
    # Penanda versi memaksa peramban mengambil ulang aset setelah package
    # diperbarui, sehingga perbaikan tidak tertutup oleh cache lama.
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", href = paste0("klb/styles.css?v=", versi_aset())),
      shiny::tags$script(src = paste0("klb/webllm-bridge.js?v=", versi_aset()), type = "module")
    ),
    shiny::uiOutput("kerangka")
  )
}

#' @keywords internal
#' @noRd
ui_utama_node <- function(pengguna, node) {
  shiny::navbarPage(
    title = shiny::span("Analisis KLB Federated",
                        shiny::span(class = "klb-pill", style = "margin-left:8px",
                                    sprintf("%s \u00b7 %s", node$kabupaten, node$provinsi))),
    id = "nav", collapsible = TRUE,

    shiny::tabPanel(
      "Investigasi",
      shiny::div(class = "container-fluid p-3",
        shiny::fluidRow(
          shiny::column(7,
            shiny::div(class = "card mb-3",
              shiny::div(class = "card-header", "Daftar investigasi KLB"),
              shiny::div(class = "card-body",
                shiny::p(class = "klb-kecil",
                  "Setiap investigasi berpasangan dengan satu proyek KoboToolbox. Data individu tersimpan di node ini dan tidak dikirim ke provinsi."),
                DT::DTOutput("tabel_investigasi"),
                shiny::uiOutput("investigasi_terpilih")
              )
            )
          ),
          shiny::column(5,
            shiny::div(class = "card mb-3",
              shiny::div(class = "card-header", "Investigasi baru"),
              shiny::div(class = "card-body",
                shiny::textInput("inv_nama", "Nama investigasi", width = "100%"),
                shiny::selectInput("inv_jenis", "Jenis KLB", width = "100%",
                  choices = c("KLB keracunan pangan" = "keracunan_pangan",
                              "KLB penyakit menular" = "penyakit_menular",
                              "KLB penyakit yang dapat dicegah dengan imunisasi" = "pd3i")),
                shiny::textInput("inv_penyakit", "Penyakit atau sindrom", width = "100%"),
                shiny::fluidRow(
                  shiny::column(6, shiny::textInput("inv_provinsi", "Provinsi", node$provinsi)),
                  shiny::column(6, shiny::textInput("inv_kabupaten", "Kabupaten atau kota", node$kabupaten))
                ),
                shiny::fluidRow(
                  shiny::column(6, shiny::textInput("inv_kecamatan", "Kecamatan")),
                  shiny::column(6, shiny::textInput("inv_desa", "Desa atau kelurahan"))
                ),
                shiny::fluidRow(
                  shiny::column(6, shiny::dateInput("inv_tanggal", "Tanggal laporan diterima", value = Sys.Date())),
                  shiny::column(6, shiny::numericInput("inv_populasi", "Populasi berisiko", NA, min = 1))
                ),
                shiny::fluidRow(
                  shiny::column(6, shiny::numericInput("inv_lat", "Lintang", NA)),
                  shiny::column(6, shiny::numericInput("inv_lon", "Bujur", NA))
                ),
                shiny::actionButton("inv_simpan", "Simpan investigasi", class = "btn-primary"),
                shiny::actionButton("inv_contoh", "Muat data contoh", class = "btn-outline-secondary")
              )
            )
          )
        )
      )
    ),

    shiny::tabPanel(
      "Data KoboToolbox",
      shiny::div(class = "container-fluid p-3",
        shiny::div(class = "card mb-3",
          shiny::div(class = "card-header", "Tautan ke proyek KoboToolbox"),
          shiny::div(class = "card-body",
            shiny::uiOutput("kobo_status"),
            shiny::fluidRow(
              shiny::column(4, shiny::actionButton("kobo_muat", "Muat daftar proyek", class = "btn-outline-primary")),
              shiny::column(4, shiny::actionButton("kobo_tautkan", "Tautkan proyek terpilih", class = "btn-outline-primary")),
              shiny::column(4, shiny::actionButton("kobo_impor", "Tarik data terbaru", class = "btn-primary"))
            ),
            shiny::br(),
            DT::DTOutput("tabel_kobo")
          )
        ),
        shiny::div(class = "card",
          shiny::div(class = "card-header", "Pratinjau data investigasi"),
          shiny::div(class = "card-body",
            shiny::p(class = "klb-kecil",
              "Data individu ini tersimpan di node kabupaten atau kota dan tidak pernah dikirim ke provinsi."),
            DT::DTOutput("tabel_pratinjau")
          )
        )
      )
    ),

    shiny::tabPanel(
      "Pemetaan variabel",
      shiny::div(class = "container-fluid p-3",
        shiny::div(class = "card mb-3",
          shiny::div(class = "card-header", "Status kelengkapan variabel"),
          shiny::div(class = "card-body", shiny::uiOutput("validasi_pemetaan"))
        ),
        shiny::fluidRow(
          shiny::column(7,
            shiny::div(class = "card mb-3",
              shiny::div(class = "card-header", "Peran variabel"),
              shiny::div(class = "card-body", shiny::uiOutput("ui_pemetaan"))
            )
          ),
          shiny::column(5,
            shiny::div(class = "card mb-3",
              shiny::div(class = "card-header", "Pengaturan analisis"),
              shiny::div(class = "card-body",
                shiny::textInput("cfg_umur", "Batas kelompok umur (pisahkan dengan koma)", "0, 5, 13, 19, 46, 65"),
                shiny::fluidRow(
                  shiny::column(6, shiny::selectInput("cfg_satuan", "Satuan waktu kurva",
                    c("Menit" = "menit", "Jam" = "jam", "Hari" = "hari", "Minggu" = "minggu"), "jam")),
                  shiny::column(6, shiny::numericInput("cfg_interval", "Interval kurva (kosong berarti otomatis)", NA))
                ),
                shiny::fluidRow(
                  shiny::column(6, shiny::selectInput("cfg_desain", "Desain studi",
                    c("Kohort retrospektif (RR)" = "kohort", "Kasus kontrol (OR)" = "kasus_kontrol"))),
                  shiny::column(6, shiny::numericInput("cfg_ambang", "Ambang p kandidat multivariabel", 0.25, step = 0.05))
                ),
                shiny::fluidRow(
                  shiny::column(4, shiny::numericInput("cfg_populasi", "Populasi berisiko", NA)),
                  shiny::column(4, shiny::numericInput("cfg_ink_min", "Inkubasi rujukan min (jam)", NA)),
                  shiny::column(4, shiny::numericInput("cfg_ink_maks", "Inkubasi rujukan maks (jam)", NA))
                ),
                shiny::hr(),
                shiny::h6("Data pembanding kriteria KLB"),
                shiny::fluidRow(
                  shiny::column(6, shiny::numericInput("pb_kasus", "Kasus periode sebelumnya", NA)),
                  shiny::column(6, shiny::numericInput("pb_rata_lalu", "Rata-rata kasus per bulan tahun lalu", NA))
                ),
                shiny::fluidRow(
                  shiny::column(6, shiny::numericInput("pb_rata_ini", "Rata-rata kasus per bulan tahun ini", NA)),
                  shiny::column(6, shiny::numericInput("pb_cfr", "CFR periode sebelumnya (persen)", NA))
                ),
                shiny::fluidRow(
                  shiny::column(6, shiny::numericInput("pb_prop_lalu", "Proporsi periode sebelumnya (persen)", NA)),
                  shiny::column(6, shiny::numericInput("pb_prop_ini", "Proporsi periode ini (persen)", NA))
                ),
                shiny::selectInput("pb_baru", "Penyakit belum pernah ada di wilayah ini",
                                   c("Belum dinilai" = "", "Ya" = "ya", "Tidak" = "tidak")),
                shiny::actionButton("pemetaan_simpan", "Simpan pemetaan dan pengaturan", class = "btn-primary")
              )
            )
          )
        )
      )
    ),

    shiny::tabPanel(
      "Analisis",
      shiny::div(class = "container-fluid p-3",
        shiny::div(class = "card mb-3",
          shiny::div(class = "card-header", "Analisis epidemiologi"),
          shiny::div(class = "card-body",
            shiny::p(class = "klb-kecil",
              "Analisis dijalankan dengan R di server node ini. Data individu tidak dikirim ke luar node."),
            shiny::actionButton("analisis_jalankan", "Jalankan analisis", class = "btn-primary"),
            shiny::uiOutput("analisis_catatan")
          )
        ),
        shiny::uiOutput("analisis_isi")
      )
    ),

    shiny::tabPanel(
      "Laporan",
      shiny::div(class = "container-fluid p-3",
        shiny::fluidRow(
          shiny::column(5,
            shiny::div(class = "card mb-3",
              shiny::div(class = "card-header", "Metadata laporan"),
              shiny::div(class = "card-body",
                shiny::selectInput("lap_tipe", "Jenis laporan",
                  c("Laporan akhir (KLB berakhir)" = "akhir",
                    "Laporan sementara (KLB berlangsung)" = "sementara")),
                shiny::textInput("lap_pi", "Principal investigator"),
                shiny::textInput("lap_copi", "Co-principal investigator"),
                shiny::textInput("lap_institusi", "Institusi"),
                shiny::textInput("lap_patogen", "Patogen atau agen yang diduga"),
                shiny::textInput("lap_sumber", "Sumber penularan yang diduga"),
                shiny::textAreaInput("lap_kronologi", "Sumber dan kronologi laporan awal", rows = 3),
                shiny::textAreaInput("lap_definisi", "Definisi kasus operasional", rows = 3),
                shiny::textAreaInput("lap_lab", "Hasil laboratorium", rows = 3),
                shiny::textAreaInput("lap_lingkungan", "Temuan studi lingkungan", rows = 3),
                shiny::textAreaInput("lap_upaya", "Upaya pengendalian yang telah dilakukan", rows = 3),
                shiny::textAreaInput("lap_rekomendasi", "Rekomendasi (satu baris satu butir)", rows = 3),
                shiny::actionButton("lap_susun", "Susun draf template", class = "btn-primary"),
                shiny::actionButton("lap_simpan", "Simpan laporan", class = "btn-outline-primary")
              )
            ),
            shiny::div(class = "card mb-3",
              shiny::div(class = "card-header", "Model bahasa lokal (WebLLM)"),
              shiny::div(class = "card-body",
                shiny::p(class = "klb-kecil",
                  "Model berjalan penuh di peramban Anda melalui WebGPU. Isi laporan tidak dikirim ke layanan luar."),
                shiny::uiOutput("llm_status"),
                shiny::selectInput("llm_model", "Model", choices = model_bawaan()),
                shiny::actionButton("llm_muat", "Muat model", class = "btn-outline-primary"),
                shiny::actionButton("llm_lepas", "Lepas model", class = "btn-outline-secondary")
              )
            ),
            shiny::div(class = "card",
              shiny::div(class = "card-header", "Ekspor laporan"),
              shiny::div(class = "card-body",
                shiny::downloadButton("unduh_docx", "Unduh Word (.docx)", class = "btn-outline-primary"),
                shiny::downloadButton("unduh_md", "Unduh Markdown", class = "btn-outline-secondary")
              )
            )
          ),
          shiny::column(7,
            shiny::div(class = "card mb-3",
              shiny::div(class = "card-header", "Naskah laporan"),
              shiny::div(class = "card-body",
                shiny::selectInput("lap_bagian", "Bab laporan",
                                   choices = stats::setNames(bagian_laporan()$kunci,
                                                             bagian_laporan()$judul),
                                   selected = "intisari"),
                shiny::actionButton("llm_tulis", "Tulis bagian ini dengan WebLLM", class = "btn-outline-primary"),
                shiny::actionButton("lap_kembalikan", "Kembalikan draf template", class = "btn-outline-secondary"),
                shiny::uiOutput("llm_periksa"),
                shiny::textAreaInput("lap_naskah", NULL, rows = 20, width = "100%")
              )
            ),
            shiny::div(class = "card",
              shiny::div(class = "card-header", "Checklist kelengkapan laporan"),
              shiny::div(class = "card-body", shiny::uiOutput("ui_checklist"))
            )
          )
        )
      )
    ),

    shiny::tabPanel(
      "Kirim ke provinsi",
      shiny::div(class = "container-fluid p-3",
        shiny::div(class = "card mb-3",
          shiny::div(class = "card-header", "Kirim laporan agregat"),
          shiny::div(class = "card-body",
            shiny::p(class = "klb-kecil",
              "Hanya ringkasan agregat yang dikirim. Baris data individu tidak pernah keluar dari node ini."),
            shiny::fluidRow(
              shiny::column(4, shiny::textInput("kirim_patogen", "Patogen atau agen yang diduga")),
              shiny::column(4, shiny::textInput("kirim_sumber", "Sumber penularan yang diduga")),
              shiny::column(4, shiny::numericInput("kirim_lab", "Jumlah kasus terkonfirmasi laboratorium", NA))
            ),
            shiny::textAreaInput("kirim_ringkasan", "Ringkasan naratif untuk dashboard provinsi", rows = 3, width = "100%"),
            shiny::textAreaInput("kirim_rekomendasi", "Rekomendasi (satu baris satu butir)", rows = 3, width = "100%"),
            shiny::actionButton("kirim_pratinjau", "Buat pratinjau kiriman", class = "btn-outline-primary"),
            shiny::actionButton("kirim_kirim", "Kirim ke provinsi", class = "btn-primary")
          )
        ),
        shiny::uiOutput("kirim_pratinjau_ui"),
        shiny::div(class = "card",
          shiny::div(class = "card-header", "Riwayat pengiriman"),
          shiny::div(class = "card-body", DT::DTOutput("tabel_pengiriman"))
        )
      )
    ),

    shiny::tabPanel(
      "Pengaturan",
      shiny::div(class = "container-fluid p-3",
        shiny::fluidRow(
          shiny::column(6,
            shiny::div(class = "card mb-3",
              shiny::div(class = "card-header", "Koneksi KoboToolbox"),
              shiny::div(class = "card-body",
                shiny::textInput("set_kobo_url", "URL server KoboToolbox", width = "100%"),
                shiny::passwordInput("set_kobo_token", "Token API", width = "100%"),
                shiny::p(class = "klb-kecil", "Token diambil dari menu Account Settings pada KoboToolbox dan disimpan di node ini."),
                shiny::actionButton("set_kobo_simpan", "Uji dan simpan", class = "btn-primary")
              )
            ),
            shiny::div(class = "card mb-3",
              shiny::div(class = "card-header", "Identitas node dan tujuan pengiriman"),
              shiny::div(class = "card-body",
                shiny::fluidRow(
                  shiny::column(6, shiny::textInput("set_node_kode", "Kode node")),
                  shiny::column(6, shiny::textInput("set_node_nama", "Nama node"))
                ),
                shiny::fluidRow(
                  shiny::column(6, shiny::textInput("set_kabupaten", "Kabupaten atau kota")),
                  shiny::column(6, shiny::textInput("set_provinsi", "Provinsi"))
                ),
                shiny::textInput("set_prov_url", "URL webapp provinsi", width = "100%"),
                shiny::passwordInput("set_prov_key", "Kunci API provinsi", width = "100%"),
                shiny::actionButton("set_simpan", "Simpan pengaturan", class = "btn-primary")
              )
            )
          ),
          shiny::column(6,
            shiny::div(class = "card mb-3",
              shiny::div(class = "card-header", "Ganti kata sandi"),
              shiny::div(class = "card-body",
                shiny::passwordInput("sandi_baru", "Kata sandi baru (minimal 8 karakter)"),
                shiny::actionButton("sandi_simpan", "Ganti kata sandi", class = "btn-primary")
              )
            ),
            shiny::uiOutput("ui_pengguna")
          )
        )
      )
    ),

    shiny::tabPanel(
      shiny::span(shiny::icon("right-from-bracket"), "Keluar"), value = "keluar",
      shiny::div(class = "container-fluid p-3",
        shiny::p("Sesi diakhiri. Muat ulang halaman untuk masuk kembali."))
    )
  )
}
