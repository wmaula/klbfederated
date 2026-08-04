#' @keywords internal
#' @noRd
ui_provinsi <- function() {
  shiny::fluidPage(
    theme = tema_app(),
    shiny::tags$head(shiny::tags$link(rel = "stylesheet", href = "klb/styles.css")),
    shiny::uiOutput("kerangka")
  )
}

#' @keywords internal
#' @noRd
ui_login_provinsi <- function() {
  shiny::div(
    class = "klb-login",
    shiny::div(class = "card", shiny::div(class = "card-body",
      shiny::h4("Dashboard KLB Provinsi", class = "mb-1"),
      shiny::p(class = "klb-kecil", "Dinas kesehatan provinsi. Masuk dengan akun Anda."),
      shiny::textInput("login_user", "Nama pengguna"),
      shiny::passwordInput("login_sandi", "Kata sandi"),
      shiny::actionButton("login_kirim", "Masuk", class = "btn-primary w-100"),
      shiny::uiOutput("login_pesan")
    ))
  )
}

#' @keywords internal
#' @noRd
ui_utama_provinsi <- function(pengguna) {
  shiny::navbarPage(
    title = shiny::span("Dashboard KLB Provinsi",
                        shiny::span(class = "klb-pill", style = "margin-left:8px",
                                    "data agregat dari node kabupaten atau kota")),
    id = "nav", collapsible = TRUE,

    shiny::tabPanel(
      "Dashboard",
      shiny::div(class = "container-fluid p-3",
        shiny::uiOutput("kartu_ringkas"),
        shiny::div(class = "card mb-3",
          shiny::div(class = "card-header", "Penyaring"),
          shiny::div(class = "card-body",
            shiny::fluidRow(
              shiny::column(3, shiny::selectInput("f_jenis", "Jenis KLB",
                c("Semua" = "", "Keracunan pangan" = "keracunan_pangan",
                  "Penyakit menular" = "penyakit_menular", "PD3I" = "pd3i"))),
              shiny::column(3, shiny::selectInput("f_status", "Status",
                c("Semua" = "", "Berlangsung" = "berlangsung", "Berakhir" = "berakhir"))),
              shiny::column(3, shiny::selectInput("f_kabupaten", "Kabupaten atau kota", "Semua")),
              shiny::column(3, shiny::dateRangeInput("f_tanggal", "Rentang tanggal mulai",
                                                     start = NA, end = NA))
            ))),
        shiny::fluidRow(
          shiny::column(6, shiny::div(class = "card mb-3",
            shiny::div(class = "card-header", "Sebaran KLB di wilayah provinsi"),
            shiny::div(class = "card-body", leaflet::leafletOutput("peta_provinsi", height = "420px")))),
          shiny::column(6, shiny::div(class = "card mb-3",
            shiny::div(class = "card-header", "Kasus menurut bulan kejadian"),
            shiny::div(class = "card-body", shiny::plotOutput("plot_bulan", height = "200px"),
                       shiny::h6("Kasus menurut jenis KLB", class = "mt-3"),
                       DT::DTOutput("tabel_jenis"))))
        ),
        shiny::fluidRow(
          shiny::column(6, shiny::div(class = "card mb-3",
            shiny::div(class = "card-header", "Rekapitulasi menurut kabupaten atau kota"),
            shiny::div(class = "card-body", DT::DTOutput("tabel_kabupaten")))),
          shiny::column(6, shiny::div(class = "card mb-3",
            shiny::div(class = "card-header", "Patogen atau agen penyebab yang dilaporkan"),
            shiny::div(class = "card-body", DT::DTOutput("tabel_patogen"))))
        ),
        shiny::div(class = "card mb-3",
          shiny::div(class = "card-header", "Daftar KLB"),
          shiny::div(class = "card-body",
            shiny::p(class = "klb-kecil", "Pilih satu baris untuk melihat rincian laporan agregat."),
            DT::DTOutput("tabel_klb"))),
        shiny::uiOutput("detail_klb")
      )
    ),

    shiny::tabPanel(
      "Node dan pengguna",
      shiny::div(class = "container-fluid p-3",
        shiny::uiOutput("kunci_baru"),
        shiny::fluidRow(
          shiny::column(7, shiny::div(class = "card mb-3",
            shiny::div(class = "card-header", "Node kabupaten atau kota terdaftar"),
            shiny::div(class = "card-body",
              shiny::fluidRow(
                shiny::column(4, shiny::textInput("node_kode", "Kode node", placeholder = "KAB-SLEMAN")),
                shiny::column(5, shiny::textInput("node_nama", "Nama node")),
                shiny::column(3, shiny::textInput("node_kabupaten", "Kabupaten atau kota"))),
              shiny::actionButton("node_daftar", "Daftarkan node", class = "btn-primary"),
              shiny::actionButton("node_putar", "Ganti kunci node terpilih", class = "btn-outline-secondary"),
              shiny::br(), shiny::br(),
              DT::DTOutput("tabel_node")))),
          shiny::column(5, shiny::div(class = "card mb-3",
            shiny::div(class = "card-header", "Pengguna dashboard"),
            shiny::div(class = "card-body",
              shiny::textInput("pg_username", "Nama pengguna"),
              shiny::textInput("pg_nama", "Nama lengkap"),
              shiny::passwordInput("pg_sandi", "Kata sandi awal"),
              shiny::selectInput("pg_peran", "Peran",
                c("Admin" = "admin", "Analis" = "analis", "Viewer" = "viewer"), "viewer"),
              shiny::actionButton("pg_tambah", "Tambah pengguna", class = "btn-primary"),
              shiny::br(), shiny::br(),
              DT::DTOutput("tabel_pengguna")))))
      )
    ),

    shiny::tabPanel(shiny::span("Keluar"), value = "keluar",
      shiny::div(class = "container-fluid p-3",
                 shiny::p("Sesi diakhiri. Muat ulang halaman untuk masuk kembali.")))
  )
}

#' @keywords internal
#' @noRd
server_provinsi <- function(con) {
  function(input, output, session) {
    rv <- shiny::reactiveValues(pengguna = NULL, muat = 0, kunci = NULL, uid = NULL)

    output$kerangka <- shiny::renderUI({
      if (is.null(rv$pengguna)) ui_login_provinsi() else ui_utama_provinsi(rv$pengguna)
    })

    shiny::observeEvent(input$login_kirim, {
      pengguna <- periksa_login(con, input$login_user, input$login_sandi)
      if (is.null(pengguna)) {
        output$login_pesan <- shiny::renderUI(
          shiny::div(class = "alert alert-danger mt-2 py-2", "Username atau kata sandi salah."))
        return(invisible())
      }
      rv$pengguna <- pengguna
    })

    shiny::observeEvent(input$nav, if (identical(input$nav, "keluar")) rv$pengguna <- NULL)

    laporan <- shiny::reactive({
      rv$muat
      shiny::req(rv$pengguna)
      d <- DBI::dbGetQuery(con, "SELECT * FROM laporan_agregat ORDER BY
        COALESCE(tanggal_mulai, diterima_pada) DESC")
      if (nrow(d) == 0) return(d)
      if (nzchar(input$f_jenis %||% "")) d <- d[d$jenis == input$f_jenis, ]
      if (nzchar(input$f_status %||% "")) d <- d[d$status == input$f_status, ]
      if (!is.null(input$f_kabupaten) && !identical(input$f_kabupaten, "Semua")) {
        d <- d[d$kabupaten == input$f_kabupaten, ]
      }
      rentang <- input$f_tanggal
      if (!is.null(rentang) && !any(is.na(rentang))) {
        mulai <- as.Date(substr(d$tanggal_mulai, 1, 10))
        d <- d[!is.na(mulai) & mulai >= rentang[1] & mulai <= rentang[2], ]
      }
      d
    })

    shiny::observe({
      shiny::req(rv$pengguna)
      semua <- DBI::dbGetQuery(con, "SELECT DISTINCT kabupaten FROM laporan_agregat ORDER BY kabupaten")
      shiny::updateSelectInput(session, "f_kabupaten", choices = c("Semua", semua$kabupaten))
    })

    output$kartu_ringkas <- shiny::renderUI({
      d <- laporan()
      shiny::fluidRow(
        shiny::column(3, kartu_stat("Total KLB dilaporkan", nrow(d),
          sprintf("%d masih berlangsung", sum(d$status == "berlangsung")))),
        shiny::column(3, kartu_stat("Total kasus", sum(d$total_kasus, na.rm = TRUE))),
        shiny::column(3, kartu_stat("Total kematian", sum(d$total_meninggal, na.rm = TRUE))),
        shiny::column(3, kartu_stat("Kabupaten atau kota terdampak", length(unique(d$kabupaten))))
      )
    })

    output$peta_provinsi <- leaflet::renderLeaflet({
      d <- laporan()
      d <- d[!is.na(d$latitude) & !is.na(d$longitude), ]
      peta <- leaflet::leaflet() |> leaflet::addProviderTiles("CartoDB.Positron")
      if (nrow(d) == 0) return(peta |> leaflet::setView(110.37, -7.8, zoom = 9))
      warna <- ifelse(d$status == "berlangsung", "#a63232", "#16556b")
      peta |>
        leaflet::addCircleMarkers(
          lng = d$longitude, lat = d$latitude,
          radius = 6 + 18 * d$total_kasus / max(d$total_kasus, na.rm = TRUE),
          color = warna, fillColor = warna, fillOpacity = 0.35, weight = 1.5,
          popup = sprintf("<strong>%s</strong><br>%s<br>%s kasus, attack rate %s persen<br>Patogen diduga: %s",
                          d$nama_klb, d$kabupaten, d$total_kasus, angka_id(d$attack_rate),
                          ifelse(is.na(d$patogen), "belum ditetapkan", d$patogen))) |>
        leaflet::addLegend("bottomright", colors = c("#a63232", "#16556b"),
                           labels = c("KLB berlangsung", "KLB berakhir"), opacity = 0.7)
    })

    output$plot_bulan <- shiny::renderPlot({
      d <- laporan()
      shiny::req(nrow(d) > 0)
      d$bulan <- substr(ifelse(is.na(d$tanggal_mulai), d$diterima_pada, d$tanggal_mulai), 1, 7)
      ringkas <- stats::aggregate(total_kasus ~ bulan, data = d, FUN = sum)
      ggplot2::ggplot(ringkas, ggplot2::aes(x = .data$bulan, y = .data$total_kasus)) +
        ggplot2::geom_col(fill = WARNA_KLB[["utama"]], width = 0.65) +
        ggplot2::labs(x = NULL, y = "Jumlah kasus") +
        tema_klb() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    })

    output$tabel_jenis <- DT::renderDT({
      d <- laporan()
      shiny::req(nrow(d) > 0)
      ringkas <- stats::aggregate(cbind(jumlah_klb = rep(1, nrow(d)), kasus = d$total_kasus),
                                  by = list(Jenis = gsub("_", " ", d$jenis)), FUN = sum)
      tabel_dt(ringkas)
    })

    output$tabel_kabupaten <- DT::renderDT({
      d <- laporan()
      shiny::req(nrow(d) > 0)
      ringkas <- stats::aggregate(
        cbind(KLB = rep(1, nrow(d)), Kasus = d$total_kasus, Meninggal = d$total_meninggal),
        by = list(`Kabupaten atau kota` = d$kabupaten), FUN = sum)
      tabel_dt(ringkas[order(-ringkas$Kasus), ])
    })

    output$tabel_patogen <- DT::renderDT({
      d <- laporan()
      shiny::req(nrow(d) > 0)
      patogen <- ifelse(is.na(d$patogen) | !nzchar(d$patogen), "belum ditetapkan", d$patogen)
      ringkas <- stats::aggregate(cbind(KLB = rep(1, nrow(d)), Kasus = d$total_kasus),
                                  by = list(`Patogen atau agen` = patogen), FUN = sum)
      tabel_dt(ringkas[order(-ringkas$Kasus), ])
    })

    output$tabel_klb <- DT::renderDT({
      d <- laporan()
      if (nrow(d) == 0) return(tabel_dt(NULL))
      tampil <- data.frame(
        KLB = d$nama_klb, `Kabupaten atau kota` = d$kabupaten,
        `Patogen diduga` = ifelse(is.na(d$patogen), "belum ditetapkan", d$patogen),
        Mulai = substr(d$tanggal_mulai, 1, 10), Status = d$status,
        Kasus = d$total_kasus, Meninggal = d$total_meninggal,
        `AR persen` = round(d$attack_rate, 1),
        check.names = FALSE, stringsAsFactors = FALSE)
      DT::datatable(tampil, rownames = FALSE, selection = "single",
                    options = list(dom = "tp", pageLength = 10, scrollX = TRUE))
    })

    shiny::observeEvent(input$tabel_klb_rows_selected, {
      d <- laporan()
      baris <- input$tabel_klb_rows_selected
      rv$uid <- if (length(baris) == 1) d$uid[baris] else NULL
    })

    output$detail_klb <- shiny::renderUI({
      shiny::req(rv$uid)
      baris <- DBI::dbGetQuery(con, "SELECT payload FROM laporan_agregat WHERE uid = ?", list(rv$uid))
      shiny::req(nrow(baris) > 0)
      p <- jsonlite::fromJSON(baris$payload[1], simplifyVector = FALSE)

      tabel_list <- function(x, kolom) {
        if (is.null(x) || length(x) == 0) return(NULL)
        d <- do.call(rbind, lapply(x, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
        d <- d[, intersect(kolom, names(d)), drop = FALSE]
        tabel_dt(bulat(d))
      }

      shiny::div(class = "card mb-3",
        shiny::div(class = "card-header", sprintf("Rincian: %s", p$klb$nama)),
        shiny::div(class = "card-body",
          shiny::p(class = "klb-kecil", sprintf(
            "Dilaporkan oleh %s (%s) pada %s. Data individu tetap berada di node tersebut.",
            p$node$nama, p$dikirim_oleh, p$dikirim_pada)),
          shiny::fluidRow(
            shiny::column(3, kartu_stat("Kasus", p$angka$total_kasus,
                                        sprintf("populasi berisiko %s", p$angka$populasi_berisiko))),
            shiny::column(3, kartu_stat("Attack rate", sprintf("%s persen", angka_id(p$angka$attack_rate)))),
            shiny::column(3, kartu_stat("Meninggal", p$angka$total_meninggal)),
            shiny::column(3, kartu_stat("Konfirmasi laboratorium",
                                        p$angka$konfirmasi_lab %||% "-"))),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(6,
              shiny::h6("Attack rate menurut kelompok umur"),
              tabel_list(p$distribusi$kelompok_umur, c("kategori", "populasi", "kasus", "ar")),
              shiny::h6("Attack rate menurut jenis kelamin", class = "mt-3"),
              tabel_list(p$distribusi$jenis_kelamin, c("kategori", "populasi", "kasus", "ar"))),
            shiny::column(6,
              shiny::h6("Gejala utama"),
              tabel_list(p$distribusi$gejala_utama, c("kategori", "n", "persen")),
              shiny::h6("Faktor risiko teratas", class = "mt-3"),
              tabel_list(p$faktor_risiko_teratas, c("label", "ukuran", "estimasi", "ci_bawah", "ci_atas", "p_value")))
          ),
          shiny::h6("Ringkasan naratif dan rekomendasi", class = "mt-3"),
          shiny::p(class = "klb-kecil", p$ringkasan_naratif %||% "tidak ada"),
          shiny::tags$ol(lapply(p$rekomendasi, shiny::tags$li)),
          shiny::h6("Kriteria KLB", class = "mt-3"),
          tabel_list(p$kriteria_klb, c("kode", "uraian", "terpenuhi", "keterangan"))
        ))
    })

    ## ---------------- Node dan pengguna ----------------
    output$tabel_node <- DT::renderDT({
      rv$muat
      shiny::req(rv$pengguna)
      d <- DBI::dbGetQuery(con,
        "SELECT id, kode, nama, kabupaten, aktif, terakhir_kirim FROM node ORDER BY kode")
      DT::datatable(d, rownames = FALSE, selection = "single", options = list(dom = "tp"))
    })

    shiny::observeEvent(input$node_daftar, {
      if (!identical(rv$pengguna$peran, "admin")) {
        return(notifikasi("Hanya admin yang dapat mendaftarkan node.", "error"))
      }
      hasil <- tryCatch(
        daftarkan_node(con, input$node_kode, input$node_nama, input$node_kabupaten),
        error = function(e) e)
      if (inherits(hasil, "error")) return(notifikasi(conditionMessage(hasil), "error"))
      rv$kunci <- list(kode = input$node_kode, kunci = hasil)
      rv$muat <- rv$muat + 1
    })

    shiny::observeEvent(input$node_putar, {
      shiny::req(input$tabel_node_rows_selected)
      d <- DBI::dbGetQuery(con, "SELECT id, kode FROM node ORDER BY kode")
      baris <- d[input$tabel_node_rows_selected, ]
      kunci <- putar_kunci_node(con, baris$id)
      rv$kunci <- list(kode = baris$kode, kunci = kunci)
      rv$muat <- rv$muat + 1
    })

    output$kunci_baru <- shiny::renderUI({
      if (is.null(rv$kunci)) return(NULL)
      shiny::div(class = "alert alert-warning",
        shiny::strong(sprintf("Kunci API untuk node %s hanya ditampilkan satu kali.", rv$kunci$kode)),
        shiny::p("Salin dan masukkan pada menu Pengaturan di node kabupaten atau kota tersebut."),
        shiny::tags$code(style = "word-break:break-all", rv$kunci$kunci))
    })

    shiny::observeEvent(input$pg_tambah, {
      if (!identical(rv$pengguna$peran, "admin")) {
        return(notifikasi("Hanya admin yang dapat menambah pengguna.", "error"))
      }
      hasil <- tryCatch(
        buat_pengguna(con, input$pg_username, input$pg_sandi, input$pg_nama, input$pg_peran),
        error = function(e) e)
      if (inherits(hasil, "error")) return(notifikasi(conditionMessage(hasil), "error"))
      rv$muat <- rv$muat + 1
      notifikasi("Pengguna baru dibuat.")
    })

    output$tabel_pengguna <- DT::renderDT({
      rv$muat
      shiny::req(rv$pengguna)
      tabel_dt(DBI::dbGetQuery(con,
        "SELECT username, nama, peran, aktif FROM pengguna ORDER BY id"))
    })
  }
}
