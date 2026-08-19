#' Menjalankan aplikasi node kabupaten atau kota
#'
#' Membuka aplikasi Shiny untuk petugas dinas kesehatan kabupaten atau kota:
#' menarik data investigasi dari KoboToolbox, memetakan variabel wajib,
#' menjalankan analisis KLB, menyusun draf laporan dengan bantuan WebLLM, dan
#' mengirim ringkasan agregat ke webapp provinsi.
#'
#' Data individu disimpan pada basis data SQLite lokal di [direktori_data()]
#' dan tidak pernah dikirim ke tingkat provinsi.
#'
#' @param port Port aplikasi.
#' @param berkas_db Lokasi berkas SQLite. Bila `NULL` memakai lokasi bawaan.
#' @param data_contoh Bila `TRUE`, memuat dua investigasi contoh pada basis data
#'   yang masih kosong.
#' @param launch_browser Membuka peramban secara otomatis.
#' @return Tidak mengembalikan nilai, menjalankan aplikasi Shiny.
#' @export
#' @examplesIf interactive()
#' jalankan_node(data_contoh = TRUE)
jalankan_node <- function(port = 4001, berkas_db = NULL, data_contoh = FALSE,
                          launch_browser = interactive()) {
  con <- buka_db("kabupaten", berkas_db)
  if (data_contoh) {
    jumlah <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM investigasi")$n
    if (jumlah == 0) muat_data_contoh(con)
  }
  shiny::addResourcePath("klb", system.file("app/www", package = "klbfederated"))
  aplikasi <- shiny::shinyApp(ui = ui_node(), server = server_node(con),
                              onStart = function() {
                                shiny::onStop(function() DBI::dbDisconnect(con))
                              })
  shiny::runApp(aplikasi, port = port, launch.browser = launch_browser, host = "127.0.0.1")
}

#' Menjalankan dashboard provinsi
#'
#' Membuka aplikasi Shiny untuk dinas kesehatan provinsi yang menampilkan
#' seluruh KLB yang dilaporkan node kabupaten atau kota, sekaligus membuka
#' endpoint penerimaan laporan agregat pada port terpisah.
#'
#' @param port Port aplikasi Shiny.
#' @param port_ingest Port endpoint penerimaan laporan agregat.
#' @param berkas_db Lokasi berkas SQLite. Bila `NULL` memakai lokasi bawaan.
#' @param launch_browser Membuka peramban secara otomatis.
#' @return Tidak mengembalikan nilai, menjalankan aplikasi Shiny.
#' @export
#' @examplesIf interactive()
#' jalankan_dashboard_provinsi()
jalankan_dashboard_provinsi <- function(port = 4102, port_ingest = 4002,
                                        berkas_db = NULL, launch_browser = interactive()) {
  con <- buka_db("provinsi", berkas_db)
  server_ingest <- jalankan_penerima(con, port_ingest)
  shiny::addResourcePath("klb", system.file("app/www", package = "klbfederated"))
  aplikasi <- shiny::shinyApp(
    ui = ui_provinsi(), server = server_provinsi(con),
    onStart = function() {
      shiny::onStop(function() {
        try(httpuv::stopServer(server_ingest), silent = TRUE)
        DBI::dbDisconnect(con)
      })
    })
  shiny::runApp(aplikasi, port = port, launch.browser = launch_browser, host = "127.0.0.1")
}
