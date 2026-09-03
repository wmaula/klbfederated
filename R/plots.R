#' Tema grafik untuk laporan KLB
#'
#' Tema minimalis tanpa garis kisi tebal agar gambar siap dipakai pada laporan
#' cetak dan naskah publikasi.
#'
#' @param dasar Ukuran huruf dasar.
#' @return Objek tema ggplot2.
#' @export
tema_klb <- function(dasar = 11) {
  ggplot2::theme_minimal(base_size = dasar) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(linewidth = 0.25, colour = "grey88"),
      axis.line.x = ggplot2::element_line(linewidth = 0.4, colour = "grey40"),
      axis.ticks.x = ggplot2::element_line(linewidth = 0.3, colour = "grey60"),
      plot.title = ggplot2::element_text(face = "bold", size = dasar + 1),
      plot.subtitle = ggplot2::element_text(colour = "grey35", size = dasar - 1),
      plot.caption = ggplot2::element_text(colour = "grey40", size = dasar - 2, hjust = 0)
    )
}

WARNA_KLB <- c(utama = "#16556b", aksen = "#a63232", netral = "#c8d2d9")

#' Kurva epidemik
#'
#' @param hasil Objek `klb_hasil` dari [analisis_klb()].
#' @param judul Judul gambar.
#' @return Objek ggplot.
#' @export
#' @examples
#' contoh <- contoh_keracunan_pangan(n = 60)
#' hasil <- analisis_klb(contoh$data, contoh$pemetaan, contoh$konfigurasi)
#' plot_kurva_epidemik(hasil)
plot_kurva_epidemik <- function(hasil, judul = "Kurva epidemik KLB") {
  bins <- hasil$kurva$bins
  if (is.null(bins) || nrow(bins) == 0) {
    return(ggplot2::ggplot() + ggplot2::annotate("text", x = 1, y = 1,
      label = "Belum ada data onset untuk kurva epidemik") + ggplot2::theme_void())
  }
  lebar <- as.numeric(difftime(bins$selesai[1], bins$mulai[1], units = "secs"))
  satuan <- hasil$kurva$satuan
  format_x <- if (satuan %in% c("hari", "minggu")) "%d/%m/%Y" else "%d/%m %H:%M"

  ggplot2::ggplot(bins, ggplot2::aes(x = .data$mulai + lebar / 2, y = .data$kasus)) +
    ggplot2::geom_col(fill = WARNA_KLB[["utama"]], width = lebar * 0.92) +
    ggplot2::scale_x_datetime(labels = function(x) format(x, format_x)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.08)),
                                breaks = function(l) unique(floor(pretty(l)))) +
    ggplot2::labs(
      title = judul,
      subtitle = sprintf("Interval %s jam, bentuk kurva %s",
                         angka_id(hasil$kurva$interval_jam, 2),
                         gsub("_", " ", hasil$kurva$tipe)),
      x = "Waktu mulai sakit", y = "Jumlah kasus",
      caption = hasil$kurva$alasan_tipe
    ) +
    tema_klb() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1,size=14))
}

#' Grafik attack rate menurut kategori
#'
#' @param tabel `data.frame` hasil [tabel_attack_rate()].
#' @param judul Judul gambar.
#' @return Objek ggplot.
#' @export
plot_attack_rate <- function(tabel, judul = "Attack rate menurut kelompok") {
  if (is.null(tabel) || nrow(tabel) == 0) return(ggplot2::ggplot() + ggplot2::theme_void())
  tabel$kategori <- factor(tabel$kategori, levels = rev(tabel$kategori))
  ggplot2::ggplot(tabel, ggplot2::aes(x = .data$ar, y = .data$kategori)) +
    ggplot2::geom_col(fill = WARNA_KLB[["utama"]], width = 0.62) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%s%%", angka_id(.data$ar))),
                       hjust = -0.15, size = 3.1, colour = "grey30") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.16))) +
    ggplot2::labs(title = judul, x = "Attack rate (persen)", y = NULL) +
    tema_klb() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(linewidth = 0.25, colour = "grey88"),
                   axis.text.x=element_text(size=14),
                   axis.text.y=element_text(size=14)
}

#' Grafik hasil analisis paparan
#'
#' Menampilkan estimasi RR atau OR beserta selang kepercayaan 95 persen pada
#' skala logaritmik, dengan garis acuan pada nilai satu.
#'
#' @param paparan `data.frame` hasil [analisis_paparan()].
#' @param judul Judul gambar.
#' @return Objek ggplot.
#' @export
plot_paparan <- function(paparan, judul = "Hasil analisis bivariat variabel paparan") {
  if (is.null(paparan) || nrow(paparan) == 0) return(ggplot2::ggplot() + ggplot2::theme_void())
  d <- paparan[is.finite(paparan$estimasi), ]
  d$label <- factor(d$label, levels = rev(d$label[order(d$estimasi)]))
  d$bermakna <- is.finite(d$p_value) & d$p_value < 0.05
  ggplot2::ggplot(d, ggplot2::aes(x = .data$estimasi, y = .data$label, colour = .data$bermakna)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey55") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$ci_bawah, xmax = .data$ci_atas),
                            height = 0.16, linewidth = 0.5) +
    ggplot2::geom_point(size = 2.1) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_colour_manual(values = c(`TRUE` = WARNA_KLB[["aksen"]], `FALSE` = WARNA_KLB[["utama"]]),
                                 guide = "none") +
    ggplot2::labs(title = judul,
                  subtitle = sprintf("%s dengan selang kepercayaan 95 persen, skala logaritmik",
                                     d$ukuran[1]),
                  x = d$ukuran[1], y = NULL) +
    tema_klb() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

#' Grafik distribusi gejala
#' @param gejala `data.frame` distribusi gejala.
#' @param judul Judul gambar.
#' @return Objek ggplot.
#' @export
plot_gejala <- function(gejala, judul = "Distribusi tanda dan gejala pada kasus") {
  if (is.null(gejala) || nrow(gejala) == 0) return(ggplot2::ggplot() + ggplot2::theme_void())
  gejala$kategori <- factor(gejala$kategori, levels = rev(gejala$kategori))
  ggplot2::ggplot(gejala, ggplot2::aes(x = .data$persen, y = .data$kategori)) +
    ggplot2::geom_col(fill = WARNA_KLB[["utama"]], width = 0.62) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%s (%s%%)", .data$n, angka_id(.data$persen))),
                       hjust = -0.1, size = 3.1, colour = "grey30") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.22))) +
    ggplot2::labs(title = judul, x = "Persentase kasus", y = NULL) +
    tema_klb() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(linewidth = 0.25, colour = "grey88"),
                   axis.text.x=element_text(size=14),
                   axis.text.y=element_text(size=14)
}

#' Spot map sebaran kasus
#'
#' @param hasil Objek `klb_hasil`.
#' @return Peta leaflet, atau `NULL` bila koordinat tidak tersedia.
#' @export
peta_kasus <- function(hasil) {
  spot <- hasil$spot_map
  if (is.null(spot) || nrow(spot) == 0) return(NULL)
  warna <- ifelse(spot$sakit, WARNA_KLB[["aksen"]], WARNA_KLB[["netral"]])
  leaflet::leaflet(spot) |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::addCircleMarkers(
      lng = ~lon, lat = ~lat, radius = ifelse(spot$sakit, 6, 4),
      color = warna, fillColor = warna, fillOpacity = 0.75, weight = 1,
      popup = paste0(spot$id_kasus, ": ", ifelse(spot$sakit, "kasus", "bukan kasus"))
    ) |>
    leaflet::addLegend("bottomright", colors = unname(WARNA_KLB[c("aksen", "netral")]),
                       labels = c("Kasus", "Bukan kasus"), opacity = 0.8)
}
