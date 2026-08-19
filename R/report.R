#' Metadata laporan kosong
#' @return Daftar metadata laporan.
#' @export
metadata_laporan <- function() {
  list(
    principal_investigator = "", co_principal_investigator = "", institusi = "",
    tahun = format(Sys.Date(), "%Y"), tipe = "akhir",
    sumber_laporan = "", kronologi_tambahan = "", definisi_kasus = "",
    metode_pengumpulan = "", patogen_diduga = "", sumber_diduga = "",
    hasil_laboratorium = "", studi_lingkungan = "", upaya_pengendalian = "",
    rekomendasi = "", ucapan_terima_kasih = ""
  )
}

tanggal_panjang <- function(x) {
  if (is.null(x) || all(is.na(x))) return("tanggal belum tersedia")
  bulan <- c("Januari", "Februari", "Maret", "April", "Mei", "Juni",
             "Juli", "Agustus", "September", "Oktober", "November", "Desember")
  d <- as.POSIXlt(x)
  sprintf("%d %s %d", d$mday, bulan[d$mon + 1], d$year + 1900)
}

jam_teks <- function(jam) {
  if (is.null(jam) || !is.finite(jam)) return("tidak dapat dihitung")
  j <- floor(jam)
  m <- round((jam - j) * 60)
  if (j == 0) return(sprintf("%d menit", m))
  if (m == 0) return(sprintf("%d jam", j))
  sprintf("%d jam %d menit", j, m)
}

p_teks <- function(p) {
  if (is.null(p) || !is.finite(p)) return("-")
  if (p < 1e-4) return("<0,0001")
  sub("\\.", ",", formatC(p, format = "f", digits = 4))
}

tipe_kurva_teks <- function(tipe) {
  switch(tipe, common_source = "common source", propagated_source = "propagated source",
         "belum dapat ditentukan")
}

lokasi_teks <- function(ket) {
  paste(Filter(nzchar, c(ket$desa, ket$kecamatan, ket$kabupaten)), collapse = ", ")
}

#' Menyusun draf laporan KLB
#'
#' Draf disusun secara deterministik dari hasil analisis. Seluruh angka pada
#' narasi berasal dari objek hasil analisis, tidak ada angka yang dibuat oleh
#' model bahasa. Struktur bab mengikuti Panduan Penulisan Laporan KLB.
#'
#' @param hasil Objek `klb_hasil` dari [analisis_klb()].
#' @param keterangan Daftar identitas investigasi (nama, penyakit, lokasi, tanggal).
#' @param meta Metadata laporan dari [metadata_laporan()].
#' @return Daftar bernama berisi naskah tiap bab laporan.
#' @export
#' @examples
#' contoh <- contoh_keracunan_pangan(n = 60)
#' hasil <- analisis_klb(contoh$data, contoh$pemetaan, contoh$konfigurasi)
#' draf <- susun_draf_laporan(hasil, contoh$keterangan, metadata_laporan())
#' substr(draf$intisari, 1, 60)
susun_draf_laporan <- function(hasil, keterangan, meta = metadata_laporan()) {
  r <- hasil$ringkasan
  lokasi <- lokasi_teks(keterangan)
  ukuran <- if (!is.null(hasil$paparan)) hasil$paparan$ukuran[1] else "RR"

  gejala_teks <- if (!is.null(hasil$gejala) && nrow(hasil$gejala) > 0) {
    paste(sprintf("%s (%s persen)", tolower(utils::head(hasil$gejala$kategori, 5)),
                  angka_id(utils::head(hasil$gejala$persen, 5))), collapse = ", ")
  } else "belum tersedia"

  bermakna <- if (!is.null(hasil$paparan)) {
    b <- hasil$paparan[is.finite(hasil$paparan$p_value) & hasil$paparan$p_value < 0.05 &
                         hasil$paparan$estimasi > 1, ]
    b[order(-b$estimasi), ]
  } else NULL

  kalimat_paparan <- if (!is.null(bermakna) && nrow(bermakna) > 0) {
    sprintf("Analisis bivariat menunjukkan hubungan bermakna pada %s.",
            paste(sprintf("%s (%s %s; 95 persen CI %s sampai %s; p %s)",
                          bermakna$label, bermakna$ukuran, angka_id(bermakna$estimasi, 2),
                          angka_id(bermakna$ci_bawah, 2), angka_id(bermakna$ci_atas, 2),
                          vapply(bermakna$p_value, p_teks, character(1))),
                  collapse = "; "))
  } else "Tidak ada variabel paparan yang menunjukkan hubungan bermakna secara statistik pada analisis bivariat."

  desain_teks <- if (identical(keterangan$jenis, "keracunan_pangan")) {
    "kohort retrospektif"
  } else "deskriptif dengan penelusuran kontak"

  intisari <- paste(
    sprintf("Latar belakang: %s di %s. Investigasi dilakukan untuk memastikan terjadinya KLB, mengidentifikasi agen penyebab dan sumber penularan, menggambarkan karakteristik kasus menurut orang, tempat dan waktu, serta merumuskan tindakan pengendalian.",
            if (nzchar(meta$sumber_laporan)) meta$sumber_laporan
            else sprintf("Laporan dugaan KLB diterima pada %s", tanggal_panjang(as.Date(keterangan$tanggal_lapor))),
            lokasi),
    "",
    sprintf("Metode: Penyelidikan menggunakan desain %s pada populasi berisiko di lokasi kejadian. %s Analisis deskriptif mencakup distribusi gejala serta attack rate menurut jenis kelamin, kelompok umur dan tempat. Analisis analitik menghitung %s dengan selang kepercayaan 95 persen dan dilanjutkan model multivariabel.",
            desain_teks,
            if (nzchar(meta$metode_pengumpulan)) meta$metode_pengumpulan
            else "Data dikumpulkan melalui active case finding dengan kuesioner terstruktur pada KoboToolbox, dilengkapi observasi lingkungan.",
            ukuran),
    "",
    sprintf("Hasil: Dari %s orang yang berhasil diinvestigasi ditemukan %s kasus dengan attack rate %s persen%s. Gejala terbanyak adalah %s. %sKurva epidemik berbentuk %s. %s",
            r$total_diinvestigasi, r$total_kasus, angka_id(r$attack_rate),
            if (is.finite(r$cfr)) sprintf(" dan case fatality rate %s persen", angka_id(r$cfr)) else "",
            gejala_teks,
            if (is.finite(hasil$inkubasi$min_jam)) sprintf(
              "Masa inkubasi berkisar %s sampai %s dengan rata-rata %s. ",
              jam_teks(hasil$inkubasi$min_jam), jam_teks(hasil$inkubasi$maks_jam),
              jam_teks(hasil$inkubasi$rata_jam)) else "",
            tipe_kurva_teks(hasil$kurva$tipe), kalimat_paparan),
    "",
    sprintf("Kesimpulan: %s%s. %s",
            if (nzchar(meta$patogen_diduga)) sprintf("Agen yang diduga menjadi penyebab adalah %s", meta$patogen_diduga)
            else "Agen penyebab masih dalam penelusuran",
            if (nzchar(meta$sumber_diduga)) sprintf(" dengan sumber penularan %s", meta$sumber_diduga) else "",
            if (nzchar(meta$rekomendasi)) strsplit(meta$rekomendasi, "\n")[[1]][1]
            else "Rekomendasi pengendalian diuraikan pada bagian akhir laporan."),
    sep = "\n"
  )

  pendahuluan <- paste(
    sprintf("%s %s",
            if (nzchar(meta$sumber_laporan)) meta$sumber_laporan
            else sprintf("Pada %s petugas surveilans menerima laporan dugaan %s di %s.",
                         tanggal_panjang(as.Date(keterangan$tanggal_lapor)),
                         if (!is.null(keterangan$penyakit)) keterangan$penyakit else "kejadian luar biasa",
                         lokasi),
            meta$kronologi_tambahan),
    "",
    sprintf("Berdasarkan laporan awal tersebut, tim melakukan penyelidikan epidemiologi bersama petugas puskesmas dan dinas kesehatan setempat. Sampai analisis ini disusun, %s orang memenuhi definisi kasus dari %s orang yang berhasil diwawancarai%s. Gejala yang paling banyak dilaporkan adalah %s.",
            r$total_kasus, r$total_diinvestigasi,
            if (r$populasi_berisiko > r$total_diinvestigasi)
              sprintf(", dengan estimasi populasi berisiko %s orang", r$populasi_berisiko) else "",
            if (!is.null(hasil$gejala) && nrow(hasil$gejala) > 0)
              sprintf("%s (%s persen kasus)", tolower(hasil$gejala$kategori[1]),
                      angka_id(hasil$gejala$persen[1])) else "belum tersedia"),
    "",
    "Penyelidikan ini penting dilakukan untuk memastikan adanya KLB sesuai kriteria Peraturan Menteri Kesehatan Nomor 1501 Tahun 2010, mengidentifikasi sumber dan cara penularan, serta menetapkan tindakan penanggulangan agar tidak jatuh korban berikutnya.",
    sep = "\n"
  )

  tujuan_khusus <- if (identical(keterangan$jenis, "keracunan_pangan")) {
    c("Memastikan terjadinya KLB keracunan pangan berdasarkan kriteria yang berlaku.",
      "Mengetahui gambaran epidemiologi kasus menurut orang, tempat dan waktu.",
      "Mengidentifikasi jenis pangan yang berhubungan dengan kejadian sakit.",
      "Mengidentifikasi agen penyebab melalui gejala, masa inkubasi, dan pemeriksaan laboratorium.",
      "Mengetahui faktor risiko lingkungan pada alur pengelolaan pangan.",
      "Merumuskan tindakan pengendalian dan pencegahan.")
  } else {
    c("Memastikan terjadinya KLB berdasarkan kriteria yang berlaku.",
      "Mengetahui gambaran epidemiologi kasus menurut orang, tempat dan waktu.",
      "Mengidentifikasi sumber penularan dan kelompok berisiko tinggi.",
      "Menilai cakupan imunisasi dan faktor risiko penularan pada populasi berisiko.",
      "Merumuskan tindakan pengendalian dan pencegahan.")
  }
  tujuan <- paste(c(
    "Tujuan umum",
    "Melakukan pencegahan dan pengendalian terhadap meluasnya kejadian luar biasa di wilayah terjangkit.",
    "", "Tujuan khusus",
    sprintf("%d. %s", seq_along(tujuan_khusus), tujuan_khusus)
  ), collapse = "\n")

  metode <- paste(
    sprintf("Desain penyelidikan. Penyelidikan menggunakan desain %s. Batas wilayah penyelidikan adalah %s. Populasi berisiko adalah seluruh orang yang terpapar pada lokasi dan waktu kejadian, berjumlah %s orang, dan %s orang di antaranya berhasil diinvestigasi.",
            desain_teks, lokasi, r$populasi_berisiko, r$total_diinvestigasi),
    "",
    sprintf("Definisi kasus. %s",
            if (nzchar(meta$definisi_kasus)) meta$definisi_kasus
            else "Definisi kasus disusun mencakup unsur orang, tempat, waktu, tanda dan gejala klinis, serta hasil laboratorium bila tersedia. Definisi kasus disesuaikan selama investigasi berlangsung."),
    "",
    sprintf("Pengumpulan data. %s Data individu tetap tersimpan pada node kabupaten atau kota dan hanya ringkasan agregat yang dikirim ke tingkat provinsi.",
            if (nzchar(meta$metode_pengumpulan)) meta$metode_pengumpulan
            else "Penemuan kasus dilakukan secara aktif melalui wawancara tatap muka menggunakan kuesioner terstruktur yang dikelola pada platform KoboToolbox, dilengkapi observasi lingkungan dan penelusuran riwayat paparan."),
    "",
    sprintf("Analisis data. Analisis deskriptif menyajikan distribusi frekuensi gejala serta attack rate menurut jenis kelamin, kelompok umur, tempat, dan variabel terkait lainnya. Distribusi menurut waktu disajikan dalam kurva epidemik dengan interval %s jam%s. Analisis analitik menghitung %s beserta selang kepercayaan 95 persen dan nilai p menggunakan uji chi-square atau uji Fisher exact sesuai kecukupan frekuensi harapan. Variabel dengan nilai p di bawah %s dimasukkan ke dalam model multivariabel%s. Seluruh analisis dijalankan dengan R menggunakan paket klbfederated.",
            angka_id(hasil$kurva$interval_jam, 2),
            if (is.finite(hasil$inkubasi$rata_jam)) " yang ditetapkan sebesar seperempat masa inkubasi rata-rata" else "",
            ukuran, angka_id(hasil$konfigurasi$ambang_kandidat, 2),
            switch(hasil$multivariabel$model,
                   poisson_robust = " regresi Poisson dengan galat baku robust untuk memperoleh adjusted risk ratio",
                   logistik = " regresi logistik untuk memperoleh adjusted odds ratio", "")),
    sep = "\n"
  )

  hasil_teks <- susun_bagian_hasil(hasil, keterangan)

  pembahasan <- paste(
    sprintf("Hasil penyelidikan epidemiologi%s dan pengamatan lingkungan mengarahkan dugaan penyebab KLB ini pada %s%s. %s",
            if (nzchar(meta$hasil_laboratorium)) ", laboratorium," else "",
            if (nzchar(meta$patogen_diduga)) meta$patogen_diduga else "agen yang masih perlu dipastikan",
            if (nzchar(meta$sumber_diduga)) sprintf(" dengan sumber penularan %s", meta$sumber_diduga) else "",
            if (is.finite(hasil$inkubasi$min_jam)) sprintf(
              "Masa inkubasi yang teramati (%s sampai %s) menjadi dasar penyingkiran sebagian agen pada tabel diagnosis banding.",
              jam_teks(hasil$inkubasi$min_jam), jam_teks(hasil$inkubasi$maks_jam)) else ""),
    "",
    sprintf("Bentuk kurva epidemik yang %s konsisten dengan %s.",
            tipe_kurva_teks(hasil$kurva$tipe),
            switch(hasil$kurva$tipe,
                   common_source = "paparan pada satu sumber yang sama dalam rentang waktu pendek",
                   propagated_source = "penularan berkelanjutan dari orang ke orang",
                   "pola yang belum dapat ditetapkan sehingga memerlukan data waktu paparan yang lebih lengkap")),
    "",
    "[Bandingkan temuan ini dengan hasil penyelidikan KLB serupa pada literatur: persamaan dan perbedaan jenis pangan atau agen, cara pengolahan, kelompok umur terdampak, serta mekanisme biologis yang menjelaskan gejala dan masa inkubasi. Sertakan sitasi.]",
    "",
    sprintf("Keterbatasan. %s",
            if (length(hasil$peringatan)) paste(hasil$peringatan, collapse = " ")
            else "Keterbatasan investigasi perlu diuraikan, mencakup kelengkapan data, recall bias pada wawancara riwayat paparan, dan ketersediaan sampel laboratorium."),
    sep = "\n"
  )

  poin <- sprintf("Telah terjadi KLB %s di %s dengan %s kasus dan attack rate %s persen.",
                  if (!is.null(keterangan$penyakit)) keterangan$penyakit else gsub("_", " ", keterangan$jenis),
                  lokasi, r$total_kasus, angka_id(r$attack_rate))
  if (nzchar(meta$patogen_diduga)) poin <- c(poin, sprintf("Agen penyebab yang diduga adalah %s.", meta$patogen_diduga))
  if (nzchar(meta$sumber_diduga)) poin <- c(poin, sprintf("Sumber penularan yang diduga adalah %s.", meta$sumber_diduga))
  if (!is.null(hasil$ar_umur) && !is.null(hasil$ar_jenis_kelamin)) {
    umur_top <- hasil$ar_umur[which.max(hasil$ar_umur$ar), ]
    jk_top <- hasil$ar_jenis_kelamin[which.max(hasil$ar_jenis_kelamin$ar), ]
    poin <- c(poin, sprintf("Kelompok dengan attack rate tertinggi adalah %s (%s persen) dan kelompok umur %s (%s persen).",
                            tolower(jk_top$kategori), angka_id(jk_top$ar), umur_top$kategori, angka_id(umur_top$ar)))
  }
  kesimpulan <- paste(sprintf("%d. %s", seq_along(poin), poin), collapse = "\n")

  rekomendasi <- if (nzchar(trimws(meta$rekomendasi))) trimws(meta$rekomendasi) else paste(
    "1. [Tindakan pengendalian segera pada sumber yang dicurigai.]",
    "2. [Penguatan surveilans dan penemuan kasus baru selama dua kali masa inkubasi terpanjang.]",
    "3. [Edukasi kepada masyarakat dan pengelola tempat kejadian.]",
    "4. [Penguatan koordinasi lintas sektor dan pelaporan berjenjang.]", sep = "\n")

  list(
    sampul = paste(
      "LAPORAN PENYELIDIKAN KEJADIAN LUAR BIASA", toupper(keterangan$nama),
      toupper(lokasi), sprintf("TAHUN %s", meta$tahun), "",
      sprintf("Principal Investigator: %s", if (nzchar(meta$principal_investigator)) meta$principal_investigator else "[nama]"),
      sprintf("Co-Principal Investigator: %s", if (nzchar(meta$co_principal_investigator)) meta$co_principal_investigator else "[nama]"),
      if (nzchar(meta$institusi)) meta$institusi else "[minat, fakultas, universitas]",
      sprintf("Jenis laporan: %s", if (identical(meta$tipe, "sementara")) "Laporan sementara (KLB masih berlangsung)" else "Laporan akhir (KLB telah berakhir)"),
      sep = "\n"),
    pengesahan = paste(
      "LEMBAR PENGESAHAN", "",
      sprintf("Laporan penyelidikan kejadian luar biasa dengan judul \"%s\" telah diperiksa dan disetujui.", keterangan$nama),
      "", "Pembimbing Lapangan                    Supervisor", "", "",
      "(............................)          (............................)",
      "Tanggal: ..................", sep = "\n"),
    intisari = intisari,
    abstract = "[Terjemahan intisari ke bahasa Inggris. Gunakan tombol WebLLM untuk menyusun draf terjemahan.]",
    pendahuluan = pendahuluan,
    tujuan = tujuan,
    metode = metode,
    hasil = hasil_teks,
    lingkungan = if (nzchar(meta$studi_lingkungan)) meta$studi_lingkungan
      else "[Uraikan hasil observasi lingkungan dan alur pengelolaan pangan atau kondisi lingkungan tempat tinggal.]",
    laboratorium = if (nzchar(meta$hasil_laboratorium)) meta$hasil_laboratorium
      else "[Uraikan jenis spesimen, laboratorium pemeriksa, tanggal dan hasil pemeriksaan.]",
    pembahasan = pembahasan,
    upaya = if (nzchar(meta$upaya_pengendalian)) meta$upaya_pengendalian
      else "[Uraikan upaya promotif, preventif, kuratif dan lintas sektor yang telah dilakukan.]",
    kesimpulan = kesimpulan,
    rekomendasi = rekomendasi,
    referensi = "[Daftar pustaka gaya Vancouver.]"
  )
}

#' @keywords internal
#' @noRd
susun_bagian_hasil <- function(hasil, keterangan) {
  r <- hasil$ringkasan
  bagian <- character(0)

  bagian <- c(bagian, sprintf(
    "Hasil penyelidikan epidemiologi menemukan %s kasus dari %s orang yang diinvestigasi, dengan attack rate keseluruhan %s persen%s. Kasus pertama mulai sakit pada %s dan kasus terakhir pada %s.",
    r$total_kasus, r$total_diinvestigasi, angka_id(r$attack_rate),
    if (is.finite(r$cfr)) sprintf(" dan case fatality rate %s persen (%s kematian)", angka_id(r$cfr), r$total_meninggal) else "",
    tanggal_panjang(r$tanggal_kasus_pertama), tanggal_panjang(r$tanggal_kasus_terakhir)))

  if (!is.null(hasil$gejala) && nrow(hasil$gejala) > 0) {
    tiga <- paste(sprintf("%s (%s persen)", tolower(utils::head(hasil$gejala$kategori, 3)),
                          angka_id(utils::head(hasil$gejala$persen, 3))), collapse = ", ")
    bagian <- c(bagian, sprintf(
      "\nDistribusi berdasarkan gejala. Gejala yang paling banyak dilaporkan kasus adalah %s. Rincian seluruh gejala disajikan pada Tabel 1.", tiga))
  }

  tertinggi <- function(tab) {
    if (is.null(tab) || nrow(tab) == 0) return("")
    b <- tab[which.max(tab$ar), ]
    sprintf("%s (AR %s persen)", b$kategori, angka_id(b$ar))
  }

  if (!is.null(hasil$ar_jenis_kelamin)) {
    bagian <- c(bagian, sprintf(
      "\nDistribusi berdasarkan jenis kelamin. Attack rate tertinggi ditemukan pada kelompok %s. Rincian disajikan pada Tabel 2.",
      tertinggi(hasil$ar_jenis_kelamin)))
  }
  if (!is.null(hasil$ar_umur)) {
    terbanyak <- hasil$ar_umur[which.max(hasil$ar_umur$kasus), ]
    bagian <- c(bagian, sprintf(
      "\nDistribusi berdasarkan kelompok umur. Jumlah kasus terbanyak berada pada kelompok umur %s sebanyak %s kasus, sedangkan attack rate tertinggi terdapat pada kelompok %s. Rincian disajikan pada Tabel 3.",
      terbanyak$kategori, terbanyak$kasus, tertinggi(hasil$ar_umur)))
  }
  if (!is.null(hasil$ar_tempat)) {
    bagian <- c(bagian, sprintf(
      "\nDistribusi berdasarkan tempat. Attack rate menurut wilayah tertinggi pada %s. Rincian disajikan pada Tabel 4.",
      tertinggi(hasil$ar_tempat)))
  }
  for (nama in names(hasil$ar_lain)) {
    bagian <- c(bagian, sprintf("\nDistribusi berdasarkan %s. Attack rate tertinggi ditemukan pada %s.",
                                tolower(nama), tertinggi(hasil$ar_lain[[nama]])))
  }

  if (nrow(hasil$kurva$bins) > 0) {
    puncak <- hasil$kurva$bins[which.max(hasil$kurva$bins$kasus), ]
    bagian <- c(bagian, sprintf(
      "\nDistribusi berdasarkan waktu. Kurva epidemik disusun dengan interval %s jam. Puncak kasus terjadi pada interval yang dimulai %s dengan %s kasus. %sBentuk kurva sesuai %s. %s",
      angka_id(hasil$kurva$interval_jam, 2),
      format(puncak$mulai, "%d/%m/%Y %H:%M"), puncak$kasus,
      if (is.finite(hasil$inkubasi$min_jam)) sprintf(
        "Masa inkubasi terpendek %s, terpanjang %s, dan rata-rata %s. ",
        jam_teks(hasil$inkubasi$min_jam), jam_teks(hasil$inkubasi$maks_jam),
        jam_teks(hasil$inkubasi$rata_jam)) else "",
      tipe_kurva_teks(hasil$kurva$tipe), hasil$kurva$alasan_tipe))
    if (!is.null(hasil$inkubasi$periode_paparan)) {
      bagian <- c(bagian, sprintf(
        "Dengan menghitung mundur dari kasus pertama dan kasus terakhir, periode paparan diperkirakan berlangsung antara %s dan %s.",
        format(hasil$inkubasi$periode_paparan$mulai, "%d/%m/%Y %H:%M"),
        format(hasil$inkubasi$periode_paparan$selesai, "%d/%m/%Y %H:%M")))
    }
  }

  if (!is.null(hasil$paparan) && nrow(hasil$paparan) > 0) {
    arr_top <- hasil$paparan[which.max(replace(hasil$paparan$arr, !is.finite(hasil$paparan$arr), -Inf)), ]
    bermakna <- hasil$paparan[is.finite(hasil$paparan$p_value) & hasil$paparan$p_value < 0.05 &
                                hasil$paparan$estimasi > 1, ]
    bermakna <- bermakna[order(-bermakna$estimasi), ]
    bagian <- c(bagian, sprintf(
      "\nAnalisis paparan. Attack rate ratio tertinggi ditemukan pada %s sebesar %s, dengan attack rate %s persen pada kelompok terpapar dibandingkan %s persen pada kelompok tidak terpapar. %s Rincian disajikan pada Tabel 5 dan Tabel 6.",
      arr_top$label, angka_id(arr_top$arr, 2), angka_id(arr_top$ar_terpapar), angka_id(arr_top$ar_tak_terpapar),
      if (nrow(bermakna) > 0) sprintf("Sebanyak %d variabel menunjukkan hubungan bermakna dengan kejadian sakit, yaitu %s.",
                                      nrow(bermakna),
                                      paste(sprintf("%s (%s %s; 95 persen CI %s sampai %s; p %s)",
                                                    bermakna$label, bermakna$ukuran, angka_id(bermakna$estimasi, 2),
                                                    angka_id(bermakna$ci_bawah, 2), angka_id(bermakna$ci_atas, 2),
                                                    vapply(bermakna$p_value, p_teks, character(1))), collapse = "; "))
      else "Tidak ditemukan variabel dengan hubungan bermakna secara statistik."))
  }

  if (!is.null(hasil$multivariabel$tabel) && nrow(hasil$multivariabel$tabel) > 0) {
    utama <- hasil$multivariabel$tabel[which.max(hasil$multivariabel$tabel$estimasi), ]
    ukuran <- if (identical(hasil$multivariabel$model, "logistik")) "adjusted odds ratio" else "adjusted risk ratio"
    bagian <- c(bagian, sprintf(
      "\nAnalisis multivariabel. Setelah mengendalikan variabel lain dalam model, %s menunjukkan %s %s (95 persen CI %s sampai %s; p %s). %s",
      utama$label, ukuran, angka_id(utama$estimasi, 2), angka_id(utama$ci_bawah, 2),
      angka_id(utama$ci_atas, 2), p_teks(utama$p_value), hasil$multivariabel$catatan))
  }

  if (!is.null(hasil$diagnosis_banding)) {
    belum <- hasil$diagnosis_banding[hasil$diagnosis_banding$status == "belum disingkirkan", ]
    bagian <- c(bagian, sprintf(
      "\nDiagnosis banding. Berdasarkan distribusi gejala dan masa inkubasi KLB, %s. Dasar penyingkiran mengikuti aturan perbandingan masa inkubasi KLB terhadap masa inkubasi agen pada Pedoman KLB 2020. Rincian disajikan pada Tabel 7.",
      if (nrow(belum) > 0) sprintf("agen yang belum dapat disingkirkan adalah %s", paste(belum$agen, collapse = ", "))
      else "seluruh agen pada tabel rujukan dapat disingkirkan berdasarkan kriteria masa inkubasi"))
  }

  terpenuhi <- hasil$kriteria_klb[!is.na(hasil$kriteria_klb$terpenuhi) & hasil$kriteria_klb$terpenuhi, ]
  bagian <- c(bagian, sprintf("\nPenetapan KLB. %s",
    if (nrow(terpenuhi) > 0) sprintf("Kejadian ini memenuhi kriteria KLB huruf %s menurut Permenkes 1501 Tahun 2010. %s",
                                     paste(terpenuhi$kode, collapse = ", "), paste(terpenuhi$keterangan, collapse = " "))
    else "Kriteria KLB belum dapat dinilai secara lengkap karena data pembanding periode sebelumnya belum diisi."))

  paste(bagian, collapse = "\n")
}
