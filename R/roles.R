#' Katalog peran variabel yang dapat dipetakan
#'
#' Setiap peran dipetakan ke satu atau lebih kolom pada data KoboToolbox.
#' Kolom `wajib_untuk` menentukan jenis KLB yang tidak dapat dianalisis
#' sebelum peran tersebut dipetakan.
#'
#' Acuan: Pedoman Penyelidikan dan Penanggulangan KLB Penyakit Menular dan
#' Keracunan Pangan (Kemenkes, Edisi Revisi III 2020) Bab II langkah IV,
#' Checklist Laporan KLB Keracunan Makanan, dan Panduan Penulisan Laporan KLB.
#'
#' @return Sebuah `data.frame` berisi katalog peran variabel.
#' @export
#' @examples
#' head(peran_variabel())
peran_variabel <- function() {
  b <- function(key, label, deskripsi, tipe, ganda, wajib, butuh, acuan = NA_character_) {
    data.frame(
      key = key, label = label, deskripsi = deskripsi, tipe = tipe,
      ganda = ganda, wajib_untuk = wajib, dibutuhkan_oleh = butuh, acuan = acuan,
      stringsAsFactors = FALSE
    )
  }

  rbind(
    b("id_kasus", "ID responden",
      "Pengenal unik tiap responden pada line listing.",
      "teks", FALSE, "", "Line listing, penelusuran kasus"),
    b("status_sakit", "Status kasus (sakit atau tidak sakit)",
      "Variabel yang menentukan seseorang memenuhi definisi kasus. Tentukan pula nilai mana yang berarti sakit.",
      "kategorik", FALSE, "keracunan_pangan,penyakit_menular,pd3i",
      "Attack rate, seluruh analisis analitik, kurva epidemik",
      "Pedoman KLB 2020 Bab II langkah I dan III"),
    b("klasifikasi_kasus", "Klasifikasi kasus (suspek, probable, konfirmasi, carrier)",
      "Kategori kasus sesuai definisi kasus operasional.",
      "kategorik", FALSE, "penyakit_menular,pd3i",
      "Distribusi kasus menurut definisi kasus",
      "Laporan KLB Pertusis 2024 Tabel 5"),
    b("tanggal_onset", "Tanggal atau waktu mulai sakit",
      "Tanggal atau tanggal dan jam gejala pertama muncul.",
      "waktu", FALSE, "keracunan_pangan,penyakit_menular,pd3i",
      "Kurva epidemik, masa inkubasi, periode KLB",
      "Pedoman KLB 2020 Bab II variabel waktu"),
    b("jam_onset", "Jam mulai sakit bila terpisah dari tanggal",
      "Diisi bila jam onset dikumpulkan pada pertanyaan terpisah.",
      "teks", FALSE, "", "Kurva epidemik satuan jam"),
    b("tanggal_paparan", "Tanggal atau waktu paparan (waktu makan)",
      "Waktu konsumsi pangan tersangka atau waktu kontak.",
      "waktu", FALSE, "keracunan_pangan",
      "Masa inkubasi, periode paparan, penetapan etiologi",
      "Pedoman KLB 2020 Bab Keracunan Pangan bagian 2a"),
    b("jam_paparan", "Jam paparan bila terpisah dari tanggal",
      "Diisi bila jam makan atau kontak dikumpulkan terpisah.",
      "teks", FALSE, "", "Masa inkubasi satuan jam"),
    b("umur", "Umur dalam tahun",
      "Bila hanya tersedia tanggal lahir, petakan pada peran tanggal lahir.",
      "angka", FALSE, "keracunan_pangan,penyakit_menular,pd3i",
      "Attack rate menurut kelompok umur",
      "Pedoman KLB 2020 Bab II variabel orang"),
    b("tanggal_lahir", "Tanggal lahir",
      "Alternatif bila umur tidak ditanyakan. Umur dihitung terhadap tanggal onset.",
      "tanggal", FALSE, "", "Attack rate menurut kelompok umur"),
    b("jenis_kelamin", "Jenis kelamin",
      "Tentukan pemetaan nilai ke Laki-laki dan Perempuan.",
      "kategorik", FALSE, "keracunan_pangan,penyakit_menular,pd3i",
      "Attack rate menurut jenis kelamin",
      "Pedoman KLB 2020 Bab II variabel orang"),
    b("wilayah", "Wilayah tempat tinggal (dusun, desa, RT)",
      "Satuan tempat terkecil yang tersedia untuk analisis distribusi tempat.",
      "kategorik", FALSE, "keracunan_pangan,penyakit_menular,pd3i",
      "Attack rate menurut tempat, spot map",
      "Pedoman KLB 2020 Bab II variabel tempat"),
    b("lokasi_paparan", "Lokasi atau cara paparan",
      "Misalnya makan di tempat acara dibanding dibungkus, kelas sekolah, atau unit kerja.",
      "kategorik", FALSE, "keracunan_pangan",
      "Attack rate menurut tempat makan atau asal pangan",
      "Checklist Laporan KLB Keracunan Makanan butir 4"),
    b("gejala", "Tanda dan gejala",
      "Pilih semua kolom gejala. Setiap kolom menjadi satu baris tabel distribusi gejala.",
      "biner", TRUE, "keracunan_pangan,penyakit_menular,pd3i",
      "Tabel distribusi gejala, diagnosis banding",
      "Pedoman KLB 2020 Tabel 1"),
    b("makanan", "Jenis pangan yang dikonsumsi",
      "Pilih semua kolom pangan dan minuman. Setiap kolom menjadi satu baris analisis paparan.",
      "biner", TRUE, "keracunan_pangan",
      "Tabel makan dan tidak makan, ARR, RR atau OR, analisis multivariabel",
      "Pedoman KLB 2020 Tabel 16, Checklist butir 6"),
    b("faktor_risiko", "Faktor risiko non pangan",
      "Kontak serumah, sumber air, kunjungan lokasi, kepadatan hunian, dan sejenisnya.",
      "biner", TRUE, "penyakit_menular",
      "Analisis analitik RR atau OR",
      "Pedoman KLB 2020 Tabel 4"),
    b("status_imunisasi", "Status imunisasi",
      "Misalnya lengkap, tidak lengkap, tidak imunisasi, tidak tahu.",
      "kategorik", FALSE, "pd3i",
      "Attack rate menurut status imunisasi",
      "Laporan KLB Pertusis 2024 Tabel 6"),
    b("meninggal", "Status meninggal",
      "Diperlukan untuk menghitung case fatality rate.",
      "biner", FALSE, "", "CFR, kriteria KLB huruf f",
      "Permenkes 1501 Tahun 2010 kriteria huruf f"),
    b("hasil_lab", "Hasil pemeriksaan laboratorium",
      "Misalnya positif atau negatif PCR, kultur, kadar toksin.",
      "kategorik", FALSE, "", "Tabel hasil laboratorium, klasifikasi kasus konfirmasi",
      "Pedoman KLB 2020 Bab II hasil laboratorium"),
    b("jenis_spesimen", "Jenis spesimen",
      "Misalnya usap nasofaring, tinja, sisa makanan, muntahan.",
      "kategorik", FALSE, "", "Tabel hasil laboratorium"),
    b("tempat_berobat", "Tempat berobat",
      "Misalnya puskesmas, rumah sakit, atau tidak berobat.",
      "kategorik", FALSE, "", "Distribusi pencarian pengobatan"),
    b("pekerjaan_status", "Status atau pekerjaan",
      "Misalnya santri, guru, juru masak, karyawan.",
      "kategorik", FALSE, "", "Attack rate menurut status",
      "Laporan KLB Histamin 2024 Tabel 2"),
    b("latitude", "Lintang", "Untuk spot map sebaran kasus.",
      "geo", FALSE, "", "Spot map", "Pedoman KLB 2020 Gambar 1"),
    b("longitude", "Bujur", "Untuk spot map sebaran kasus.",
      "geo", FALSE, "", "Spot map"),
    b("geopoint", "Titik koordinat gabungan Kobo",
      "Kolom geopoint berformat lintang bujur ketinggian akurasi.",
      "geo", FALSE, "", "Spot map"),
    b("kontak_kasus", "Hubungan epidemiologi atau kontak dengan kasus",
      "Untuk KLB propagated source dan penyusunan pohon penularan.",
      "kategorik", FALSE, "", "Pohon penularan, klasifikasi probable",
      "Laporan KLB Pertusis 2024")
  )
}

#' Peran variabel yang wajib untuk satu jenis KLB
#'
#' @param jenis Jenis KLB: `"keracunan_pangan"`, `"penyakit_menular"`, atau `"pd3i"`.
#' @return `data.frame` berisi peran wajib.
#' @export
peran_wajib <- function(jenis) {
  katalog <- peran_variabel()
  wajib <- vapply(
    strsplit(katalog$wajib_untuk, ","),
    function(x) jenis %in% x, logical(1)
  )
  katalog[wajib, , drop = FALSE]
}

#' Analisis yang bergantung pada peran variabel tertentu
#' @keywords internal
#' @noRd
kebutuhan_analisis <- function() {
  list(
    list(analisis = "Ringkasan populasi berisiko dan attack rate", butuh = "status_sakit"),
    list(analisis = "Tabel distribusi gejala", butuh = c("status_sakit", "gejala")),
    list(analisis = "Attack rate menurut jenis kelamin", butuh = c("status_sakit", "jenis_kelamin")),
    list(analisis = "Attack rate menurut kelompok umur", butuh = "status_sakit",
         opsional = c("umur", "tanggal_lahir")),
    list(analisis = "Attack rate menurut tempat", butuh = c("status_sakit", "wilayah")),
    list(analisis = "Attack rate menurut status imunisasi", butuh = c("status_sakit", "status_imunisasi")),
    list(analisis = "Kurva epidemik", butuh = "tanggal_onset"),
    list(analisis = "Masa inkubasi dan periode paparan", butuh = c("tanggal_onset", "tanggal_paparan")),
    list(analisis = "Tabel makan dan tidak makan serta ARR", butuh = c("status_sakit", "makanan")),
    list(analisis = "Analisis analitik RR atau OR pangan", butuh = c("status_sakit", "makanan")),
    list(analisis = "Analisis analitik RR atau OR faktor risiko", butuh = c("status_sakit", "faktor_risiko")),
    list(analisis = "Analisis multivariabel", butuh = "status_sakit",
         opsional = c("makanan", "faktor_risiko")),
    list(analisis = "Case fatality rate", butuh = c("status_sakit", "meninggal")),
    list(analisis = "Diagnosis banding etiologi", butuh = "gejala"),
    list(analisis = "Spot map", butuh = character(0), opsional = c("geopoint", "latitude")),
    list(analisis = "Tabel hasil laboratorium", butuh = "hasil_lab")
  )
}

#' Memeriksa kelengkapan pemetaan variabel
#'
#' @param jenis Jenis KLB.
#' @param pemetaan Daftar bernama. Setiap elemen berisi `kolom` (karakter),
#'   dan opsional `nilai_positif`, `peta_nilai`, serta `label`.
#' @return Daftar berisi `lengkap`, `wajib_kurang`, `peringatan`,
#'   `analisis_tersedia`, dan `analisis_terkunci`.
#' @export
#' @examples
#' pemetaan <- list(status_sakit = list(kolom = "sakit", nilai_positif = "ya"))
#' periksa_pemetaan("keracunan_pangan", pemetaan)$lengkap
periksa_pemetaan <- function(jenis, pemetaan) {
  ada <- function(key) {
    m <- pemetaan[[key]]
    !is.null(m) && length(m$kolom) > 0 && any(nzchar(m$kolom))
  }

  wajib <- peran_wajib(jenis)
  kurang <- wajib[!vapply(wajib$key, ada, logical(1)), , drop = FALSE]

  peringatan <- character(0)
  if (ada("status_sakit")) {
    nilai <- pemetaan[["status_sakit"]]$nilai_positif
    if (is.null(nilai) || length(nilai) == 0) {
      peringatan <- c(peringatan, paste(
        "Nilai yang berarti sakit pada variabel status kasus belum ditentukan.",
        "Analisis tidak dapat membedakan kasus dan bukan kasus."
      ))
    }
  }
  if (!ada("umur") && !ada("tanggal_lahir")) {
    peringatan <- c(peringatan, paste(
      "Umur maupun tanggal lahir belum dipetakan sehingga attack rate menurut",
      "kelompok umur tidak dapat dihitung."
    ))
  }
  if (!ada("meninggal")) {
    peringatan <- c(peringatan,
      "Status meninggal belum dipetakan sehingga case fatality rate tidak dihitung.")
  }
  if (identical(jenis, "keracunan_pangan") && !ada("tanggal_paparan")) {
    peringatan <- c(peringatan, paste(
      "Waktu paparan belum dipetakan sehingga masa inkubasi dan periode paparan",
      "tidak dapat dihitung."
    ))
  }

  katalog <- peran_variabel()
  label_peran <- stats::setNames(katalog$label, katalog$key)

  tersedia <- character(0)
  terkunci <- list()
  for (dep in kebutuhan_analisis()) {
    hilang <- dep$butuh[!vapply(dep$butuh, ada, logical(1))]
    ops_ok <- is.null(dep$opsional) || any(vapply(dep$opsional, ada, logical(1)))
    if (length(hilang) == 0 && ops_ok) {
      tersedia <- c(tersedia, dep$analisis)
    } else {
      teks <- c(
        unname(label_peran[hilang]),
        if (!ops_ok) paste(unname(label_peran[dep$opsional]), collapse = " atau ")
      )
      terkunci[[length(terkunci) + 1]] <- list(
        analisis = dep$analisis,
        alasan = paste0("Peran belum dipetakan: ", paste(teks, collapse = ", "))
      )
    }
  }

  list(
    lengkap = nrow(kurang) == 0,
    wajib_kurang = kurang,
    peringatan = peringatan,
    analisis_tersedia = tersedia,
    analisis_terkunci = terkunci
  )
}
