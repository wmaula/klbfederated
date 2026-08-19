#' Tujuh kriteria penetapan KLB
#'
#' Kriteria menurut Peraturan Menteri Kesehatan Nomor 1501 Tahun 2010,
#' sebagaimana dikutip pada Pedoman KLB 2020 Bab I bagian D butir 2.
#'
#' @return `data.frame` dengan kolom `kode`, `uraian`, `otomatis`, dan `pembanding`.
#' @export
kriteria_klb <- function() {
  data.frame(
    kode = letters[1:7],
    uraian = c(
      "Timbulnya suatu penyakit menular tertentu yang sebelumnya tidak ada atau tidak dikenal pada suatu daerah.",
      "Peningkatan kejadian kesakitan terus menerus selama 3 kurun waktu dalam jam, hari atau minggu berturut-turut menurut jenis penyakitnya.",
      "Peningkatan kejadian kesakitan dua kali atau lebih dibandingkan periode sebelumnya dalam kurun waktu jam, hari atau minggu menurut jenis penyakitnya.",
      "Jumlah penderita baru dalam periode waktu 1 bulan menunjukkan kenaikan dua kali atau lebih dibandingkan angka rata-rata per bulan tahun sebelumnya.",
      "Rata-rata jumlah kejadian kesakitan per bulan selama 1 tahun menunjukkan kenaikan dua kali atau lebih dibandingkan rata-rata per bulan tahun sebelumnya.",
      "Angka kematian kasus (case fatality rate) dalam satu kurun waktu menunjukkan kenaikan 50 persen atau lebih dibandingkan CFR periode sebelumnya dalam kurun waktu yang sama.",
      "Angka proporsi penyakit (proportional rate) penderita baru pada satu periode menunjukkan kenaikan dua kali atau lebih dibanding periode sebelumnya dalam kurun waktu yang sama."
    ),
    otomatis = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    pembanding = c(
      NA, "Kurva epidemik periode berjalan", "Jumlah kasus periode sebelumnya",
      "Rata-rata kasus per bulan tahun sebelumnya",
      "Rata-rata kasus per bulan dua tahun terakhir",
      "CFR periode sebelumnya", "Proporsi penyakit periode sebelumnya"
    ),
    stringsAsFactors = FALSE
  )
}

#' Data pembanding kosong untuk penilaian kriteria KLB
#' @return Daftar bernama berisi nilai `NA`.
#' @export
pembanding_kosong <- function() {
  list(
    kasus_periode_sebelumnya = NA_real_,
    rata_bulan_tahun_lalu = NA_real_,
    rata_bulan_tahun_ini = NA_real_,
    cfr_periode_sebelumnya = NA_real_,
    proporsi_periode_sebelumnya = NA_real_,
    proporsi_periode_ini = NA_real_,
    penyakit_baru = NA,
    catatan = ""
  )
}

#' Tabel rujukan agen penyebab untuk diagnosis banding
#'
#' Nilai masa inkubasi berasal dari dua kelompok sumber. Baris dengan
#' `perlu_verifikasi = FALSE` bersumber dari dokumen acuan proyek, yaitu
#' Pedoman KLB 2020 (Tabel 2 dan 3 Bab Keracunan Pangan), Laporan KLB
#' Keracunan Makanan Kulon Progo 2018 (Tabel 9), dan Laporan KLB Histamin
#' Gunung Kidul 2024 (Tabel 5). Baris dengan `perlu_verifikasi = TRUE`
#' berasal dari rujukan umum compendium penyakit bawaan pangan dan wajib
#' diverifikasi penyelidik sebelum dipakai menarik kesimpulan.
#'
#' @return `data.frame` rujukan agen penyebab.
#' @export
rujukan_patogen <- function() {
  p <- function(id, nama, kategori, min_jam, maks_jam, gejala, sumber_pangan, sumber, verif) {
    data.frame(
      id = id, nama = nama, kategori = kategori,
      inkubasi_min_jam = min_jam, inkubasi_maks_jam = maks_jam,
      gejala = paste(gejala, collapse = "|"),
      sumber_pangan = paste(sumber_pangan, collapse = "|"),
      sumber = sumber, perlu_verifikasi = verif,
      stringsAsFactors = FALSE
    )
  }

  rbind(
    p("histamin", "Keracunan histamin (skombroid)", "biogenik", 0.08, 2,
      c("sakit kepala", "pusing", "mual", "muntah", "tenggorokan terbakar",
        "muka bengkak", "ruam", "gatal", "sakit perut", "diare"),
      c("tongkol", "tuna", "mackerel", "keju"),
      "Laporan KLB Histamin Gunung Kidul 2024 Tabel 5; WHO dan FAO 2013 dikutip pada laporan yang sama",
      FALSE),
    p("msg", "Keracunan monosodium glutamat", "kimia", 0.08, 1,
      c("rasa terbakar", "gatal", "ruam", "pusing", "sakit kepala", "mual"),
      "makanan berbumbu mengandung MSG",
      "Laporan KLB Histamin Gunung Kidul 2024 Tabel 5", FALSE),
    p("naoh", "Keracunan natrium hidroksida", "kimia", 0.02, 0.5,
      c("tenggorokan terbakar", "muntah", "sakit perut", "diare"),
      "minuman botol",
      "Laporan KLB Histamin Gunung Kidul 2024 Tabel 5", FALSE),
    p("proteus_mirabilis", "Proteus mirabilis", "bakteri", 0.67, 9,
      c("sakit perut", "diare", "mual", "muntah", "pusing", "demam"),
      c("daging", "telur", "sayuran"),
      "Laporan KLB Keracunan Makanan Kulon Progo 2018 Tabel 9 (biasanya 3 sampai 5 jam, rentang 40 menit sampai 9 jam)",
      FALSE),
    p("klebsiella", "Klebsiella pneumoniae", "bakteri", 5, 19,
      c("sakit perut", "diare", "mual", "demam", "menggigil", "keringat berlebihan"),
      c("daging", "sayuran"),
      "Laporan KLB Keracunan Makanan Kulon Progo 2018 Tabel 9 (rerata 10 jam, rentang 5 sampai 19 jam)",
      FALSE),
    p("pseudomonas", "Pseudomonas sp.", "bakteri", 24, 72,
      c("sakit perut", "mual", "keringat berlebihan"),
      c("susu", "daging", "ikan", "unggas", "telur"),
      "Laporan KLB Keracunan Makanan Kulon Progo 2018 Tabel 9", FALSE),
    p("c_perfringens", "Clostridium perfringens", "toksin bakteri", 8, 22,
      c("mual", "muntah", "sakit perut", "diare", "lemas"),
      c("daging matang yang disimpan lama", "gulai", "rendang"),
      "Pedoman KLB 2020 Tabel 2 dan 3 Bab Keracunan Pangan", FALSE),
    p("v_parahaemolyticus", "Vibrio parahaemolyticus", "bakteri", 2, 48,
      c("sakit perut", "mual", "muntah", "diare", "menggigil", "sakit kepala", "demam"),
      c("hasil laut", "ikan", "kerang"),
      "Pedoman KLB 2020 Tabel 2 dan 3 Bab Keracunan Pangan", FALSE),
    p("shigella", "Shigella dysenteriae", "bakteri", 12, 96,
      c("diare berdarah", "diare berlendir", "sakit perut", "demam", "sakit kepala"),
      "pangan atau air tercemar tinja",
      "Batas bawah 12 jam dari Pedoman KLB 2020 Tabel 3; batas atas dari rujukan umum compendium",
      TRUE),
    p("s_aureus", "Staphylococcus aureus (enterotoksin)", "toksin bakteri", 0.5, 8,
      c("mual", "muntah", "sakit perut", "diare"),
      c("pangan siap saji", "nasi", "daging olahan", "kue berkrim"),
      "Rujukan umum compendium penyakit bawaan pangan", TRUE),
    p("b_cereus_emetik", "Bacillus cereus tipe emetik", "toksin bakteri", 0.5, 6,
      c("mual", "muntah"),
      c("nasi", "pangan bertepung yang disimpan pada suhu ruang"),
      "Rujukan umum compendium penyakit bawaan pangan", TRUE),
    p("b_cereus_diare", "Bacillus cereus tipe diare", "toksin bakteri", 6, 15,
      c("diare", "sakit perut"), c("daging", "sup", "sayuran"),
      "Rujukan umum compendium penyakit bawaan pangan", TRUE),
    p("salmonella", "Salmonella spp. non tifoid", "bakteri", 6, 72,
      c("diare", "demam", "sakit perut", "mual", "muntah", "menggigil"),
      c("telur", "unggas", "daging", "susu mentah"),
      "Rujukan umum compendium penyakit bawaan pangan", TRUE),
    p("ecoli", "Escherichia coli patogen", "bakteri", 24, 240,
      c("diare", "diare berdarah", "sakit perut", "mual"),
      c("daging giling kurang matang", "sayuran mentah", "air tercemar"),
      "Rujukan umum compendium penyakit bawaan pangan", TRUE),
    p("norovirus", "Norovirus", "virus", 12, 48,
      c("muntah", "diare", "mual", "sakit perut", "demam"),
      c("pangan siap saji", "kerang", "air"),
      "Rujukan umum compendium penyakit bawaan pangan", TRUE),
    p("botulinum", "Clostridium botulinum", "toksin bakteri", 12, 72,
      c("pandangan kabur", "paralisis", "mulut kering", "muntah", "sulit menelan"),
      c("pangan kaleng", "pangan fermentasi rumahan"),
      "Rujukan umum compendium penyakit bawaan pangan", TRUE),
    p("organofosfat", "Pestisida organofosfat seperti malation", "kimia", 0.25, 12,
      c("mual", "muntah", "pusing", "keringat berlebihan", "sakit perut", "diare", "pandangan kabur"),
      c("sayuran atau buah dengan residu pestisida"),
      "Jenis racun disebut pada Pedoman KLB 2020 Bab Keracunan Pangan; rentang masa inkubasi dari rujukan umum",
      TRUE)
  )
}

#' Sinonim gejala untuk pencocokan label kolom
#' @keywords internal
#' @noRd
sinonim_gejala <- function() {
  list(
    "mual" = c("mual", "nausea", "eneg"),
    "muntah" = c("muntah", "vomit"),
    "diare" = c("diare", "mencret", "berak cair", "bab cair"),
    "diare berdarah" = c("diare berdarah", "bab berdarah", "disentri"),
    "diare berlendir" = c("diare berlendir", "bab berlendir"),
    "sakit perut" = c("sakit perut", "nyeri perut", "kram perut", "kejang perut", "mulas"),
    "demam" = c("demam", "panas badan", "fever"),
    "pusing" = c("pusing", "melayang"),
    "sakit kepala" = c("sakit kepala", "nyeri kepala", "headache"),
    "ruam" = c("ruam", "kemerahan", "rash", "bercak merah"),
    "gatal" = c("gatal", "itching"),
    "tenggorokan terbakar" = c("tenggorokan panas", "tenggorokan terbakar", "sakit tenggorokan"),
    "muka bengkak" = c("muka bengkak", "bengkak wajah"),
    "menggigil" = c("menggigil", "kedinginan"),
    "keringat berlebihan" = c("keringat berlebihan", "berkeringat"),
    "lemas" = c("lemas", "letih", "lelah", "lesu"),
    "paralisis" = c("paralisis", "lumpuh"),
    "pandangan kabur" = c("pandangan kabur", "penglihatan kabur"),
    "mulut kering" = "mulut kering",
    "sulit menelan" = c("sulit menelan", "disfagia"),
    "batuk" = c("batuk", "cough"),
    "pilek" = c("pilek", "flu ringan"),
    "rasa terbakar" = c("rasa terbakar", "panas di leher")
  )
}

#' Menormalkan label gejala menjadi kunci gejala baku
#' @param label Karakter label gejala.
#' @return Kunci gejala baku atau `NA`.
#' @export
normalkan_gejala <- function(label) {
  s <- tolower(trimws(label))
  for (kunci in names(sinonim_gejala())) {
    if (any(vapply(sinonim_gejala()[[kunci]], function(x) grepl(x, s, fixed = TRUE), logical(1)))) {
      return(kunci)
    }
  }
  NA_character_
}

#' Checklist kelengkapan laporan KLB
#'
#' Disusun dari Checklist Laporan KLB Keracunan Makanan dan Panduan Penulisan
#' Laporan KLB, diselaraskan dengan format laporan pada Pedoman KLB 2020
#' Bab II bagian VIII.
#'
#' @return `data.frame` butir checklist.
#' @export
checklist_laporan <- function() {
  c1 <- function(id, bagian, judul, uraian, berlaku, kunci, jenis) {
    data.frame(id = id, bagian = bagian, judul = judul, uraian = uraian,
               berlaku = berlaku, kunci = kunci, jenis = jenis, stringsAsFactors = FALSE)
  }
  semua <- "keracunan_pangan,penyakit_menular,pd3i"
  rbind(
    c1("cover", "Bagian awal", "Cover atau sampul",
       "Judul memuat jenis KLB, tempat dan tahun; nama principal investigator dan co-principal investigator; institusi dan tahun laporan.",
       "keduanya", "laporan.sampul", semua),
    c1("pengesahan", "Bagian awal", "Lembar pengesahan",
       "Dibuat saat KLB berakhir dan ditandatangani pembimbing lapangan serta supervisor.",
       "akhir", "laporan.pengesahan", semua),
    c1("intisari", "Bagian awal", "Intisari dan abstract",
       "Ringkasan 250 sampai 300 kata mencakup latar belakang, tujuan, metode, hasil dan kesimpulan, dalam dua bahasa.",
       "keduanya", "laporan.intisari", semua),
    c1("kronologi", "Pendahuluan", "Kronologi awal",
       "Tanggal dilaporkan, asal pelaporan, jumlah sakit, gejala kasus pertama, kegiatan dan pangan terkait, tempat, perkiraan populasi berisiko, dan tindak lanjut.",
       "keduanya", "laporan.pendahuluan", semua),
    c1("tujuan", "Pendahuluan", "Tujuan investigasi",
       "Tujuan umum dan tujuan khusus penyelidikan KLB.",
       "keduanya", "laporan.tujuan", semua),
    c1("metode_acf", "Metode", "Cara penemuan kasus",
       "Active case finding melalui wawancara dan observasi, serta pemeriksaan laboratorium bila tersedia.",
       "keduanya", "laporan.metode", semua),
    c1("definisi_kasus", "Metode", "Definisi kasus",
       "Mencakup orang, tempat, waktu, tanda dan gejala klinis, serta hasil laboratorium bila ada.",
       "keduanya", "laporan.definisi_kasus", semua),
    c1("sampel_lab", "Metode", "Sampel dan rencana pemeriksaan laboratorium",
       "Pada laporan sementara: sampel yang diperoleh, patogen atau bahan kimia yang akan diperiksa, dan laboratorium tujuan.",
       "sementara", "laporan.laboratorium", "keracunan_pangan"),
    c1("deskripsi_populasi", "Hasil", "Deskripsi populasi berisiko dan jumlah kasus",
       "Jumlah populasi berisiko, jumlah kasus, dan jumlah yang berhasil diinvestigasi.",
       "keduanya", "analisis.ringkasan", semua),
    c1("tabel_gejala", "Hasil", "Tabel distribusi gejala",
       "Kolom tanda dan gejala, jumlah kasus, dan persentase.",
       "keduanya", "analisis.gejala", semua),
    c1("tabel_jk", "Hasil", "Tabel distribusi menurut jenis kelamin",
       "Kolom jenis kelamin, jumlah kasus, populasi berisiko, dan attack rate.",
       "keduanya", "analisis.ar_jenis_kelamin", semua),
    c1("tabel_umur", "Hasil", "Tabel distribusi menurut kelompok umur",
       "Kolom kelompok umur, jumlah kasus, populasi berisiko, dan attack rate.",
       "keduanya", "analisis.ar_umur", semua),
    c1("tabel_tempat", "Hasil", "Tabel distribusi menurut tempat",
       "Distribusi menurut tempat makan, asal pangan, cara penyajian, atau wilayah tempat tinggal.",
       "keduanya", "analisis.ar_tempat", semua),
    c1("kurva", "Hasil", "Kurva epidemik",
       "Interval mengikuti seperempat atau seperdelapan masa inkubasi rata-rata, disertai masa inkubasi terpendek, terpanjang, rata-rata, dan tipe kurva.",
       "keduanya", "analisis.kurva", semua),
    c1("tabel_pangan", "Hasil", "Tabel distribusi pangan dan attack rate ratio",
       "Jumlah sakit dan tidak sakit pada kelompok mengonsumsi dan tidak mengonsumsi, attack rate keduanya, serta attack rate ratio.",
       "keduanya", "analisis.paparan", "keracunan_pangan"),
    c1("diagnosis_banding", "Hasil", "Tabel diagnosis banding",
       "Membandingkan agen berdasarkan jenis pangan, gejala, dan masa inkubasi, diakhiri kesimpulan agen penyebab.",
       "keduanya", "analisis.diagnosis_banding", semua),
    c1("hasil_lab", "Hasil", "Hasil uji laboratorium",
       "Parameter dan hasil uji bila pemeriksaan laboratorium dilakukan.",
       "keduanya", "laporan.laboratorium", semua),
    c1("analisis_lanjutan", "Hasil", "Analisis lanjutan",
       "Perhitungan RR atau OR kasar dan hasil analisis multivariabel disertai selang kepercayaan 95 persen dan nilai p.",
       "akhir", "analisis.multivariabel", semua),
    c1("lingkungan", "Hasil", "Studi lingkungan",
       "Kondisi lingkungan yang dicurigai, termasuk alur pengelolaan pangan dari penerimaan bahan sampai penyajian.",
       "keduanya", "laporan.lingkungan", semua),
    c1("pembahasan", "Pembahasan", "Diskusi atau pembahasan",
       "Perbandingan dengan investigasi serupa, penerimaan atau penolakan hipotesis, keterbatasan, dan lesson learned.",
       "akhir", "laporan.pembahasan", semua),
    c1("upaya", "Pembahasan", "Upaya pengendalian dan pencegahan",
       "Upaya promotif, preventif, kuratif, dan lintas sektor beserta penilaian keberhasilannya.",
       "keduanya", "laporan.upaya", semua),
    c1("kriteria", "Hasil", "Penetapan KLB menurut Permenkes 1501 Tahun 2010",
       "Menyatakan kriteria KLB yang terpenuhi beserta data pembandingnya.",
       "keduanya", "analisis.kriteria", semua),
    c1("kesimpulan", "Penutup", "Kesimpulan",
       "Menjawab tujuan umum dan tujuan khusus penyelidikan.",
       "keduanya", "laporan.kesimpulan", semua),
    c1("rekomendasi", "Penutup", "Rekomendasi dan saran",
       "Langkah operasional yang feasible dan operable, disusun sejak laporan sementara hingga KLB berakhir.",
       "keduanya", "laporan.rekomendasi", semua)
  )
}

#' Urutan bab laporan KLB
#'
#' Mengikuti Panduan Penulisan Laporan KLB dengan format IMRAD.
#' @return `data.frame` dengan kolom `kunci` dan `judul`.
#' @export
bagian_laporan <- function() {
  data.frame(
    kunci = c("sampul", "pengesahan", "intisari", "abstract", "pendahuluan", "tujuan",
              "metode", "hasil", "lingkungan", "laboratorium", "pembahasan", "upaya",
              "kesimpulan", "rekomendasi", "referensi"),
    judul = c("Halaman Sampul", "Lembar Pengesahan", "Intisari", "Abstract",
              "Pendahuluan", "Tujuan Penyelidikan", "Metodologi", "Hasil Penyelidikan",
              "Investigasi Lingkungan", "Investigasi Laboratorium", "Pembahasan",
              "Upaya Pengendalian dan Pencegahan", "Kesimpulan", "Rekomendasi dan Saran",
              "Daftar Pustaka"),
    stringsAsFactors = FALSE
  )
}
