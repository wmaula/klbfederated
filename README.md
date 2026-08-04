# klbfederated

Analisis federated kejadian luar biasa untuk dinas kesehatan, dalam bentuk package R dengan dua aplikasi Shiny.

Node kabupaten atau kota menarik data investigasi dari KoboToolbox, menjalankan analisis epidemiologi sesuai Pedoman Penyelidikan dan Penanggulangan KLB Kementerian Kesehatan edisi revisi III tahun 2020, menyusun draf laporan dengan bantuan model bahasa yang berjalan di peramban, lalu mengirim ringkasan agregat ke dashboard provinsi.

**Data individu tidak pernah meninggalkan node kabupaten atau kota.**

```
KoboToolbox  ──httr2──>  Node kabupaten/kota  ──agregat──>  Dashboard provinsi
                          (SQLite lokal,             (tanpa data individu)
                           analisis R,
                           laporan .docx)
```

## Pemasangan

```r
# install.packages("remotes")
remotes::install_github("awatsiq/klbfederated")
```

## Menjalankan

```r
library(klbfederated)

# Node kabupaten atau kota, sekaligus memuat dua investigasi contoh
jalankan_node(port = 4001, data_contoh = TRUE)

# Dashboard provinsi pada sesi R terpisah
jalankan_dashboard_provinsi(port = 4102, port_ingest = 4002)
```

Akun awal `admin` dengan kata sandi `admin123`, wajib diganti setelah login pertama. Data contoh juga membuat akun `analis` dan `viewer`. Ubah lewat variabel lingkungan `KLB_ADMIN_USER` dan `KLB_ADMIN_PASSWORD` sebelum basis data pertama kali dibuat.

Lokasi basis data mengikuti `KLB_DATA_DIR`, dan bila tidak diatur memakai `~/.klbfederated`.

## Alur pemakaian

1. **Pengaturan**: isi URL KoboToolbox dan token API, identitas node, URL webapp provinsi, dan kunci API yang diterbitkan dashboard provinsi.
2. **Investigasi**: buat investigasi baru, satu investigasi berpasangan dengan satu proyek KoboToolbox.
3. **Data KoboToolbox**: tautkan proyek lalu tarik data. Pertanyaan `select_multiple` dimekarkan otomatis menjadi kolom biner.
4. **Pemetaan variabel**: petakan peran variabel wajib. Analisis terkunci sampai variabel wajib lengkap.
5. **Analisis**: jalankan analisis dan periksa seluruh tabel serta gambar.
6. **Laporan**: susun draf template, perhalus dengan WebLLM, pantau checklist, ekspor ke Word atau Markdown.
7. **Kirim ke provinsi**: buat pratinjau payload agregat, periksa isinya, lalu konfirmasi pengiriman.

## Analisis di luar aplikasi

Seluruh fungsi analisis dapat dipanggil langsung dari konsol R atau dokumen Quarto.

```r
library(klbfederated)

contoh <- contoh_keracunan_pangan()
hasil <- analisis_klb(contoh$data, contoh$pemetaan, contoh$konfigurasi,
                      jenis = "keracunan_pangan")
hasil

hasil$paparan[, c("label", "ar_terpapar", "ar_tak_terpapar", "arr",
                  "ukuran", "estimasi", "ci_bawah", "ci_atas", "p_value")]

plot_kurva_epidemik(hasil)
plot_paparan(hasil$paparan)
peta_kasus(hasil)

naskah <- susun_draf_laporan(hasil, contoh$keterangan, metadata_laporan())
ekspor_docx(hasil, contoh$keterangan, naskah, "laporan-klb.docx")
```

Untuk data sendiri, susun pemetaan peran variabel seperti berikut.

```r
pemetaan <- list(
  status_sakit = list(kolom = "status", nilai_positif = "sakit"),
  tanggal_onset = list(kolom = "tgl_onset"),
  tanggal_paparan = list(kolom = "tgl_makan"),
  umur = list(kolom = "umur"),
  jenis_kelamin = list(kolom = "jk", peta_nilai = list(L = "Laki-laki", P = "Perempuan")),
  wilayah = list(kolom = "dusun"),
  gejala = list(kolom = c("gjl_diare", "gjl_mual"), nilai_positif = "1",
                label = list(gjl_diare = "Diare", gjl_mual = "Mual")),
  makanan = list(kolom = c("mkn_nasi", "mkn_rendang"), nilai_positif = "1",
                 label = list(mkn_nasi = "Nasi", mkn_rendang = "Rendang"))
)

periksa_pemetaan("keracunan_pangan", pemetaan)$lengkap
```

## Analisis yang tersedia

Deskriptif:

- Populasi berisiko, jumlah kasus, attack rate, case fatality rate, tanggal kasus pertama dan terakhir
- Distribusi frekuensi tanda dan gejala pada kasus
- Attack rate menurut jenis kelamin, kelompok umur, tempat, lokasi paparan, status imunisasi, status atau pekerjaan
- Distribusi kasus menurut klasifikasi kasus, hasil laboratorium, jenis spesimen, tempat berobat
- Kurva epidemik dengan interval bawaan seperempat masa inkubasi rata-rata, disertai klasifikasi common source atau propagated source
- Masa inkubasi terpendek, terpanjang, rata-rata, median, dan perkiraan periode paparan dengan hitung mundur
- Spot map sebaran kasus di atas peta leaflet

Analitik:

- Tabel 2x2 per variabel paparan: attack rate kelompok terpapar dan tidak terpapar serta attack rate ratio
- RR pada desain kohort atau OR pada desain kasus kontrol, dengan selang kepercayaan 95 persen metode Katz dan Woolf
- Uji chi-square bila seluruh frekuensi harapan minimal 5, selain itu uji Fisher exact, dengan koreksi 0,5 bila ada sel nol
- Regresi Poisson dengan galat baku robust (adjusted risk ratio) atau regresi logistik (adjusted odds ratio)

Penetapan etiologi dan KLB:

- Diagnosis banding dengan aturan penyingkiran masa inkubasi Pedoman KLB 2020
- Penilaian tujuh kriteria KLB Permenkes 1501 Tahun 2010

## Model bahasa

Model berjalan penuh di peramban pengguna melalui WebGPU dengan pustaka WebLLM, sehingga isi laporan tidak dikirim ke layanan luar. Model tersedia: Qwen2.5 3B, Llama 3.2 3B, Qwen2.5 1.5B, dan Llama 3.2 1B varian q4f16.

Model hanya menyusun narasi. Seluruh angka berasal dari hasil analisis R dan dikirim ke model sebagai daftar fakta disertai larangan menambah angka maupun sitasi. Naskah keluaran model diperiksa otomatis oleh [`periksa_narasi()`] terhadap dua risiko: angka yang tidak ada pada hasil analisis, dan kalimat yang menyatakan kepastian penyebab.

Bila WebGPU tidak tersedia, aplikasi tetap menghasilkan draf laporan lengkap dari template deterministik.

Batasan yang perlu disadari: model kecil masih dapat salah menyalin angka atau menulis kalimat spekulatif. Draf hasil model wajib diperiksa terhadap tabel analisis sebelum laporan dipakai.

## Tabel rujukan agen penyebab

Tabel diagnosis banding memuat dua kelompok sumber. Baris dari dokumen acuan proyek ditandai `perlu_verifikasi = FALSE`, sedangkan baris dari rujukan umum compendium ditandai `TRUE` dan wajib diverifikasi penyelidik sebelum dipakai menarik kesimpulan.

```r
subset(rujukan_patogen(), !perlu_verifikasi, c(nama, inkubasi_min_jam, inkubasi_maks_jam, sumber))
```

## Keamanan dan privasi

- Kata sandi disimpan sebagai hash argon2 melalui paket sodium, dengan tiga peran: admin, analis, dan viewer.
- Token KoboToolbox tersimpan di node kabupaten dan hanya dipakai oleh proses R di node tersebut.
- Payload agregat dibentuk dari objek hasil analisis, bukan dari data individu, sehingga tidak ada jalur yang memungkinkan baris responden ikut terkirim.
- Pengiriman ke provinsi memerlukan konfirmasi eksplisit dan dicatat pada tabel `pengiriman`.
- Kunci API node dibuat di dashboard provinsi, disimpan sebagai hash, dan hanya ditampilkan satu kali.
- Aktivitas penting dicatat pada tabel `audit`.

## Acuan

| Dokumen | Dipakai untuk |
|---|---|
| Pedoman Penyelidikan dan Penanggulangan KLB Penyakit Menular dan Keracunan Pangan, Kemenkes, Edisi Revisi III 2020 | Kriteria KLB Permenkes 1501/2010, langkah penyelidikan, analisis orang tempat waktu, kurva epidemik, aturan penyingkiran etiologi, tabel kohort |
| Checklist Laporan KLB Keracunan Makanan | Panel kelengkapan laporan dan daftar tabel wajib |
| Panduan Penulisan Laporan KLB | Struktur bab laporan dengan format IMRAD |

## Pengembangan

```r
devtools::load_all()
devtools::test()
devtools::check()
```

## Lisensi

MIT
