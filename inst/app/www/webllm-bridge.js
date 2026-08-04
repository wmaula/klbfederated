/**
 * Jembatan WebLLM untuk Shiny.
 *
 * Model bahasa berjalan penuh di peramban pengguna melalui WebGPU sehingga isi
 * laporan dan angka hasil analisis tidak dikirim ke layanan luar. R hanya
 * mengirim perintah dan menerima teks hasil melalui Shiny.setInputValue.
 *
 * Pustaka WebLLM diambil dari CDN saat pertama dipakai. Bobot model juga
 * diunduh dari internet lalu disimpan pada cache peramban, sehingga pemakaian
 * berikutnya tidak perlu mengunduh ulang.
 */
(function () {
  let engine = null;
  let modelAktif = null;
  let pustaka = null;

  function status(tahap, pesan, progres) {
    Shiny.setInputValue(
      "webllm_status",
      { tahap: tahap, pesan: pesan, progres: progres || 0, waktu: Date.now() },
      { priority: "event" }
    );
  }

  async function muatPustaka() {
    if (pustaka) return pustaka;
    status("memuat", "Mengambil pustaka WebLLM...", 0);
    pustaka = await import("https://esm.run/@mlc-ai/web-llm");
    return pustaka;
  }

  async function webgpuTersedia() {
    if (!navigator.gpu) return false;
    try {
      return (await navigator.gpu.requestAdapter()) !== null;
    } catch (e) {
      return false;
    }
  }

  async function muatModel(modelId) {
    if (engine && modelAktif === modelId) return engine;
    if (!(await webgpuTersedia())) {
      status(
        "tidak_didukung",
        "Peramban ini tidak mendukung WebGPU sehingga model bahasa tidak dapat dijalankan. Draf template deterministik tetap tersedia.",
        0
      );
      throw new Error("WebGPU tidak tersedia");
    }
    const webllm = await muatPustaka();
    engine = await webllm.CreateMLCEngine(modelId, {
      initProgressCallback: function (r) {
        status("memuat", r.text, r.progress || 0);
      },
    });
    modelAktif = modelId;
    status("siap", "Model " + modelId + " siap dipakai", 1);
    return engine;
  }

  Shiny.addCustomMessageHandler("webllm_periksa", async function () {
    const ada = await webgpuTersedia();
    Shiny.setInputValue("webllm_webgpu", ada, { priority: "event" });
    if (ada) {
      try {
        const webllm = await muatPustaka();
        const daftar = webllm.prebuiltAppConfig.model_list
          .filter(function (m) {
            return /Qwen2\.5-(1\.5B|3B)-Instruct-q4f16_1-MLC|Llama-3\.2-(1B|3B)-Instruct-q4f16_1-MLC/.test(
              m.model_id
            );
          })
          .map(function (m) {
            return { id: m.model_id, vram: m.vram_required_MB || null };
          });
        Shiny.setInputValue("webllm_katalog", daftar, { priority: "event" });
        status("belum", "Pustaka WebLLM siap, model belum dimuat", 0);
      } catch (e) {
        status("gagal", "Gagal memuat pustaka WebLLM: " + e.message, 0);
      }
    }
  });

  Shiny.addCustomMessageHandler("webllm_muat", async function (pesan) {
    try {
      await muatModel(pesan.model);
    } catch (e) {
      status("gagal", e.message, 0);
    }
  });

  Shiny.addCustomMessageHandler("webllm_tulis", async function (pesan) {
    try {
      const mesin = await muatModel(pesan.model);
      status("menulis", "Model sedang menulis bagian " + pesan.bagian, 1);
      const aliran = await mesin.chat.completions.create({
        messages: [
          { role: "system", content: pesan.sistem },
          { role: "user", content: pesan.perintah },
        ],
        temperature: 0.3,
        max_tokens: 1200,
        stream: true,
      });
      let hasil = "";
      for await (const bagian of aliran) {
        hasil += bagian.choices[0]?.delta?.content || "";
      }
      Shiny.setInputValue(
        "webllm_hasil",
        { bagian: pesan.bagian, teks: hasil.trim(), waktu: Date.now() },
        { priority: "event" }
      );
      status("siap", "Naskah bagian " + pesan.bagian + " selesai ditulis", 1);
    } catch (e) {
      status("gagal", "Model gagal menulis: " + e.message, 0);
    }
  });

  Shiny.addCustomMessageHandler("webllm_lepas", async function () {
    if (engine) {
      await engine.unload();
      engine = null;
      modelAktif = null;
      status("belum", "Model dilepas dari memori", 0);
    }
  });
})();
