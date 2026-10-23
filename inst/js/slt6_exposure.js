/* =============================================================================
 * slt6_exposure.js
 * SLT v6 – Expositionsblock mit Oddball-Reaktionszeit (Plan Abschnitt 6).
 *
 * - Laedt die Einzeltoene (Klavier + Klarinette) und spielt den Strom selbst
 *   nach Plan ab: AudioBufferSourceNode.start(t) auf der Audio-Uhr, mit
 *   Lookahead-Scheduler. Dadurch sind alle Onsets exakt bekannt; mp3-Padding
 *   und Offset-Messung pro Blockdatei entfallen.
 * - Der AudioContext wird erst beim Start (Klick) erzeugt. Beim Laden der
 *   Seite werden nur die Dateien geholt (21.10.: Edge blieb sonst haengen).
 * - Leertaste: event.timeStamp (performance-Uhr) wird ueber
 *   ctx.getOutputTimestamp() in Audio-Kontextzeit umgerechnet (= Zeitpunkt,
 *   zu dem der Ton tatsaechlich aus dem Ausgabegeraet kam). Zur Diagnose wird
 *   jeder Druck zusaetzlich ueber ctx.currentTime umgerechnet (presses_diag).
 * - Zuordnung: Ein Druck 150–1500 ms nach einem Oddball-Onset ist ein Treffer
 *   (erster Druck zaehlt), alle anderen Druecke sind Falschalarme.
 *
 * Verwendung:
 *   SLT6Exposure.load({ baseUrl, tones }) -> Promise<player>   (beim Seitenaufbau)
 *   player.run({ schedule, onProgress, onDone, onError })      (im Klick-Handler!)
 *     schedule: [{ t: Sekunden ab Blockstart, tone: "C4" | "BEEP", odd: bool,
 *                  odd_id: "...", condition: "..." }]
 *     onDone(result): result = { presses, presses_diag, oddballs, hits, false_alarms, meta }
 * ========================================================================== */
(function (global) {
  "use strict";

  var WINDOW_MIN = 0.150, WINDOW_MAX = 1.500;   // Antwortfenster (s); 21.10.: 1200 -> 1500 ms wegen Bluetooth-Versatz
  var LOOKAHEAD = 1.0, TICK_MS = 25, LEAD_IN = 0.8, TAIL = 1.4, RESUME_TIMEOUT_MS = 4000;

  function fileName(timbre, tone) { return timbre + "_" + tone.replace("#", "s") + ".wav"; }

  // Phase 1 (Seitenaufbau): nur Dateien holen
  function load(opts) {
    var raw = { grand: {}, clarinet: {} }, jobs = [];
    ["grand", "clarinet"].forEach(function (tb) {
      opts.tones.forEach(function (tn) {
        jobs.push(fetch(opts.baseUrl + fileName(tb, tn))
          .then(function (r) { if (!r.ok) throw new Error("Ladefehler " + r.url); return r.arrayBuffer(); })
          .then(function (ab) { raw[tb][tn] = ab; }));
      });
    });
    return Promise.all(jobs).then(function () { return makePlayer(raw); });
  }

  // Technik-Test: kurzer Sinuston (1 kHz, 60 ms, 5 ms Rampen) als Ton "BEEP"
  function makeBeep(ctx) {
    var n = Math.round(0.06 * ctx.sampleRate), r = Math.round(0.005 * ctx.sampleRate);
    var buf = ctx.createBuffer(1, n, ctx.sampleRate), d = buf.getChannelData(0);
    for (var i = 0; i < n; i++) {
      var env = Math.min(1, i / r, (n - 1 - i) / r);
      d[i] = 0.3 * env * Math.sin(2 * Math.PI * 1000 * i / ctx.sampleRate);
    }
    return buf;
  }

  function decodeAll(ctx, raw) {
    var buffers = { grand: {}, clarinet: {} }, jobs = [];
    Object.keys(raw).forEach(function (tb) {
      Object.keys(raw[tb]).forEach(function (tn) {
        var copy = raw[tb][tn].slice(0);   // decodeAudioData verbraucht den Puffer
        jobs.push(new Promise(function (res, rej) { ctx.decodeAudioData(copy, res, rej); })
          .then(function (buf) { buffers[tb][tn] = buf; }));
      });
    });
    return Promise.all(jobs).then(function () { return buffers; });
  }

  function withTimeout(p, ms, msg) {
    return Promise.race([p, new Promise(function (_, rej) { setTimeout(function () { rej(new Error(msg)); }, ms); })]);
  }

  // Phase 2 (Klick): AudioContext erzeugen, dekodieren, abspielen
  function makePlayer(raw) {
    var ctx = null, buffers = null, beep = null;

    function ensureContext() {
      if (!ctx) {
        var Ctx = global.AudioContext || global.webkitAudioContext;
        ctx = new Ctx({ latencyHint: "interactive" });
      }
      var resumed = withTimeout(ctx.resume(), RESUME_TIMEOUT_MS,
        "Audioausgabe startet nicht (Zustand: " + ctx.state + "). Anderes Ausgabegeraet oder anderen Browser versuchen.");
      return resumed.then(function () {
        if (buffers) return;
        beep = makeBeep(ctx);
        return decodeAll(ctx, raw).then(function (b) { buffers = b; });
      });
    }

    function keyTimes(ev) {
      var age = (performance.now() - ev.timeStamp) / 1000;
      var cur = ctx.currentTime - age;                         // Kontextzeit, ohne Ausgabelatenz
      var out = null;
      if (ctx.getOutputTimestamp) {
        var ts = ctx.getOutputTimestamp();
        if (ts && ts.performanceTime > 0) out = ts.contextTime + (ev.timeStamp - ts.performanceTime) / 1000;
      }
      var lat = ctx.outputLatency || ctx.baseLatency || 0;
      return { t: out !== null ? out : cur - lat, method: out !== null ? "outputTimestamp" : "currentTime",
               out: out, cur: cur, lat: lat };
    }

    function run(o) {
      var schedule = o.schedule.slice().sort(function (a, b) { return a.t - b.t; });
      var presses = [], diag = [], next = 0, timer = null, t0 = null, methods = {};

      function onKey(ev) {
        if (ev.code !== "Space" && ev.key !== " ") return;
        ev.preventDefault();
        if (ev.repeat || t0 === null) return;
        var k = keyTimes(ev);
        methods[k.method] = true;
        presses.push(k.t - t0);
        diag.push({ out_ms: k.out === null ? null : Math.round((k.out - t0) * 1000),
                    cur_ms: Math.round((k.cur - t0) * 1000), lat_ms: Math.round(k.lat * 1000) });
      }

      function tick() {
        var horizon = ctx.currentTime + LOOKAHEAD;
        while (next < schedule.length && t0 + schedule[next].t < horizon) {
          var e = schedule[next];
          var src = ctx.createBufferSource();
          src.buffer = e.tone === "BEEP" ? beep : buffers[e.odd ? "clarinet" : "grand"][e.tone];
          src.connect(ctx.destination);
          src.start(t0 + e.t);
          next++;
        }
        var end = schedule[schedule.length - 1].t + TAIL;
        if (o.onProgress) o.onProgress(Math.min(1, (ctx.currentTime - t0) / end));
        if (next >= schedule.length && ctx.currentTime > t0 + end) finish();
      }

      function finish() {
        clearInterval(timer);
        global.removeEventListener("keydown", onKey, true);
        var odds = schedule.filter(function (e) { return e.odd; }).map(function (e) {
          return { odd_id: e.odd_id || null, condition: e.condition || null, tone: e.tone, onset: e.t, rt: null };
        });
        var used = presses.map(function () { return false; });
        odds.forEach(function (od) {
          for (var i = 0; i < presses.length; i++) {
            var d = presses[i] - od.onset;
            if (!used[i] && d >= WINDOW_MIN && d <= WINDOW_MAX) { od.rt = Math.round(d * 1000); used[i] = true; break; }
          }
        });
        if (o.onDone) o.onDone({
          presses: presses.map(function (p) { return Math.round(p * 1000); }),
          presses_diag: diag,
          oddballs: odds,
          hits: odds.filter(function (x) { return x.rt !== null; }).length,
          false_alarms: used.filter(function (u) { return !u; }).length,
          meta: {
            sampleRate: ctx.sampleRate, baseLatency: ctx.baseLatency || null,
            outputLatency: ctx.outputLatency || null, timing_method: Object.keys(methods).join(","),
            userAgent: navigator.userAgent, n_events: schedule.length
          }
        });
      }

      function fail(err) {
        clearInterval(timer);
        global.removeEventListener("keydown", onKey, true);
        if (o.onError) o.onError(err); else throw err;
      }

      return ensureContext().then(function () {
        global.addEventListener("keydown", onKey, true);
        t0 = ctx.currentTime + LEAD_IN;
        var c0 = ctx.currentTime;
        tick();
        timer = setInterval(tick, TICK_MS);
        // Waechter (21.10.): Meldet das Audiogeraet einen Fehler, bleibt die
        // Audio-Uhr stehen und der Block wuerde nie enden.
        setTimeout(function () {
          if (ctx.state !== "running" || ctx.currentTime - c0 < 0.5)
            fail(new Error("Die Audio-Uhr des Browsers läuft nicht (Zustand: " + ctx.state +
              "). Meist ist das Audio-Ausgabegerät gerade nicht verfügbar: Kopfhörer neu verbinden, " +
              "in anderen Tabs/Programmen Audio beenden und die Seite neu laden."));
        }, 1500);
      }).catch(fail);
    }
    return { run: run };
  }

  global.SLT6Exposure = { load: load, WINDOW_MIN: WINDOW_MIN, WINDOW_MAX: WINDOW_MAX };
})(window);
