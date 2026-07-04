// COINr Builder — client-side glue.

// ---- native <input type="range"> binding (Shiny has no default binding for these) ----
var cbRangeBinding = new Shiny.InputBinding();
$.extend(cbRangeBinding, {
  find: function(scope) { return $(scope).find("input.cb-range"); },
  getValue: function(el) { return parseFloat(el.value); },
  setValue: function(el, value) { el.value = value; },
  subscribe: function(el, callback) {
    $(el).on("change.cbRange", function() { callback(true); });
  },
  unsubscribe: function(el) { $(el).off(".cbRange"); },
  receiveMessage: function(el, data) {
    if (data.hasOwnProperty("value")) {
      el.value = data.value;
      cbUpdateLiveLabel(el);
      $(el).trigger("change");
    }
  },
  getRatePolicy: function() { return { policy: "debounce", delay: 350 }; }
});
Shiny.inputBindings.register(cbRangeBinding, "cb.range");

// ---- live value labels next to range sliders ----
function cbUpdateLiveLabel(el) {
  var lbl = document.querySelector('[data-live-for="' + el.id + '"]');
  if (!lbl) return;
  var v = parseFloat(el.value);
  var mode = lbl.dataset.liveMode || "raw";
  if (mode === "pct") lbl.textContent = Math.round(v * 100) + "%";
  else if (mode === "fix2") lbl.textContent = v.toFixed(2);
  else lbl.textContent = v;
}
document.addEventListener("input", function(e) {
  if (e.target.matches && e.target.matches("input.cb-range")) cbUpdateLiveLabel(e.target);
});

// ---- click delegation ----
document.addEventListener("click", function(e) {
  // generic "pick" elements: set a Shiny input from data attributes
  var pick = e.target.closest("[data-pick-input]");
  if (pick) {
    Shiny.setInputValue(pick.dataset.pickInput, pick.dataset.pickValue, { priority: "event" });
  }
  // client-side collapsibles (framework pillars)
  var tog = e.target.closest("[data-toggle-next]");
  if (tog) {
    var next = tog.nextElementSibling;
    if (next) {
      next.classList.toggle("open");
      var caret = tog.querySelector(".caret");
      if (caret) caret.textContent = next.classList.contains("open") ? "▾" : "▸";
    }
  }
  // copy-to-clipboard buttons
  var cp = e.target.closest("[data-copy-target]");
  if (cp) {
    var src = document.querySelector(cp.dataset.copyTarget);
    var txt = src ? (src.tagName === "PRE" ? src.textContent : src.innerText) : (cp.dataset.copyText || "");
    if (cp.dataset.copyText) txt = cp.dataset.copyText;
    if (navigator.clipboard && txt) {
      navigator.clipboard.writeText(txt);
      var old = cp.textContent;
      cp.textContent = "Copied ✓";
      setTimeout(function() { cp.textContent = old; }, 1600);
    }
  }
});

// ---- step switching ----
Shiny.addCustomMessageHandler("set-step", function(id) {
  document.querySelectorAll(".step-page").forEach(function(el) {
    el.classList.toggle("active", el.dataset.step === id);
  });
  var sc = document.querySelector(".main-scroll");
  if (sc) sc.scrollTop = 0;
  // wake hidden outputs (plots/htmlwidgets) now they are visible
  setTimeout(function() { $(window).trigger("resize"); }, 60);
});

// ---- code panel show/hide ----
Shiny.addCustomMessageHandler("code-panel", function(open) {
  var el = document.getElementById("code-panel");
  if (el) el.classList.toggle("hidden", !open);
  setTimeout(function() { $(window).trigger("resize"); }, 60);
});

// ---- programmatic input updates (weight reset etc.) ----
Shiny.addCustomMessageHandler("set-vals", function(items) {
  items.forEach(function(it) {
    var el = document.getElementById(it.id);
    if (!el) return;
    el.value = it.value;
    if (el.classList.contains("cb-range")) cbUpdateLiveLabel(el);
    $(el).trigger("change");
  });
});
