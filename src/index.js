// ── Shared helpers ────────────────────────────────────────────────────────────

function clearErrors() {
    document.getElementById("syntax-errors").classList.add("hidden");
    document.getElementById("error-box").classList.add("hidden");
    document.getElementById("textbox").classList.remove("has-error");
}

function showErrors(errors) {
    const panel = document.getElementById("syntax-errors");
    const list  = document.getElementById("syntax-error-list");
    list.innerHTML = errors.map(e => `<li>${e}</li>`).join("");
    panel.classList.remove("hidden");
    document.getElementById("textbox").classList.add("has-error");
    highlightFirstError(errors);
}

function highlightFirstError(errors) {
    const textarea = document.getElementById("textbox");
    const text = textarea.value;

    for (const msg of errors) {
        const match = msg.match(/'([^']+)'/);
        if (!match) continue;

        const term = match[1];
        const idx  = text.indexOf(term);
        if (idx === -1) continue;

        textarea.focus();
        textarea.setSelectionRange(idx, idx + term.length);

        const linesBefore = text.substring(0, idx).split("\n").length - 1;
        const lineHeight  = parseInt(getComputedStyle(textarea).lineHeight) || 20;
        textarea.scrollTop = Math.max(0, (linesBefore - 2)) * lineHeight;
        return;
    }
}

// ── Run (tokenise + validate) ─────────────────────────────────────────────────

async function run() {
    const text    = document.getElementById("textbox").value.trim();
    const btn     = document.getElementById("run-btn");
    const loading = document.getElementById("loading");
    const results = document.getElementById("results");

    clearErrors();
    results.classList.add("hidden");
    loading.classList.remove("hidden");
    btn.disabled = true;
    btn.style.opacity = "0.5";

    try {
        const response = await fetch("/", {
            method:  "POST",
            headers: { "Content-Type": "application/json" },
            body:    JSON.stringify({ input: text, mode: "" })
        });

        const data = await response.json();

        if (!response.ok) {
            showErrors(data.errors || ["Server error: " + response.status]);
            return;
        }

        document.getElementById("token-output").innerHTML = data.result;
        document.getElementById("graph-output").src = "data:image/png;base64," + data.image;
        results.classList.remove("hidden");

    } catch (err) {
        const errBox = document.getElementById("error-box");
        errBox.textContent = err.message;
        errBox.classList.remove("hidden");
    } finally {
        loading.classList.add("hidden");
        btn.disabled = false;
        btn.style.opacity = "1";
    }
}

// ── Simulate ──────────────────────────────────────────────────────────────────

async function simulate() {
    const definition = document.getElementById("textbox").value.trim();
    const simInput   = document.getElementById("sim-input").value;
    const btn        = document.getElementById("sim-btn");
    const resultDiv  = document.getElementById("sim-result");
    const verdict    = document.getElementById("sim-verdict");
    const path       = document.getElementById("sim-path");

    btn.disabled = true;
    btn.style.opacity = "0.5";
    resultDiv.classList.add("hidden");
    clearErrors();

    try {
        const response = await fetch("/simulate", {
            method:  "POST",
            headers: { "Content-Type": "application/json" },
            body:    JSON.stringify({ definition, input: simInput, mode: "" })
        });

        const data = await response.json();

        if (!response.ok) {
            showErrors(data.errors || ["Server error: " + response.status]);
            return;
        }

        if (data.accepted) {
            verdict.textContent = "ACCEPTED";
            verdict.style.color = "#a6e22e";
        } else {
            verdict.textContent = "REJECTED";
            verdict.style.color = "#f92672";
        }
        path.textContent = data.path ? "Path: " + data.path : "";
        resultDiv.classList.remove("hidden");

    } catch (err) {
        verdict.textContent = err.message;
        verdict.style.color = "#f92672";
        path.textContent    = "";
        resultDiv.classList.remove("hidden");
    } finally {
        btn.disabled = false;
        btn.style.opacity = "1";
    }
}
