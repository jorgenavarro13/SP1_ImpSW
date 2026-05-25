// ── Shared helpers ────────────────────────────────────────────────────────────

function clearErrors(id) {
    document.getElementById(`syntax-errors-${id}`).classList.add("hidden");
    document.getElementById(`error-box-${id}`).classList.add("hidden");
    document.getElementById(`textbox-${id}`).classList.remove("has-error");
}

function showErrors(errors, id) {
    const panel = document.getElementById(`syntax-errors-${id}`);
    const list  = document.getElementById(`syntax-error-list-${id}`);
    list.innerHTML = errors.map(e => `<li>${e}</li>`).join("");
    panel.classList.remove("hidden");
    document.getElementById(`textbox-${id}`).classList.add("has-error");
    highlightFirstError(errors, id);
}

function highlightFirstError(errors, id) {
    const textarea = document.getElementById(`textbox-${id}`);
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

async function run(id) {
    const text    = document.getElementById(`textbox-${id}`).value.trim();
    const btn     = document.getElementById(`run-btn-${id}`);
    const loading = document.getElementById(`loading-${id}`);
    const results = document.getElementById(`results-${id}`);

    clearErrors(id);
    results.classList.add("hidden");
    loading.classList.remove("hidden");
    btn.disabled = true;
    btn.style.opacity = "0.5";

    try {
        const response = await fetch("/", {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({ input: text })
        });

        const data = await response.json();

        if (!response.ok) {
            showErrors(data.errors || ["Server error: " + response.status], id);
            return;
        }

        document.getElementById(`token-output-${id}`).innerHTML = data.result;
        document.getElementById(`graph-output-${id}`).src = "data:image/png;base64," + data.image;
        results.classList.remove("hidden");

    } catch (err) {
        const errBox = document.getElementById(`error-box-${id}`);
        errBox.textContent = err.message;
        errBox.classList.remove("hidden");
    } finally {
        loading.classList.add("hidden");
        btn.disabled = false;
        btn.style.opacity = "1";
    }
}

// ── Simulate ──────────────────────────────────────────────────────────────────

async function simulate(id) {
    const definition = document.getElementById(`textbox-${id}`).value.trim();
    const simInput   = document.getElementById(`sim-input-${id}`).value;
    const btn        = document.getElementById(`sim-btn-${id}`);
    const resultDiv  = document.getElementById(`sim-result-${id}`);
    const verdict    = document.getElementById(`sim-verdict-${id}`);
    const path       = document.getElementById(`sim-path-${id}`);

    btn.disabled = true;
    btn.style.opacity = "0.5";
    resultDiv.classList.add("hidden");
    clearErrors(id);

    try {
        const response = await fetch("/simulate", {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({ definition, input: simInput })
        });

        const data = await response.json();

        if (!response.ok) {
            showErrors(data.errors || ["Server error: " + response.status], id);
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
