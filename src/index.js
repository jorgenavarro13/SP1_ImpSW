async function run() {
    const text    = document.getElementById("textbox").value.trim();
    const btn     = document.getElementById("run-btn");
    const loading = document.getElementById("loading");
    const errBox  = document.getElementById("error-box");
    const results = document.getElementById("results");

    errBox.classList.add("hidden");
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

        if (!response.ok) throw new Error("Server error: " + response.status);

        const data = await response.json();

        document.getElementById("token-output").innerHTML = data.result;
        document.getElementById("graph-output").src = "data:image/png;base64," + data.image;
        results.classList.remove("hidden");

    } catch (err) {
        errBox.textContent = err.message;
        errBox.classList.remove("hidden");
    } finally {
        loading.classList.add("hidden");
        btn.disabled = false;
        btn.style.opacity = "1";
    }
}

async function simulate() {
    const definition   = document.getElementById("textbox").value.trim();
    const simInput     = document.getElementById("sim-input").value;
    const btn          = document.getElementById("sim-btn");
    const resultDiv    = document.getElementById("sim-result");
    const verdict      = document.getElementById("sim-verdict");
    const path         = document.getElementById("sim-path");
    const syntaxErrors = document.getElementById("syntax-errors");
    const errorList    = document.getElementById("syntax-error-list");

    btn.disabled = true;
    btn.style.opacity = "0.5";
    resultDiv.classList.add("hidden");
    syntaxErrors.classList.add("hidden");

    try {
        const response = await fetch("/simulate", {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({ definition, input: simInput })
        });

        if (response.status === 400) {
            const data = await response.json();
            errorList.innerHTML = (data.errors || ["Unknown syntax error"])
                .map(e => `<li>${e}</li>`)
                .join("");
            syntaxErrors.classList.remove("hidden");
            return;
        }

        if (!response.ok) throw new Error("Server error: " + response.status);

        const data = await response.json();

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
        path.textContent = "";
        resultDiv.classList.remove("hidden");
    } finally {
        btn.disabled = false;
        btn.style.opacity = "1";
    }
}
