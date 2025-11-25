const API_BASE = "http://localhost:8080";

async function fetchJson(url, options = {}) {
    const response = await fetch(url, {
        headers: { "content-type": "application/json" },
        ...options,
    });
    if (!response.ok) {
        const text = await response.text();
        throw new Error(text || `Erro HTTP ${response.status}`);
    }
    if (response.status === 204) return null;
    return response.json();
}

async function carregarVeiculos() {
    try {
        const data = await fetchJson(`${API_BASE}/veiculos`);
        renderVeiculos(data.veiculos || []);
    } catch (error) {
        mostrarMensagem(error.message, true);
    }
}

function renderVeiculos(veiculos) {
    const tbody = document.querySelector("#tabela-veiculos tbody");
    tbody.innerHTML = "";

    if (veiculos.length === 0) {
        const row = document.createElement("tr");
        const cell = document.createElement("td");
        cell.colSpan = 5;
        cell.textContent = "Nenhum veículo cadastrado.";
        row.appendChild(cell);
        tbody.appendChild(row);
        return;
    }

    veiculos.forEach((veiculo) => {
        const row = document.createElement("tr");
        row.innerHTML = `
            <td>${veiculo.idVeiculo}</td>
            <td>${veiculo.placa}</td>
            <td>${veiculo.modelo}</td>
            <td>${veiculo.ano}</td>
            <td class="actions"></td>
        `;

        const actionsCell = row.querySelector(".actions");

        const patchButton = document.createElement("button");
        patchButton.textContent = "Atualizar modelo";
        patchButton.onclick = () => atualizarModelo(veiculo.idVeiculo);

        const putButton = document.createElement("button");
        putButton.textContent = "Editar (PUT)";
        putButton.onclick = () => atualizarVeiculo(veiculo.idVeiculo, veiculo);

        const deleteButton = document.createElement("button");
        deleteButton.textContent = "Excluir";
        deleteButton.onclick = () => removerVeiculo(veiculo.idVeiculo);

        actionsCell.appendChild(patchButton);
        actionsCell.appendChild(putButton);
        actionsCell.appendChild(deleteButton);
        tbody.appendChild(row);
    });
}

async function criarVeiculo(event) {
    event.preventDefault();
    const placa = document.querySelector("#placa").value.trim();
    const modelo = document.querySelector("#modelo").value.trim();
    const ano = parseInt(document.querySelector("#ano").value, 10);

    if (!placa || !modelo || Number.isNaN(ano)) {
        mostrarMensagem("Preencha todos os campos.", true);
        return;
    }

    try {
        const data = await fetchJson(`${API_BASE}/veiculo`, {
            method: "POST",
            body: JSON.stringify({ placa, modelo, ano }),
        });
        mostrarMensagem(`Veículo cadastrado (id: ${data.resultado}).`);
        document.querySelector("#veiculo-form").reset();
        carregarVeiculos();
    } catch (error) {
        mostrarMensagem(error.message, true);
    }
}

async function removerVeiculo(id) {
    if (!confirm(`Remover veículo ${id}?`)) return;
    try {
        await fetchJson(`${API_BASE}/veiculo/${id}`, { method: "DELETE" });
        mostrarMensagem(`Veículo ${id} removido.`);
        carregarVeiculos();
    } catch (error) {
        mostrarMensagem(error.message, true);
    }
}

async function atualizarModelo(id) {
    const novoModelo = prompt("Novo modelo:");
    if (!novoModelo) return;
    try {
        await fetchJson(`${API_BASE}/veiculo/${id}`, {
            method: "PATCH",
            body: JSON.stringify({ novoModelo }),
        });
        mostrarMensagem(`Modelo do veículo ${id} atualizado.`);
        carregarVeiculos();
    } catch (error) {
        mostrarMensagem(error.message, true);
    }
}

async function atualizarVeiculo(id, veiculoAtual) {
    const novaPlaca = prompt("Placa:", veiculoAtual.placa);
    if (novaPlaca === null) return;
    const novoModelo = prompt("Modelo:", veiculoAtual.modelo);
    if (novoModelo === null) return;
    const novoAnoStr = prompt("Ano:", veiculoAtual.ano);
    if (novoAnoStr === null) return;
    const ano = parseInt(novoAnoStr, 10);
    if (!novaPlaca.trim() || !novoModelo.trim() || Number.isNaN(ano)) {
        mostrarMensagem("Valores inválidos para atualização.", true);
        return;
    }

    try {
        await fetchJson(`${API_BASE}/veiculo/${id}`, {
            method: "PUT",
            body: JSON.stringify({ placa: novaPlaca.trim(), modelo: novoModelo.trim(), ano }),
        });
        mostrarMensagem(`Veículo ${id} atualizado (PUT).`);
        carregarVeiculos();
    } catch (error) {
        mostrarMensagem(error.message, true);
    }
}

function mostrarMensagem(msg, isError = false) {
    const div = document.querySelector("#mensagem");
    div.textContent = msg;
    div.style.color = isError ? "red" : "green";
}

function iniciar() {
    document.querySelector("#veiculo-form").addEventListener("submit", criarVeiculo);
    carregarVeiculos();
}

window.onload = iniciar;
