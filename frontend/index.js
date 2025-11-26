const API_BASE = "http://localhost:8080";

async function fetchJson(url, options = {}) {
  const response = await fetch(url, {
    headers: { "content-type": "application/json" },
    ...options,
  });
  const text = await response.text();
  if (!response.ok) {
    throw new Error(text || `Erro HTTP ${response.status}`);
  }
  if (!text.trim()) return null;
  try {
    return JSON.parse(text);
  } catch (err) {
    throw new Error(text || "Resposta vazia ou inválida");
  }
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
    row.classList.add("empty-row");
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
    patchButton.className = "btn btn-modelo";
    patchButton.onclick = () => atualizarModelo(veiculo.idVeiculo);

    const placaButton = document.createElement("button");
    placaButton.textContent = "Atualizar placa";
    placaButton.className = "btn btn-placa";
    placaButton.onclick = () => atualizarPlaca(veiculo.idVeiculo, veiculo);

    const deleteButton = document.createElement("button");
    deleteButton.textContent = "Excluir";
    deleteButton.className = "btn btn-delete";
    deleteButton.onclick = () => removerVeiculo(veiculo.idVeiculo);

    actionsCell.appendChild(patchButton);
    actionsCell.appendChild(placaButton);
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

async function atualizarPlaca(id, veiculoAtual) {
  const novaPlaca = prompt("Nova placa:", veiculoAtual.placa);
  if (novaPlaca === null) return;

  const placa = novaPlaca.trim();
  if (!placa) {
    mostrarMensagem("Placa inválida.", true);
    return;
  }

  try {
    await fetchJson(`${API_BASE}/veiculo/${id}`, {
      method: "PUT",
      body: JSON.stringify({
        placa,
        modelo: veiculoAtual.modelo,
        ano: veiculoAtual.ano,
      }),
    });
    mostrarMensagem(`Placa do veículo ${id} atualizada.`);
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
  document
    .querySelector("#veiculo-form")
    .addEventListener("submit", criarVeiculo);
  carregarVeiculos();
}

window.onload = iniciar;
