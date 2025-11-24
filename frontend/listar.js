function listarClientes() {
    fetch('http://localhost:8080/clientes')
        .then(responde => responde.json())
        .then(data => {
            let clientes = data.clientes
            let div = document.querySelector('#lista-clientes')
            let table = document.createElement('table')
            let head = document.createElement('thead')
            let headerRow = document.createElement('tr')
            let thId = document.createElement('th')
            thId.textContent = 'ID'
            let thNome = document.createElement('th')
            thNome.textContent = 'Nome'
            let thCpf = document.createElement('th')
            thCpf.textContent = 'CPF'
            headerRow.appendChild(thId)
            headerRow.appendChild(thNome)
            headerRow.appendChild(thCpf)
            head.appendChild(headerRow)
            table.appendChild(tHead)
            let tBody = document.createElement('tbody')
            clientes.forEach(cliente => {
                let row = document.createElement('tr')
                let tdId = document.createElement('td')
                tdId.textContent = cliente.idCliente
                let tdNome = document.createElement('td')
                tdNome.textContent = cliente.nome
                let tdCpf = document.createElement('td')
                tdCpf.textContent = cliente.cpf
                row.appendChild(tdId)
                row.appendChild(tdNome)
                row.appendChild(tdCpf)
                tBody.appendChild(row)
            })
            table.appendChild(tBody)
            div.appendChild(table)
        })
}

listarClientes()