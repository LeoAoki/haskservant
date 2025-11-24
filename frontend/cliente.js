function servidor(){
    let clienteNome = document.querySelector("#nome").value
    let clienteCPF = document.querySelector("#cpf").value 
    fetch("http://localhost:8080/cliente",{
        method: "POST",
        headers: {"content-type" : "application/json"},
        body: JSON.stringify({id : 0, nome: clienteNome, cpf: clienteCPF}),
    })
    .then(response => response.json())
    .then(json => {
        document.querySelector("#res").innerHTML = "id: " + json.resultado
    })
    .catch(error => alert(error))
}

function teste(){
    document.querySelector("#btn").addEventListener("click", () => {
        servidor()
    })
}

window.onload = teste