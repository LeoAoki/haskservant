function servidor(){
    let x = document.querySelector("#n1").value
    let y = document.querySelector("#n2").value 
    fetch("http://localhost:8080/soma", {
        method: "POST",
        headers: {"content-type" : "application/json"},
        body: JSON.stringify({n1: parseInt(x), n2: parseInt(y)}),
    })
    .then(response => response.json())
    .then(json => {
        document.querySelector("#resultado").innerHTML = json.resultado
    })
    .catch(error => alert(error))
}

function teste(){
    document.querySelector("#btn").addEventListener("click", () => {
        servidor()
    })
}

window.onload = teste