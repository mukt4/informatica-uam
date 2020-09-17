function validateForm(){
	var x = document.forms["formulario-registro"]["cuestionario_nombreCompleto"].value;
	var filter = /^([a-zA-Z0-9_.-])+@(([a-zA-Z0-9-])+.)+([a-zA-Z0-9]{2,4})+$/;

	if(x == null || x == ""){
		alert("El campo nombre completo debe estar relleno");
		return false;
	}

	x = document.forms["formulario-registro"]["cuestionario_contrasenia"].value;
	var y = document.forms["formulario-registro"]["cuestionario_contrasenia2"].value;

	if(x == null || x == "" || x.length < 8){
		alert("Debes introducir una contrasenia de mas de 8 caracteres");
		return false;
	}
	else if(y == null || x != y){
		alert("Ambas contrasenias deben coincidir");
		return false;
	}

	x = document.forms["formulario-registro"]["cuestionario_correo"].value;

	if(x == null || x == "" || !filter.test(x)){
		alert("El campo de email no esta escrito correctamente");
		return false;
	}

	x = document.forms["formulario-registro"]["cuestionario_nombre"].value;
	if(x == null || x == ""){
		alert("Debes rellenar el campo de nombre de usuario");
		return false;
	}

	x = document.forms["formulario-registro"]["cuestionario_cuenta"].value;
	if(x == null || x == "" || isNaN(x) || x.length !== 16){
		alert("El numero de tarjeta no es valido");
		return false;
	}
}