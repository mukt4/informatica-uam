function validateForm(){
	var usr = document.forms["login-form"]["nombre-usuario"].value;
	var psw = document.forms["login-form"]["contrasenia"].value;

	if(usr == null || usr == ""){
		alert("El campo nombre de usuario debe estar relleno");
		return false;
	}

	if(psw == null || psw == ""){
		alert("El campo contrasenia debe estar relleno");
		return false;
	}
}