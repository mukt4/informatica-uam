# Practica realizada por Tomas Higuera Viso y Guillermo Hoyo Bravo
import argparse
import sys

from actions import CreateAction, SearchAction, DeleteAction, ListAction,DeleteFileAction,\
					SignAction, DestIdAction, SourceIdAction

"""
CLASS NAME: SecureBoxClient
Esta es la clase principal de la aplicacion que se encargara de mostrar el menu principal de acciones
cada accion llamara a los modulos correspdientes
"""
class SecureBoxClient(object):
	"""
	NAME: __init__(Constructor)
	DEFINITION: Este es el constructor de la clase que haciendo uso de argparse creara el menu principal
				con las diferentes acciones
	PARAMETERS: None
	RETURN: Instancia de la clase SecureBoxClient creada
	"""
	def __init__(self):
		self.parser = argparse.ArgumentParser(prog="securebox_client", usage="%(prog)s [options]")
		group1 = self.parser.add_argument_group("Commands")
		group1.add_argument("--create_id", "-c", metavar=("name","email"), nargs=2, 
							help="Name and email of the user that you want to register", action=CreateAction)
		group1.add_argument("--search_id", "-s", metavar="name", help="Name or email of the user you want to search",
							action=SearchAction)
		group1.add_argument("--delete_id", "-d", metavar="id", help="Id of the user you want to delete",
							action=DeleteAction)
		group1.add_argument("--upload", metavar="file", help="Name of the file you want to upload. Use --dest_id to get the public key of the specified id",
							action="store")
		group1.add_argument("--list_files", "-l", nargs="?", action=ListAction)
		group1.add_argument("--download", metavar="id", help="Id of the file you want to download. Argument source_id needed",
							action="store")
		group1.add_argument("--delete_file", metavar="id", help="Id of the file you want to delete",
							action=DeleteFileAction)
		group1.add_argument("--encrypt", metavar="file", help="Name of the file you want to encrypt. Use --dest_id to get the public key of the specified id",
							action="store")
		group1.add_argument("--sign", metavar="file", help="Name of the file you want to sign", action=SignAction)
		group1.add_argument("--enc_sign", metavar="file", help="Name of the file you want to encrypt-sign. Use --dest_id to get the public key of the specified id",
							action="store")
		self.parser.add_argument("--source_id", metavar="id", help="Id of the file transmitter", action=SourceIdAction)
		self.parser.add_argument("--dest_id", metavar="id", help="Id of the file receptor", action=DestIdAction)
	
	"""
	NAME: start
	DEFINITION: Esta funcion se encarga simplemente de iniciar el menu con argparse y obtener los
				argumentos de entrada.
	PARAMETERS: None
	RETURN: void
	"""
	def start(self):
		if(len(sys.argv) == 1):
			self.parser.print_help()
		else:
			self.parser.parse_args()

if __name__ == "__main__":
	secureBox = SecureBoxClient()
	secureBox.start()