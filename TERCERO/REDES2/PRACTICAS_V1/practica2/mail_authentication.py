import smtplib

import config

class Email:
	def send_mail(self, receiver, message):
		mail = smtplib.SMTP('smtp.gmail.com:587')
		mail.ehlo()
		mail.starttls()
		mail.login(config.EMAIL_ADDRESS, config.PASSWORD)
		content = 'Codigo de verificacion: ' + str(message)
		mail.sendmail(config.EMAIL_ADDRESS, receiver, content)
		mail.quit()
		
		print('Correo enviado')