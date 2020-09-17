# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.shortcuts import render
from data.models import Category, WorkFlow
from find.forms import SearchForm
from django.http import HttpResponse
from django.conf import settings

# Create your views here.
import json
import urllib
import urllib2
	
def about(request):
	return render(request, "find/about.html")

# Funcion utilizada para definir la vista que muestra la pantalla principal
def workflow_list(request, category_slug=None):
	_dict = {
			'category': None, # category associated to category_slug
			'categories': None, # list with all categories
			# usefull to repaint the category
			# menu
			'workflows': None, # all workflows associated to category
			# category_slug
	  		'result': False, # False if no workflow satisfices the query
			'error': None # message to display if results == False
	}
	# YOUR CODE GOES HERE
	_dict['categories'] = Category.objects.all()

	if(category_slug):
		categoria = Category.objects.filter(slug = category_slug)
		if(categoria):
			_dict['category'] = categoria[0]
			workflows = WorkFlow.objects.filter(category = categoria)
			if(workflows):
				_dict['result'] = True
				_dict['workflows'] = workflows
			else:
				_dict['result'] = False
				_dict['error'] = "No existe ningun workflow asociado a la categoria " + category_slug

		else:
			_dict['result'] = False
			_dict['error'] = "No existe ninguna categoria asociada al slug " + category_slug
	else:
		# Si no buscamos ningun workflow el resultado que guardamos en el diccionario es true
		_dict['result'] = True
		_dict['workflows'] = WorkFlow.objects.all()
	# queries that fill, category, categories, workflows
	# and error
	return render(request, 'find/list.html', _dict)

# Funcion que define la vista para ver un workflow con detalle
def workflow_detail(request, id, slug):
	#Your code goes here
	#query that returns the workflow with id=id
	_dict = {
		'result' : False, # False if no workflow satisfices the query
		'workflow' : None, # workflow with id = id
		'error' : None # message to display if results == False
	}

	resultado = WorkFlow.objects.filter(id = id, slug = slug)

	if(resultado):
		_dict['result'] = True
		_dict['workflow'] = resultado[0]
	else:
		_dict['result'] = False
		_dict['error'] = "No existe ningun workflow que satisfaga la busqueda"

	return render(request, 'find/detail.html', _dict)

# Funcion que define la vista para realizar una busqueda de un workflow
def workflow_search(request, name=None):
	#YOUR CODE GOES HERE
	#query that returns the workflow with name = name
	_dict = {
		'result' : False, # False if no workflow satisfices the query
		'workflow' : None, # workflow with name = name
		'error' : None # message to display if results == False
	}
	if(request.method == "POST"):
		name = SearchForm(request.POST)
		name = request.POST.get('key')
		try:
			workflow = WorkFlow.objects.get(slug = name)
			_dict['result'] = True
			_dict['workflow'] = workflow
		except WorkFlow.DoesNotExist:
			workflow = WorkFlow.objects.filter(name = name)
			if(workflow):
				_dict['result'] = True
				_dict['workflow'] = workflow[0]
			else:
				_dict['result'] = False
				_dict['error'] = "No existe ningun workflow que satisfaga la busqueda"
	else:
		_dict['result'] = False
		_dict['error'] = "Metodo GET no soportado para la busqueda"

	return render(request, 'find/detail.html', _dict)

# Funcion utilizada para descargar un workflow
def workflow_download(request, id, slug, count = True):
	# Creamos este diccionario por si se trata de descargar un workflow que no existe
	_dict = {
		'error' : None,
		'result' : False
	}

	# Parte de validacion con recaptcha, descomentar a partir de aqui

	#recaptcha_response = request.POST.get('g-recaptcha-response')
	#url = 'https://www.google.com/recaptcha/api/siteverify'
	#values = {
    #            'secret': settings.GOOGLE_RECAPTCHA_SECRET_KEY,
    #            'response': recaptcha_response
    #        }

	#data = urllib.urlencode(values)
	#req = urllib2.Request(url, data)
	#response = urllib2.urlopen(req)
	#result = json.load(response)

	# Para validar utilizando recapthcha comentar la linea siguiente 
	# y descomentar las lineas indicadas anteriormente
	result ={'success' : True}

	if(result['success']):
	    # Si se ha validado recaptcha
		workflow = WorkFlow.objects.filter(id = id, slug = slug)

		if(workflow):
			if count == True:
				workflow[0].downloads += 1
				workflow[0].views += 1
				workflow[0].save()

			response = HttpResponse(workflow[0].json, content_type="application/octet-stream")
			if(workflow[0].name.endswith('.json')):
				filename = workflow[0].slug
			else:
				filename = workflow[0].slug + ".json"
			response['Content-Disposition'] = 'inline; filename=%s' % filename
			return response
		else:
			_dict['error'] = "No existe el workflow que quieres descargar."
			_dict['result'] = False
			return render(request, 'find/detail.html',_dict)
	else:
		_dict['error'] = "El reCAPTCHA no ha sido validado"
		_dict['result'] = False
		return render(request, 'find/detail.html',_dict)

# Funcion utilizada para descargar un workflow
def workflow_download_json(request, id, slug):
	_dict = {
		'error' : None,
		'result' : False
	}

	workflow = WorkFlow.objects.filter(id = id, slug = slug)

	if(workflow):		
		return HttpResponse(workflow[0].json, content_type="application/octet-stream")
	else:
		_dict['error'] = "No existe el workflow que quieres descargar."
		_dict['result'] = False
		return render(request, 'find/detail.html',_dict)

# Funcion utilizada para eliminar un workflow
def workflow_delete(request, id, slug):
	_dict = {
		'error' : None,
		'result' : False,
		'categories' : None,
		'workflows' : None
	}

	# Primero comprobamos reCAPTCHA
	recaptcha_response = request.POST.get('g-recaptcha-response')
	url = 'https://www.google.com/recaptcha/api/siteverify'
	values = {
                'secret': settings.GOOGLE_RECAPTCHA_SECRET_KEY,
                'response': recaptcha_response
            }

	data = urllib.urlencode(values)
	req = urllib2.Request(url, data)
	response = urllib2.urlopen(req)
	result = json.load(response)

	if(result['success']):

		workflow = WorkFlow.objects.filter(id = id, slug = slug)

		if(workflow):
			if(workflow[0].delete_id == request.session.session_key):
				workflow[0].delete()
				_dict['result'] = True
				_dict['error'] = None
				_dict['categories'] = Category.objects.all()
				_dict['workflows'] = WorkFlow.objects.all()
				return render(request, 'find/list.html', _dict)
			else:
				_dict['result'] = False
				_dict['error'] = "No coincide la id de session, no se puede borrar workflow"
				return render(request, 'find/detail.html', _dict)
		else:
			_dict['error'] = "No existe el workflow que quieres borrar."
			_dict['result'] = False
			return render(request, 'find/detail.html', _dict)
	else:
		_dict['error'] = "El reCAPTCHA no ha sido validado"
		_dict['result'] = False
		return render(request, 'find/detail.html', _dict)