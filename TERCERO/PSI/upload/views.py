# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.shortcuts import render, redirect
from data.models import Category, WorkFlow
from upload.forms import UploadWorkflow

# Create your views here.

# Esta funcion se encarga de la vista para subir un workflow nuevo a la aplicacion
def add_workflow(request):

	_dict = {
		'workflow_form' : None,
		'error': None
	}

	_dict['workflow_form'] = UploadWorkflow()

	if request.method == 'POST':
		workflow_form = UploadWorkflow(request.POST, request.FILES)
		name = request.POST.get('name')
		workflow = WorkFlow.objects.filter(name = name)

		if workflow:
			_dict['error'] = "Ya existe un workflow con ese nombre"
			return render(request, 'upload/add_workflow.html', _dict)
		else:
			if workflow_form.is_valid():
				workflowFile = workflow_form.cleaned_data['json']
				name = workflowFile.name
				if name.endswith('.json'):
					file_data = workflowFile.read().decode('utf-8')
					workflow_form.instance.json = file_data	 
					if request.session.session_key:
						workflow_form.instance.delete_id = request.session.session_key
					else:
						request.session.cycle_key()
						request.session.set_expiry(600)
						workflow_form.instance.delete_id = request.session.session_key
					workflow = workflow_form.save(commit=True)
					return redirect('find:workflow_detail', workflow_form.instance.id, workflow_form.instance.slug)
				else:
					_dict['error'] = "El fichero que se ha subido no es del tipo json"
					return render(request, 'upload/add_workflow.html', _dict)
			else:	
				_dict['error'] = workflow_form.errors
				return render(request, 'upload/add_workflow.html', _dict)

	return render(request, 'upload/add_workflow.html', _dict)