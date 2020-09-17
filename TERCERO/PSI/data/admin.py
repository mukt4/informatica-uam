# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.contrib import admin
from data.models import Category, WorkFlow

# Clase que define la pantalla del modelo Category en la ventana admin
class CategoryAdmin(admin.ModelAdmin):
	# Elementos que se mostraran
	list_display = ('name', 'slug')
	# Elementos que se poblaran automaticamente
	prepopulated_fields = {'slug':('name',)}

# Clase que define la pantalla del modelo WorkFlow en la ventana admin
class WorkFlowAdmin(admin.ModelAdmin):
	# Elementos que se mostraran
	list_display = ('name', 'slug', 'views', 'downloads', 'client_ip', 'created')
	# Elementos que se poblaran automaticamente
	prepopulated_fields = {'slug':('name',)}

admin.site.register(Category, CategoryAdmin)
admin.site.register(WorkFlow, WorkFlowAdmin)
