# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models
from django.template.defaultfilters import slugify
from django.utils import timezone

# Modelo que define un objeto de tipo Category
class Category(models.Model):
	name = models.CharField(max_length = 128, unique = True, blank = False)
	slug = models.SlugField(unique = True)
	created = models.DateField(default=timezone.now)
	tooltip = models.CharField(max_length = 512, default = "")

	# Modificacion de la funcion save para hacer uso de la funcion slugify
	def save(self, *args, **kwargs):
		self.slug = slugify(self.name)
		super(Category, self).save(*args, **kwargs)

	class Meta:
		verbose_name_plural = 'Categories'

	def __str__(self):
		return self.name

# Modelo que define un objeto de tipo WorkFlow
class WorkFlow(models.Model):
	name = models.CharField(max_length = 128, unique = True, blank = False)
	slug = models.SlugField(unique = True)
	description = models.CharField(max_length = 512, default = "")
	views = models.IntegerField(default = 0)
	downloads = models.IntegerField(default = 0)
	versionInit = models.CharField(max_length = 128)
	category = models.ManyToManyField(Category)
	client_ip = models.GenericIPAddressField(default = "1.1.1.1")
	keywords = models.CharField(max_length = 256, default = "")
	json = models.CharField(max_length = 10000, default = "")
	created = models.DateField(default=timezone.now)
	delete_id = models.CharField(default = "-1", max_length = 128, null = True)

	# Modificacion de la funcion save para hacer uso de la funcion slugify
	def save(self, *args, **kwargs):
		self.slug = slugify(self.name)
		super(WorkFlow, self).save(*args, **kwargs)

	def __str__(self):
		return self.name

