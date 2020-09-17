from django import forms
from data.models import WorkFlow

# Clase que define el form de busqueda
class SearchForm(forms.ModelForm):
	key = forms.CharField(max_length=128, help_text="Por favor introduce el slug o el nombre del workflow")

	class Meta:
		model = WorkFlow
		fields = ('key',)
