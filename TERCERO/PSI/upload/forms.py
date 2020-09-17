from django import forms
from data.models import WorkFlow, Category

class UploadWorkflow(forms.ModelForm):

	name = forms.CharField(label = 'Name')
	category = forms.ModelMultipleChoiceField(label = 'Category', queryset = Category.objects.all(), required = False)
	keywords = forms.CharField(label = 'Keywords', max_length = 256)
	description = forms.CharField(label = 'Description', max_length = 512)
	versionInit = forms.CharField(label = 'Version', max_length = 128)
	json = forms.FileField(label = 'Upload JSON')
    
	class Meta:
		model = WorkFlow
		fields = ('name', 'keywords', 'description', 'category', 'versionInit', 'json',)