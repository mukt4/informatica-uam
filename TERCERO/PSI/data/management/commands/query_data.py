#populate database
# This code has to be placed in a file within the
# data/management/commands directory in your project.
# If that directory doesn't exist, create it.
# The name of the script is the name of the custom command,
# so let's call it populate.py. Another thing that has to be done
# is creating __init__.py files in both the management and commands
# directories, because these have to be Python packages.
#
# execute python manage.py  populate


from django.core.management.base import BaseCommand
from django.contrib.auth.models import User
from data.models import Category, WorkFlow

import django
import random

django.setup()
#models
CATEGORY = 'category'
USER = 'user'
WORKFLOW = 'workflow'
# The name of this class is not optional must  be Command
# otherwise manage.py will not process it properly
class Command(BaseCommand):
    #  args = '<-no arguments>'
    # helps and arguments shown when command python manage.py help populate
    # is executed.
    help = 'This scripts populates de workflow database, no arguments needed.' \
           'Execute it with the command line python manage.py populate'

    def getParragraph(self, init, end):
        # getParragraph returns a parragraph, useful for testing
        if end > 445:
            end = 445
        if init < 0:
            init = 0
        return """Lorem ipsum dolor sit amet, consectetur adipiscing elit,
sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris
nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in
reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia
deserunt mollit anim id est laborum."""[init:end]

    # handle is another compulsory name, This function will be
    # executed by default
    def handle(self, *args, **options):
            data = self.query_data()

    def query_data(self):
        categorias = Category.objects.all()
        # Comprobamos sobre el slug para no tener problemas si los nombres tiene mayusculas y minusculas
        comprobacion = Category.objects.filter(slug = "category-1")
        if comprobacion:
            print("La categoria 'category 1' ya existe por lo que no se crea")
        else:
            c1 = Category.objects.get_or_create(name = 'category 1')[0]
            c1.tooltip = "Esta es la categoria 1"
            c1.save()
            print("Se ha creado la categoria 'category 1'")
        
        # Creacion de category 2
        comprobacion = Category.objects.filter(slug = "category-2")
        if comprobacion:
            print("La categoria 'category 2' ya existe por lo que no se crea")
        else:
            c2 = Category.objects.get_or_create(name = 'category 2')[0]
            c2.tooltip = "Esta es la categoria 2"
            c2.save()
            print("Se ha creado la categoria 'category 2'")

        # Creacion de workflow 11
        comprobacion = WorkFlow.objects.filter(slug = "workflow-11")
        if comprobacion:
            print("El workflow 'workflow 11' ya existe por lo que no se crea")
        else:
            w1 = WorkFlow.objects.get_or_create(name = 'workflow 11')[0]
            w1.category.add(categorias[0])
            w1.versionInit = "1.0"
            w1.client_ip = "1.2.3.4"
            w1.json = self.getJson()
            w1.save()
            print("Se ha creado el workflow 'workflow 11'")

        # Creacion de workflow 12
        comprobacion = WorkFlow.objects.filter(slug = "workflow-12")
        if comprobacion:
            print("El workflow 'workflow 12' ya existe por lo que no se crea")
        else:
            w2 = WorkFlow.objects.get_or_create(name = 'workflow 12')[0]
            w2.category.add(categorias[0])
            w2.versionInit = "1.0"
            w2.client_ip = "1.2.3.4"
            w2.json = self.getJson()
            w2.save()
            print("Se ha creado el workflow 'workflow 12'")

        # Creacion de workflow 13
        comprobacion = WorkFlow.objects.filter(slug = "workflow-13")
        if comprobacion:
            print("El workflow 'workflow 13' ya existe por lo que no se crea")
        else:
            w3 = WorkFlow.objects.get_or_create(name = 'workflow 13')[0]
            w3.category.add(categorias[0])
            w3.versionInit = "1.0"
            w3.client_ip = "1.2.3.4"
            w3.json = self.getJson()
            w3.save()
            print("Se ha creado el workflow 'workflow 13'")

        # Creacion de workflow 21
        comprobacion = WorkFlow.objects.filter(slug = "workflow-21")
        if comprobacion:
            print("El workflow 'workflow 21' ya existe por lo que no se crea")
        else:
            w4 = WorkFlow.objects.get_or_create(name = 'workflow 21')[0]
            w4.category.add(categorias[1])
            w4.versionInit = "1.0"
            w4.client_ip = "1.2.3.4"
            w4.json = self.getJson()
            w4.save()
            print("Se ha creado el workflow 'workflow 21'")
        
        # Creacion de workflow 22
        comprobacion = WorkFlow.objects.filter(slug = "workflow-22")
        if comprobacion:
            print("El workflow 'workflow 22' ya existe por lo que no se crea")
        else:
            w5 = WorkFlow.objects.get_or_create(name = 'workflow 22')[0]
            w5.category.add(categorias[1])
            w5.versionInit = "1.0"
            w5.client_ip = "1.2.3.4"
            w5.json = self.getJson()
            w5.save()
            print("Se ha creado el workflow 'workflow 22'")
        
        # Creacion de workflow 23
        comprobacion = WorkFlow.objects.filter(slug = "workflow-23")
        if comprobacion:
            print("El workflow 'workflow 23' ya existe por lo que no se crea")
        else:
            w6 = WorkFlow.objects.get_or_create(name = 'workflow 23')[0]
            w6.category.add(categorias[1])
            w6.versionInit = "1.0"
            w6.client_ip = "1.2.3.4"
            w6.json = self.getJson()
            w6.save()
            print("Se ha creado el workflow 'workflow 23'")

        print("Una vez creados workflows y categorias pasamos a realizar las consultas")

        print("Primera consulta-> Listado de todos los workflows asociados a 'category 1'")

        workflows = WorkFlow.objects.filter(category = categorias[0])
        if workflows:
            for workflow in workflows:
                print("\t- " + workflow.name + " \n")
        else:
            print("\tNo hay ningun workflow asociado a 'category 1'")

        print("Segunda consulta-> Obtener la categoria del workflow con slug 'workflow-11'")

        workflows = WorkFlow.objects.filter(slug = 'workflow-11')
        if workflows:
            for category in workflows[0].category.all():
                print("\tSlug de la categoria: " + category.slug + "\n")
        else:
            print("\tNo hay ningun workflow asociado al slug 'workflow-11'")

        print("Tercera consulta-> Obtener la categoria del workflow con slug 'workflow-10'")

        workflows = WorkFlow.objects.filter(slug = 'workflow-10')
        if workflows:
            for category in workflows[0].category.all():
                print("\tSlug de la categoria: " + category.slug + "\n")
        else:
            print("\tNo hay ningun workflow asociado al slug 'workflow-10'")

    def getJson(self):
        return """[
    {
        "object.className": "ProtImportMovies",
        "object.id": "2",
        "object.label": "import movies",
        "object.comment": "\\n",
        "runName": null,
        "runMode": 0,
        "importFrom": 0,
        "filesPath": "",
        "filesPattern": "Falcon*.mrcs",
        "copyFiles": false,
        "acquisitionWizard": null,
        "voltage": 300.0,
        "sphericalAberration": 2.0,
        "amplitudeContrast": 0.1,
        "magnification": 39548,
        "samplingRateMode": 0,
        "samplingRate": 3.54,
        "scannedPixelSize": 14.0,
        "gainFile": null
    },
    {
        "object.className": "ProtMovieAlignment",
        "object.id": "40",
        "object.label": "movie alignment",
        "object.comment": "\\n",
        "runName": null,
        "runMode": 0,
        "cleanMovieData": true,
        "alignMethod": 0,
        "alignFrame0": 0,
        "alignFrameN": 0,
        "doGPU": false,
        "GPUCore": 0,
        "winSize": 150,
        "sumFrame0": 0,
        "sumFrameN": 0,
        "cropOffsetX": 0,
        "cropOffsetY": 0,
        "cropDimX": 0,
        "cropDimY": 0,
        "binFactor": 1,
        "extraParams": "",
        "hostName": "localhost",
        "numberOfThreads": 4,
        "numberOfMpi": 1,
        "inputMovies": "2.__attribute__outputMovies"
    },
    {
        "object.className": "ProtCTFFind",
        "object.id": "82",
        "object.label": "ctffind4",
        "object.comment": "\\n",
        "runName": null,
        "runMode": 0,
        "recalculate": false,
        "sqliteFile": null,
        "ctfDownFactor": 1.0,
        "useCftfind4": true,
        "astigmatism": 100.0,
        "findPhaseShift": false,
        "lowRes": 0.05,
        "highRes": 0.35,
        "minDefocus": 0.5,
        "maxDefocus": 4.0,
        "windowSize": 256,
        "hostName": "localhost",
        "numberOfThreads": 4,
        "numberOfMpi": 1,
        "inputMicrographs": "40.__attribute__outputMicrographs"
    },
    {
        "object.className": "EmanProtBoxing",
        "object.id": "369",
        "object.label": "eman2 - boxer",
        "object.comment": "",
        "runName": null,
        "runMode": 0,
        "inputMicrographs": "40.__attribute__outputMicrographs"
    },
    {
        "object.className": "ProtUserSubSet",
        "object.id": "380",
        "object.label": "3mics",
        "object.comment": "",
        "runName": null,
        "runMode": 0,
        "other": null,
        "sqliteFile": "Runs/000082_ProtCTFFind/ctfs_selection.sqlite,",
        "outputClassName": "SetOfMicrographs",
        "inputObject": "82.__attribute__outputCTF"
    },
    {
        "object.className": "XmippProtParticlePicking",
        "object.id": "420",
        "object.label": "xmipp3 - manual picking",
        "object.comment": "",
        "runName": null,
        "runMode": 0,
        "memory": 2.0,
        "inputMicrographs": "40.__attribute__outputMicrographs"
    },
    {
        "object.className": "XmippProtExtractParticles",
        "object.id": "449",
        "object.label": "extract 3 mics",
        "object.comment": "\\n",
        "runName": null,
        "runMode": 0,
        "micsSource": 0,
        "boxSize": 64,
        "doSort": false,
        "rejectionMethod": 0,
        "maxZscore": 3,
        "percentage": 5,
        "doRemoveDust": true,
        "thresholdDust": 3.5,
        "doInvert": true,
        "doFlip": false,
        "doNormalize": true,
        "normType": 2,
        "backRadius": -1,
        "hostName": "localhost",
        "numberOfThreads": 1,
        "numberOfMpi": 1,
        "ctfRelations": "82.__attribute__outputCTF",
        "inputCoordinates": "123.__attribute__outputCoordinates",
        "inputMicrographs": "369.outputMicrographs"
    },
    {
        "object.className": "XmippParticlePickingAutomatic",
        "object.id": "517",
        "object.label": "xmipp3 - auto-picking",
        "object.comment": "",
        "runName": null,
        "runMode": 0,
        "micsToPick": 0,
        "memory": 2.0,
        "hostName": "localhost",
        "numberOfThreads": 1,
        "numberOfMpi": 1,
        "xmippParticlePicking": "420"
    }
]"""

#There's no need to bypass manage.py, since it's a wonderful convenience wrapper around
        # the Django project administration tools. It can be used to create custom
        # management commands - e.g. your own commands parallel to shell, dumpdata,
        # and so on. Not only that creating such commands gives you a very succinct,
        # boilterplate-free way of writing custom management scripts, it also gives
        # you a natural location to house them, per application.


