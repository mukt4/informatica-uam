from unittest import TestCase
from django.test import Client
from data.models import Category, WorkFlow
from data.management.commands.populate import Command, CATEGORY, \
    WORKFLOW
from django.urls import reverse
from django.db.models import Q
import sys

#execute: python manage.py test find.tests.FindTests.testNAME

#very basis model testing, we just check the objects exists
# returns all  workflows
WORKFLOW_LIST = 'find:workflow_list'
# list of workflows related to a given category
WORKFLOW_LIST_BY_CARTEGORY = 'find:workflow_list_by_category'
# full information related with a workflow
WORKFLOW_DETAIL = 'find:workflow_detail'
# Dowload json with workflow
WORKFLOW_DOWNLOAD = 'find:workflow_download'
# get workflow but do not
# increment download counter (it is used by workflow viewer -web component)
WORKFLOW_DOWNLOAD_NO_COUNT = 'find:workflow_download_no_count'
# search for workflows vy name or keyword
WORKFLOW_SEARCH = 'find:workflow_search'
# Keywords
KEYWORDS = 'KeyWords'
# DEscription
DESCRIPTION = 'Description'
#DO NOT MODIFIED ANYTHING BELLOW THIS POINT
class FindTests(TestCase):
    def setUp(self):
        # The test client is a Python class that acts as a dummy Web browser
        self._client   = Client()

        # populate database
        self.populate = Command()
        self.populate.handle()


    def test_0_workflow_list(self): #CHECK
        #firts check all workflows
        this_function_name = sys._getframe().f_code.co_name
        print ("executing: %s" % this_function_name)
        # WORKFLOW_LIST returns all  workflows
        # If you set follow = True the client will follow any redirects
        response = self._client.get(reverse(WORKFLOW_LIST),
                                    follow=True)

        # Check that all categories and workflow are in the returned page
        workflows = WorkFlow.objects.all()
        self.assertEqual(13, len(workflows))

        # if pagination implemented fill free to use
        # for workflow in workflows[:10]:
        for workflow in workflows:
            self.assertIn(workflow.name, str(response.content))
            print ("    assert: %s"%workflow.name)

    def test_1_workflow_category(self):
        # firts check all workflows related with a category
        print "executing: ", sys._getframe().f_code.co_name
        categories = Category.objects.all()
        category = categories[0]
        response = self._client.get(reverse(WORKFLOW_LIST_BY_CARTEGORY,
                                            kwargs={'category_slug':category.slug}))

        workflows = WorkFlow.objects.filter(category=category)
        for itemName in workflows:
            self.assertIn("%s</a>"%itemName.name,str(response.content))
            print("    assert: %s in %s" % (itemName.name, category.name))

        # django Q objects have being designed to make complex queries
        # in this case search for objects NOT equal to a given category
        workflows = WorkFlow.objects.filter(~Q(category=category))
        for itemName in workflows:
            self.assertNotIn("%s</a>"%itemName.name,str(response.content))
            print("    assert: %s NOT in %s" % (itemName.name, category.name))

    def test_2_workflow_detail(self):
        print "executing: ", sys._getframe().f_code.co_name
        # getworkflow detail
        baseName = WORKFLOW
        workflows = WorkFlow.objects.all()
        for workflow in workflows:
            response = self._client.get(reverse(WORKFLOW_DETAIL,
                                                kwargs={'id':workflow.id,
                                                        'slug':workflow.slug}))
            self.assertIn(KEYWORDS,str(response.content))
            self.assertIn(DESCRIPTION,str(response.content))
            self.assertIn(workflow.name,str(response.content))
            self.assertIn(workflow.description, response.content.decode("utf-8"))
            self.assertTrue(len(workflow.description) > 16 )
            print ("Assert workflow %s" % workflow)

    def test_3_workflow_download(self):
        #THIS TEST IS FOR THE 4TH ASSIGNMENT
        print "executing: ", sys._getframe().f_code.co_name
        workflows = WorkFlow.objects.all()
        workflow = workflows[0]
        # download workflow & get workflow again
        # downloads should be 1 after download
        response = self._client.get(reverse(WORKFLOW_DOWNLOAD,
                                            kwargs={'id' : workflow.id,
                                                    'slug' : workflow.slug}))

        self.assertIn(self.populate.getJson(), response.content.decode("utf-8"))

    def test_4_workflow_search(self):
        #search for a workflow workflow_search/
        print "executing: ", sys._getframe().f_code.co_name
        workflows = WorkFlow.objects.all()
        for workflow in workflows:
            response = self._client.post(reverse(WORKFLOW_SEARCH),
                                                {
                                                 'key': workflow.slug})
            print ("Search for workflow %s" % workflow.slug)
            self.assertIn(workflow.name,str(response.content))
            self.assertIn(workflow.description,
                          response.content.decode("utf-8"))
