# -*- coding: utf-8 -*-
#NOTE, if selenium is not installed execute pip install --user selenium
#NOTE: chromedriver is available at moodle. For labs use version 2.22
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import Select
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import NoAlertPresentException
import unittest, time, os
from collections import OrderedDict

import os, sys, inspect

currentdir = os.path.dirname(os.path.abspath(inspect.getfile(inspect.currentframe())))
parentdir = os.path.dirname(currentdir)
sys.path.append(parentdir)
os.environ.setdefault("DJANGO_SETTINGS_MODULE", "workflowrepository.settings")
import django
django.setup()
from data.management.commands.populate import Command
from data.models import Category, WorkFlow

ID_CATEGORY = "id_category"
ID_DESCRIPTION = "id_description"
ID_KEYWORDS = "id_keywords"
ID_NAME = "id_name"
ID_VERSION = "id_versionInit"
SEARCH_TEXTBOX_NAME='key'
SEARCH_BUTTOM_NAME='byName'
LABEL_UPLOAD_WORFLOW = "Upload workflow"
dirname_base = os.path.dirname(__file__)
CHROMEDRIVER = os.path.join(dirname_base, "chromedriver")
WAITFOR = 1
BASE_LOCAL_URL = "http://127.0.0.1:8000/"
BASE_REMOTE_URL = "https://practica1-tom.herokuapp.com/"
BASE_URL = BASE_REMOTE_URL
#DO NOT MODIFIED THE CODE BELLOW THIS POINT

class workflowTester(unittest.TestCase):

    chromeDriver = CHROMEDRIVER

    def setUp(self):
        print ("Setup")
#        self.driver = webdriver.Firefox()
        self.driver = webdriver.Chrome(self.chromeDriver)
        self.base_url = BASE_URL 

    def find_element_by_id(self,_id,value,waitFor=WAITFOR):
        self.driver.find_element_by_id(_id).clear()
        self.driver.find_element_by_id(_id).send_keys(value)
        time.sleep(waitFor)

    def find_element_by_xpath(self,_xpath,waitFor=WAITFOR):
        self.driver.find_element_by_xpath(_xpath).click()
        time.sleep(waitFor)

    def find_element_by_name(self,_name,waitFor=WAITFOR):
        self.driver.find_element_by_name(_name).click()
        time.sleep(waitFor)

    def find_element_by_name_value(self,_name, value, waitFor=WAITFOR):
        self.driver.find_element_by_name(_name).clear()
        self.driver.find_element_by_name(_name).send_keys(value)
        time.sleep(waitFor)

    def find_element_by_link_text(self,_name, waitFor=WAITFOR):
        self.driver.find_element_by_link_text(_name).click()
        time.sleep(waitFor)

    def find_element_by_partial_link_text(self,_name, waitFor=WAITFOR):
        self.driver.find_element_by_partial_link_text(_name).click()
        time.sleep(waitFor)

    def find_select_element_by_id(self, _id, workflowName, waitFor=WAITFOR):
        selected = self.driver.find_element_by_id(_id)
        select = Select(selected)
        select.select_by_visible_text(workflowName)


    def populateDataBase(self):
        populate = Command()
        populate.handle()
        categories = Category.objects.all()
        self.CATEGORYNAME1 = categories[0].name
        self.CATEGORYNAME2 = categories[1].name

        workflows = WorkFlow.objects.all()
        self.workflow1 = workflows[0]
        self.categoryWorkflow1 = \
            Category.objects.filter(workflow=self.workflow1)[0]

    def seeHome(self, waitFor=0):
        """ go to home page"""
        print ("seeHome")
        self.driver.get(self.base_url)
        time.sleep(waitFor)

    def selectCategory(self, linktext, waitFor=0):
        print "connect to link containing: %s" % linktext
        self.find_element_by_partial_link_text(linktext)
        time.sleep(waitFor)

    def selectWorkflow(self, linktext, waitFor=0):
        print "connect to link containing: %s" % linktext
        self.find_element_by_partial_link_text(linktext)
        time.sleep(waitFor)

    def searchWorkflow(self, nameTextBox, nameButtom, value, waitFor=0):
        print "Search for workflow %s" % value
        self.find_element_by_name_value(nameTextBox, value, waitFor)
        self.find_element_by_name(nameButtom, waitFor)
        time.sleep(waitFor)

    def test_repository(self):
        # execute populate, although is
        self.populateDataBase()
        # connect to Home
        self.seeHome(WAITFOR)
        # select first category
        self.selectCategory(self.CATEGORYNAME1)
        # select second category
        self.selectCategory(self.CATEGORYNAME2)
        # back to workflow category
        self.selectWorkflow(self.categoryWorkflow1.name)
        # search
        self.seeHome(WAITFOR)
        self.searchWorkflow(SEARCH_TEXTBOX_NAME,
                            SEARCH_BUTTOM_NAME,
                            self.workflow1.name)

        #select upload Workflow //*[@id="sidebar"]/h3[2]/a
        #self.uploadWorkflow(self.CATEGORYNAME, WAITFOR)
        time.sleep(30)

if __name__ == "__main__":
    unittest.main()
