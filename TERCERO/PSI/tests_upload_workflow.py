# -*- coding: utf-8 -*-
#NOTE, if selenium is not installed execute pip install --user selenium
#NOTE: chromedriver is available at moodle. For labs use version 2.22
#NOTE; execute with command python ./test_upload_workflow.py
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
from data.management.commands.populate import Command, CATEGORY
from data.models import Category

ID_CATEGORY = "id_category"
ID_DESCRIPTION = "id_description"
ID_KEYWORDS = "id_keywords"
ID_NAME = "id_name"
ID_VERSION = "id_versionInit"
LABEL_UPLOAD_WORFLOW = "Upload workflow"
LABEL_SUMIT_WORKFLOW = "submit_workflow"
dirname_base = os.path.dirname(__file__)
CHROMEDRIVER = os.path.join(dirname_base, "chromedriver")
WAITFOR = 01
BASE_LOCAL_URL = "http://127.0.0.1:8000/"
BASE_REMOTE_URL = "https://practica1-tom.herokuapp.com/"
BASE_URL = BASE_REMOTE_URL

#DO NOT MODIFIED THE CODE BELOW THIS POINT

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
        #data = populate.getData()
        self.CATEGORYNAME = (Category.objects.all()[0]).name 

    def seeHome(self, waitFor=0):
        """ go to home page"""
        print ("seeHome")
        self.driver.get(self.base_url)
        time.sleep(waitFor)

    def uploadWorkflow(self, workflowName, waitFor):
        self.find_element_by_link_text(LABEL_UPLOAD_WORFLOW, waitFor)# click on Upload workflow
        #fill form
        self.find_element_by_id(ID_NAME,"workflow_aaa")
        self.find_element_by_id(ID_KEYWORDS,"key1, key2, key3")
        populate = Command()
        self.find_element_by_id(ID_DESCRIPTION,populate.getParragraph(20,400))
        self.find_element_by_id(ID_VERSION,"1.2")
        self.find_select_element_by_id(ID_CATEGORY, workflowName)

        f=open("/tmp/workflow.json","w")
        f.write(populate.getJson())
        f.close()
        element = self.driver.find_element_by_id("id_json")
        element.send_keys("/tmp/workflow.json")

        self.find_element_by_name(LABEL_SUMIT_WORKFLOW)


    def test_workflow(self):
        # execute populate, although is
        self.populateDataBase()
        # connect to Home
        self.seeHome(WAITFOR)
        #select upload Workflow //*[@id="sidebar"]/h3[2]/a
        self.uploadWorkflow(self.CATEGORYNAME, WAITFOR)
        time.sleep(30)

if __name__ == "__main__":
    unittest.main()
