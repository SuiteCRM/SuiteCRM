<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class AOD_IndexTest extends SuitePHPUnitFrameworkTestCase
{
    public function testAOD_Index(): void
    {
        // Execute the constructor and check for the Object type and type attribute
        $aod_index = BeanFactory::newBean('AOD_Index');
        self::assertInstanceOf('AOD_Index', $aod_index);
        self::assertInstanceOf('Basic', $aod_index);
        self::assertInstanceOf('SugarBean', $aod_index);

        self::assertEquals('AOD_Index', $aod_index->module_dir);
        self::assertEquals('AOD_Index', $aod_index->object_name);
        self::assertEquals('aod_index', $aod_index->table_name);
        self::assertEquals(true, $aod_index->new_schema);
        self::assertEquals(true, $aod_index->disable_row_level_security);
        self::assertEquals(false, $aod_index->importable);
        self::assertEquals(false, $aod_index->tracker_visibility);
    }

    public function testisEnabled(): void
    {
        // execute the method and verify that it returns true
        $result = BeanFactory::newBean('AOD_Index')->isEnabled();
        self::assertTrue($result);
    }

    public function testgetIndex(): void
    {
        $aod_index = BeanFactory::newBean('AOD_Index');
        $result = $aod_index->getIndex();

        //execute the method and verify it returns a different instance of samme type
        self::assertInstanceOf('AOD_Index', $result);
        self::assertNotSame($aod_index, $result);
    }

    public function testgetDocumentForBean(): void
    {
        $user = new User(1);

        $result = BeanFactory::newBean('AOD_Index')->getDocumentForBean($user);

        //execute the method and verify that it returns an array
        self::assertIsArray($result);

        //verify that returned array has a valid Zend_Search_Lucene_Document instance
        self::assertInstanceOf('Zend_Search_Lucene_Document', $result['document']);
    }

    public function testisModuleSearchable(): void
    {
        //test with an invalid module
        self::assertFalse(AOD_Index::isModuleSearchable('', ''));

        //test for modules that are searchable
        self::assertTrue(AOD_Index::isModuleSearchable('DocumentRevisions', 'DocumentRevision'));
        self::assertTrue(AOD_Index::isModuleSearchable('Cases', 'Case'));
        self::assertTrue(AOD_Index::isModuleSearchable('Accounts', 'Account'));

        //test for modules that are not searchable
        self::assertFalse(AOD_Index::isModuleSearchable('AOD_IndexEvent', 'AOD_IndexEvent'));
        self::assertFalse(AOD_Index::isModuleSearchable('AOD_Index', 'AOD_Index'));
        self::assertFalse(AOD_Index::isModuleSearchable('AOW_Actions', 'AOW_Action'));
        self::assertFalse(AOD_Index::isModuleSearchable('AOW_Conditions', 'AOW_Condition'));
        self::assertFalse(AOD_Index::isModuleSearchable('AOW_Processed', 'AOW_Processed'));
        self::assertFalse(AOD_Index::isModuleSearchable('SchedulersJobs', 'SchedulersJob'));
        self::assertFalse(AOD_Index::isModuleSearchable('Users', 'User'));
    }

    public function testindex(): void
    {
        $aod_index = BeanFactory::newBean('AOD_Index');
        $aod_index->id = 1;
        $aod_index->location = 'modules/AOD_Index/Index/Index';

        //test with a not searchable module, it will return false
        $result = $aod_index->index('Users', 1);
        self::assertFalse($result);

        //test with a searchable module but invalid bean id, it will still index it
        $result = $aod_index->index('Accounts', 1);
        self::assertEquals(null, $result);
    }

    public function testgetIndexableModules(): void
    {
        $expected = array(
            'AM_ProjectTemplates' => 'AM_ProjectTemplates',
            'AM_TaskTemplates' => 'AM_TaskTemplates',
            'AOK_KnowledgeBase' => 'AOK_KnowledgeBase',
            'AOK_Knowledge_Base_Categories' => 'AOK_Knowledge_Base_Categories',
            'AOP_Case_Events' => 'AOP_Case_Events',
            'AOP_Case_Updates' => 'AOP_Case_Updates',
            'AOR_Charts' => 'AOR_Chart',
            'AOR_Conditions' => 'AOR_Condition',
            'AOR_Fields' => 'AOR_Field',
            'AOR_Reports' => 'AOR_Report',
            'AOS_Contracts' => 'AOS_Contracts',
            'AOS_Product_Categories' => 'AOS_Product_Categories',
            'AOW_WorkFlow' => 'AOW_WorkFlow',
            'Accounts' => 'Account',
            'Bugs' => 'Bug',
            'Calls' => 'Call',
            'Calls_Reschedule' => 'Calls_Reschedule',
            'Campaigns' => 'Campaign',
            'Cases' => 'aCase',
            'Contacts' => 'Contact',
            'DocumentRevisions' => 'DocumentRevision',
            'Documents' => 'Document',
            'FP_events' => 'FP_events',
            'Leads' => 'Lead',
            'Meetings' => 'Meeting',
            'Notes' => 'Note',
            'Opportunities' => 'Opportunity',
            'Project' => 'Project',
            'ProjectTask' => 'ProjectTask',
            'ProspectLists' => 'ProspectList',
            'Prospects' => 'Prospect',
            'SurveyQuestionOptions' => 'SurveyQuestionOptions',
            'SurveyQuestionResponses' => 'SurveyQuestionResponses',
            'SurveyQuestions' => 'SurveyQuestions',
            'SurveyResponses' => 'SurveyResponses',
            'Surveys' => 'Surveys',
            'Tasks' => 'Task'
        );

        //execute the method and verify that it retunrs expected results
        $actual = BeanFactory::newBean('AOD_Index')->getIndexableModules();
        foreach ($expected as $key => $expect){
            self::assertEquals($expect, $actual[$key]);
        }
    }
}
