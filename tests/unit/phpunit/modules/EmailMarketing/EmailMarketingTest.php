<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class EmailMarketingTest extends SuitePHPUnitFrameworkTestCase
{
    public function testEmailMarketing(): void
    {
        // execute the constructor and check for the Object type and attributes
        $emailMarketing = BeanFactory::newBean('EmailMarketing');

        self::assertInstanceOf('EmailMarketing', $emailMarketing);
        self::assertInstanceOf('SugarBean', $emailMarketing);

        self::assertEquals('EmailMarketing', $emailMarketing->module_dir);
        self::assertEquals('EmailMarketing', $emailMarketing->object_name);
        self::assertEquals('email_marketing', $emailMarketing->table_name);
        self::assertEquals(true, $emailMarketing->new_schema);
    }

    public function testretrieve(): void
    {
        $result = BeanFactory::newBean('EmailMarketing')->retrieve();
        self::assertInstanceOf('EmailMarketing', $result);
    }

    public function testget_summary_text(): void
    {
        $emailMarketing = BeanFactory::newBean('EmailMarketing');

        // test without setting name
        self::assertEquals(null, $emailMarketing->get_summary_text());

        // test with name set
        $emailMarketing->name = 'test';
        self::assertEquals('test', $emailMarketing->get_summary_text());
    }

    public function testcreate_export_query(): void
    {
        $emailMarketing = BeanFactory::newBean('EmailMarketing');

        //test with empty string params
        $expected = " SELECT  email_marketing.*  , jt0.name template_name , jt0.assigned_user_id template_name_owner  , 'EmailTemplates' template_name_mod FROM email_marketing   LEFT JOIN  email_templates jt0 ON email_marketing.template_id=jt0.id AND jt0.deleted=0\n\n AND jt0.deleted=0 where email_marketing.deleted=0";
        $actual = $emailMarketing->create_export_query('', '');
        self::assertSame($expected, $actual);

        //test with valid string params
        $expected = " SELECT  email_marketing.*  , jt0.name template_name , jt0.assigned_user_id template_name_owner  , 'EmailTemplates' template_name_mod FROM email_marketing   LEFT JOIN  email_templates jt0 ON email_marketing.template_id=jt0.id AND jt0.deleted=0\n\n AND jt0.deleted=0 where (email_marketing.name=\"\") AND email_marketing.deleted=0";
        $actual = $emailMarketing->create_export_query('email_marketing.id', 'email_marketing.name=""');
        self::assertSame($expected, $actual);
    }

    public function testget_list_view_data(): void
    {
        $emailMarketing = BeanFactory::newBean('EmailMarketing');

        //execute the method and verify that it retunrs expected results
        $expected = array(
                'ALL_PROSPECT_LISTS' => '0',
        );

        $actual = $emailMarketing->get_list_view_data();
        self::assertSame($expected, $actual);
    }

    public function testbean_implements(): void
    {
        $emailMarketing = BeanFactory::newBean('EmailMarketing');
        self::assertEquals(false, $emailMarketing->bean_implements('')); //test with blank value
        self::assertEquals(false, $emailMarketing->bean_implements('test')); //test with invalid value
        self::assertEquals(true, $emailMarketing->bean_implements('ACL')); //test with valid value
    }

    public function testget_all_prospect_lists(): void
    {
        $emailMarketing = BeanFactory::newBean('EmailMarketing');

        //execute the method and verify that it retunrs expected results
        $expected = "select prospect_lists.* from prospect_lists  left join prospect_list_campaigns on prospect_list_campaigns.prospect_list_id=prospect_lists.id where prospect_list_campaigns.deleted=0 and prospect_list_campaigns.campaign_id='' and prospect_lists.deleted=0 and prospect_lists.list_type not like 'exempt%'";
        $actual = $emailMarketing->get_all_prospect_lists();
        self::assertSame($expected, $actual);
    }
}
