<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2021 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */

namespace SuiteCRM\Tests\Unit\modules\Cases;

use BeanFactory;
use Exception;
use Sugar_Smarty;
use SugarThemeRegistry;
use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

/**
 * Class aCaseTest
 * @package SuiteCRM\Tests\Unit\modules\Cases
 */
class aCaseTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testaCase(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $aCase = BeanFactory::newBean('Cases');
        self::assertInstanceOf('aCase', $aCase);
        self::assertInstanceOf('Basic', $aCase);
        self::assertInstanceOf('SugarBean', $aCase);

        self::assertEquals('Cases', $aCase->module_dir);
        self::assertEquals('Case', $aCase->object_name);
        self::assertEquals('cases', $aCase->table_name);
        self::assertEquals('accounts_cases', $aCase->rel_account_table);
        self::assertEquals('contacts_cases', $aCase->rel_contact_table);
        self::assertEquals(true, $aCase->importable);
        self::assertEquals(true, $aCase->new_schema);
    }

    public function testget_summary_text(): void
    {
        $aCase = BeanFactory::newBean('Cases');
        self::assertEquals(null, $aCase->get_summary_text());

        $aCase->name = 'test';
        self::assertEquals('test', $aCase->get_summary_text());
    }

    public function testlistviewACLHelper(): void
    {
        self::markTestIncomplete('environment dependency');


        $aCase = BeanFactory::newBean('Cases');
        $expected = array('MAIN' => 'span', 'ACCOUNT' => 'span');
        $actual = $aCase->listviewACLHelper();
        self::assertSame($expected, $actual);
    }

    public function testsave_relationship_changes(): void
    {
        $aCase = BeanFactory::newBean('Cases');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $aCase->save_relationship_changes(true);
            $aCase->save_relationship_changes(false);

            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testset_case_contact_relationship(): void
    {
        $aCase = BeanFactory::newBean('Cases');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $aCase->set_case_contact_relationship(1);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testfill_in_additional_list_fields(): void
    {
        $aCase = BeanFactory::newBean('Cases');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $aCase->fill_in_additional_list_fields();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testfill_in_additional_detail_fields(): void
    {
        $aCase = BeanFactory::newBean('Cases');
        $aCase->assigned_user_id = 1;
        $aCase->created_by = 1;
        $aCase->modified_user_id = 1;

        $aCase->fill_in_additional_detail_fields();

        self::assertEquals('Administrator', $aCase->assigned_user_name);
        self::assertEquals('Administrator', $aCase->created_by_name);
        self::assertEquals('Administrator', $aCase->modified_by_name);
    }

    public function testget_contacts(): void
    {
        $result = BeanFactory::newBean('Cases')->get_contacts();
        self::assertIsNotArray($result);
        self::assertEquals(false, $result);
    }

    public function testget_list_view_data(): void
    {
        $aCase = BeanFactory::newBean('Cases');
        $current_theme = SugarThemeRegistry::current();
        //test without setting attributes
        $expected = array(
                'DELETED' => 0,
                'STATE' => 'Open',
                'UPDATE_TEXT' => '',
                'NAME' => '<em>blank</em>',
                'PRIORITY' => '',
                'STATUS' => '',
                'ENCODED_NAME' => null,
                'CASE_NUMBER' => null,
                'SET_COMPLETE' => '~'
                                .preg_quote('<a href=\'index.php?return_module=Home&return_action=index&action=EditView&module=Cases&record=&status=Closed\'><img src="themes/'.$current_theme.'/images/close_inline.png?v=')
                                .'[\w-]+'
                                .preg_quote('"    title=Close border=\'0\' alt="Close" /></a>')
                                .'~',
        );

        $actual = $aCase->get_list_view_data();
        //$this->assertSame($expected ,$actual);
        self::assertEquals($expected['STATE'], $actual['STATE']);
        self::assertEquals($expected['UPDATE_TEXT'], $actual['UPDATE_TEXT']);
        self::assertEquals($expected['NAME'], $actual['NAME']);
        self::assertEquals($expected['PRIORITY'], $actual['PRIORITY']);
        self::assertMatchesRegularExpression($expected['SET_COMPLETE'], $actual['SET_COMPLETE']);

        //test with attributes preset
        $aCase->name = 'test';
        $aCase->priority = 'P1';
        $aCase->status = 'Open_New';
        $aCase->case_number = 1;

        $expected = array(
                'NAME' => 'test',
                'DELETED' => 0,
                'CASE_NUMBER' => 1,
                'STATUS' => 'New',
                'PRIORITY' => 'High',
                'STATE' => 'Open',
                'UPDATE_TEXT' => '',
                'ENCODED_NAME' => 'test',
                'SET_COMPLETE' => '<a href=\'index.php?return_module=Home&return_action=index&action=EditView&module=Cases&record=&status=Closed\'><img src="themes/'.$current_theme.'/images/close_inline.png?v=fqXdFZ_r6FC1K7P_Fy3mVw"    title=Close border=\'0\' alt="Close" /></a>',
        );

        $actual = $aCase->get_list_view_data();
        //$this->assertSame($expected ,$actual);
        self::assertEquals($expected['NAME'], $actual['NAME']);
        self::assertEquals($expected['CASE_NUMBER'], $actual['CASE_NUMBER']);
        self::assertEquals($expected['STATUS'], $actual['STATUS']);
        self::assertEquals($expected['PRIORITY'], $actual['PRIORITY']);
        self::assertEquals($expected['STATE'], $actual['STATE']);
    }

    public function testbuild_generic_where_clause(): void
    {
        $aCase = BeanFactory::newBean('Cases');

        //test with string
        $expected = "(cases.name like 'test%' or accounts.name like 'test%')";
        $actual = $aCase->build_generic_where_clause('test');
        self::assertSame($expected, $actual);

        //test with number
        $expected = "(cases.name like '1%' or accounts.name like '1%' or cases.case_number like '1%')";
        $actual = $aCase->build_generic_where_clause(1);
        self::assertSame($expected, $actual);
    }

    public function testset_notification_body(): void
    {
        $aCase = BeanFactory::newBean('Cases');

        $aCase->name = 'test';
        $aCase->priority = 'P1';
        $aCase->status = 'Open_New';
        $aCase->description = 'some text';

        $result = $aCase->set_notification_body(new Sugar_Smarty(), $aCase);

        self::assertEquals($aCase->name, $result->tpl_vars['CASE_SUBJECT']->value);
        self::assertEquals('High', $result->tpl_vars['CASE_PRIORITY']->value);
        self::assertEquals('New', $result->tpl_vars['CASE_STATUS']->value);
        self::assertEquals($aCase->description, $result->tpl_vars['CASE_DESCRIPTION']->value);
    }

    public function testbean_implements(): void
    {
        $aCase = BeanFactory::newBean('Cases');
        self::assertEquals(false, $aCase->bean_implements('')); //test with blank value
        self::assertEquals(false, $aCase->bean_implements('test')); //test with invalid value
        self::assertEquals(true, $aCase->bean_implements('ACL')); //test with valid value
    }

    public function testsave(): void
    {
        $aCase = BeanFactory::newBean('Cases');
        $aCase->name = 'test';
        $aCase->priority = 'P1';

        $aCase->save();

        //test for record ID to verify that record is saved
        self::assertTrue(isset($aCase->id));
        self::assertEquals(36, strlen((string) $aCase->id));

        //mark the record as deleted and verify that this record cannot be retrieved anymore.
        $aCase->mark_deleted($aCase->id);
        $result = $aCase->retrieve($aCase->id);
        self::assertEquals(null, $result);
    }

    public function testgetEmailSubjectMacro(): void
    {
        $result = BeanFactory::newBean('Cases')->getEmailSubjectMacro();
        self::assertEquals('[CASE:%1]', $result);
    }

    public function testgetAccount(): void
    {
        $result = BeanFactory::newBean('Cases')->getAccount(1);
        self::assertIsArray($result);
        self::assertEquals(array('account_name' => '', 'account_id' => ''), $result);
    }
}
