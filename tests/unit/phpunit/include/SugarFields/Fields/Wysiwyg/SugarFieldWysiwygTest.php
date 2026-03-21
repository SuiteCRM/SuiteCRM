<?php
/**
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

namespace SuiteCRM\Tests\Unit\includes\SugarFields\Fields\Wysiwyg;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;
use SugarFieldWysiwyg;
use SugarTinyMCE;

require_once __DIR__ . '/../../../../../../../../include/SugarFields/Fields/Wysiwyg/SugarFieldWysiwyg.php';

/**
 * Class SugarFieldWysiwygTest
 * Tests for SugarFieldWysiwyg HP-2 fix
 * @package SuiteCRM\Tests\Unit\includes\SugarFields\Fields\Wysiwyg
 */
class SugarFieldWysiwygTest extends SuitePHPUnitFrameworkTestCase
{
    /**
     * @var SugarFieldWysiwyg
     */
    protected $field;

    protected function setUp(): void
    {
        parent::setUp();
        $this->field = new SugarFieldWysiwyg('wysiwyg');
    }

    /**
     * Test RuntimeException handling in getEditViewSmarty
     * This tests the HP-2 fix: try-catch error handling for TinyMCE initialization
     */
    public function testGetEditViewSmartyHandlesTinyMCEError(): void
    {
        $vardef = [
            'name' => 'content',
            'type' => 'wysiwyg'
        ];

        $displayParams = [
            'formName' => 'EditView'
        ];

        $mockTinyMCE = $this->getMockBuilder(SugarTinyMCE::class)
            ->disableOriginalConstructor()
            ->getMock();

        $mockTinyMCE->method('getConfig')
            ->willThrowException(new \RuntimeException('TinyMCE configuration file missing'));

        $mockLog = $this->getMockBuilder(\LoggerManager::class)
            ->disableOriginalConstructor()
            ->getMock();

        $mockLog->expects($this->once())
            ->method('error')
            ->with($this->stringContains('[SugarFieldWysiwyg][getEditViewSmarty] Failed to initialize TinyMCE:'));

        $GLOBALS['log'] = $mockLog;

        $parentFieldArray = ['value' => 'test content'];
        $result = $this->field->getEditViewSmarty($parentFieldArray, $vardef, $displayParams, 1);

        $this->assertIsString($result);
    }

    /**
     * Test successful TinyMCE initialization
     * Verifies that TinyMCE config is properly generated
     */
    public function testGetEditViewSmartySuccess(): void
    {
        $vardef = [
            'name' => 'content',
            'type' => 'wysiwyg'
        ];

        $displayParams = [
            'formName' => 'EditView'
        ];

        $parentFieldArray = ['value' => 'test content'];
        $result = $this->field->getEditViewSmarty($parentFieldArray, $vardef, $displayParams, 1);

        $this->assertIsString($result);
        $this->assertArrayHasKey('tiny', $this->field->ss->_tpl_vars);

        $tinyVariable = $this->field->ss->_tpl_vars['tiny'];
        $this->assertIsString($tinyVariable);
    }

    /**
     * Test selector generation with form_name
     * Verifies correct selector format when displayParams['formName'] is set
     */
    public function testGetEditViewSmartyWithFormName(): void
    {
        $vardef = [
            'name' => 'content',
            'type' => 'wysiwyg'
        ];

        $displayParams = [
            'formName' => 'EditView'
        ];

        $parentFieldArray = ['value' => 'test content'];
        $result = $this->field->getEditViewSmarty($parentFieldArray, $vardef, $displayParams, 1);

        $this->assertArrayHasKey('tiny', $this->field->ss->_tpl_vars);
        $tinyVariable = $this->field->ss->_tpl_vars['tiny'];

        $this->assertStringContainsString('#EditView #content', $tinyVariable);
    }

    /**
     * Test selector generation without form_name
     * Verifies correct selector format when displayParams['formName'] is empty
     */
    public function testGetEditViewSmartyWithoutFormName(): void
    {
        $vardef = [
            'name' => 'content',
            'type' => 'wysiwyg'
        ];

        $displayParams = [];

        $parentFieldArray = ['value' => 'test content'];
        $result = $this->field->getEditViewSmarty($parentFieldArray, $vardef, $displayParams, 1);

        $this->assertArrayHasKey('tiny', $this->field->ss->_tpl_vars);
        $tinyVariable = $this->field->ss->_tpl_vars['tiny'];

        $this->assertStringContainsString('#content', $tinyVariable);
        $this->assertStringNotContainsString('#EditView #content', $tinyVariable);
    }

    /**
     * Test that TinyMCE config includes proper JavaScript structure
     * Verifies the generated JavaScript has the expected format
     */
    public function testGetEditViewSmartyIncludesJavaScriptStructure(): void
    {
        $vardef = [
            'name' => 'content',
            'type' => 'wysiwyg'
        ];

        $displayParams = [
            'formName' => 'EditView'
        ];

        $parentFieldArray = ['value' => 'test content'];
        $result = $this->field->getEditViewSmarty($parentFieldArray, $vardef, $displayParams, 1);

        $this->assertArrayHasKey('tiny', $this->field->ss->_tpl_vars);
        $tinyVariable = $this->field->ss->_tpl_vars['tiny'];

        $this->assertStringContainsString('<script type="text/javascript">', $tinyVariable);
        $this->assertStringContainsString('tinyConfig.selector', $tinyVariable);
        $this->assertStringContainsString('tinymce.init(tinyConfig)', $tinyVariable);
    }

    /**
     * Test that height override is properly set
     * Verifies that the height configuration is included in the config
     */
    public function testGetEditViewSmartyIncludesHeightOverride(): void
    {
        $vardef = [
            'name' => 'content',
            'type' => 'wysiwyg'
        ];

        $displayParams = [
            'formName' => 'EditView'
        ];

        $parentFieldArray = ['value' => 'test content'];
        $result = $this->field->getEditViewSmarty($parentFieldArray, $vardef, $displayParams, 1);

        $this->assertArrayHasKey('tiny', $this->field->ss->_tpl_vars);
        $tinyVariable = $this->field->ss->_tpl_vars['tiny'];

        $this->assertStringContainsString('tinyConfig.height = 250', $tinyVariable);
    }

    /**
     * Test that logging occurs when loading editor
     * Verifies the info log is written when TinyMCE is initialized
     */
    public function testGetEditViewSmartyLogsInfo(): void
    {
        $vardef = [
            'name' => 'content',
            'type' => 'wysiwyg'
        ];

        $displayParams = [
            'formName' => 'EditView'
        ];

        $mockLog = $this->getMockBuilder(\LoggerManager::class)
            ->disableOriginalConstructor()
            ->getMock();

        $mockLog->expects($this->atLeastOnce())
            ->method('info')
            ->with($this->stringContains('[SugarFieldWysiwyg][getEditViewSmarty] Loading editor for field: content'));

        $GLOBALS['log'] = $mockLog;

        $parentFieldArray = ['value' => 'test content'];
        $result = $this->field->getEditViewSmarty($parentFieldArray, $vardef, $displayParams, 1);
    }

    /**
     * Test graceful fallback when TinyMCE fails
     * Verifies that parent::getEditViewSmarty is called even when TinyMCE initialization fails
     */
    public function testGetEditViewSmartyFallsBackToParentOnError(): void
    {
        $vardef = [
            'name' => 'content',
            'type' => 'wysiwyg'
        ];

        $displayParams = [
            'formName' => 'EditView'
        ];

        $mockTinyMCE = $this->getMockBuilder(SugarTinyMCE::class)
            ->disableOriginalConstructor()
            ->getMock();

        $mockTinyMCE->method('getConfig')
            ->willThrowException(new \RuntimeException('Configuration error'));

        $mockLog = $this->getMockBuilder(\LoggerManager::class)
            ->disableOriginalConstructor()
            ->getMock();

        $GLOBALS['log'] = $mockLog;

        $parentFieldArray = ['value' => 'test content'];
        $result = $this->field->getEditViewSmarty($parentFieldArray, $vardef, $displayParams, 1);

        $this->assertIsString($result);
        $tinyVariable = $this->field->ss->_tpl_vars['tiny'] ?? null;
        $this->assertNull($tinyVariable);
    }

    /**
     * Test that setup is called before TinyMCE initialization
     * Verifies the setup method is invoked at the start of getEditViewSmarty
     */
    public function testGetEditViewSmartyCallsSetup(): void
    {
        $vardef = [
            'name' => 'content',
            'type' => 'wysiwyg'
        ];

        $displayParams = [
            'formName' => 'EditView'
        ];

        $parentFieldArray = ['value' => 'test content'];
        $result = $this->field->getEditViewSmarty($parentFieldArray, $vardef, $displayParams, 1);

        $this->assertNotNull($this->field->ss);
        $this->assertIsObject($this->field->ss);
    }

    /**
     * Test field name is correctly used in selector
     * Verifies the field name from vardef is properly included in the selector
     */
    public function testGetEditViewSmartyUsesCorrectFieldName(): void
    {
        $vardef = [
            'name' => 'custom_wysiwyg_field',
            'type' => 'wysiwyg'
        ];

        $displayParams = [
            'formName' => 'CustomForm'
        ];

        $parentFieldArray = ['value' => 'test content'];
        $result = $this->field->getEditViewSmarty($parentFieldArray, $vardef, $displayParams, 1);

        $this->assertArrayHasKey('tiny', $this->field->ss->_tpl_vars);
        $tinyVariable = $this->field->ss->_tpl_vars['tiny'];

        $this->assertStringContainsString('#CustomForm #custom_wysiwyg_field', $tinyVariable);
    }
}
