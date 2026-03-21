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

namespace SuiteCRM\Tests\Unit\includes\SugarFields\Fields\Text;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;
use SugarFieldText;
use SugarTinyMCE;

require_once __DIR__ . '/../../../../../../../../include/SugarFields/Fields/Text/SugarFieldText.php';

/**
 * Class SugarFieldTextTest
 * Tests for SugarFieldText HP-1 and HP-2 fixes
 * @package SuiteCRM\Tests\Unit\includes\SugarFields\Fields\Text
 */
class SugarFieldTextTest extends SuitePHPUnitFrameworkTestCase
{
    /**
     * @var SugarFieldText
     */
    protected $field;

    protected function setUp(): void
    {
        parent::setUp();
        $this->field = new SugarFieldText('text');
    }

    protected function tearDown(): void
    {
        if (isset($_REQUEST['action'])) {
            unset($_REQUEST['action']);
        }
        parent::tearDown();
    }

    /**
     * Test that RuntimeException from SugarTinyMCE is caught and logged
     * This tests the HP-2 fix: try-catch error handling for TinyMCE initialization
     */
    public function testSetupHandlesTinyMCEInitializationError(): void
    {
        $vardef = [
            'name' => 'description',
            'editor' => 'html',
            'type' => 'text'
        ];

        $displayParams = [
            'formName' => 'EditView'
        ];

        $_REQUEST['action'] = 'EditView';

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
            ->with($this->stringContains('[SugarFieldText][setup] Failed to initialize TinyMCE:'));

        $GLOBALS['log'] = $mockLog;

        $parentFieldArray = ['value' => 'test content'];
        $this->field->setup($parentFieldArray, $vardef, $displayParams, 1);

        $tinyMCEVariable = $this->field->ss->_tpl_vars['tinymce'] ?? '';
        $this->assertEquals('', $tinyMCEVariable);
    }

    /**
     * Test successful TinyMCE initialization with HTML editor
     * Verifies that when all conditions are met, TinyMCE is properly initialized
     */
    public function testSetupWithValidHTMLEditor(): void
    {
        $vardef = [
            'name' => 'description',
            'editor' => 'html',
            'type' => 'text'
        ];

        $displayParams = [
            'formName' => 'EditView'
        ];

        $_REQUEST['action'] = 'EditView';

        $parentFieldArray = ['value' => 'test content'];
        $this->field->setup($parentFieldArray, $vardef, $displayParams, 1);

        $this->assertArrayHasKey('tinymce', $this->field->ss->_tpl_vars);
        $tinyMCEVariable = $this->field->ss->_tpl_vars['tinymce'];

        $this->assertIsString($tinyMCEVariable);
    }

    /**
     * Test no TinyMCE initialization when editor is not "html"
     * Ensures TinyMCE is only loaded for HTML editor fields
     */
    public function testSetupWithoutHTMLEditor(): void
    {
        $vardef = [
            'name' => 'description',
            'editor' => 'plain',
            'type' => 'text'
        ];

        $displayParams = [
            'formName' => 'EditView'
        ];

        $_REQUEST['action'] = 'EditView';

        $parentFieldArray = ['value' => 'test content'];
        $this->field->setup($parentFieldArray, $vardef, $displayParams, 1);

        $tinyMCEVariable = $this->field->ss->_tpl_vars['tinymce'] ?? '';
        $this->assertEquals('', $tinyMCEVariable);
    }

    /**
     * Test HP-1 fix: isset($_REQUEST['action']) prevents undefined index error
     * Verifies that the isset check prevents PHP notices when action is not set
     */
    public function testSetupWithoutEditViewAction(): void
    {
        $vardef = [
            'name' => 'description',
            'editor' => 'html',
            'type' => 'text'
        ];

        $displayParams = [
            'formName' => 'EditView'
        ];

        unset($_REQUEST['action']);

        $parentFieldArray = ['value' => 'test content'];
        $this->field->setup($parentFieldArray, $vardef, $displayParams, 1);

        $tinyMCEVariable = $this->field->ss->_tpl_vars['tinymce'] ?? '';
        $this->assertEquals('', $tinyMCEVariable);
    }

    /**
     * Test that TinyMCE is not initialized when action is not "EditView"
     * Ensures TinyMCE only loads on the EditView action
     */
    public function testSetupWithDifferentAction(): void
    {
        $vardef = [
            'name' => 'description',
            'editor' => 'html',
            'type' => 'text'
        ];

        $displayParams = [
            'formName' => 'EditView'
        ];

        $_REQUEST['action'] = 'DetailView';

        $parentFieldArray = ['value' => 'test content'];
        $this->field->setup($parentFieldArray, $vardef, $displayParams, 1);

        $tinyMCEVariable = $this->field->ss->_tpl_vars['tinymce'] ?? '';
        $this->assertEquals('', $tinyMCEVariable);
    }

    /**
     * Test TinyMCE selector generation with form name
     * Verifies correct selector format when formName is provided
     */
    public function testSetupWithFormNameGeneratesCorrectSelector(): void
    {
        $vardef = [
            'name' => 'description',
            'editor' => 'html',
            'type' => 'text'
        ];

        $displayParams = [
            'formName' => 'EditView'
        ];

        $_REQUEST['action'] = 'EditView';

        $parentFieldArray = ['value' => 'test content'];
        $this->field->setup($parentFieldArray, $vardef, $displayParams, 1);

        $this->assertArrayHasKey('tinymce', $this->field->ss->_tpl_vars);
        $tinyMCEVariable = $this->field->ss->_tpl_vars['tinymce'];

        $this->assertStringContainsString('#EditView #description', $tinyMCEVariable);
    }

    /**
     * Test TinyMCE selector generation without form name
     * Verifies correct selector format when formName is empty
     */
    public function testSetupWithoutFormNameGeneratesCorrectSelector(): void
    {
        $vardef = [
            'name' => 'description',
            'editor' => 'html',
            'type' => 'text'
        ];

        $displayParams = [];

        $_REQUEST['action'] = 'EditView';

        $parentFieldArray = ['value' => 'test content'];
        $this->field->setup($parentFieldArray, $vardef, $displayParams, 1);

        $this->assertArrayHasKey('tinymce', $this->field->ss->_tpl_vars);
        $tinyMCEVariable = $this->field->ss->_tpl_vars['tinymce'];

        $this->assertStringContainsString('#description', $tinyMCEVariable);
        $this->assertStringNotContainsString('#EditView #description', $tinyMCEVariable);
    }

    /**
     * Test that TinyMCE config includes height override
     * Verifies that the height configuration is properly set
     */
    public function testSetupIncludesHeightOverride(): void
    {
        $vardef = [
            'name' => 'description',
            'editor' => 'html',
            'type' => 'text'
        ];

        $displayParams = [
            'formName' => 'EditView'
        ];

        $_REQUEST['action'] = 'EditView';

        $parentFieldArray = ['value' => 'test content'];
        $this->field->setup($parentFieldArray, $vardef, $displayParams, 1);

        $this->assertArrayHasKey('tinymce', $this->field->ss->_tpl_vars);
        $tinyMCEVariable = $this->field->ss->_tpl_vars['tinymce'];

        $this->assertStringContainsString('tinyConfig.height = 250', $tinyMCEVariable);
    }

    /**
     * Test that logging occurs when loading HTML editor
     * Verifies the info log is written when TinyMCE is initialized
     */
    public function testSetupLogsInfoWhenLoadingHTMLEditor(): void
    {
        $vardef = [
            'name' => 'description',
            'editor' => 'html',
            'type' => 'text'
        ];

        $displayParams = [
            'formName' => 'EditView'
        ];

        $_REQUEST['action'] = 'EditView';

        $mockLog = $this->getMockBuilder(\LoggerManager::class)
            ->disableOriginalConstructor()
            ->getMock();

        $mockLog->expects($this->atLeastOnce())
            ->method('info')
            ->with($this->stringContains('[SugarFieldText][setup] Loading HTML editor for field: description'));

        $GLOBALS['log'] = $mockLog;

        $parentFieldArray = ['value' => 'test content'];
        $this->field->setup($parentFieldArray, $vardef, $displayParams, 1);
    }
}
