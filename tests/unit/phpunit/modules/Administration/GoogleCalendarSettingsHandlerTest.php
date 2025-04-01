<?php
/**
*
* SugarCRM Community Edition is a customer relationship management program developed by
* SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
*
* SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
* Copyright (C) 2011 - 2018 SalesAgility Ltd.
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

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

include_once __DIR__ . '/../../../../../include/utils/BaseHandler.php';
include_once __DIR__ . '/../../../../../modules/Administration/GoogleCalendarSettingsHandler.php';
include_once __DIR__ . '/GoogleCalendarSettingsHandlerMock.php';
include_once __DIR__ . '/../../../../../include/utils/layout_utils.php';

class GoogleCalendarSettingsHandlerTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();
        $GLOBALS['mod_strings'] = return_module_language($GLOBALS['current_language'], 'Administration');
    }

    public function testFirst(): void
    {
        self::assertEquals(true, true);
    }

    public function testDoAction(): void
    {
        global $current_user;
        global $mod_strings;
        global $app_strings;

        $tplPath = 'modules/Administration/GoogleCalendarSettings.tpl';

        $request = array('do' => 'save');

        $gcsHandler = new GoogleCalendarSettingsHandlerMock(
            $tplPath,
            $current_user,
            $request,
            $mod_strings,
            new Configurator(),
            new Sugar_Smarty(),
            new javascript()
        );

        self::assertTrue($gcsHandler->getExitOk());
        self::assertEquals('index.php?module=Administration&action=index', $gcsHandler->getRedirectUrl());
    }

    public function testNoDoAction(): void
    {
        global $current_user;
        global $mod_strings;
        global $app_strings;

        $tplPath = 'modules/Administration/GoogleCalendarSettings.tpl';

        $request = array();

        $gcsHandler = new GoogleCalendarSettingsHandlerMock(
            $tplPath,
            $current_user,
            $request,
            $mod_strings,
            new Configurator(),
            new Sugar_Smarty(),
            new javascript()
        );

        self::assertFalse($gcsHandler->getExitOk());
        self::assertEquals('', $gcsHandler->getRedirectUrl());
    }

    public function testHandleDisplay(): void
    {
        global $current_user;
        global $mod_strings;
        global $app_strings;

        $tplPath = 'modules/Administration/GoogleCalendarSettings.tpl';

        $request = array();

        $cfg = new Configurator();
        unset($cfg->config['google_auth_json']);
        $gcsHandler = new GoogleCalendarSettingsHandlerMock(
            $tplPath,
            $current_user,
            $request,
            $mod_strings,
            $cfg,
            $s = new Sugar_Smarty(),
            new javascript()
        );

        $ret = $gcsHandler->handleDisplay();

        self::assertTrue($gcsHandler->getJavascriptCalled());
        self::assertFalse($cfg->config['google_auth_json']);

        self::assertEquals(array(
            'status' => 'UNCONFIGURED',
            'color' => 'black'
            ), $s->get_template_vars('GOOGLE_JSON_CONF'));
    }
}
