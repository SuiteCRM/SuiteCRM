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

namespace SuiteCRM\Tests\Unit\includes\utils;

use Exception;
use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

require_once __DIR__ . '/../../../../../include/utils/mvc_utils.php';

/**
 * Class mvc_utilsTest
 * @package SuiteCRM\Tests\Unit\utils
 */
class mvc_utilsTest extends SuitePHPUnitFrameworkTestCase
{
    public function testloadParentView(): void
    {
        //execute the method and test if it doesn't throws an exception
        try {
            loadParentView('classic');
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testgetPrintLink(): void
    {
        //test without setting REQUEST param
        $expected = "javascript:void window.open('index.php?','printwin','menubar=1,status=0,resizable=1,scrollbars=1,toolbar=0,location=1')";
        $actual = getPrintLink();
        self::assertSame($expected, $actual);

        //test with required REQUEST param set
        $_REQUEST['action'] = 'ajaxui';
        $expected = 'javascript:SUGAR.ajaxUI.print();';
        $actual = getPrintLink();
        self::assertSame($expected, $actual);
    }

    public function testajaxBannedModules(): void
    {
        //execute the method and test verify it returns true
        $result = ajaxBannedModules();
        self::assertIsArray($result);
    }

    public function testajaxLink(): void
    {
        global $sugar_config;
        $ajaxUIDisabled = isset($sugar_config['disableAjaxUI']) && $sugar_config['disableAjaxUI'];

        if (!$ajaxUIDisabled) {
            self::assertSame('?action=ajaxui#ajaxUILoc=', ajaxLink(''));
            $testModules = array(
                'Calendar',
                'Emails',
                'Campaigns',
                'Documents',
                'DocumentRevisions',
                'Project',
                'ProjectTask',
                'EmailMarketing',
                'CampaignLog',
                'CampaignTrackers',
                'Releases',
                'Groups',
                'EmailMan',
                "Administration",
                "ModuleBuilder",
                'Schedulers',
                'SchedulersJobs',
                'DynamicFields',
                'EditCustomFields',
                'EmailTemplates',
                'Users',
                'Currencies',
                'Trackers',
                'Connectors',
                'Import_1',
                'Import_2',
                'vCals',
                'CustomFields',
                'Roles',
                'Audit',
                'InboundEmail',
                'SavedSearch',
                'UserPreferences',
                'MergeRecords',
                'EmailAddresses',
                'Relationships',
                'Employees',
                'Import',
                'OAuthKeys'
            );
            $bannedModules = ajaxBannedModules();
            foreach ($testModules as $module) {
                $uri = "index.php?module=$module&action=detail&record=1";
                if (!in_array($module, $bannedModules)) {
                    self::assertSame("?action=ajaxui#ajaxUILoc=" . urlencode($uri), ajaxLink($uri));
                } else {
                    self::assertSame($uri, ajaxLink($uri));
                }
            }
        }
    }
}
