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

use Codeception\Actor;
use Codeception\Lib\Friend;
use Helper\WebDriverHelper;
use SuiteCRM\Enumerator\DatabaseDriver;


/**
 * Inherited Methods
 * @method void wantToTest($text)
 * @method void wantTo($text)
 * @method void execute($callable)
 * @method void expectTo($prediction)
 * @method void expect($prediction)
 * @method void amGoingTo($argumentation)
 * @method void am($role)
 * @method void lookForwardTo($achieveValue)
 * @method void comment($description)
 * @method Friend haveFriend($name, $actorClass = null)
 * @method checkOption(string $string)
 * @method click(string $string)
 * @method see(string $string)
 * @method waitForText(string $string)
 * @method seeElement(string $string)
 * @method fillField(string $string, string $string1)
 * @method waitForElement(string $string, int $int)
 * @method dontSee(string $string)
 * @method executeJS(string $string)
 *
 * @SuppressWarnings(PHPMD)
 */
#[\AllowDynamicProperties]
class InstallTester extends Actor
{
    use _generated\InstallTesterActions;

    /**
     * Tests the old version detected screen and moves to the next stage in the wizard
     */
    public function maySeeOldVersionDetected()
    {
        $I = $this;
        $scenario = $I->getScenario();

        if (!$this->isOldPhpVersionDetected()) {
            $scenario->comment('PHP Version ' . PHP_VERSION . ' meets the recommended requirements.');
        } else {
            $scenario->comment('PHP Version ' . PHP_VERSION . ' does not meet the recommended requirements.');
            $I->see('Old PHP Version Detected');
            $I->dontSeeMissingLabels();
            $I->checkOption('setup_old_php');
            $I->click('Next');
            $I->waitForText('GNU AFFERO GENERAL PUBLIC LICENSE');
        }
    }

    /**
     * Accept the license agreement and moves to the next stage of the wizard
     */
    public function acceptLicense()
    {
        $I = $this;
        $I->comment('Accept the license agreement.');
        $I->seeElement('.licensetext');
        $I->seeElement('#licenseaccept');
        $I->seeElement('#button_print_license');
        $I->dontSeeMissingLabels();
        $I->click('I Accept');
        $I->click('Next');

        // Wait for next screen. Fail if next button doesn't work
        $I->waitForText('System Environment');
    }


    /**
     * Tests the System Environment Stage and moves to the next stage of the wizard
     */
    public function seeValidSystemEnvironment()
    {
        $I = $this;
        $I->comment('View System Environment.');
        $I->see('System Environment');
        $I->dontSeeMissingLabels();
        $I->click('Next');
        $I->waitForText('Database Configuration');
    }

    /**
     * @param WebDriverHelper $webDriverHelper
     * @throws Exception
     * Configures the installer based on the webdriver configuration and moves to the next stage of the wizard
     */
    public function configureInstaller(WebDriverHelper $webDriverHelper)
    {
        $I = $this;
        $I->comment('Configure installer using Web Driver configuration.');
        $I->see('Database Configuration');
        $I->dontSeeMissingLabels();

        // TODO: TASK: UNDEFINED - Add mocha testing here
        // TODO: TASK: UNDEFINED - Add tests for form validation

        // Select Database type
        switch ($webDriverHelper->getDatabaseDriver()) {
            case DatabaseDriver::MYSQL:
                $I->checkOption('#setup_db_type[value=mysql]');
                break;
            case DatabaseDriver::MSSQL:
                $I->checkOption('#setup_db_type[value=mssql]');
                // clear instance field
                $I->fillField('#setup_db_host_instance', '');
                break;
            default:
                throw new \RuntimeException('No Database Driver Specified');
        }

        // Fill in database configuration
        $I->fillField('#setup_db_database_name', $webDriverHelper->getDatabaseName());
        $I->fillField('#setup_db_host_name', $webDriverHelper->getDatabaseHost());
        $I->fillField('#setup_db_admin_user_name', $webDriverHelper->getDatabaseUser());
        $I->fillField('#setup_db_admin_password_entry', $webDriverHelper->getDatabasePassword());

        // Fill In Site Configuration
        $I->fillField('[name=setup_site_admin_user_name]', $webDriverHelper->getAdminUser());
        $I->fillField('[name=setup_site_admin_password]', $webDriverHelper->getAdminPassword());
        $I->fillField('[name=setup_site_admin_password_retype]', $webDriverHelper->getAdminPassword());
        $I->fillField('[name=setup_site_url]', $webDriverHelper->getInstanceURL());
        $I->fillField('[name=email1]', 'install.tester@example.com');


        $I->click('Next');
        $I->waitForText('Install', 120);
    }

    /**
     * Waits for the login screen after the installer has finished and moves to the next stage of the wizard
     */
    public function waitForInstallerToFinish()
    {
        $I = $this;
        $I->comment('wait for installer progress to finish');
        $I->waitForElement('[type=submit]', 90);
        $I->dontSeeMissingLabels();
        $I->dontSeeErrors();
    }

    /**
     *
     */
    public function dontSeeMissingLabels()
    {
        $I = $this;
        $I->dontSee('LBL_');
    }

    public function dontSeeErrors()
    {
        $I = $this;
        $I->dontSee('Warning');
        $I->dontSee('Notice');
        $I->dontSee('Error');
        $I->dontSee('error');
    }

    protected function isOldPhpVersionDetected()
    {
        return $this->executeJS('return document.getElementsByName(\'setup_old_php\').length > 0;');
    }
}
