<?php
/**
 * Class UserWizardCest
 * As an administrative user, I want to use the install wizard (web based) to install SuiteCRM.
 */
#[\AllowDynamicProperties]
class UserWizardCest
{

    /**
     * @param InstallTester $I
     */
    public function _before(InstallTester $I)
    {
    }

    /**
     * @param InstallTester $I
     */
    public function _after(InstallTester $I)
    {
    }

    /**
     * @param \Helper\WebDriverHelper $webDriverHelper
     * Dependency injection
     */
    protected function _inject(\Helper\WebDriverHelper $webDriverHelper)
    {
        $this->webDriverHelper = $webDriverHelper;
    }

    // tests
    /**
     * @param InstallTester $I
     * @param \Helper\WebDriverHelper $webDriverHelper
     *
     * As an administrative user, I want to use the install wizard (web based) to install SuiteCRM.
     * Given that that I install SuiteCRM with the default configuration settings I
     * Expect to be able to login as an administrator.
     */
    public function testScenarioInstallSuiteCRMWithDefaultConfiguration(InstallTester $I, Step\Acceptance\EmailManTester $I2, \Helper\WebDriverHelper $webDriverHelper)
    {
        $I->wantTo('check the php version meets the recommended requirements.');
        $I->amOnUrl($webDriverHelper->getInstanceURL());
        $I->waitForText('Setup');
        $I->maySeeOldVersionDetected();
        $I->acceptLicense();
        $I->seeValidSystemEnvironment();
        $I->configureInstaller($webDriverHelper);
        $I->waitForInstallerToFinish();

        // ---------- Email Settings ---------------

        $I2->wantTo('Save an outgoing email configuration');
        // Navigate to email configuration and save settings
        $I2->loginAsAdmin();
        $I2->createEmailSettings();
        $I2->dontSee('Note: To send record assignment notifications, an SMTP server must be configured in Email Settings.');
    }
}
