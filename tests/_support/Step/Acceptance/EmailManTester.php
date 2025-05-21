<?php

namespace Step\Acceptance;

#[\AllowDynamicProperties]
class EmailManTester extends \AcceptanceTester
{
    private static $testerEmailAddress = 'sa.tester2@gmail.com';
    private static $testerEmailPassword = 'chilisauce';

    /**
     * Go to email settings
     */
    public function gotoEmailSettings()
    {
        $I = new NavigationBarTester($this->getScenario());
        $I->clickUserMenuItem('#admin_link');
        $I->click('#mass_Email_config');
    }

    /**
     * Populate email settings
     *
     * @param $name
     */
    public function createEmailSettings()
    {
//
//        $I = new AcceptanceTester($scenario);
//        $I->amOnPage('/SuiteCRM/index.php');
//        $I->click('Gmail');
//        $I->fillField('mail_smtpuser', 'sa.tester2@gmail.com');
//        $I->fillField('mail_smtppass', 'chilisauce');
//        $I->checkOption('notify_allow_default_outbound');
//        $I->click('button');
//        $I->seeCurrentURLEquals('/SuiteCRM/index.php');
//        return ; // using different email settings in the future..

        $I = new NavigationBarTester($this->getScenario());
        $EditView = new EditView($this->getScenario());
        $faker = $this->getFaker();

        $I->clickUserMenuItem('#admin_link');
        $I->click('#mass_Email_config');

        $I->fillField('#notify_fromname', $faker->name());
        $I->fillField('#notify_fromaddress', $faker->email);
        $I->click('#gmail-button');

        $I->checkOption('#mail_smtpauth_req');
        $I->fillField('#mail_smtpuser', self::$testerEmailAddress);
        $I->executeJS('SUGAR.util.setEmailPasswordEdit(\'mail_smtppass\')');
        $I->fillField('#mail_smtppass', self::$testerEmailPassword);
        $I->checkOption('#notify_allow_default_outbound');

        $EditView->clickSaveButton();
    }
}
