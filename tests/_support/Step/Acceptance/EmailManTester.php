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
        $I = new NavigationBarTester($this->getScenario());
        $EditView = new EditView($this->getScenario());

        $I->clickUserMenuItem('#admin_link');
        $I->click('#mass_Email_config');

        $I->checkOption('#notify_allow_default_outbound');

        $EditView->clickSaveButton();
    }
}
