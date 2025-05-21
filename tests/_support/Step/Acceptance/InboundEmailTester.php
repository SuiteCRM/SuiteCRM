<?php

namespace Step\Acceptance;

#[\AllowDynamicProperties]
class InboundEmailTester extends \AcceptanceTester
{
    /**
     * Go to inbound email
     */
    public function gotoEmailSettings()
    {
        $I = new NavigationBarTester($this->getScenario());
        $I->clickUserMenuItem('#admin_link');
        $I->click('#mailboxes');
    }

    /**
     * Populate bounce email account
     */
    public function createBounceEmail()
    {
        $I = new NavigationBarTester($this->getScenario());
        $EditView = new EditView($this->getScenario());
        $sideBar = new SideBar($this->getScenario());
        $faker = $this->getFaker();

        $I->clickUserMenuItem('#admin_link');
        $I->click('#mailboxes');
        $sideBar->clickSideBarAction('New Bounce Handling Account');

        $I->click('#prefill_gmail_defaults_link');
        $I->fillField('#name', 'Test_BounceHandling');
        $I->fillField('#email_user', $faker->name);
        $I->fillField('#email_password', $faker->name);
        $EditView->clickSaveButton();
    }
}
