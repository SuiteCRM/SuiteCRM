<?php

namespace Step\Acceptance;

#[\AllowDynamicProperties]
class UsersTester extends \AcceptanceTester
{
    /**
     * Navigate to users profile module
     */
    public function gotoProfile()
    {
        $I = new NavigationBarTester($this->getScenario());
        $I->clickUserMenuItem('Profile');
    }

    /**
     * Logout a user
     */
    public function logoutUser()
    {
        $I = new NavigationBarTester($this->getScenario());
        $I->clickUserMenuItem('#logout_link');
    }
}
