<?php

namespace Step\Acceptance;

use \AcceptanceTester as Tester;

#[\AllowDynamicProperties]
class Dashboard extends Tester
{
    /**
     * Wait for for the edit view to become visible
     */
    public function waitForDashboardVisible()
    {
        $I = $this;
        $I->waitForElementVisible('.dashboard');
        $I->waitForElementVisible('.dashletcontainer');
    }
}
