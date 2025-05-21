<?php

namespace Step\Acceptance;

use \AcceptanceTester as Tester;

#[\AllowDynamicProperties]
class EditView extends Tester
{
    /**
     * Wait for for the edit view to become visible
     */
    public function waitForEditViewVisible()
    {
        $I = $this;
        $I->waitForElementVisible('#EditView');
    }

    public function clickSaveButton()
    {
        $I = $this;
        $I->executeJS('window.scrollTo(0,0); return true;');
        $I->click('Save');
    }
}
