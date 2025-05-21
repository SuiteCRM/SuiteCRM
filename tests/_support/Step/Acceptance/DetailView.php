<?php

namespace Step\Acceptance;

use \AcceptanceTester as Tester;

#[\AllowDynamicProperties]
class DetailView extends Tester
{

    /**
     * Click on the action menu and select a menu item
     * @param string $link
     * <?php
     * $detailView = new \Page\DetailView($i);
     * $detailView->clickActionMenuItem('Edit');
     *
     */
    public function clickActionMenuItem($link)
    {
        $I = $this;

        $I->waitForElementVisible('#tab-actions');
        $I->click('ACTIONS', '#tab-actions');
        $I->waitForElementVisible('#tab-actions > .dropdown-menu');

        $I->click($link, '#tab-actions > .dropdown-menu');
    }

    /**
     * Wait for for the detail view to become visible
     */
    public function waitForDetailViewVisible()
    {
        $I = $this;
        $I->waitForElementVisible('.detail-view');
    }
}
