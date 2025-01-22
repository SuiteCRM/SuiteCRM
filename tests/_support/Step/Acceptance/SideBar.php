<?php

namespace Step\Acceptance;

use \AcceptanceTester as Tester;

#[\AllowDynamicProperties]
class SideBar extends Tester
{
    /**
     * @param string $link
     * <?php
     * $sideBar = new \Page\Sidebar($I);
     * $sideBar->clickSideBarAction('Import');
     */
    public function clickSideBarAction($link)
    {
        $I = $this;
        $I->click($link, '#actionMenuSidebar');
    }

    /**
     * @param string $link
     * <?php
     * $sideBar = new \Page\Sidebar($I);
     * $sideBar->clickSideBarRecentlyViewed('John Smith');
     */
    public function clickSideBarRecentlyViewed($link)
    {
        $I = $this;
        $I->click($link, '#recentlyViewedSidebar');
    }

    /**
     * @param string $link
     * <?php
     * $sideBar = new \Page\Sidebar($I);
     * $sideBar->clickSideBarFavorite('Amazing Technologies');
     */
    public function clickSideBarFavorite($link)
    {
        $I = $this;
        $I->click($link, '#favoritesSidebar');
    }

    /**
     * Toggles the side bar
     */
    public function clickToggleSideBar()
    {
        $I = $this;
        $I->click('#buttontoggle', '#sidebar_container');
    }
}
