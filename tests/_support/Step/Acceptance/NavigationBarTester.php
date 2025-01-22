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

namespace Step\Acceptance;

use AcceptanceTester as Tester;
use Page\Design;
use SuiteCRM\Enumerator\DesignBreakPoint;

#[\AllowDynamicProperties]
class NavigationBarTester extends Tester
{
    /**
     * Click on the home buton / navbar brand
     */
    public function clickHome(): void
    {
        $I = $this;
        $breakpoint = (new Design($I))->getBreakpointString();
        switch ($breakpoint) {
            // The home button is only available on the large desktop
            // We need to select the home module from the all menu for tablet and mobile.
            case DesignBreakPoint::lg:
                $I->click('#navbar-brand');
                break;
            case DesignBreakPoint::md:
                $this->clickAllMenuItem('Home');
                break;
            case DesignBreakPoint::sm:
                $this->clickAllMenuItem('Home');
                break;
            case DesignBreakPoint::xs:
                $this->clickAllMenuItem('Home');
                break;
        }
    }

    /**
     * Selects a menu item from the users menu (global links)
     * @param $link
     *
     * <?php
     * $I->clickUserMenuItem('Admin')
     * $I->clickUserMenuItem('#admin_link')
     */
    public function clickUserMenuItem($link): void
    {
        $I = $this;
        $breakpoint = (new Design($I))->getBreakpointString();
        switch ($breakpoint) {
            case DesignBreakPoint::lg:
                $I->moveMouseOver('.desktop-bar #toolbar .globalLinks-desktop');
                $I->click($link, '.desktop-bar #toolbar .globalLinks-desktop');
                break;
            case DesignBreakPoint::md:
                $I->click('.tablet-bar #toolbar .globalLinks-mobile');
                $I->click($link, '.tablet-bar #toolbar .globalLinks-mobile');
                break;
            case DesignBreakPoint::sm:
                $I->click('.tablet-bar #toolbar .globalLinks-mobile');
                $I->click($link, '.tablet-bar #toolbar .globalLinks-mobile');
                break;
            case DesignBreakPoint::xs:
                $I->click('.mobile-bar #toolbar .globalLinks-mobile');
                $I->click($link, '.mobile-bar #toolbar .globalLinks-mobile');
                break;
        }
    }

    /**
     * Navigates to module. Selects a menu item from the all menu (top nav)
     * @param $link
     *
     * <?php
     * $I->clickAllMenuItem('Accounts')
     *
     * Watch out - the mobile navigation employs a different structure with with tablet and desktop versions. It is
     * best to use just the module translations.
     *
     * Also:
     * the non filter navigation is not supported by this method
     */
    public function clickAllMenuItem($link): void
    {
        $I = $this;
        $breakpoint = (new Design($I))->getBreakpointString();
        switch ($breakpoint) {
            case DesignBreakPoint::lg:
                $allMenuButton = '#toolbar.desktop-toolbar  > ul.nav.navbar-nav > li.topnav.all';
                $I->waitForElementVisible($allMenuButton);
                $I->click('All', $allMenuButton);
                $allMenu = $allMenuButton . ' > span.notCurrentTab > ul.dropdown-menu';
                $I->waitForElementVisible($allMenu);
                $I->click($link, $allMenu);
                break;
            case DesignBreakPoint::md:
                $allMenuButton = 'div.navbar-header > button.navbar-toggle';
                $I->waitForElementVisible($allMenuButton);
                $I->click($allMenuButton);
                $allMenu = 'div.navbar-header > #mobile_menu';
                $I->waitForElementVisible($allMenu);
                $I->click($link, $allMenu);
                break;
            case DesignBreakPoint::sm:
                $allMenuButton = 'div.navbar-header > button.navbar-toggle';
                $I->waitForElementVisible($allMenuButton);
                $I->click($allMenuButton);
                $allMenu = 'div.navbar-header > #mobile_menu';
                $I->waitForElementVisible($allMenu);
                $I->click($link, $allMenu);
                break;
            case DesignBreakPoint::xs:
                $allMenuButton = 'div.navbar-header > button.navbar-toggle';
                $I->waitForElementVisible($allMenuButton);
                $I->click($allMenuButton);
                $allMenu = 'div.navbar-header > #mobile_menu';
                $I->waitForElementVisible($allMenu);
                $I->click($link, $allMenu);
                break;
        }
    }

    /**
     * Selects a menu item from the current module menu (top nav)
     * @param $link
     *
     * <?php
     * $I->clickAllMenuItem('Accounts')
     *
     * Watch out - the mobile navigation employs a different structure with with tablet and desktop versions. It is
     * best to use just the module translations.
     *
     * Also:
     * the non filter navigation is not supported by this method
     */
    public function clickCurrentMenuItem($link): void
    {
        $I = $this;
        $breakpoint = (new Design($I))->getBreakpointString();
        switch ($breakpoint) {
            case DesignBreakPoint::lg:
                $I->moveMouseOver('//*[@id="toolbar"]/ul/li[2]/span[2]/a');
                $I->waitForElementVisible('#toolbar.desktop-toolbar  > ul.nav.navbar-nav > li.topnav ul.dropdown-menu > li.current-module-action-links > ul');
                $I->waitForText($link, 30, '#toolbar.desktop-toolbar  > ul.nav.navbar-nav > li.topnav > ul.dropdown-menu > li.current-module-action-links');
                $I->click($link, '#toolbar.desktop-toolbar  > ul.nav.navbar-nav > li.topnav > ul.dropdown-menu > li.current-module-action-links');
                break;
            case DesignBreakPoint::md:
                $I->click('div#mobileheader > div#modulelinks > .modulename > a');
                $I->waitForElementVisible('div#mobileheader > div#modulelinks > ul.dropdown-menu > li.mobile-current-actions > ul.mobileCurrentTab');
                $I->waitForText($link, 30, 'div#mobileheader > div#modulelinks > ul.dropdown-menu > li.mobile-current-actions > ul.mobileCurrentTab');
                $I->click($link, 'div#mobileheader > div#modulelinks > ul.dropdown-menu > li.mobile-current-actions > ul.mobileCurrentTab');
                break;
            case DesignBreakPoint::sm:
                $I->click('div#mobileheader > div#modulelinks > .modulename > a');
                $I->waitForElementVisible('#modulelinks > ul.dropdown-menu');
                $I->waitForText($link, 30, 'div#mobileheader > div#modulelinks > ul.dropdown-menu > li.mobile-current-actions > ul.mobileCurrentTab');
                $I->click($link, 'div#mobileheader > div#modulelinks > ul.dropdown-menu > li.mobile-current-actions > ul.mobileCurrentTab');
                break;
            case DesignBreakPoint::xs:
                $I->click('div#mobileheader > div#modulelinks > .modulename > a');
                $I->waitForElementVisible('div#mobileheader > div#modulelinks > ul.dropdown-menu > li.mobile-current-actions > ul.mobileCurrentTab');
                $I->waitForText($link, 30, 'div#mobileheader > div#modulelinks > ul.dropdown-menu > li.mobile-current-actions > ul.mobileCurrentTab');
                $I->click($link, 'div#mobileheader > div#modulelinks > ul.dropdown-menu > li.mobile-current-actions > ul.mobileCurrentTab');
                break;
        }
    }
}
