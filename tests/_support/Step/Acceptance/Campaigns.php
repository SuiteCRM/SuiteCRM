<?php

namespace Step\Acceptance;

#[\AllowDynamicProperties]
class Campaigns extends \AcceptanceTester
{
    /**
     * Create a non-emails campaign
     *
     * @param $name
     */
    public function createNonEmailCampaign($name)
    {
        $I = new EditView($this->getScenario());
        $Sidebar = new SideBar($this->getScenario());
        $faker = $this->getFaker();

        $I->see('Create Campaign', '.actionmenulink');
        $Sidebar->clickSideBarAction('Create');
        $I->see('Campaign Wizard');
        $I->click('A non email campaign is a campaign that does not send an email.');

        // Step 1
        $I->fillField('#name', $name);
        $I->fillField('#wiz_content', $faker->text());
        $I->selectOption('#status', 'Active');
        $I->selectOption('#campaign_type', 'Print');
        $I->seeElement('#assigned_user_name');
        $I->click('#next_button_div');

        // Step 2
        $I->fillField('#budget', $name);
        $I->fillField('#actual_cost', $name);
        $I->fillField('#expected_revenue', $name);
        $I->fillField('#expected_cost', $name);
        $I->fillField('#impressions', $name);
        $I->fillField('#objective', $faker->text());
        $I->selectOption('#currency_id', 'US Dollars : $');
        $I->click('#next_button_div');

        // Step 3
        $I->fillField('#target_list_name', 'DefaultTargetList');
        $I->executeJS('add_target(\'false\');');
        $I->fillField('#target_list_name', 'SeedTargetList');
        $I->executeJS('add_target(\'false\');');
        $I->fillField('#target_list_name', 'TestTargetList');
        $I->executeJS('add_target(\'false\');');
        $I->click('#wiz_submit_finish_button');

        // Step 4
        $I->waitForText('You have no associated targets in your selected target list(s) for this campaign. You can populate your list after finishing.');
        $I->see('You have no associated targets in your selected target list(s) for this campaign. You can populate your list after finishing.');
        $Sidebar->clickSideBarAction('View Campaigns');
    }

    /**
     * Create a Newletter campaign
     *
     * @param $name
     */
    public function createNewletterCampaign($name)
    {
        $I = new EditView($this->getScenario());
        $Sidebar = new SideBar($this->getScenario());
        $targetList = new TargetList($this->getScenario());
        $listView = new ListView($this->getScenario());
        $detailView = new DetailView($this->getScenario());
        $faker = $this->getFaker();

        $I->see('Create Campaign', '.actionmenulink');
        $Sidebar->clickSideBarAction('Create');
        $I->see('Campaign Wizard');
        $I->click('A newsletter campaign is a type of email campaign, which allows you to send an email to a single target list.');

        // Step 1
        $I->fillField('#name', $name);
        $I->fillField('#wiz_content', $faker->text());
        $I->selectOption('#status', 'Active');
        $I->seeElement('#assigned_user_name');
        $I->fillField('#budget', $faker->randomDigit);
        $I->fillField('#actual_cost', $faker->randomDigit);
        $I->fillField('#expected_revenue', $faker->randomDigit);
        $I->fillField('#expected_cost', $faker->randomDigit);
        $I->fillField('#impressions', $faker->randomDigit);
        $I->fillField('#objective', $faker->text());
        $I->selectOption('#currency_id', 'US Dollars : $');
        $I->executeJS('window.scrollTo(0,0); return true;');
        $I->click('#next_button_div');

        // Step 2
        $I->selectOption('#wiz_subscriptions_def_type', 1);
        $I->selectOption('#wiz_unsubscriptions_def_type', 1);
        $I->selectOption('#wiz_test_def_type', 1);
        $I->executeJS('window.scrollTo(0,0); return true;');
        $I->click('#wiz_submit_button');

        // Step 3
        $I->click('#template_option_create');
        $I->fillField('#template_name', 'Test_NewsLetterTemplate');
        $I->click('#LBL_CREATE_EMAIL_TEMPLATE_BTN');
        $I->fillField('#template_subject', $faker->name);
        $I->fillField('#mozaik_width_set', 800);
        $I->executeJS('window.scrollTo(0,0); return true;');
        $I->click('#next_button_div');

        // Step 5
        $I->waitForElementVisible('#marketing_name');
        $I->fillField('#marketing_name', $faker->email);
        $I->selectOption('#inbound_email_id', 'Test_BounceHandling');
        $I->click('#date_start_trigger');
        $I->click('#callnav_today');
        $I->waitForElementVisible('#wiz_submit_button');
        $I->click('#wiz_submit_button');

        // Step 6
        $I->waitForText('You cannot send a marketing email until your subscription list has at least one entry. You can populate your list after finishing.');
        $I->see('You cannot send a marketing email until your subscription list has at least one entry. You can populate your list after finishing.');

        // Populate target list
        $I->visitPage('ProspectLists', 'index');
        $listView->waitForListViewVisible();
        $I->click($name . ' Subscription List');
        $detailView->waitForDetailViewVisible();
        $I->waitForElementVisible('#whole_subpanel_contacts');
        $I->click('#whole_subpanel_contacts');
        $I->waitForElementVisible('#prospect_list_contacts_create_button');
        $I->click('#prospect_list_contacts_create_button');
        $I->waitForElementVisible('#last_name');
        $I->fillField('#last_name', $faker->name);
        $I->click('Save');

        $I->visitPage('ProspectLists', 'index');
        $listView->waitForListViewVisible();
        $I->click($name . ' Unsubscription List');
        $detailView->waitForDetailViewVisible();
        $I->waitForElementVisible('#whole_subpanel_contacts');
        $I->click('#whole_subpanel_contacts');
        $I->waitForElementVisible('#prospect_list_contacts_create_button');
        $I->click('#prospect_list_contacts_create_button');
        $I->waitForElementVisible('#last_name');
        $I->fillField('#last_name', $faker->name);
        $I->click('Save');

        $I->visitPage('ProspectLists', 'index');
        $listView->waitForListViewVisible();
        $I->click($name . ' Test List');
        $detailView->waitForDetailViewVisible();
        $I->waitForElementVisible('#whole_subpanel_contacts');
        $I->click('#whole_subpanel_contacts');
        $I->waitForElementVisible('#prospect_list_contacts_create_button');
        $I->click('#prospect_list_contacts_create_button');
        $I->waitForElementVisible('#last_name');
        $I->fillField('#last_name', $faker->name);
        $I->click('Save');
    }
}
