<?php

namespace Step\Acceptance;

#[\AllowDynamicProperties]
class PDFTemplates extends \AcceptanceTester
{
    /**
     * Create a PDF Template
     *
     * @param $name
     */
    public function createPDFTemplate($name)
    {
        $I = new EditView($this->getScenario());
        $DetailView = new DetailView($this->getScenario());
        $Sidebar = new SideBar($this->getScenario());

        $I->see('Create PDF Template', '.actionmenulink');
        $Sidebar->clickSideBarAction('Create');
        $I->waitForEditViewVisible();
        $I->fillField('#name', $name);

        $I->checkOption('#active');
        $I->selectOption('#type', 'Invoices');
        $I->selectOption('#module_name', 'Accounts : Account');
        $I->selectOption('#variable_name', 'Name:');
        $I->selectOption('#page_size', 'Letter');
        $I->selectOption('#orientation', 'Landscape');

        $I->seeElement('#variable_text');
        $I->seeElement('#assigned_user_name');
        $I->seeElement('#sample');

        $I->executeJS('tinyMCE.activeEditor.setContent("TinyMCE Content Test");');

        $I->fillField('#margin_left', 20);
        $I->fillField('#margin_right', 20);
        $I->fillField('#margin_top', 20);
        $I->fillField('#margin_bottom', 20);
        $I->fillField('#margin_header', 10);
        $I->fillField('#margin_footer', 10);

        $I->clickSaveButton();
        $DetailView->waitForDetailViewVisible();
    }
}
