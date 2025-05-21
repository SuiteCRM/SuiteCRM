<?php

require_once('modules/Accounts/AccountsListViewSmarty.php');

#[\AllowDynamicProperties]
class AccountsViewList extends ViewList
{
    /**
     * @see ViewList::preDisplay()
     */
    public function preDisplay()
    {
        // STIC-Custom 20220124 MHP - Do not add the Print PDF logic in this module because it is added generically through include/MVC/View/views/view.list.php
        // STIC#564   
        // require_once('modules/AOS_PDF_Templates/formLetter.php');
        // formLetter::LVPopupHtml('Accounts');
        // END STIC-Custom
        parent::preDisplay();

        $this->lv = new AccountsListViewSmarty();
    }
}
