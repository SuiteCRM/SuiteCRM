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
        require_once('modules/AOS_PDF_Templates/formLetter.php');
        formLetter::LVPopupHtml('Accounts');
        parent::preDisplay();

        $this->lv = new AccountsListViewSmarty();
    }
}
