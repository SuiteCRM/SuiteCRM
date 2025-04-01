<?php

require_once('modules/Contacts/ContactsListViewSmarty.php');

#[\AllowDynamicProperties]
class ContactsViewList extends ViewList
{
    /**
     * @see ViewList::preDisplay()
     */
    public function preDisplay()
    {
        require_once('modules/AOS_PDF_Templates/formLetter.php');
        formLetter::LVPopupHtml('Contacts');
        parent::preDisplay();

        $this->lv = new ContactsListViewSmarty();
    }
}
