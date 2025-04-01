<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}


#[\AllowDynamicProperties]
class AOS_PDF_TemplatesViewDetail extends ViewDetail
{
    public function __construct()
    {
        parent::__construct();
    }




    public function display()
    {
        $this->setDecodeHTML();
        parent::display();
    }

    public function setDecodeHTML()
    {
        $this->bean->pdfheader = html_entity_decode(str_replace('&nbsp;', ' ', (string) $this->bean->pdfheader));
        $this->bean->description = html_entity_decode(str_replace('&nbsp;', ' ', (string) $this->bean->description));
        $this->bean->pdffooter = html_entity_decode(str_replace('&nbsp;', ' ', (string) $this->bean->pdffooter));
    }
}
