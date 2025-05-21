<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}


#[\AllowDynamicProperties]
class AOS_InvoicesViewEdit extends ViewEdit
{
    public function __construct()
    {
        parent::__construct();
    }




    public function display()
    {
        $this->populateInvoiceTemplates();
        parent::display();
    }

    public function populateInvoiceTemplates()
    {
        global $app_list_strings;

        $sql = "SELECT id, name FROM aos_pdf_templates WHERE deleted='0' AND type='AOS_Invoices'";
        $res = $this->bean->db->query($sql);

        $app_list_strings['template_ddown_c_list'] = array();
        while ($row = $this->bean->db->fetchByAssoc($res)) {
            $app_list_strings['template_ddown_c_list'][$row['id']] = $row['name'];
        }
    }
}
