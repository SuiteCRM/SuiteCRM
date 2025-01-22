<?php
/**
 * SubPanelSearchForm.php
 * @author SalesAgility <info@salesagility.com>
 * Date: 28/01/14
 */

require_once('include/SearchForm/SearchForm2.php');

class SubPanelSearchForm extends SearchForm
{
    public $subPanel; // the instantiated bean of the subPanel

    public function __construct($seed, $module, $subPanel, $options = array())
    {
        $this->subPanel = $subPanel;
        parent::__construct($seed, $module, 'DetailView', $options);
    }

    public function display($header = false)
    {
        /*//global $app_list_strings;
        if($this->subPanel->subpanel_defs->isCollection() && isset($this->subPanel->subpanel_defs->base_collection_list)){
            $GLOBALS['app_list_strings']['collection_temp_list'] = $this->getCollectionList($this->subPanel->subpanel_defs->base_collection_list);
        }*/
        $this->th->ss->assign('subpanel', $this->subPanel->subpanel_id);

        // Adding the offset to subpanel search field - this has no affect on pagination
        if ($this->subPanel->parent_bean->module_dir != '') {
            $this->th->ss->assign('subpanelPageOffset', '<input type="hidden" name="'.$this->subPanel->parent_bean->module_dir.'_'.$this->subPanel->subpanel_id.'_CELL_offset" value="0" />');
        }
        $this->parsedView = 'sps';
        return parent::display($header);
    }

    public function getCollectionList($collection = array())
    {
        global $app_list_strings;

        $select = array();

        if (!empty($collection)) {
            $select = array();
            foreach ($collection as $name => $value_array) {
                if (isset($app_list_strings['moduleList'][$value_array['module']])) {
                    $select[$name] = $app_list_strings['moduleList'][$value_array['module']];
                }
            }
        }
        return $select;
    }

    public function displaySavedSearchSelect()
    {
        return null;
    }
}
