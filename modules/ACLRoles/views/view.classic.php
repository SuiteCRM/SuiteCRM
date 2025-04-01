<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}


#[\AllowDynamicProperties]
class ACLRolesViewClassic extends ViewDetail
{
    public function __construct()
    {
        parent::__construct();

        //turn off normal display of subpanels
        $this->options['show_subpanels'] = false; //no longer works in 6.3.0
    }




    public function display()
    {
        $this->dv->process();

        $file = SugarController::getActionFilename($this->action);
        $this->includeClassicFile('modules/'. $this->module . '/'. $file . '.php');
    }

    public function preDisplay()
    {
        parent::preDisplay();

        $this->options['show_subpanels'] = false; //eggsurplus: will display subpanels twice otherwise
    }
}
