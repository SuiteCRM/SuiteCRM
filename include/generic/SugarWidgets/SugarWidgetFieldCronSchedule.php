<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
class SugarWidgetFieldCronSchedule extends SugarWidgetFieldVarchar
{
    public function __construct($layout_manager)
    {
        parent::__construct($layout_manager);
    }
}
