<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once('modules/DynamicFields/templates/Fields/TemplateText.php');
#[\AllowDynamicProperties]
class TemplateCronSchedule extends TemplateText
{
    public $type='CronSchedule';

    public function __construct()
    {
        parent::__construct();
    }




    public function get_field_def()
    {
        $def = parent::get_field_def();
        $def['dbType'] = 'varchar';
        return $def;
    }
}
