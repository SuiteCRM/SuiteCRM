<?php

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once('include/Dashlets/DashletGeneric.php');
require_once('modules/jjwg_Areas/jjwg_Areas.php');

#[\AllowDynamicProperties]
class jjwg_AreasDashlet extends DashletGeneric
{
    public function __construct($id, $def = null)
    {

        global $dashletData;

        $dashletData = $dashletData ?? [];

        require('modules/jjwg_Areas/metadata/dashletviewdefs.php');

        parent::__construct($id, $def);

        if (empty($def['title'])) {
            $this->title = translate('LBL_HOMEPAGE_TITLE', 'jjwg_Areas');
        }

        $this->searchFields = $dashletData['jjwg_AreasDashlet']['searchFields'];
        $this->columns = $dashletData['jjwg_AreasDashlet']['columns'];

        $this->seedBean = BeanFactory::newBean('jjwg_Areas');
    }


}
