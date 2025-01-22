<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once('include/Dashlets/DashletGeneric.php');
require_once('modules/jjwg_Markers/jjwg_Markers.php');

#[\AllowDynamicProperties]
class jjwg_MarkersDashlet extends DashletGeneric
{
    public function __construct($id, $def = null)
    {

        global $dashletData;

        $dashletData = $dashletData ?? [];

        require('modules/jjwg_Markers/metadata/dashletviewdefs.php');

        parent::__construct($id, $def);

        if (empty($def['title'])) {
            $this->title = translate('LBL_HOMEPAGE_TITLE', 'jjwg_Markers');
        }

        $this->searchFields = $dashletData['jjwg_MarkersDashlet']['searchFields'];
        $this->columns = $dashletData['jjwg_MarkersDashlet']['columns'];

        $this->seedBean = BeanFactory::newBean('jjwg_Markers');
    }


}
