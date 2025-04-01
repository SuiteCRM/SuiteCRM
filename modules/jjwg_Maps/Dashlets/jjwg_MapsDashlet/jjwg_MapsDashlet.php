<?php if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once('include/Dashlets/DashletGeneric.php');
require_once('modules/jjwg_Maps/jjwg_Maps.php');

#[\AllowDynamicProperties]
class jjwg_MapsDashlet extends DashletGeneric
{
    public function __construct($id, $def = null)
    {
        global $dashletData;

        $dashletData = $dashletData ?? [];

        require('modules/jjwg_Maps/metadata/dashletviewdefs.php');

        parent::__construct($id, $def);

        if (empty($def['title'])) {
            $this->title = translate('LBL_HOMEPAGE_TITLE', 'jjwg_Maps');
        }

        $this->searchFields = $dashletData['jjwg_MapsDashlet']['searchFields'];
        $this->columns = $dashletData['jjwg_MapsDashlet']['columns'];

        $this->seedBean = BeanFactory::newBean('jjwg_Maps');
    }


}
