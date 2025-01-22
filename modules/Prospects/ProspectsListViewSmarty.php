<?php

require_once('include/ListView/ListViewSmarty.php');

#[\AllowDynamicProperties]
class ProspectsListViewSmarty extends ListViewSmarty
{
    public function __construct()
    {
        parent::__construct();
    }




    public function buildExportLink($id = 'export_link')
    {
        global $app_strings;

        $script = "<a href='javascript:void(0)' class=\"parent-dropdown-action-handler\" id='export_listview_top' ".
                "onclick=\"return sListView.send_form(true, '{$_REQUEST['module']}', " .
                "'index.php?entryPoint=export', " .
                "'{$app_strings['LBL_LISTVIEW_NO_SELECTED']}')\">{$app_strings['LBL_EXPORT']}</a>" .
                "</li><li>". // List item hack
                "<a href='javascript:void(0)' id='map_listview_top' " .
                " onclick=\"return sListView.send_form(true, 'jjwg_Maps', " .
                "'index.php?entryPoint=jjwg_Maps&display_module={$_REQUEST['module']}', " .
                "'{$app_strings['LBL_LISTVIEW_NO_SELECTED']}')\">{$app_strings['LBL_MAP']}</a>";

        return $script;
    }

    /**
     *
     * @param File $file Template file to use
     * @param array $data from ListViewData
     * @param string $htmlpublic the corresponding html public in xtpl per row
     * @return bool|void
     */
    public function process($file, $data, $htmlpublic)
    {
        $configurator = new Configurator();
        if ($configurator->isConfirmOptInEnabled()) {
            $this->actionsMenuExtraItems[] = $this->buildSendConfirmOptInEmailToPersonAndCompany();
        }

        return parent::process($file, $data, $htmlpublic);
    }
}
