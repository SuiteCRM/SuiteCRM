<?php
require_once('include/ListView/ListViewSmarty.php');
require_once('modules/AOS_PDF_Templates/formLetter.php');


#[\AllowDynamicProperties]
class AccountsListViewSmarty extends ListViewSmarty
{
    public function __construct()
    {
        parent::__construct();
        $this->targetList = true;
    }

    protected function buildAddAccountContactsToTargetList()
    {
        global $app_strings;
        unset($_REQUEST[session_name()]);
        unset($_REQUEST['PHPSESSID']);
        $current_query_by_page = htmlentities(json_encode($_REQUEST));

        $js = <<<EOF
             if(sugarListView.get_checks_count() < 1) {
                 alert('{$app_strings['LBL_LISTVIEW_NO_SELECTED']}');
                 return false;
             }
 			if ( document.forms['targetlist_form'] ) {
 				var form = document.forms['targetlist_form'];
 				form.reset;
 			} else
 				var form = document.createElement ( 'form' ) ;
 			form.setAttribute ( 'name' , 'targetlist_form' );
 			form.setAttribute ( 'method' , 'post' ) ;
 			form.setAttribute ( 'action' , 'index.php' );
 			document.body.appendChild ( form ) ;
 			if ( !form.module ) {
 			    var input = document.createElement('input');
 			    input.setAttribute ( 'name' , 'module' );
 			    input.setAttribute ( 'value' , '{$this->seed->module_dir}' );
 			    input.setAttribute ( 'type' , 'hidden' );
 			    form.appendChild ( input ) ;
 			    var input = document.createElement('input');
 			    input.setAttribute ( 'name' , 'action' );
 			    input.setAttribute ( 'value' , 'TargetListUpdate' );
 			    input.setAttribute ( 'type' , 'hidden' );
 			    form.appendChild ( input ) ;
 			}
 			if ( !form.uids ) {
 			    var input = document.createElement('input');
 			    input.setAttribute ( 'name' , 'uids' );
 			    input.setAttribute ( 'type' , 'hidden' );
 			    form.appendChild ( input ) ;
 			}
 			if ( !form.prospect_list ) {
 			    var input = document.createElement('input');
 			    input.setAttribute ( 'name' , 'prospect_list' );
 			    input.setAttribute ( 'type' , 'hidden' );
 			    form.appendChild ( input ) ;
 			}
 			if ( !form.return_module ) {
 			    var input = document.createElement('input');
 			    input.setAttribute ( 'name' , 'return_module' );
 			    input.setAttribute ( 'type' , 'hidden' );
 			    form.appendChild ( input ) ;
 			}
 			if ( !form.return_action ) {
 			    var input = document.createElement('input');
 			    input.setAttribute ( 'name' , 'return_action' );
 			    input.setAttribute ( 'type' , 'hidden' );
 			    form.appendChild ( input ) ;
 			}
 			if ( !form.select_entire_list ) {
 			    var input = document.createElement('input');
 			    input.setAttribute ( 'name' , 'select_entire_list' );
 			    input.setAttribute ( 'value', document.MassUpdate.select_entire_list.value);
 			    input.setAttribute ( 'type' , 'hidden' );
 			    form.appendChild ( input ) ;
 			}
 			if ( !form.current_query_by_page ) {
 			    var input = document.createElement('input');
 			    input.setAttribute ( 'name' , 'current_query_by_page' );
 			    input.setAttribute ( 'value', '{$current_query_by_page}' );
 			    input.setAttribute ( 'type' , 'hidden' );
 			    form.appendChild ( input ) ;
 			}
 			open_popup('ProspectLists','600','400','',true,false,{ 'call_back_function':'set_return_and_save_targetlist', 'form_name':'targetlist_form','field_to_name_array':{'id':'prospect_list'}, 'passthru_data':{'do_contacts' : 1 }   } );
EOF;
        $js = str_replace(array("\r","\n"), '', $js);
        return "<a href='javascript:void(0)' class=\"parent-dropdown-action-handler\" id=\"targetlist_listview \" onclick=\"$js\">{$app_strings['LBL_ADD_TO_PROSPECT_LIST_BUTTON_LABEL_ACCOUNTS_CONTACTS']}</a>";
    }


    /**
     *
     * @param File $file deprecated
     * @param array $data
     * @param string $htmlVar
     * @return void|bool
     */
    public function process($file, $data, $htmlVar)
    {
        $this->actionsMenuExtraItems[] = $this->buildAddAccountContactsToTargetList();

        $configurator = new Configurator();
        if ($configurator->isConfirmOptInEnabled()) {
            $this->actionsMenuExtraItems[] = $this->buildSendConfirmOptInEmailToPersonAndCompany();
        }

        $ret = parent::process($file, $data, $htmlVar);

        if (!ACLController::checkAccess($this->seed->module_dir, 'export', true) || !$this->export) {
            $this->ss->assign('exportLink', $this->buildExportLink());
        }

        return $ret;
    }

    /**
     * override
     */
    protected function buildActionsLink($id = 'actions_link', $location = 'top')
    {
        $ret = parent::buildActionsLink($id, $location);

        $replaces = array(
            6 => 7,
        );

        foreach ($replaces as $i => $j) {
            $tmp = $ret['buttons'][$j];
            $ret['buttons'][$j] = $ret['buttons'][$i];
            $ret['buttons'][$i] = $tmp;
        }

        return $ret;
    }

    public function buildExportLink($id = 'export_link')
    {
        global $app_strings;
        global $sugar_config;

        $script = "";
        if (ACLController::checkAccess($this->seed->module_dir, 'export', true)) {
            if ($this->export) {
                $script = parent::buildExportLink($id);
            }
        }

        $script .= "<a href='javascript:void(0)' id='map_listview_top' " .
                    " onclick=\"return sListView.send_form(true, 'jjwg_Maps', " .
                    "'index.php?entryPoint=jjwg_Maps&display_module={$_REQUEST['module']}', " .
                    "'{$app_strings['LBL_LISTVIEW_NO_SELECTED']}')\">{$app_strings['LBL_MAP']}</a>";

        return formLetter::LVSmarty().$script;
    }
}
