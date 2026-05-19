<?php
$module_name = 'stic_Conversations';
$layout_defs[$module_name]['subpanel_setup']['securitygroups'] = array(
    'top_buttons' => array(array('widget_class' => 'SubPanelTopSelectButton', 'popup_module' => 'SecurityGroups', 'mode' => 'MultiSelect')),
    'order' => 900,
    'sort_by' => 'name',
    'sort_order' => 'asc',
    'module' => 'SecurityGroups',
    'refresh_page' => 1,
    'subpanel_name' => 'default',
    'get_subpanel_data' => 'SecurityGroups',
    'add_subpanel_data' => 'securitygroup_id',
    'title_key' => 'LBL_SECURITYGROUPS_SUBPANEL_TITLE',
);

// Messages subpanel
$layout_defs[$module_name]['subpanel_setup']['stic_conversations_stic_messages'] = array (
    'order' => 100,
    'module' => 'stic_Messages',
    // 'subpanel_name' => 'default',
    'subpanel_name' => 'Forstic_Conversations',
    'sort_order' => 'asc',
    'sort_by' => 'id',
    'title_key' => 'LBL_STIC_MESSAGES_SUBPANEL_TITLE',
    'get_subpanel_data' => 'stic_conversations_stic_messages',
    'top_buttons' => 
    array (
      0 => array ('widget_class' => 'SubPanelEditMessagesButton',),
    //   1 => array ('widget_class' => 'SubPanelTopSelectButton', 'mode' => 'MultiSelect',),
    ),
);
