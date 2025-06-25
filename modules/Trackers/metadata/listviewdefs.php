<?php
$module_name = 'Trackers';
$listViewDefs[$module_name] =
array(
    'DATE_MODIFIED' => array(
        'type' => 'datetimecombo',
        'label' => 'LBL_DATE_LAST_ACTION',
        'width' => '5%',
        'default' => true,
    ),
    'ASSIGNED_USER_NAME' => array(
        'width' => '9%',
        'label' => 'LBL_ASSIGNED_TO_NAME',
        'module' => 'Users',
        'id' => 'ASSIGNED_USER_ID',
        'default' => true,
        'sortable' => false,
    ),
    'ACTION' => array(
        'type' => 'enum',
        'studio' => 'visible',
        'label' => 'LBL_ACTION',
        'width' => '10%',
        'default' => true,
    ),
    'MODULE_NAME' => array(
        'type' => 'enum',
        'label' => 'LBL_MODULE',
        'width' => '10%',
        'default' => true,
    ),
    'ITEM_ID' => array(
        'type' => 'varchar',
        'label' => 'LBL_ITEM_ID',
        'width' => '10%',
        'default' => false,
        'link' => false,
    ),
    'ITEM_SUMMARY' => array(
        'type' => 'html',
        'label' => 'LBL_ITEM_SUMMARY',
        'width' => '20%',
        'default' => true,
        'link' => false,
        'customCode' => '<div style="max-width:300px; white-space:nowrap; overflow:hidden; text-overflow:ellipsis;">{$ITEM_SUMMARY}</div>',
    ),
    'SESSION_ID' => array(
        'name' => 'session_id',
        'label' => 'LBL_SESSION_ID',
        'type' => 'varchar',
        'width' => '10%',
        'default' => false,
    ),
);
