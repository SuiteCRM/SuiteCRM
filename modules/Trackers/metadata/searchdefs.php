<?php
$module_name = 'Trackers';
$searchdefs[$module_name] =
array(
    'layout' => array(
        'basic_search' => array(
            'assigned_user_id' => array(
                'name' => 'assigned_user_id',
                'type' => 'enum',
                'function' => array (
                    'name' => 'get_user_array',
                    'params' => array (
                        0 => false,
                    ),
                ),
                'width' => '10%',
                'default' => true,
                'label' => 'LBL_ASSIGNED_TO_NAME',
            ),
            'date_modified' => array(
                'type' => 'datetimecombo',
                'label' => 'LBL_DATE_LAST_ACTION',
                'width' => '10%',
                'default' => true,
                'name' => 'date_modified',
            ),
            'action' => array(
                'type' => 'enum',
                'label' => 'LBL_ACTION',
                'width' => '10%',
                'default' => true,
                'name' => 'action',
            ),
            'module_name' => array(
                'type' => 'enum',
                'label' => 'LBL_MODULE',
                'width' => '10%',
                'default' => true,
                'name' => 'module_name',
            ),
            'item_summary' => array(
                'type' => 'varchar',
                'label' => 'LBL_ITEM_SUMMARY',
                'width' => '10%',
                'default' => true,
                'name' => 'item_summary',
            ),
        ),
        'advanced_search' => array(
            'assigned_user_id' => array(
                'name' => 'assigned_user_id',
                'type' => 'enum',
                'function' => array (
                    'name' => 'get_user_array',
                    'params' => array (
                        0 => false,
                    ),
                ),
                'width' => '10%',
                'default' => true,
                'label' => 'LBL_ASSIGNED_TO_NAME',
            ),
            'date_modified' => array(
                'type' => 'datetimecombo',
                'label' => 'LBL_DATE_LAST_ACTION',
                'width' => '10%',
                'default' => true,
                'name' => 'date_modified',
            ),
            'action' => array(
                'type' => 'enum',
                'label' => 'LBL_ACTION',
                'width' => '10%',
                'default' => true,
                'name' => 'action',
            ),
            'module_name' => array(
                'type' => 'enum',
                'label' => 'LBL_MODULE',
                'width' => '10%',
                'default' => true,
                'name' => 'module_name',
            ),
            'session_id' => array(
                'name' => 'session_id',
                'label' => 'LBL_SESSION_ID',
                'type' => 'varchar',
                'width' => '5%',
                'default' => true,
            ),
            'item_summary' => array(
                'type' => 'varchar',
                'label' => 'LBL_ITEM_SUMMARY',
                'width' => '10%',
                'default' => true,
                'name' => 'item_summary',
            ),
            'item_id' => array(
                'type' => 'varchar',
                'label' => 'LBL_ITEM_ID',
                'width' => '10%',
                'default' => true,
                'name' => 'item_id',
            ),
        ),
    ),
    'templateMeta' => array(
        'maxColumns' => '3',
        'maxColumnsBasic' => '4',
        'widths' => array(
            'label' => '10',
            'field' => '30',
        ),
    ),
);
