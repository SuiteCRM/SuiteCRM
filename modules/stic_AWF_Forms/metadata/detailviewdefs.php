<?php
$module_name = 'stic_AWF_Forms';
$viewdefs [$module_name] = array (
  'DetailView' => array (
    'templateMeta' => array (
      'form' => array (
        'buttons' => array (
          0 => 'EDIT',
          1 => 'DUPLICATE',
          2 => 'DELETE',
          3 => 'FIND_DUPLICATES',
        ),
      ),
      'maxColumns' => '2',
      'widths' => array (
        0 => array (
          'label' => '10',
          'field' => '30',
        ),
        1 => array (
          'label' => '10',
          'field' => '30',
        ),
      ),
      'useTabs' => true,
      'tabDefs' => array(
        'LBL_DEFAULT_PANEL' => array(
          'newTab' => true,
          'panelDefault' => 'expanded',
        ),
        'LBL_PANEL_RECORD_DETAILS' => array(
          'newTab' => true,
          'panelDefault' => 'expanded',
        ),
      ),
    ),
    'panels' => array (
      'lbl_default_panel' => array (
        0 => array (
          0 => 'name',
          1 => 'assigned_user_name',
        ),
        1 => array (
          0 => array (
            'name' => 'form_type',
            'label' => 'LBL_FORM_TYPE',
          ),
          1 => array (
            'name' => 'processing_mode',
            'label' => 'LBL_PROCESSING_MODE',
          ),
        ),
        2 => array (
          0 => array (
            'name' => 'status',
            'label' => 'LBL_STATUS',
          ),
          1 => array (
          ),
        ),
        3 => array (
          0 => array (
            'name' => 'start_date',
            'label' => 'LBL_START_DATE',
          ),
          1 => array (
            'name' => 'end_date',
            'label' => 'LBL_END_DATE',
          ),
        ),        
        4 => array (
          0 => array (
            'name' => 'analytics_views',
            'label' => 'LBL_ANALYTICS_VIEWS',
          ),
          1 => array (
            'name' => 'analytics_blocked',
            'label' => 'LBL_ANALYTICS_BLOCKED',
          ),
        ),
        5 => array (
          0 => array (
            'name' => 'analytics_submissions',
            'label' => 'LBL_ANALYTICS_SUBMISSIONS',
          ),
          1 => array (
            'name' => 'analytics_spam',
            'label' => 'LBL_ANALYTICS_SPAM',
          ),
        ),
        6 => array (
          0 => array (
            'name' => 'analytics_referrers',
            'label' => 'LBL_ANALYTICS_REFERRERS',
          ),
        ),
        7 => array (
          0 => 'description',
        ),
      ),
      'lbl_panel_record_details' => array(
        0 => array(
          0 => array(
            'name' => 'created_by_name',
            'label' => 'LBL_CREATED',
          ),
          1 => array(
            'name' => 'date_entered',
            'customCode' => '{$fields.date_entered.value}',
            'label' => 'LBL_DATE_ENTERED',
          ),
        ),
        1 => array(
          0 => array(
            'name' => 'modified_by_name',
            'label' => 'LBL_MODIFIED_NAME',
          ),
          1 => array(
            'name' => 'date_modified',
            'customCode' => '{$fields.date_modified.value}',
            'label' => 'LBL_DATE_MODIFIED',
          ),
        ),
      ),
    ),
  ),
);
;
?>
