<?php
$module_name = 'AOS_Contracts';
$viewdefs [$module_name] =
array(
  'QuickCreate' =>
  array(
    'templateMeta' =>
    array(
      'maxColumns' => '2',
      'widths' =>
      array(
        0 =>
        array(
          'label' => '10',
          'field' => '30',
        ),
        1 =>
        array(
          'label' => '10',
          'field' => '30',
        ),
      ),
      'useTabs' => false,
      'syncDetailEditViews' => false,
    ),
    'panels' =>
    array(
      'default' =>
      array(
        0 =>
        array(
          0 => 'name',
          1 =>
          array(
            'name' => 'status',
            'studio' => 'visible',
            'label' => 'LBL_STATUS',
          ),
        ),
        1 =>
        array(
          0 => '',
          1 =>
          array(
            'name' => 'start_date',
            'label' => 'LBL_START_DATE',
          ),
        ),
        2 =>
        array(
          0 =>
          array(
            'name' => 'reference_code',
            'label' => 'LBL_REFERENCE_CODE ',
          ),
          1 =>
          array(
            'name' => 'end_date',
            'label' => 'LBL_END_DATE',
          ),
        ),
        3 =>
        array(
          0 =>
          array(
              'name' => 'contract_account',
              'label' => 'LBL_CONTRACT_ACCOUNT',
          ),
          1 =>
          array(
            'name' => 'renewal_reminder_date',
            'label' => 'LBL_RENEWAL_REMINDER_DATE',
          ),
        ),
        4 =>
        array(
          0 =>
          array(
              'name' => 'opportunity',
              'label' => 'LBL_OPPORTUNITY',
          ),
          1 => '',
        ),
        5 =>
        array(
          0 =>
          array(
            'name' => 'customer_signed_date',
            'label' => 'LBL_CUSTOMER_SIGNED_DATE',
          ),
          1 =>
          array(
            'name' => 'company_signed_date',
            'label' => 'LBL_COMPANY_SIGNED_DATE',
          ),
        ),
        6 =>
        array(
          0 =>
          array(
            'name' => 'contract_type',
            'studio' => 'visible',
            'label' => 'LBL_CONTRACT_TYPE',
          ),
          1 =>
          array(
            'name' => 'rminder',
            'label' => 'LBL_RMINDER',
          ),
        ),
        7 =>
        array(
          0 =>
          array(
            'name' => 'description',
            'comment' => 'Full text of the note',
            'label' => 'LBL_DESCRIPTION',
          ),
        ),
      ),
    ),
  ),
);
